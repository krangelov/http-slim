-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.HandleStream
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Ganesh Sittampalam <ganesh@earth.li>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- For more detailed information about what the individual exports do, please consult
-- the documentation for "Network.HTTP". /Notice/ however that the functions here do
-- not perform any kind of normalization prior to transmission (or receipt); you are
-- responsible for doing any such yourself, or, if you prefer, just switch to using
-- "Network.HTTP" function instead.
--
-----------------------------------------------------------------------------
module Network.HTTP.HandleStream
       ( simpleHTTP      -- :: Request -> IO (Result Response)
       , simpleHTTP_     -- :: Connection -> Request -> IO (Result Response)
       , sendHTTP        -- :: Connection -> Request -> IO (Result Response)
       , sendHTTP_notify -- :: Connection -> Request -> IO () -> IO (Result Response)
       , receiveHTTP     -- :: Connection -> IO (Result Request)
       , respondHTTP     -- :: Connection -> Response -> IO ()
       ) where

-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Network.URI ( uriRegName )
import Network.TCP ( Connection, openTCPConnection, close, closeOnEnd,
                     readBlock, readLine, writeBlock, setEncoding )

import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Utils ( trim, readsOne, fmapE, Result, crlf, lf,
                            responseParseError )

import Data.Char (toLower)
import Control.Exception (onException)
import Control.Monad (when)
import Numeric       ( readHex )
import System.IO ( mkTextEncoding, latin1 )

-----------------------------------------------------------------
------------------ Misc -----------------------------------------
-----------------------------------------------------------------

-- | @simpleHTTP@ transmits a resource across a non-persistent connection.
simpleHTTP :: Request -> IO (Result Response)
simpleHTTP r = do
  auth <- getAuth r
  ctxt <- getSSLContext (rqURI r)
  let host = uriRegName auth
      port = uriAuthPort (Just (rqURI r)) auth
  c <- openTCPConnection ctxt host port
  simpleHTTP_ c r

-- | Like 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: Connection -> Request -> IO (Result Response)
simpleHTTP_ s r = sendHTTP s r

-- | @sendHTTP hStream httpRequest@ transmits @httpRequest@ over
-- @hStream@, but does not alter the status of the connection, nor request it to be
-- closed upon receiving the response.
sendHTTP :: Connection -> Request -> IO (Result Response)
sendHTTP conn rq = sendHTTP_notify conn rq (return ())

-- | @sendHTTP_notify hStream httpRequest action@ behaves like 'sendHTTP', but
-- lets you supply an IO @action@ to execute once the request has been successfully
-- transmitted over the connection. Useful when you want to set up tracing of
-- request transmission and its performance.
sendHTTP_notify :: Connection
                -> Request
                -> IO ()
                -> IO (Result Response)
sendHTTP_notify conn rq onSendComplete = do
  when providedClose $ (closeOnEnd conn True)
  onException (sendMain conn rq onSendComplete)
              (close conn)
 where
  providedClose = findConnClose (rqHeaders rq)

-- From RFC 2616, section 8.2.3:
-- 'Because of the presence of older implementations, the protocol allows
-- ambiguous situations in which a client may send "Expect: 100-
-- continue" without receiving either a 417 (Expectation Failed) status
-- or a 100 (Continue) status. Therefore, when a client sends this
-- header field to an origin server (possibly via a proxy) from which it
-- has never seen a 100 (Continue) status, the client SHOULD NOT wait
-- for an indefinite period before sending the request body.'
--
-- Since we would wait forever, I have disabled use of 100-continue for now.
sendMain :: Connection
         -> Request
         -> (IO ())
         -> IO (Result Response)
sendMain conn rqst onSendComplete = do
  -- TODO review throwing away of result
  _ <- writeBlock conn (show rqst)
  setupCharset conn (rqHeaders rqst)
  _ <- writeBlock conn (rqBody rqst)
  setEncoding conn latin1
  onSendComplete
  rsp <- getResponseHead conn
  switchResponse conn True False rsp rqst

   -- Hmmm, this could go bad if we keep getting "100 Continue"
   -- responses...  Except this should never happen according
   -- to the RFC.

switchResponse :: Connection
               -> Bool {- allow retry? -}
               -> Bool {- is body sent? -}
               -> Result ResponseData
               -> Request
               -> IO (Result Response)
switchResponse _ _ _ (Left e) _ = return (Left e)
                -- retry on connreset?
                -- if we attempt to use the same socket then there is an excellent
                -- chance that the socket is not in a completely closed state.

switchResponse conn allow_retry bdy_sent (Right (cd,rn,hdrs)) rqst =
   case matchResponse (rqMethod rqst) cd of
     Continue
      | not bdy_sent -> do {- Time to send the body -}
        writeBlock conn (rqBody rqst) >>= either (return . Left)
           (\ _ -> do
              rsp <- getResponseHead conn
              switchResponse conn allow_retry True rsp rqst)
      | otherwise    -> do {- keep waiting -}
        rsp <- getResponseHead conn
        switchResponse conn allow_retry bdy_sent rsp rqst

     Retry -> do {- Request with "Expect" header failed.
                    Trouble is the request contains Expects
                    other than "100-Continue" -}
        -- TODO review throwing away of result
        _ <- writeBlock conn (show rqst ++ rqBody rqst)
        rsp <- getResponseHead conn
        switchResponse conn False bdy_sent rsp rqst

     Done -> do
       when (findConnClose hdrs)
            (closeOnEnd conn True)
       return (Right $ Response cd rn hdrs "")

     DieHorribly str -> do
       close conn
       return (responseParseError "Invalid response:" str)
     ExpectEntity -> do
       setupCharset conn hdrs
       r <- fmapE (\(ftrs,bdy) -> Right (Response cd rn (hdrs++ftrs) bdy)) $
             maybe (maybe (hopefulTransfer conn [])
                          (\x ->
                              readsOne (linearTransfer conn)
                                       (return$responseParseError "unrecognized content-length value" x)
                                       x)
                          cl)
                   (ifChunked (chunkedTransfer conn)
                              (uglyDeathTransfer "sendHTTP"))
                   tc
       setEncoding conn latin1
       case r of
         Left{} -> do
           close conn
           return r
         Right (Response _ _ hs _) -> do
           when (findConnClose hs)
                (closeOnEnd conn True)
           return r

      where
       tc = lookupHeader HdrTransferEncoding hdrs
       cl = lookupHeader HdrContentLength hdrs

setupCharset :: Connection -> [Header] -> IO ()
setupCharset conn hdrs =
  case lookupHeader HdrContentType hdrs of
    Just val -> case dropWhile (/=';') val of
                  (';':cs) -> case break (=='=') cs of
                                (xs,'=':ys) | trim (map toLower xs) == "charset" ->
                                     do enc <- mkTextEncoding (trim ys++"//IGNORE")
                                        setEncoding conn enc
                                _ -> return ()
                  _        -> return ()
    Nothing  -> return ()

-- reads and parses headers
getResponseHead :: Connection -> IO (Result ResponseData)
getResponseHead conn =
   fmapE parseResponseHead
         (readTillEmpty1 conn)

-- | @receiveHTTP conn@ reads a 'Request' from the 'Connection' @conn@
receiveHTTP :: Connection -> IO (Result Request)
receiveHTTP conn = getRequestHead >>= either (return . Left) processRequest
  where
    -- reads and parses headers
   getRequestHead :: IO (Result RequestData)
   getRequestHead = do
     fmapE parseRequestHead
           (readTillEmpty1 conn)

   processRequest (rm,uri,hdrs) = do
     setupCharset conn hdrs
     res <- fmapE (\(ftrs,bdy) -> Right (Request uri rm (hdrs++ftrs) bdy))
                  (maybe
                     (maybe (return (Right ([], "")))
                            (\x -> readsOne (linearTransfer conn)
                                            (return $ responseParseError "unrecognized Content-Length value" x)
                                            x)
                            cl)
                     (ifChunked (chunkedTransfer conn)
                                (uglyDeathTransfer "receiveHTTP"))
                     tc)
     setEncoding conn latin1
     return res
     where
        -- FIXME : Also handle 100-continue.
        tc = lookupHeader HdrTransferEncoding hdrs
        cl = lookupHeader HdrContentLength hdrs

-- | @respondHTTP conn httpResponse@ transmits an HTTP 'Response' over
-- the 'Connection' @conn@. It could be used to implement simple web
-- server interactions, performing the dual role to 'sendHTTP'.
respondHTTP :: Connection -> Response -> IO ()
respondHTTP conn rsp = do
  -- TODO: review throwing away of result
  _ <- writeBlock conn (show rsp)
   -- write body immediately, don't wait for 100 CONTINUE
  -- TODO: review throwing away of result
  setupCharset conn (rspHeaders rsp)
  _ <- writeBlock conn (rspBody rsp)
  setEncoding conn latin1
  return ()

------------------------------------------------------------------------------

headerName :: String -> String
headerName x = map toLower (trim x)

ifChunked :: a -> a -> String -> a
ifChunked a b s =
  case headerName s of
    "chunked" -> a
    _ -> b

-- | Used when we know exactly how many bytes to expect.
linearTransfer :: Connection -> Int -> IO (Result ([Header],String))
linearTransfer conn n = fmapE (\str -> Right ([],str)) (readBlock conn n)

-- | Used when nothing about data is known,
--   Unfortunately waiting for a socket closure
--   causes bad behaviour.  Here we just
--   take data once and give up the rest.
hopefulTransfer :: Connection
                -> [String]
                -> IO (Result ([Header],String))
hopefulTransfer conn strs
    = readLine conn >>=
      either (\v -> return $ Left v)
             (\more -> if null more
                         then return (Right ([], concat $ reverse strs))
                         else hopefulTransfer conn (more:strs))

-- | A necessary feature of HTTP\/1.1
--   Also the only transfer variety likely to
--   return any footers.
chunkedTransfer :: Connection
                -> IO (Result ([Header], String))
chunkedTransfer conn = chunkedTransferC conn [] 0

chunkedTransferC :: Connection
                 -> [String]
                 -> Int
                 -> IO (Result ([Header], String))
chunkedTransferC conn acc n = do
  v <- readLine conn
  case v of
    Left e -> return (Left e)
    Right line
     | size == 0 ->
         -- last chunk read; look for trailing headers..
         fmapE (\strs -> do
                  ftrs <- parseHeaders strs
                   -- insert (computed) Content-Length header.
                  let ftrs' = Header HdrContentLength (show n) : ftrs
                  return (ftrs',concat (reverse acc)))
               (readTillEmpty2 conn [])
     | otherwise -> do
         some <- readBlock conn size
         case some of
           Left e      -> return (Left e)
           Right cdata -> do
               _ <- readLine conn
               chunkedTransferC conn (cdata:acc) (n+size)
     where
       size
         | null line = 0
         | otherwise =
             case readHex line of
               (hx,_):_ -> hx
               _        -> 0

-- | Maybe in the future we will have a sensible thing
--   to do here, at that time we might want to change
--   the name.
uglyDeathTransfer :: String -> IO (Result ([Header],a))
uglyDeathTransfer loc = return (responseParseError loc "Unknown Transfer-Encoding")


-- | Remove leading crlfs then call readTillEmpty2 (not required by RFC)
readTillEmpty1 :: Connection
               -> IO (Result [String])
readTillEmpty1 conn =
  readLine conn >>=
    either (return . Left)
           (\s ->
               if s == crlf || s == lf
                then readTillEmpty1 conn
                else readTillEmpty2 conn [s])

-- | Read lines until an empty line (CRLF),
--   also accepts a connection close as end of
--   input, which is not an HTTP\/1.1 compliant
--   thing to do - so probably indicates an
--   error condition.
readTillEmpty2 :: Connection
               -> [String]
               -> IO (Result [String])
readTillEmpty2 conn list =
    readLine conn >>=
      either (return . Left)
             (\ s ->
                if s == crlf || s == lf || null s
                 then return (Right $ reverse (s:list))
                 else readTillEmpty2 conn (s:list))
