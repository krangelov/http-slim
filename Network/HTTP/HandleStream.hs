-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.HandleStream
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Krasimir Angelov <kr.angelov@gmail.com>
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
       ( simpleHTTP      -- :: Request -> IO Response
       , simpleHTTP_     -- :: Connection -> Request -> IO Response
       , sendHTTP        -- :: Connection -> Request -> IO Response
       , sendHTTP_notify -- :: Connection -> Request -> IO () -> IO Response
       , receiveHTTP     -- :: Connection -> IO Request
       , respondHTTP     -- :: Connection -> Response -> IO ()
       ) where

-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Network.URI ( uriRegName )
import Network.TCP ( Connection, openTCPConnection, close, closeOnEnd,
                     readBlock, readLine, writeAscii, writeBlock )

import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Utils ( trim, readsOne, crlf, lf, HttpError(..), encodeString )

import Data.Char (toLower)
import Control.Exception (onException, throwIO, bracket)
import Control.Monad (when)
import Numeric       ( readHex )
import System.IO ( TextEncoding, latin1 )
import qualified Data.ByteString.Lazy as LBS

-----------------------------------------------------------------
------------------ Misc -----------------------------------------
-----------------------------------------------------------------

-- | @simpleHTTP@ transmits a resource across a non-persistent connection.
simpleHTTP :: Request -> IO Response
simpleHTTP r = do
  auth <- getAuth r
  ctxt <- getSSLContext (rqURI r)
  let host = uriRegName auth
      port = uriAuthPort (Just (rqURI r)) auth
  c <- openTCPConnection ctxt host port
  simpleHTTP_ c r

-- | Like 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: Connection -> Request -> IO Response
simpleHTTP_ s r = sendHTTP s r

-- | @sendHTTP hStream httpRequest@ transmits @httpRequest@ over
-- @hStream@, but does not alter the status of the connection, nor request it to be
-- closed upon receiving the response.
sendHTTP :: Connection -> Request -> IO Response
sendHTTP conn rq = sendHTTP_notify conn rq (return ())

-- | @sendHTTP_notify hStream httpRequest action@ behaves like 'sendHTTP', but
-- lets you supply an IO @action@ to execute once the request has been successfully
-- transmitted over the connection. Useful when you want to set up tracing of
-- request transmission and its performance.
sendHTTP_notify :: Connection
                -> Request
                -> IO ()
                -> IO Response
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
         -> IO Response
sendMain conn rqst onSendComplete = do
  enc <- getEncoding (rqHeaders rqst)
  lbs <- encodeString enc (rqBody rqst)
  let rqst' = insertHeader HdrContentLength (show (LBS.length lbs)) rqst
  _ <- writeAscii conn (show rqst')
  _ <- writeBlock conn lbs
  onSendComplete
  rsp <- getResponseHead conn
  switchResponse conn True False rsp rqst'

   -- Hmmm, this could go bad if we keep getting "100 Continue"
   -- responses...  Except this should never happen according
   -- to the RFC.

switchResponse :: Connection
               -> Bool {- allow retry? -}
               -> Bool {- is body sent? -}
               -> ResponseData
               -> Request
               -> IO Response
switchResponse conn allow_retry bdy_sent (cd,rn,hdrs) rqst =
   case matchResponse (rqMethod rqst) cd of
     Continue
      | not bdy_sent -> do {- Time to send the body -}
        writeAscii conn (rqBody rqst) >>=
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
        _ <- writeAscii conn (show rqst ++ rqBody rqst)
        rsp <- getResponseHead conn
        switchResponse conn False bdy_sent rsp rqst

     Done -> do
       when (findConnClose hdrs)
            (closeOnEnd conn True)
       return (Response cd rn hdrs "")

     DieHorribly str -> do
       close conn
       throwIO (ErrorParse str)
     ExpectEntity -> do
       enc <- getEncoding hdrs
       (ftrs,bdy) <- 
           onException
             (maybe (maybe (hopefulTransfer conn enc [])
                           (\x -> readsOne (linearTransfer conn enc)
                                           (throwIO (ErrorParse ("unrecognized content-length value"++x)))
                                           x)
                           cl)
                    (ifChunked (chunkedTransfer conn enc)
                               (uglyDeathTransfer "sendHTTP"))
                    tc)
             (close conn)
       let hs  = hdrs++ftrs
           rsp = Response cd rn hs bdy
       when (findConnClose hs)
            (closeOnEnd conn True)
       return rsp
      where
       tc = lookupHeader HdrTransferEncoding hdrs
       cl = lookupHeader HdrContentLength hdrs

-- reads and parses headers
getResponseHead :: Connection -> IO ResponseData
getResponseHead conn = do
   strs <- readTillEmpty1 conn latin1
   case parseResponseHead strs of
     Left err -> throwIO err
     Right rd -> return rd

-- | @receiveHTTP conn@ reads a 'Request' from the 'Connection' @conn@
receiveHTTP :: Connection -> IO Request
receiveHTTP conn = getRequestHead >>= processRequest
  where
    -- reads and parses headers
   getRequestHead :: IO RequestData
   getRequestHead = do
     strs <- readTillEmpty1 conn latin1
     case parseRequestHead strs of
       Left err -> throwIO err
       Right rq -> return rq

   processRequest (rm,uri,hdrs) = do
     enc <- getEncoding hdrs
     (ftrs,bdy) <- maybe
                     (maybe (return ([], ""))
                            (\x -> readsOne (linearTransfer conn enc)
                                            (throwIO (ErrorParse ("unrecognized Content-Length value"++x)))
                                            x)
                            cl)
                     (ifChunked (chunkedTransfer conn enc)
                                (uglyDeathTransfer "receiveHTTP"))
                     tc
     return (Request uri rm (hdrs++ftrs) bdy)
     where
        -- FIXME : Also handle 100-continue.
        tc = lookupHeader HdrTransferEncoding hdrs
        cl = lookupHeader HdrContentLength hdrs

-- | @respondHTTP conn httpResponse@ transmits an HTTP 'Response' over
-- the 'Connection' @conn@. It could be used to implement simple web
-- server interactions, performing the dual role to 'sendHTTP'.
respondHTTP :: Connection -> Response -> IO ()
respondHTTP conn rsp = do
  enc <- getEncoding (rspHeaders rsp)
  bs <- encodeString enc (rspBody rsp)
  let rsp' = normalizeResponse (Just (LBS.length bs)) rsp
  _ <- writeAscii conn (show rsp')
  writeBlock conn bs

------------------------------------------------------------------------------

headerName :: String -> String
headerName x = map toLower (trim x)

ifChunked :: a -> a -> String -> a
ifChunked a b s =
  case headerName s of
    "chunked" -> a
    _ -> b

-- | Used when we know exactly how many bytes to expect.
linearTransfer :: Connection -> TextEncoding -> Int -> IO ([Header],String)
linearTransfer conn enc n = do
  str <- readBlock conn enc n
  return ([],str)

-- | Used when nothing about data is known,
--   Unfortunately waiting for a socket closure
--   causes bad behaviour.  Here we just
--   take data once and give up the rest.
hopefulTransfer :: Connection
                -> TextEncoding
                -> [String]
                -> IO ([Header],String)
hopefulTransfer conn enc strs = do
  more <- readLine conn enc
  if null more
    then return ([], concat $ reverse strs)
    else hopefulTransfer conn enc (more:strs)

-- | A necessary feature of HTTP\/1.1
--   Also the only transfer variety likely to
--   return any footers.
chunkedTransfer :: Connection
                -> TextEncoding
                -> IO ([Header], String)
chunkedTransfer conn enc = chunkedTransferC conn enc [] 0

chunkedTransferC :: Connection
                 -> TextEncoding
                 -> [String]
                 -> Int
                 -> IO ([Header], String)
chunkedTransferC conn enc acc n = do
  line <- readLine conn enc
  let size | null line = 0
           | otherwise =
               case readHex line of
                 (hx,_):_ -> hx
                 _        -> 0
  if size == 0
    then do strs <- readTillEmpty2 conn enc []
            case parseHeaders strs of
              Left err   -> throwIO err
              Right ftrs -> do -- insert (computed) Content-Length header.
                               let ftrs' = Header HdrContentLength (show n) : ftrs
                               return (ftrs',concat (reverse acc))
    else do cdata <- readBlock conn enc size
            _     <- readLine conn enc
            chunkedTransferC conn enc (cdata:acc) (n+size)

-- | Maybe in the future we will have a sensible thing
--   to do here, at that time we might want to change
--   the name.
uglyDeathTransfer :: String -> IO ([Header],a)
uglyDeathTransfer loc = throwIO (ErrorParse ("Unknown Transfer-Encoding in "++loc))


-- | Remove leading crlfs then call readTillEmpty2 (not required by RFC)
readTillEmpty1 :: Connection -> TextEncoding -> IO [String]
readTillEmpty1 conn enc = do
  s <- readLine conn enc
  if s == crlf || s == lf
    then readTillEmpty1 conn enc
    else readTillEmpty2 conn enc [s]

-- | Read lines until an empty line (CRLF),
--   also accepts a connection close as end of
--   input, which is not an HTTP\/1.1 compliant
--   thing to do - so probably indicates an
--   error condition.
readTillEmpty2 :: Connection
               -> TextEncoding
               -> [String]
               -> IO [String]
readTillEmpty2 conn enc list = do
  s <- readLine conn enc
  if s == crlf || s == lf || null s
    then return (reverse list)
    else readTillEmpty2 conn enc (s:list)
