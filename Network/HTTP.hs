{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Ganesh Sittampalam <ganesh@earth.li>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- The 'Network.HTTP' module provides a simple interface for sending and
-- receiving content over HTTP in Haskell. Here's how to fetch a document from
-- a URL and return it as a String:
--
-- >
-- >    simpleHTTP (getRequest "http://www.haskell.org/") >>= fmap (take 100) . getResponseBody
-- >        -- fetch document and return it (as a 'String'.)
--
-- Other functions let you control the submission and transfer of HTTP
-- 'Request's and 'Response's more carefully, letting you integrate the use
-- of 'Network.HTTP' functionality into your application.
--
-- The module also exports the main types of the package, 'Request' and 'Response',
-- along with 'Header' and functions for working with these.
--
-- The actual functionality is implemented by modules in the @Network.HTTP.*@
-- namespace, letting you either use the default implementation here
-- by importing @Network.HTTP@ or, for more specific uses, selectively
-- import the modules in @Network.HTTP.*@. To wit, more than one kind of
-- representation of the bulk data that flows across a HTTP connection is
-- supported. (see "Network.HTTP.Connection".)
--
-- /NOTE:/ The 'Request' send actions will normalize the @Request@ prior to transmission.
-- Normalization such as having the request path be in the expected form and, possibly,
-- introduce a default @Host:@ header if one isn't already present.
-- Normalization also takes the @"user:pass\@"@ portion out of the the URI,
-- if it was supplied, and converts it into @Authorization: Basic@ header.
-- If you do not
-- want the requests tampered with, but sent as-is, please import and use the
-- the "Network.HTTP.Connection" module instead. They
-- export the same functions, but leaves construction and any normalization of
-- @Request@s to the user.
-----------------------------------------------------------------------------
module Network.HTTP
       ( module Network.HTTP.Base
       , module Network.HTTP.Headers

       -- ** High-level API
       , simpleHTTP       -- :: Request -> IO (Result Response)
       , simpleHTTP_      -- :: Connection -> Request -> IO (Result Response)
       , simpleServer     -- :: SockAddr -> (Request -> IO Response) -> IO ()
       , simpleServerBind -- :: Int -> HostAddress -> (Request -> IO Response) -> IO ()

       , output
       , outputChunked
       , outputHTML
       , outputText

       -- ** Low-level API
       , sendHTTP         -- :: Connection -> Request -> IO (Result Response)
       , sendHTTP_notify  -- :: Connection -> Request -> IO () -> IO (Result Response)
       , S.receiveHTTP    -- :: Connection -> IO (Result Request)
       , S.respondHTTP    -- :: Connection -> Response -> IO ()

       , module Network.TCP

       -- ** Create requests
       , getRequest          -- :: String -> Request
       , headRequest         -- :: String -> Request
       , postRequest         -- :: String -> Request
       , postRequestWithBody -- :: String -> String -> String -> Request_String

       -- ** Handle responses
       , getResponseBody  -- :: Result Request -> IO ty
       , getResponseCode  -- :: Result Request -> IO ResponseCode
       ) where

-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Network.HTTP.Headers
import Network.HTTP.Base
import Network.HTTP.Utils ( crlf, Result )
import qualified Network.HTTP.HandleStream as S ( sendHTTP, sendHTTP_notify,
                                                  receiveHTTP, respondHTTP )
import Network.TCP
import Network.URI ( parseURI, uriRegName )
import qualified Network.Socket as Socket
import Network.BSD (getProtocolNumber)
#if MIN_VERSION_network(3,0,0)
#else
import Network.Socket (iNADDR_ANY)
#endif
import Network.Socket (
          Socket, SockAddr(SockAddrInet),
          setSocketOption, socket)

import qualified OpenSSL.Session as SSL

import Control.Concurrent (forkIO)
import Control.Exception (finally)

import Numeric (showHex)
import Data.Maybe (isJust)

#if MIN_VERSION_network(3,0,0)
iNADDR_ANY :: Socket.HostAddress
iNADDR_ANY = Socket.tupleToHostAddress (0,0,0,0)
#endif


-- | @simpleHTTP req@ transmits the 'Request' @req@ by opening a /direct/, non-persistent
-- connection to the HTTP server that @req@ is destined for, followed by transmitting
-- it and gathering up the response as a 'Result'. Prior to sending the request,
-- it is normalized (via 'normalizeRequest'). If you have to mediate the request
-- via an HTTP proxy, you will have to normalize the request yourself. Or switch to
-- using 'Network.Browser' instead.
--
-- Examples:
--
-- > simpleHTTP (getRequest "http://hackage.haskell.org/")
-- > simpleHTTP (getRequest "http://hackage.haskell.org:8012/")

simpleHTTP :: Request -> IO (Result Response)
simpleHTTP r = do
  auth <- getAuth r
  ctxt <- getSSLContext (rqURI r)
  let host = uriRegName auth
      port = uriAuthPort (Just (rqURI r)) auth
  c <- openTCPConnection ctxt host port
  let norm_r = normalizeRequest defaultNormalizeRequestOptions{normDoClose=True} r
  S.sendHTTP c norm_r

-- | Identical to 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: Connection -> Request -> IO (Result Response)
simpleHTTP_ c r = do
  let norm_r = normalizeRequest defaultNormalizeRequestOptions{normDoClose=True} r
  S.sendHTTP c norm_r

-- | @sendHTTP hStream httpRequest@ transmits @httpRequest@ (after normalization) over
-- @hStream@, but does not alter the status of the connection, nor request it to be
-- closed upon receiving the response.
sendHTTP :: Connection -> Request -> IO (Result Response)
sendHTTP conn rq = do
  let norm_r = normalizeRequest defaultNormalizeRequestOptions rq
  S.sendHTTP conn norm_r

-- | @sendHTTP_notify conn httpRequest action@ behaves like 'sendHTTP', but
-- lets you supply an IO @action@ to execute once the request has been successfully
-- transmitted over the connection. Useful when you want to set up tracing of
-- request transmission and its performance.
sendHTTP_notify :: Connection
                -> Request
                -> IO ()
                -> IO (Result Response)
sendHTTP_notify conn rq onSendComplete = do
  let norm_r = normalizeRequest defaultNormalizeRequestOptions rq
  S.sendHTTP_notify conn norm_r onSendComplete

-- | A convenience constructor for a GET 'Request'.
--
-- If the URL isn\'t syntactically valid, the function raises an error.
getRequest
    :: String            -- ^URL to fetch
    -> Request           -- ^The constructed request
getRequest urlString =
  case parseURI urlString of
    Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest GET u

-- | A convenience constructor for a HEAD 'Request'.
--
-- If the URL isn\'t syntactically valid, the function raises an error.
headRequest
    :: String         -- ^URL to fetch
    -> Request        -- ^The constructed request
headRequest urlString =
  case parseURI urlString of
    Nothing -> error ("headRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest HEAD u

-- | A convenience constructor for a POST 'Request'.
--
-- If the URL isn\'t syntactically valid, the function raises an error.
postRequest
    :: String            -- ^URL to POST to
    -> Request           -- ^The constructed request
postRequest urlString =
  case parseURI urlString of
    Nothing -> error ("postRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest POST u

-- | A convenience constructor for a POST 'Request'.
--
-- It constructs a request and sets the body as well as
-- the Content-Type and Content-Length headers. The contents of the body
-- are forced to calculate the value for the Content-Length header.
--
-- If the URL isn\'t syntactically valid, the function raises an error.
postRequestWithBody
    :: String                      -- ^URL to POST to
    -> String                      -- ^Content-Type of body
    -> String                      -- ^The body of the request
    -> Request                     -- ^The constructed request
postRequestWithBody urlString typ body =
  case parseURI urlString of
    Nothing -> error ("postRequestWithBody: Not a valid URL - " ++ urlString)
    Just u  -> setRequestBody (mkRequest POST u) (typ, body)

-- | @getResponseBody response@ takes the response of a HTTP requesting action and
-- tries to extricate the body of the 'Response' @response@. If the request action
-- returned an error, an IO exception is raised.
getResponseBody :: Result Response -> IO String
getResponseBody (Left err) = fail (show err)
getResponseBody (Right r)  = return (rspBody r)

-- | @getResponseCode response@ takes the response of a HTTP requesting action and
-- tries to extricate the status code of the 'Response' @response@. If the request action
-- returned an error, an IO exception is raised.
getResponseCode :: Result Response -> IO ResponseCode
getResponseCode (Left err) = fail (show err)
getResponseCode (Right r)  = return (rspCode r)


simpleServer
   :: Maybe Int                      -- ^ http  port
   -> Maybe (Int,FilePath,FilePath)  -- ^ https port,private and public keys
   -> (Request -> IO Response)       -- ^ The functionality of the Server
   -> IO ()
simpleServer mb_http_port mb_https_port callOut =
  simpleServerBind mb_http_port mb_https_port iNADDR_ANY callOut

simpleServerBind
   :: Maybe Int                     -- ^ http  port
   -> Maybe (Int,FilePath,FilePath) -- ^ https port,private and public keys
   -> Socket.HostAddress            -- ^ The host address
   -> (Request -> IO Response)      -- ^ The functionality of the Server
   -> IO ()
simpleServerBind mb_http_port mb_https_port addr callOut = do
  case mb_https_port of
    Just (port,priv_key,cert_key)
            -> do ctxt <- SSL.context
                  SSL.contextSetPrivateKeyFile ctxt priv_key
                  SSL.contextSetCertificateFile ctxt cert_key

                  let fork | isJust mb_http_port = \f -> forkIO f >> return ()
                           | otherwise           = id

                      mkSSL sock = do
                        ssl <- SSL.connection ctxt sock
                        SSL.accept ssl
                        return (Just ssl)

                  fork (simpleServerMain (SockAddrInet (fromIntegral port) addr) mkSSL callOut)
    Nothing -> return ()
  case mb_http_port of
    Just port -> do let noSSL sock = return Nothing
                    simpleServerMain (SockAddrInet (fromIntegral port) addr) noSSL callOut
    Nothing   -> return ()

simpleServerMain
   :: SockAddr
   -> (Socket -> IO (Maybe SSL.SSL))
   -> (Request -> IO Response)
   -> IO ()
simpleServerMain sockaddr mkSSL callOut = do
  num <- getProtocolNumber "tcp"
  sock <- socket Socket.AF_INET Socket.Stream num
  setSocketOption sock Socket.ReuseAddr 1
  Socket.bind sock sockaddr
  Socket.listen sock Socket.maxListenQueue

  loopIO (do (acceptedSock,_) <- Socket.accept sock
             mb_ssl <- mkSSL acceptedSock
             stream <- socketConnection "localhost" (fromIntegral num) acceptedSock mb_ssl
             forkIO $ do 
               ereq <- S.receiveHTTP stream
               case ereq of
                 Right req -> do resp <- callOut req
                                 S.respondHTTP stream resp
                 _         -> return ()
               close stream
         ) `finally` Socket.close sock
  where
    loopIO m = m >> loopIO m

{- | return the respons with the content-length added -}
output :: Response -> IO Response
output resp@(Response{rspBody=body}) =
  return (insertHeaderIfMissing HdrContentLength (show (length body)) resp)

outputChunked :: Int -> Response -> IO Response
outputChunked chunkSize resp@(Response{rspBody=body}) =
  return resp{rspHeaders = rspHeaders resp ++ [chunkedHdr]
             ,rspBody    = foldr ($) "" $
                             map (\str ->
                                     showHex (length str) . showString crlf .
                                     showString str . showString crlf)
                                 (slice chunkSize body) ++
                             -- terminating chunk
                             showString "0" . showString crlf :
                             -- terminating trailer
                             showString crlf :
                             []
             }
  where
    chunkedHdr = Header HdrTransferEncoding "chunked"

    slice :: Int -> [a] -> [[a]]
    slice n = map (take n) . takeWhile (not . null) . iterate (drop n)

outputHTML :: String -> IO Response
outputHTML html =
  return (Response
            { rspCode = (2,0,0)
            , rspReason = "OK"
            , rspHeaders = [Header HdrServer defaultServer
                           ,Header HdrContentType "text/html"
                           ,Header HdrContentLength (show (length html))
                           ]
            , rspBody = html
            })

outputText :: String -> IO Response
outputText text =
  return (Response
            { rspCode = (2,0,0)
            , rspReason = "OK"
            , rspHeaders = [Header HdrServer defaultServer
                           ,Header HdrContentType "text/plain"
                           ,Header HdrContentLength (show (length text))
                           ]
            , rspBody = text
            })

--
-- * TODO
--     - request pipelining
--     - https upgrade (includes full TLS, i.e. SSL, implementation)
--         - use of Stream classes will pay off
--         - consider C implementation of encryption\/decryption
--     - comm timeouts
--     - MIME & entity stuff (happening in separate module)
--     - support \"*\" uri-request-string for OPTIONS request method
--
--
-- * Header notes:
--
--     [@Host@]
--                  Required by HTTP\/1.1, if not supplied as part
--                  of a request a default Host value is extracted
--                  from the request-uri.
--
--     [@Connection@]
--                  If this header is present in any request or
--                  response, and it's value is "close", then
--                  the current request\/response is the last
--                  to be allowed on that connection.
--
--     [@Expect@]
--                  Should a request contain a body, an Expect
--                  header will be added to the request.  The added
--                  header has the value \"100-continue\".  After
--                  a 417 \"Expectation Failed\" response the request
--                  is attempted again without this added Expect
--                  header.
--
--     [@TransferEncoding,ContentLength,...@]
--                  if request is inconsistent with any of these
--                  header values then you may not receive any response
--                  or will generate an error response (probably 4xx).
--
--
-- * Response code notes
-- Some response codes induce special behaviour:
--
--   [@1xx@]   \"100 Continue\" will cause any unsent request body to be sent.
--             \"101 Upgrade\" will be returned.
--             Other 1xx responses are ignored.
--
--   [@417@]   The reason for this code is \"Expectation failed\", indicating
--             that the server did not like the Expect \"100-continue\" header
--             added to a request.  Receipt of 417 will induce another
--             request attempt (without Expect header), unless no Expect header
--             had been added (in which case 417 response is returned).
