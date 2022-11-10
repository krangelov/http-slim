{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, CPP #-}
module Network.FastCGI
         ( -- * Accepting requests
           simpleFastCGI,

           -- * Environment
           Env,
           getDocumentRoot,
           getGatewayInterface,
           getPathInfo,
           getPathTranslated,
           getQueryString,
           getRedirectStatus,
           getRedirectURI,
           getRemoteAddress,
           getRemotePort,
           getRemoteHost,
           getRemoteIdent,
           getRemoteUser,
           getScriptFilename,
           getScriptName,
           getServerAddress,
           getServerName,
           getServerPort,
           getServerProtocol,
           getServerSoftware,
           getAuthenticationType
         ) where

import Control.Concurrent
import Control.Exception ( bracket, catch )
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BSC (unpack, pack)
import Data.Char
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import qualified Network.Socket as Socket hiding (recv)
import qualified Network.Socket.ByteString as Socket
import Network.URI ( URI(..), URIAuth(..)
#if MIN_VERSION_network_uri(2,6,2)
                   , nullURI, nullURIAuth
#endif
                   , parseURIReference )
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Utils ( parseInt, trim, encodeString, decodeString )
import System.IO ( TextEncoding, latin1 )

-- | An opaque type representing the state of a single connection from the web server.
data FastCGIState = FastCGIState {
      fcgiSocket    :: Socket.Socket,
      fcgiPeer      :: Socket.SockAddr,
      fcgiRequests  :: Map.Map Int PartialRequest
    }
    
type Env = [(String,String)]

data PartialRequest = PartialRequest {
       prqEnv       :: Env,
       prqParamsBuf :: BS.ByteString,
       prqEncoding  :: TextEncoding,
       prqBody      :: [String],
       prqReq       :: Request
     }

data Record = Record {
      recordType :: RecordType,
      recordRequestID :: Int,
      recordContent :: BS.ByteString
    } deriving (Show)

data RecordType = BeginRequestRecord
                | AbortRequestRecord
                | EndRequestRecord
                | ParamsRecord
                | StdinRecord
                | StdoutRecord
                | StderrRecord
                | DataRecord
                | GetValuesRecord
                | GetValuesResultRecord
                | UnknownTypeRecord
                | OtherRecord Int
                  deriving (Eq, Show)

instance Enum RecordType where
    toEnum 1 = BeginRequestRecord
    toEnum 2 = AbortRequestRecord
    toEnum 3 = EndRequestRecord
    toEnum 4 = ParamsRecord
    toEnum 5 = StdinRecord
    toEnum 6 = StdoutRecord
    toEnum 7 = StderrRecord
    toEnum 8 = DataRecord
    toEnum 9 = GetValuesRecord
    toEnum 10 = GetValuesResultRecord
    toEnum 11 = UnknownTypeRecord
    toEnum code = OtherRecord code

    fromEnum BeginRequestRecord = 1
    fromEnum AbortRequestRecord = 2
    fromEnum EndRequestRecord = 3
    fromEnum ParamsRecord = 4
    fromEnum StdinRecord = 5
    fromEnum StdoutRecord = 6
    fromEnum StderrRecord = 7
    fromEnum DataRecord = 8
    fromEnum GetValuesRecord = 9
    fromEnum GetValuesResultRecord = 10
    fromEnum UnknownTypeRecord = 11
    fromEnum (OtherRecord code) = code


-- | Takes a handler, and concurrently accepts requests from the web server
--   by calling the handler.
simpleFastCGI
    :: (Env -> Request -> IO Response)
    -- ^ A handler which is invoked once for each incoming connection.
    -> IO ()
    -- ^ Never actually returns.
simpleFastCGI handler = do
#if MIN_VERSION_network(3,0,0)
  listenSocket <- Socket.mkSocket 0
#else
  listenSocket <- Socket.mkSocket 0 Socket.AF_INET Socket.Stream Socket.defaultProtocol Socket.NotConnected
#endif
  let acceptLoop' = do
        (socket, peer) <- Socket.accept listenSocket
        let state = FastCGIState {
                       fcgiSocket = socket,
                       fcgiPeer = peer,
                       fcgiRequests = Map.empty
                    }
        requestLoop state handler
        acceptLoop'
  acceptLoop'

requestLoop :: FastCGIState -> ([(String,String)] -> Request -> IO Response) -> IO ()
requestLoop state handler = do
  maybeRecord <- recvRecord state
  case maybeRecord of
    Nothing -> do
      Socket.close (fcgiSocket state)
    Just record -> do
      case recordType record of
        BeginRequestRecord -> do
          let req = Request { rqURI=nullURI
                            , rqMethod = GET
                            , rqHeaders = []
                            , rqBody = ""
                            }
              prq = PartialRequest { prqEnv = []
                                   , prqParamsBuf = BS.empty
                                   , prqEncoding = latin1
                                   , prqBody = []
                                   , prqReq = req
                                   }
              state' = state{fcgiRequests=Map.insert 
                                                (recordRequestID record)
                                                prq
                                                (fcgiRequests state)
                            }
          requestLoop state' handler
        GetValuesRecord -> do
          fLog state (recordRequestID record)
               ("Get values record: " ++ show record)
          requestLoop state handler
        ParamsRecord -> do
          let requestID = recordRequestID record
          case Map.lookup requestID (fcgiRequests state) of
            Nothing -> fLog state (recordRequestID record)
                            ("Ignoring record for unknown request ID " ++ show requestID)
            Just prq
              | BS.length (recordContent record) == 0 -> do
                   enc <- getEncoding (rqHeaders (prqReq prq))
                   let state' = state{fcgiRequests=Map.insert 
                                                         requestID
                                                         prq{prqEncoding=enc}
                                                         (fcgiRequests state)}
                   requestLoop state' handler
              | otherwise -> do
                   let takeUntilEmpty prq bufferTail =
                         case takeNameValuePair bufferTail of
                           Nothing -> return prq{prqParamsBuf=bufferTail}
                           Just ((name, value), bufferTail) -> do
                              let name'  = BSC.unpack name
                                  value' = BSC.unpack value
                                  prq'   = processRequestVariable name' value' prq
                              takeUntilEmpty prq' bufferTail
                   prq <- takeUntilEmpty
                            prq
                            (BS.append (prqParamsBuf prq)
                                       (recordContent record))
                   let state' = state{fcgiRequests=Map.insert 
                                                         requestID
                                                         prq
                                                         (fcgiRequests state)}
                   requestLoop state' handler
        StdinRecord -> do
          let requestID = recordRequestID record
          case Map.lookup requestID (fcgiRequests state) of
            Nothing  -> 
               fLog state (recordRequestID record)
                          ("Ignoring record for unknown request ID " ++ show requestID)
            Just prq
              | BS.length (recordContent record) == 0 -> do
                   forkIO $ do
                     rsp <- handleErrors (fLog state requestID)
                               (handler (prqEnv prq)
                                        (prqReq prq){rqBody=concat (reverse (prqBody prq))})
                     sendResponse state requestID rsp
                     sendRecord state $ Record {
                       recordType = EndRequestRecord,
                       recordRequestID = requestID,
                       recordContent = BS.pack [0, 0, 0, 0, 0, 0, 0, 0]
                     }
                   let state' = state{fcgiRequests=Map.delete
                                                         requestID
                                                         (fcgiRequests state)}
                   requestLoop state' handler
              | otherwise -> do
                   s <- decodeString (prqEncoding prq)
                                     (recordContent record)
                   let prq'   = prq{prqBody=s:prqBody prq}
                       state' = state{fcgiRequests=Map.insert requestID
                                                          prq'
                                                          (fcgiRequests state)}
                   requestLoop state' handler
        OtherRecord unknownCode -> do
          sendRecord state $ Record {
            recordType = UnknownTypeRecord,
            recordRequestID = 0,
            recordContent = BS.pack [fromIntegral unknownCode,
                                     0, 0, 0, 0, 0, 0, 0]
          }
        _ -> fLog state (recordRequestID record)
                  ("Ignoring record of unexpected type "++show (recordType record))

#if MIN_VERSION_network_uri(2,6,2)
#else
-- |Blank URI
nullURI :: URI
nullURI = URI
    { uriScheme     = ""
    , uriAuthority  = Nothing
    , uriPath       = ""
    , uriQuery      = ""
    , uriFragment   = ""
    }

-- |Blank URIAuth.
nullURIAuth :: URIAuth
nullURIAuth = URIAuth
    { uriUserInfo   = ""
    , uriRegName    = ""
    , uriPort       = ""
    }
#endif

processRequestVariable :: String -> String -> PartialRequest -> PartialRequest
processRequestVariable "REQUEST_METHOD" value prq =
  prq{prqReq=(prqReq prq){rqMethod=parseRequestMethod value}}
processRequestVariable "REQUEST_URI" value prq =
  let rq   = prqReq prq
      uri  = rqURI rq
      sch  = uriScheme    uri
      auth = uriAuthority uri
  in case parseURIReference value of
       Just uri -> prq{prqReq=rq{rqURI=uri{uriScheme=sch,uriAuthority=auth}}}
       Nothing  -> prq
processRequestVariable "SERVER_NAME" value prq =
  let rq   = prqReq prq
      uri  = rqURI rq
      auth = fromMaybe nullURIAuth (uriAuthority uri)
  in prq{prqReq=rq{rqURI=uri{uriAuthority=Just auth{uriRegName=value}}}}
processRequestVariable "SERVER_PORT" value prq =
  let rq   = prqReq prq
      uri  = rqURI rq
      auth = fromMaybe nullURIAuth (uriAuthority uri)
  in case value of
       "80"  -> prq{prqReq=rq{rqURI=uri{uriScheme="http:"}}}
       "443" -> prq{prqReq=rq{rqURI=uri{uriScheme="https:"}}}
       _     -> prq{prqReq=rq{rqURI=uri{uriAuthority=Just auth{uriPort=':':value}}}}
processRequestVariable "QUERY_STRING" value prq =
  prq
processRequestVariable name value prq =
  case variableToHeaderName name of
    Nothing     -> prq{prqEnv=(name,value) : prqEnv prq}
    Just header -> prq{prqReq=insertHeader header value (prqReq prq)}

recvRecord :: FastCGIState -> IO (Maybe Record)
recvRecord state = do
  byteString <- recvAll (fcgiSocket state) 8
  case BS.length byteString of
    8 -> do
      let recordVersion = BS.index byteString 0
          recordTypeCode = fromIntegral $ BS.index byteString 1
          recordRequestIDB1 = BS.index byteString 2
          recordRequestIDB0 = BS.index byteString 3
          recordRequestID = (fromIntegral recordRequestIDB1) * 256
                            + (fromIntegral recordRequestIDB0)
          recordContentLengthB1 = BS.index byteString 4
          recordContentLengthB0 = BS.index byteString 5
          recordContentLength = (fromIntegral recordContentLengthB1) * 256
                                + (fromIntegral recordContentLengthB0)
          recordPaddingLength = BS.index byteString 6
      if recordVersion /= 1
        then return Nothing
        else do
          let recordType = toEnum recordTypeCode
          recordContent <- recvAll (fcgiSocket state) recordContentLength
          recvAll (fcgiSocket state) (fromIntegral recordPaddingLength)
          return (Just (Record {
                          recordType = recordType,
                          recordRequestID = recordRequestID,
                          recordContent = recordContent
                        }))
    _ -> return Nothing


sendRecord :: FastCGIState -> Record -> IO ()
sendRecord state record = do
  let recordRequestIDB0 = fromIntegral $ recordRequestID record `mod` 256
      recordRequestIDB1 = fromIntegral $ (recordRequestID record `div` 256) `mod` 256
      recordContentLength = BS.length $ recordContent record
      recordContentLengthB0 = fromIntegral $ recordContentLength `mod` 256
      recordContentLengthB1 = fromIntegral $ (recordContentLength `div` 256) `mod` 256
      headerByteString = BS.pack [1,
                                  fromIntegral $ fromEnum $ recordType record,
                                  recordRequestIDB1,
                                  recordRequestIDB0,
                                  recordContentLengthB1,
                                  recordContentLengthB0,
                                  0,
                                  0]
      byteString = BS.append headerByteString $ recordContent record
  Socket.sendAll (fcgiSocket state) byteString

recvAll :: Socket.Socket -> Int -> IO BS.ByteString
recvAll socket totalSize = do
  if totalSize == 0
    then return BS.empty
    else do
      byteString <- Socket.recv socket totalSize
      case BS.length byteString of
        0 -> return byteString
        receivedSize | receivedSize == totalSize -> return byteString
                     | otherwise                 -> do
                         restByteString
                            <- recvAll socket (totalSize - receivedSize)
                         return (BS.append byteString restByteString)


takeLength :: BS.ByteString -> Maybe (Int, BS.ByteString)
takeLength byteString
    = if BS.length byteString < 1
        then Nothing
        else let firstByte = BS.index byteString 0
                 threeMoreComing = (firstByte .&. 0x80) == 0x80
             in if threeMoreComing
                  then if BS.length byteString < 4
                         then Nothing
                         else let secondByte = BS.index byteString 1
                                  thirdByte = BS.index byteString 2
                                  fourthByte = BS.index byteString 3
                                  decoded = ((fromIntegral $ firstByte .&. 0x7F)
                                             `shiftL` 24)
                                            + (fromIntegral secondByte `shiftL` 16)
                                            + (fromIntegral thirdByte `shiftL` 8)
                                            + (fromIntegral fourthByte)
                              in Just (decoded, BS.drop 4 byteString)
                  else Just (fromIntegral firstByte, BS.drop 1 byteString)

takeNameValuePair :: BS.ByteString
                  -> Maybe ((BS.ByteString, BS.ByteString), BS.ByteString)
takeNameValuePair byteString
    = let maybeNameLength = takeLength byteString
      in case maybeNameLength of
           Nothing -> Nothing
           Just (nameLength, byteString')
             -> let maybeValueLength = takeLength byteString'
                in case maybeValueLength of
                     Nothing -> Nothing
                     Just (valueLength, byteString'')
                       -> let name = BS.take nameLength byteString''
                              byteString''' = BS.drop nameLength byteString''
                              value = BS.take valueLength byteString'''
                              byteString'''' = BS.drop valueLength byteString'''
                          in Just ((name, value), byteString'''')

-- | Logs a message using the web server's logging facility.
fLog :: FastCGIState -> Int -> String -> IO ()
fLog state requestID message
  | length message > 0 =
      sendRecord state $ Record {
        recordType = StderrRecord,
        recordRequestID = requestID,
        recordContent = BSC.pack message
      }
  | otherwise = return ()


-- | Return the document root, as provided by the web server, if it was provided.
getDocumentRoot :: Env -> Maybe String
getDocumentRoot = lookup "DOCUMENT_ROOT"


-- | Return the gateway interface, as provided by the web server, if it was provided.
getGatewayInterface :: Env -> Maybe String
getGatewayInterface = lookup "GATEWAY_INTERFACE"


-- | Return the path info, as provided by the web server, if it was provided.
getPathInfo :: Env -> Maybe String
getPathInfo = lookup "PATH_INFO"

-- | Return the path-translated value, as provided by the web server, if it was provided.
getPathTranslated :: Env -> Maybe String
getPathTranslated = lookup "PATH_TRANSLATED"


-- | Return the query string, as provided by the web server, if it was provided.
getQueryString :: Env -> Maybe String
getQueryString = lookup "QUERY_STRING"


-- | Return the redirect status, as provided by the web server, if it was provided.
getRedirectStatus :: Env -> Maybe Int
getRedirectStatus env = lookup "REDIRECT_STATUS" env >>= parseInt


-- | Return the redirect URI, as provided by the web server, if it was provided.
getRedirectURI :: Env -> Maybe String
getRedirectURI = lookup "REDIRECT_URI"


-- | Return the remote address, as provided by the web server, if it was provided.
getRemoteAddress :: Env -> Maybe String
getRemoteAddress = lookup "REMOTE_ADDR"


-- | Return the remote port, as provided by the web server, if it was provided.
getRemotePort :: Env -> Maybe Int
getRemotePort env = lookup "REMOTE_PORT" env >>= parseInt


-- | Return the remote hostname, as provided by the web server, if it was provided.
getRemoteHost :: Env -> Maybe String
getRemoteHost = lookup "REMOTE_HOST"


-- | Return the remote ident value, as provided by the web server, if it was provided.
getRemoteIdent :: Env -> Maybe String
getRemoteIdent = lookup "REMOTE_IDENT"


-- | Return the remote user name, as provided by the web server, if it was provided.
getRemoteUser :: Env -> Maybe String
getRemoteUser = lookup "REMOTE_USER"


-- | Return the script filename, as provided by the web server, if it was provided.
getScriptFilename :: Env -> Maybe String
getScriptFilename = lookup "SCRIPT_FILENAME"


-- | Return the script name, as provided by the web server, if it was provided.
getScriptName :: Env -> Maybe String
getScriptName = lookup "SCRIPT_NAME"


-- | Return the server address, as provided by the web server, if it was provided.
getServerAddress :: Env -> Maybe String
getServerAddress = lookup "SERVER_ADDR"


-- | Return the server name, as provided by the web server, if it was provided.
getServerName :: Env -> Maybe String
getServerName = lookup "SERVER_NAME"


-- | Return the server port, as provided by the web server, if it was provided.
getServerPort :: Env -> Maybe Int
getServerPort env = lookup "SERVER_PORT" env >>= parseInt


-- | Return the server protocol, as provided by the web server, if it was provided.
getServerProtocol :: Env -> Maybe String
getServerProtocol = lookup "SERVER_PROTOCOL"


-- | Return the server software name and version, as provided by the web server, if
--   it was provided.
getServerSoftware :: Env -> Maybe String
getServerSoftware = lookup "SERVER_SOFTWARE"


-- | Return the authentication type, as provided by the web server, if it was provided.
getAuthenticationType :: Env -> Maybe String
getAuthenticationType = lookup "AUTH_TYPE"


sendBuffer :: FastCGIState -> Int -> BS.ByteString -> IO ()
sendBuffer state requestID buffer = do
  let len = BS.length buffer
      lengthThisRecord = min len 0xFFFF
      bufferThisRecord = BS.take lengthThisRecord buffer
      bufferRemaining  = BS.drop lengthThisRecord buffer
  if lengthThisRecord > 0
    then sendRecord state $ Record {
           recordType = StdoutRecord,
           recordRequestID = requestID,
           recordContent = bufferThisRecord
         }
    else return ()
  if len > lengthThisRecord
    then sendBuffer state requestID bufferRemaining
    else return ()

sendBody :: FastCGIState -> Int -> LBS.ByteString -> IO ()
sendBody state requestID lbs =
  mapM_ send (LBS.toChunks lbs)
  where
    send bs =
      sendRecord state $ Record {
         recordType = StdoutRecord,
         recordRequestID = requestID,
         recordContent = bs
      }

sendResponse :: FastCGIState -> Int -> Response -> IO ()
sendResponse state requestID rsp = do
  enc <- getEncoding (rspHeaders rsp)
  bs <- encodeString enc (rspBody rsp)
  let rsp' = normalizeResponse (Just (LBS.length bs)) rsp
      status  = show (rspCode rsp')
      headers = rspHeaders rsp'
      nameValuePairs = ("Status", status) :
                       map (\(Header hdr val) -> (show hdr,val))
                           headers
      bytestrings
          = map (\(name, value) -> BSC.pack $ name ++ ": " ++ value ++ "\r\n")
                nameValuePairs
            ++ [BSC.pack "\r\n"]
      buffer = foldl BS.append BS.empty bytestrings
  sendBuffer state requestID buffer
  sendBody state requestID bs
