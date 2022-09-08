{-# LANGUAGE CPP, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Base
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Krasimir Angelov <kr.angelov@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Definitions of @Request@ and @Response@ types along with functions
-- for normalizing them. It is assumed to be an internal module; user
-- code should, if possible, import @Network.HTTP@ to access the functionality
-- that this module provides.
--
-- Additionally, the module exports internal functions for working with URLs,
-- and for handling the processing of requests and responses coming back.
--
-----------------------------------------------------------------------------
module Network.HTTP.Base
       (
          -- ** Constants
         httpVersion                 -- :: String

          -- ** HTTP
       , Request(..)
       , Response(..)
       , RequestMethod(..)

       , parseResponseHead
       , parseRequestHead
       , parseRequestMethod
       
       , HttpError(..)

       , ResponseNextStep(..)
       , matchResponse
       , ResponseData
       , ResponseCode
       , RequestData

       , NormalizeRequestOptions(..)
       , defaultNormalizeRequestOptions -- :: NormalizeRequestOptions ty
       , RequestNormalizer

       , normalizeRequest   -- :: NormalizeRequestOptions ty -> Request ty -> Request ty

       , splitRequestURI

       , getAuth
       , uriAuthPort
       , findConnClose

       , rqQuery, Query(..)

       , defaultGETRequest
       , mkRequest
       , setRequestBody

       , defaultUserAgent
       , defaultServer
       , httpPackageVersion

       , getRequestVersion
       , getResponseVersion
       , setRequestVersion
       , setResponseVersion

       , getSSLContext
       
       , handleErrors

       ) where

import Network.URI
   ( URI(uriAuthority, uriPath, uriScheme)
   , URIAuth(URIAuth, uriUserInfo, uriRegName, uriPort)
   , parseURIReference, uriAuthToString
   )

import Control.Monad ( guard )
import Control.Exception ( SomeException )

import Data.Word     ( Word8 )
import Data.Char     ( chr, digitToInt, intToDigit, toLower, isDigit, isHexDigit )
import Data.List     ( find )
import Data.Maybe    ( listToMaybe, fromMaybe )

import Network.URI ( uriQuery )
import Network.HTTP.Headers
import Network.HTTP.Cookie ( renderCookies )
import Network.HTTP.Utils ( trim, crlf, sp, 
                            HttpError(..), readsOne )
import qualified Network.HTTP.Base64 as Base64 (encode)

import Text.Read.Lex (readDecP)
import Text.ParserCombinators.ReadP
   ( ReadP, readP_to_S, char, (<++), look, munch, munch1, sepBy )

import qualified Paths_http_slim as Self (version)
import Data.Version (showVersion)

import qualified OpenSSL.Session as SSL

-----------------------------------------------------------------
------------------ URI Authority parsing ------------------------
-----------------------------------------------------------------

-- | Parse the authority part of a URL.
--
-- > RFC 1732, section 3.1:
-- >
-- >       //<user>:<password>@<host>:<port>/<url-path>
-- >  Some or all of the parts "<user>:<password>@", ":<password>",
-- >  ":<port>", and "/<url-path>" may be excluded.
parseURIAuthority :: String -> Maybe URIAuth
parseURIAuthority s = listToMaybe (map fst (readP_to_S pURIAuthority s))


pURIAuthority :: ReadP URIAuth
pURIAuthority = do
                u <- pUserInfo `before` char '@'
                h <- rfc2732host <++ munch (/=':')
                p <- (char ':' >> fmap (\(p :: Int) -> ':':show p) readDecP) <++ return ""
                look >>= guard . null
                return URIAuth{ uriUserInfo=u, uriRegName=h, uriPort=p }

-- RFC2732 adds support for '[literal-ipv6-address]' in the host part of a URL
rfc2732host :: ReadP String
rfc2732host = do
    _ <- char '['
    res <- munch1 (/=']')
    _ <- char ']'
    return res

pUserInfo :: ReadP String
pUserInfo = munch (/='@')

before :: Monad m => m a -> m b -> m a
before a b = a >>= \x -> b >> return x

-----------------------------------------------------------------
------------------ HTTP Messages --------------------------------
-----------------------------------------------------------------


-- Protocol version
httpVersion :: String
httpVersion = "HTTP/1.1"


-- | The HTTP request method, to be used in the 'Request' object.
-- We are missing a few of the stranger methods, but these are
-- not really necessary until we add full TLS.
data RequestMethod = HEAD | PUT | GET | POST | DELETE | OPTIONS | TRACE | CONNECT | Custom String
    deriving(Eq)

instance Show RequestMethod where
  show x =
    case x of
      HEAD     -> "HEAD"
      PUT      -> "PUT"
      GET      -> "GET"
      POST     -> "POST"
      DELETE   -> "DELETE"
      OPTIONS  -> "OPTIONS"
      TRACE    -> "TRACE"
      CONNECT  -> "CONNECT"
      Custom c -> c

parseRequestMethod :: String -> RequestMethod
parseRequestMethod s = fromMaybe (Custom s) (lookup s rqMethodMap)

rqMethodMap :: [(String, RequestMethod)]
rqMethodMap = [("HEAD",    HEAD),
               ("PUT",     PUT),
               ("GET",     GET),
               ("POST",    POST),
               ("DELETE",  DELETE),
               ("OPTIONS", OPTIONS),
               ("TRACE",   TRACE),
               ("CONNECT", CONNECT)]

-- | An HTTP Request.
-- The 'Show' instance of this type is used for message serialisation,
-- which means no body data is output.
data Request =
     Request { rqURI       :: URI   -- ^ might need changing in future
                                    --  1) to support '*' uri in OPTIONS request
                                    --  2) transparent support for both relative
                                    --     & absolute uris, although this should
                                    --     already work (leave scheme & host parts empty).
             , rqMethod    :: RequestMethod
             , rqHeaders   :: [Header]
             , rqBody      :: String
             }

-- Notice that request body is not included,
-- this show function is used to serialise
-- a request for the transport link, we send
-- the body separately where possible.
instance Show Request where
    show req@(Request u m h _) =
        show m ++ sp ++ alt_uri ++ sp ++ ver ++ crlf
        ++ foldr (++) [] (map show (dropHttpVersion h)) ++ crlf
        where
            ver = fromMaybe httpVersion (getRequestVersion req)
            alt_uri = show $ if null (uriPath u) || head (uriPath u) /= '/'
                        then u { uriPath = '/' : uriPath u }
                        else u

instance HasHeaders Request where
    getHeaders = rqHeaders
    setHeaders rq hdrs = rq { rqHeaders=hdrs }

    getCookies rq = snd (headersToCookies "" HdrCookie (rqHeaders rq))
    setCookies rq cookies = replaceHeader HdrCookie (renderCookies cookies) rq

-- | For easy pattern matching, HTTP response codes @xyz@ are
-- represented as @(x,y,z)@.
type ResponseCode  = Int

-- | @ResponseData@ contains the head of a response payload;
-- HTTP response code, accompanying text description + header
-- fields.
type ResponseData  = (ResponseCode,String,[Header])

-- | @RequestData@ contains the head of a HTTP request; method,
-- its URL along with the auxiliary/supporting header data.
type RequestData   = (RequestMethod,URI,[Header])

-- | An HTTP Response.
-- The 'Show' instance of this type is used for message serialisation,
-- which means no body data is output, additionally the output will
-- show an HTTP version of 1.1 instead of the actual version returned
-- by a server.
data Response =
    Response { rspCode     :: ResponseCode
             , rspReason   :: String
             , rspHeaders  :: [Header]
             , rspBody     :: String
             }

-- This is an invalid representation of a received response,
-- since we have made the assumption that all responses are HTTP/1.1
instance Show Response where
  show rsp@(Response code reason headers _) =
    ver ++ ' ' : show code ++ ' ' : reason ++ crlf ++
    foldr (++) [] (map show (dropHttpVersion headers)) ++ crlf
    where
      ver = fromMaybe httpVersion (getResponseVersion rsp)

instance HasHeaders Response where
  getHeaders = rspHeaders
  setHeaders rsp hdrs = rsp { rspHeaders=hdrs }

  getCookies rsp = snd (headersToCookies "" HdrSetCookie (rspHeaders rsp))
  setCookies rsp cookies = replaceHeader HdrCookie (renderCookies cookies) rsp


------------------------------------------------------------------
------------------ Request Building ------------------------------
------------------------------------------------------------------

-- | A default user agent string. The string is @\"haskell-http-slim/$version\"@
-- where @$version@ is the version of this HTTP package.
--
defaultUserAgent :: String
defaultUserAgent = "haskell-http-slim/" ++ httpPackageVersion

-- | The version of this HTTP package as a string, e.g. @\"4000.1.2\"@. This
-- may be useful to include in a user agent string so that you can determine
-- from server logs what version of this package HTTP clients are using.
-- This can be useful for tracking down HTTP compatibility quirks.
--
httpPackageVersion :: String
httpPackageVersion = showVersion Self.version

defaultGETRequest :: URI -> Request
defaultGETRequest uri = mkRequest GET uri

-- | 'mkRequest method uri' constructs a well formed
-- request for the given HTTP method and URI. It does not
-- normalize the URI for the request _nor_ add the required
-- Host: header. That is done either explicitly by the user
-- or when requests are normalized prior to transmission.
mkRequest :: RequestMethod -> URI -> Request
mkRequest meth uri = req
 where
  req =
    Request { rqURI      = uri
            , rqBody     = ""
            , rqHeaders  = [ Header HdrContentLength "0"
                           , Header HdrUserAgent     defaultUserAgent
                           ]
            , rqMethod   = meth
            }

type Query = [(String,String)]

-- | Decode application/x-www-form-urlencoded
rqQuery :: Request -> Query
rqQuery rq =
  case [q | (q,"") <- qparse] of
    [q] -> q
    _   -> []
  where
    qparse =
      case rqMethod rq of
        POST -> readP_to_S pQuery (rqBody rq)
        _    -> readP_to_S (char '?' >> pQuery) (uriQuery (rqURI rq))

    pQuery = sepBy param (char '&')
    param = do
      var <- munch (\c -> c /= '=' && c /= '&')
      char '='
      val <- munch (\c -> c /= '&')
      return (decode var,decode val)

    -- | Decode "+" and hexadecimal escapes
    decode [] = []
    decode ('%':'u':d1:d2:d3:d4:cs)
      | all isHexDigit [d1,d2,d3,d4] = chr(fromhex4 d1 d2 d3 d4):decode cs
    decode ('%':d1:d2:cs)
      | all isHexDigit [d1,d2] = chr(fromhex2 d1 d2):decode cs
    decode ('+':cs) = ' ':decode cs
    decode (c:cs) = c:decode cs

    fromhex4 d1 d2 d3 d4 = 256*fromhex2 d1 d2+fromhex2 d3 d4
    fromhex2 d1 d2 = 16*digitToInt d1+digitToInt d2


-- set rqBody, Content-Type and Content-Length headers.
setRequestBody :: Request -> (String, String) -> Request
setRequestBody req (typ, body) = req' { rqBody=body }
  where
    req' = replaceHeader HdrContentType typ .
           replaceHeader HdrContentLength (show (length body)) $
           req

-----------------------------------------------------------------
------------------ Parsing --------------------------------------
-----------------------------------------------------------------

-- Parsing a request
parseRequestHead :: [String] -> Either HttpError RequestData
parseRequestHead []     = Left ErrorClosed
parseRequestHead (s:ss) = do
  (version,rqm,uri) <- parseCommand s (words s)
  hdrs              <- parseHeaders ss
  return (rqm,uri,withVersion version hdrs)
  where
    parseCommand l (rqm:uri:version:_) =
      case (parseURIReference uri, lookup rqm rqMethodMap) of
        (Just u, Just r ) -> return (version,r,u)
        (Just u, Nothing) -> return (version,Custom rqm,u)
        _                 -> parse_err l
    parseCommand l _
      | null l    = Left ErrorClosed
      | otherwise = parse_err l

    parse_err l = Left (ErrorParse ("Request command line parse failure: " ++ l))


-- Parsing a response
parseResponseHead :: [String] -> Either HttpError ResponseData
parseResponseHead []         = Left ErrorClosed
parseResponseHead (sts:hdrs) = do
  (version,code,reason)  <- responseStatus sts (words sts)
  hdrs'                  <- parseHeaders hdrs
  return (code,reason,withVersion version hdrs')
 where
  responseStatus _l _yes@(version:code:reason) =
    return (version,match code,concatMap (++" ") reason)
  responseStatus l _no
    | null l    = Left ErrorClosed  -- an assumption
    | otherwise = parse_err l

  parse_err l = Left (ErrorParse ("Response status line parse failure: " ++ l))

  match s =
    case reads s of
      [(code,"")] -> code
      _           -> -1 -- will create appropriate behaviour

-- To avoid changing the @RequestData@ and @ResponseData@ types
-- just for this (and the upstream backwards compat. woes that
-- will result in), encode version info as a custom header.
-- Used by 'parseResponseData' and 'parseRequestData'.
--
-- Note: the Request and Response types do not currently represent
-- the version info explicitly in their record types. You have to use
-- {get,set}{Request,Response}Version for that.
withVersion :: String -> [Header] -> [Header]
withVersion v hs
 | v == httpVersion = hs  -- don't bother adding it if the default.
 | otherwise        = (Header (HdrCustom "X-HTTP-Version") v) : hs

-- | @getRequestVersion req@ returns the HTTP protocol version of
-- the request @req@. If @Nothing@, the default 'httpVersion' can be assumed.
getRequestVersion :: Request -> Maybe String
getRequestVersion r = getHttpVersion r

-- | @setRequestVersion v req@ returns a new request, identical to
-- @req@, but with its HTTP version set to @v@.
setRequestVersion :: String -> Request -> Request
setRequestVersion s r = setHttpVersion r s


-- | @getResponseVersion rsp@ returns the HTTP protocol version of
-- the response @rsp@. If @Nothing@, the default 'httpVersion' can be
-- assumed.
getResponseVersion :: Response -> Maybe String
getResponseVersion r = getHttpVersion r

-- | @setResponseVersion v rsp@ returns a new response, identical to
-- @rsp@, but with its HTTP version set to @v@.
setResponseVersion :: String -> Response -> Response
setResponseVersion s r = setHttpVersion r s

-- internal functions for accessing HTTP-version info in
-- requests and responses. Not exported as it exposes ho
-- version info is represented internally.

getHttpVersion :: HasHeaders a => a -> Maybe String
getHttpVersion r =
  fmap toVersion      $
   find isHttpVersion $
    getHeaders r
 where
  toVersion (Header _ x) = x

setHttpVersion :: HasHeaders a => a -> String -> a
setHttpVersion r v =
  setHeaders r $
   withVersion v  $
    dropHttpVersion $
     getHeaders r

dropHttpVersion :: [Header] -> [Header]
dropHttpVersion hs = filter (not.isHttpVersion) hs

isHttpVersion :: Header -> Bool
isHttpVersion (Header (HdrCustom "X-HTTP-Version") _) = True
isHttpVersion _ = False

-----------------------------------------------------------------
------------------ HTTP Send / Recv ----------------------------------
-----------------------------------------------------------------

data ResponseNextStep
 = Continue
 | Retry
 | Done
 | ExpectEntity
 | DieHorribly String

matchResponse :: RequestMethod -> ResponseCode -> ResponseNextStep
matchResponse rqst code =
    case code of
      100 -> Continue
      101 -> Done        -- upgrade to TLS
      _ | code > 101 && code < 200 -> Continue    -- default
      204 -> Done
      205 -> Done
      304 -> Done
      305 -> Done
      417 -> Retry       -- Expectation failed
      _ | code >= 200 && code < 600 -> ans
      _   -> DieHorribly ("Response code " ++ show code ++ " not recognised")
    where
      ans | rqst == HEAD = Done
          | otherwise    = ExpectEntity


-----------------------------------------------------------------
------------------ A little friendly funtionality ---------------
-----------------------------------------------------------------

-- | @getAuth req@ fishes out the authority portion of the URL in a request's @Host@
-- header.
#if MIN_VERSION_base(4,13,0)
getAuth :: MonadFail m => Request -> m URIAuth
#else
getAuth :: Monad m => Request -> m URIAuth
#endif
getAuth r = 
  case findHeader HdrHost r of
    Just val -> case parseURIAuthority val of
                  Just x -> return x
                  Nothing -> fail $ "Network.HTTP.Base.getAuth: Error parsing URI authority '" ++ val ++ "'"
    Nothing  -> case uriAuthority (rqURI r) of
                  Just auth -> return auth
                  Nothing   -> fail $ "Network.HTTP.Base.getAuth: No authority"

-- | @NormalizeRequestOptions@ brings together the various defaulting\/normalization options
-- over 'Request's. Use 'defaultNormalizeRequestOptions' for the standard selection of option
data NormalizeRequestOptions
 = NormalizeRequestOptions
     { normDoClose   :: Bool
     , normForProxy  :: Bool
     , normUserAgent :: Maybe String
     , normCustoms   :: [RequestNormalizer]
     }

-- | @RequestNormalizer@ is the shape of a (pure) function that rewrites
-- a request into some normalized form.
type RequestNormalizer = NormalizeRequestOptions -> Request -> Request

defaultNormalizeRequestOptions :: NormalizeRequestOptions
defaultNormalizeRequestOptions = NormalizeRequestOptions
     { normDoClose   = False
     , normForProxy  = False
     , normUserAgent = Just defaultUserAgent
     , normCustoms   = []
     }

-- | @normalizeRequest opts req@ is the entry point to use to normalize your
-- request prior to transmission (or other use.) Normalization is controlled
-- via the @NormalizeRequestOptions@ record.
normalizeRequest :: NormalizeRequestOptions
                 -> Request
                 -> Request
normalizeRequest opts req = foldr (\ f -> f opts) req normalizers
 where
  --normalizers :: [RequestNormalizer ty]
  normalizers =
     ( normalizeHostURI
     : normalizeBasicAuth
     : normalizeConnectionClose
     : normalizeUserAgent
     : normCustoms opts
     )

-- | @normalizeUserAgent ua x req@ augments the request @req@ with
-- a @User-Agent: ua@ header if @req@ doesn't already have a
-- a @User-Agent:@ set.
normalizeUserAgent :: RequestNormalizer
normalizeUserAgent opts req =
  case normUserAgent opts of
    Nothing -> req
    Just ua ->
     case findHeader HdrUserAgent req of
       Just u  | u /= defaultUserAgent -> req
       _ -> replaceHeader HdrUserAgent ua req

-- | @normalizeConnectionClose opts req@ sets the header @Connection: close@
-- to indicate one-shot behavior iff @normDoClose@ is @True@. i.e., it then
-- _replaces_ any an existing @Connection:@ header in @req@.
normalizeConnectionClose :: RequestNormalizer
normalizeConnectionClose opts req
 | normDoClose opts = replaceHeader HdrConnection "close" req
 | otherwise        = req

-- | @normalizeBasicAuth opts req@ sets the header @Authorization: Basic...@
-- if the "user:pass@" part is present in the "http://user:pass@host/path"
-- of the URI. If Authorization header was present already it is not replaced.
normalizeBasicAuth :: RequestNormalizer
normalizeBasicAuth _ req =
  case getAuth req of
    Just uriauth ->
      case uriUserInfo uriauth of
        "" -> req
        u  ->
          insertHeaderIfMissing HdrAuthorization astr req
            where
              astr = "Basic " ++ base64encode u
              base64encode = Base64.encode . stringToOctets :: String -> String
              stringToOctets = map (fromIntegral . fromEnum) :: String -> [Word8]
    Nothing ->req

-- | @normalizeHostURI forProxy req@ rewrites your request to have it
-- follow the expected formats by the receiving party (proxy or server.)
--
normalizeHostURI :: RequestNormalizer
normalizeHostURI opts req =
  case splitRequestURI uri of
    ("",_uri_abs)
      | forProxy ->
         case findHeader HdrHost req of
           Nothing -> req -- no host/authority in sight..not much we can do.
           Just h  -> req{rqURI=uri{ uriAuthority=Just URIAuth{uriUserInfo="", uriRegName=hst, uriPort=pNum}
                                   , uriScheme=if null (uriScheme uri) then "http" else uriScheme uri
                                   }}
            where
              hst = case span (/='@') user_hst of
                       (as,'@':bs) ->
                          case span (/=':') as of
                            (_,_:_) -> bs
                            _ -> user_hst
                       _ -> user_hst

              (user_hst, pNum) =
                 case span isDigit (reverse h) of
                   (ds,':':bs) -> (reverse bs, ':':reverse ds)
                   _ -> (h,"")
      | otherwise ->
         case findHeader HdrHost req of
           Nothing -> req -- no host/authority in sight..not much we can do...complain?
           Just{}  -> req
    (h,uri_abs)
      | forProxy  -> insertHeaderIfMissing HdrHost h req
      | otherwise -> replaceHeader HdrHost h req{rqURI=uri_abs} -- Note: _not_ stubbing out user:pass
 where
   uri0     = rqURI req
     -- stub out the user:pass
   uri      = uri0{uriAuthority=fmap (\ x -> x{uriUserInfo=""}) (uriAuthority uri0)}

   forProxy = normForProxy opts

{- Comments re: above rewriting:
    RFC 2616, section 5.1.2:
     "The most common form of Request-URI is that used to identify a
      resource on an origin server or gateway. In this case the absolute
      path of the URI MUST be transmitted (see section 3.2.1, abs_path) as
      the Request-URI, and the network location of the URI (authority) MUST
      be transmitted in a Host header field."
   We assume that this is the case, so we take the host name from
   the Host header if there is one, otherwise from the request-URI.
   Then we make the request-URI an abs_path and make sure that there
   is a Host header.
-}

splitRequestURI :: URI -> ({-authority-}String, URI)
splitRequestURI uri = (drop 2 (uriAuthToString id (uriAuthority uri) ""), uri{uriScheme="", uriAuthority=Nothing})

-- Looks for a "Connection" header with the value "close".
-- Returns True when this is found.
findConnClose :: [Header] -> Bool
findConnClose hdrs =
  maybe False
        (\ x -> map toLower (trim x) == "close")
        (lookupHeader HdrConnection hdrs)

uriAuthPort :: Maybe URI -> URIAuth -> Int
uriAuthPort mbURI auth =
  case uriPort auth of
    (':':s) -> readsOne id (default_port mbURI) s
    _       -> default_port mbURI
 where
  default_port Nothing = default_http
  default_port (Just url) =
    case map toLower $ uriScheme url of
      "http:" -> default_http
      "https:" -> default_https
        -- todo: refine
      _ -> default_http

  default_http  = 80
  default_https = 443

getSSLContext :: URI -> IO (Maybe SSL.SSLContext)
getSSLContext uri =
  case map toLower $ uriScheme uri of
    "http:"  -> return Nothing
    "https:" -> do ctxt <- SSL.context
                   SSL.contextSetCiphers ctxt "DEFAULT"
                   SSL.contextSetVerificationMode ctxt SSL.VerifyNone
                   return (Just ctxt)
    _        -> return Nothing

-- | A default server string. The string is @\"haskell-http-slim/$version\"@
-- where @$version@ is the version of this HTTP package.
--
defaultServer :: String
defaultServer = "haskell-http-slim/" ++ httpPackageVersion

handleErrors :: (String -> IO ()) -> SomeException -> IO Response
handleErrors logIt e = do
  logIt (show e)
  return (Response
            { rspCode = 500
            , rspReason = "Internal Server Error"
            , rspHeaders = []
            , rspBody = ""
            })
