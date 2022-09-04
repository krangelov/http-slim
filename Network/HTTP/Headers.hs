-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Headers
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Krasimir Angelov <kr.angelov@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- This module provides the data types for representing HTTP headers, and
-- operations for looking up header values and working with sequences of
-- header values in 'Request's and 'Response's. To avoid having to provide
-- separate set of operations for doing so, we introduce a type class 'HasHeaders'
-- to facilitate writing such processing using overloading instead.
--
-----------------------------------------------------------------------------
module Network.HTTP.Headers
   ( HasHeaders(..)     -- type class

   , Header(..)
   , mkHeader           -- :: HeaderName -> String -> Header
   , hdrName            -- :: Header     -> HeaderName
   , hdrValue           -- :: Header     -> String

   , HeaderName(..)

   , insertHeader          -- :: HasHeaders a => HeaderName -> String -> a -> a
   , insertHeaderIfMissing -- :: HasHeaders a => HeaderName -> String -> a -> a
   , insertHeaders         -- :: HasHeaders a => [Header] -> a -> a
   , retrieveHeaders       -- :: HasHeaders a => HeaderName -> a -> [Header]
   , replaceHeader         -- :: HasHeaders a => HeaderName -> String -> a -> a
   , findHeader            -- :: HasHeaders a => HeaderName -> a -> Maybe String
   , lookupHeader          -- :: HeaderName -> [Header] -> Maybe String

   , parseHeader           -- :: parseHeader :: String -> Result Header
   , parseHeaders          -- :: [String] -> Result [Header]
   , variableToHeaderName

   , Cookie(..), headersToCookies
   ) where

import Data.Char (toLower,toUpper)
import Network.HTTP.Cookie
import Network.HTTP.Utils (trim, split, crlf, Result, failParse)

-- | The @Header@ data type pairs header names & values.
data Header = Header HeaderName String

hdrName :: Header -> HeaderName
hdrName (Header h _) = h

hdrValue :: Header -> String
hdrValue (Header _ v) = v

-- | Header constructor as a function, hiding above rep.
mkHeader :: HeaderName -> String -> Header
mkHeader = Header

instance Show Header where
  show (Header key value) = shows key (':':' ':value ++ crlf)

-- | HTTP @HeaderName@ type, a Haskell data constructor for each
-- specification-defined header, prefixed with @Hdr@ and CamelCased,
-- (i.e., eliding the @-@ in the process.) Should you require using
-- a custom header, there's the @HdrCustom@ constructor which takes
-- a @String@ argument.
--
-- Encoding HTTP header names differently, as Strings perhaps, is an
-- equally fine choice..no decidedly clear winner, but let's stick
-- with data constructors here.
--
data HeaderName
    -- Generic Headers --
 = HdrCacheControl
 | HdrConnection
 | HdrDate
 | HdrPragma
 | HdrTransferEncoding
 | HdrUpgrade
 | HdrVia
    -- Request Headers --
 | HdrAccept
 | HdrAcceptCharset
 | HdrAcceptEncoding
 | HdrAcceptLanguage
 | HdrAuthorization
 | HdrCookie
 | HdrExpect
 | HdrFrom
 | HdrHost
 | HdrIfModifiedSince
 | HdrIfMatch
 | HdrIfNoneMatch
 | HdrIfRange
 | HdrIfUnmodifiedSince
 | HdrMaxForwards
 | HdrProxyAuthorization
 | HdrRange
 | HdrReferer
 | HdrUserAgent
    -- Response Headers
 | HdrAcceptRanges
 | HdrAge
 | HdrLocation
 | HdrProxyAuthenticate
 | HdrPublic
 | HdrRetryAfter
 | HdrServer
 | HdrSetCookie
 | HdrTE
 | HdrTrailer
 | HdrVary
 | HdrWarning
 | HdrWWWAuthenticate
    -- Entity Headers
 | HdrAllow
 | HdrContentBase
 | HdrContentEncoding
 | HdrContentLanguage
 | HdrContentLength
 | HdrContentLocation
 | HdrContentMD5
 | HdrContentRange
 | HdrContentType
 | HdrETag
 | HdrExpires
 | HdrLastModified
    -- | MIME entity headers (for sub-parts)
 | HdrContentTransferEncoding
    -- | Allows for unrecognised or experimental headers.
 | HdrCustom String -- not in header map below.
 | HdrExtensionHeader

instance Eq HeaderName where
    HdrCustom a                == HdrCustom b                = map toLower a == map toLower b
    HdrCacheControl            == HdrCacheControl            = True
    HdrConnection              == HdrConnection              = True
    HdrDate                    == HdrDate                    = True
    HdrPragma                  == HdrPragma                  = True
    HdrTransferEncoding        == HdrTransferEncoding        = True
    HdrUpgrade                 == HdrUpgrade                 = True
    HdrVia                     == HdrVia                     = True
    HdrAccept                  == HdrAccept                  = True
    HdrAcceptCharset           == HdrAcceptCharset           = True
    HdrAcceptEncoding          == HdrAcceptEncoding          = True
    HdrAcceptLanguage          == HdrAcceptLanguage          = True
    HdrAuthorization           == HdrAuthorization           = True
    HdrCookie                  == HdrCookie                  = True
    HdrExpect                  == HdrExpect                  = True
    HdrFrom                    == HdrFrom                    = True
    HdrHost                    == HdrHost                    = True
    HdrIfModifiedSince         == HdrIfModifiedSince         = True
    HdrIfMatch                 == HdrIfMatch                 = True
    HdrIfNoneMatch             == HdrIfNoneMatch             = True
    HdrIfRange                 == HdrIfRange                 = True
    HdrIfUnmodifiedSince       == HdrIfUnmodifiedSince       = True
    HdrMaxForwards             == HdrMaxForwards             = True
    HdrProxyAuthorization      == HdrProxyAuthorization      = True
    HdrRange                   == HdrRange                   = True
    HdrReferer                 == HdrReferer                 = True
    HdrUserAgent               == HdrUserAgent               = True
    HdrAcceptRanges            == HdrAcceptRanges            = True
    HdrAge                     == HdrAge                     = True
    HdrLocation                == HdrLocation                = True
    HdrProxyAuthenticate       == HdrProxyAuthenticate       = True
    HdrPublic                  == HdrPublic                  = True
    HdrRetryAfter              == HdrRetryAfter              = True
    HdrServer                  == HdrServer                  = True
    HdrSetCookie               == HdrSetCookie               = True
    HdrTE                      == HdrTE                      = True
    HdrTrailer                 == HdrTrailer                 = True
    HdrVary                    == HdrVary                    = True
    HdrWarning                 == HdrWarning                 = True
    HdrWWWAuthenticate         == HdrWWWAuthenticate         = True
    HdrAllow                   == HdrAllow                   = True
    HdrContentBase             == HdrContentBase             = True
    HdrContentEncoding         == HdrContentEncoding         = True
    HdrContentLanguage         == HdrContentLanguage         = True
    HdrContentLength           == HdrContentLength           = True
    HdrContentLocation         == HdrContentLocation         = True
    HdrContentMD5              == HdrContentMD5              = True
    HdrContentRange            == HdrContentRange            = True
    HdrContentType             == HdrContentType             = True
    HdrETag                    == HdrETag                    = True
    HdrExpires                 == HdrExpires                 = True
    HdrLastModified            == HdrLastModified            = True
    HdrContentTransferEncoding == HdrContentTransferEncoding = True
    _                          == _                          = False


-- | @headerMap@ is a straight assoc list for translating between 
-- header names, variable names and Haskell values.
headerMap :: [(String,String,HeaderName)]
headerMap =
   [ p "Cache-Control"             "HTTP_CACHE_CONTROL"             HdrCacheControl
   , p "Connection"                "HTTP_CONNECTION"                HdrConnection
   , p "Date"                      "HTTP_DATE"                      HdrDate
   , p "Pragma"                    "HTTP_PRAGMA"                    HdrPragma
   , p "Transfer-Encoding"         "HTTP_TRANSFER_ENCODING"         HdrTransferEncoding
   , p "Upgrade"                   "HTTP_UPGRADE"                   HdrUpgrade
   , p "Via"                       "HTTP_VIA"                       HdrVia
   , p "Accept"                    "HTTP_ACCEPT"                    HdrAccept
   , p "Accept-Charset"            "HTTP_ACCEPT_CHARSET"            HdrAcceptCharset
   , p "Accept-Encoding"           "HTTP_ACCEPT_ENCODING"           HdrAcceptEncoding
   , p "Accept-Language"           "HTTP_ACCEPT_LANGUAGE"           HdrAcceptLanguage
   , p "Authorization"             "HTTP_AUTHORIZATION"             HdrAuthorization
   , p "Cookie"                    "HTTP_COOKIE"                    HdrCookie
   , p "Expect"                    "HTTP_EXPECT"                    HdrExpect
   , p "From"                      "HTTP_FROM"                      HdrFrom
   , p "Host"                      "HTTP_HOST"                      HdrHost
   , p "If-Modified-Since"         "HTTP_IF_MODIFIED_SINCE"         HdrIfModifiedSince
   , p "If-Match"                  "HTTP_IF_MATCH"                  HdrIfMatch
   , p "If-None-Match"             "HTTP_IF_NONE_MATCH"             HdrIfNoneMatch
   , p "If-Range"                  "HTTP_IF_RANGE"                  HdrIfRange
   , p "If-Unmodified-Since"       "HTTP_IF_UNMODIFIED_SINCE"       HdrIfUnmodifiedSince
   , p "Max-Forwards"              "HTTP_MAX_FORWARDS"              HdrMaxForwards
   , p "Proxy-Authorization"       "HTTP_PROXY_AUTHORIZATION"       HdrProxyAuthorization
   , p "Range"                     "HTTP_RANGE"                     HdrRange
   , p "Referer"                   "HTTP_REFERER"                   HdrReferer
   , p "TE"                        "HTTP_TE"                        HdrTE
   , p "User-Agent"                "HTTP_USER_AGENT"                HdrUserAgent
   , p "Accept-Ranges"             "HTTP_ACCEPT_RANGES"             HdrAcceptRanges
   , p "Age"                       "HTTP_AGE"                       HdrAge
   , p "ETag"                      "HTTP_ETAG"                      HdrETag
   , p "Location"                  "HTTP_LOCATION"                  HdrLocation
   , p "Proxy-Authenticate"        "HTTP_PROXY_AUTHENTICATE"        HdrProxyAuthenticate
   , p "Public"                    "HTTP_PUBLIC"                    HdrPublic
   , p "Retry-After"               "HTTP_RETRY_AFTER"               HdrRetryAfter
   , p "Server"                    "HTTP_SERVER"                    HdrServer
   , p "Set-Cookie"                "HTTP_SET_COOKIE"                HdrSetCookie
   , p "Trailer"                   "HTTP_TRAILER"                   HdrTrailer
   , p "Vary"                      "HTTP_VARY"                      HdrVary
   , p "Warning"                   "HTTP_WARNING"                   HdrWarning
   , p "WWW-Authenticate"          "HTTP_WWW_AUTHENTICATE"          HdrWWWAuthenticate
   , p "Allow"                     "HTTP_ALLOW"                     HdrAllow
   , p "Content-Base"              "HTTP_CONTENT_BASE"              HdrContentBase
   , p "Content-Encoding"          "HTTP_CONTENT_ENCODING"          HdrContentEncoding
   , p "Content-Language"          "HTTP_CONTENT_LANGUAGE"          HdrContentLanguage
   , p "Content-Length"            "CONTENT_LENGTH"                 HdrContentLength
   , p "Content-Location"          "HTTP_CONTENT_LOCATION"          HdrContentLocation
   , p "Content-MD5"               "HTTP_CONTENT_MD5"               HdrContentMD5
   , p "Content-Range"             "HTTP_CONTENT_RANGE"             HdrContentRange
   , p "Content-Type"              "CONTENT_TYPE"                   HdrContentType
   , p "Expires"                   "HTTP_EXPIRES"                   HdrExpires
   , p "Last-Modified"             "HTTP_LAST_MODIFIED"             HdrLastModified
   , p "Content-Transfer-Encoding" "HTTP_CONTENT_TRANSFER_ENCODING" HdrContentTransferEncoding
   ]
 where
  p a b c = (a,b,c)

instance Show HeaderName where
    show (HdrCustom s) = s
    show x = case filter (\(_,_,y)->x==y) headerMap of
                []          -> error "headerMap incomplete"
                ((h,_,_):_) -> h

-- | @HasHeaders@ is a type class for types containing HTTP headers, allowing
-- you to write overloaded header manipulation functions
-- for both 'Request' and 'Response' data types, for instance.
class HasHeaders x where
  getHeaders :: x -> [Header]
  setHeaders :: x -> [Header] -> x

  getCookies :: x -> [Cookie]
  setCookies :: x -> [Cookie] -> x


-- | @insertHeader hdr x@ inserts a header. Does not check for 
-- existing headers with same name, allowing duplicates to be
-- introduced (use 'replaceHeader' if you want to avoid this.)
insertHeader :: HasHeaders a => HeaderName -> String -> a -> a
insertHeader name value x = setHeaders x (Header name value : getHeaders x)

-- | @insertHeaderIfMissing hdr val x@ adds the new header only if no previous
-- header with name @hdr@ exists in @x@.
insertHeaderIfMissing :: HasHeaders a => HeaderName -> String -> a -> a
insertHeaderIfMissing name value x = setHeaders x (update (getHeaders x))
  where
    update []     = [Header name value]
    update list@(h@(Header n _) : rest)
      | n == name = list
      | otherwise = h : update rest

-- | @replaceHeader hdr val o@ replaces the header @hdr@ with the
-- value @val@, dropping any existing
replaceHeader :: HasHeaders a => HeaderName -> String -> a -> a
replaceHeader name value x = setHeaders x (update (getHeaders x))
  where
    update []     = [Header name value]
    update (h@(Header n _) : rest)
      | n == name = update rest
      | otherwise = h : update rest

-- | @insertHeaders hdrs x@ appends multiple headers to @x@'s existing
-- set.
insertHeaders :: HasHeaders a => [Header] -> a -> a
insertHeaders hdrs x = setHeaders x (getHeaders x ++ hdrs)

-- | @retrieveHeaders hdrNm x@ gets a list of headers with 'HeaderName' @hdrNm@.
retrieveHeaders :: HasHeaders a => HeaderName -> a -> [Header]
retrieveHeaders name x = filter matchname (getHeaders x)
    where
        matchname (Header n _) = n == name

-- | @findHeader hdrNm x@ looks up @hdrNm@ in @x@, returning the first
-- header that matches, if any.
findHeader :: HasHeaders a => HeaderName -> a -> Maybe String
findHeader n x = lookupHeader n (getHeaders x)

-- | @lookupHeader hdr hdrs@ locates the first header matching @hdr@ in the
-- list @hdrs@.
lookupHeader :: HeaderName -> [Header] -> Maybe String
lookupHeader _ [] = Nothing
lookupHeader v (Header n s:t)
  |  v == n   =  Just s
  | otherwise =  lookupHeader v t

-- | @parseHeader headerNameAndValueString@ tries to unscramble a
-- @header: value@ pairing and returning it as a 'Header'.
parseHeader :: String -> Result Header
parseHeader str =
    case split ':' str of
      Nothing -> failParse ("Unable to parse header: " ++ str)
      Just (k,v) -> return $ Header (fn k) (trim $ drop 1 v)
    where
        fn x = case filter (\(y,_,_) -> match x y) headerMap of
                 []          -> HdrCustom x
                 ((_,_,h):_) -> h

        match :: String -> String -> Bool
        match s1 s2 = map toLower s1 == map toLower s2
        
variableToHeaderName :: String -> Maybe HeaderName
variableToHeaderName x =
  case filter (\(_,y,_) -> x==y) headerMap of
    []          -> variableToHeader x
    ((_,_,h):_) -> Just h
  where
    variableToHeader ('H':'T':'T':'P':'_':cs)
      | null cs        = Nothing
      | otherwise      = Just (HdrCustom (translate cs))
    variableToHeader _ = Nothing

    translate [] = []
    translate cs = case break (=='_') cs of
                     (first, '_':rest) -> titleCase first++'-':translate rest
                     _                 -> cs

    titleCase []     = []
    titleCase (c:cs) = toUpper c : map toLower cs


-- | @parseHeaders hdrs@ takes a sequence of strings holding header
-- information and parses them into a set of headers (preserving their
-- order in the input argument.) Handles header values split up over
-- multiple lines.
parseHeaders :: [String] -> Result [Header]
parseHeaders = catRslts [] .
                 map (parseHeader . clean) .
                     joinExtended ""
   where
        -- Joins consecutive lines where the second line
        -- begins with ' ' or '\t'.
        joinExtended old      [] = [old]
        joinExtended old (h : t)
          | isLineExtension h    = joinExtended (old ++ ' ' : tail h) t
          | otherwise            = old : joinExtended h t

        isLineExtension (x:_) = x == ' ' || x == '\t'
        isLineExtension _ = False

        clean [] = []
        clean (h:t) | h `elem` "\t\r\n" = ' ' : clean t
                    | otherwise = h : clean t

        -- tolerant of errors?  should parse
        -- errors here be reported or ignored?
        -- currently ignored.
        catRslts :: [a] -> [Result a] -> Result [a]
        catRslts list (h:t) =
            case h of
                Left _ -> catRslts list t
                Right v -> catRslts (v:list) t
        catRslts list [] = Right $ reverse list

-- | @processCookieHeaders dom hdrs@
headersToCookies :: String -> HeaderName -> [Header] -> ([String], [Cookie])
headersToCookies dom hdr hdrs =
  foldr (\(Header hdr' val) st ->
             if hdr == hdr'
               then parseCookies dom val st
               else st)
        ([],[])
        hdrs
