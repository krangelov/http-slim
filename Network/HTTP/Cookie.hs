-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Cookie
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Krasimir Angelov <kr.angelov@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- This module provides the data types and functions for working with HTTP cookies.
-- Right now, it contains mostly functionality needed by 'Network.Browser'.
--
-----------------------------------------------------------------------------
module Network.HTTP.Cookie
       ( Cookie(..)
       , mkSimpleCookie, mkCookie
       , cookieMatch          -- :: (String,String) -> Cookie -> Bool

          -- functions for translating cookies and headers.
       , renderSetCookie, renderCookie
       , parseSetCookie,  parseCookie
       ) where

import Data.Char
import Data.List
import Data.Maybe

import Text.ParserCombinators.Parsec
   ( Parser, char, many, many1, satisfy, parse, option, try
   , (<|>), sepBy1
   )
import Network.HTTP.Utils ( parseInt )

------------------------------------------------------------------
----------------------- Cookie Stuff -----------------------------
------------------------------------------------------------------

-- | @Cookie@ is the Haskell representation of HTTP cookie values.
-- See its relevant specs for authoritative details.
data Cookie
 = MkCookie
    { ckDomain  :: String         -- Maybe String
    , ckName    :: String
    , ckValue   :: String
    , ckPath    :: Maybe String
    , ckComment :: Maybe String
    , ckVersion :: Maybe String
    , ckMaxAge  :: Maybe Int
    , ckSecure  :: Bool
    }
    deriving(Show,Read)

-- | Constructs a cookie with the given name and value.  Version is set to 1;
--   path, domain, and maximum age are set to @Nothing@; and the secure flag is
--   set to @False@.  Constructing the cookie does not cause it to be set; to do
--   that, call 'setCookie' on it.
mkSimpleCookie
    :: String -- ^ The name of the cookie to construct.
    -> String -- ^ The value of the cookie to construct.
    -> Cookie -- ^ A cookie with the given name and value.
mkSimpleCookie name value = MkCookie {
                              ckName = name,
                              ckValue = value,
                              ckVersion = Just "1",
                              ckPath = Nothing,
                              ckDomain = "",
                              ckMaxAge = Nothing,
                              ckSecure = False,
                              ckComment = Nothing
                            }


-- | Constructs a cookie with the given parameters.  Version is set to 1.
--   Constructing the cookie does not cause it to be set; to do that, call 'setCookie'
--   on it.
mkCookie
    :: String -- ^ The name of the cookie to construct.
    -> String -- ^ The value of the cookie to construct.
    -> (Maybe String) -- ^ The path of the cookie to construct.
    -> String -- ^ The domain of the cookie to construct.
    -> (Maybe Int) -- ^ The maximum age of the cookie to construct, in seconds.
    -> Bool -- ^ Whether to flag the cookie to construct as secure.
    -> Cookie -- ^ A cookie with the given parameters.
mkCookie name value maybePath domain maybeMaxAge secure
    = MkCookie {
        ckName = name,
        ckValue = value,
        ckVersion = Just "1",
        ckPath = maybePath,
        ckDomain = domain,
        ckMaxAge = maybeMaxAge,
        ckSecure = secure,
        ckComment = Nothing
      }

instance Eq Cookie where
    a == b  =  ckDomain a == ckDomain b
            && ckName a == ckName b
            && ckPath a == ckPath b

-- | Turn a list of cookies into a key=value pair list, separated by
-- commas.
renderSetCookie :: Cookie -> String
renderSetCookie = intercalate ";" . map printNameValuePair . nameValuePairs
  where
    printNameValuePair (name, Nothing   ) = name
    printNameValuePair (name, Just value) = name ++ "=" ++ value
    
    nameValuePairs cookie = [(ckName cookie, Just (ckValue cookie))]
                            ++ (case ckComment cookie of
                                  Nothing -> []
                                  Just comment -> [("Comment", Just comment)])
                            ++ (case ckDomain cookie of
                                  ""     -> []
                                  domain -> [("Domain", Just domain)])
                            ++ (case ckMaxAge cookie of
                                  Nothing -> []
                                  Just maxAge -> [("Max-Age", Just (show maxAge))])
                            ++ (case ckPath cookie of
                                  Nothing -> []
                                  Just path -> [("Path", Just path)])
                            ++ (case ckSecure cookie of
                                  False -> []
                                  True -> [("Secure", Nothing)])
                            ++ (case ckVersion cookie of
                                  Nothing      -> []
                                  Just version -> [("Version", Just version)])

-- | Turn a list of cookies into a key=value pair list, separated by
-- commas.
renderCookie :: [Cookie] -> String
renderCookie = intercalate ";" . map printNameValuePair
  where
    printNameValuePair cookie = ckName cookie ++ "=" ++ ckValue cookie

-- | @cookieMatch (domain,path) ck@ performs the standard cookie
-- match wrt the given domain and path.
cookieMatch :: (String, String) -> Cookie -> Bool
cookieMatch (dom,path) ck =
 ckDomain ck `isSuffixOf` dom &&
 case ckPath ck of
   Nothing -> True
   Just p  -> p `isPrefixOf` path


-- | parse a SetCookie header into a list of cookies and errors
parseSetCookie :: String -> ([String], [Cookie]) -> ([String], [Cookie])
parseSetCookie val (accErr, accCookie) =
  case parse cookie "" val of
    Left{}  -> (val:accErr, accCookie)
    Right x -> (accErr, x : accCookie)
  where
   cookie :: Parser Cookie
   cookie =
       do name <- word
          _    <- spaces_l
          _    <- char '='
          _    <- spaces_l
          val1 <- cvalue
          args <- cdetail
          return (MkCookie
                    { ckName    = name
                    , ckValue   = val1
                    , ckDomain  = map toLower (fromMaybe "" (lookup "domain" args))
                    , ckPath    = lookup "path"    args
                    , ckVersion = lookup "version" args
                    , ckComment = lookup "comment" args
                    , ckMaxAge  = case lookup "max-age" args of
                                    Nothing -> Nothing
                                    Just s  -> parseInt s
                    , ckSecure  = isJust (lookup "secure" args)
                    })

   -- all keys in the result list MUST be in lower case
   cdetail :: Parser [(String,String)]
   cdetail = many $
       try (do _  <- spaces_l
               _  <- char ';'
               _  <- spaces_l
               s1 <- word
               _  <- spaces_l
               s2 <- option "" (char '=' >> spaces_l >> cvalue)
               return (map toLower s1,s2)
           )

-- | parse a Cookie header into a cookie
parseCookie :: String -> String -> Maybe [Cookie]
parseCookie dom val =
  case parse cookies "" val of
    Left{}  -> Nothing
    Right x -> Just x
  where
   cookies :: Parser [Cookie]
   cookies = sepBy1 cookie (char ';')

   cookie :: Parser Cookie
   cookie =
       do name <- word
          _    <- spaces_l
          _    <- char '='
          _    <- spaces_l
          val1 <- cvalue
          return (MkCookie
                    { ckName    = name
                    , ckValue   = val1
                    , ckDomain  = dom
                    , ckPath    = Nothing
                    , ckVersion = Nothing
                    , ckComment = Nothing
                    , ckMaxAge  = Nothing
                    , ckSecure  = False
                    })

cvalue :: Parser String
cvalue = quotedstring <|> many1 (satisfy $ not . (==';')) <|> return ""

spaces_l :: Parser String
spaces_l = many (satisfy isSpace)

word, quotedstring :: Parser String
quotedstring =
    do _   <- char '"'  -- "
       str <- many (satisfy $ not . (=='"'))
       _   <- char '"'
       return str

word = many1 (satisfy (\x -> isAlphaNum x || x=='_' || x=='.' || x=='-' || x==':'))
