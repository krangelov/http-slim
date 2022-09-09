-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Utils
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Krasimir Angelov <kr.angelov@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Set of utility functions and definitions used by package modules.
--
module Network.HTTP.Utils
       ( trim     -- :: String -> String
       , trimL    -- :: String -> String
       , trimR    -- :: String -> String
       , parseInt

       , crlf     -- :: String
       , lf       -- :: String
       , sp       -- :: String

       , split    -- :: Eq a => a -> [a] -> Maybe ([a],[a])
       , splitBy  -- :: Eq a => a -> [a] -> [[a]]

       , readsOne -- :: Read a => (a -> b) -> b -> String -> b

       , dropWhileTail -- :: (a -> Bool) -> [a] -> [a]
       , chopAtDelim   -- :: Eq a => a -> [a] -> ([a],[a])

       , decodeString
       , encodeString

       , HttpError(..)
       ) where

import Data.Char
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe )
import Control.Exception
import GHC.IO.Buffer
import GHC.IO.Encoding ( TextEncoding(..), latin1, mkTextEncoding, encode )
import qualified GHC.IO.Encoding as Enc
import qualified Data.ByteString.Internal as BS ( ByteString(..) )
import qualified Data.ByteString.Lazy as LBS
import Foreign ( peekArray, pokeElemOff )

-- | @crlf@ is our beloved two-char line terminator.
crlf :: String
crlf = "\r\n"

-- | @lf@ is a tolerated line terminator, per RFC 2616 section 19.3.
lf :: String
lf = "\n"

-- | @sp@ lets you save typing one character.
sp :: String
sp   = " "

-- | @split delim ls@ splits a list into two parts, the @delim@ occurring
-- at the head of the second list.  If @delim@ isn't in @ls@, @Nothing@ is
-- returned.
split :: Eq a => a -> [a] -> Maybe ([a],[a])
split delim list = case delim `elemIndex` list of
    Nothing -> Nothing
    Just x  -> Just $ splitAt x list

-- | @trim str@ removes leading and trailing whitespace from @str@.
trim :: String -> String
trim xs = trimR (trimL xs)

-- | @trimL str@ removes leading whitespace (as defined by 'Data.Char.isSpace')
-- from @str@.
trimL :: String -> String
trimL xs = dropWhile isSpace xs

-- | @trimL str@ removes trailing whitespace (as defined by 'Data.Char.isSpace')
-- from @str@.
trimR :: String -> String
trimR str = fromMaybe "" $ foldr trimIt Nothing str
 where
  trimIt x (Just xs) = Just (x:xs)
  trimIt x Nothing
   | isSpace x = Nothing
   | otherwise = Just [x]

-- | @splitMany delim ls@ removes the delimiter @delim@ from @ls@.
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c xs =
    case break (==c) xs of
      (_,[]) -> [xs]
      (as,_:bs) -> as : splitBy c bs

-- | @readsOne f def str@ tries to 'read' @str@, taking
-- the first result and passing it to @f@. If the 'read'
-- doesn't succeed, return @def@.
readsOne :: Read a => (a -> b) -> b -> String -> b
readsOne f n str =
 case reads str of
   ((v,_):_) -> f v
   _ -> n


-- | @dropWhileTail p ls@ chops off trailing elements from @ls@
-- until @p@ returns @False@.
dropWhileTail :: (a -> Bool) -> [a] -> [a]
dropWhileTail f ls =
 case foldr chop Nothing ls of { Just xs -> xs; Nothing -> [] }
  where
    chop x (Just xs) = Just (x:xs)
    chop x _
     | f x       = Nothing
     | otherwise = Just [x]

-- | @chopAtDelim elt ls@ breaks up @ls@ into two at first occurrence
-- of @elt@; @elt@ is elided too. If @elt@ does not occur, the second
-- list is empty and the first is equal to @ls@.
chopAtDelim :: Eq a => a -> [a] -> ([a],[a])
chopAtDelim elt xs =
  case break (==elt) xs of
    (_,[])    -> (xs,[])
    (as,_:bs) -> (as,bs)

data HttpError
 = ErrorReset
 | ErrorClosed
 | ErrorParse String
 | ErrorMisc Int String String
   deriving(Show,Eq)

instance Exception HttpError

parseInt :: String -> Maybe Int
parseInt string =
  case reads string of
    [(n,"")] -> Just n
    _        -> Nothing


encodeString :: TextEncoding -> String -> IO LBS.ByteString
encodeString enc s = do
  cbuf <- newCharBuffer max_len ReadBuffer
  case enc of
    TextEncoding{mkTextEncoder=mkEncoder} ->
      bracket mkEncoder Enc.close $ \encoder -> do
        bss <- convert encoder cbuf s []
        return (LBS.fromChunks (reverse bss))
  where
    max_len = 256

    convert encoder cbuf cs bss
      | isEmptyBuffer cbuf && null cs = return bss
      | otherwise = do (cbuf,cs) <- pokeElems cbuf cs
                       bbuf <- newByteBuffer max_len WriteBuffer
                       (_,cbuf',bbuf') <- encode encoder cbuf bbuf
                       let bs = BS.PS (bufRaw bbuf')
                                      (bufL bbuf')
                                      (bufferElems bbuf')
                       convert encoder cbuf' cs (bs:bss)
      where
        pokeElems cbuf cs
          | null cs || isFullCharBuffer cbuf = return (cbuf,cs)
        pokeElems cbuf (c:cs)  = do
          withBuffer cbuf $ \ptr ->
            pokeElemOff ptr (bufR cbuf) c
          pokeElems (bufferAdd 1 cbuf) cs    

decodeString :: TextEncoding -> BS.ByteString -> IO String
decodeString enc bs = 
  case bs of
    BS.PS fptr offs len -> do
      let bbuf = Buffer {
                   bufRaw   = fptr,
                   bufState = ReadBuffer,
                   bufSize  = offs+len,
                   bufL = offs,
                   bufR = offs+len
                 }
      cbuf <- newCharBuffer len WriteBuffer
      case enc of
        TextEncoding{mkTextDecoder=mkDecoder} ->
          bracket mkDecoder Enc.close $ \decoder -> do
                  (_,bbuf_,cbuf_) <- encode decoder bbuf cbuf
                  withBuffer cbuf_ (peekArray (bufferElems cbuf_))
