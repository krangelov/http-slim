module Network.HTTP.MD5
   (md5ss,  md5si, md5bs,  md5bi) where

import Data.Char (ord, chr)
import Data.Bits (rotateL, shiftL, shiftR, (.&.), (.|.), xor, complement)
import Data.Word (Word32, Word64)
import qualified Data.ByteString.Lazy as BS
import GHC.Show
import System.IO
import System.IO.Unsafe ( unsafePerformIO )
import Network.HTTP.Utils
import Debug.Trace

-- ===================== TYPES AND CLASS DEFINTIONS ========================


type Rotation = Int
data XYZ = XYZ {-# UNPACK #-} !Word32
               {-# UNPACK #-} !Word32
               {-# UNPACK #-} !Word32
data ABCD = ABCD {-# UNPACK #-} !Word32
                 {-# UNPACK #-} !Word32
                 {-# UNPACK #-} !Word32
                 {-# UNPACK #-} !Word32
            deriving (Eq, Show)

addABCD :: ABCD -> ABCD -> ABCD
addABCD (ABCD a1 b1 c1 d1) (ABCD a2 b2 c2 d2) = ABCD (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

-- ===================== EXPORTED FUNCTIONS ========================

md5 :: BS.ByteString -> ABCD
md5 m = md5_main 0 magic_numbers m

-- | Encodes a string given the encoding and returns an MD5 hex number
-- à la md5sum program
md5ss :: TextEncoding -> String -> String
md5ss enc s = (abcd_to_string . md5) (unsafePerformIO (encodeString enc s))

-- | Returns an MD5 hex number of a byte string à la md5sum program
md5bs :: BS.ByteString -> String
md5bs = abcd_to_string . md5

-- | Encodes a string given the encoding and returns an MD5 hex number
-- à la md5sum program
md5si :: TextEncoding -> String -> Integer
md5si enc s = (abcd_to_integer . md5) (unsafePerformIO (encodeString enc s))

-- | Returns an MD5 hex number of a byte string à la md5sum program
md5bi :: BS.ByteString -> Integer
md5bi = abcd_to_integer . md5


-- ===================== THE CORE ALGORITHM ========================


-- Decides what to do. The first argument indicates if padding has been
-- added. The second is the length mod 2^64 so far. Then we have the
-- starting state, the rest of the string and the final state.

md5_main :: Word64        -- The length so far mod 2^64
         -> ABCD          -- The initial state
         -> BS.ByteString -- The non-processed portion of the message
         -> ABCD          -- The resulting state
md5_main ilen abcd bs
  | BS.null bs = abcd
  | otherwise  = md5_main (ilen + 512) (addABCD abcd abcd') bs'
  where
    (m16, bs') = get_next 16 bs
    abcd'      = md5_do_block abcd m16

    get_next 0 bs = ([],bs)
    get_next n bs
      | len == 4  =
          let (ws,bs'') = get_next (n-1) bs'
          in (w:ws,bs'')
      | otherwise =
          let w1 = shiftL 0x80 (len * 8) + w
              ws = replicate (fromIntegral zeros) 0++size
          in (w1:ws,BS.empty)
      where
        (s, bs') = BS.splitAt 4 bs
        len      = fromIntegral (BS.length s)
        w        = BS.foldr (\c w -> shiftL w 8 + fromIntegral c) 0 s

        c64  = ilen + 32 * (16 - n) + 8 * fromIntegral len
        c64' = ilen + 32 * (16 - n) + 32

        zeros = shiftR ((448 - c64') .&. 511) 5

        size = [ fromIntegral (c64 .&. 0xFFFFFFFF)
               , fromIntegral (shiftR c64 32)
               ]

-- md5_do_block processes a 512 bit block by calling md5_round 4 times to
-- apply each round with the correct constants and permutations of the
-- block

md5_do_block :: ABCD     -- Initial state
             -> [Word32] -- The block to be processed - 16 32bit words
             -> ABCD     -- Resulting state
md5_do_block abcd0 w = abcd4
 where
   (r1, r2, r3, r4) = rounds

   perm5 [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15] =
     [c1,c6,c11,c0,c5,c10,c15,c4,c9,c14,c3,c8,c13,c2,c7,c12]
   perm5 _ = error "broke at perm5"

   perm3 [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15] =
     [c5,c8,c11,c14,c1,c4,c7,c10,c13,c0,c3,c6,c9,c12,c15,c2]
   perm3 _ = error "broke at perm3"

   perm7 [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15] =
     [c0,c7,c14,c5,c12,c3,c10,c1,c8,c15,c6,c13,c4,c11,c2,c9]
   perm7 _ = error "broke at perm7"

   abcd1 = md5_round md5_f abcd0        w  r1
   abcd2 = md5_round md5_g abcd1 (perm5 w) r2
   abcd3 = md5_round md5_h abcd2 (perm3 w) r3
   abcd4 = md5_round md5_i abcd3 (perm7 w) r4


-- md5_round does one of the rounds. It takes an auxiliary function and foldls
-- (md5_inner_function f) to repeatedly apply it to the initial state with the
-- correct constants

md5_round :: (XYZ -> Word32)      -- Auxiliary function (F, G, H or I
                                  -- for those of you with a copy of
                                  -- the prayer book^W^WRFC)
          -> ABCD                 -- Initial state
          -> [Word32]             -- The 16 32bit words of input
          -> [(Rotation, Word32)] -- The list of 16 rotations and
                                  -- additive constants
          -> ABCD                 -- Resulting state
md5_round f abcd s ns = foldl (md5_inner_function f) abcd ns'
 where
   ns' = zipWith (\x (y, z) -> (y, x + z)) s ns


-- Apply one of the functions md5_[fghi] and put the new ABCD together

md5_inner_function :: (XYZ -> Word32)    -- Auxiliary function
                   -> ABCD               -- Initial state
                   -> (Rotation, Word32) -- The rotation and additive
                                         -- constant (X[i] + T[j])
                   -> ABCD               -- Resulting state
md5_inner_function f (ABCD a b c d) (s, ki) = ABCD d a' b c
 where
   mid_a = a + f (XYZ b c d) + ki
   rot_a = rotateL mid_a s
   a' = b + rot_a


-- The 4 auxiliary functions

md5_f :: XYZ -> Word32
md5_f (XYZ x y z) = z `xor` (x .&. (y `xor` z))
{- optimised version of: (x .&. y) .|. ((complement x) .&. z) -}

md5_g :: XYZ -> Word32
md5_g (XYZ x y z) = md5_f (XYZ z x y)
{- was: (x .&. z) .|. (y .&. (complement z)) -}

md5_h :: XYZ -> Word32
md5_h (XYZ x y z) = x `xor` y `xor` z

md5_i :: XYZ -> Word32
md5_i (XYZ x y z) = y `xor` (x .|. (complement z))


-- The magic numbers from the RFC.

magic_numbers :: ABCD
magic_numbers = ABCD 0x67452301 0xefcdab89 0x98badcfe 0x10325476


-- The 4 lists of (rotation, additive constant) tuples, one for each round

rounds :: ([(Rotation, Word32)],
           [(Rotation, Word32)],
           [(Rotation, Word32)],
           [(Rotation, Word32)])
rounds = (r1, r2, r3, r4)
 where r1 = [(s11, 0xd76aa478), (s12, 0xe8c7b756), (s13, 0x242070db),
             (s14, 0xc1bdceee), (s11, 0xf57c0faf), (s12, 0x4787c62a),
             (s13, 0xa8304613), (s14, 0xfd469501), (s11, 0x698098d8),
             (s12, 0x8b44f7af), (s13, 0xffff5bb1), (s14, 0x895cd7be),
             (s11, 0x6b901122), (s12, 0xfd987193), (s13, 0xa679438e),
             (s14, 0x49b40821)]
       r2 = [(s21, 0xf61e2562), (s22, 0xc040b340), (s23, 0x265e5a51),
             (s24, 0xe9b6c7aa), (s21, 0xd62f105d), (s22,  0x2441453),
             (s23, 0xd8a1e681), (s24, 0xe7d3fbc8), (s21, 0x21e1cde6),
             (s22, 0xc33707d6), (s23, 0xf4d50d87), (s24, 0x455a14ed),
             (s21, 0xa9e3e905), (s22, 0xfcefa3f8), (s23, 0x676f02d9),
             (s24, 0x8d2a4c8a)]
       r3 = [(s31, 0xfffa3942), (s32, 0x8771f681), (s33, 0x6d9d6122),
             (s34, 0xfde5380c), (s31, 0xa4beea44), (s32, 0x4bdecfa9),
             (s33, 0xf6bb4b60), (s34, 0xbebfbc70), (s31, 0x289b7ec6),
             (s32, 0xeaa127fa), (s33, 0xd4ef3085), (s34,  0x4881d05),
             (s31, 0xd9d4d039), (s32, 0xe6db99e5), (s33, 0x1fa27cf8),
             (s34, 0xc4ac5665)]
       r4 = [(s41, 0xf4292244), (s42, 0x432aff97), (s43, 0xab9423a7),
             (s44, 0xfc93a039), (s41, 0x655b59c3), (s42, 0x8f0ccc92),
             (s43, 0xffeff47d), (s44, 0x85845dd1), (s41, 0x6fa87e4f),
             (s42, 0xfe2ce6e0), (s43, 0xa3014314), (s44, 0x4e0811a1),
             (s41, 0xf7537e82), (s42, 0xbd3af235), (s43, 0x2ad7d2bb),
             (s44, 0xeb86d391)]
       s11 = 7
       s12 = 12
       s13 = 17
       s14 = 22
       s21 = 5
       s22 = 9
       s23 = 14
       s24 = 20
       s31 = 4
       s32 = 11
       s33 = 16
       s34 = 23
       s41 = 6
       s42 = 10
       s43 = 15
       s44 = 21

-- Turn the 4 32 bit words into a string representing the hex number they
-- represent.

abcd_to_string :: ABCD -> String
abcd_to_string (ABCD a b c d) =
  (show_hex a . show_hex b . show_hex c . show_hex d) ""
  where
    show_hex w = showIt (rev_num w) 8
      where
        showIt _ 0 r = r
        showIt n i r = c `seq` showIt n' (i-1) (c : r)
          where
            (n',d) = quotRem n 16
            c      = intToDigit (fromIntegral d)

-- Convert to an integer, performing endianness magic as we go

abcd_to_integer :: ABCD -> Integer
abcd_to_integer (ABCD a b c d)
  = toInteger (rev_num a) * 2^(96 :: Int)
  + toInteger (rev_num b) * 2^(64 :: Int)
  + toInteger (rev_num c) * 2^(32 :: Int)
  + toInteger (rev_num d)

rev_num :: Word32 -> Word32
rev_num i = j
  where
    j = shiftL (i .&. 0x000000FF) 24 .|.
        shiftL (i .&. 0x0000FF00)  8 .|.
        shiftR (i .&. 0x00FF0000)  8 .|.
        shiftR (i .&. 0xFF000000) 24
