-- | Base64 puro, necessário para o header @Authorization: Basic@ do CouchDB.
-- |
-- | O driver HTTP do @affjax-node@ ignora o usuário/senha embutido na URL,
-- | então montamos o header à mão. A implementação é total e opera sobre
-- | code points (UTF-8) em vez de bytes soltos, para não quebrar senhas com
-- | acento. Só aritmética: nada de @Data.Bits@.
-- |
-- | Todas as operações aritméticas estão entre parênteses de propósito:
-- | em PureScript @+@ e @div@ têm a mesma precedência, e confiar na
-- | associatividadewnd à esquerda já produziu bugs de um caractere.
module Data.Base64
  ( encode
  , decode
  ) where

import Prelude

import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodePoints (CodePoint, toCodePointArray)
import Data.String.CodeUnits (charAt, fromCharArray, length, take) as String

-- | Codifica a string em Base64 com padding @=@.
encode :: String -> String
encode input =
  let bytes = Array.concatMap utf8Bytes (toCodePointArray input)
      pad = (3 - (Array.length bytes `mod` 3)) `mod` 3
      padded = bytes <> Array.replicate pad 0
      full = encodeGroups padded
  in String.take (String.length full - pad) full <> replicateChars pad '='

-- | Trios de bytes -> 4 caracteres, sempre grupos completos.
encodeGroups :: Array Int -> String
encodeGroups bytes = go 0
  where
  n = Array.length bytes
  byte i = fromMaybe 0 (atMay i bytes)
  char i = String.fromCharArray [fromMaybe '=' (String.charAt i alphabet)]
  go i
    | i + 2 < n =
        char (byte i `div` 4)
          <> char (((byte i `mod` 4) * 16) + (byte (i + 1) `div` 16))
          <> char (((byte (i + 1) `mod` 16) * 4) + (byte (i + 2) `div` 64))
          <> char (byte (i + 2) `mod` 64)
          <> go (i + 3)
    | i + 1 < n =
        char (byte i `div` 4)
          <> char (((byte i `mod` 4) * 16) + (byte (i + 1) `div` 16))
          <> go (i + 2)
    | i < n = char (byte i `div` 4)
    | otherwise = ""

-- | Decodifica Base64 aceitando quebras de linha MIME; ignora qualquer
-- | caractere fora do alfabeto (inclusive o @=@ de padding).
decode :: String -> Either String String
decode input =
  let values = map valueToIndex (Array.filter isBase64Value (map fromEnum (toCodePointArray input)))
  in if (Array.length values `mod` 4) == 1
    then Left "Base64 inválido: sobra de 1 caractere"
    else Right (utf8String (decodeGroups values))

isBase64Value :: Int -> Boolean
isBase64Value v = (v >= 0) && (v < 128) && (String.charAt (valueToIndex v) alphabet /= Nothing)

-- | Índice base64 (0..63) de um valor ASCII, ou -1 se não for do alfabeto.
valueToIndex :: Int -> Int
valueToIndex v =
  if (v >= 65) && (v <= 90) then v - 65
  else if (v >= 97) && (v <= 122) then (v - 97) + 26
  else if (v >= 48) && (v <= 57) then (v - 48) + 52
  else if v == 43 then 62
  else if v == 47 then 63
  else -1

decodeGroups :: Array Int -> Array Int
decodeGroups values = go 0
  where
  n = Array.length values
  value i = fromMaybe 0 (atMay i values)
  go i
    | i + 3 < n =
        [ (value i * 4) + (value (i + 1) `div` 16)
        , ((value (i + 1) `mod` 16) * 16) + (value (i + 2) `div` 4)
        , ((value (i + 2) `mod` 4) * 64) + value (i + 3)
        ] <> go (i + 4)
    | i + 2 < n =
        [ (value i * 4) + (value (i + 1) `div` 16)
        , ((value (i + 1) `mod` 16) * 16) + (value (i + 2) `div` 4)
        ] <> go (i + 3)
    | i + 1 < n = [(value i * 4) + (value (i + 1) `div` 16)] <> go (i + 2)
    | otherwise = []

-- | Code point -> bytes UTF-8. As sequências de continuação saem da direita
-- | para a esquerda, ou seja, @cont 0@ carrega sempre os 6 bits mais baixos.
utf8Bytes :: CodePoint -> Array Int
utf8Bytes cp =
  let n = fromEnum cp
      cont i = 0x80 + ((n `div` pow6 i) `mod` 64)
  in if n < 0x80 then [n]
     else if n < 0x800 then [0xC0 + (n `div` 64), cont 0]
     else if n < 0x10000 then [0xE0 + (n `div` 4096), cont 1, cont 0]
     else [0xF0 + (n `div` 262144), cont 2, cont 1, cont 0]

pow6 :: Int -> Int
pow6 = go 1
  where
  go acc 0 = acc
  go acc k = go (acc * 64) (k - 1)

-- | Bytes UTF-8 -> String. Code points acima de U+FFFF viram par de
-- | surrogates, como o JavaScript espera.
utf8String :: Array Int -> String
utf8String bytes = go 0
  where
  n = Array.length bytes
  byte i = fromMaybe 0 (atMay i bytes)
  cont i = byte i - 0x80
  ch i = fromMaybe replacementChar (fromCharCode i)
  char1 i = String.fromCharArray [ch i]
  cpChars cp
    | cp < 0x10000 = char1 cp
    | otherwise =
        let rest = cp - 0x10000
            high = 0xD800 + (rest `div` 0x400)
            low = 0xDC00 + (rest `mod` 0x400)
        in String.fromCharArray [ch high, ch low]
  go i
    | i >= n = ""
    | otherwise =
        let x = byte i
        in if x < 0x80
          then char1 x <> go (i + 1)
          else if x < 0xE0
            then cpChars (((x - 0xC0) * 0x40) + cont (i + 1)) <> go (i + 2)
            else if x < 0xF0
              then
                cpChars
                  ( ((x - 0xE0) * 0x1000)
                      + (cont (i + 1) * 0x40)
                      + cont (i + 2)
                  )
                  <> go (i + 3)
              else
                cpChars
                  ( ((x - 0xF0) * 0x40000)
                      + (cont (i + 1) * 0x1000)
                      + (cont (i + 2) * 0x40)
                      + cont (i + 3)
                  )
                  <> go (i + 4)

replacementChar :: Char
replacementChar = '�'

alphabet :: String
alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

replicateChars :: Int -> Char -> String
replicateChars n c = String.fromCharArray (Array.replicate n c)

atMay :: forall a. Int -> Array a -> Maybe a
atMay i arr = if i < 0 then Nothing else Array.head (Array.drop i arr)
