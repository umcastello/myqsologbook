-- | Grid Locator Maidenhead, distância de grande círculo e azimute.
-- |
-- | O código anterior usava @Data.Array.!!@ (parcial) para fatiar o grid: um
-- | grid de 4 caracteres ou um caractere fora do alfabeto derrubava o
-- | processo em runtime. Aqui o parsing é total e devolve um ADT de erro com
-- | posição e tipo de caractere inválido.
module Data.Maidenhead
  ( -- * Coordenadas
    Coordinates
  , parseCoordinates
  , formatCoordinates

    -- * Parsing do grid
  , GridError(..)
  , GridCharError(..)
  , parseGrid
  , isValidGrid
  , gridPrecision
  , normalizeGrid

    -- * Conversão
  , gridToCoordinates
  , coordinatesToGrid

    -- * Geometria
  , earthRadiusKm
  , greatCircleDistanceKm
  , initialBearingDeg
  , DistanceAndBearing
  , distanceAndBearing

    -- * Formatação
  , formatDistanceKm
  , formatBearing
  ) where

import Prelude

import Data.Array (head) as Array
import Data.Array (replicate, uncons) as Array
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Int (fromNumber, toNumber) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (atan2, cos, floor, max, min, pi, remainder, sin, sqrt)
import Data.String (Pattern(..), split, toUpper, trim)
import Data.String.CodeUnits (charAt, drop, fromCharArray, length, singleton)

-- | Coordenadas geográficas em graus decimais.
type Coordinates =
  { lat :: Number
  , lon :: Number
  }

-- | Aceita @"-23.5505, -46.6333"@ e valida os limites do planeta.
parseCoordinates :: String -> Either String Coordinates
parseCoordinates raw =
  case split (Pattern ",") (trim raw) of
    [latS, lonS] -> do
      lat <- readDecimal "latitude" (trim latS)
      lon <- readDecimal "longitude" (trim lonS)
      if lat < -90.0 || lat > 90.0
        then Left "Latitude fora do intervalo -90..90"
        else if lon < -180.0 || lon > 180.0
          then Left "Longitude fora do intervalo -180..180"
          else Right { lat: lat, lon: lon }
    _ -> Left "Informe \"latitude, longitude\" (ex: -23.5505, -46.6333)"
  where
  readDecimal label s = case readNumber s of
    Just n  -> Right n
    Nothing -> Left ("Número inválido em " <> label <> ": " <> s)

-- | Parser decimal minimalista, total, sem @Partial@ e sem FFI.
readNumber :: String -> Maybe Number
readNumber raw =
  let s = trim raw
      n = length s
      firstChar = charAt 0 s
      negative = firstChar == Just '-'
      body = if negative || firstChar == Just '+' then dropFirst s else s
      go i acc seenDot =
        if i >= n
          then if n == 0 then Nothing else Just acc
          else case charAt i body of
            Just '.' | not seenDot -> go (i + 1) acc true
            Just d | isDigit d -> go (i + 1) (acc * 10.0 + Int.toNumber (toCharCode d - charCode0)) seenDot
            _ -> Nothing
  in case go 0 0.0 false of
    Just v  -> Just (if negative then -v else v)
    Nothing -> Nothing
  where
  isDigit c = c >= '0' && c <= '9'
  charCode0 = toCharCode '0'
  dropFirst = drop 1

-- | Erro de caractere dentro do grid, com a classe esperada.
data GridCharError
  = NotField Char      -- ^ Esperado A..R
  | NotSquare Char     -- ^ Esperado 0..9
  | NotSubsquare Char  -- ^ Esperado A..X
  | NotExtended Char   -- ^ Esperado 0..9 (grid de 8 caracteres)

-- | Erro de parsing do grid, sempre com a posição (base 0) do problema.
data GridError
  = GridLengthError Int
  | GridCharError Int GridCharError

derive instance eqGridCharError :: Eq GridCharError

derive instance eqGridError :: Eq GridError

instance showGridCharError :: Show GridCharError where
  show (NotField c)     = "esperado campo A-R, veio '" <> show c <> "'"
  show (NotSquare c)    = "esperado quadrado 0-9, veio '" <> show c <> "'"
  show (NotSubsquare c) = "esperado subquadrado A-X, veio '" <> show c <> "'"
  show (NotExtended c)  = "esperado dígito 0-9, veio '" <> show c <> "'"

instance showGridError :: Show GridError where
  show (GridLengthError n) =
    "grid locator deve ter 4, 6 ou 8 caracteres (tem " <> show n <> ")"
  show (GridCharError i e) =
    "caractere inválido na posição " <> show (i + 1) <> ": " <> show e

-- | Precisão do grid em caracteres (4, 6 ou 8).
gridPrecision :: String -> Maybe Int
gridPrecision s = case length s of
  4 -> Just 4
  6 -> Just 6
  8 -> Just 8
  _ -> Nothing

-- | @Right@ com o grid normalizado (maiúsculas, sem espaços).
parseGrid :: String -> Either GridError String
parseGrid raw =
  let g = toUpper (trim raw)
      n = length g
      field i    = check i g 'A' 'R' NotField
      square i   = check i g '0' '9' NotSquare
      subsquare i = check i g 'A' 'X' NotSubsquare
      extended i  = check i g '0' '9' NotExtended
  in case n of
    4 -> do
      _ <- field 0
      _ <- field 1
      _ <- square 2
      _ <- square 3
      pure g
    6 -> do
      _ <- field 0
      _ <- field 1
      _ <- square 2
      _ <- square 3
      _ <- subsquare 4
      _ <- subsquare 5
      pure g
    8 -> do
      _ <- field 0
      _ <- field 1
      _ <- square 2
      _ <- square 3
      _ <- subsquare 4
      _ <- subsquare 5
      _ <- extended 6
      _ <- extended 7
      pure g
    _ -> Left (GridLengthError n)
  where
  check i g lo hi mk =
    case charAt i g of
      Just c
        | c >= lo && c <= hi -> Right c
        | otherwise -> Left (GridCharError i (mk c))
      Nothing -> Left (GridLengthError (length g))

isValidGrid :: String -> Boolean
isValidGrid s = case parseGrid s of
  Right _ -> true
  Left _  -> false

-- | Maiúsculas, sem espaços nas pontas.
normalizeGrid :: String -> String
normalizeGrid = compose toUpper trim

-- | Centro do quadrado do grid, em graus.
gridToCoordinates :: String -> Either GridError Coordinates
gridToCoordinates raw = do
  g <- parseGrid raw
  let n = length g
      -- | Índice do caractere na posição @i@, relativo a @base@; 0 se o
      -- | grid for curto demais (4 caracteres) ou o caractere não existir.
      o i base = fromNumber' (fromMaybe 0 (charOffset base (charAt i g)))
      lonDeg =
        (o 0 'A' * 20.0)
          + (o 2 '0' * 2.0)
          + (o 4 'A' * (2.0 / 24.0))
          + (o 6 '0' * (2.0 / 240.0))
      latDeg =
        (o 1 'A' * 10.0)
          + (o 3 '0' * 1.0)
          + (o 5 'A' * (1.0 / 24.0))
          + (o 7 '0' * (1.0 / 240.0))
  pure
    { lon: lonDeg - 180.0 + halfLon n
    , lat: latDeg - 90.0 + halfLat n
    }
  where
  fromNumber' i = Int.toNumber i
  halfLon n = case n of
    4 -> 1.0
    6 -> 2.0 / 48.0
    _ -> 2.0 / 480.0
  halfLat n = case n of
    4 -> 0.5
    6 -> 1.0 / 48.0
    _ -> 1.0 / 480.0

charOffset :: Char -> Maybe Char -> Maybe Int
charOffset base m = case m of
  Just c  -> Just (toCharCode c - toCharCode base)
  Nothing -> Nothing

-- | Grid de 6 caracteres cujo centro contém as coordenadas.
coordinatesToGrid :: Coordinates -> Either String String
coordinatesToGrid coords =
  let lon = max (-180.0) (min 180.0 coords.lon)
      lat = max (-90.0) (min 90.0 coords.lat)
      lonAdj = lon + 180.0
      latAdj = lat + 90.0
      lonField = min 17.0 (floor (lonAdj / 20.0))
      latField = min 17.0 (floor (latAdj / 10.0))
      lonRest = lonAdj - lonField * 20.0
      latRest = latAdj - latField * 10.0
      lonSquare = floor (lonRest / 2.0)
      latSquare = floor latRest
      lonSub = floor ((lonRest - lonSquare * 2.0) * 12.0)
      latSub = floor ((latRest - latSquare) * 24.0)
  in if lonSub > 23.0 || latSub > 23.0 || lonSquare > 9.0 || latSquare > 9.0
    then Left "Coordenadas fora da grade Maidenhead"
    else Right $
      fieldChar lonField <> fieldChar latField
      <> digitChar lonSquare <> digitChar latSquare
      <> fieldChar lonSub <> fieldChar latSub
  where
  fieldChar i = singleton (fromMaybe 'A' (fromCharCode (toCharCode 'A' + toIndex i)))
  digitChar i = singleton (fromMaybe '0' (fromCharCode (toCharCode '0' + toIndex i)))
  toIndex n = clamp 0 23 (fromMaybe 0 (Int.fromNumber n))
  clamp lo hi v = if v < lo then lo else if v > hi then hi else v

-- | Raio médio da Terra em quilômetros.
earthRadiusKm :: Number
earthRadiusKm = 6371.0

toRadians :: Number -> Number
toRadians deg = deg * pi / 180.0

toDegrees :: Number -> Number
toDegrees rad = rad * 180.0 / pi

-- | Distância de grande círculo (Haversine) em quilômetros.
greatCircleDistanceKm :: Coordinates -> Coordinates -> Number
greatCircleDistanceKm origin dest =
  let lat1 = toRadians origin.lat
      lon1 = toRadians origin.lon
      lat2 = toRadians dest.lat
      lon2 = toRadians dest.lon
      dLat = lat2 - lat1
      dLon = lon2 - lon1
      a = sin (dLat / 2.0) * sin (dLat / 2.0) +
          cos lat1 * cos lat2 * sin (dLon / 2.0) * sin (dLon / 2.0)
      c = 2.0 * atan2 (sqrt a) (sqrt (1.0 - a))
  in earthRadiusKm * c

-- | Azimute inicial de @origin@ para @dest@, em graus de 0 a 360.
initialBearingDeg :: Coordinates -> Coordinates -> Number
initialBearingDeg origin dest =
  let lat1 = toRadians origin.lat
      lon1 = toRadians origin.lon
      lat2 = toRadians dest.lat
      lon2 = toRadians dest.lon
      dLon = lon2 - lon1
      y = sin dLon * cos lat2
      x = cos lat1 * sin lat2 - sin lat1 * cos lat2 * cos dLon
      deg = toDegrees (atan2 y x)
  in if deg < 0.0 then deg + 360.0 else deg

type DistanceAndBearing =
  { distanceKm :: Number
  , azimuthDeg :: Number
  }

distanceAndBearing :: Coordinates -> Coordinates -> DistanceAndBearing
distanceAndBearing origin dest =
  { distanceKm: greatCircleDistanceKm origin dest
  , azimuthDeg: initialBearingDeg origin dest
  }

-- | Legível: metros abaixo de 1 km, quilômetros com 1 casa até 10 km.
formatDistanceKm :: Number -> String
formatDistanceKm km
  | km < 0.0 = "-" <> formatDistanceKm (0.0 - km)
  | km < 1.0 = show (roundInt (km * 1000.0)) <> " m"
  | km < 10.0 = showDecimal 1 km <> " km"
  | otherwise = show (roundInt km) <> " km"

-- | Azimute em rosa dos ventos de 16 pontos.
formatBearing :: Number -> String
formatBearing deg =
  let norm = normalizeAngle deg
      idx = clampInt 0 15 (roundInt (floor ((norm + 11.25) / 22.5)))
  in fromMaybe "?" (atMay idx compassPoints) <> " (" <> show (roundInt deg) <> "°)"

compassPoints :: Array String
compassPoints =
  [ "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE"
  , "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"
  ]

normalizeAngle :: Number -> Number
normalizeAngle deg =
  let r = remainder deg 360.0
  in if r < 0.0 then r + 360.0 else r

-- | @"-23.5505, -46.6333"@
formatCoordinates :: Coordinates -> String
formatCoordinates coords =
  showDecimal 4 coords.lat <> ", " <> showDecimal 4 coords.lon

-- | Arredonda @x@ para o inteiro mais próximo, com @.5@ indo para cima.
-- | Só faz sentido com @digits = 0@; para casas decimais use @showDecimal@.
roundInt :: Number -> Int
roundInt x = fromMaybe 0 (Int.fromNumber (floor (x + 0.5)))

-- | Escala @x@ por @10^digits@ e arredonda, devolvendo o inteiro
-- | correspondente. É o passo que @showDecimal@ usa para separar a parte
-- | inteira da fracionária.
scaleTo :: Int -> Number -> Int
scaleTo digits x =
  let factor = pow10 digits
  in fromMaybe 0 (Int.fromNumber (floor (x * factor + 0.5)))

-- | Formata @x@ com exatamente @digits@ casas decimais (sem separador de
-- | milhar, para não depender de locale).
showDecimal :: Int -> Number -> String
showDecimal digits x =
  let neg = x < 0.0
      abs' = if neg then 0.0 - x else x
      scale = ipow10 digits
      scaled = scaleTo digits abs'
      intPart = scaled `div` scale
      fracPart = scaled - intPart * scale
      fracStr = padLeft digits (show fracPart)
      sign' = if neg then "-" else ""
  in sign' <> show intPart <> (if digits == 0 then "" else "." <> fracStr)

padLeft :: Int -> String -> String
padLeft n s =
  let len = length s
  in if len >= n then s else replicateString (n - len) '0' <> s

replicateString :: Int -> Char -> String
replicateString n c = fromCharArray (Array.replicate n c)

pow10 :: Int -> Number
pow10 n = Int.toNumber (ipow10 n)

ipow10 :: Int -> Int
ipow10 = go 1
  where
  go acc 0 = acc
  go acc k = go (acc * 10) (k - 1)

atMay :: forall a. Int -> Array a -> Maybe a
atMay i arr
  | i <= 0 = Array.head arr
  | otherwise = case Array.uncons arr of
    Nothing -> Nothing
    Just r  -> atMay (i - 1) r.tail

clampInt :: Int -> Int -> Int -> Int
clampInt lo hi v = if v < lo then lo else if v > hi then hi else v
