-- | Suíte de testes: a camada de dados (Base64 do header @Authorization@,
-- | Maidenhead, bandas/modos e codecs do CouchDB) e, em @Test.CLI@, a camada
-- | de linha de comando. Tudo puro, sem rede: rode @spago test@.
module Test.Main (main) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Base64 (decode, encode)
import Data.Either (Either(..), isLeft)
import Data.Maidenhead
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Number (abs)
import Data.QSO
import Data.QSO.Codec
import Data.Station
import Data.String.Pattern (Pattern(..))
import Data.String.CodeUnits (contains)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Test.CLI (cliTests)
import Test.Unit (Test, TestSuite, describe, it)
import Test.Unit.Assert (assert, assertFalse, equal)
import Test.Unit.Main (runTest)

-- | Grids usados nos testes de distância: ألمães e deceive britânica.
homeGrid :: String
homeGrid = "JN58TD"

dxGrid :: String
dxGrid = "IO91WM"

-- | QSO completo: exercita todos os campos opcionais do codec.
fullQso :: QSO
fullQso =
  { id: Just "qso:2026-09-29T12:00:00.000Z:PY2UQH"
  , rev: Just "1-abc"
  , callsign: "PY2UQH"
  , band: B20m
  , mode: SSB
  , rstSent: "59"
  , rstRcvd: "57"
  , grid: Just "GG66"
  , operatorName: Just "Ulisses"
  , qth: Just "Sao Paulo"
  , dxcc: Just "Brazil"
  , country: Just "Brasil"
  , notes: Just "primeiro contato"
  , qslStatus: Pending
  , timestamp: "2026-09-29T12:00:00.000Z"
  }

-- | @Right@ sem exploding: falhar o teste com a mensagem do @Left@.
expectRight :: forall a e. Show a => Show e => String -> Either e a -> Aff a
expectRight label result = case result of
  Right a  -> pure a
  Left err -> throwError (error (label <> ": " <> show err))

-- | @Json@ não tem @Show@, então o erro vem como texto.
expectJson :: String -> Either String Json -> Aff Json
expectJson label result = case result of
  Right j  -> pure j
  Left err -> throwError (error (label <> ": " <> err))

near :: Number -> Number -> Number -> Boolean
near eps expected actual = abs (actual - expected) <= eps

between :: Number -> Number -> Number -> Boolean
between lo hi v = (v >= lo) && (v <= hi)

main :: Effect Unit
main = runTest tests

tests :: TestSuite
tests = describe "dados" mainSuite

-- | Atalhos: @Test@ é @Aff Unit@, então tudo é encadeável com @do@.
assertEq :: forall a. Eq a => Show a => String -> a -> a -> Test
assertEq label expected actual =
  if expected == actual
    then pure unit
    else throwError (error (label <> ": esperado " <> show expected <> ", veio " <> show actual))

mainSuite :: TestSuite
mainSuite = describe "Data.Base64" base64Tests
  <> describe "Data.Maidenhead" maidenheadTests
  <> describe "Data.QSO" qsoTests
  <> describe "Data.QSO.Codec" codecTests
  <> describe "Data.Station" stationTests
  <> cliTests

base64Tests :: TestSuite
base64Tests = do
  describe "ascii" do
    -- | Credencial ficticia de proposito: o vetor exercita o Base64, e a
    -- | senha real do CouchDB nao tem por que estar num repositorio.
    it "codifica credencial" do
      assertEq "encode" "dXNlcjpzZW5oYS1zZWNyZXRh" (encode "user:senha-secreta")
    it "decodifica credencial" do
      r <- expectRight "decode" (decode "dXNlcjpzZW5oYS1zZWNyZXRh")
      assertEq "decode" "user:senha-secreta" r
  describe "padding" do
    it "1, 2 e 3 bytes" do
      assertEq "1 byte" "QQ==" (encode "A")
      assertEq "2 bytes" "QUI=" (encode "AB")
      assertEq "3 bytes" "QUJD" (encode "ABC")
      assertEq "vazio" "" (encode "")
    it "roundtrip" do
      a <- expectRight "1" (decode "QQ==")
      b <- expectRight "2" (decode "QUI=")
      c <- expectRight "3" (decode "QUJD")
      assertEq "1" "A" a
      assertEq "2" "AB" b
      assertEq "3" "ABC" c
  describe "unicode" do
    it "ida e volta com acento" do
      let encoded = encode "Olá"
      assertEq "encode" "T2zDoQ==" encoded
      back <- expectRight "decode" (decode encoded)
      assertEq "decode" "Olá" back
    it "ida e volta com varios acentos" do
      let source = "QSL-ção-ação"
          encoded = encode source
      assertEq "encode" "UVNMLcOnw6NvLWHDp8Ojbw==" encoded
      back <- expectRight "decode" (decode encoded)
      assertEq "decode" source back
    it "ida e volta com caractere de 4 bytes" do
      let source = "📡 station"
          encoded = encode source
      assertEq "encode" "8J+ToSBzdGF0aW9u" encoded
      back <- expectRight "decode" (decode encoded)
      assertEq "decode" source back
  describe "entrada tolerante" do
    it "ignora quebra de linha e espacos" do
      a <- expectRight "mime" (decode "dXNl\n")
      b <- expectRight "espaco" (decode " dXNl ")
      assertEq "mime" "use" a
      assertEq "espaco" "use" b
    it "rejeita sobra de 1 caractere" do
      assert "mod 4 == 1" (isLeft (decode "dXNlcmUhR"))

maidenheadTests :: TestSuite
maidenheadTests = do
  describe "parseGrid" do
    it "aceita 4, 6 e 8 caracteres" do
      equal (Right "JN58") (parseGrid "jn58")
      equal (Right "JN58TD") (parseGrid "JN58td")
      equal (Right "JN58TD42") (parseGrid "  jn58td42 ")
    it "rejeita grids invalidos" do
      assert "5 caracteres" (isLeft (parseGrid "JN58T"))
      assert "campo invalido" (isLeft (parseGrid "JZ58"))
      assert "subsquare invalido" (isLeft (parseGrid "JN58TZ"))
      assert "vazio" (isLeft (parseGrid ""))
  describe "normalizeGrid" do
    it "apara e passa a maiusculas" do
      equal "JN58TD" (normalizeGrid "  jn58td ")
  describe "gridToCoordinates" do
    it "centro do quadrado" do
      c <- expectRight "grid" (gridToCoordinates homeGrid)
      assert ("lon " <> show c.lon) (near 0.01 11.625 c.lon)
      assert ("lat " <> show c.lat) (near 0.01 48.1458 c.lat)
  describe "coordinatesToGrid" do
    it "ida e volta" do
      c <- expectRight "grid" (gridToCoordinates homeGrid)
      g <- expectRight "volta" (coordinatesToGrid c)
      equal "JN58TD" g
    it "rejeita longitude fora da grade" do
      assert "lon 200" (isLeft (coordinatesToGrid { lat: 0.0, lon: 200.0 }))
  describe "distanceAndBearing" do
    it "Alemanha -> Inglaterra" do
      home <- expectRight "home" (gridToCoordinates homeGrid)
      dx <- expectRight "dx" (gridToCoordinates dxGrid)
      let d = distanceAndBearing home dx
      assert ("distancia " <> show d.distanceKm) (between 850.0 1000.0 d.distanceKm)
      assert ("azimute " <> show d.azimuthDeg) (between 280.0 310.0 d.azimuthDeg)
    it "mesmo ponto da distancia zero" do
      home <- expectRight "home" (gridToCoordinates homeGrid)
      let d = distanceAndBearing home home
      assert ("distancia " <> show d.distanceKm) (near 0.001 0.0 d.distanceKm)
    it "valor de referencia conferido por calculo independente" do
      home <- expectRight "home" (gridToCoordinates homeGrid)
      far <- expectRight "far" (gridToCoordinates "GG66QR")
      let d = distanceAndBearing home far
      -- | 9827,39 km / 231,40° entre os centros dos quadrados, conferido
      -- | fora do PureScript com a fórmula de haversine.
      assert ("distancia " <> show d.distanceKm) (near 0.05 9827.39 d.distanceKm)
      assert ("azimute " <> show d.azimuthDeg) (near 0.05 231.40 d.azimuthDeg)
      equal "9827 km" (formatDistanceKm d.distanceKm)
  describe "formatDistanceKm" do
    it "metros abaixo de 1 km" do
      equal "850 m" (formatDistanceKm 0.85)
    it "uma casa decimal ate 10 km" do
      equal "9.5 km" (formatDistanceKm 9.46)
    it "arredonda a partir de 10 km" do
      equal "12 km" (formatDistanceKm 12.34)
      equal "1200 km" (formatDistanceKm 1199.6)
    it "negativo marca distancia indisponivel" do
      equal "-12 km" (formatDistanceKm (0.0 - 12.34))

qsoTests :: TestSuite
qsoTests = do
  describe "parseBand" do
    it "aceita rotulo novo e legado" do
      equal (Right B20m) (parseBand "20m")
      equal (Right B20m) (parseBand "20M")
      equal (Right B20m) (parseBand "B20m")
      equal (Right B20m) (parseBand "20")
      equal (Right B70cm) (parseBand "70cm")
      equal (Right B6cm) (parseBand "6cm")
    it "rejeita banda desconhecida" do
      assert "11m" (isLeft (parseBand "11m"))
      assert "vazio" (isLeft (parseBand ""))
  describe "bandFromMhz" do
    it "mapeia frequencia para banda" do
      equal (Just B20m) (bandFromMhz 14.2)
      equal (Just B40m) (bandFromMhz 7.1)
      equal (Just B2m) (bandFromMhz 144.3)
    it "devolve Nothing fora de banda" do
      assert "100 MHz" (isNothing (bandFromMhz 100.0))
  describe "tags estaveis no JSON" do
    it "band e mode" do
      equal "B20m" (bandTag B20m)
      equal "FT8" (modeTag FT8)
      equal "PSK31" (modeTag PSK31)
  describe "parseMode" do
    it "case-insensitive" do
      equal (Right FT8) (parseMode "ft8")
      equal (Right FT8) (parseMode "FT8")
      equal (Right SSB) (parseMode "SSB")
    it "rejeita modo desconhecido" do
      assert "FSK5000" (isLeft (parseMode "FSK5000"))
  describe "QSL" do
    it "ida e volta" do
      equal (Right Pending) (parseQSLStatus "pending")
      equal (Right Confirmed) (parseQSLStatus "Confirmed")
      equal "Sent" (qslLabel Sent)
    it "rejeita status invalido" do
      assert "talvez" (isLeft (parseQSLStatus "talvez"))
  describe "modeKind" do
    it "analogico e digital" do
      equal Digital (modeKind FT8)
      equal Analog (modeKind SSB)

codecTests :: TestSuite
codecTests = do
  describe "encodeQSO/decodeQSO" do
    it "roundtrip completo" do
      back <- expectRight "roundtrip" (decodeQSO (encodeQSO fullQso))
      equal fullQso back
    it "roundtrip so com campos obrigatorios" do
      let minimal =
            { id: Nothing
            , rev: Nothing
            , callsign: "PY2ABC"
            , band: B40m
            , mode: CW
            , rstSent: "599"
            , rstRcvd: "599"
            , grid: Nothing
            , operatorName: Nothing
            , qth: Nothing
            , dxcc: Nothing
            , country: Nothing
            , notes: Nothing
            , qslStatus: Confirmed
            , timestamp: "2026-01-02T03:04:05.000Z"
            }
      back <- expectRight "roundtrip" (decodeQSO (encodeQSO minimal))
      equal minimal back
  describe "compatibilidade" do
    it "lê documento legado com B20m/BSSB" do
      let legacy =
            "{\"_id\":\"qso:antigo\",\"_rev\":\"3-x\",\"type\":\"qso\",\"callsign\":\"LU1ABC\",\"band\":\"B20m\",\"mode\":\"BSSB\",\"rstSent\":\"59\",\"rstRcvd\":\"59\",\"timestamp\":\"2019-05-06T07:08:09.000Z\"}"
      json <- expectJson "json" (jsonParser legacy)
      qso <- expectRight "qso" (decodeQSO json)
      equal "LU1ABC" qso.callsign
      equal B20m qso.band
      equal SSB qso.mode
  describe "filterSelector" do
    it "monta selector com os filtros presentes" do
      let filter' =
            noFilter
              { bandFilter = Just B20m
              , callsignFilter = Just "PY"
              , yearFilter = Just 2026
              }
          json = stringify (filterSelector filter')
      assert ("band " <> json) (contains (Pattern "\"band\":\"B20m\"") json)
      assert ("regex " <> json) (contains (Pattern "$regex") json)
      assert ("ano " <> json) (contains (Pattern "2026-01-01") json)
      assertFalse "sem filtro de mode" (contains (Pattern "\"mode\"") json)
  describe "encodeFindRequest" do
    it "inclui limit e type" do
      let json = stringify (encodeFindRequest noFilter)
      assert ("limit " <> json) (contains (Pattern "\"limit\":200") json)
      assert ("type " <> json) (contains (Pattern "\"type\":\"qso\"") json)
  describe "estacao" do
    it "roundtrip" do
      let st =
            { id: Just "station"
            , rev: Just "2-y"
            , callsign: "PY2UQH"
            , grid: "JN58TD"
            , operatorName: "Ulisses"
            , rig: Just "IC-7300"
            , antenna: Just "Yagi"
            }
      back <- expectRight "station" (decodeStation (encodeStation st))
      equal st back

stationTests :: TestSuite
stationTests = do
  describe "mergeStationDraft" do
    it "mantem o que o draft nao traz" do
      let draft = emptyStationDraft { callsign = Just "PY2UQH", grid = Just "JN58TD", rig = Just "IC-7300" }
          st = newStation draft
          merged = mergeStationDraft st emptyStationDraft
      equal "PY2UQH" merged.callsign
      equal "JN58TD" merged.grid
      equal (Just "IC-7300") merged.rig
    it "sobrescreve o que o draft traz e normaliza o grid" do
      let st = newStation (emptyStationDraft { callsign = Just "PY2UQH", grid = Just "JN58TD" })
          merged =
            mergeStationDraft
              st
              (emptyStationDraft { grid = Just "jn58td", rig = Just "FT-991" })
      equal "PY2UQH" merged.callsign
      equal "JN58TD" merged.grid
      equal (Just "FT-991") merged.rig
  describe "stationToQso" do
    it "calcula distancia a partir do grid da estacao" do
      let st = newStation (emptyStationDraft { callsign = Just "PY2UQH", grid = Just "GG66QR" })
      assert "tem distancia" (isJust (stationToQso st fullQso))
    it "grid invalido nao quebra" do
      let st = newStation (emptyStationDraft { callsign = Just "PY2UQH", grid = Just "ZZ99" })
      assert "sem grid" (isNothing (stationToQso st fullQso))
