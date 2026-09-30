-- | Configuração da estação do operador: o que é gravado em um documento
-- | único do CouchDB e usado para calcular distâncias e azimutes.
module Data.Station
  ( Station
  , stationDocId
  , stationTypeTag
  , qsoTypeTag
  , newStation
  , stationCoordinates
  , describeStation
  , mergeStationDraft
  , StationDraft
  , emptyStationDraft
  , stationToQso
  ) where

import Prelude

import Data.Either (Either, either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maidenhead
  ( Coordinates
  , DistanceAndBearing
  , GridError
  , distanceAndBearing
  , gridToCoordinates
  , normalizeGrid
  )
import Data.QSO (QSO)

-- | @_id@ fixo do documento de configuração.
stationDocId :: String
stationDocId = "station"

-- | Campo @type@ usado pelo design doc para separar documentos.
stationTypeTag :: String
stationTypeTag = "station"

qsoTypeTag :: String
qsoTypeTag = "qso"

-- | A estação do operador.
type Station =
  { id :: Maybe String
  , rev :: Maybe String
  , callsign :: String
  , grid :: String
  , operatorName :: String
  , rig :: Maybe String
  , antenna :: Maybe String
  }

-- | Campos que podem ser atualizados sem recriar o documento.
type StationDraft =
  { callsign :: Maybe String
  , grid :: Maybe String
  , operatorName :: Maybe String
  , rig :: Maybe String
  , antenna :: Maybe String
  }

-- | Draft vazio: útil para atualizar um campo por vez.
emptyStationDraft :: StationDraft
emptyStationDraft =
  { callsign: Nothing
  , grid: Nothing
  , operatorName: Nothing
  , rig: Nothing
  , antenna: Nothing
  }

newStation :: StationDraft -> Station
newStation draft =
  { id: Nothing
  , rev: Nothing
  , callsign: fromMaybe "" (draft.callsign)
  , grid: maybe "" normalizeGrid draft.grid
  , operatorName: fromMaybe "" draft.operatorName
  , rig: draft.rig
  , antenna: draft.antenna
  }

-- | @Right@ quando a estação tem grid válido, @Left@ com o erro de grid.
stationCoordinates :: Station -> Either GridError Coordinates
stationCoordinates st = gridToCoordinates st.grid

describeStation :: Station -> String
describeStation st =
  st.callsign
    <> " em " <> st.grid
    <> (if st.operatorName == "" then "" else " (" <> st.operatorName <> ")")

-- | Aplica um rascunho sobre a estação existente, campo a campo.
mergeStationDraft :: Station -> StationDraft -> Station
mergeStationDraft st draft =
  { id: st.id
  , rev: st.rev
  , callsign: fromMaybe st.callsign draft.callsign
  , grid: maybe st.grid normalizeGrid draft.grid
  , operatorName: fromMaybe st.operatorName draft.operatorName
  , rig: pick draft.rig st.rig
  , antenna: pick draft.antenna st.antenna
  }

-- | Primeiro valor não-nulo, sem depender de @Alternative Maybe@.
pick :: forall a. Maybe a -> Maybe a -> Maybe a
pick (Just a) _ = Just a
pick Nothing b = b

-- | Distância e azimute da estação até um QSO. @Nothing@ quando algum dos
-- | lados não tem grid válido.
stationToQso :: Station -> QSO -> Maybe DistanceAndBearing
stationToQso st qso = do
  origin <- either (const Nothing) Just (stationCoordinates st)
  dest <- either (const Nothing) Just (gridToCoordinates (fromMaybe "" qso.grid))
  pure (distanceAndBearing origin dest)
