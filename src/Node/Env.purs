-- | Acesso a variáveis de ambiente do processo Node.
-- |
-- | O @affjax-node@ ignora credenciais na URL, então a autenticação vai no
-- | header. A configuração vem do ambiente para o binário não carregar
-- | usuário e senha no código.
module Node.Env
  ( getEnv
  , getEnvWith
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

-- | A importação é pura de propósito. Se a assinatura dissesse @Effect@, o
-- | compilador geraria @lookupEnv(name)()@ e o @.js@ teria de devolver a
-- | função que carrega o efeito; devolver o valor direto e levantar em
-- | PureScript deixa o FFI legível e sem camada extra.
-- |
-- | O retorno é @Nullable String@ e não um tipo próprio com construtores:
-- | um data type do PureScript vira @instanceof@ no JavaScript, e o @.js@
-- | não tem como construir esse valor.
foreign import lookupEnv :: String -> Nullable String

-- | Valor da variável, ou @Nothing@ se não existir ou estiver vazia.
getEnv :: String -> Effect (Maybe String)
getEnv name =
  pure $ case toMaybe (lookupEnv name) of
    Just value
      | value == "" -> Nothing
      | otherwise -> Just value
    Nothing -> Nothing

-- | Valor da variável, com um texto de reserva para quando faltar.
getEnvWith :: String -> String -> Effect String
getEnvWith name fallback =
  getEnv name <#> fromMaybe fallback
