-- | Ponte com o processo Node: argumentos, saída e código de saída.
-- |
-- | O FFI é deliberadamente mínimo e fica no @.js@ ao lado, porque esta versão
-- | do compilador não aceita código inline no @.purs@.
-- |
-- | @process.exit@ é evitado de propósito: ele pode cortar o stdout quando a
-- | saída é um pipe (@qsologbook list | head@ perde as últimas linhas). O
-- | @Main@ escreve tudo primeiro e só então ajusta @exitCode@, o que deixa o
-- | Node encerrar sozinho depois de drenar a saída.
module Node.Process
  ( getArgs
  , printLine
  , setExitCode
  , nowTimestamp
  ) where

import Prelude

import Effect (Effect)

-- | As importações são puras. Declarar @Effect@ na assinatura faz o
-- | compilador esperar que o @.js@ devolva a função do efeito, e aí o valor
-- | real se perde; devolver o valor e envolver em @pure@ deixa o @.js@ limpo.
-- |
-- | As que não recebem argumento levam @Unit@ de propósito. Uma importação
-- | estrangeira sem argumento aparece no JavaScript como a própria função,
-- | e não como uma chamada: @pure getArgsRaw@ envolveria a função e
-- | @getArgs@ devolveria o código-fonte em vez dos argumentos. O @unit@ à
-- | frente força a chamada, que é o que o FFI precisa fazer.

-- | @argv@ sem os dois primeiros itens, que são @node@ e o caminho do script.
foreign import getArgsRaw :: Unit -> Array String

-- | Uma linha no stdout, com @\n@ no fim.
foreign import printLineRaw :: String -> Unit

-- | Ajusta o código de saída sem matar o processo.
foreign import setExitCodeRaw :: Int -> Unit

-- | Momento atual em ISO 8601 UTC, o formato do campo @timestamp@.
-- |
-- | O milissegundo é cortado para o texto ter a mesma forma do @timestamp@ dos
-- | documentos já gravados, e não só de ordenar igual.
foreign import nowTimestampRaw :: Unit -> String

getArgs :: Effect (Array String)
getArgs = pure (getArgsRaw unit)

printLine :: String -> Effect Unit
printLine text = pure (printLineRaw text)

setExitCode :: Int -> Effect Unit
setExitCode code = pure (setExitCodeRaw code)

nowTimestamp :: Effect String
nowTimestamp = pure (nowTimestampRaw unit)
