-- |
-- Module      : Utils.Database
-- Description : Camada de persistência para o Mini-SIGAA.
--
-- Este módulo gerencia a serialização e desserialização do estado do 'Sistema'
-- em arquivos JSON, garantindo que os dados de alunos e turmas persistam
-- entre as execuções do programa

module Utils.Database
  ( -- * Persistência
    carregarSistema
  , salvarSistema
    -- * Configurações
  , dbPath
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode)
import qualified Data.Aeson.Encode.Pretty as Pretty
import System.Directory (doesFileExist)
import Sistema (Sistema, sistemaVazio)

-- | Caminho fixo para o arquivo de banco de dados JSON.
dbPath :: FilePath
dbPath = "dados.json"

-- | Carrega o estado do sistema a partir do arquivo definido em 'dbPath'.
--
-- Caso o arquivo não exista ou o conteúdo seja inválido/corrompido,
-- a função retorna um 'sistemaVazio' para evitar o travamento da aplicação.
carregarSistema :: IO Sistema
carregarSistema = do
  existe <- doesFileExist dbPath
  if not existe then return sistemaVazio
  else do
    conteudo <- B.readFile dbPath
    case decode conteudo of
      Just s -> return s
      Nothing -> return sistemaVazio

-- | Salva o estado atual do 'Sistema' no disco de forma formatada (pretty print).
--
-- Utiliza codificação UTF-8 via Lazy ByteString. O arquivo gerado é sobrescrito
-- a cada chamada.
salvarSistema :: Sistema -> IO ()
salvarSistema s = B.writeFile dbPath (Pretty.encodePretty s)