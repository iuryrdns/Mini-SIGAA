module Utils.Database where

import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode)
import qualified Data.Aeson.Encode.Pretty as Pretty
import System.Directory (doesFileExist)
import Sistema (Sistema, sistemaVazio)

dbPath :: FilePath
dbPath = "dados.json"

carregarSistema :: IO Sistema
carregarSistema = do
  existe <- doesFileExist dbPath
  if not existe then return sistemaVazio
  else do
    conteudo <- B.readFile dbPath
    case decode conteudo of
      Just s -> return s
      Nothing -> return sistemaVazio

salvarSistema :: Sistema -> IO ()
salvarSistema s = B.writeFile dbPath (Pretty.encodePretty s)