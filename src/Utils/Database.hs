module Utils.Database (carregarSistema, salvarSistema) where

import Sistema (Sistema, dbPath, sistemaVazio)
import System.Directory (doesFileExist, renameFile, removeFile)
import System.IO (IOMode (..), hGetContents, withFile)
import Text.Read (readMaybe)

carregarSistema :: IO (Sistema, String)
carregarSistema = do
  existeDb <- doesFileExist dbPath

  if existeDb
    then do
      conteudo <- readFile dbPath

      case readMaybe conteudo of
        Just sistemaLido -> do
          return (sistemaLido, "Dados carregados com sucesso!")
        Nothing -> do
          return (sistemaVazio, "Erro ao ler dados. Iniciando sistema vazio.")
    else do
      return (sistemaVazio, "Nenhum arquivo encontrado. Iniciando sistema vazio.")

salvarSistema :: Sistema -> IO ()
salvarSistema sistema = do
  let temp = dbPath ++ ".tmp"
  writeFile temp (show sistema)
  existeDb <- doesFileExist dbPath
  if existeDb
    then removeFile dbPath
    else return ()
  renameFile temp dbPath