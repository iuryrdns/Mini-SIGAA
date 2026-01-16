module Utils.Database (carregarSistema, salvarSistema) where

import Sistema (Sistema, sistemaVazio, dbPath)
import System.Directory (doesFileExist, renameFile)
import System.IO (withFile, IOMode(..), hGetContents)
import Text.Read (readMaybe)

carregarSistema :: IO Sistema
carregarSistema = do
    existeDb <- doesFileExist dbPath
    
    if existeDb then do
        conteudo <- readFile dbPath
       
        case readMaybe conteudo of
            Just sistemaLido -> do
                putStrLn "Dados carregados!"
                return sistemaLido
                
            Nothing -> do
                putStrLn "Iniciando sistema vazio."
                return sistemaVazio
                
    else do
        putStrLn "Nenhum arquivo encontrado."
        putStrLn "Iniciando sistema vazio."
        return sistemaVazio


salvarSistema :: Sistema -> IO ()
salvarSistema sistema = do
    let temp = dbPath ++ ".tmp"
    writeFile temp (show sistema)
    renameFile temp dbPath
    putStrLn "Sistema salvo."