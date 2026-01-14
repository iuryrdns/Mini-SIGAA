module Sistema where

import Models.Aluno (Aluno)
import Models.Disciplina (Disciplina)
import Models.Professor (Professor)
import System.Directory (doesDirectoryExist, doesFileExist)
import Text.Read (readMaybe)

data Sistema = Sistema
  { alunos :: [Aluno],
    professores :: [Professor],
    disciplinas :: [Disciplina],
    fase :: Integer
  }
  deriving (Show, Read)

dbPath :: String
dbPath = "dados.db"

iniciarSistema :: [Aluno] -> [Professor] -> [Disciplina] -> Integer -> IO ()
