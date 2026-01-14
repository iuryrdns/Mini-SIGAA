module Models.Disciplina where

import Models.Professor (Professor)

data Disciplina = Disciplina
  { codigo :: String,
    nome :: String,
    turmas :: [Turma]
  }
  deriving (Show)
