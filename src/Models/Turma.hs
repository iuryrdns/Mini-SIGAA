module Models.Turma where

import Models.Aluno (Aluno)
import Models.Professor (Professor)

data Turma = Turma
  { codigo :: String,
    professor :: Professor,
    horario :: String,
    alunos :: [Aluno]
  }
  deriving (Show)