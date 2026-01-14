module Models.Aluno (Aluno) where

data Aluno = Aluno
  { matricula :: Int,
    nomr :: String,
    curso :: String,
    cra :: Float
  }
  deriving (Show)
