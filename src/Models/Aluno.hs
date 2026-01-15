module Models.Aluno (Aluno, getMatriculaAluno, getCraAluno, getCursoAluno, getNomeAluno, criarAluno) where

import qualified Data.Map as Map
data Aluno = Aluno {
  _matricula :: Int,
  _nome :: String,
  _curso :: String,
  _cra :: Float,
  _notas :: Map.Map Int Int
  } deriving (Show, Read, Eq)

criarAluno :: Int -> String -> String -> Float -> Aluno
criarAluno matricula nome curso cra = Aluno {
  _matricula = matricula,
  _nome = nome,
  _curso = curso,
  _cra = cra,
  _notas = Map.empty
}

getMatriculaAluno :: Aluno -> Int
getMatriculaAluno = _matricula

getNomeAluno :: Aluno -> String
getNomeAluno = _nome

getCursoAluno :: Aluno -> String
getCursoAluno = _curso

getCraAluno :: Aluno -> Float
getCraAluno = _cra



