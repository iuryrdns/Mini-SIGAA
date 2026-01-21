{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models.Aluno 
  ( Aluno
  , NotasDisciplina(..)
  , getMatriculaAluno
  , getCraAluno
  , getCursoAluno
  , getNomeAluno
  , getNotasAluno
  , criarAluno
  , notasVazias
  ) where

import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Tipo específico para organizar as notas de uma disciplina
data NotasDisciplina = NotasDisciplina
  { n1 :: Float
  , n2 :: Float
  , n3 :: Float
  , optativa :: Maybe Float
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Aluno = Aluno {
    _matricula :: Int,
    _nome :: String,
    _curso :: String,
    _cra :: Float,
    _notas :: Map.Map Int NotasDisciplina,
    _disciplinasConcluidas :: [String]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

criarAluno :: Int -> String -> String -> Float -> Aluno
criarAluno matricula nome curso cra = Aluno {
    _matricula = matricula,
    _nome = nome,
    _curso = curso,
    _cra = cra,
    _notas = Map.empty,
    _disciplinasConcluidas = []
}

-- Funções de acesso
getMatriculaAluno :: Aluno -> Int
getMatriculaAluno = _matricula

getNomeAluno :: Aluno -> String
getNomeAluno = _nome

getCursoAluno :: Aluno -> String
getCursoAluno = _curso

getCraAluno :: Aluno -> Float
getCraAluno = _cra

getNotasAluno :: Aluno -> Map.Map Int NotasDisciplina
getNotasAluno = _notas

notasVazias :: NotasDisciplina
notasVazias = NotasDisciplina 0 0 0 Nothing