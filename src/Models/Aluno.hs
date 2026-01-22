module Models.Aluno (Aluno, getMatriculaAluno, getCraAluno, getCursoAluno, getNomeAluno, getDisciplinasConcluidas, criarAluno) where

import qualified Data.Map as Map
import Models.Types (CRA, Curso, Matricula, Nome)

data Aluno = Aluno
  { _matricula :: Matricula,
    _nome :: Nome,
    _curso :: Curso,
    _cra :: CRA,
    _notas :: Map.Map Int [Int],
    _disciplinasConcluidas :: [String]
  }
  deriving (Show, Read, Eq)

criarAluno :: Matricula -> Nome -> Curso -> CRA -> Aluno
criarAluno matricula nome curso cra =
  Aluno
    { _matricula = matricula,
      _nome = nome,
      _curso = curso,
      _cra = cra,
      _notas = Map.empty,
      _disciplinasConcluidas = []
    }

getMatriculaAluno :: Aluno -> Matricula
getMatriculaAluno = _matricula

getNomeAluno :: Aluno -> Nome
getNomeAluno = _nome

getCursoAluno :: Aluno -> Curso
getCursoAluno = _curso

getCraAluno :: Aluno -> CRA
getCraAluno = _cra

getDisciplinasConcluidas :: Aluno -> [String]
getDisciplinasConcluidas = _disciplinasConcluidas