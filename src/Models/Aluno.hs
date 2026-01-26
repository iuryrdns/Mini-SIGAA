module Models.Aluno (
    Aluno(..), 
    getMatriculaAluno, 
    getCraAluno, 
    getCursoAluno, 
    getNomeAluno, 
    getDisciplinasConcluidas, 
    getNotasAluno,
    criarAluno, 
    adicionarNota
) where

import qualified Data.Map as Map
import Models.Types (CRA, Curso, Matricula, Nome, Codigo)

data Aluno = Aluno
  { _matricula :: Matricula,
    _nome :: Nome,
    _curso :: Curso,
    _cra :: CRA,
    _notas :: Map.Map Codigo [Int],
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

getNotasAluno :: Aluno -> Map.Map Codigo [Int]
getNotasAluno = _notas

getDisciplinasConcluidas :: Aluno -> [String]
getDisciplinasConcluidas = _disciplinasConcluidas

adicionarNota :: Aluno -> Codigo -> Int -> Aluno
adicionarNota aluno codigo nota =
    let mapNotas = _notas aluno
        novoMapNotas = Map.insertWith (++) codigo [nota] mapNotas
    in aluno { _notas = novoMapNotas }