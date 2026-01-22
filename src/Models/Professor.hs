module Models.Professor (Professor, getMatriculaProfessor, getDepartamentoProfessor, getFormacaoProfessor, getNomeProfessor, criarProfessor) where

import Models.Types (Matricula, Nome)

data Professor = Professor
  { _matricula :: Matricula,
    _nome :: Nome,
    _departamento :: String,
    _formacao :: String
  }
  deriving (Show, Read, Eq)

criarProfessor :: Matricula -> Nome -> String -> String -> Professor
criarProfessor matricula nome departamento formacao =
  Professor
    { _matricula = matricula,
      _nome = nome,
      _departamento = departamento,
      _formacao = formacao
    }

getMatriculaProfessor :: Professor -> Matricula
getMatriculaProfessor = _matricula

getNomeProfessor :: Professor -> Nome
getNomeProfessor = _nome

getDepartamentoProfessor :: Professor -> String
getDepartamentoProfessor = _departamento

getFormacaoProfessor :: Professor -> String
getFormacaoProfessor = _formacao