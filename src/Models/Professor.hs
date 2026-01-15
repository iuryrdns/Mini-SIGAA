module Models.Professor(Professor, getMatriculaProfessor, getDepartamentoProfessor, getFormacaoProfessor, getNomeProfessor, criarProfessor) where

data Professor = Professor{ 
  _matricula :: Int, 
  _nome :: String,
  _departamento :: String,
  _formacao :: String
  } deriving (Show, Read, Eq)

criarProfessor :: Int -> String -> String -> String -> Professor
criarProfessor matricula nome departamento formacao = Professor {
  _matricula = matricula,
  _nome = nome,
  _departamento = departamento,
  _formacao = formacao
}


getMatriculaProfessor :: Professor -> Int
getMatriculaProfessor = _matricula

getNomeProfessor :: Professor -> String
getNomeProfessor = _nome

getDepartamentoProfessor :: Professor -> String
getDepartamentoProfessor = _departamento

getFormacaoProfessor :: Professor -> String
getFormacaoProfessor = _formacao

