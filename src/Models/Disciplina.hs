module Models.Disciplina (Disciplina, getCodigoDisciplina, getNomeDisciplina, getRequisitosDisciplina, getCursosDisciplina, criarDisciplina) where

import Models.Types (Codigo, Curso, Nome)

data Disciplina = Disciplina
  { _codigo :: Codigo,
    _nome :: Nome,
    _preRequisitos :: [Codigo],
    _cursos :: [Curso]
  }
  deriving (Show, Read, Eq)

criarDisciplina :: Codigo -> Nome -> [Codigo] -> [Curso] -> Disciplina
criarDisciplina codigo nome preRequisitos cursos =
  Disciplina
    { _codigo = codigo,
      _nome = nome,
      _preRequisitos = preRequisitos,
      _cursos = cursos
    }

getCodigoDisciplina :: Disciplina -> Codigo
getCodigoDisciplina = _codigo

getNomeDisciplina :: Disciplina -> Nome
getNomeDisciplina = _nome

getRequisitosDisciplina :: Disciplina -> [Codigo]
getRequisitosDisciplina = _preRequisitos

getCursosDisciplina :: Disciplina -> [Curso]
getCursosDisciplina = _cursos