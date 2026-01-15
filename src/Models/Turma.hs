module Models.Turma(Turma, getCodigoTurma, getAlunosTurma, getDisciplinaTurma, getHorarioTurma, getProfessorTurma, adicionarAlunoTurma, criarTurma) where

import Models.Professor
import Models.Aluno
import Models.Disciplina

data Turma = Turma{ 
  _codigo :: Int,
  _professor :: Professor,
  _disciplina :: Disciplina,
  _horario :: String,
  _alunos :: [Aluno]
  } deriving (Show, Read, Eq)

criarTurma :: Int -> Professor -> Disciplina -> String -> Turma 
criarTurma codigo professor disciplina horario = Turma {
  _codigo = codigo,
  _professor = professor,
  _disciplina = disciplina,
  _horario = horario,
  _alunos = []
}

adicionarAlunoTurma :: Turma -> Aluno -> Turma
adicionarAlunoTurma turma aluno = turma {
  _alunos = _alunos turma ++ [aluno]
}

getCodigoTurma :: Turma -> Int
getCodigoTurma = _codigo

getProfessorTurma :: Turma -> Professor
getProfessorTurma = _professor

getDisciplinaTurma :: Turma -> Disciplina
getDisciplinaTurma = _disciplina

getHorarioTurma :: Turma -> String
getHorarioTurma = _horario

getAlunosTurma :: Turma -> [Aluno]
getAlunosTurma = _alunos
