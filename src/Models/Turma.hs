module Models.Turma(Turma, getCodigoTurma, getAlunosTurma, getDisciplinaTurma, getHorarioTurma, getProfessorTurma, adicionarAlunoTurma, criarTurma, temVagaTurma) where

import Models.Professor
import Models.Aluno
import Models.Disciplina

data Turma = Turma{ 
  _codigo :: Int,
  _professor :: Professor,
  _disciplina :: Disciplina,
  _horario :: String,
  _alunos :: [Aluno],
  _qtdMaxAlunos :: Int
  } deriving (Show, Read, Eq)

criarTurma :: Int -> Professor -> Disciplina -> String -> Int -> Turma 
criarTurma codigo professor disciplina horario qtdAlunos = Turma {
  _codigo = codigo,
  _professor = professor,
  _disciplina = disciplina,
  _horario = horario,
  _qtdMaxAlunos = qtdAlunos,
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

temVagaTurma :: Turma -> Bool
temVagaTurma turma = length (_alunos turma) < _qtdMaxAlunos turma
