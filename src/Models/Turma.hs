module Models.Turma (Turma, getCapacidadeTurma, getCodigoTurma, getAlunosTurma, getDisciplinaTurma, getHorarioTurma, getProfessorTurma, adicionarAlunoTurma, limparAlunosTurma, criarTurma, getSalaTurma, setHorarioTurma, setSalaTurma) where

import Models.Aluno
import Models.Disciplina
import Models.Professor
import Models.Types (Codigo, Horario, Matricula)

data Turma = Turma
  { _codigo :: Int,
    _matriculaProfessor :: Matricula,
    _disciplina :: Codigo,
    _horario :: Horario,
    _sala :: String,
    _alunos :: [Aluno],
    _qtdMaxAlunos :: Int
  }
  deriving (Show, Read, Eq)

criarTurma :: Int -> Matricula -> Codigo -> Horario -> String -> Int -> Turma
criarTurma codigo professor disciplina horario sala qtdAlunos =
  Turma
    { _codigo = codigo,
      _matriculaProfessor = professor,
      _disciplina = disciplina,
      _horario = horario,
      _sala = sala,
      _qtdMaxAlunos = qtdAlunos,
      _alunos = []
    }

adicionarAlunoTurma :: Turma -> Aluno -> Turma
adicionarAlunoTurma turma aluno =
  turma
    { _alunos = _alunos turma ++ [aluno]
    }

limparAlunosTurma :: Turma -> Turma
limparAlunosTurma turma = turma { _alunos = [] }

getCodigoTurma :: Turma -> Int
getCodigoTurma = _codigo

getProfessorTurma :: Turma -> Matricula
getProfessorTurma = _matriculaProfessor

getDisciplinaTurma :: Turma -> Codigo
getDisciplinaTurma = _disciplina

getHorarioTurma :: Turma -> Horario
getHorarioTurma = _horario

getAlunosTurma :: Turma -> [Aluno]
getAlunosTurma = _alunos

getSalaTurma :: Turma -> String
getSalaTurma = _sala

setHorarioTurma :: Turma -> Horario -> Turma
setHorarioTurma turma novoHorario = turma {_horario = novoHorario}

setSalaTurma :: Turma -> String -> Turma
setSalaTurma turma novaSala = turma {_sala = novaSala}

-- Paulo fez as alterações abaixo para testar a IO
getCapacidadeTurma :: Turma -> Int
getCapacidadeTurma = _qtdMaxAlunos