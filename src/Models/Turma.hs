module Models.Turma(Turma, getCapacidadeTurma, getCodigoTurma, getAlunosTurma, getDisciplinaTurma, getHorarioTurma, getProfessorTurma, adicionarAlunoTurma, criarTurma, temVagaTurma, getSalaTurma) where
--Paulo adicionou o getCapacidadeTurma na parte de exportação para teste da IO
import Models.Professor
import Models.Aluno
import Models.Disciplina

data Turma = Turma{ 
  _codigo :: Int,
  _matriculaProfessor :: Int,
  _disciplina :: String,
  _horario :: String,
  _sala :: String,
  _alunos :: [Aluno],
  _qtdMaxAlunos :: Int
  } deriving (Show, Read, Eq)

criarTurma :: Int -> Int -> String -> String -> String -> Int -> Turma 
criarTurma codigo professor disciplina horario sala qtdAlunos = Turma {
  _codigo = codigo,
  _matriculaProfessor = professor,
  _disciplina = disciplina,
  _horario = horario,
  _sala = sala,
  _qtdMaxAlunos = qtdAlunos,
  _alunos = []
}

adicionarAlunoTurma :: Turma -> Aluno -> Turma
adicionarAlunoTurma turma aluno = turma {
  _alunos = _alunos turma ++ [aluno]
}

getCodigoTurma :: Turma -> Int
getCodigoTurma = _codigo

getProfessorTurma :: Turma -> Int
getProfessorTurma = _matriculaProfessor

getDisciplinaTurma :: Turma -> String
getDisciplinaTurma = _disciplina

getHorarioTurma :: Turma -> String
getHorarioTurma = _horario

getAlunosTurma :: Turma -> [Aluno]
getAlunosTurma = _alunos

temVagaTurma :: Turma -> Bool
temVagaTurma turma = length (_alunos turma) < _qtdMaxAlunos turma

getSalaTurma :: Turma -> String
getSalaTurma = _sala

-- Paulo fez as alterações abaixo para testar a IO
getCapacidadeTurma :: Turma -> Int
getCapacidadeTurma = _qtdMaxAlunos
