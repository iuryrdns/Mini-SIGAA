{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models.Turma
  ( Turma
  , Dia(..)
  , getCodigoTurma
  , getAlunosTurma
  , getDisciplinaTurma
  , getDiaTurma        -- Novo: Acesso direto ao dia
  , getHoraTurma       -- Novo: Acesso direto à string da hora
  , getHorarioTurma    -- Mantido para compatibilidade se necessário
  , getProfessorTurma
  , adicionarAlunoTurma
  , criarTurma
  , temVagaTurma
  , getCapacidadeTurma
  , numParaDia         -- Novo: utilitário para converter 2-6 em Dia
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Models.Aluno (Aluno)


data Dia = Segunda | Terca | Quarta | Quinta | Sexta
    deriving (Eq, Enum, Bounded, Generic, ToJSON, FromJSON)

instance Show Dia where
    show Segunda = "Segunda"
    show Terca   = "Terca"
    show Quarta  = "Quarta"
    show Quinta  = "Quinta"
    show Sexta   = "Sexta"

data Turma = Turma
  { _codigo            :: Int
  , _matriculaProfessor :: Int
  , _disciplina        :: String
  , _dia               :: Dia    
  , _hora              :: String  
  , _alunos            :: [Aluno]
  , _qtdMaxAlunos      :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Função para converter números (2 a 6) para o tipo Dia
numParaDia :: Int -> Either String Dia
numParaDia 2 = Right Segunda
numParaDia 3 = Right Terca
numParaDia 4 = Right Quarta
numParaDia 5 = Right Quinta
numParaDia 6 = Right Sexta
numParaDia _ = Left "Dia inválido (use 2 para Segunda a 6 para Sexta)"

criarTurma :: Int -> Int -> String -> Dia -> String -> Int -> Turma
criarTurma codigo professor disciplina dia hora qtdAlunos =
  Turma
    { _codigo = codigo
    , _matriculaProfessor = professor
    , _disciplina = disciplina
    , _dia = dia
    , _hora = hora
    , _qtdMaxAlunos = qtdAlunos
    , _alunos = []
    }

getDiaTurma :: Turma -> Dia
getDiaTurma = _dia

getHoraTurma :: Turma -> String
getHoraTurma = _hora

getHorarioTurma :: Turma -> (Dia, String)
getHorarioTurma t = (_dia t, _hora t)

getCodigoTurma :: Turma -> Int
getCodigoTurma = _codigo

getProfessorTurma :: Turma -> Int
getProfessorTurma = _matriculaProfessor

getDisciplinaTurma :: Turma -> String
getDisciplinaTurma = _disciplina

getAlunosTurma :: Turma -> [Aluno]
getAlunosTurma = _alunos

temVagaTurma :: Turma -> Bool
temVagaTurma turma = length (_alunos turma) < _qtdMaxAlunos turma

getCapacidadeTurma :: Turma -> Int
getCapacidadeTurma = _qtdMaxAlunos

adicionarAlunoTurma :: Turma -> Aluno -> Turma
adicionarAlunoTurma turma aluno =
  turma { _alunos = _alunos turma ++ [aluno] }