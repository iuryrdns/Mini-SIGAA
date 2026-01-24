{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Models.Turma
-- Description : Define a estrutura de uma turma ofertada.
-- Gerencia o horário, professor responsável, alunos matriculados e limite de vagas.
module Models.Turma
  ( -- * Tipos de Dados
    Turma
  
    -- * Construtores
  , criarTurma
  
    -- * Getters (Acessores) e setters
  , getCodigoTurma
  , getAlunosTurma
  , getDisciplinaTurma
  , getHorarioTurma
  , getProfessorTurma
  , getCapacidadeTurma
  , setAlunosTurma
  
    -- * Lógica de Vagas e Matrícula
  , temVagaTurma
  , adicionarAlunoTurma
  ) where

-- Bibliotecas Externas
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Módulos Internos
import Models.Types 
    ( Matricula
    , Codigo
    , Horario(..)
    )
import Models.Aluno (Aluno)

-------------------------------------------------------------------------------
-- Tipos de Dados
-------------------------------------------------------------------------------

-- | Representa uma oferta específica de uma disciplina em um período.
data Turma = Turma
  { _codigo               :: Int            -- ^ Identificador único da turma
  , _matriculaProfessor   :: Matricula      -- ^ Professor responsável
  , _disciplina           :: Codigo         -- ^ Código da disciplina vinculada
  , _horario             :: [Horario]        -- ^ Representação do horário (ex: "24M12")
  , _alunos               :: [Aluno]        -- ^ Lista de alunos confirmados
  , _qtdMaxAlunos         :: Int            -- ^ Limite de vagas
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-------------------------------------------------------------------------------
-- Construtor
-------------------------------------------------------------------------------

-- | Cria uma nova turma com a lista de alunos inicialmente vazia.
criarTurma :: Int -> Matricula -> Codigo -> [Horario] -> Int -> Turma
criarTurma codigo professor disciplina horario qtdAlunos =
  Turma
    { _codigo             = codigo
    , _matriculaProfessor = professor
    , _disciplina        = disciplina
    , _horario           = horario
    , _qtdMaxAlunos      = qtdAlunos
    , _alunos            = []
    }

-------------------------------------------------------------------------------
-- Getters e Setters
-------------------------------------------------------------------------------

-- | Retorna o código da turma.
getCodigoTurma :: Turma -> Int
getCodigoTurma = _codigo

-- | Retorna o código da disciplina ofertada nesta turma.
getDisciplinaTurma :: Turma -> Codigo
getDisciplinaTurma = _disciplina

-- | Retorna o objeto Horario da turma.
getHorarioTurma :: Turma -> [Horario]
getHorarioTurma = _horario

-- | Retorna a matrícula do professor responsável.
getProfessorTurma :: Turma -> Matricula
getProfessorTurma = _matriculaProfessor

-- | Retorna a lista de alunos já matriculados (pós-processamento).
getAlunosTurma :: Turma -> [Aluno]
getAlunosTurma = _alunos

-- | Retorna o limite máximo de vagas.
getCapacidadeTurma :: Turma -> Int
getCapacidadeTurma = _qtdMaxAlunos

-- | Atualiza a lista de alunos matriculados na turma.
setAlunosTurma :: [Aluno] -> Turma -> Turma
setAlunosTurma novosAlunos t = t { _alunos = novosAlunos }

-------------------------------------------------------------------------------
-- Lógica de Vagas e Matrícula
-------------------------------------------------------------------------------

-- | Verifica se a turma ainda possui vagas disponíveis.
temVagaTurma :: Turma -> Bool
temVagaTurma turma = length (_alunos turma) < _qtdMaxAlunos turma

-- | Adiciona um aluno à lista de matriculados da turma.
adicionarAlunoTurma :: Turma -> Aluno -> Turma
adicionarAlunoTurma t a = t { _alunos = a : _alunos t }