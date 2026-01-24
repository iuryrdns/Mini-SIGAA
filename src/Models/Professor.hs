{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Models.Professor
-- Description : Define a estrutura de dados do docente e suas informações acadêmicas.
-- Gerencia os dados de identificação, lotação (departamento) e titulação do professor.
module Models.Professor
  ( -- * Tipos de Dados
    Professor
  
    -- * Construtores
  , criarProfessor
  
    -- * Getters (Acessores)
  , getMatriculaProfessor
  , getNomeProfessor
  , getDepartamentoProfessor
  , getFormacaoProfessor
  ) where

-- Bibliotecas Externas
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Módulos Internos
import Models.Types 
    ( Matricula
    , Nome
    )

-------------------------------------------------------------------------------
-- Tipos de Dados
-------------------------------------------------------------------------------

-- | Representa um docente no sistema acadêmico.
data Professor = Professor
  { _matricula    :: Matricula -- ^ Identificador único do professor
  , _nome         :: Nome      -- ^ Nome completo
  , _departamento :: String    -- ^ Centro ou Departamento de lotação
  , _formacao     :: String    -- ^ Titulação (ex: Doutorado em Computação)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-------------------------------------------------------------------------------
-- Construtor
-------------------------------------------------------------------------------

-- | Cria um novo registro de professor.
criarProfessor :: Matricula -> Nome -> String -> String -> Professor
criarProfessor matricula nome departamento formacao = Professor
  { _matricula    = matricula
  , _nome         = nome
  , _departamento = departamento
  , _formacao     = formacao
  }

-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------

-- | Retorna a matrícula do professor.
getMatriculaProfessor :: Professor -> Matricula
getMatriculaProfessor = _matricula

-- | Retorna o nome do professor.
getNomeProfessor :: Professor -> Nome
getNomeProfessor = _nome

-- | Retorna o departamento ao qual o professor está vinculado.
getDepartamentoProfessor :: Professor -> String
getDepartamentoProfessor = _departamento

-- | Retorna a titulação ou formação do professor.
getFormacaoProfessor :: Professor -> String
getFormacaoProfessor = _formacao
