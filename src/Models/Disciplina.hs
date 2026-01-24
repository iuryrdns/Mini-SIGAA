{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Models.Disciplina
-- Description : Define a estrutura de uma disciplina da grade curricular.
-- Contém informações sobre créditos, período sugerido e dependências (pré-requisitos).
module Models.Disciplina
  ( -- * Tipos de Dados
    Disciplina
  
    -- * Construtores
  , criarDisciplina
  
    -- * Getters (Acessores)
  , getCodigoDisciplina
  , getNomeDisciplina
  , getPreRequisitosDisciplina
  , getPeriodoDisciplina
  ) where

-- Bibliotecas Externas
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Módulos Internos
import Models.Types 
    ( Codigo
    , Nome
    )

-------------------------------------------------------------------------------
-- Tipos de Dados
------------------------------------------------------------------------------

-- | Representa uma unidade curricular do curso.
data Disciplina = Disciplina
  { _codigo        :: Codigo   -- ^ Identificador único (ex: "COMP01")
  , _nome          :: Nome     -- ^ Nome descritivo
  , _preRequisitos :: [Codigo] -- ^ Lista de códigos de disciplinas exigidas
  , _periodo       :: Int      -- ^ Período ideal para cursar
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-------------------------------------------------------------------------------
-- Construtor
-------------------------------------------------------------------------------

-- | Cria uma nova disciplina para o catálogo do sistema.
criarDisciplina :: String -> String -> [String] -> Int -> Disciplina
criarDisciplina codigo nome preRequisitos periodo = Disciplina {
  _codigo         = codigo,
  _nome           = nome,
  _preRequisitos  = preRequisitos,
  _periodo        = periodo
}

-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------

-- | Retorna o código identificador da disciplina.
getCodigoDisciplina :: Disciplina -> String
getCodigoDisciplina = _codigo

-- | Retorna o nome da disciplina.
getNomeDisciplina :: Disciplina -> String
getNomeDisciplina = _nome

-- | Retorna a lista de códigos que são pré-requisitos para esta disciplina
getPreRequisitosDisciplina :: Disciplina -> [String]
getPreRequisitosDisciplina = _preRequisitos

-- | Retorna o período sugerido no fluxo do curso.
getPeriodoDisciplina :: Disciplina -> Int
getPeriodoDisciplina = _periodo


