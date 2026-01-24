{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Models.Aluno
-- Description : Define a estrutura de dados Aluno e operações de histórico e notas.
-- Mantém o estado acadêmico do discente, incluindo CRA e registros de períodos passados.

module Models.Aluno 
  ( -- * Tipos de Dados
    Aluno (..)
  , NotasDisciplina(..)
  , RegistroHistorico

  -- * Construtores
  , criarAluno
  , notasVazias

  -- * Getters (Acessores)
  , getMatriculaAluno
  , getCraAluno
  , getCursoAluno
  , getNomeAluno
  , getNotasAluno
  , getHistoricoAluno
  , getCodigoDisciplinaHistorico
  
  -- * Transformações e Lógica de Negócio
  , atualizarNotas
  , processarFimSemestre
  ) where

-- Bibliotecas Externas
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

-- Módulos Internos
import Models.Types 
    ( Matricula
    , Nome
    , Curso
    , CRA
    , Codigo
    )

-------------------------------------------------------------------------------
-- Tipos de Dados
-------------------------------------------------------------------------------

-- | Armazena as notas parciais de uma disciplina no semestre corrente.
data NotasDisciplina = NotasDisciplina
  { n1 :: Maybe Double
  , n2 :: Maybe Double
  , n3 :: Maybe Double
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Representa o resultado final de uma disciplina já concluída.
data RegistroHistorico = RegistroHistorico
  { codigoDisciplina :: Codigo
  , mediaFinal       :: Double
  , aprovado         :: Bool 
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Entidade principal do discente.
data Aluno = Aluno {
    _matricula             :: Matricula,
    _nome                  :: Nome,
    _curso                 :: Curso,
    _cra                   :: CRA,
    _notas                 :: Map.Map Int NotasDisciplina, -- ^ Notas do semestre atual (Chave: ID da Turma)
    _historico             :: [RegistroHistorico],         -- ^ Disciplinas concluídas em períodos anteriores
    _periodoAtual          :: Int      
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-------------------------------------------------------------------------------
-- Construtores
-------------------------------------------------------------------------------

-- | Cria um novo aluno com valores iniciais e histórico vazio.
criarAluno :: Matricula -> Nome -> Curso -> CRA -> Aluno
criarAluno m n c craValue = Aluno {
    _matricula             = m,
    _nome                  = n,
    _curso                 = c,
    _cra                   = craValue,
    _notas                 = Map.empty,
    _historico             = [],
    _periodoAtual          = 1          
}

-- | Retorna uma estrutura de notas sem valores preenchidos.
notasVazias :: NotasDisciplina
notasVazias = NotasDisciplina Nothing Nothing Nothing

-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------

-- | Retorna a matrícula do aluno.
getMatriculaAluno :: Aluno -> Int
getMatriculaAluno = _matricula

-- | Retorna o nome do aluno.
getNomeAluno :: Aluno -> String
getNomeAluno = _nome

-- | Retorna o curso do aluno.
getCursoAluno :: Aluno -> String
getCursoAluno = _curso

-- | Retorna o CRA (Coeficiente de Rendimento Acadêmico) do aluno.
getCraAluno :: Aluno -> Double
getCraAluno = _cra

-- | Retorna o mapa de notas do aluno.
getNotasAluno :: Aluno -> Map.Map Int NotasDisciplina
getNotasAluno = _notas

-- | Retorna o histórico de disciplinas concluídas do aluno.
getHistoricoAluno :: Aluno -> [RegistroHistorico]
getHistoricoAluno = _historico

-- | Retorna o código da disciplina de um registro do histórico.
getCodigoDisciplinaHistorico :: RegistroHistorico -> Codigo
getCodigoDisciplinaHistorico = codigoDisciplina

-------------------------------------------------------------------------------
-- Lógica de Negócio
-------------------------------------------------------------------------------

-- | Substitui o mapa de notas atual do aluno.
atualizarNotas :: Map.Map Int NotasDisciplina -> Aluno -> Aluno
atualizarNotas novasNotas aluno = aluno { _notas = novasNotas }

-- | Calcula a média aritmética simples das notas disponíveis (assume 0 para Nothing).
calcularMedia :: NotasDisciplina -> Double
calcularMedia nd = (n1' + n2' + n3') / 3.0
  where
    n1' = fromMaybe 0 (n1 nd)
    n2' = fromMaybe 0 (n2 nd)
    n3' = fromMaybe 0 (n3 nd)

-- | Consolida o semestre atual: move notas para o histórico, limpa o mapa de notas,
-- incrementa o período e recalcula o CRA.
processarFimSemestre :: Aluno -> Aluno
processarFimSemestre al =
    let 
        novosRegistros = [ RegistroHistorico (show idT) (calcularMedia notas) (calcularMedia notas >= 7.0) 
                         | (idT, notas) <- Map.toList (_notas al) ]
        
        historicoCompleto = _historico al ++ novosRegistros
        
        novoCra = if null historicoCompleto
                  then 0.0
                  else sum (map mediaFinal historicoCompleto) / fromIntegral (length historicoCompleto)
                  
    in al { _notas = Map.empty
          , _historico = historicoCompleto
          , _cra = novoCra
          , _periodoAtual = _periodoAtual al + 1
          }