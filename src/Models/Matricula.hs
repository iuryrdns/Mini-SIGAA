{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models.Matricula where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Matricula = Matricula
  { _idAluno     :: Int
  , _idTurma     :: Int
  , _craNoMomento :: Float -- Importante guardar o CRA que o aluno tinha ao pedir
  , _status       :: StatusMatricula
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data StatusMatricula = Solicitada | Deferida | Indeferida
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

criarMatricula :: Int -> Int -> Float -> Matricula
criarMatricula idA idT cra = Matricula idA idT cra Solicitada

getIdAlunoMatricula :: Matricula -> Int
getIdAlunoMatricula = _idAluno

getIdTurmaMatricula :: Matricula -> Int
getIdTurmaMatricula = _idTurma

getStatusMatricula :: Matricula -> StatusMatricula
getStatusMatricula = _status