{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Models.Types
-- Description : Tipos base e lógica de parsing de horários para o Mini-SIGAA.
--
-- Este módulo contém as definições de tipos fundamentais utilizadas em todo o sistema,
-- bem como a lógica necessária para converter strings de horários (ex: "2M23")
-- em estruturas de dados processáveis (Slots).

module Models.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Char (isDigit)
import Data.List (intersect)

-------------------------------------------------------------------------------
-- Entidades de Sistema
-------------------------------------------------------------------------------

-- documentar
data StatusSolicitacao = Aceita | Recusada String deriving (Show, Generic, ToJSON, FromJSON)

data ResultadoProcessamento = ResultadoProcessamento
  { _rpMatricula :: Matricula
  , _rpTurma     :: Int
  , _rpStatus    :: StatusSolicitacao
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Representa um pedido de matrícula realizado por um aluno.
data Solicitacao = Solicitacao
  { _sMatricula :: Matricula -- ^ Matrícula do aluno solicitante
  , _sTurma     :: Int       -- ^ Identificador único da turma desejada
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-------------------------------------------------------------------------------
-- Aliases de Tipos (Sinônimos)
-------------------------------------------------------------------------------

-- | Identificador numérico único para alunos e professores.
type Matricula = Int

-- | Representação textual de nomes próprios.
type Nome      = String

-- | Nome do curso de graduação.
type Curso     = String

-- | Coeficiente de Rendimento Acadêmico.
type CRA       = Double

-- | Código identificador de disciplinas (ex: "COMP01").
type Codigo    = String

-------------------------------------------------------------------------------
-- Tipos de Dados e Domínio
-------------------------------------------------------------------------------

-- | Wrapper para strings de horários no formato padrão acadêmico.
-- Exemplo: @Horario "24M12"@ representa Segunda e Quarta, Manhã, primeiro e segundo horários.
newtype Horario = Horario String
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Dias úteis
data DiaSemana = Seg | Ter | Qua | Qui | Sex
  deriving (Eq, Show, Enum, Ord, Generic, ToJSON, FromJSON)

-- | Representa uma unidade mínima de tempo na grade horária.
-- Uma tupla contendo o dia, o turno ('M', 'T') e o número do horário.
type Slot = (DiaSemana, Char, Int)

-------------------------------------------------------------------------------
-- Lógica de Intersecção e Conflitos
-------------------------------------------------------------------------------

-- | Verifica se dois horários individuais possuem algum 'Slot' em comum.
horarioTemInterseccao :: Horario -> Horario -> Bool
horarioTemInterseccao (Horario h1) (Horario h2) =
  let slots1 = parseHorario h1
      slots2 = parseHorario h2
   in not (null (slots1 `intersect` slots2))

-- | Verifica se há conflito entre duas listas de horários.
-- Útil para validar a grade de uma turma que possui múltiplos encontros semanais.
listasHorarioConflitam :: [Horario] -> [Horario] -> Bool
listasHorarioConflitam h1s h2s =
  let slots1 = concatMap (\(Horario s) -> parseHorario s) h1s
      slots2 = concatMap (\(Horario s) -> parseHorario s) h2s
   in not (null (slots1 `intersect` slots2))

-------------------------------------------------------------------------------
-- Parsing de Horários
-------------------------------------------------------------------------------

-- | Converte uma string completa de horários em uma lista de 'Slot'.
-- Suporta strings com múltiplos blocos separados por espaços (ex: "2M23 4T45").
parseHorario :: String -> [Slot]
parseHorario s = concatMap parsePart (words s)

-- | Processa uma parte individual da string de horário (ex: "24M12").
-- O formato esperado é [Dígitos dos Dias][Letra do Turno][Dígitos dos Slots].
parsePart :: String -> [Slot]
parsePart s =
  let (diaStr, rest) = span isDigit s
      (turnoStr, slotsStr) = splitAt 1 rest
   in if null diaStr || null turnoStr || null slotsStr then []
      else
          let turno = head turnoStr
              slots = [read [c] :: Int | c <- slotsStr, isDigit c]
              dias = map parseDia diaStr
           in [(d, turno, slot) | d <- dias, slot <- slots]

-- | Mapeia caracteres numéricos para seus respectivos 'DiaSemana'.
-- '2' corresponde a Segunda-feira, '3' a Terça-feira, e assim sucessivamente.
parseDia :: Char -> DiaSemana
parseDia '2' = Seg
parseDia '3' = Ter
parseDia '4' = Qua
parseDia '5' = Qui
parseDia '6' = Sex
parseDia _   = Seg

