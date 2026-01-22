module Models.Horario (Horario, lerHorario, temInterseccao, validarHorario) where

import Data.Char (isDigit)
import Data.List (intersect, nub)

newtype Horario = Horario String
  deriving (Eq, Read)

instance Show Horario where
  show (Horario s) = s

-- Internal types
data DiaSemana = Seg | Ter | Qua | Qui | Sex | Sab | Dom
  deriving (Eq, Show, Enum, Ord)

-- (Dia, Turno, SlotIndex)
type Slot = (DiaSemana, Char, Int)

lerHorario :: String -> Horario
lerHorario = Horario

temInterseccao :: Horario -> Horario -> Bool
temInterseccao (Horario h1) (Horario h2) =
  let slots1 = parseHorario h1
      slots2 = parseHorario h2
   in not (null (slots1 `intersect` slots2))

validarHorario :: Horario -> Bool
validarHorario (Horario s) = all validarPart (words s)

-- Parsing logic
parseHorario :: String -> [Slot]
parseHorario s = concatMap parsePart (words s)

parsePart :: String -> [Slot]
parsePart s =
  let (diaStr, rest) = span isDigit s
      (turnoStr, slotsStr) = splitAt 1 rest
   in if not (validarPart s)
        then []
        else
          let turno = head turnoStr
              slots = [read [c] :: Int | c <- slotsStr, isDigit c]
              dias = map parseDia diaStr
           in [(d, turno, slot) | d <- dias, slot <- slots]

parseDia :: Char -> DiaSemana
parseDia '2' = Seg
parseDia '3' = Ter
parseDia '4' = Qua
parseDia '5' = Qui
parseDia '6' = Sex
parseDia '7' = Sab
parseDia _ = error "Dia invalido (use 2-7)"

validarPart :: String -> Bool
validarPart s =
  let (diaStr, rest) = span isDigit s
      (turnoStr, slotsStr) = splitAt 1 rest
   in not (null diaStr)
        && not (null turnoStr)
        && not (null slotsStr)
        && all (`elem` "234567") diaStr
        && head turnoStr `elem` "MTN"
        && all isDigit slotsStr