module Models.Types where

import Data.Char (isDigit)
import Data.List (intersect, nub)


type Horario = String

data DiaSemana = Seg | Ter | Qua | Qui | Sex | Sab | Dom
  deriving (Eq, Show, Enum, Ord)

type Slot = (DiaSemana, Char, Int)

newtype Matricula = Matricula Int
  deriving (Show, Read, Eq, Ord)

newtype Nome = Nome String
  deriving (Show, Read, Eq)

newtype Curso = Curso String
  deriving (Show, Read, Eq)

newtype CRA = CRA Float
  deriving (Show, Read, Eq, Ord)

newtype Codigo = Codigo String
  deriving (Show, Read, Eq, Ord)

mkCRA :: Float -> Maybe CRA
mkCRA x
  | x >= 0.0 && x <= 10.0 = Just (CRA x)
  | otherwise = Nothing

unMatricula :: Matricula -> Int
unMatricula (Matricula m) = m

unNome :: Nome -> String
unNome (Nome n) = n

unCurso :: Curso -> String
unCurso (Curso c) = c

unCRA :: CRA -> Float
unCRA (CRA c) = c

unCodigo :: Codigo -> String
unCodigo (Codigo c) = c

lerHorario :: String -> Horario
lerHorario s = s

horarioTemInterseccao :: Horario -> Horario -> Bool
horarioTemInterseccao h1 h2 =
  let slots1 = parseHorario h1
      slots2 = parseHorario h2
   in not (null (slots1 `intersect` slots2))

validarHorario :: Horario -> Bool
validarHorario s = all validarPart (words s)

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