module Models.Types where

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
