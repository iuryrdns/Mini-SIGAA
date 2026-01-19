module Sistema where
<<<<<<< HEAD
import qualified Data.Map as Map
import Models.Aluno
import Models.Professor
import Models.Disciplina
import Models.Turma
import System.Directory (doesDirectoryExist, doesFileExist)
import Text.Read (readMaybe)
data Sistema = Sistema {
  _alunos :: Map.Map Int Aluno,
  _professores :: Map.Map Int Professor,
  _disciplinas :: Map.Map String Disciplina,
  _turmas :: Map.Map Int Turma,
  _fase :: Int
  } deriving (Show, Read)
=======

import Data.Map as Map (Map, empty, insert, member)
import Models.Aluno (Aluno, getMatriculaAluno)
import Models.Disciplina (Disciplina, getCodigoDisciplina)
import Models.Professor (Professor, getMatriculaProfessor)
import Models.Turma (Turma, getCodigoTurma)
import System.Directory (doesDirectoryExist, doesFileExist)
import Text.Read (readMaybe)

data Sistema = Sistema
  { _alunos :: Map.Map Int Aluno,
    _professores :: Map.Map Int Professor,
    _disciplinas :: Map.Map String Disciplina,
    _matriculas :: Map.Map Int Int,
    _turmas :: Map.Map Int Turma,
    _fase :: Int
  }
  deriving (Show, Read)
>>>>>>> fba7626 (Início da separação de fases)

dbPath :: String
dbPath = "dados.db"

sistemaVazio :: Sistema
<<<<<<< HEAD
sistemaVazio = Sistema {
  _alunos = Map.empty,
  _professores = Map.empty,
  _disciplinas = Map.empty,
  _turmas = Map.empty,
  _fase = 0
}
=======
sistemaVazio =
  Sistema
    { _alunos = Map.empty,
      _professores = Map.empty,
      _disciplinas = Map.empty,
      _matriculas = Map.empty,
      _turmas = Map.empty,
      _fase = 0
    }
>>>>>>> fba7626 (Início da separação de fases)

cadastrar :: (Ord i) => (v -> i) -> (Sistema -> Map.Map i v) -> (Map.Map i v -> Sistema -> Sistema) -> String -> v -> Sistema -> Either String Sistema
cadastrar getId getMap updateSystem nomeEntidade item sistema =
  let
    chave = getId item
    mapaAtual = getMap sistema
  in
    if Map.member chave mapaAtual then
      Left (nomeEntidade ++ " ja Cadastrado!")
    else
      let
        novoMapa = Map.insert chave item mapaAtual
        novoSistema = updateSystem novoMapa sistema
      in
        Right novoSistema

abrirPeriodoMatriculas :: Sistema -> Either String Sistema
abrirPeriodoMatriculas sistema = 
  if _fase sistema == 1 then
    Left "Matriculas ja estao abertas"
  else 
    Right sistema {_fase = 1}

cadastrarAluno :: Aluno -> Sistema -> Either String Sistema
cadastrarAluno = cadastrar getMatriculaAluno _alunos (\m s -> s {_alunos = m}) "Aluno"

cadastrarProfessor :: Professor -> Sistema -> Either String Sistema
cadastrarProfessor = cadastrar getMatriculaProfessor _professores (\m s -> s {_professores = m}) "Professor"

cadastrarDisciplina :: Disciplina -> Sistema -> Either String Sistema
cadastrarDisciplina = cadastrar getCodigoDisciplina _disciplinas (\m s -> s {_disciplinas = m}) "Disciplina"

cadastrarTurma :: Turma -> Sistema -> Either String Sistema
cadastrarTurma = cadastrar getCodigoTurma _turmas (\m s -> s {_turmas = m}) "Turma"

realizarMatricula :: Int -> Sistema -> Either String Sistema
realizarMatricula = cadastrar id _matriculas (\m s -> s {_matriculas = m}) "Matricula"

verificarRequisitos :: [String] -> Sistema -> Either String [String]
verificarRequisitos requisistos sistema = mapM verificar requisistos
  where
    mapaDisciplinas = _disciplinas sistema
    verificar codigo = 
        if Map.member codigo mapaDisciplinas 
        then Right codigo 
        else Left ("A disciplina requisito '" ++ codigo ++ "' nao existe!")

getAlunos :: Sistema -> Map.Map Int Aluno
getAlunos = _alunos

getProfessores :: Sistema -> Map.Map Int Professor
getProfessores = _professores

getDisciplinas :: Sistema -> Map.Map String Disciplina
getDisciplinas = _disciplinas

<<<<<<< HEAD
getTurmas ::Sistema -> Map.Map Int Turma
getTurmas = _turmas
=======
getTurmas :: Sistema -> Map.Map Int Turma
getTurmas = _turmas

getFase :: Sistema -> Int
getFase = _fase
>>>>>>> fba7626 (Início da separação de fases)
