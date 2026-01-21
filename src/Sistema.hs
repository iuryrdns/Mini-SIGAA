{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Sistema where

import Data.Map as Map (Map, empty, insert, member, (!), null, toList)
import Models.Aluno (Aluno, getMatriculaAluno, getNomeAluno)
import Models.Disciplina (Disciplina, getCodigoDisciplina, getNomeDisciplina)
import Models.Professor (Professor, getMatriculaProfessor)
import Models.Turma (Turma, getCodigoTurma, temVagaTurma, getDisciplinaTurma, getProfessorTurma)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, decode, encode)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as B

data Sistema = Sistema
  { _alunos :: Map.Map Int Aluno,
    _professores :: Map.Map Int Professor,
    _disciplinas :: Map.Map String Disciplina,
    _matriculas :: Map.Map Int Int,
    _turmas :: Map.Map Int Turma,
    _fase :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

dbPath :: FilePath
dbPath = "dados.json"

sistemaVazio :: Sistema
sistemaVazio =
  Sistema
    { _alunos = Map.empty,
      _professores = Map.empty,
      _disciplinas = Map.empty,
      _matriculas = Map.empty,
      _turmas = Map.empty,
      _fase = 0
    }

cadastrar :: (Ord i) => (v -> i) -> (Sistema -> Map.Map i v) -> (Map.Map i v -> Sistema -> Sistema) -> String -> v -> Sistema -> Either String Sistema
cadastrar getId getMap updateSystem nomeEntidade item sistema =
  let chave = getId item
      mapaAtual = getMap sistema
  in if Map.member chave mapaAtual 
     then Left (nomeEntidade ++ " ja Cadastrado!")
     else Right $ updateSystem (Map.insert chave item mapaAtual) sistema

abrirPeriodoMatriculas :: Sistema -> Either String Sistema
abrirPeriodoMatriculas sistema =
  if _fase sistema == 1 then
    Left "Matriculas ja estao abertas"
  else
    Right sistema {_fase = 1}

realizarMatricula :: Int -> Int -> Sistema -> Either String Sistema
realizarMatricula matricula idTurma sistema
  | not (Map.member matricula (_alunos sistema)) = Left "Aluno não cadastrado"
  | not (Map.member idTurma (_turmas sistema)) = Left "Turma não cadastrada"
  | not (temVagaTurma turmaEncontrada) = Left "Turma sem Vaga!"
  | otherwise =
    cadastrar (const matricula) _matriculas (\m s -> s {_matriculas = m}) "Matricula" idTurma sistema
  where
    turmaEncontrada = _turmas sistema ! idTurma

cadastrarAluno :: Aluno -> Sistema -> Either String Sistema
cadastrarAluno = cadastrar getMatriculaAluno _alunos (\m s -> s {_alunos = m}) "Aluno"

cadastrarProfessor :: Professor -> Sistema -> Either String Sistema
cadastrarProfessor = cadastrar getMatriculaProfessor _professores (\m s -> s {_professores = m}) "Professor"

cadastrarDisciplina :: Disciplina -> Sistema -> Either String Sistema
cadastrarDisciplina = cadastrar getCodigoDisciplina _disciplinas (\m s -> s {_disciplinas = m}) "Disciplina"

cadastrarTurma :: Turma -> Sistema -> Either String Sistema
cadastrarTurma turma sistema
  | not (Map.member (getProfessorTurma turma) (_professores sistema)) = Left "Professor não existe"
  | not (Map.member (getDisciplinaTurma turma) (_disciplinas sistema)) = Left "Disciplina não existe"
  | otherwise =
    cadastrar getCodigoTurma _turmas (\m s -> s {_turmas = m}) "Turma" turma sistema

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

getTurmas :: Sistema -> Map.Map Int Turma
getTurmas = _turmas

getFase :: Sistema -> Int
getFase = _fase

-- Persistência
carregarSistema :: IO Sistema
carregarSistema = do
  existe <- doesFileExist dbPath
  if not existe then return sistemaVazio
  else do
    conteudo <- B.readFile dbPath
    case decode conteudo of
      Just s -> return s
      Nothing -> return sistemaVazio

salvarSistema :: Sistema -> IO ()
salvarSistema s = B.writeFile dbPath (encode s)

getMatriculasRealizadas :: Sistema -> Either String String
getMatriculasRealizadas sistema
  | Map.null (_matriculas sistema) = Left "Não há nenhuma matrícula!"
  | otherwise = Right relatorioMatriculas
  where
    listaMatriculas = Map.toList (_matriculas sistema)

    lista = zip [1..] listaMatriculas

    montarLinha (idx, (idAluno, idTurma)) =
      let
        aluno = _alunos sistema ! idAluno
        turma = _turmas sistema ! idTurma
        codDisc = getDisciplinaTurma turma
        disciplina = _disciplinas sistema ! codDisc
        
        nomeAluno   = getNomeAluno aluno
        matrAluno   = show idAluno
        nomeDisc  = getNomeDisciplina disciplina
        codTurma  = show (getCodigoTurma turma)
      in
        show idx ++ ". " ++ nomeAluno ++ " - " ++ matrAluno ++ ": " ++ nomeDisc ++ " " ++ codTurma
    relatorioMatriculas = unlines (map montarLinha lista)
