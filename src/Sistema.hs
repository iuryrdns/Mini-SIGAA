{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Sistema where

import qualified Data.Map as M
import Data.List (intercalate)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, decode, encode)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as B

import Models.Aluno (Aluno, getMatriculaAluno, getNomeAluno)
import qualified Models.Aluno as A
import Models.Disciplina (Disciplina, getCodigoDisciplina, getNomeDisciplina)
import Models.Professor (Professor, getMatriculaProfessor)
import Models.Turma (Turma, getCodigoTurma, getDisciplinaTurma, getProfessorTurma)
import Models.Matricula (Matricula, criarMatricula, getIdAlunoMatricula, getIdTurmaMatricula)

data Sistema = Sistema
  { _alunos      :: M.Map Int Aluno
  , _professores :: M.Map Int Professor
  , _disciplinas :: M.Map String Disciplina
  , _matriculas  :: M.Map Int Int       
  , _turmas      :: M.Map Int Turma
  , _solicitacoes :: [Matricula]        
  , _fase        :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

dbPath :: FilePath
dbPath = "dados.json"

sistemaVazio :: Sistema
sistemaVazio = Sistema
  { _alunos      = M.empty
  , _professores = M.empty
  , _disciplinas = M.empty
  , _matriculas  = M.empty
  , _turmas      = M.empty
  , _solicitacoes = []
  , _fase        = 0
  }

--- --- FUNÇÕES DE CADASTRO GERAL --- ---

cadastrar :: (Ord i) => (v -> i) -> (Sistema -> M.Map i v) -> (M.Map i v -> Sistema -> Sistema) -> String -> v -> Sistema -> Either String Sistema
cadastrar getId getMap updateSystem nomeEntidade item sistema =
  let chave = getId item
      mapaAtual = getMap sistema
  in if M.member chave mapaAtual 
     then Left (nomeEntidade ++ " já cadastrado!")
     else Right $ updateSystem (M.insert chave item mapaAtual) sistema

cadastrarAluno :: Aluno -> Sistema -> Either String Sistema
cadastrarAluno = cadastrar getMatriculaAluno _alunos (\m s -> s {_alunos = m}) "Aluno"

cadastrarProfessor :: Professor -> Sistema -> Either String Sistema
cadastrarProfessor = cadastrar getMatriculaProfessor _professores (\m s -> s {_professores = m}) "Professor"

cadastrarDisciplina :: Disciplina -> Sistema -> Either String Sistema
cadastrarDisciplina = cadastrar getCodigoDisciplina _disciplinas (\m s -> s {_disciplinas = m}) "Disciplina"

cadastrarTurma :: Turma -> Sistema -> Either String Sistema
cadastrarTurma turma sistema
  | not (M.member (getProfessorTurma turma) (_professores sistema)) = Left "Professor não existe"
  | not (M.member (getDisciplinaTurma turma) (_disciplinas sistema)) = Left "Disciplina não existe"
  | otherwise = cadastrar getCodigoTurma _turmas (\m s -> s {_turmas = m}) "Turma" turma sistema

--- --- LÓGICA DE MATRÍCULA (SOLICITAÇÕES) --- ---

abrirPeriodoMatriculas :: Sistema -> Either String Sistema
abrirPeriodoMatriculas s 
    | _fase s == 1 = Left "O período já está aberto"
    | M.null (_alunos s) = Left "Lista de alunos vazia" 
    | otherwise = Right s { _fase = 1 }

cadastrarSolicitacao :: Int -> Int -> Sistema -> Either String Sistema
cadastrarSolicitacao idA idT sis = do
    -- 1. Validações de existência
    aluno <- maybe (Left "Aluno não encontrado!") Right (M.lookup idA (_alunos sis))
    _     <- maybe (Left "Turma não encontrada!") Right (M.lookup idT (_turmas sis))
    
    -- 2. Verifica se o aluno já solicitou essa mesma turma
    let jaPediu = any (\m -> getIdAlunoMatricula m == idA && getIdTurmaMatricula m == idT) (_solicitacoes sis)
    
    if jaPediu 
       then Left "Este aluno já solicitou matrícula nesta turma!"
       else 
           let novaM = criarMatricula idA idT (A.getCraAluno aluno)
               novoSis = sis { _solicitacoes = novaM : _solicitacoes sis }
           in Right novoSis

--- --- RELATÓRIOS --- ---

getRelatorioSolicitacoes :: Sistema -> Either String String
getRelatorioSolicitacoes sistema
  | null (_solicitacoes sistema) = Left "Não há solicitações pendentes!"
  | otherwise = Right relatorio
  where
    lista = zip [1..] (_solicitacoes sistema)
    montarLinha (idx, sol) =
      let
        idA = getIdAlunoMatricula sol
        idT = getIdTurmaMatricula sol
        aluno = _alunos sistema M.! idA
        turma = _turmas sistema M.! idT
        disc  = _disciplinas sistema M.! (getDisciplinaTurma turma)
      in
        show idx ++ ". [Pendente] " ++ getNomeAluno aluno ++ " -> " ++ getNomeDisciplina disc ++ " (Turma " ++ show idT ++ ")"
    relatorio = unlines (map montarLinha lista)

--- --- PERSISTÊNCIA --- ---

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

-- Getters auxiliares
getAlunos :: Sistema -> M.Map Int Aluno
getAlunos = _alunos

getTurmas :: Sistema -> M.Map Int Turma
getTurmas = _turmas

getFase :: Sistema -> Int
getFase = _fase

getProfessores :: Sistema -> M.Map Int Professor
getProfessores = _professores

getDisciplinas :: Sistema -> M.Map String Disciplina
getDisciplinas = _disciplinas