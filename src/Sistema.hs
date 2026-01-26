{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Sistema
-- Description : Núcleo do SIGAA. Gerencia o estado global e a lógica de matrículas.
-- Este módulo coordena as interações entre Alunos, Professores, Turmas e Disciplinas.
module Sistema
  ( -- * Tipos
    Sistema(..)
  , sistemaVazio
    -- * Operações de Cadastro
  , cadastrarAluno
  , cadastrarProfessor
  , cadastrarDisciplina
  , cadastrarTurma
    -- * Lógica de Matrícula
  , abrirPeriodoMatriculas
  , cadastrarSolicitacao
  , finalizarSemestre
  , lancarNotas
    -- * Relatórios e Getters
  --, getRelatorioSolicitacoes
  , getAlunos
  , getTurmas
  , getFase
  , getProfessores
  , getDisciplinas
    -- * Lógica Interna
  , efetivarMatriculas
  ) where

import qualified Data.Map as M
import Data.List (intercalate, sortBy)
import Data.Ord (comparing, Down(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Importações de Modelos
import Models.Types (Matricula, Codigo, Solicitacao(..), StatusSolicitacao(..), ResultadoProcessamento(..), listasHorarioConflitam)
import Models.Aluno (Aluno, NotasDisciplina(..))
import qualified Models.Aluno as A
import Models.Disciplina (Disciplina)
import qualified Models.Disciplina as D
import Models.Professor (Professor)
import qualified Models.Professor as P
import Models.Turma (Turma)
import qualified Models.Turma as T

-------------------------------------------------------------------------------
-- Estado do Sistema
-------------------------------------------------------------------------------

-- | Estrutura central que armazena todos os dados em memória.
data Sistema = Sistema
  { _alunos         :: M.Map Matricula Aluno
  , _professores    :: M.Map Matricula Professor
  , _disciplinas    :: M.Map Codigo Disciplina
  , _turmas         :: M.Map Int Turma          -- ^ Mape código da turma para a entidade Turma
  , _solicitacoes   :: [Solicitacao]            -- ^ Fila de espera para processamento
  , _historicoProc  :: [ResultadoProcessamento] -- ^ Histórico de processamentos de solicitações
  , _fase           :: Int                      -- ^ 0: Planejamento, 1: Matrícula, 2: Semestre Ativo
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Estado inicial padrão
sistemaVazio :: Sistema
sistemaVazio = Sistema
  { _alunos         = M.empty
  , _professores    = M.empty
  , _disciplinas    = M.empty
  , _turmas         = M.empty
  , _solicitacoes   = []
  , _historicoProc  = []
  , _fase           = 0
  }

-------------------------------------------------------------------------------
-- Funções de Cadastro
-------------------------------------------------------------------------------

-- | Função genérica para inserção em mapas evitando duplicidade.
cadastrar :: (Ord i)
          => (v -> i)
          -> (Sistema -> M.Map i v)
          -> (M.Map i v -> Sistema -> Sistema)
          -> String -> v -> Sistema -> Either String Sistema
cadastrar getId getMap updateSystem nomeEntidade item sistema =
  let chave = getId item
      mapaAtual = getMap sistema
  in if M.member chave mapaAtual
     then Left (nomeEntidade ++ " já cadastrado!")
     else Right $ updateSystem (M.insert chave item mapaAtual) sistema

-- | Cadastra um novo aluno no sistema.
cadastrarAluno :: Aluno -> Sistema -> Either String Sistema
cadastrarAluno = cadastrar A.getMatriculaAluno _alunos (\m s -> s {_alunos = m}) "Aluno"

-- | Cadastra um novo professor no sistema.
cadastrarProfessor :: Professor -> Sistema -> Either String Sistema
cadastrarProfessor = cadastrar P.getMatriculaProfessor _professores (\m s -> s {_professores = m}) "Professor"

-- | Cadastra uma nova disciplina no sistema.
cadastrarDisciplina :: Disciplina -> Sistema -> Either String Sistema
cadastrarDisciplina disc sistema
  | not (all (\req -> M.member req (_disciplinas sistema)) (D.getPreRequisitosDisciplina disc)) =
      Left "Um ou mais pré-requisitos informados não estão cadastrados!"
  | otherwise =
      cadastrar D.getCodigoDisciplina _disciplinas (\m s -> s {_disciplinas = m}) "Disciplina" disc sistema

-- | Cadastra uma nova turma no sistema.
cadastrarTurma :: Turma -> Sistema -> Either String Sistema
cadastrarTurma turma sistema
  | not (M.member (T.getProfessorTurma turma) (_professores sistema)) =
      Left "Erro: Professor não cadastrado!"

  | not (M.member (T.getDisciplinaTurma turma) (_disciplinas sistema)) =
      Left "Erro: Disciplina não existe!"

  | checarConflitoGeral turma sistema =
      Left "Erro: Conflito de horário! Já existe uma turma neste slot."

  | otherwise =
      cadastrar T.getCodigoTurma _turmas (\m s -> s {_turmas = m}) "Turma" turma sistema

-------------------------------------------------------------------------------
-- Lógica de Matrícula
-------------------------------------------------------------------------------

-- | Altera a fase do sistema para permitir que alunos enviem solicitações.
abrirPeriodoMatriculas :: Sistema -> Either String Sistema
abrirPeriodoMatriculas s
    | _fase s == 1 = Left "O período já está aberto"
    | M.null (_alunos s)  = Left "Não é possível abrir: nenhum aluno cadastrado."
    | M.null (_turmas s)  = Left "Não é possível abrir: nenhuma turma cadastrada."
    | otherwise = Right s { _fase = 1, _historicoProc = [] }

cadastrarSolicitacao :: Matricula -> Int -> Sistema -> Either String Sistema
cadastrarSolicitacao idA idT sis = do
    aluno <- maybe (Left "Aluno não encontrado!") Right (M.lookup idA (_alunos sis))
    turmaAlvo <- maybe (Left "Turma não encontrada!") Right (M.lookup idT (_turmas sis))
    discAlvo  <- maybe (Left "Disciplina não encontrada!") Right (M.lookup (T.getDisciplinaTurma turmaAlvo) (_disciplinas sis))

    -- 1. Validação de Pré-requisitos
    let concluidas = map A.getCodigoDisciplinaHistorico (A.getHistoricoAluno aluno)
        requisitos = D.getPreRequisitosDisciplina discAlvo
        faltam = filter (`notElem` concluidas) requisitos
    if not (null faltam) then Left $ "Faltam pré-requisitos: " ++ intercalate ", " faltam else Right ()

    -- 2. Validação de Choque de Horário
    let turmasJaPedidas = [ t | s <- _solicitacoes sis, _sMatricula s == idA, (identifier, t) <- M.toList (_turmas sis), identifier == _sTurma s ]
        choque = any (listasHorarioConflitam (T.getHorarioTurma turmaAlvo) . T.getHorarioTurma) turmasJaPedidas
    if choque then Left "Choque de horário com outra solicitação!" else Right ()

    -- 3. Duplicidade
    if any (\s -> _sMatricula s == idA && _sTurma s == idT) (_solicitacoes sis)
       then Left "Solicitação já realizada!"
       else Right sis { _solicitacoes = Solicitacao idA idT : _solicitacoes sis }

-- | Processa todas as solicitações, rankeia por CRA e efetiva as matrículas..
efetivarMatriculas :: Sistema -> Sistema
efetivarMatriculas sis =
    let
        -- 1. Agrupar solicitações por ID de Turma: M.Map idTurma [Matricula]
        mapaSols = M.fromListWith (++) [(_sTurma s, [_sMatricula s]) | s <- _solicitacoes sis]

        -- 2. Decidir quem entra em cada turma
        processarPorTurma idT mats =
            let capacidade = maybe 0 T.getCapacidadeTurma (M.lookup idT (_turmas sis))
                -- Ordena por CRA (Down) e usa Matrícula como desempate
                ordenados = sortBy (comparing (\m -> (Down (getCra m), m))) mats
                getCra m = maybe 0.0 A.getCraAluno (M.lookup m (_alunos sis))
            in take capacidade ordenados

        -- 3. Mapa Final de Aprovados: M.Map idTurma [Matricula]
        aprovadosPorTurma = M.mapWithKey processarPorTurma mapaSols

        -- 4. Atualizar as Turmas com os novos alunos
        novasTurmas = M.mapWithKey (\idT t ->
            let idsAlunos = M.findWithDefault [] idT aprovadosPorTurma
                objetosAlunos = [ al | (mat, al) <- M.toList (_alunos sis), mat `elem` idsAlunos ]
            in T.setAlunosTurma objetosAlunos t) (_turmas sis)

        -- 5. Atualizar os Alunos (inserir turmas aprovadas no campo _notas)
        novosAlunos = M.mapWithKey (\mat al ->
            let turmasDoAluno = [ idT | (idT, lista) <- M.toList aprovadosPorTurma, mat `elem` lista ]
                novasNotas = foldr (`M.insert` A.notasVazias) (A._notas al) turmasDoAluno
            in al { A._notas = novasNotas }) (_alunos sis)

        -- 6. Gerar o histórico de processamento
        gerarResultado sol =
            let mat = _sMatricula sol
                idT = _sTurma sol
                aprovadosNaTurma = M.findWithDefault [] idT aprovadosPorTurma
                status = if mat `elem` aprovadosNaTurma
                         then Aceita
                         else Recusada "Vagas esgotadas (Critério: CRA)"
            in ResultadoProcessamento mat idT status

        historicoFinal = map gerarResultado (_solicitacoes sis)

    in sis { _turmas = novasTurmas
           , _alunos = novosAlunos
           , _solicitacoes = []
           , _historicoProc = historicoFinal
           , _fase = 2 -- Semestre Ativo
           }

-------------------------------------------------------------------------------
-- Notas e Fim de Período
-------------------------------------------------------------------------------

-- | Registra as 3 notas de um aluno em uma turma específica.
lancarNotas :: Matricula -> Int -> Double -> Double -> Double -> Sistema -> Either String Sistema
lancarNotas idA idT nota1 nota2 nota3 sis = do
    aluno <- maybe (Left "Aluno não encontrado!") Right (M.lookup idA (_alunos sis))
    let novasNotasObj = NotasDisciplina (Just nota1) (Just nota2) (Just nota3)
        mapaNotasAtualizado = M.insert idT novasNotasObj (A.getNotasAluno aluno)
        alunoAtualizado = A.atualizarNotas mapaNotasAtualizado aluno
    Right $ sis { _alunos = M.insert idA alunoAtualizado (_alunos sis) }

-- | Processa o fechamento de todas as notas e limpa as solicitações.
finalizarSemestre :: Sistema -> Sistema
finalizarSemestre s = s
    { _alunos       = M.map A.processarFimSemestre (_alunos s)
    , _fase         = 0
    , _solicitacoes = []
    }

-- | Verifica se a nova turma conflita em horário E local com turmas existentes.
-- O conflito só ocorre se (Mesmo Horário) AND (Mesma Sala).
checarConflitoGeral :: Turma -> Sistema -> Bool
checarConflitoGeral novaTurma sistema =
    let turmasAtuais = M.elems (_turmas sistema)

        conflita t =
            let mesmoHorario = listasHorarioConflitam (T.getHorarioTurma t) (T.getHorarioTurma novaTurma)
                mesmaSala    = T.getSalaTurma t == T.getSalaTurma novaTurma
            in mesmoHorario && mesmaSala

    in any conflita turmasAtuais

-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------

-- | Retorna o mapa de alunos do sistema.
getAlunos :: Sistema -> M.Map Int Aluno
getAlunos = _alunos

-- | Retorna o mapa de turmas do sistema.
getTurmas :: Sistema -> M.Map Int Turma
getTurmas = _turmas

-- | Retorna a fase atual do sistema.
getFase :: Sistema -> Int
getFase = _fase

-- | Retorna o mapa de professores do sistema.
getProfessores :: Sistema -> M.Map Int Professor
getProfessores = _professores

-- | Retorna o mapa de disciplinas do sistema.
getDisciplinas :: Sistema -> M.Map String Disciplina
getDisciplinas = _disciplinas