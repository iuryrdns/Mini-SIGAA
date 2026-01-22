module Sistema where

import Data.List (sortBy)
import Data.Map as Map (Map, elems, empty, findWithDefault, insert, member, toList, union, (!))
import qualified Data.Map as M
import Data.Ord (Down (..), comparing)
import Models.Aluno (Aluno, getCraAluno, getCursoAluno, getDisciplinasConcluidas, getMatriculaAluno, getNomeAluno)
import Models.Disciplina (Disciplina, getCodigoDisciplina, getCursosDisciplina, getNomeDisciplina, getRequisitosDisciplina)
import Models.Horario (temInterseccao)
import Models.Professor (Professor, getMatriculaProfessor)
import Models.Turma (Turma, getCapacidadeTurma, getCodigoTurma, getDisciplinaTurma, getHorarioTurma, getProfessorTurma, getSalaTurma)
import Models.Types (Codigo (..), Curso, Matricula (..), Nome, unCRA, unCodigo, unMatricula, unNome)
import System.Directory (doesDirectoryExist, doesFileExist)
import Text.Read (readMaybe)

data Sistema = Sistema
  { _alunos :: Map.Map Matricula Aluno,
    _professores :: Map.Map Matricula Professor,
    _disciplinas :: Map.Map Codigo Disciplina,
    _matriculas :: [(Matricula, Int)],
    _turmas :: Map.Map Int Turma,
    _cadastroDeTurmas :: Map.Map Int Turma,
    _fase :: Int
  }
  deriving (Show, Read)

dbPath :: String
dbPath = "dados.db"

sistemaVazio :: Sistema
sistemaVazio =
  Sistema
    { _alunos = Map.empty,
      _professores = Map.empty,
      _disciplinas = Map.empty,
      _matriculas = [],
      _turmas = Map.empty,
      _cadastroDeTurmas = Map.empty,
      _fase = 0
    }

efetivarAlteracoes :: Sistema -> Sistema
efetivarAlteracoes sistema =
  sistema
    { _turmas = Map.union (_turmas sistema) (_cadastroDeTurmas sistema),
      _cadastroDeTurmas = Map.empty
    }

cadastrar :: (Ord i) => (v -> i) -> (Sistema -> Map.Map i v) -> (Map.Map i v -> Sistema -> Sistema) -> String -> v -> Sistema -> Either String Sistema
cadastrar getId getMap updateSystem nomeEntidade item sistema =
  let chave = getId item
      mapaAtual = getMap sistema
   in if Map.member chave mapaAtual
        then
          Left (nomeEntidade ++ " ja Cadastrado!")
        else
          let novoMapa = Map.insert chave item mapaAtual
              novoSistema = updateSystem novoMapa sistema
           in Right novoSistema

abrirPeriodoMatriculas :: Sistema -> Either String Sistema
abrirPeriodoMatriculas sistema =
  if _fase sistema == 1
    then
      Left "Matriculas ja estao abertas"
    else
      Right sistema {_fase = 1}

realizarMatricula :: Matricula -> Int -> Sistema -> Either String Sistema
realizarMatricula matricula idTurma sistema
  | not (Map.member matricula (_alunos sistema)) = Left "Aluno não cadastrado"
  | not (Map.member idTurma (_turmas sistema)) = Left "Turma não cadastrada"
  | cursoAluno `notElem` cursosPermitidos = Left "Disciplina não disponível para o curso do aluno"
  | not (all (`elem` disciplinasConcluidas) requisitos) = Left "Aluno não cumpre os pré-requisitos"
  | (matricula, idTurma) `elem` _matriculas sistema = Left "Matricula ja realizada!"
  | otherwise =
      Right sistema {_matriculas = (matricula, idTurma) : _matriculas sistema}
  where
    turmaEncontrada = _turmas sistema ! idTurma
    aluno = _alunos sistema ! matricula
    cursoAluno = getCursoAluno aluno
    disciplina = _disciplinas sistema ! getDisciplinaTurma turmaEncontrada
    cursosPermitidos = getCursosDisciplina disciplina
    disciplinasConcluidas = getDisciplinasConcluidas aluno
    requisitos = map unCodigo (getRequisitosDisciplina disciplina)

finalizarPeriodoMatriculas :: Sistema -> Sistema
finalizarPeriodoMatriculas = 

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
      cadastrar getCodigoTurma _turmas (\m s -> s {_cadastroDeTurmas = m}) "Turma" turma sistema

verificarRequisitos :: [String] -> Sistema -> Either String [String]
verificarRequisitos requisitos sistema = mapM verificar requisitos
  where
    mapaDisciplinas = _disciplinas sistema
    verificar codigoStr =
      let codigo = Codigo codigoStr
       in if Map.member codigo mapaDisciplinas
            then Right codigoStr
            else Left ("A disciplina requisito '" ++ codigoStr ++ "' nao existe!")

getAlunos :: Sistema -> Map.Map Matricula Aluno
getAlunos = _alunos

getProfessores :: Sistema -> Map.Map Matricula Professor
getProfessores = _professores

getDisciplinas :: Sistema -> Map.Map Codigo Disciplina
getDisciplinas = _disciplinas

getTurmas :: Sistema -> Map.Map Int Turma
getTurmas = _turmas

getTurmasCadastradas :: Sistema -> Map.Map Int Turma
getTurmasCadastradas = _cadastroDeTurmas

getFase :: Sistema -> Int
getFase = _fase

getMatriculasRealizadas :: Sistema -> Either String String
getMatriculasRealizadas sistema
  | null (_matriculas sistema) = Left "Não há nenhuma matrícula!"
  | otherwise = Right relatorioMatriculas
  where
    listaMatriculas = _matriculas sistema

    lista = zip [1 ..] listaMatriculas

    montarLinha (idx, (idAluno, idTurma)) =
      let aluno = _alunos sistema ! idAluno
          turma = _turmas sistema ! idTurma
          codDisc = getDisciplinaTurma turma
          disciplina = _disciplinas sistema ! codDisc

          nomeAluno = unNome (getNomeAluno aluno)
          matrAluno = show (unMatricula idAluno)
          nomeDisc = unNome (getNomeDisciplina disciplina)
          codTurma = show (getCodigoTurma turma)
       in show idx ++ ". " ++ nomeAluno ++ " - " ++ matrAluno ++ ": " ++ nomeDisc ++ " " ++ codTurma
    relatorioMatriculas = unlines (map montarLinha lista)

getTurmasConflitantes :: Sistema -> [(Turma, Turma)]
getTurmasConflitantes sistema =
  let turmas = M.elems (_cadastroDeTurmas sistema)
   in [ (t1, t2) | t1 <- turmas, t2 <- turmas, getCodigoTurma t1 < getCodigoTurma t2, getSalaTurma t1 /= getSalaTurma t2, temInterseccao (getHorarioTurma t1) (getHorarioTurma t2)
      ]

compararAlunos :: Sistema -> Matricula -> Matricula -> Ordering
compararAlunos sistema a1 a2 =
  maisNota <> ordemMatricula
  where
    getNota :: Matricula -> Float
    getNota mat = maybe 0.0 (unCRA . getCraAluno) (M.lookup mat (_alunos sistema))
    maisNota = comparing (Down . getNota) a1 a2
    ordemMatricula = comparing unMatricula a1 a2

processarMatriculas :: Sistema -> (M.Map Int [Matricula], M.Map Int [Matricula])
processarMatriculas sistema =
  let turmas = M.fromListWith (++) [(v, [k]) | (k, v) <- _matriculas sistema]
      processarTurma k vs =
        let capacidade = maybe 0 getCapacidadeTurma (M.lookup k (_turmas sistema))
            alunosOrdenados = sortBy (compararAlunos sistema) vs
         in splitAt capacidade alunosOrdenados

      turmasProcessadas = M.mapWithKey processarTurma turmas
   in (M.map fst turmasProcessadas, M.map snd turmasProcessadas)