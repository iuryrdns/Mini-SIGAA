module Sistema where

import Data.List (sortBy)
import Data.Map as Map (Map, delete, elems, empty, findWithDefault, insert, member, toList, union, (!), lookup)
import qualified Data.Map as M
import Data.Ord (Down (..), comparing)
import Models.Aluno (Aluno(..), getCraAluno, getCursoAluno, getDisciplinasConcluidas, getMatriculaAluno, getNomeAluno, adicionarNota, getNotasAluno)
import Models.Disciplina (Disciplina, getCodigoDisciplina, getCursosDisciplina, getNomeDisciplina, getRequisitosDisciplina)
import Models.Professor (Professor, getMatriculaProfessor)
import Models.Turma (Turma, adicionarAlunoTurma, getAlunosTurma, getCapacidadeTurma, getCodigoTurma, getDisciplinaTurma, getHorarioTurma, getProfessorTurma, getSalaTurma, limparAlunosTurma, setHorarioTurma, setSalaTurma)
import Models.Types (Codigo (..), Curso, Matricula (..), Nome, horarioTemInterseccao, unCRA, unCodigo, unMatricula, unNome)
import System.Directory (doesDirectoryExist, doesFileExist)
import Text.Read (readMaybe)

data Sistema = Sistema
  { _alunos :: Map.Map Matricula Aluno,
    _professores :: Map.Map Matricula Professor,
    _disciplinas :: Map.Map Codigo Disciplina,
    _matriculas :: [(Matricula, Codigo)], 
    _rematriculas :: [(Matricula, Codigo)], 
    _turmas :: Map.Map Codigo Turma, 
    _cadastroDeTurmas :: Map.Map Codigo Turma,
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
      _rematriculas = [],
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
abrirPeriodoMatriculas sistema
  | _fase sistema == 0 = Right sistema {_fase = 1, _matriculas = [], _rematriculas = []}
  | otherwise = Left "O sistema precisa estar na Fase 0 (Planejamento) para abrir matrículas."

abrirPeriodoRematriculas :: Sistema -> Either String Sistema
abrirPeriodoRematriculas sistema
  | _fase sistema == 1 = Right sistema {_fase = 2}
  | otherwise = Left "O sistema precisa estar na Fase 1 (Matrícula) para abrir rematrículas."

finalizarSemestre :: Sistema -> Either String Sistema
finalizarSemestre sistema
  | _fase sistema == 2 = Right sistema {_fase = 3}
  | otherwise = Left "O sistema precisa estar na Fase 2 (Rematrícula) para finalizar."

iniciarNovoSemestre :: Sistema -> Either String Sistema
iniciarNovoSemestre sistema =
  case _fase sistema of
    4 -> 
      let turmasLimpas = M.map limparAlunosTurma (_turmas sistema)
       in Right sistema {_fase = 0, _matriculas = [], _rematriculas = [], _turmas = turmasLimpas}
    0 -> Left "Sistema já está em fase de planejamento!"
    _ -> Left "É necessário finalizar o semestre (Fase 4) antes de iniciar um novo."

realizarMatricula :: Matricula -> Codigo -> Sistema -> Either String Sistema
realizarMatricula matricula codigoTurma sistema
  | not (Map.member matricula (_alunos sistema)) = Left "Aluno não cadastrado"
  | not (Map.member codigoTurma (_turmas sistema)) = Left "Turma não cadastrada"
  | alunoJaNaTurma = Left "Aluno já está matriculado nesta turma!"
  | cursoAluno `notElem` cursosPermitidos = Left "Disciplina não disponível para o curso do aluno"
  | not (all (`elem` disciplinasConcluidas) requisitos) = Left "Aluno não cumpre os pré-requisitos"
  | (matricula, codigoTurma) `elem` _matriculas sistema = Left "Aluno já matriculado!"
  | (matricula, codigoTurma) `elem` _rematriculas sistema = Left "Aluno já matriculado!"
  | _fase sistema == 2 = 
      Right sistema {_rematriculas = (matricula, codigoTurma) : _rematriculas sistema}
  | otherwise =
      Right sistema {_matriculas = (matricula, codigoTurma) : _matriculas sistema}
  where
    turmaEncontrada = _turmas sistema ! codigoTurma
    aluno = _alunos sistema ! matricula
    alunosNaTurma = getAlunosTurma turmaEncontrada
    alunoJaNaTurma = matricula `elem` map getMatriculaAluno alunosNaTurma
    cursoAluno = getCursoAluno aluno
    disciplina = _disciplinas sistema ! getDisciplinaTurma turmaEncontrada
    cursosPermitidos = getCursosDisciplina disciplina
    disciplinasConcluidas = getDisciplinasConcluidas aluno
    requisitos = map unCodigo (getRequisitosDisciplina disciplina)

finalizarPeriodoMatriculas :: Sistema -> Sistema
finalizarPeriodoMatriculas = id

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
  | Map.member (getCodigoTurma turma) (_turmas sistema) = Left "Turma ja Cadastrada!"
  | Map.member (getCodigoTurma turma) (_cadastroDeTurmas sistema) = Left "A solicitação de Cadastro para essa turma ja foi Efetuado!"
  | otherwise =
      let novoCadastro = Map.insert (getCodigoTurma turma) turma (_cadastroDeTurmas sistema)
       in Right sistema {_cadastroDeTurmas = novoCadastro}

editarTurmaPendente :: Codigo -> Maybe String -> Maybe String -> Sistema -> Either String Sistema
editarTurmaPendente codigoTurma novaSala novoHorario sistema
  | not (Map.member codigoTurma (_cadastroDeTurmas sistema)) = Left "Turma não encontrada nas operações pendentes"
  | otherwise =
      let turmaAtual = _cadastroDeTurmas sistema ! codigoTurma
          turmaComSala = maybe turmaAtual (setSalaTurma turmaAtual) novaSala
          turmaAtualizada = maybe turmaComSala (setHorarioTurma turmaComSala) novoHorario
          novoCadastro = Map.insert codigoTurma turmaAtualizada (_cadastroDeTurmas sistema)
       in Right sistema {_cadastroDeTurmas = novoCadastro}

removerTurmaPendente :: Codigo -> Sistema -> Either String Sistema
removerTurmaPendente codigoTurma sistema
  | not (Map.member codigoTurma (_cadastroDeTurmas sistema)) = Left "Turma não encontrada nas operações pendentes"
  | otherwise = Right sistema {_cadastroDeTurmas = Map.delete codigoTurma (_cadastroDeTurmas sistema)}

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

getTurmas :: Sistema -> Map.Map Codigo Turma
getTurmas = _turmas

getTurmasCadastradas :: Sistema -> Map.Map Codigo Turma
getTurmasCadastradas = _cadastroDeTurmas

setFase :: Sistema -> Int -> Sistema
setFase sistema fase = sistema {_fase = fase}

getFase :: Sistema -> Int
getFase = _fase

getMatriculasRealizadas :: Sistema -> Either String String
getMatriculasRealizadas sistema
  | null listaMatriculas = Left mensagemErro
  | otherwise = Right relatorioMatriculas
  where
    (listaMatriculas, mensagemErro) = if _fase sistema == 2
                                      then (_rematriculas sistema, "Não há nenhuma rematrícula!")
                                      else (_matriculas sistema, "Não há nenhuma matrícula!")

    lista = zip [1 ..] listaMatriculas

    montarLinha (idx, (idAluno, idTurma)) =
      let aluno = _alunos sistema ! idAluno
          turma = _turmas sistema ! idTurma
          codDisc = getDisciplinaTurma turma
          disciplina = _disciplinas sistema ! codDisc

          nomeAluno = unNome (getNomeAluno aluno)
          matrAluno = show (unMatricula idAluno)
          nomeDisc = unNome (getNomeDisciplina disciplina)
          codTurma = unCodigo (getCodigoTurma turma)
       in show idx ++ ". " ++ nomeAluno ++ " - " ++ matrAluno ++ ": " ++ nomeDisc ++ " " ++ codTurma
    relatorioMatriculas = unlines (map montarLinha lista)

getTurmasConflitantes :: Sistema -> [(Turma, Turma)]
getTurmasConflitantes sistema =
  let turmasNovas = M.elems (_cadastroDeTurmas sistema)
      turmasExistentes = M.elems (_turmas sistema)
      temConflito t1 t2 = getSalaTurma t1 == getSalaTurma t2 && horarioTemInterseccao (getHorarioTurma t1) (getHorarioTurma t2)
   in [(t1, t2) | t1 <- turmasNovas, t2 <- turmasExistentes, temConflito t1 t2] ++
      [(t1, t2) | t1 <- turmasNovas, t2 <- turmasNovas, getCodigoTurma t1 < getCodigoTurma t2, temConflito t1 t2]

compararAlunos :: Sistema -> Matricula -> Matricula -> Ordering
compararAlunos sistema a1 a2 =
  maisNota <> ordemMatricula
  where
    getNota :: Matricula -> Float
    getNota mat = maybe 0.0 (unCRA . getCraAluno) (M.lookup mat (_alunos sistema))
    maisNota = comparing (Down . getNota) a1 a2
    ordemMatricula = comparing unMatricula a1 a2

obterMatriculasProcessadas :: Sistema -> (M.Map Codigo [Matricula], M.Map Codigo [Matricula])
obterMatriculasProcessadas sistema =
  let todasMatriculas = if _fase sistema == 2 
                        then _rematriculas sistema
                        else _matriculas sistema
      turmas = M.fromListWith (++) [(v, [k]) | (k, v) <- todasMatriculas]
      
      processarTurma k vs =
        case M.lookup k (_turmas sistema) of
          Nothing -> ([], vs)
          Just turma ->
            let capacidade = getCapacidadeTurma turma
                numAlunosAtuais = length (getAlunosTurma turma)
                vagasDisponiveis = capacidade - numAlunosAtuais
                alunosOrdenados = sortBy (compararAlunos sistema) vs
             in splitAt vagasDisponiveis alunosOrdenados

      turmasProcessadas = M.mapWithKey processarTurma turmas
   in (M.map fst turmasProcessadas, M.map snd turmasProcessadas)

processarListaMatriculas :: Sistema -> [(Matricula, Codigo)] -> Sistema
processarListaMatriculas sistema listaMatriculas =
  let turmas = M.fromListWith (++) [(v, [k]) | (k, v) <- listaMatriculas]
      
      tentarAdicionarAluno turmaAtual matriculaAluno =
        case M.lookup matriculaAluno (_alunos sistema) of
          Nothing -> turmaAtual
          Just aluno ->
            let capacidade = getCapacidadeTurma turmaAtual
                numAlunosAtuais = length (getAlunosTurma turmaAtual)
             in if numAlunosAtuais < capacidade
                then adicionarAlunoTurma turmaAtual aluno
                else turmaAtual
      
      processarTurma turmaId matriculasAlunos turmasAtuais =
        case M.lookup turmaId turmasAtuais of
          Nothing -> turmasAtuais
          Just turma ->
            let 
                alunosOrdenados = sortBy (compararAlunos sistema) matriculasAlunos
                turmaAtualizada = foldl tentarAdicionarAluno turma alunosOrdenados
             in M.insert turmaId turmaAtualizada turmasAtuais
      
      turmasAtualizadas = M.foldrWithKey processarTurma (_turmas sistema) turmas
      
   in sistema { _turmas = turmasAtualizadas }

processarMatriculas :: Sistema -> Sistema
processarMatriculas sistema =
  let todasMatriculas = if _fase sistema == 2 
                        then _rematriculas sistema
                        else _matriculas sistema
   in processarListaMatriculas sistema todasMatriculas

adicionarNotasSistema :: Matricula -> Codigo -> Int -> Sistema -> Either String Sistema
adicionarNotasSistema matricula codigo nota sistema =
  case Map.lookup matricula (_alunos sistema) of
    Nothing -> Left "Aluno não encontrado!"
    Just aluno ->
      if not (Map.member codigo (_disciplinas sistema))
        then Left "Disciplina não encontrada!"
        else
          let 
              alunoComNota = adicionarNota aluno codigo nota
              notasDaDisciplina = Map.findWithDefault [] codigo (getNotasAluno alunoComNota)
              
              soma = sum notasDaDisciplina
              qtd = length notasDaDisciplina
              
              media = if qtd > 0 
                      then fromIntegral soma / fromIntegral qtd 
                      else 0.0
              
              codigoStr = unCodigo codigo
              jaConcluiu = codigoStr `elem` _disciplinasConcluidas alunoComNota
              alunoAtualizado = 
                if media >= 7.0 && qtd >= 3 && not jaConcluiu
                  then alunoComNota { _disciplinasConcluidas = codigoStr : _disciplinasConcluidas alunoComNota }
                  else alunoComNota
              
              novosAlunos = Map.insert matricula alunoAtualizado (_alunos sistema)
           in Right sistema { _alunos = novosAlunos }