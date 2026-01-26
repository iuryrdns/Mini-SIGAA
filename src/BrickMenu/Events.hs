{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module BrickMenu.Events (handleEvent) where

import Brick
import Brick.Focus (focusGetCurrent, focusNext, focusRing)
import Brick.Widgets.Edit (editor, getEditContents, handleEditorEvent)
import qualified Brick.Widgets.List as L
import BrickMenu.Tipos
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import IOs.Relatorio (gerarRelatorioGeral)
import Lens.Micro (lens, (&), (.~), (^.), Lens')
import Lens.Micro.Mtl (zoom)
import Models.Aluno (criarAluno, getNomeAluno)
import Models.Disciplina (criarDisciplina, getNomeDisciplina)
import Models.Professor (criarProfessor)
import Models.Turma (criarTurma, getCodigoTurma, getDisciplinaTurma)
import Models.Types
import Sistema
import Text.Read (readMaybe)
import Utils.Database (salvarSistema)
import Data.List (nub)

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent e = do
  st <- get
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> handleEsc st
    VtyEvent (V.EvKey (V.KChar '\t') []) -> modify $ \s -> s {_foco = focusNext (s ^. foco)}
    
    VtyEvent (V.EvKey V.KDel []) -> case st ^. telaAtiva of
        TelaListaTurmas -> handleDeletarTurma st
        _ -> return ()
    VtyEvent (V.EvKey V.KEnter []) -> case st ^. telaAtiva of
        TelaListaTurmas -> if getFase (st ^. sistema) == 0 then handleEditarTurmaPre st else return ()
        _ -> handleEnter st

    _ -> case st ^. telaAtiva of
      TelaMenu -> handleListEvent listaMenu e
      TelaListaAlunos -> handleListEvent listaMenuAlunos e
      TelaListaProfessores -> handleListEvent listaMenuProfessores e
      TelaListaTurmas -> handleListEvent listaMenuTurmas e
      TelaListaDisciplinas -> handleListEvent listaMenuDisciplinas e
      TelaListaSolicitacoes -> handleListEvent listaMenuSolicitacoes e
      TelaConflitos -> handleListEvent listaConflitos e
      TelaRelatorio -> handleScrollEvent ScrollRelatorio e
      
      TelaConfirmacao -> case e of
         VtyEvent (V.EvKey (V.KChar 's') []) -> processarTransicaoFase st
         VtyEvent (V.EvKey (V.KChar 'S') []) -> processarTransicaoFase st
         VtyEvent (V.EvKey (V.KChar 'n') []) -> modify $ \s -> s {_telaAtiva = TelaMenu, _mensagemErro = Nothing}
         _ -> return ()
      
      TelaConfirmacaoFimSemestre -> case e of
         VtyEvent (V.EvKey (V.KChar 's') []) -> encerrarSemestre st
         VtyEvent (V.EvKey (V.KChar 'S') []) -> encerrarSemestre st
         VtyEvent (V.EvKey (V.KChar 'n') []) -> modify $ \s -> s {_telaAtiva = TelaMenu}
         _ -> return ()
      
      _ -> case focusGetCurrent (st ^. foco) of
        Just n -> handleEditorGeneric st n e
        Nothing -> return ()

handleListEvent :: Lens.Micro.Lens' AppState (L.List Name a) -> BrickEvent Name e -> EventM Name AppState ()
handleListEvent l e = case e of VtyEvent ev -> zoom l (L.handleListEvent ev); _ -> return ()

handleScrollEvent :: Name -> BrickEvent Name e -> EventM Name AppState ()
handleScrollEvent _ (VtyEvent (V.EvKey k [])) = case k of
    V.KUp -> vScrollBy (viewportScroll ScrollRelatorio) (-1)
    V.KDown -> vScrollBy (viewportScroll ScrollRelatorio) 1
    V.KPageUp -> vScrollBy (viewportScroll ScrollRelatorio) (-5)
    V.KPageDown -> vScrollBy (viewportScroll ScrollRelatorio) 5
    _ -> return ()
handleScrollEvent _ _ = return ()

handleEsc :: AppState -> EventM Name AppState ()
handleEsc st = case st ^. telaAtiva of
  TelaMenu -> halt
  TelaInicial -> halt
  _ -> atualizarMenu (st ^. sistema) Nothing

handleEnter :: AppState -> EventM Name AppState ()
handleEnter st = case st ^. telaAtiva of
  TelaInicial -> atualizarMenu (st ^. sistema) Nothing
  TelaMenu -> handleMenuSelection st
  TelaCadAluno -> finalizarCadastro extrairAluno [EditNomeAluno, EditMatricula, EditCurso, EditCRA]
  TelaCadProfessor -> finalizarCadastro extrairProfessor [EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao]
  TelaCadDisciplina -> finalizarCadastro extrairDisciplina [EditNomeDisciplina, EditCodigoDisciplina, EditRequisitos, EditCursosPermitidos]
  TelaCadTurma -> finalizarCadastro extrairTurma [EditCodTurma, EditProfTurma, EditDiscTurma, EditHorario, EditSala, EditMaxAlunosTurma]
  TelaCadSolicitacao -> finalizarCadastro extrairSolicitacao [EditMatAluno, EditMatTurma]
  TelaLancarNotas -> finalizarCadastro extrairNota [EditNotaMatricula, EditNotaCodDisc, EditNotaValor]
  TelaEditarTurma -> finalizarCadastro extrairEdicaoTurma [EditPendenteSala, EditPendenteHorario]
  TelaRelatorio -> atualizarMenu (st ^. sistema) Nothing
  _ -> return ()

handleMenuSelection :: AppState -> EventM Name AppState ()
handleMenuSelection st = case L.listSelectedElement (st ^. listaMenu) of
  Just (_, "Cadastrar Aluno") -> mudarTela TelaCadAluno [EditNomeAluno, EditMatricula, EditCurso, EditCRA]
  Just (_, "Cadastrar Professor") -> mudarTela TelaCadProfessor [EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao]
  Just (_, "Cadastrar Disciplina") -> mudarTela TelaCadDisciplina [EditCodigoDisciplina, EditNomeDisciplina, EditRequisitos, EditCursosPermitidos]
  Just (_, "Cadastrar Turma") -> mudarTela TelaCadTurma [EditCodTurma, EditProfTurma, EditDiscTurma, EditHorario, EditSala, EditMaxAlunosTurma]
  
  Just (_, "Gerenciar Turmas Pendentes") -> carregarListaTurmas st
  Just (_, "Verificar Conflitos") -> verificarConflitos st

  Just (_, "Listar Alunos") -> do
    let todosAlunos = M.elems (_alunos (st ^. sistema))
    modify $ \s -> s {_telaAtiva = TelaListaAlunos, _listaMenuAlunos = L.list ListaAlunos (Vec.fromList todosAlunos) 1}
  Just (_, "Listar Professores") -> do
    let todosProfs = M.elems (_professores (st ^. sistema))
    modify $ \s -> s {_telaAtiva = TelaListaProfessores, _listaMenuProfessores = L.list ListaProfessores (Vec.fromList todosProfs) 1}
  Just (_, "Listar Disciplinas") -> do
    let todasDiscs = M.elems (_disciplinas (st ^. sistema))
    modify $ \s -> s {_telaAtiva = TelaListaDisciplinas, _listaMenuDisciplinas = L.list ListaDisciplinas (Vec.fromList todasDiscs) 1}
  Just (_, "Listar Turmas Oficiais") -> carregarListaTurmas st

  Just (_, "Cadastrar Matrícula") -> mudarTela TelaCadSolicitacao [EditMatAluno, EditMatTurma]
  Just (_, "Cadastrar Rematrícula") -> mudarTela TelaCadSolicitacao [EditMatAluno, EditMatTurma]
  Just (_, "Mostrar Matrículas") -> mostrarSolicitacoes (_matriculas (st ^. sistema))
  Just (_, "Mostrar Rematrículas") -> mostrarSolicitacoes (_rematriculas (st ^. sistema))
  Just (_, "Encerrar Matrículas") -> modify $ \s -> s {_telaAtiva = TelaConfirmacao}
  Just (_, "Encerrar Rematrículas") -> modify $ \s -> s {_telaAtiva = TelaConfirmacao}

  Just (_, "Lançar Notas") -> mudarTela TelaLancarNotas [EditNotaMatricula, EditNotaCodDisc, EditNotaValor]
  Just (_, "Encerrar Semestre") -> modify $ \s -> s {_telaAtiva = TelaConfirmacaoFimSemestre}

  Just (_, "Visualizar Relatório Geral") -> do
    let rel = gerarRelatorioGeral (st ^. sistema)
    modify $ \s -> s {_telaAtiva = TelaRelatorio, _textoRelatorio = rel}

  Just (_, "Sair") -> halt
  _ -> return ()

verificarConflitos :: AppState -> EventM Name AppState ()
verificarConflitos st = do
    let conflitos = getTurmasConflitantes (st ^. sistema)
    if null conflitos
        then do
             let sisEfetivado = efetivarAlteracoes (st ^. sistema)
             case abrirPeriodoMatriculas sisEfetivado of
                 Left err -> modify $ \s -> s {_mensagemErro = Just err}
                 Right sisFase1 -> do
                     liftIO $ salvarSistema sisFase1
                     atualizarMenu sisFase1 (Just "Sem conflitos! Fase 0 encerrada. Matrículas Abertas.")
        else do
             modify $ \s -> s { _telaAtiva = TelaConflitos
                              , _listaConflitos = L.list ListaConflitos (Vec.fromList conflitos) 1
                              , _mensagemErro = Just "Conflitos detectados! Remova turmas pendentes." }

handleDeletarTurma :: AppState -> EventM Name AppState ()
handleDeletarTurma st = do
    case L.listSelectedElement (st ^. listaMenuTurmas) of
        Just (_, turma) -> do
            if getFase (st ^. sistema) == 0
                then case removerTurmaPendente (getCodigoTurma turma) (st ^. sistema) of
                    Left err -> modify $ \s -> s {_mensagemErro = Just err}
                    Right novoSis -> do
                        liftIO $ salvarSistema novoSis
                        modify $ \s -> s {_sistema = novoSis} 
                        carregarListaTurmas st 
                        modify $ \s -> s {_mensagemErro = Just "Turma removida."}
                else modify $ \s -> s {_mensagemErro = Just "Só é possível remover turmas na Fase 0."}
        Nothing -> return ()

handleEditarTurmaPre :: AppState -> EventM Name AppState ()
handleEditarTurmaPre st = do
    case L.listSelectedElement (st ^. listaMenuTurmas) of
        Just (_, turma) -> do
             modify $ \s -> s { _telaAtiva = TelaEditarTurma
                              , _turmaEmEdicao = Just (getCodigoTurma turma)
                              , _foco = focusRing [EditPendenteSala, EditPendenteHorario]
                              , _mensagemErro = Just ("Editando Turma " ++ unCodigo (getCodigoTurma turma)) }
        Nothing -> return ()

processarTransicaoFase :: AppState -> EventM Name AppState ()
processarTransicaoFase st = do
    let sis = st ^. sistema
    case getFase sis of
        1 -> do
            let (deferidasMap, indeferidasMap) = obterMatriculasProcessadas sis
            let deferidas = [(m, t) | (t, ms) <- M.toList deferidasMap, m <- ms]
            let indeferidas = [(m, t) | (t, ms) <- M.toList indeferidasMap, m <- ms]
            
            let sisProc = processarListaMatriculas sis (_matriculas sis)
            let sisFase2 = setFase sisProc {_matriculas = deferidas} 2
            
            liftIO $ salvarSistema sisFase2
            
            let relatorio = "--- RESULTADO DA 1ª ETAPA ---\n\n" ++
                            "MATRÍCULAS DEFERIDAS:\n" ++ formatarMatriculasUI sisFase2 deferidas ++
                            "\n\nMATRÍCULAS INDEFERIDAS (Necessário Rematrícula):\n" ++ formatarMatriculasUI sisFase2 indeferidas
            
            modify $ \s -> s { _sistema = sisFase2, _telaAtiva = TelaRelatorio, _textoRelatorio = relatorio }
            
        2 -> do
            let (deferidasMap, indeferidasMap) = obterMatriculasProcessadas sis
            let deferidas = [(m, t) | (t, ms) <- M.toList deferidasMap, m <- ms]
            let indeferidas = [(m, t) | (t, ms) <- M.toList indeferidasMap, m <- ms]
            
            let sisProc = processarListaMatriculas sis (_rematriculas sis)
            let todasMatriculas = nub (_matriculas sis ++ deferidas)
            let sisFinal = sisProc { _matriculas = todasMatriculas, _rematriculas = [], _fase = 4 }
            
            liftIO $ salvarSistema sisFinal
            
            let relatorio = "--- RESULTADO FINAL DAS REMATRÍCULAS ---\n\n" ++
                            "NOVAS MATRÍCULAS:\n" ++ formatarMatriculasUI sisFinal deferidas ++
                            "\n\nINDEFERIDAS (Sem vaga):\n" ++ formatarMatriculasUI sisFinal indeferidas ++
                            "\n\n--- SEMESTRE INICIADO (FASE 4) ---"
            
            modify $ \s -> s { _sistema = sisFinal, _telaAtiva = TelaRelatorio, _textoRelatorio = relatorio }
            
        _ -> modify $ \s -> s {_mensagemErro = Just "Transição inválida."}

mudarTela :: Tela -> [Name] -> EventM Name AppState ()
mudarTela tela camposFoco = modify $ \s -> s {_telaAtiva = tela, _foco = focusRing camposFoco, _mensagemErro = Nothing}

carregarListaTurmas :: AppState -> EventM Name AppState ()
carregarListaTurmas st = do
    let fase = getFase (st ^. sistema)
    let turmas = if fase == 0 
                 then M.elems (_cadastroDeTurmas (st ^. sistema))
                 else M.elems (_turmas (st ^. sistema))
    modify $ \s -> s {_telaAtiva = TelaListaTurmas, _listaMenuTurmas = L.list ListaTurmas (Vec.fromList turmas) 1}

mostrarSolicitacoes :: [(Matricula, Codigo)] -> EventM Name AppState ()
mostrarSolicitacoes lista = modify $ \s -> s {_telaAtiva = TelaListaSolicitacoes, _listaMenuSolicitacoes = L.list ListaSolicitacoes (Vec.fromList lista) 1}

atualizarMenu :: Sistema -> Maybe String -> EventM Name AppState ()
atualizarMenu sis msg = do
    let fase = getFase sis
    let opcoes = case fase of
            0 -> [ "Cadastrar Aluno", "Cadastrar Professor", "Cadastrar Disciplina", "Cadastrar Turma"
                 , "Gerenciar Turmas Pendentes", "Verificar Conflitos"
                 , "Listar Alunos", "Listar Professores", "Listar Disciplinas", "Visualizar Relatório Geral", "Sair"]
            1 -> [ "Cadastrar Matrícula", "Mostrar Matrículas", "Listar Turmas Oficiais"
                 , "Visualizar Relatório Geral", "Encerrar Matrículas", "Sair"]
            2 -> [ "Cadastrar Rematrícula", "Mostrar Rematrículas", "Listar Turmas Oficiais"
                 , "Visualizar Relatório Geral", "Encerrar Rematrículas", "Sair"]
            4 -> [ "Lançar Notas", "Visualizar Relatório Geral", "Encerrar Semestre", "Sair"]
            _ -> ["Sair"]
    
    modify $ \s -> s { _sistema = sis
                     , _listaMenu = L.list MenuPrincipal (Vec.fromList opcoes) 1
                     , _telaAtiva = TelaMenu
                     , _mensagemErro = msg
                     , _foco = focusRing []
                     }

encerrarSemestre :: AppState -> EventM Name AppState ()
encerrarSemestre st = do
    case iniciarNovoSemestre (st ^. sistema) of
        Left err -> modify $ \s -> s {_mensagemErro = Just err}
        Right novoSis -> do
            liftIO $ salvarSistema novoSis
            atualizarMenu novoSis (Just "Semestre encerrado. Voltando à Fase 0.")

formatarMatriculasUI :: Sistema -> [(Matricula, Codigo)] -> String
formatarMatriculasUI sis lista = 
    let linhas = map (\(m, t) -> 
            let aluno = case M.lookup m (_alunos sis) of Just a -> unNome (getNomeAluno a); Nothing -> show (unMatricula m)
                turma = unCodigo t
                disc = case M.lookup t (_turmas sis) of 
                        Just tr -> let dCod = getDisciplinaTurma tr in case M.lookup dCod (_disciplinas sis) of
                            Just d -> unNome (getNomeDisciplina d)
                            Nothing -> ""
                        Nothing -> ""
            in aluno ++ " -> " ++ disc ++ " (" ++ turma ++ ")") lista
    in unlines linhas

handleEditorGeneric :: AppState -> Name -> BrickEvent Name e -> EventM Name AppState ()
handleEditorGeneric st n e =
  let campoLens = lens (\s -> M.findWithDefault (editor n (Just 1) "") n (s ^. formularios))
                       (\s ed -> s {_formularios = M.insert n ed (s ^. formularios)})
   in zoom campoLens (handleEditorEvent e)

getCampo :: Name -> AppState -> String
getCampo n st = case M.lookup n (st ^. formularios) of Just ed -> concat $ getEditContents ed; Nothing -> ""

resetCampos :: [Name] -> AppState -> AppState
resetCampos nomes s = let formsLimpis = foldr (\n m -> M.insert n (editor n (Just 1) "") m) (s ^. formularios) nomes in s { _formularios = formsLimpis }

finalizarCadastro :: (AppState -> Either String Sistema) -> [Name] -> EventM Name AppState ()
finalizarCadastro extrator campos = do
  st <- get
  case extrator st of
    Left erro -> modify $ \s -> s {_mensagemErro = Just erro}
    Right novoSistema -> do
      liftIO $ salvarSistema novoSistema
      modify $ \s -> resetCampos campos s
      atualizarMenu novoSistema (Just "Operação realizada com sucesso!")

extrairAluno :: AppState -> Either String Sistema
extrairAluno st = do
  mVal <- maybe (Left "Matrícula inválida") Right (readMaybe $ getCampo EditMatricula st)
  craVal <- maybe (Left "CRA inválido") Right (readMaybe $ getCampo EditCRA st)
  cra <- maybe (Left "CRA 0-10") Right (mkCRA craVal)
  cadastrarAluno (criarAluno (Matricula mVal) (Nome $ getCampo EditNomeAluno st) (Curso $ getCampo EditCurso st) cra) (st ^. sistema)

extrairProfessor :: AppState -> Either String Sistema
extrairProfessor st = do
  mVal <- maybe (Left "Matrícula inválida") Right (readMaybe $ getCampo EditMatriculaProfessor st)
  cadastrarProfessor (criarProfessor (Matricula mVal) (Nome $ getCampo EditNomeProfessor st) (getCampo EditDepto st) (getCampo EditFormacao st)) (st ^. sistema)

extrairDisciplina :: AppState -> Either String Sistema
extrairDisciplina st = do
  let c = getCampo EditCodigoDisciplina st
  if null c then Left "Código obrigatório" else
    cadastrarDisciplina (criarDisciplina (Codigo c) (Nome $ getCampo EditNomeDisciplina st) (map Codigo $ words $ getCampo EditRequisitos st) (map Curso $ words $ getCampo EditCursosPermitidos st)) (st ^. sistema)

extrairTurma :: AppState -> Either String Sistema
extrairTurma st = do
  mProf <- maybe (Left "Mat. Prof inválida") Right (readMaybe $ getCampo EditProfTurma st)
  cap <- maybe (Left "Capacidade inválida") Right (readMaybe $ getCampo EditMaxAlunosTurma st)
  cadastrarTurma (criarTurma (Codigo $ getCampo EditCodTurma st) (Matricula mProf) (Codigo $ getCampo EditDiscTurma st) (lerHorario $ getCampo EditHorario st) (getCampo EditSala st) cap) (st ^. sistema)

extrairSolicitacao :: AppState -> Either String Sistema
extrairSolicitacao st = do
  mAluno <- maybe (Left "Mat. Aluno inválida") Right (readMaybe $ getCampo EditMatAluno st)
  realizarMatricula (Matricula mAluno) (Codigo $ getCampo EditMatTurma st) (st ^. sistema)

extrairNota :: AppState -> Either String Sistema
extrairNota st = do
  mAluno <- maybe (Left "Mat. Aluno inválida") Right (readMaybe $ getCampo EditNotaMatricula st)
  val <- maybe (Left "Nota inválida") Right (readMaybe $ getCampo EditNotaValor st)
  adicionarNotasSistema (Matricula mAluno) (Codigo $ getCampo EditNotaCodDisc st) val (st ^. sistema)

extrairEdicaoTurma :: AppState -> Either String Sistema
extrairEdicaoTurma st = do
    case st ^. turmaEmEdicao of
        Nothing -> Left "Nenhuma turma selecionada."
        Just cod -> do
            let sala = getCampo EditPendenteSala st
            let horario = getCampo EditPendenteHorario st
            let s = if null sala then Nothing else Just sala
            let h = if null horario then Nothing else Just horario
            editarTurmaPendente cod s h (st ^. sistema)