{-# LANGUAGE OverloadedStrings #-}

module BrickMenu.Events (handleEvent) where

import Brick
import Brick.Widgets.Edit (handleEditorEvent, getEditContents, editor)
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import Brick.Focus (focusGetCurrent, focusNext, focusRing)
import Lens.Micro ((^.), (.~), (&), lens)
import Lens.Micro.Mtl (zoom)
import Control.Monad.IO.Class (liftIO)
import Text.Read (readMaybe)
import qualified Data.Map as M
import qualified Data.Vector as Vec
import System.Exit (exitSuccess)

import BrickMenu.Tipos
import Sistema
import Models.Types
import Models.Aluno (criarAluno)
import Models.Professor (criarProfessor)
import Models.Disciplina (criarDisciplina)
import Models.Turma (criarTurma, getCodigoTurma, getSalaTurma)
import Utils.Database (salvarSistema)
import IOs.Relatorio (gerarRelatorioGeral)

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent e = do
    st <- get
    case e of
        -- 1. TRATAMENTO DE TECLAS ESPECÍFICAS
        VtyEvent (V.EvKey V.KEsc []) -> handleEsc st
        
        VtyEvent (V.EvKey (V.KChar '\t') []) -> 
            modify $ \s -> s { _foco = focusNext (s^.foco) }

        VtyEvent (V.EvKey V.KEnter []) -> handleEnter st

        -- 2. TRATAMENTO GERAL (Setas e Edição)
        _ -> case st^.telaAtiva of
            TelaMenu -> case e of
                 VtyEvent ev -> zoom listaMenu (L.handleListEvent ev)
                 _ -> return ()
            
            TelaMatriculas -> case e of 
                 VtyEvent ev -> zoom listaMenu (L.handleListEvent ev)
                 _ -> return ()

            TelaListaAlunos -> case e of
                VtyEvent ev -> zoom listaMenuAlunos (L.handleListEvent ev)
                _ -> return ()

            TelaListaProfessores -> case e of
                VtyEvent ev -> zoom listaMenuProfessores (L.handleListEvent ev)
                _ -> return ()

            TelaListaTurmas -> case e of
                VtyEvent ev -> zoom listaMenuTurmas (L.handleListEvent ev)
                _ -> return ()

            TelaListaDisciplinas -> case e of
                VtyEvent ev -> zoom listaMenuDisciplinas (L.handleListEvent ev)
                _ -> return ()

            TelaListaSolicitacoes -> case e of
                VtyEvent ev -> zoom listaMenuSolicitacoes (L.handleListEvent ev)
                _ -> return ()

            TelaRelatorio -> case e of
                VtyEvent (V.EvKey V.KUp []) -> vScrollBy (viewportScroll ScrollRelatorio) (-1)
                VtyEvent (V.EvKey V.KDown []) -> vScrollBy (viewportScroll ScrollRelatorio) 1
                VtyEvent (V.EvKey V.KPageUp []) -> vScrollBy (viewportScroll ScrollRelatorio) (-5)
                VtyEvent (V.EvKey V.KPageDown []) -> vScrollBy (viewportScroll ScrollRelatorio) 5
                _ -> return ()
            
            TelaConfirmacao -> case e of
                VtyEvent (V.EvKey (V.KChar 's') []) -> do
                    liftIO $ salvarSistema (st^.sistema)
                    halt
                VtyEvent (V.EvKey (V.KChar 'S') []) -> do
                    liftIO $ salvarSistema (st^.sistema)
                    halt
                VtyEvent (V.EvKey (V.KChar 'n') []) -> halt
                VtyEvent (V.EvKey (V.KChar 'N') []) -> halt
                _ -> return ()
                
            _ -> case focusGetCurrent (st^.foco) of
                Just n -> handleEditorGeneric st n e
                Nothing -> return ()

--- --- SUB-ROTINAS DE EVENTOS --- ---

handleEsc :: AppState -> EventM Name AppState ()
handleEsc st = case st^.telaAtiva of
    TelaMenu -> halt
    TelaInicial -> halt
    
    TelaMatriculas -> 
        if focusGetCurrent (st^.foco) /= Nothing
        then modify $ \s -> s { _foco = focusRing [], _mensagemErro = Nothing }
        else modify $ \s -> s { _telaAtiva = TelaMenu, _mensagemErro = Nothing }

    TelaListaTurmas -> 
        modify $ \s -> s { _telaAtiva = TelaMenu, _foco = focusRing [], _mensagemErro = Nothing }
    
    TelaListaDisciplinas -> 
        modify $ \s -> s { _telaAtiva = TelaMenu, _foco = focusRing [], _mensagemErro = Nothing }

    TelaRelatorio -> 
        modify $ \s -> s { _telaAtiva = TelaMenu, _foco = focusRing [], _mensagemErro = Nothing }

    TelaCadSolicitacao ->
        modify $ \s -> s { _telaAtiva = TelaMatriculas, _foco = focusRing [], _mensagemErro = Nothing }
    
    TelaListaSolicitacoes -> 
        modify $ \s -> s { _telaAtiva = TelaMatriculas }

    TelaConfirmacao ->
         modify $ \s -> s { _telaAtiva = TelaMenu } -- Cancel exit

    _ -> modify $ \s -> s 
        { _telaAtiva = if getFase (st^.sistema) == 1 then TelaMatriculas else TelaMenu
        , _mensagemErro = Nothing 
        , _foco = focusRing [] 
        }

handleEnter :: AppState -> EventM Name AppState ()
handleEnter st = case st^.telaAtiva of
    TelaInicial          -> modify $ \s -> s { _telaAtiva = TelaMenu, _mensagemErro = Nothing }
    TelaMenu             -> handleMenuSelection st
    TelaCadAluno         -> finalizarCadastro extrairAluno [EditNomeAluno, EditMatricula, EditCurso, EditCRA]
    TelaCadProfessor     -> finalizarCadastro extrairProfessor [EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao]
    TelaCadDisciplina    -> finalizarCadastro extrairDisciplina [EditNomeDisciplina, EditCodigoDisciplina, EditRequisitos, EditCursosPermitidos]
    TelaCadTurma         -> finalizarCadastro extrairTurma 
                            [EditCodTurma, EditProfTurma, EditDiscTurma, EditHorario, EditSala, EditMaxAlunosTurma]
    TelaCadSolicitacao   -> finalizarCadastro extrairSolicitacao [EditMatAluno, EditMatTurma]
    TelaMatriculas       -> 
        case focusGetCurrent (st^.foco) of
            Nothing -> handleMenuSelection st
            Just _  -> handleEnter st 
    TelaRelatorio ->
        let isFinalized = st^.mensagemErro == Just "Período de matrículas finalizado."
        in if isFinalized
           then modify $ \s -> s { _telaAtiva = TelaConfirmacao }
           else modify $ \s -> s { _telaAtiva = TelaMenu }
           
    _ -> return ()

handleMenuSelection :: AppState -> EventM Name AppState ()
handleMenuSelection st = case L.listSelectedElement (st^.listaMenu) of
    Just (_, "Cadastrar Aluno") -> 
        modify $ \s -> s { _telaAtiva = TelaCadAluno, _foco = focusRing [EditNomeAluno, EditMatricula, EditCurso, EditCRA] }
    
    Just (_, "Cadastrar Professor") -> 
        modify $ \s -> s { _telaAtiva = TelaCadProfessor, _foco = focusRing [EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao] }

    Just (_, "Cadastrar Turma") -> 
        modify $ \s -> s { _telaAtiva = TelaCadTurma, _foco = focusRing [EditCodTurma, EditProfTurma, EditDiscTurma, EditHorario, EditSala, EditMaxAlunosTurma] }

    Just (_, "Cadastrar Disciplina") -> 
        modify $ \s -> s { _telaAtiva = TelaCadDisciplina, _foco = focusRing [EditCodigoDisciplina, EditNomeDisciplina, EditRequisitos, EditCursosPermitidos] }
    
    Just (_, "Listar Alunos") -> do
        let todosAlunos = M.elems (_alunos (st^.sistema))
        modify $ \s -> s { _telaAtiva = TelaListaAlunos, _listaMenuAlunos = L.list ListaAlunos (Vec.fromList todosAlunos) 1 }

    Just (_, "Listar Professores") -> do
        let todosProfs = M.elems (_professores (st^.sistema))
        modify $ \s -> s { _telaAtiva = TelaListaProfessores, _listaMenuProfessores = L.list ListaProfessores (Vec.fromList todosProfs) 1 }

    Just (_, "Listar Disciplinas") -> do
        let todasDiscs = M.elems (_disciplinas (st^.sistema))
        modify $ \s -> s { _telaAtiva = TelaListaDisciplinas, _listaMenuDisciplinas = L.list ListaDisciplinas (Vec.fromList todasDiscs) 1 }
    
    Just (_, "Listar Turmas") -> do
        let fase = getFase (st^.sistema)
        let turmas = if fase == 1 
                     then M.elems (_turmas (st^.sistema)) 
                     else M.elems (_cadastroDeTurmas (st^.sistema)) ++ M.elems (_turmas (st^.sistema))
        
        modify $ \s -> s { _telaAtiva = TelaListaTurmas, _listaMenuTurmas = L.list ListaTurmas (Vec.fromList turmas) 1 }

    Just (_, "Visualizar Relatório Geral") -> do
        let rel = gerarRelatorioGeral (st^.sistema)
        modify $ \s -> s { _telaAtiva = TelaRelatorio, _textoRelatorio = rel }

    Just (_, "Iniciar Novo Semestre") -> do
        case iniciarNovoSemestre (st^.sistema) of
            Left erro -> modify $ \s -> s { _mensagemErro = Just erro }
            Right novoSistema -> do
                liftIO $ salvarSistema novoSistema
                let novasOpcoes = Vec.fromList 
                        [ "Cadastrar Aluno", "Cadastrar Professor", "Cadastrar Disciplina", "Cadastrar Turma"
                        , "Listar Alunos", "Listar Professores", "Listar Turmas", "Listar Disciplinas"
                        , "Visualizar Relatório Geral", "Iniciar Matrículas", "Sair"
                        ]
                modify $ \s -> s 
                    { _sistema = novoSistema
                    , _listaMenu = L.list MenuPrincipal novasOpcoes 1
                    , _mensagemErro = Just "Novo semestre iniciado! Fase de cadastros."
                    }

    Just (_, "Iniciar Matrículas") -> iniciarPeriodoMatriculas st
    Just (_, "Iniciar Rematrícula") -> iniciarPeriodoMatriculas st

    Just (_, "Cadastrar Matrícula") ->
        modify $ \s -> s { _telaAtiva = TelaCadSolicitacao, _foco = focusRing [EditMatAluno, EditMatTurma] }

    Just (_, "Mostrar Solicitações") -> do
        let todasSols = _matriculas (st^.sistema) -- [(Matricula, Int)]
        modify $ \s -> s 
            { _telaAtiva = TelaListaSolicitacoes
            , _listaMenuSolicitacoes = L.list ListaSolicitacoes (Vec.fromList todasSols) 1 
            }

    Just (_, "Encerrar Matrículas") -> do
        let (deferidas, indeferidas) = processarMatriculas (st^.sistema)
        
        -- Flatten Deferidas to [(Matricula, TurmaID)]
        let listaDeferidas = [(m, tId) | (tId, ms) <- M.toList deferidas, m <- ms]
        
        -- Relatorio Base
        let relatorio = "--- RESULTADO DAS MATRÍCULAS ---\n\n" ++
                        "MATRÍCULAS REALIZADAS:\n" ++ 
                        unlines [ show m ++ " -> Turma " ++ show t | (m, t) <- listaDeferidas ] ++
                        "\n\nMATRÍCULAS INDEFERIDAS:\n" ++
                        unlines [ show m ++ " -> Turma " ++ show t | (t, ms) <- M.toList indeferidas, m <- ms ] ++
                        "\n\nPressione [Enter] para continuar..."

        if M.null indeferidas
        then do
             -- Vai para Fase 2 (Fim)
             let novoSistema = (st^.sistema) { _matriculas = listaDeferidas, _fase = 2 }
             liftIO $ salvarSistema novoSistema
             let novasOpcoes = Vec.fromList ["Visualizar Relatório Geral", "Iniciar Novo Semestre", "Sair"]
             modify $ \s -> s 
                { _sistema = novoSistema
                , _telaAtiva = TelaRelatorio
                , _textoRelatorio = relatorio
                , _listaMenu = L.list MenuPrincipal novasOpcoes 1
                , _mensagemErro = Just "Período de matrículas finalizado."
                }
        else do
             -- Vai para Rematrícula (Mantém Fase 1)
             -- Atualiza _matriculas com as deferidas para não perder
             let novoSistema = (st^.sistema) { _matriculas = listaDeferidas, _fase = 1 }
             liftIO $ salvarSistema novoSistema
             let novasOpcoes = Vec.fromList ["Cadastrar Rematrícula", "Mostrar Rematrículas", "Finalizar Rematrícula", "Sair"]
             modify $ \s -> s 
                { _sistema = novoSistema
                , _telaAtiva = TelaRelatorio
                , _textoRelatorio = relatorio
                , _listaMenu = L.list MenuPrincipal novasOpcoes 1
                , _mensagemErro = Just "Matrículas com pendências. Iniciando Rematrícula."
                }

    Just (_, "Cadastrar Rematrícula") ->
        modify $ \s -> s { _telaAtiva = TelaCadSolicitacao, _foco = focusRing [EditMatAluno, EditMatTurma] }

    Just (_, "Mostrar Rematrículas") -> do
        let todasSols = _matriculas (st^.sistema) 
        modify $ \s -> s 
            { _telaAtiva = TelaListaSolicitacoes
            , _listaMenuSolicitacoes = L.list ListaSolicitacoes (Vec.fromList todasSols) 1 
            }

    Just (_, "Finalizar Rematrícula") -> do
        let (deferidas, indeferidas) = processarMatriculas (st^.sistema)
        let listaDeferidas = [(m, tId) | (tId, ms) <- M.toList deferidas, m <- ms]
        
        let relatorio = "--- RESULTADO DAS REMATRÍCULAS ---\n\n" ++
                        "MATRÍCULAS REALIZADAS:\n" ++ 
                        unlines [ show m ++ " -> Turma " ++ show t | (m, t) <- listaDeferidas ] ++
                        "\n\nMATRÍCULAS INDEFERIDAS:\n" ++
                        unlines [ show m ++ " -> Turma " ++ show t | (t, ms) <- M.toList indeferidas, m <- ms ] ++
                        "\n\nPressione [Enter] para continuar..."
        
        let novoSistema = (st^.sistema) { _matriculas = listaDeferidas, _fase = 2 }
        liftIO $ salvarSistema novoSistema
        let novasOpcoes = Vec.fromList ["Visualizar Relatório Geral", "Iniciar Novo Semestre", "Sair"]
        modify $ \s -> s 
            { _sistema = novoSistema
            , _telaAtiva = TelaRelatorio
            , _textoRelatorio = relatorio
            , _listaMenu = L.list MenuPrincipal novasOpcoes 1
            , _mensagemErro = Just "Período de rematrículas finalizado."
            }

    Just (_, "Sair") -> halt
    _ -> return ()

iniciarPeriodoMatriculas :: AppState -> EventM Name AppState ()
iniciarPeriodoMatriculas st = do
    let conflitos = getTurmasConflitantes (st^.sistema)
    if null conflitos
    then do
        -- Efetivar alterações (Merge pending turmas)
        let sisEfetivado = efetivarAlteracoes (st^.sistema)
        case abrirPeriodoMatriculas sisEfetivado of
            Left erro -> modify $ \s -> s { _mensagemErro = Just erro }
            Right novoSistema -> do
                liftIO $ salvarSistema novoSistema
                let novasOpcoes = Vec.fromList ["Cadastrar Matrícula", "Mostrar Solicitações", "Encerrar Matrículas", "Sair"]
                modify $ \s -> s 
                    { _sistema = novoSistema
                    , _telaAtiva = TelaMatriculas 
                    , _listaMenu = L.list MenuPrincipal novasOpcoes 1
                    , _foco = focusRing [] 
                    , _mensagemErro = Nothing 
                    }
    else do
        let msg = "Conflitos de horário encontrados! Verifique as turmas."
        modify $ \s -> s { _mensagemErro = Just msg }

handleEditorGeneric :: AppState -> Name -> BrickEvent Name e -> EventM Name AppState ()
handleEditorGeneric st n e = 
    let campoLens = lens (\s -> M.findWithDefault (editor n (Just 1) "") n (s^.formularios))
                         (\s ed -> s { _formularios = M.insert n ed (s^.formularios) })
    in zoom campoLens (handleEditorEvent e)

finalizarCadastro :: (AppState -> Either String Sistema) -> [Name] -> EventM Name AppState ()
finalizarCadastro extrair camposParaReset = do
    st <- get
    case extrair st of
        Left erro -> modify $ \s -> s { _mensagemErro = Just erro }
        Right novoSistema -> do
            liftIO $ salvarSistema novoSistema
            let novosForms = foldr (\n m -> M.insert n (editor n (Just 1) "") m) (st^.formularios) camposParaReset
            modify $ \s -> s 
                { _sistema = novoSistema
                , _telaAtiva = if getFase novoSistema == 1 then TelaMatriculas else TelaMenu
                , _foco = focusRing [] 
                , _formularios = novosForms 
                , _mensagemErro = Just "Operação realizada com sucesso!" 
                }

--- --- AUXILIARES DE EXTRAÇÃO --- ---

getCampo :: Name -> AppState -> String
getCampo n st = case M.lookup n (st^.formularios) of
    Just ed -> concat $ getEditContents ed
    Nothing -> ""

extrairAluno :: AppState -> Either String Sistema
extrairAluno st = do
    let n = getCampo EditNomeAluno st
    mVal <- maybe (Left "Matrícula inválida") Right (readMaybe $ getCampo EditMatricula st)
    let c = getCampo EditCurso st
    craVal <- maybe (Left "CRA inválido") Right (readMaybe $ getCampo EditCRA st)
    cra <- maybe (Left "CRA deve ser entre 0 e 10") Right (mkCRA craVal)
    
    cadastrarAluno (criarAluno (Matricula mVal) (Nome n) (Curso c) cra) (st^.sistema)

extrairProfessor :: AppState -> Either String Sistema
extrairProfessor st = do
    let txtMatricula = getCampo EditMatriculaProfessor st
    let nome = getCampo EditNomeProfessor st
    let depto = getCampo EditDepto st
    let formacao = getCampo EditFormacao st
    
    mVal <- maybe (Left "Matrícula do professor inválida") Right (readMaybe txtMatricula)
    
    if null nome then Left "Nome obrigatório" 
    else cadastrarProfessor (criarProfessor (Matricula mVal) (Nome nome) depto formacao) (st^.sistema)

extrairDisciplina :: AppState -> Either String Sistema
extrairDisciplina st = do
    let n = getCampo EditNomeDisciplina st
    let c = getCampo EditCodigoDisciplina st
    let reqsStr = getCampo EditRequisitos st
    let cursosStr = getCampo EditCursosPermitidos st
    
    let reqs = map Codigo (words reqsStr)
    let cursos = map Curso (words cursosStr)
    
    if null n || null c then Left "Campos obrigatórios" 
    else cadastrarDisciplina (criarDisciplina (Codigo c) (Nome n) reqs cursos) (st^.sistema)

extrairTurma :: AppState -> Either String Sistema
extrairTurma st = do
    cod  <- maybe (Left "Cód. inválido") Right (readMaybe $ getCampo EditCodTurma st)
    profVal <- maybe (Left "Prof. inválido") Right (readMaybe $ getCampo EditProfTurma st)
    maxA <- maybe (Left "Qtd. inválida") Right (readMaybe $ getCampo EditMaxAlunosTurma st)
    let discStr = getCampo EditDiscTurma st
    let horarioStr = getCampo EditHorario st
    let sala = getCampo EditSala st
    
    if null discStr || null horarioStr 
       then Left "Campos obrigatórios faltando" 
       else cadastrarTurma (criarTurma cod (Matricula profVal) (Codigo discStr) (lerHorario horarioStr) sala maxA) (st^.sistema)

extrairSolicitacao :: AppState -> Either String Sistema
extrairSolicitacao st = do
    idAVal <- maybe (Left "ID Aluno inválido") Right (readMaybe $ getCampo EditMatAluno st)
    idT <- maybe (Left "ID Turma inválido") Right (readMaybe $ getCampo EditMatTurma st)
    
    realizarMatricula (Matricula idAVal) idT (st^.sistema)