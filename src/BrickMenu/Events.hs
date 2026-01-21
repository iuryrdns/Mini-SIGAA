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

import BrickMenu.Tipos
import Sistema
import Models.Aluno
import Models.Professor
import Models.Disciplina
import Models.Turma
import Models.Matricula (getIdAlunoMatricula)

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
            t | t == TelaMenu || t == TelaMatriculas -> 
                case focusGetCurrent (st^.foco) of
                    Nothing -> case e of
                        VtyEvent ev -> zoom listaMenu (L.handleListEvent ev)
                        _ -> return ()
                    Just n -> handleEditorGeneric st n e

            TelaListaAlunos -> case e of
                VtyEvent ev -> zoom listaMenuAlunos (L.handleListEvent ev)
                _ -> return ()

            TelaListaProfessores -> case e of
                VtyEvent ev -> zoom listaMenuProfessores (L.handleListEvent ev)
                _ -> return ()
                
            _ -> case focusGetCurrent (st^.foco) of
                Just n -> handleEditorGeneric st n e
                Nothing -> return ()

--- --- SUB-ROTINAS DE EVENTOS --- ---

handleEsc :: AppState -> EventM Name AppState ()
handleEsc st = case st^.telaAtiva of
    TelaMenu -> halt
    
    TelaMatriculas -> 
        if focusGetCurrent (st^.foco) /= Nothing
        then modify $ \s -> s { _foco = focusRing [], _mensagemErro = Nothing }
        else modify $ \s -> s { _telaAtiva = TelaMenu, _mensagemErro = Nothing }

    TelaAgenda -> 
        modify $ \s -> s { _telaAtiva = TelaMenu, _foco = focusRing [], _mensagemErro = Nothing }

    TelaCadSolicitacao ->
        modify $ \s -> s { _telaAtiva = TelaMatriculas, _foco = focusRing [], _mensagemErro = Nothing }
    
    TelaListaSolicitacoes -> 
        modify $ \s -> s { _telaAtiva = TelaMatriculas }

    _ -> modify $ \s -> s 
        { _telaAtiva = if getFase (st^.sistema) == 1 then TelaMatriculas else TelaMenu
        , _mensagemErro = Nothing 
        , _foco = focusRing [] 
        }

handleEnter :: AppState -> EventM Name AppState ()
handleEnter st = case st^.telaAtiva of
    TelaMenu             -> handleMenuSelection st
    TelaCadAluno         -> finalizarCadastro extrairAluno [EditNomeAluno, EditMatricula, EditCurso, EditCRA]
    TelaCadProfessor     -> finalizarCadastro extrairProfessor [EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao]
    TelaCadDisciplina    -> finalizarCadastro extrairDisciplina [EditNomeDisciplina, EditCodigoDisciplina]
    TelaCadTurma         -> finalizarCadastro extrairTurma 
                            [EditCodTurma, EditProfTurma, EditDiscTurma, EditDiaTurma, EditHoraTurma, EditMaxAlunosTurma]
    TelaCadSolicitacao   -> finalizarCadastro extrairSolicitacao [EditMatAluno, EditMatTurma]
    TelaMatriculas       -> 
        case focusGetCurrent (st^.foco) of
            Nothing -> handleMenuSelection st
            Just _  -> handleEnter st 
    _ -> return ()

handleMenuSelection :: AppState -> EventM Name AppState ()
handleMenuSelection st = case L.listSelectedElement (st^.listaMenu) of
    Just (_, "Cadastrar Aluno") -> 
        modify $ \s -> s { _telaAtiva = TelaCadAluno, _foco = focusRing [EditNomeAluno, EditMatricula, EditCurso, EditCRA] }
    
    Just (_, "Cadastrar Professor") -> 
        modify $ \s -> s { _telaAtiva = TelaCadProfessor, _foco = focusRing [EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao] }

    Just (_, "Cadastrar Turma") -> 
        modify $ \s -> s { _telaAtiva = TelaCadTurma, _foco = focusRing [EditCodTurma, EditProfTurma, EditDiscTurma, EditDiaTurma, EditHoraTurma, EditMaxAlunosTurma] }

    Just (_, "Cadastrar Disciplina") -> 
        modify $ \s -> s { _telaAtiva = TelaCadDisciplina, _foco = focusRing [EditCodigoDisciplina, EditNomeDisciplina] }
    
    Just (_, "Listar Alunos") -> do
        let todosAlunos = M.elems (_alunos (st^.sistema))
        modify $ \s -> s { _telaAtiva = TelaListaAlunos, _listaMenuAlunos = L.list ListaAlunos (Vec.fromList todosAlunos) 1 }

    Just (_, "Listar Professores") -> do
        let todosProfs = M.elems (_professores (st^.sistema))
        modify $ \s -> s { _telaAtiva = TelaListaProfessores, _listaMenuProfessores = L.list ListaProfessores (Vec.fromList todosProfs) 1 }
    
    Just (_, "Listar Turmas") -> 
        modify $ \s -> s { _telaAtiva = TelaAgenda }

    Just (_, "Período de Matrículas") -> do
        case abrirPeriodoMatriculas (st^.sistema) of
            Left erro -> modify $ \s -> s { _mensagemErro = Just erro }
            Right novoSistema -> do
                liftIO $ salvarSistema novoSistema
                let novasOpcoes = Vec.fromList ["Cadastrar Matrícula", "Mostrar Solicitações", "Sair"]
                modify $ \s -> s 
                    { _sistema = novoSistema
                    , _telaAtiva = TelaMatriculas 
                    , _listaMenu = L.list MenuPrincipal novasOpcoes 1
                    , _foco = focusRing [] 
                    , _mensagemErro = Nothing 
                    }

    Just (_, "Cadastrar Matrícula") ->
        modify $ \s -> s { _telaAtiva = TelaCadSolicitacao, _foco = focusRing [EditMatAluno, EditMatTurma] }

    Just (_, "Mostrar Solicitações") -> do
        let todasSols = _solicitacoes (st^.sistema)
        modify $ \s -> s 
            { _telaAtiva = TelaListaSolicitacoes
            , _listaMenuSolicitacoes = L.list ListaSolicitacoes (Vec.fromList todasSols) 1 
            }

    Just (_, "Sair") -> halt
    _ -> return ()

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
    m   <- maybe (Left "Matrícula inválida") Right (readMaybe $ getCampo EditMatricula st)
    let c = getCampo EditCurso st
    cra <- maybe (Left "CRA inválido") Right (readMaybe $ getCampo EditCRA st)
    cadastrarAluno (criarAluno m n c cra) (st^.sistema)

extrairProfessor :: AppState -> Either String Sistema
extrairProfessor st = do
    let txtMatricula = getCampo EditMatriculaProfessor st
    let nome = getCampo EditNomeProfessor st
    let depto = getCampo EditDepto st
    let formacao = getCampo EditFormacao st
    m <- maybe (Left "Matrícula do professor inválida") Right (readMaybe txtMatricula)
    if null nome then Left "Nome obrigatório" else cadastrarProfessor (criarProfessor m nome depto formacao) (st^.sistema)

extrairDisciplina :: AppState -> Either String Sistema
extrairDisciplina st = do
    let n = getCampo EditNomeDisciplina st
    let c = getCampo EditCodigoDisciplina st
    if null n || null c then Left "Campos obrigatórios" else cadastrarDisciplina (criarDisciplina c n []) (st^.sistema)

parseDia :: String -> Either String Dia
parseDia s = case s of
    "2" -> Right Segunda
    "3" -> Right Terca
    "4" -> Right Quarta
    "5" -> Right Quinta
    "6" -> Right Sexta
    _   -> Left "Dia inválido! Use 2:Seg, 3:Ter, 4:Qua, 5:Qui, 6:Sex"

extrairTurma :: AppState -> Either String Sistema
extrairTurma st = do
    cod  <- maybe (Left "Cód. inválido") Right (readMaybe $ getCampo EditCodTurma st)
    prof <- maybe (Left "Prof. inválido") Right (readMaybe $ getCampo EditProfTurma st)
    maxA <- maybe (Left "Qtd. inválida") Right (readMaybe $ getCampo EditMaxAlunosTurma st)
    let disc = getCampo EditDiscTurma st
    let hora = getCampo EditHoraTurma st
    dia <- parseDia (getCampo EditDiaTurma st)
    if null disc || null hora 
       then Left "Campos obrigatórios faltando" 
       else cadastrarTurma (criarTurma cod prof disc dia hora maxA) (st^.sistema)

extrairSolicitacao :: AppState -> Either String Sistema
extrairSolicitacao st = do
    idA <- maybe (Left "ID Aluno inválido") Right (readMaybe $ getCampo EditMatAluno st)
    idT <- maybe (Left "ID Turma inválido") Right (readMaybe $ getCampo EditMatTurma st)
    cadastrarSolicitacao idA idT (st^.sistema)