module BrickMenu.Events (handleEvent) where

import Brick
import Brick.Widgets.Edit (handleEditorEvent, getEditContents, editor, handleEditorEvent)
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

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    case st^.telaAtiva of
        TelaMenu -> halt
        _        -> modify $ \s -> s { _telaAtiva = TelaMenu }

handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = 
    modify $ \s -> s { _foco = focusNext (s^.foco) }

-- TRATAMENTO DO ENTER
handleEvent (VtyEvent ev@(V.EvKey V.KEnter [])) = do
    st <- get
    case st^.telaAtiva of
        TelaMenu          -> handleMenuSelection st
        TelaCadAluno      -> finalizarCadastro extrairAluno [EditNomeAluno, EditMatricula, EditCurso, EditCRA]
        TelaCadProfessor  -> finalizarCadastro extrairProfessor [EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao]
        TelaCadDisciplina -> finalizarCadastro extrairDisciplina [EditNomeDisciplina, EditCodigoDisciplina]
        TelaCadTurma      -> finalizarCadastro extrairTurma 
                             [EditCodTurma, EditProfTurma, EditDiscTurma, EditHorarioTurma, EditMaxAlunosTurma]
        
        TelaListaAlunos   -> return () 

-- TRATAMENTO GERAL DE EVENTOS (Setas, Teclado, etc)
handleEvent e = do
    st <- get
    case st^.telaAtiva of
        TelaMenu -> case e of
            VtyEvent ev -> zoom listaMenu (L.handleListEventVi L.handleListEvent ev)
            _ -> return ()
            
        
        TelaListaAlunos -> case e of
            VtyEvent ev -> zoom listaMenuAlunos (L.handleListEventVi L.handleListEvent ev)
            _ -> return ()
            
        
        _ -> case focusGetCurrent (st^.foco) of
            Just n -> case M.lookup n (st^.formularios) of
                Just _ -> 
                    let campoLens = lens (\s -> M.findWithDefault (editor n (Just 1) "") n (s^.formularios))
                                         (\s ed -> s { _formularios = M.insert n ed (s^.formularios) })
                    in zoom campoLens (handleEditorEvent e)
                Nothing -> return ()
            _ -> return ()

--- --- AUXILIARES DE EXTRAÇÃO  --- ---

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
    -- 1. Pegamos os textos dos editores
    let txtMatricula = getCampo EditMatriculaProfessor st
    let nome = getCampo EditNomeProfessor st
    let depto = getCampo EditDepto st
    let formacao = getCampo EditFormacao st

    -- 2. Tentamos converter a matrícula para número
    -- Se o usuário digitar "abc", o readMaybe retorna Nothing e cai no Left
    m <- maybe (Left "A matrícula do professor deve conter apenas números") 
               Right 
               (readMaybe txtMatricula)

    -- 3. Validamos se o nome não está vazio
    if null nome 
       then Left "O nome do professor é obrigatório"
       else cadastrarProfessor (criarProfessor m nome depto formacao) (st^.sistema)

extrairDisciplina :: AppState -> Either String Sistema
extrairDisciplina st = do
    let n = getCampo EditNomeDisciplina st
    let c = getCampo EditCodigoDisciplina st
    if null n || null c 
       then Left "Nome e Código são obrigatórios"
       else cadastrarDisciplina (criarDisciplina c n []) (st^.sistema)

extrairTurma :: AppState -> Either String Sistema
extrairTurma st = do
    cod  <- maybe (Left "Cod. Turma inválido") Right (readMaybe $ getCampo EditCodTurma st)
    prof <- maybe (Left "ID Professor inválido") Right (readMaybe $ getCampo EditProfTurma st)
    let disc = getCampo EditDiscTurma st
    let hor  = getCampo EditHorarioTurma st
    maxA <- maybe (Left "Qtd Max inválida") Right (readMaybe $ getCampo EditMaxAlunosTurma st)
    
    cadastrarTurma (criarTurma cod prof disc hor maxA) (st^.sistema)

--- --- GESTÃO DE ESTADO  --- ---

handleMenuSelection :: AppState -> EventM Name AppState ()
handleMenuSelection st = case L.listSelectedElement (st^.listaMenu) of
    Just (_, "Cadastrar Aluno") -> 
        modify $ \s -> s { _telaAtiva = TelaCadAluno, _foco = focusRing [EditNomeAluno, EditMatricula, EditCurso, EditCRA] }
    Just (_, "Cadastrar Professor") -> 
        modify $ \s -> s { _telaAtiva = TelaCadProfessor, _foco = focusRing [EditMatriculaProfessor, EditNomeProfessor, EditDepto, EditFormacao] }
    Just (_, "Cadastrar Disciplina") -> 
        modify $ \s -> s { _telaAtiva = TelaCadDisciplina, _foco = focusRing [EditCodigoDisciplina, EditNomeDisciplina] }
    Just (_, "Cadastrar Turma") -> 
        modify $ \s -> s { _telaAtiva = TelaCadTurma
                         , _foco = focusRing [EditCodTurma, EditProfTurma, EditDiscTurma, EditHorarioTurma, EditMaxAlunosTurma] 
                         }
    Just (_, "Listar Alunos") -> do
        let todosAlunos = M.elems (_alunos (st^.sistema))
        modify $ \s -> s { _telaAtiva = TelaListaAlunos
                         , _listaMenuAlunos = L.list ListaAlunos (Vec.fromList todosAlunos) 1 
                         }
    _ -> return ()

finalizarCadastro :: (AppState -> Either String Sistema) -> [Name] -> EventM Name AppState ()
finalizarCadastro extrair camposParaReset = do
    st <- get
    case extrair st of
        Left erro -> return () 
        Right novoSistema -> do
            liftIO $ salvarSistema novoSistema
            let novosForms = foldr (\n m -> M.insert n (editor n (Just 1) "") m) (st^.formularios) camposParaReset
            modify $ \s -> s { _sistema = novoSistema
                             , _telaAtiva = TelaMenu
                             , _formularios = novosForms 
                             }