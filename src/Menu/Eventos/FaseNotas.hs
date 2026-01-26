{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Menu.Eventos.FaseNotas
Description : Lógica de controle para lançamento de notas e encerramento.

Este módulo gerencia a 'Fase 2' do sistema. Ele permite o lançamento de 
notas por matrícula, consulta de aproveitamento e a finalização do 
semestre letivo, que reinicia o ciclo para a Fase de Cadastro.
-}

module Menu.Eventos.FaseNotas 
    ( handleEnterFaseNotas
    , handleEscFaseNotas
    ) where

-- Bibliotecas Externas
import Brick
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Data.Map as M
import Data.Char (isSpace)
import Text.Read (readMaybe)
import Brick.Focus (focusRing, focusGetCurrent)
import Lens.Micro ((^.))
import Control.Monad.IO.Class (liftIO)

-- Módulos Internos
import Menu.Tipos
import Menu.Eventos.Util (finalizarCadastro, extrairLancamentoNota, getCampo)
import qualified Models.Aluno as A
import Sistema
import Utils.Database (salvarSistema)

-------------------------------------------------------------------------------
-- Tratamento de Teclas
-------------------------------------------------------------------------------

-- | Gerencia o acionamento da tecla 'ESC' na Fase de Notas.
handleEscFaseNotas :: AppState -> EventM Name AppState ()
handleEscFaseNotas st = case st^.telaAtiva of
    -- Se estiver no menu principal da fase, encerra o programa
    TelaMenuNotas -> halt 

    -- Se eu der ESC vendo o boletim, volto para a tela de digitar matrícula
    TelaExibirNotasAluno -> modify $ \s -> s { _telaAtiva = TelaConsultarNotas }

    -- Para qualquer outra tela de notas, retorna ao menu principal da fase
    _ -> modify $ \s -> s 
        { _telaAtiva = TelaMenuNotas
        , _foco = focusRing []
        , _mensagemErro = Nothing 
        }

-- | Gerencia o acionamento da tecla 'Enter' na Fase de Notas.
handleEnterFaseNotas :: AppState -> EventM Name AppState ()
handleEnterFaseNotas st = case st^.telaAtiva of
    -- No menu, protege contra acionamentos acidentais se houver foco em widgets
    TelaMenuNotas -> case focusGetCurrent (st^.foco) of
        Nothing -> handleMenuSelectionFaseNotas st
        Just _  -> return ()

    -- Finalização do formulário de inserção de notas
    TelaInserirNotas -> do
        finalizarCadastro extrairLancamentoNota [EditMatriculaNota, EditTurmaNota, EditNota1, EditNota2, EditNota3]

    -- Processamento da consulta de notas
    TelaConsultarNotas -> do
        let matriculaStr = filter (not . isSpace) (getCampo EditConsultaNotaMatricula st)
        let alunosMap = _alunos (st^.sistema)
        

        case readMaybe matriculaStr of
            Just m -> case M.lookup m alunosMap of
                Just aluno -> do
                    let notasL = M.toList (A.getNotasAluno aluno)
                    modify $ \s -> s 
                        { _telaAtiva = TelaExibirNotasAluno
                        , _listaMenuNotas = L.list EditListaNotas (Vec.fromList notasL) 1
                        , _mensagemErro = Nothing
                        }
                Nothing -> modify $ \s -> s { _mensagemErro = Just "Aluno não encontrado!" }
            Nothing -> modify $ \s -> s { _mensagemErro = Just "Digite uma matrícula válida!" }
    _ -> return ()

-------------------------------------------------------------------------------
-- Lógica de Seleção de Menu
-------------------------------------------------------------------------------

-- | Roteia a seleção do menu de Notas para a ação correspondente.
handleMenuSelectionFaseNotas :: AppState -> EventM Name AppState ()
handleMenuSelectionFaseNotas st = case L.listSelectedElement (st^.listaMenu) of
    -- Navegação para Formulários
    Just (_, "Inserir Nota") ->
        modify $ \s -> s { _telaAtiva = TelaInserirNotas, _foco = focusRing [EditMatriculaNota, EditTurmaNota, EditNota1, EditNota2, EditNota3] }
    
    Just (_, "Consultar Notas") -> do
        modify $ \s -> s 
            { _telaAtiva = TelaConsultarNotas, _foco = focusRing [EditConsultaNotaMatricula] }

    Just (_, "Ver Resultados Matrícula") -> do
        let resultados = _historicoProc (st^.sistema)
        modify $ \s -> s 
            { _telaAtiva = TelaListaResultados
            , _listaMenuResultados = L.list ListaResultados (Vec.fromList resultados) 1 
            }
            
    -- Finalização do Ciclo: Notas -> Cadastro (Fase 0)
    Just (_, "Finalizar Semestre") -> do
        -- 1. Reinicia o sistema (fase 0) mantendo dados persistentes
        let novoSis = finalizarSemestre (st^.sistema)
        
        -- 2. Prepara o menu para a Fase 0
        let opcoesFase0 = Vec.fromList 
                [ "Cadastrar Aluno"
                , "Cadastrar Professor"
                , "Cadastrar Disciplina"
                , "Cadastrar Turma"
                , "Listar Alunos"
                , "Listar Professores"
                , "Listar Turmas"
                , "Listar Disciplinas"
                , "Período de Matrículas"
                , "Sair"
                ]
        
        -- 3. Atualiza o Widget de Lista existente com as novas opções
        let listaAtualizada = L.listReplace opcoesFase0 (Just 0) (st^.listaMenu)

        -- 4. Persiste a mudança de fase no armazenamento
        liftIO $ salvarSistema novoSis

        -- 5. Aplica a transição de estado completa
        modify $ \s -> s 
            { _sistema = novoSis
            , _telaAtiva = TelaMenu
            , _listaMenu = listaAtualizada 
            , _mensagemErro = Just "Semestre finalizado! Menu da Fase 0 carregado."
            , _foco = focusRing []
            }

    Just (_, "Sair") -> halt
    _ -> return ()
