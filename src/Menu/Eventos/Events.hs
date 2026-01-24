{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Events
Description : Gerenciamento de eventos e roteamento de entrada do usuário.

Este módulo centraliza o tratamento de eventos da biblioteca Brick, 
gerenciando a alternância de foco, entradas de teclado e o roteamento 
baseado no estado atual da aplicação (AppState).
-}

module Menu.Eventos.Events (handleEvent) where

-- Imports de bibliotecas externas
import Brick
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import Brick.Focus (focusGetCurrent, focusNext)
import Lens.Micro ((^.))

-- Imports de tipos e sistema local
import Menu.Tipos
import Sistema

-- Imports de roteamento (Fases e Util)
import Menu.Eventos.FaseCadastro (handleEnterFaseCadastro, handleEscFaseCadastro)
import Menu.Eventos.FaseMatricula (handleEnterFaseMatricula, handleEscFaseMatricula)
import Menu.Eventos.FaseNotas (handleEnterFaseNotas, handleEscFaseNotas)
import Menu.Eventos.Util (handleEditorGeneric)

-- | Função principal de tratamento de eventos.
-- Recebe um evento do Brick e modifica o estado da aplicação ('AppState') 
-- dentro do monad 'EventM'.
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent e = do
    st <- get
    let faseAtual = getFase (st^.sistema)

    case e of
        -- 1. TRATAMENTO DE TECLAS ESPECÍFICAS

        -- | Esc: Retorna ou cancela a ação dependendo da fase atual.
        VtyEvent (V.EvKey V.KEsc []) -> handleEsc st faseAtual
        
        -- | Tab: Alterna o foco entre os elementos da interface (campos de texto/botões).
        VtyEvent (V.EvKey (V.KChar '\t') []) -> 
            modify $ \s -> s { _foco = focusNext (s^.foco) }

        -- | Enter: Confirma seleções ou submete formulários.
        VtyEvent (V.EvKey V.KEnter []) -> handleEnter st faseAtual

        -- 2. TRATAMENTO POR CONTEXTO (TELA ATIVA)
        _ -> case st^.telaAtiva of

            -- Contexto: Menus e Cadastros que utilizam Listas ou Editores
            t | t `elem` [TelaMenu, TelaMatriculas, TelaMenuNotas, TelaConsultarNotas] -> 
                case focusGetCurrent (st^.foco) of
                    -- Se nada tem foco, os eventos de seta controlam a lista principal
                    Nothing -> case e of
                        VtyEvent ev -> zoom listaMenu (L.handleListEvent ev)
                        _ -> return ()
                    -- Se um campo tem foco, o evento é enviado para o editor genérico
                    Just n -> handleEditorGeneric st n e

            -- Contexto: Visualização de Listagens Específicas
            TelaListaAlunos -> case e of
                VtyEvent ev -> zoom listaMenuAlunos (L.handleListEvent ev)
                _ -> return ()

            TelaListaProfessores -> case e of
                VtyEvent ev -> zoom listaMenuProfessores (L.handleListEvent ev)
                _ -> return ()

            TelaListaDisciplinas -> case e of
                VtyEvent ev -> zoom listaMenuDisciplinas (L.handleListEvent ev)
                _ -> return ()

            TelaListaSolicitacoes -> case e of
                VtyEvent ev -> zoom listaMenuSolicitacoes (L.handleListEvent ev)
                _ -> return ()

            TelaListaResultados -> case e of
                VtyEvent ev -> zoom listaMenuResultados (L.handleListEvent ev)
                _ -> return ()

            TelaExibirNotasAluno -> case e of
                VtyEvent ev -> zoom listaMenuNotas (L.handleListEvent ev)
                _ -> return ()

            TelaAgenda -> case e of
                VtyEvent (V.EvKey V.KDown []) ->
                    vScrollBy (viewportScroll AgendaViewport) 1

                VtyEvent (V.EvKey V.KUp []) ->
                    vScrollBy (viewportScroll AgendaViewport) (-1)

                VtyEvent (V.EvKey V.KPageDown []) ->
                    vScrollPage (viewportScroll AgendaViewport) Down

                VtyEvent (V.EvKey V.KPageUp []) ->
                    vScrollPage (viewportScroll AgendaViewport) Up 

                _ -> return () 
                
            -- Contexto Padrão: Tenta processar editor se houver foco, caso contrário ignora    
            _ -> case focusGetCurrent (st^.foco) of
                Just n -> handleEditorGeneric st n e
                Nothing -> return ()

-------------------------------------------------------------------------------
-- Funções Auxiliares de Roteamento
-------------------------------------------------------------------------------


-- | Roteia a tecla 'Esc' para o tratador específico da fase do sistema.
-- Fases: 0 = Cadastro, 1 = Matrícula, 2 = Notas.
handleEsc :: AppState -> Int -> EventM Name AppState ()
handleEsc st 0 = handleEscFaseCadastro st
handleEsc st 1 = handleEscFaseMatricula st
handleEsc st 2 = handleEscFaseNotas st
handleEsc _ _  = return ()

-- | Roteia a tecla 'Enter' para o tratador específico da fase do sistema.
-- Fases: 0 = Cadastro, 1 = Matrícula, 2 = Notas.
handleEnter :: AppState -> Int -> EventM Name AppState ()
handleEnter st 0 = handleEnterFaseCadastro st
handleEnter st 1 = handleEnterFaseMatricula st
handleEnter st 2 = handleEnterFaseNotas st
handleEnter _ _  = return ()