{-|
Module      : Menu.UI.Common
Description : Componentes de interface compartilhados e templates.

Este módulo centraliza os elementos visuais reutilizáveis da aplicação,
como o template principal de janelas, a estilização de formulários e
a renderização de mensagens de feedback ao usuário.
-}

module Menu.UI.Common
  ( templateUI
  , drawLogo
  , drawForm
  , drawFeedback
  , drawGenericMenu
  ) where

-- Bibliotecas Externas
import Brick
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Edit (renderEditor)
import Brick.Focus (focusGetCurrent)
import Lens.Micro ((^.))
import Data.List (isInfixOf)
import qualified Brick.Widgets.List as L
import qualified Data.Map as M

-- Módulos Internos
import Menu.Tipos

-------------------------------------------------------------------------------
-- Templates e Estrutura
-------------------------------------------------------------------------------

-- | Envolve um widget com a logo superior, bordas padronizadas e um título.
-- Define um limite horizontal de 70 colunas para manter a legibilidade.
templateUI :: String -> Widget Name -> Widget Name
templateUI titulo conteudo = 
    vBox [ padBottom (Pad 1) drawLogo
         , hCenter $ hLimit 70 $ 
           withAttr (attrName "border") $
           borderWithLabel (str titulo) $
             padLeftRight 2 $ vBox [ str " ", conteudo, str " " ]
         ]

-- | Renderiza a logo em arte ASCII estilizada para o cabeçalho.
drawLogo :: Widget Name
drawLogo = padTop (Pad 2) $ withAttr (attrName "logo") $ vBox
    [ hCenter $ str " __  __ _      _   ___ _             "
    , hCenter $ str " |  \\/  (_)_ _ (_) / __(_)__ _ __ _ __ _ "
    , hCenter $ str " | |\\/| | | ' \\| | \\__ \\ / _` / _` / _` |"
    , hCenter $ str " |_|  |_|_|_||_|_| |___/_\\__, \\__,_\\__,_|"
    , hCenter $ str "                         |___/           "
    ]

-------------------------------------------------------------------------------
-- Formulários e Campos
-------------------------------------------------------------------------------

-- | Renderiza uma lista de campos rotulados e exibe instruções de navegação.
drawForm :: [(String, Name)] -> AppState -> Widget Name
drawForm campos s = 
    vBox [ vBox $ map (drawField s) campos
         , str " "
         , hCenter $ str "[Tab] Prox. Campo | [Enter] Salvar | [Esc] Voltar" ]

-- | Desenha um campo individual (rótulo : editor).
-- Destaca o editor visualmente caso ele possua o foco atual.
drawField :: AppState -> (String, Name) -> Widget Name
drawField s (label, name) = 
    let ed = (s^.formularios) M.! name
        focado = focusGetCurrent (s^.foco) == Just name
    in hBox [ hLimit 20 $ padLeft Max (str label)
            , str ": "
            , renderEditor (str . Prelude.unlines) focado ed 
            ]

-------------------------------------------------------------------------------
-- Feedback e Menus
-------------------------------------------------------------------------------

-- | Exibe mensagens de erro ou sucesso centralizadas no rodapé.
-- A cor é definida dinamicamente baseada no conteúdo da mensagem.
drawFeedback :: AppState -> Widget Name
drawFeedback st = maybe emptyWidget renderizar (st^.mensagemErro)
  where
    renderizar msg = 
        let estilo = if "sucesso" `isInfixOf` msg || "realizada" `isInfixOf` msg
                     then attrName "sucesso"
                     else attrName "erro"
        in hCenter $ padTop (Pad 1) $ withAttr estilo $ str msg

-- | Renderiza um menu de seleção baseado em lista com cabeçalho e rodapé.
drawGenericMenu :: AppState -> String -> String -> Widget Name
drawGenericMenu s titulo instrucao = templateUI titulo $
    vBox [ L.renderList (\selected el -> 
            if selected 
            then withAttr L.listSelectedAttr (str $ "> " ++ el) 
            else padLeft (Pad 2) (str el)) True (s^.listaMenu)
         , str " "
         , hCenter $ str instrucao 
         ]
