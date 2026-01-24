{-|
Module      : Menu.UI.Agenda
Description : Componente visual de grade de horários (Agenda).

Este módulo é responsável por transformar os dados brutos de turmas e disciplinas
em uma tabela bidimensional, organizando as turmas por dia da semana e 
blocos de horário (Manhã I e II).
-}

module Menu.UI.Agenda (drawAgenda) where

-- Bibliotecas Externas
import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Table
  ( renderTable
  , columnBorders
  , rowBorders
  , setDefaultColAlignment
  , table
  , alignCenter
  , Table)
import qualified Brick.Widgets.Table as T
import Lens.Micro ((^.))
import Data.List (intersperse)
import qualified Data.Map as M

-- Modelos e Tipos
import Models.Disciplina (getNomeDisciplina)
import Models.Types (DiaSemana(..), Horario(..), parseHorario, Slot)
import Models.Turma (getDisciplinaTurma, getHorarioTurma, getCodigoTurma)
import Sistema (Sistema(..))
import Menu.Tipos

-------------------------------------------------------------------------------
-- Renderização da Agenda
-------------------------------------------------------------------------------

-- | Desenha a tabela de horários centralizada.
-- Filtra as turmas do sistema e as posiciona conforme o dia e o prefixo da hora.

drawAgenda :: AppState -> Widget Name
drawAgenda st =
    let
        sis = st^.sistema
        mapDiscs = _disciplinas sis
        turmas = M.elems (_turmas sis)

        diasDaSemana = [Seg, Ter, Qua, Qui, Sex]

        horarios =
          [ ("Manhã I (08h-10h)",  'M', [2, 3])
          , ("Manhã II (10h-12h)", 'M', [4, 5])
          , ("Tarde I (14h-16h)",  'T', [2, 3])
          , ("Tarde II (16h-18h)", 'T', [4, 5])
          ]

        buscarNomeDisciplina dId =
          maybe dId getNomeDisciplina (M.lookup dId mapDiscs)

        pertenceAoBloco t diaAlvo turnoAlvo slotsAlvo =
            let 
                listaHorarios = getHorarioTurma t -- Isso agora retorna [Horario]
                -- Extraímos as strings de dentro de cada Horario e rodamos o parseHorario em todas
                slotsTurma = concatMap (\(Horario hStr) -> parseHorario hStr) listaHorarios
            in 
                any (\(d, turno, s) -> 
                    d == diaAlvo && 
                    turno == turnoAlvo && 
                    s `elem` slotsAlvo) 
                slotsTurma

        getTurmasNoBloco diaAlvo turnoAlvo slotsAlvo =
          let filtradas =
                [ withAttr (attrName "sucesso") $
                  str $
                    buscarNomeDisciplina (getDisciplinaTurma t)
                    ++ " (" ++ show (getCodigoTurma t) ++ ")"
                | t <- turmas
                , pertenceAoBloco t diaAlvo turnoAlvo slotsAlvo
                ]
          in padLeftRight 2 $
             padTopBottom 1 $
               if null filtradas
                  then withAttr (attrName "vazio") $ str "---"
                  else vBox $ intersperse (str " ") filtradas

        formataDia d = case d of
          Seg -> "Segunda-Feira"
          Ter -> "Terça-Feira"
          Qua -> "Quarta-Feira"
          Qui -> "Quinta-Feira"
          Sex -> "Sexta-Feira"
          _   -> show d

        header =
          map (withAttr (attrName "logo")
              . padTopBottom 1
              . str)
              ("Horários" : map formataDia diasDaSemana)

        makeRow (label, turno, listaSlots) =
          withAttr (attrName "bold") (padLeftRight 1 $ str label)
          : [ getTurmasNoBloco d turno listaSlots
            | d <- diasDaSemana
            ]

        tabela =
          T.renderTable $
            T.setDefaultColAlignment T.AlignCenter $
            T.columnBorders True $
            T.rowBorders True $
            T.table (header : map makeRow horarios)

    in
    withAttr (attrName "border") $
    borderWithLabel (str " Grade Horária de Turmas ") $
      padAll 1 $
        vBox
          [ viewport AgendaViewport Vertical $
              vLimit 45 $               -- altura inicial desejada
              hLimit 190 $
              vBox
                [ tabela
                , fill ' '               --  ISSO É O SEGREDO
                ]

          , hCenter $
              withAttr (attrName "info") $
              str "[↑ ↓ PgUp PgDn] Rolar  |  [Esc] Voltar"
          ]