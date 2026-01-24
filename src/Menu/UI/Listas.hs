{-|
Module      : Menu.UI.Listas
Description : Componentes de visualização para listagens do sistema.

Este módulo contém as funções responsáveis por renderizar as listas de 
entidades (Alunos, Professores, Disciplinas e Solicitações), utilizando
o widget 'Brick.Widgets.List' integrado ao template padrão da aplicação.
-}

module Menu.UI.Listas
  ( drawListaAlunos
  , drawListaProfessores
  , drawListaDisciplinas
  , drawListaSolicitacoes
  , drawTabelaNotas
  ) where

-- Bibliotecas Externas
import Brick
import Brick.Widgets.Center (hCenter, vCenter)
import Lens.Micro ((^.))
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import qualified Brick.Widgets.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Maybe

-- Módulos de UI e Sistema
import Menu.UI.Common (templateUI)
import Menu.Tipos
import Sistema (Sistema(..))

-- Modelos (Entidades)
import qualified Models.Aluno as A
import qualified Models.Professor as B
import qualified Models.Disciplina as D
import qualified Models.Turma as T
import qualified Models.Matricula as MTR
import Models.Types (Solicitacao(..))

-------------------------------------------------------------------------------
-- Listagem de Alunos e Professores
-------------------------------------------------------------------------------

-- | Renderiza a lista de todos os alunos cadastrados com CRA e Matrícula.
drawListaAlunos :: AppState -> Widget Name
drawListaAlunos s = templateUI " Lista de Alunos Cadastrados " $
    let desenhaLinha selecionado aluno =
            let estilo = if selecionado then withAttr L.listSelectedAttr else id
                matricula = A.getMatriculaAluno aluno
                nome      = A.getNomeAluno aluno
                cra       = A.getCraAluno aluno
                info = show matricula ++ " - " ++ nome ++ " (CRA: " ++ show cra ++ ")"
            in estilo $ str info
    in vBox [ vLimit 15 $ L.renderList desenhaLinha True (s^.listaMenuAlunos)
            , str " "
            , hCenter $ str "[Esc] Voltar ao Menu"
            ]
-- | Renderiza a lista de todos os professores cadastrados.
drawListaProfessores :: AppState -> Widget Name
drawListaProfessores s = templateUI " Lista de Professores Cadastrados " $
    let desenhaLinha selecionado professor =
            let estilo = if selecionado then withAttr L.listSelectedAttr else id
                
                -- Extração dos dados
                matricula = show $ B.getMatriculaProfessor professor
                nome      = B.getNomeProfessor professor
                depto     = B.getDepartamentoProfessor professor
                
                -- CONSTRUÇÃO DAS COLUNAS:
                -- 1. hLimit: Define a largura total da "célula"
                -- 2. padRight Max: Preenche o que sobrar de espaço com caracteres vazios
                colMatricula = hLimit 12 $ padRight Max $ 
                               withAttr (attrName "destaque") $ str matricula
                
                colDepto     = hLimit 15 $ padRight Max $ 
                               withAttr (attrName "sucesso")  $ str depto
                
                -- O nome pode ocupar o restante do espaço disponível
                colNome      = str nome
                
                -- hBox monta a linha horizontalmente
                linha = hBox [ colMatricula
                             , colDepto
                             , colNome
                             ]
            in estilo linha

    in vBox [ vLimit 15 $ L.renderList desenhaLinha True (s^.listaMenuProfessores)
            , fill ' '
            , hCenter $ withAttr (attrName "info") $ str "[Esc] Voltar | [↑↓] Navegar"
            ]

-------------------------------------------------------------------------------
-- Listagem de Disciplinas e Solicitações
-------------------------------------------------------------------------------

-- | Renderiza disciplinas com cores diferenciadas por período letivo.
drawListaDisciplinas :: AppState -> Widget Name
drawListaDisciplinas s = templateUI " Lista de Disciplinas " $
    let desenhaLinha selecionado disciplina =
            let
                periodo = D.getPeriodoDisciplina disciplina
                codigo  = D.getCodigoDisciplina disciplina
                nome    = D.getNomeDisciplina disciplina
                -- Atributo dinâmico baseado no período (ex: periodo1, periodo2)
                
                nomeAttr = attrName ("periodo" ++ show periodo)
                info = show periodo ++ "º Período -- " ++ nome ++ " (" ++ codigo ++ ")"

                -- Lógica de estilo
                estilo = if selecionado
                         then withAttr L.listSelectedAttr
                         else withAttr nomeAttr
            in estilo $ str info

    in vBox [ vLimit 15 $ L.renderList desenhaLinha True (s^.listaMenuDisciplinas)
            , str " "
            , hCenter $ str "[Esc] Voltar ao Menu"
            ]

-- | Renderiza as solicitações de matrícula cruzando dados de Alunos e Turmas.
drawListaSolicitacoes :: AppState -> Widget Name
drawListaSolicitacoes s = templateUI " Solicitações Pendentes " $
    let sis = s^.sistema
        -- O segundo argumento agora é do tipo Solicitacao
        desenhaLinha selecionado sol =
            let estilo = if selecionado then withAttr L.listSelectedAttr else id
                -- Acessamos os campos diretamente do record Solicitacao
                idA = _sMatricula sol
                idT = _sTurma sol

                -- Busca o nome do Aluno
                nomeA = maybe (show idA) A.getNomeAluno (M.lookup idA (_alunos sis))

                -- Busca o nome da Disciplina através da Turma
                nomeD = Maybe.fromMaybe "Turma/Disc. Inexistente" $ do
                            t <- M.lookup idT (_turmas sis)
                            d <- M.lookup (T.getDisciplinaTurma t) (_disciplinas sis)
                            return $ D.getNomeDisciplina d

                info = nomeA ++ " -> " ++ nomeD ++ " (Turma " ++ show idT ++ ")"
            in estilo $ str info

    in vBox [ vLimit 15 $ L.renderList desenhaLinha True (s^.listaMenuSolicitacoes)
            , str " "
            , hCenter $ str "[Esc] Voltar ao Menu"
            ]

-------------------------------------------------------------------------------
-- Listagem de Notas do Aluno
-------------------------------------------------------------------------------

-- | Renderiza o boletim do aluno consultado, mostrando notas por turma e média.
drawTabelaNotas :: AppState -> Widget Name
drawTabelaNotas s = templateUI " Boletim do Aluno " $
    let 
        sis = s^.sistema
        mapaTurmas = _turmas sis
        mapaDiscs  = _disciplinas sis

        desenhaLinha selecionado (idTurma, notas) =
            let 
                estilo = if selecionado then withAttr L.listSelectedAttr else id
                
                -- USANDO OS GETTERS SUGERIDOS PELO COMPILADOR:
                nomeDisc = fromMaybe ("Turma " ++ show idTurma) $ do
                    turma <- M.lookup idTurma mapaTurmas
                    -- Use T.getDisciplinaTurma em vez de _codDisciplina
                    let cod = T.getDisciplinaTurma turma 
                    disc  <- M.lookup cod mapaDiscs
                    -- Use D.getNomeDisciplina em vez de _nomeDisciplina
                    return (D.getNomeDisciplina disc)

                n1 = fromMaybe 0.0 (A.n1 notas)
                n2 = fromMaybe 0.0 (A.n2 notas)
                n3 = fromMaybe 0.0 (A.n3 notas)
                media = (n1 + n2 + n3) / 3.0
                
                info = printf "%-15s | N1: %4.1f | N2: %4.1f | N3: %4.1f | Média: %4.1f" 
                               (take 15 nomeDisc) n1 n2 n3 media
            in estilo $ str info

        cabecalho = withAttr (attrName "bold") $ 
                    str " Disciplina  |  Nota 1  |  Nota 2   |  Nota 3   |  Média Final"
        divisor   = str $ replicate 70 '-'

    in vCenter $ vBox [ hCenter cabecalho
                      , hCenter divisor
                      , hCenter $ hLimit 75 $ vLimit 15 $ L.renderList desenhaLinha True (s^.listaMenuNotas)
                      , str " "
                      , hCenter $ str "[Esc] Pesquisar outro Aluno"
                      ]