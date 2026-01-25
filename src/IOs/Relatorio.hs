module IOs.Relatorio (gerarRelatorioGeral) where

import Data.List (intercalate, sortOn)
import qualified Data.Map as Map
import Models.Aluno (getCraAluno, getCursoAluno, getMatriculaAluno, getNomeAluno)
import Models.Disciplina (getCodigoDisciplina, getCursosDisciplina, getNomeDisciplina, getRequisitosDisciplina)
import Models.Professor (getDepartamentoProfessor, getFormacaoProfessor, getMatriculaProfessor, getNomeProfessor)
import Models.Turma (getAlunosTurma, getCapacidadeTurma, getCodigoTurma, getDisciplinaTurma, getHorarioTurma, getProfessorTurma, getSalaTurma)
import Models.Types (unCRA, unCodigo, unCurso, unMatricula, unNome)
import Sistema (Sistema (..), getAlunos, getDisciplinas, getMatriculasRealizadas, getProfessores, getTurmas, getTurmasCadastradas)

gerarRelatorioGeral :: Sistema -> String
gerarRelatorioGeral sistema =
  let alunos = Map.elems (getAlunos sistema)
      professores = Map.elems (getProfessores sistema)
      disciplinas = Map.elems (getDisciplinas sistema)
      -- Combine active turmas and pending turmas
      activeTurmas = Map.elems (getTurmas sistema)
      pendingTurmas = Map.elems (getTurmasCadastradas sistema)
      allTurmas = activeTurmas ++ pendingTurmas

      matriculas = _matriculas sistema
      fase = _fase sistema

      -- Helpers para buscar nomes
      buscaProf mat = case Map.lookup mat (getProfessores sistema) of
        Just p -> unNome (getNomeProfessor p)
        Nothing -> "Desconhecido"

      buscaDisc cod = case Map.lookup cod (getDisciplinas sistema) of
        Just d -> unNome (getNomeDisciplina d)
        Nothing -> "Desconhecida"

      buscaAluno mat = case Map.lookup mat (getAlunos sistema) of
        Just a -> unNome (getNomeAluno a)
        Nothing -> "Desconhecido"

      -- Formatação Alunos
      strAlunos =
        if null alunos
          then "Nenhum aluno cadastrado.\n"
          else
            unlines
              [ show (unMatricula (getMatriculaAluno a))
                  ++ " - "
                  ++ unNome (getNomeAluno a)
                  ++ " ("
                  ++ unCurso (getCursoAluno a)
                  ++ ", CRA: "
                  ++ show (unCRA (getCraAluno a))
                  ++ ")"
                | a <- sortOn (unNome . getNomeAluno) alunos
              ]

      -- Formatação Professores
      strProfessores =
        if null professores
          then "Nenhum professor cadastrado.\n"
          else
            unlines
              [ show (unMatricula (getMatriculaProfessor p))
                  ++ " - "
                  ++ unNome (getNomeProfessor p)
                  ++ " ("
                  ++ getDepartamentoProfessor p
                  ++ ", "
                  ++ getFormacaoProfessor p
                  ++ ")"
                | p <- sortOn (unNome . getNomeProfessor) professores
              ]

      -- Formatação Disciplinas
      strDisciplinas =
        if null disciplinas
          then "Nenhuma disciplina cadastrada.\n"
          else
            unlines
              [ unCodigo (getCodigoDisciplina d)
                  ++ " - "
                  ++ unNome (getNomeDisciplina d)
                  ++ "\n   Requisitos: "
                  ++ (if null (getRequisitosDisciplina d) then "Nenhum" else intercalate ", " (map unCodigo (getRequisitosDisciplina d)))
                  ++ "\n   Cursos: "
                  ++ intercalate ", " (map unCurso (getCursosDisciplina d))
                | d <- sortOn (unNome . getNomeDisciplina) disciplinas
              ]

      -- Formatação Turmas
      strTurmas =
        if null allTurmas
          then "Nenhuma turma cadastrada.\n"
          else
            unlines
              [ "Turma "
                  ++ show (getCodigoTurma t)
                  ++ " - "
                  ++ buscaDisc (getDisciplinaTurma t)
                  ++ "\n   Professor: "
                  ++ buscaProf (getProfessorTurma t)
                  ++ "\n   Horário: "
                  ++ show (getHorarioTurma t)
                  ++ " | Sala: "
                  ++ getSalaTurma t
                  ++ "\n   Capacidade: "
                  ++ show (length (getAlunosTurma t))
                  ++ "/"
                  ++ show (getCapacidadeTurma t)
                | t <- sortOn getCodigoTurma allTurmas
              ]
      formatarAlunos t =
        let alunos = getAlunosTurma t
         in if null alunos
              then "   Alunos: Nenhum aluno matriculado"
              else
                "   Alunos:\n"
                  ++ unlines
                    [ "      - " ++ unNome (getNomeAluno a) ++ " (" ++ show (unMatricula (getMatriculaAluno a)) ++ ")"
                      | a <- alunos
                    ]

      -- Formatação Matrículas (só mostra se fase >= 2 e tem matrículas)
      strMatriculas =
        if fase >= 2 && not (null matriculas)
          then
            "--- MATRÍCULAS EFETIVADAS ("
              ++ show (length matriculas)
              ++ ") ---\n"
              ++ unlines
                [ buscaAluno mat ++ " (" ++ show (unMatricula mat) ++ ") -> Turma " ++ show codTurma
                  | (mat, codTurma) <- matriculas
                ]
          else ""
   in "\n=========================================================\n"
        ++ "          RELATÓRIO GERAL DO SISTEMA\n"
        ++ "=========================================================\n\n"
        ++ "--- ALUNOS CADASTRADOS ("
        ++ show (length alunos)
        ++ ") ---\n"
        ++ strAlunos
        ++ "\n"
        ++ "--- PROFESSORES CADASTRADOS ("
        ++ show (length professores)
        ++ ") ---\n"
        ++ strProfessores
        ++ "\n"
        ++ "--- DISCIPLINAS CADASTRADAS ("
        ++ show (length disciplinas)
        ++ ") ---\n"
        ++ strDisciplinas
        ++ "\n"
        ++ "--- TURMAS CADASTRADAS ("
        ++ show (length allTurmas)
        ++ ") ---\n"
        ++ strTurmas
        ++ "\n"
        ++ strMatriculas
        ++ "=========================================================\n"
