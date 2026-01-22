module Main (main) where

import qualified Data.Map as Map
import IOs.Relatorio (gerarRelatorioGeral)
import Models.Aluno (criarAluno, getNomeAluno)
import Models.Disciplina (criarDisciplina, getNomeDisciplina)
import Models.Professor (criarProfessor, getDepartamentoProfessor, getNomeProfessor)
import Models.Turma (criarTurma, getCodigoTurma, getDisciplinaTurma, getHorarioTurma, getProfessorTurma, getSalaTurma)
import Models.Types (Codigo (..), Curso (..), Matricula (..), Nome (..), lerHorario, mkCRA, unMatricula, unNome)
import Sistema
  ( Sistema (_alunos, _disciplinas, _fase, _matriculas, _professores, _turmas),
    abrirPeriodoMatriculas,
    cadastrarAluno,
    cadastrarDisciplina,
    cadastrarProfessor,
    cadastrarTurma,
    efetivarAlteracoes,
    getAlunos,
    getDisciplinas,
    getFase,
    getMatriculasRealizadas,
    getProfessores,
    getTurmasCadastradas,
    getTurmasConflitantes,
    iniciarNovoSemestre,
    processarMatriculas,
    realizarMatricula,
    verificarRequisitos,
  )
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Utils.Database (carregarSistema, salvarSistema)

main :: IO ()
main = do
  putStrLn "Carregando Sistema..."
  sistemaInicial <- carregarSistema
  putStrLn ""
  putStrLn "--- Mini Sigaa ---"
  putStrLn "1. Iniciar novo semestre"
  putStrLn "2. Visualizar Dados do Sistema"
  putStrLn "0. Sair"
  putStrLn ""
  putStrLn "---------------------------------------------------------"

  putStr "Escolha uma opção: "
  hFlush stdout
  opcao <- getLine

  case opcao of
    "1" -> do
      case iniciarNovoSemestre sistemaInicial of
        Left erro -> do
          putStrLn erro
          if erro == "Sistema ja esta em fase de iniciamento!"
            then
              menuAlteracoesGerais sistemaInicial
            else
              return ()
        Right sistema -> do
          menuAlteracoesGerais sistema
    "2" -> do
      putStrLn (gerarRelatorioGeral sistemaInicial)
      putStrLn "\nPressione Enter para voltar ao menu..."
      _ <- getLine
      main
    "0" -> putStrLn "Saindo..."
    _ -> do
      putStrLn "Opção inválida"
      main

menuAlteracoesGerais :: Sistema -> IO ()
menuAlteracoesGerais sistema = do
  putStrLn "\n--- Período de Alterações Gerais ---"
  putStrLn "1. Adicionar professor"
  putStrLn "2. Adicionar aluno"
  putStrLn "3. Adicionar disciplina"
  putStrLn "4. Adicionar turma"
  putStrLn "5. Ver professores adicionados"
  putStrLn "6. Ver alunos adicionados"
  putStrLn "7. Ver disciplinas adicionadas"
  putStrLn "8. Ver operações de adição de turma pendentes"
  putStrLn "9. Confirmar alterações"
  putStrLn "0. Sair"
  putStrLn ""
  putStrLn "---------------------------------------------------------"

  putStr "Escolha uma opção: "
  hFlush stdout

  opcao <- getLine

  case opcao of
    "1" -> do
      putStr "Nome do Professor: "
      hFlush stdout
      nomeProfessor <- getLine

      putStr "Matricula do Professor: "
      hFlush stdout
      matricula <- getLine

      putStr "Departamento: "
      hFlush stdout
      depProf <- getLine

      putStr "Formação: "
      hFlush stdout
      formacao <- getLine

      let novoProf = criarProfessor (Matricula (read matricula)) (Nome nomeProfessor) depProf formacao

      case cadastrarProfessor novoProf sistema of
        Left erro -> do
          putStrLn $ "\nErro: " ++ erro
          menuAlteracoesGerais sistema
        Right novoSistema -> do
          putStrLn "\nProfessor cadastrado com sucesso!"
          menuAlteracoesGerais novoSistema
    "2" -> do
      putStr "Nome do Aluno: "
      hFlush stdout
      nomeAluno <- getLine

      putStr "Matricula: "
      hFlush stdout
      matriculaAluno <- getLine

      putStr "Curso: "
      hFlush stdout
      cursoAluno <- getLine

      putStr "CRA: "
      hFlush stdout
      craAlunoStr <- getLine

      case readMaybe craAlunoStr of
        Just craVal ->
          case mkCRA craVal of
            Just cra -> do
              let novoAluno = criarAluno (Matricula (read matriculaAluno)) (Nome nomeAluno) (Curso cursoAluno) cra

              case cadastrarAluno novoAluno sistema of
                Left erro -> do
                  putStrLn $ "\nErro: " ++ erro
                  menuAlteracoesGerais sistema
                Right novoSistema -> do
                  putStrLn "\nAluno cadastrado com sucesso!"
                  menuAlteracoesGerais novoSistema
            Nothing -> do
              putStrLn "\nErro: CRA inválido (deve ser entre 0.0 e 10.0)"
              menuAlteracoesGerais sistema
        Nothing -> do
          putStrLn "\nErro: CRA deve ser um número."
          menuAlteracoesGerais sistema
    "3" -> do
      putStr "Codigo da Disciplina: "
      hFlush stdout
      codigo <- getLine

      putStr "Nome da Disciplina: "
      hFlush stdout
      nomeDisciplina <- getLine

      putStr "Requisitos (separados por espaço): "
      hFlush stdout
      entrada <- getLine

      putStr "Cursos permitidos (separados por espaço): "
      hFlush stdout
      entradaCursos <- getLine

      let requisitos = words entrada
      let cursos = map Curso (words entradaCursos)

      case verificarRequisitos requisitos sistema of
        Left erro -> do
          putStrLn $ "\nErro: " ++ erro
          menuAlteracoesGerais sistema
        Right _ -> do
          -- Requisitos validados
          let novaDisciplina = criarDisciplina (Codigo codigo) (Nome nomeDisciplina) (map Codigo requisitos) cursos

          case cadastrarDisciplina novaDisciplina sistema of
            Left erro -> do
              putStrLn $ "\nErro: " ++ erro
              menuAlteracoesGerais sistema
            Right novoSistema -> do
              putStrLn "\nDisciplina cadastrada."
              menuAlteracoesGerais novoSistema
    "4" -> do
      putStr "Codigo da Turma: "
      hFlush stdout
      codigoTurmaStr <- getLine

      putStr "Matricula Professor: "
      hFlush stdout
      professorDisciplinaStr <- getLine

      putStr "Codigo da Disciplina: "
      hFlush stdout
      disciplina <- getLine

      putStr "Horario: "
      hFlush stdout
      horario <- getLine

      putStr "Sala: "
      hFlush stdout
      sala <- getLine

      putStr "Capacidade: "
      hFlush stdout
      qtdAlunosStr <- getLine

      case (readMaybe codigoTurmaStr, readMaybe professorDisciplinaStr, readMaybe qtdAlunosStr) of
        (Just codigoTurma, Just professorDisciplina, Just qtdAlunos) -> do
          let novaTurma = criarTurma codigoTurma (Matricula professorDisciplina) (Codigo disciplina) (lerHorario horario) sala qtdAlunos
          case cadastrarTurma novaTurma sistema of
            Left erro -> do
              putStrLn $ "\nErro: " ++ erro
              menuAlteracoesGerais sistema
            Right novoSistema -> do
              putStrLn "\nOperação de adição de turma registrada!"
              menuAlteracoesGerais novoSistema
        _ -> do
          putStrLn "\nErro: Entradas inválidas! Certifique-se de que Código, Matrícula do Professor e Capacidade sejam números inteiros."
          menuAlteracoesGerais sistema
    "5" -> do
      let mapaProf = getProfessores sistema
      if Map.null mapaProf
        then putStr "\nNao ha professores cadastrados"
        else do
          putStrLn "\n========================================================"
          putStrLn "                 LISTA DE PROFESSORES"
          putStrLn "========================================================"
          putStrLn ""
          putStrLn "  #   | Matrícula | Nome                    | Departamento"
          putStrLn " ---  +-----------+-------------------------+--------------"

          let professores = Map.toList mapaProf

          mapM_
            ( \(pos, (matricula, professor)) -> do
                let nome = unNome (getNomeProfessor professor)
                let departamento = getDepartamentoProfessor professor
                putStrLn $ show pos ++ ". " ++ show (unMatricula matricula) ++ " | " ++ nome ++ " - Dep: " ++ departamento
            )
            (zip [1 ..] professores)

      putStr "\nAperte enter para continuar..."
      hFlush stdout
      _ <- getLine
      menuAlteracoesGerais sistema
    "6" -> do
      let mapaAlunos = getAlunos sistema
      if Map.null mapaAlunos
        then putStrLn "\nNao ha alunos cadastrados"
        else do
          let alunos = Map.toList mapaAlunos
          putStrLn "\n========================================================"
          putStrLn "                  LISTA DE ALUNOS"
          putStrLn "========================================================"
          putStrLn ""
          putStrLn "  #   | Matrícula | Nome                    | Curso"
          putStrLn " ---  +-----------+-------------------------+-----------"

          mapM_
            ( \(matricula, aluno) -> do
                let nome = unNome (getNomeAluno aluno)
                putStrLn $ show (unMatricula matricula) ++ " - " ++ nome
            )
            alunos

      putStr "\nAperte enter para continuar..."
      hFlush stdout
      _ <- getLine

      menuAlteracoesGerais sistema
    "7" -> do
      let mapaDisc = getDisciplinas sistema
      if Map.null mapaDisc
        then putStrLn "\nNao ha disciplinas cadastradas"
        else do
          putStrLn "\n--- Lista de Disciplinas ---"
          let disciplinas = Map.toList mapaDisc
          mapM_
            ( \(_, disc) -> do
                putStrLn $ unNome (getNomeDisciplina disc)
            )
            disciplinas

      putStr "\nAperte enter para continuar..."
      hFlush stdout
      _ <- getLine
      menuAlteracoesGerais sistema
    "8" -> do
      let mapaTurmas = getTurmasCadastradas sistema
      if Map.null mapaTurmas
        then putStrLn "\nNao ha turmas cadastradas"
        else do
          putStrLn "\n--- Operações de adição de turma ---"
          let turmas = Map.toList mapaTurmas
          mapM_
            ( \(cod, turma) -> do
                putStrLn $ "Cod: " ++ show cod ++ " | Horario: " ++ show (getHorarioTurma turma) ++ " | Sala: " ++ getSalaTurma turma
            )
            turmas

      putStr "\nAperte enter para continuar..."
      hFlush stdout
      _ <- getLine
      menuAlteracoesGerais sistema
    "9" -> do
      verificarConflitos sistema
    "0" -> putStrLn "Saindo..."
    _ -> do
      putStrLn "Opção inválida!"
      menuAlteracoesGerais sistema

verificarConflitos :: Sistema -> IO ()
verificarConflitos sistema = do
  let conflitos = getTurmasConflitantes sistema

  putStrLn "\n--- Conflitos encontrados ---"
  if null conflitos
    then do
      putStrLn "Não há nada aqui!"
      putStrLn "\nAperte enter para continuar..."
      hFlush stdout
      _ <- getLine

      let sistemaEfetivado = efetivarAlteracoes sistema
      case abrirPeriodoMatriculas sistemaEfetivado of
        Left erro -> do
          putStrLn $ "Erro ao abrir matriculas: " ++ erro
          menuAlteracoesGerais sistema
        Right sistemaMatricula -> menuMatricula sistemaMatricula
    else do
      mapM_
        ( \(i, (t1, t2)) -> do
            putStrLn $ show i ++ ". Turmas com conflito de horário: " ++ show (getCodigoTurma t1) ++ " e " ++ show (getCodigoTurma t2)
        )
        (zip [1 ..] conflitos)
      putStr "\nAperte enter para continuar..."
      hFlush stdout
      _ <- getLine
      menuAlteracoesGerais sistema

menuMatricula :: Sistema -> IO ()
menuMatricula sistema = do
  putStrLn "\n--- Período de matrículas ---"
  putStrLn "1. Cadastrar matrícula"
  putStrLn "2. Mostrar matrículas em andamento"
  putStrLn "3. Finalizar período de matrículas"
  putStrLn "0. Sair"
  putStrLn ""
  putStrLn "--------------------------------------------------------"
  putStr "Escolha uma opção: "
  hFlush stdout

  opcao <- getLine

  case opcao of
    "1" -> do
      putStr "Matrícula do aluno: "
      hFlush stdout
      matricula <- getLine

      putStr "Codigo da Turma: "
      hFlush stdout
      turma <- getLine

      case realizarMatricula (Matricula (read matricula)) (read turma) sistema of
        Left erro -> do
          putStrLn $ "\nErro: " ++ erro
          menuMatricula sistema
        Right novoSistema -> do
          putStrLn "\nMatricula cadastrada com sucesso"
          menuMatricula novoSistema
    "2" -> do
      case getMatriculasRealizadas sistema of
        Left erro -> do
          putStrLn $ "\n" ++ erro
          menuMatricula sistema
        Right relatorio -> do
          putStrLn "\n--- Matrículas realizadas ---"
          putStrLn relatorio
          putStrLn "Aperte enter para continuar..."
          hFlush stdout
          _ <- getLine
          menuMatricula sistema
    "3" -> do
      menuRematricula sistema
    "0" -> do
      putStrLn "Saindo..."
    -- Poderia salvar aqui
    _ -> do
      putStrLn "Opção inválida"
      menuMatricula sistema

menuRematricula :: Sistema -> IO ()
menuRematricula sistema = do
  let dados = processarMatriculas sistema
  let matriculasDeferidasMap = fst dados
  let matriculasIndeferidasMap = snd dados

  let matriculasDeferidas = [(m, t) | (t, ms) <- Map.toList matriculasDeferidasMap, m <- ms]
  let matriculasIndeferidas = [(m, t) | (t, ms) <- Map.toList matriculasIndeferidasMap, m <- ms]

  let sistemaAtualizado = sistema {_matriculas = matriculasDeferidas}

  putStrLn "\n--- Matrículas realizadas ---"
  if null matriculasDeferidas
    then putStrLn "Não há nenhuma matrícula!"
    else putStrLn (formatarMatriculas sistemaAtualizado matriculasDeferidas)
  putStrLn "Aperte enter para continuar..."
  hFlush stdout
  _ <- getLine

  putStrLn "\n--- Matrículas indeferidas ---"
  putStrLn ""
  if null matriculasIndeferidas
    then return ()
    else putStrLn (formatarMatriculas sistemaAtualizado matriculasIndeferidas)

  putStrLn "Aperte enter para continuar..."
  hFlush stdout
  _ <- getLine

  loopRematricula sistemaAtualizado

loopRematricula :: Sistema -> IO ()
loopRematricula sistema = do
  putStrLn "\n--- Período de rematrícula ---"
  putStrLn "1. Cadastrar rematrícula"
  putStrLn "2. Mostrar rematrículas em andamento"
  putStrLn "3. Finalizar período de rematrículas"
  putStrLn "0. Sair"
  putStrLn ""
  putStrLn "--------------------------------------------------------"
  putStr "Escolha uma opção: "
  hFlush stdout

  opcao <- getLine

  case opcao of
    "1" -> do
      putStr "Matrícula do aluno: "
      hFlush stdout
      matricula <- getLine

      putStr "Codigo da Turma: "
      hFlush stdout
      turma <- getLine

      case (readMaybe matricula, readMaybe turma) of
        (Just matId, Just turmaId) ->
          case realizarMatricula (Matricula matId) turmaId sistema of
            Left erro -> do
              putStrLn $ "\nErro: " ++ erro
              loopRematricula sistema
            Right novoSistema -> do
              putStrLn "\nRematrícula cadastrada com sucesso"
              loopRematricula novoSistema
        _ -> do
          putStrLn "\nErro: Dados inválidos"
          loopRematricula sistema
    "2" -> do
      case getMatriculasRealizadas sistema of
        Left erro -> do
          putStrLn $ "\n" ++ erro
          loopRematricula sistema
        Right relatorio -> do
          putStrLn "\n--- Rematrículas em andamento ---"
          putStrLn relatorio
          putStrLn "Aperte enter para continuar..."
          hFlush stdout
          _ <- getLine
          loopRematricula sistema
    "3" -> do
      finalizarSistema sistema
    "0" -> putStrLn "Saindo..."
    _ -> do
      putStrLn "Opção inválida"
      loopRematricula sistema

finalizarSistema :: Sistema -> IO ()
finalizarSistema sistema = do
  let dados = processarMatriculas sistema
  let matriculasDeferidasMap = fst dados
  let matriculasIndeferidasMap = snd dados

  let matriculasDeferidas = [(m, t) | (t, ms) <- Map.toList matriculasDeferidasMap, m <- ms]
  let matriculasIndeferidas = [(m, t) | (t, ms) <- Map.toList matriculasIndeferidasMap, m <- ms]

  let sistemaFinal = sistema {_matriculas = matriculasDeferidas, _fase = 2}

  putStrLn "\n--- Rematrículas realizadas ---"
  if null matriculasDeferidas
    then putStrLn "Não há nenhuma matrícula!"
    else putStrLn (formatarMatriculas sistemaFinal matriculasDeferidas)
  putStrLn "Aperte enter para continuar..."
  hFlush stdout
  _ <- getLine

  putStrLn "\n--- Rematrículas indeferidas ---"
  putStrLn ""
  if null matriculasIndeferidas
    then return ()
    else putStrLn (formatarMatriculas sistemaFinal matriculasIndeferidas)

  putStrLn "Aperte enter para continuar..."
  hFlush stdout
  _ <- getLine

  putStrLn "\n--- Panorama geral das atividades do MiniSIGAA ---"
  putStrLn (gerarRelatorioGeral sistemaFinal)
  putStrLn ""
  putStrLn "Aperte enter para continuar..."
  hFlush stdout
  _ <- getLine

  putStr "Deseja salvar essas alterações (y/n)? "
  hFlush stdout
  resp <- getLine
  if resp == "y"
    then do
      salvarSistema sistemaFinal
      putStrLn "Sistema salvo com sucesso!"
    else putStrLn "Alterações descartadas."

formatarMatriculas :: Sistema -> [(Matricula, Int)] -> String
formatarMatriculas sistema matriculas =
  let lista = zip [1 ..] matriculas
      montarLinha (idx, (idAluno, idTurma)) =
        let aluno = (Map.!) (_alunos sistema) idAluno
            turma = (Map.!) (_turmas sistema) idTurma
            codDisc = getDisciplinaTurma turma
            disciplina = (Map.!) (_disciplinas sistema) codDisc
            nomeAluno = unNome (getNomeAluno aluno)
            matrAluno = show (unMatricula idAluno)
            nomeDisc = unNome (getNomeDisciplina disciplina)
            codTurma = show (getCodigoTurma turma)
         in show idx ++ ". " ++ nomeAluno ++ " - " ++ matrAluno ++ ": " ++ nomeDisc ++ " " ++ codTurma
   in unlines (map montarLinha lista)