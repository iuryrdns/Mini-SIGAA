module Main (main) where

import Data.List (nub)
import qualified Data.Map as Map
import IOs.Relatorio (gerarRelatorioGeral)
import Models.Aluno (criarAluno, getCursoAluno, getNomeAluno)
import Models.Disciplina (criarDisciplina, getNomeDisciplina)
import Models.Professor (criarProfessor, getDepartamentoProfessor, getNomeProfessor)
import Models.Turma (criarTurma, getCodigoTurma, getDisciplinaTurma, getHorarioTurma, getProfessorTurma, getSalaTurma)
import Models.Types (Codigo (..), Curso (..), Matricula (..), Nome (..), lerHorario, mkCRA, unCurso, unMatricula, unNome)
import Sistema
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Utils.Database (carregarSistema, salvarSistema)

main :: IO ()
main = do
  putStrLn "\n--- Carregando Sistema... ---"
  sistema <- carregarSistema
  let faseAtual = getFase sistema
  putStrLn ""
  putStrLn $ "--- Mini Sigaa (Fase Atual: " ++ show faseAtual ++ ") ---"
  putStrLn "1. Fase 0: Planejamento (Alterações)"
  putStrLn "2. Fase 1: Matrículas"
  putStrLn "3. Fase 2: Rematrículas"
  putStrLn "4. Visualizar Relatórios"
  if faseAtual == 3 then putStrLn "5. Iniciar Novo Semestre" else return ()
  putStrLn "0. Sair"
  putStrLn "---------------------------------------------------------"

  putStr "Escolha uma opção: "
  hFlush stdout
  opcao <- getLine

  case opcao of
    "1" -> do
      if faseAtual == 0 
        then menuAlteracoesGerais sistema
        else do 
          putStrLn "\nErro: Alterações só permitidas na Fase 0."
          main

    "2" -> do
        case faseAtual of
            0 -> do 
                case abrirPeriodoMatriculas sistema of
                    Left erro -> do
                        putStrLn $ "\nErro: " ++ erro
                        main
                    Right sisMat -> menuMatricula sisMat
            1 -> menuMatricula sistema 
    "3" -> do
        case faseAtual of
            1 -> prepararRematricula sistema
            2 -> loopRematricula sistema 
    
    "4" -> do
      putStrLn (gerarRelatorioGeral sistema)
      putStrLn "\nPressione Enter para voltar..."
      _ <- getLine
      main 
    
    "5" -> do
      case iniciarNovoSemestre sistema of
         Left erro -> do 
             putStrLn $ "\nErro: " ++ erro
             main 
         Right novoSis -> do
             putStrLn "\nNovo semestre iniciado!"
             salvarSistema novoSis
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
  putStrLn "8. Gerenciar turmas pendentes"
  putStrLn "9. Confirmar alterações e Resolver Conflitos"
  putStrLn "0. Voltar ao Menu Principal"
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
          putStrLn "                LISTA DE PROFESSORES"
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
            ( \(pos, (matricula, aluno)) -> do
                let nome = unNome (getNomeAluno aluno)
                let curso = unCurso (getCursoAluno aluno)
                putStrLn $ show pos ++ ". " ++ show (unMatricula matricula) ++ " | " ++ nome ++ " - Curso: " ++ curso
            )
            (zip [1 ..] alunos)
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
      gerenciarTurmasPendentes sistema
    "9" -> do
      verificarConflitos sistema
    "0" -> main
    _ -> do
      putStrLn "Opção inválida!"
      menuAlteracoesGerais sistema

gerenciarTurmasPendentes :: Sistema -> IO ()
gerenciarTurmasPendentes sistema = do
  let mapaTurmas = getTurmasCadastradas sistema
  if Map.null mapaTurmas
    then do
      putStrLn "\nNao ha turmas pendentes"
      putStr "\nAperte enter para continuar..."
      hFlush stdout
      _ <- getLine
      menuAlteracoesGerais sistema
    else do
      putStrLn "\n--- Operações de adição de turma pendentes ---"
      let turmas = Map.toList mapaTurmas
      mapM_
        ( \(cod, turma) -> do
            putStrLn $ "Cod: " ++ show cod ++ " | Horario: " ++ getHorarioTurma turma ++ " | Sala: " ++ getSalaTurma turma
        )
        turmas
      putStrLn "\nO que deseja fazer?"
      putStrLn "1. Editar turma"
      putStrLn "2. Remover turma"
      putStrLn "0. Voltar"
      putStr "\nEscolha uma opção: "
      hFlush stdout
      opcao <- getLine
      case opcao of
        "1" -> do
          putStr "\nCodigo da Turma a editar: "
          hFlush stdout
          codigoStr <- getLine
          putStr "Nova sala (deixe vazio para manter): "
          hFlush stdout
          novaSala <- getLine
          putStr "Novo horário (deixe vazio para manter): "
          hFlush stdout
          novoHorario <- getLine
          case readMaybe codigoStr of
            Just codigo -> do
              let sala = if null novaSala then Nothing else Just novaSala
              let horario = if null novoHorario then Nothing else Just novoHorario
              case editarTurmaPendente codigo sala horario sistema of
                Left erro -> do
                  putStrLn $ "\nErro: " ++ erro
                  putStr "\nAperte enter para continuar..."
                  hFlush stdout
                  _ <- getLine
                  gerenciarTurmasPendentes sistema
                Right novoSistema -> do
                  putStrLn "\nTurma editada com sucesso!"
                  putStr "\nAperte enter para continuar..."
                  hFlush stdout
                  _ <- getLine
                  gerenciarTurmasPendentes novoSistema
            Nothing -> do
              putStrLn "\nErro: Código inválido"
              putStr "\nAperte enter para continuar..."
              hFlush stdout
              _ <- getLine
              gerenciarTurmasPendentes sistema
        "2" -> do
          putStr "\nCodigo da Turma a remover: "
          hFlush stdout
          codigoStr <- getLine
          case readMaybe codigoStr of
            Just codigo ->
              case removerTurmaPendente codigo sistema of
                Left erro -> do
                  putStrLn $ "\nErro: " ++ erro
                  putStr "\nAperte enter para continuar..."
                  hFlush stdout
                  _ <- getLine
                  gerenciarTurmasPendentes sistema
                Right novoSistema -> do
                  putStrLn "\nTurma removida com sucesso!"
                  putStr "\nAperte enter para continuar..."
                  hFlush stdout
                  _ <- getLine
                  gerenciarTurmasPendentes novoSistema
            Nothing -> do
              putStrLn "\nErro: Código inválido"
              putStr "\nAperte enter para continuar..."
              hFlush stdout
              _ <- getLine
              gerenciarTurmasPendentes sistema
        "0" -> menuAlteracoesGerais sistema
        _ -> do
          putStrLn "\nOpção inválida"
          putStr "\nAperte enter para continuar..."
          hFlush stdout
          _ <- getLine
          gerenciarTurmasPendentes sistema

verificarConflitos :: Sistema -> IO ()
verificarConflitos sistema = do
  let conflitos = getTurmasConflitantes sistema
  putStrLn "\n--- Verificação de Conflitos ---"
  if null conflitos
    then do
      putStrLn "Nenhum conflito encontrado!"
      
      let sistemaEfetivado = efetivarAlteracoes sistema
      
      putStrLn "Salvando sistema e efetivando turmas..."
      salvarSistema sistemaEfetivado
      
      putStrLn "\nDados salvos!"
      putStrLn "Pressione Enter para voltar ao Menu Principal..."
      _ <- getLine
      main
    else do
      putStrLn "Foram encontrados conflitos de horário/sala:"
      mapM_
        ( \(i, (t1, t2)) -> do
            putStrLn $ show i ++ ". Turmas com conflito: " ++ show (getCodigoTurma t1) ++ " e " ++ show (getCodigoTurma t2)
        )
        (zip [1 ..] conflitos)
      putStr "\nResolva os conflitos removendo ou editando turmas."
      putStr "Aperte enter para continuar..."
      hFlush stdout
      _ <- getLine
      menuAlteracoesGerais sistema


menuMatricula :: Sistema -> IO ()
menuMatricula sistema = do
  putStrLn "\n--- Período de matrículas ---"
  putStrLn "1. Cadastrar matrícula"
  putStrLn "2. Mostrar matrículas em andamento"
  putStrLn "3. Finalizar período de matrículas (Salvar e Sair)"
  putStrLn "0. Voltar sem salvar"
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
          putStrLn "\nMatricula cadastrada com sucesso!"
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
      putStrLn "Salvando matrículas..."
      salvarSistema sistema
      putStrLn "Matrículas salvas!"
      putStrLn "Pressione Enter para voltar..."
      _ <- getLine
      main
    "0" -> do
      putStrLn "Voltando..."
      main
    _ -> do
      putStrLn "Opção inválida"
      menuMatricula sistema



prepararRematricula :: Sistema -> IO ()
prepararRematricula sistema = do
    let dados = obterMatriculasProcessadas sistema 
    let matriculasDeferidasMap = fst dados
    let matriculasIndeferidasMap = snd dados

    let matriculasDeferidas = [(m, t) | (t, ms) <- Map.toList matriculasDeferidasMap, m <- ms]
    let matriculasIndeferidas = [(m, t) | (t, ms) <- Map.toList matriculasIndeferidasMap, m <- ms]
    let sistemaComAlunosAdicionados = processarListaMatriculas sistema (_matriculas sistema)
    let sistemaAtualizado = setFase sistemaComAlunosAdicionados {_matriculas = matriculasDeferidas} 2

    putStrLn "\n--- Processamento da 1ª Etapa ---"
    
    putStrLn "\n--- Matrículas Deferidas (Confirmadas) ---"
    if null matriculasDeferidas
        then putStrLn "Nenhuma matrícula confirmada."
        else putStrLn (formatarMatriculas sistemaAtualizado matriculasDeferidas)
    
    putStrLn "\n--- Matrículas Indeferidas (Alunos devem fazer Rematrícula) ---"
    if null matriculasIndeferidas
        then putStrLn "Nenhuma matrícula indeferida."
        else putStrLn (formatarMatriculas sistemaAtualizado matriculasIndeferidas)

    putStrLn "\nPressione Enter para iniciar o cadastramento de Rematrículas..."
    _ <- getLine
    
    loopRematricula sistemaAtualizado

loopRematricula :: Sistema -> IO ()
loopRematricula sistema = do
  putStrLn "\n--- Período de Rematrícula ---"
  putStrLn "1. Cadastrar rematrícula"
  putStrLn "2. Mostrar rematrículas em andamento"
  putStrLn "3. Finalizar Semestre (Consolidar e Gerar Relatório)"
  putStrLn "0. Voltar ao Menu Principal"
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
              putStrLn "\nRematrícula cadastrada com sucesso!"
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
    "0" -> main
    _ -> do
      putStrLn "Opção inválida"
      loopRematricula sistema

finalizarSistema :: Sistema -> IO ()
finalizarSistema sistema = do
  let dados = obterMatriculasProcessadas sistema
  let matriculasDeferidasMap = fst dados
  let matriculasIndeferidasMap = snd dados
  let matriculasDeferidas = [(m, t) | (t, ms) <- Map.toList matriculasDeferidasMap, m <- ms]
  let matriculasIndeferidas = [(m, t) | (t, ms) <- Map.toList matriculasIndeferidasMap, m <- ms]
  let sistemaComAlunosAdicionados = processarListaMatriculas sistema (_rematriculas sistema)
  let todasMatriculas = nub (_matriculas sistema ++ matriculasDeferidas)
  let sistemaFinal = sistemaComAlunosAdicionados {_matriculas = todasMatriculas, _rematriculas = [], _fase = 3} 
  
  putStrLn "\n--- Resultado das Rematrículas ---"
  if null matriculasDeferidas
    then putStrLn "Nenhuma nova matrícula."
    else putStrLn (formatarMatriculas sistemaFinal matriculasDeferidas)
    
  if not (null matriculasIndeferidas)
    then do
        putStrLn "\n--- Rematrículas Indeferidas ---"
        putStrLn (formatarMatriculas sistemaFinal matriculasIndeferidas)
    else return ()

  putStrLn "\n--- Relatório Final ---"
  putStrLn (gerarRelatorioGeral sistemaFinal)
  putStrLn ""
  
  putStr "Deseja salvar e encerrar o semestre (y/n)? "
  hFlush stdout
  resp <- getLine
  if resp == "y"
    then do
      salvarSistema sistemaFinal
      putStrLn "Sistema salvo e finalizado com sucesso!"
      main
    else do 
      putStrLn "Retornando..."
      loopRematricula sistema

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