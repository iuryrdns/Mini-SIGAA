<div align="center">

<h1>
   Mini-SIGAA
</h1>

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)
![Version](https://img.shields.io/badge/version-0.1.0-blue?style=for-the-badge)
![UFCG](https://img.shields.io/badge/UFCG-Universidade%20Federal%20de%20Campina%20Grande-003366?style=for-the-badge)

**Sistema Integrado de Gestão de Atividades Acadêmicas**

Uma implementação simplificada em Haskell do SIGAA para gerenciamento universitário

[Características](#-características) •
[Instalação](#-instalação) •
[Como Usar](#-como-usar) •
[Arquitetura](#-arquitetura) •
[Documentação](#-documentação)

</div>

---

##  Sobre o Projeto

O **Mini-SIGAA** é uma versão minimalista do Sistema Integrado de Gestão de Atividades Acadêmicas (SIGAA), desenvolvido em Haskell para a disciplina de Paradigmas de Linguagens de Programação. O sistema permite gerenciar:

-  **Alunos** e suas matrículas
-  **Professores** e departamentos
-  **Disciplinas** com pré-requisitos
-  **Turmas** com horários e salas
-  **Relatórios** acadêmicos completos

###  Interface TUI (Text User Interface)

O sistema utiliza a biblioteca **Brick** para fornecer uma interface de usuário textual interativa e moderna, com navegação por teclado, menus dinâmicos e formulários intuitivos.

##  Características

###  Sistema de Fases
O sistema opera em 4 fases distintas:
- **Fase 0**: Alterações Gerais (cadastros de entidades)
- **Fase 1**: Período de Matrículas
- **Fase 2**: Período de Rematrículas
- **Fase 3**: Finalizar Semestre
- **Fase 4**: Lançamento de Notas

###  Funcionalidades Principais

-  Interface TUI interativa com a biblioteca Brick
-  Cadastro de professores, alunos, disciplinas e turmas
-  Sistema de solicitações de matrícula
-  Validação de pré-requisitos
-  Detecção automática de conflitos de horário
-  Sistema de CRA (Coeficiente de Rendimento Acadêmico)
-  Lançamento de notas
-  Geração de relatórios detalhados
-  Persistência de dados em JSON
-  Validação de cursos e restrições acadêmicas
-  Edição de turmas (sala e horário)
-  Navegação intuitiva por teclado

###  Segurança de Tipos

Aproveitando o sistema de tipos de Haskell, o Mini-SIGAA garante:
- Tipos específicos para `Matricula`, `Codigo`, `Nome`, etc.
- Validação em tempo de compilação
- Impossibilidade de misturar diferentes tipos de identificadores

##  Instalação

### Pré-requisitos

- **GHC** (Glasgow Haskell Compiler) 9.6.7 ou superior
- **Cabal** 3.0 ou superior

### Instalação do GHC e Cabal (Windows)

#### Opção 1: GHCup (Recomendado)
```powershell
# Instalar GHCup
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }

# Instalar GHC 9.6.7
ghcup install ghc 9.6.7
ghcup set ghc 9.6.7

# Instalar Cabal
ghcup install cabal latest
```



### Passos de Instalação do Projeto

1. **Clone o repositório**
```bash
git clone https://github.com/seu-usuario/Mini-SIGAA.git
cd Mini-SIGAA
```

2. **Atualize as dependências**
```bash
cabal update
```

3. **Compile o projeto**
```bash
cabal build
```

4. **Execute o sistema**
```bash
cabal run mini-sigaa
```

### Compilação e Instalação Local

Para instalar o executável localmente:
```bash
cabal install
```

O executável será instalado em `~/.cabal/bin/mini-sigaa` (ou equivalente no Windows).

##  Como Usar

### Iniciando o Sistema

Ao executar o projeto com `cabal run mini-sigaa`, você será apresentado a uma interface TUI moderna com:
- **Menu principal** navegável com setas ↑/↓
- **Formulários** interativos para cadastro
- **Listas** de visualização de dados
- **Mensagens** de sucesso/erro com cores

### Navegação

- **↑/↓**: Navegar entre opções de menu
- **Enter**: Selecionar/Confirmar
- **Tab**: Alternar entre campos de formulário
- **Esc**: Voltar/Cancelar
- **q**: Sair do sistema

### Fluxo de Trabalho

#### 1️⃣ Fase 0: Planejamento (Cadastros)
```
Cadastros Disponíveis:
├── Cadastrar Aluno (Nome, Matrícula, Curso, CRA)
├── Cadastrar Professor (Nome, Matrícula, Departamento, Formação)
├── Cadastrar Disciplina (Código, Nome, Cursos, Pré-requisitos)
├── Cadastrar Turma (Código, Professor, Disciplina, Horário, Sala)
└── Visualizar Dados (Alunos, Professores, Disciplinas, Turmas)
```

#### 2️⃣ Fase 1: Período de Matrículas
```
Matrículas:
├── Solicitar Matrícula (Aluno + Turma)
├── Visualizar Solicitações
├── Verificar Conflitos de Horário
├── Processar Solicitações
└── Avançar para Rematrículas
```

#### 3️⃣ Fase 2: Rematrículas
```
Rematrículas:
├── Realizar Rematrículas
├── Processar Solicitações
└── Finalizar Semestre
```

#### 4️⃣ Fase 3/4: Finalização
```
Finalização:
├── Lançar Notas (Matrícula + Disciplina + Valor)
├── Gerar Relatórios Completos
│   ├── Alunos por Turma
│   ├── Disciplinas por Professor
│   ├── Ranking de CRA
│   └── Estatísticas Gerais
└── Iniciar Novo Semestre
```

### Exemplos de Uso

#### Cadastrar um Aluno
```
Formulário de Cadastro:
Nome: Maria Santos
Matrícula: 67890
CRA: 8.5
Curso: Ciência da Computação
```

#### Cadastrar um Professor
```
Formulário de Cadastro:
Nome: Dr. João Silva
Matrícula: 12345
Departamento: Computação
Formação: Doutorado em IA
```

#### Cadastrar uma Disciplina
```
Formulário de Cadastro:
Código: 201
Nome: Estruturas de Dados
Cursos Permitidos: Ciência da Computação,Engenharia
Pré-requisitos: (Disciplinas que já estejam cadastradas no sistema)
```

#### Criar uma Turma
```
Formulário de Cadastro:
Código da Turma: 01
Matrícula do Professor: 12345
Código da Disciplina: 201
Horário: 2T34 (Segunda à Tarde, Terceira e Quarta Aula)
Sala: Lab 3
Capacidade Máxima: 40
```


## 🏗️ Arquitetura

### Estrutura de Pastas

```
Mini-SIGAA/
├── app/
│   └── Main.hs                 # Ponto de entrada e configuração Brick
├── src/
│   ├── Sistema.hs              # Lógica principal e regras de negócio
│   ├── BrickMenu/              # Interface TUI com Brick
│   │   ├── Tipos.hs           # Tipos e estado da aplicação
│   │   ├── UI.hs              # Renderização da interface
│   │   └── Events.hs          # Manipulação de eventos
│   ├── Models/                 # Modelos de dados
│   │   ├── Aluno.hs           # Modelo de Aluno
│   │   ├── Professor.hs       # Modelo de Professor
│   │   ├── Disciplina.hs      # Modelo de Disciplina
│   │   ├── Turma.hs           # Modelo de Turma
│   │   └── Types.hs           # Tipos personalizados e utilidades
│   ├── IOs/                    # Operações de entrada/saída
│   │   ├── ListarObj.hs       # Funções de listagem
│   │   └── Relatorio.hs       # Geração de relatórios
│   └── Utils/                  # Utilitários
│       └── Database.hs         # Persistência em JSON
├── Mini-SIGAA.cabal            # Configuração do projeto
├── dados.db                    # Arquivo de persistência (JSON)
└── README.md
```

### Modelos de Dados

####  Aluno
```haskell
data Aluno = Aluno
  { _nomeAluno :: Nome
  , _matriculaAluno :: Matricula
  , _craAluno :: CRA
  , _cursoAluno :: Curso
  , _disciplinasConcluidasAluno :: [Int]
  , _notasAluno :: [(Codigo, Double)]
  }
```
####  Professor
```haskell
data Professor = Professor
  { _nomeProfessor :: Nome
  , _matriculaProfessor :: Matricula
  , _departamento :: String
  , _formacao :: String
  }
```

####  Disciplina
```haskell
data Disciplina = Disciplina
  { _codigoDisciplina :: Codigo
  , _nomeDisciplina :: Nome
  , _cursosDisciplina :: [Curso]
  , _requisitosDisciplina :: [Int]
  }
```

####  Turma
```haskell
data Turma = Turma
  { _codigoTurma :: Codigo
  , _professorTurma :: Matricula
  , _disciplinaTurma :: Codigo
  , _horarioTurma :: Horario
  , _salaTurma :: String
  , _capacidadeTurma :: Int
  , _alunosTurma :: [Matricula]
  }
```

####  Sistema
```haskell
data Sistema = Sistema
  { _alunos :: Map Matricula Aluno
  , _professores :: Map Matricula Professor
  , _disciplinas :: Map Codigo Disciplina
  , _matriculas :: [(Matricula, Codigo)]      -- Solicitações de matrícula
  , _rematriculas :: [(Matricula, Codigo)]    -- Solicitações de rematrícula
  , _turmas :: Map Codigo Turma               -- Turmas ativas
  , _cadastroDeTurmas :: Map Codigo Turma     -- Turmas pendentes
  , _fase :: Int                              -- Fase atual do semestre
  }
```

### Fluxo de Dados

```
┌─────────────────┐
│   Main.hs       │  ← Inicialização Brick App
│   (Brick App)   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   AppState      │  ← Estado da aplicação TUI
│   (BrickMenu/   │
│    Tipos.hs)    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐      ┌──────────────────┐
│   Sistema.hs    │◄────►│  Database.hs     │  ← Persistência JSON
│   (Lógica Core) │      │  (dados.db)      │
└────────┬────────┘      └──────────────────┘
         │
         ▼
┌─────────────────┐
│   Models/       │  ← Modelos de dados
│   (Aluno,       │
│   Professor,    │
│   Disciplina,   │
│   Turma)        │
└─────────────────┘
```

### Tecnologias Utilizadas

- **Brick**: Framework TUI para interfaces interativas
- **Vty**: Backend de terminal para Brick
- **Containers**: Map para estruturas de dados eficientes

## 📚 Documentação

### Tipos Personalizados

- `Matricula`: Identificador único de alunos e professores (wrapper de String)
- `Codigo`: Identificador único de disciplinas e turmas (wrapper de Int)
- `Nome`: Nome validado de entidades (wrapper de String)
- `CRA`: Coeficiente de Rendimento Acadêmico (wrapper de Double, 0.0 - 10.0)
- `Curso`: Tipo de curso (wrapper de String)
- `Horario`: Representação de dia, turno e aulas (ex: 2T34, 4M12)

### Validações Implementadas

✔️ **Pré-requisitos**: Verifica se aluno completou disciplinas necessárias  
✔️ **Conflitos de horário**: Detecta sobreposições em matrículas  
✔️ **Cursos permitidos**: Valida se disciplina é oferecida para o curso do aluno  
✔️ **Capacidade de turmas**: Controla limite de alunos por turma  
✔️ **Matrículas duplicadas**: Previne inscrição repetida na mesma disciplina  
✔️ **Fases do sistema**: Restringe ações conforme período acadêmico  
✔️ **CRA válido**: Garante valores entre 0 e 10  
✔️ **Formato de horário**: Valida padrão de horários acadêmicos

### Funcionalidades por Módulo

#### Sistema.hs
- Gerenciamento de fases do semestre
- Processamento de matrículas e rematrículas
- Validação de requisitos e conflitos
- Lançamento de notas
- Cálculo de CRA

#### BrickMenu/
- **Tipos.hs**: Definição de estado e tipos da TUI
- **UI.hs**: Renderização de telas e componentes
- **Events.hs**: Manipulação de eventos de teclado

#### Models/
- Definição de tipos de domínio
- Funções de acesso e manipulação
- Validações específicas de cada entidade

#### Utils/Database.hs
- Serialização para JSON com Aeson
- Carregamento e salvamento automático
- Tratamento de erros de I/O  

## 🔧 Dependências

```cabal
# Core
base                    # Biblioteca base do Haskell

# Estruturas de dados
containers  ^>=0.6.7    # Map, Set e outras estruturas
vector                  # Vetores imutáveis

# Interface TUI
brick                   # Framework para Text User Interface
vty                     # Backend de terminal

# Lentes
microlens               # Lentes para manipulação de estado
microlens-th            # Template Haskell para lentes
microlens-mtl           # Integração com MTL
microlens-ghc           # Integração com GHC

# Serialização
aeson                   # JSON encoding/decoding
text        ^>=2.0.2    # Manipulação eficiente de texto
bytestring              # Strings binárias

# I/O
directory   ^>=1.3.8.5  # Operações de diretório e arquivo

# Mônadas
mtl                     # Transformadores de mônadas
```

## 👥 Autores

Desenvolvido por estudantes de Ciência da Computação da UFCG para a disciplina de Paradigmas de Linguagens de Programação:

- **Iury Ruan do Nascimento Santos**
- **Adley Silva Mendes**
- **Rafael Morais**
- **José Paulo Freitas da Silva Farias**
- **Ryan Victor Lucena**

### Recursos Adicionais

-  [Documentação do Haskell](https://www.haskell.org/documentation/)
-  [Documentação do Brick](https://hackage.haskell.org/package/brick)
-  [Learn You a Haskell](http://learnyouahaskell.com/)

---

<div align="center">

**[⬆ Voltar ao topo](#-mini-sigaa)**

Feito com programação funcional 🚀

</div>
