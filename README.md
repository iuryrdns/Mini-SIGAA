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

## 📋 Sobre o Projeto

O **Mini-SIGAA** é uma versão minimalista do Sistema Integrado de Gestão de Atividades Acadêmicas (SIGAA), desenvolvido em Haskell para a disciplina de Paradigmas de Linguagens de Programação. O sistema permite gerenciar:

- 👨‍🎓 **Alunos** e suas matrículas
- 👨‍🏫 **Professores** e departamentos
- 📚 **Disciplinas** com pré-requisitos
- 🏫 **Turmas** com horários e salas
- 📊 **Relatórios** acadêmicos completos

##  Características

### 🔐 Sistema de Fases
O sistema opera em 3 fases distintas:
- **Fase 0**: Alterações Gerais (cadastros de entidades)
- **Fase 1**: Período de Matrículas
- **Fase 2**: Encerrado

### 🎯 Funcionalidades Principais

- ✅ Cadastro de professores, alunos, disciplinas e turmas
- ✅ Validação de pré-requisitos
- ✅ Detecção de conflitos de horário
- ✅ Sistema de CRA (Coeficiente de Rendimento Acadêmico)
- ✅ Geração de relatórios detalhados
- ✅ Persistência de dados em arquivo
- ✅ Validação de cursos e restrições

### 🛡️ Segurança de Tipos

Aproveitando o sistema de tipos de Haskell, o Mini-SIGAA garante:
- Tipos específicos para `Matricula`, `Codigo`, `Nome`, etc.
- Validação em tempo de compilação
- Impossibilidade de misturar diferentes tipos de identificadores

## 🚀 Instalação

### Pré-requisitos

- GHC (Glasgow Haskell Compiler) 9.6.7 ou superior
- Cabal 2.4 ou superior

### Passos

1. **Clone o repositório**
```bash
git clone https://github.com/seu-user/Mini-SIGAA.git
cd Mini-SIGAA
```

2. **Compile o projeto**
```bash
cabal build
```

3. **Execute o sistema**
```bash
cabal run mini-sigaa
```

## 💻 Como Usar

### Menu Principal

Ao iniciar o sistema, você verá o menu principal:

```
--- Mini Sigaa ---
1. Iniciar novo semestre
2. Visualizar Dados do Sistema
0. Sair
```

### Fluxo de Trabalho

#### 1️⃣ Iniciar Novo Semestre
```
Fase 0: Alterações Gerais
├── Adicionar professores
├── Adicionar alunos
├── Adicionar disciplinas
├── Adicionar turmas
└── Confirmar alterações
```

#### 2️⃣ Abrir Período de Matrículas
```
Fase 1: Matrículas
├── Realizar matrícula em turma
├── Ver turmas disponíveis
├── Verificar requisitos
├── Detectar conflitos de horário
└── Processar matrículas
```

#### 3️⃣ Finalizar Período
```
Fase 2: Encerrado
└── Gerar relatórios
```

### Exemplos de Uso

#### Cadastrar um Professor
```
Nome: Dr. João Silva
Matrícula: 12345
Departamento: Ciência da Computação
```

#### Cadastrar um Aluno
```
Nome: Maria Santos
Matrícula: 67890
CRA: 8.5
Curso: Ciência da Computação
Disciplinas Concluídas: [101, 102, 103]
```

#### Cadastrar uma Disciplina
```
Código: 201
Nome: Estruturas de Dados
Cursos: [Ciência da Computação, Engenharia]
Requisitos: [101, 102]
```

#### Criar uma Turma
```
Disciplina: 201
Professor: 12345
Sala: Lab 3
Horário: 2T34 (Terça, 14h-16h)
Capacidade: 40
```

## 🏗️ Arquitetura

### Estrutura de Pastas

```
Mini-SIGAA/
├── app/
│   └── Main.hs              # Ponto de entrada e interface do usuário
├── src/
│   ├── Sistema.hs           # Lógica principal do sistema
│   ├── Models/              # Modelos de dados
│   │   ├── Aluno.hs
│   │   ├── Professor.hs
│   │   ├── Disciplina.hs
│   │   ├── Turma.hs
│   │   └── Types.hs         # Tipos personalizados
│   ├── IOs/                 # Operações de entrada/saída
│   │   ├── ListarObj.hs
│   │   └── Relatorio.hs
│   └── Utils/               # Utilitários
│       └── Database.hs      # Persistência de dados
├── Mini-SIGAA.cabal         # Configuração do projeto
└── README.md
```

### Modelos de Dados

#### 🧑 Aluno
```haskell
data Aluno = Aluno
  { nome :: Nome
  , matricula :: Matricula
  , cra :: CRA
  , curso :: Curso
  , disciplinasConcluidas :: [Int]
  }
```

#### 👨‍🏫 Professor
```haskell
data Professor = Professor
  { nome :: Nome
  , matricula :: Matricula
  , departamento :: String
  }
```

#### 📖 Disciplina
```haskell
data Disciplina = Disciplina
  { codigo :: Codigo
  , nome :: Nome
  , cursos :: [Curso]
  , requisitos :: [Codigo]
  }
```

#### 🏫 Turma
```haskell
data Turma = Turma
  { codigoTurma :: Int
  , disciplina :: Codigo
  , professor :: Matricula
  , sala :: String
  , horario :: Horario
  , capacidade :: Int
  }
```

### Fluxo de Dados

```
┌─────────────┐
│  Interface  │
│    (Main)   │
└──────┬──────┘
       │
       ▼
┌─────────────┐      ┌──────────────┐
│   Sistema   │◄────►│   Database   │
└──────┬──────┘      └──────────────┘
       │
       ▼
┌─────────────┐
│   Models    │
│  (Aluno,    │
│  Professor, │
│  etc.)      │
└─────────────┘
```

## 📚 Documentação

### Tipos Personalizados

- `Matricula`: Identificador único de alunos e professores
- `Codigo`: Identificador único de disciplinas
- `Nome`: Nome validado de entidades
- `CRA`: Coeficiente de Rendimento Acadêmico (0.0 - 10.0)
- `Horario`: Representação de dia e hora de aulas

### Validações Implementadas

✔️ Validação de pré-requisitos  
✔️ Detecção de conflitos de horário  
✔️ Verificação de cursos permitidos  
✔️ Controle de capacidade de turmas  
✔️ Prevenção de matrículas duplicadas  
✔️ Validação de fases do sistema  

## 🔧 Dependências

```cabal
base
directory ^>=1.3.8.5
containers ^>=0.6.7
text ^>=2.0.2
```

## 🤝 Contribuindo

Contribuições são bem-vindas! Sinta-se à vontade para:

1. Fazer fork do projeto
2. Criar uma branch para sua feature (`git checkout -b feature/NovaFuncionalidade`)
3. Commit suas mudanças (`git commit -m 'Adiciona nova funcionalidade'`)
4. Push para a branch (`git push origin feature/NovaFuncionalidade`)
5. Abrir um Pull Request


## 👥 Autores

Desenvolvido com ❤️ em Haskell para a disciplina de Paradigmas de Linguagens de Programação

---

<div align="center">

**[⬆ Voltar ao topo](#-mini-sigaa)**

</div>
