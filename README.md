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
- **Fase 2**: Lançamento e Consulta de Notas

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

## ▶️ Como Usar

O sistema é operado por meio de **menus interativos em terminal (TUI)**, construídos com a biblioteca **Brick**.
Toda a navegação é feita pelo teclado.

### ⌨️ Navegação Geral

- **↑ / ↓** — Navegar entre opções e listas
- **Enter** — Confirmar seleção
- **Tab** — Alternar foco entre campos de formulário
- **Esc** — Voltar à tela anterior ou ao menu principal

---

## 🧭 Menus Principais

### 📁 Menu Inicial

Neste menu é possível realizar cadastros básicos e consultas gerais do sistema:

- **Cadastros**
  - Alunos
  - Professores
  - Disciplinas
  - Turmas
- **Listagens**
  - Listar alunos cadastrados
  - Listar disciplinas
  - Listar professores
- **Agenda de Turmas**
  - Visualização da grade horária semanal das turmas
  - Exibida em formato de agenda
  - Suporta rolagem vertical

---

### 📝 Menu Matrícula

Destinado ao período regular de matrículas:

- **Solicitar matrícula em turma**
  - O aluno pode solicitar vaga em uma ou mais turmas
- **Consultar solicitações de matrícula**
  - Visualização das solicitações pendentes

As solicitações ficam registradas até o processamento.

---

### 🔄 Menu Rematrícula

Utilizado para processar e acompanhar o resultado das matrículas:

- **Solicitar matrícula em turma**
- **Consultar solicitações**
- **Visualizar resultado do processamento de matrículas**
  - O sistema processa automaticamente as solicitações
  - As vagas são distribuídas respeitando:
    - capacidade da turma
    - CRA do aluno (critério de desempate)

---

### 📊 Menu Notas

Voltado ao acompanhamento acadêmico:

- **Visualizar resultado do processamento de matrículas**
  - O sistema processa automaticamente as solicitações
  - As vagas são distribuídas respeitando:
    - capacidade da turma
    - CRA do aluno (critério de desempate)
- **Lançar notas**
  - Inserção de notas por disciplina
- **Consultar notas**
  - Visualização das notas no semestre do aluno

---

## 🏗️ Arquitetura

### Modelos de Dados

#### 🧑 Aluno
```haskell
data Aluno = Aluno {
    _matricula             :: Matricula,
    _nome                  :: Nome,
    _curso                 :: Curso,
    _cra                   :: CRA,
    _notas                 :: Map.Map Int NotasDisciplina,
    _historico             :: [RegistroHistorico],        
    _periodoAtual          :: Int      
  }
```

#### 👨‍🏫 Professor
```haskell
data Professor = Professor
  { _matricula    :: Matricula
  , _nome         :: Nome      
  , _departamento :: String    
  , _formacao     :: String    
  }
```

#### 📖 Disciplina
```haskell
data Disciplina = Disciplina
  { codigo          :: Codigo
  , _nome           :: Nome
  , _cursos         :: [Curso]
  , _requisitos     :: [Codigo]
  , _preRequisitos  :: [Codigo]
  , _periodo        :: Int
  }
```

#### 🏫 Turma
```haskell
data Turma = Turma
  { codigoTurma         :: Int
  , _matriculaProfessor :: Matricula
  , _disciplina         :: Codigo
  , horario             :: [Horario]
  , _alunos             :: [Aluno]
  , _qtdMaxAlunos       :: Int
  , sala                :: String
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

- `Matricula`: Identificador numérico usado para alunos e professores
- `Codigo`: Código identificador de disciplinas (ex: "COMP01")
- `Nome`: Representação textual de nomes próprios
- `Curso`: Nome do curso de graduação
- `CRA`: Coeficiente de Rendimento Acadêmico (0.0 – 10.0)
- `Horario`: Codificação textual de horários acadêmicos (ex: "24M12")

### Validações Implementadas

✔️ Validação de pré-requisitos  
✔️ Detecção de conflitos de horário  
✔️ Verificação de cursos permitidos  
✔️ Controle de capacidade de turmas  
✔️ Prevenção de matrículas duplicadas  
✔️ Validação de fases do sistema  

## 🔧 Dependências Principais

- `base` — Biblioteca padrão da linguagem
- `containers` — Estruturas de dados funcionais (`Map`, `Set`)
- `text` — Manipulação eficiente de texto
- `directory` — Acesso ao sistema de arquivos
- `bytestring` — Manipulação de dados binários
- `aeson` / `aeson-pretty` — Serialização JSON
- `brick` — Interface de usuário em terminal (TUI)
- `vty` — Backend de terminal usado pelo Brick
- `mtl` — Monads e transformers
- `microlens` — Lenses para manipulação de estado

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
