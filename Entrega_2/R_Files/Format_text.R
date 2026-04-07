source("Modifica_dados.R") #carrega os dados e funções do arquivo
# ── Contadores globais ────────────────────────────────────────────
# contador de colunas convertidas para factor
total_asfactor <- 0
# contador de colunas convertidas para Date
total_asdate   <- 0

# FUNÇÃO 1: remover_acentos()
# Objetivo: converter caracteres acentuados (ã, é, ç...) em
# equivalentes sem acento (a, e, c...) usando re-codificação.
# Age sobre character e factor — numeric, Date etc. são ignorados.
remover_acentos <- function(x) {
  # Verifica se x é character ou factor.
  # Ambos os tipos são tratados — numeric, Date etc. são
  # retornados sem alteração.
  if (is.character(x) || is.factor(x)) {
    # as.character garante que iconv consiga processar o vetor,
    # independentemente de ter chegado como factor ou character.
    # iconv() re-codifica strings de um encoding para outro.
    # "UTF-8"           → encoding de entrada (padrão no R moderno)
    # "ASCII//TRANSLIT" → translitera: substitui ã→a, é→e, ç→c, etc.
    return(iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT"))
  }
  # Se x NÃO for character nem factor, devolve sem nenhuma modificação.
  return(x)
}

# FUNÇÃO 2: limpar_texto()
# Objetivo: pipeline completa de limpeza aplicada a UMA coluna.
# Recebe dois parâmetros:
#   x   → vetor de valores da coluna
#   col → nome da coluna (string), usado para tratar casos especiais
limpar_texto <- function(x, col) {
  
  # Verifica se o nome da coluna é ANO ou MES.
  # Essas colunas recebem tratamento especial: apenas limpeza de
  # espaços e conversão direta para factor (sem lowcase/underline).
  if (col %in% c("ANO", "MES")) {
    # trimws remove espaços em branco no início e no fim do texto.
    # as.character garante compatibilidade antes do trimws.
    x <- trimws(as.character(x))
    # Converte para factor — representa variável categórica.
    x <- as.factor(x)
    # <<- atualiza o contador no ambiente global (fora da função).
    total_asfactor <<- total_asfactor + 1
    # Retorna a coluna modificada.
    return(x)
  }
  
  # Verifica se o nome da coluna é ANO_MES.
  # Essa coluna é convertida para o tipo Date.
  if (col == "ANO_MES") {
    # as.Date transforma texto ou número em objeto Date.
    x <- as.Date(x)
    # <<- atualiza o contador no ambiente global.
    total_asdate <<- total_asdate + 1
    # Retorna a coluna convertida.
    return(x)
  }
  
  # Para todas as demais colunas do tipo character ou factor,
  # aplica o pipeline completo de limpeza de texto.
  # A verificação inclui character para tratar CSVs lidos com
  # stringsAsFactors = FALSE, que não geram factors automaticamente.
  if (is.character(x) || is.factor(x)) {
    # Passo 1 — Remove acentos chamando a função anterior.
    # as.character dentro de remover_acentos() garante compatibilidade.
    x <- remover_acentos(x)
    # Passo 2 — trimws() remove espaços em branco no início e
    # no fim de cada string. Ex: "  Sao Paulo  " → "Sao Paulo".
    x <- trimws(x)
    # Passo 3 — gsub() faz substituição global (todas as ocorrências).
    # Troca cada espaço interno " " por underline "_".
    # Ex: "Rio de Janeiro" → "Rio_de_Janeiro".
    x <- gsub(" ", "_", x)
    # Passo 4 — tolower() converte tudo para letras minúsculas.
    # Ex: "Rio_de_Janeiro" → "rio_de_janeiro".
    x <- tolower(x)
    # Passo 5 — Garante que o resultado final seja factor,
    # independentemente do tipo de entrada (factor ou character).
    x <- as.factor(x)
    # <<- atualiza o contador no ambiente global.
    total_asfactor <<- total_asfactor + 1
  }
  
  # Retorna x (limpo se era character/factor, intacto se não era).
  return(x)
}

# FUNÇÃO 3: limpar_colunas()
# Objetivo: aplicar limpeza de texto aos NOMES das colunas
# (não aos valores). Padroniza o cabeçalho do dataframe.
limpar_colunas <- function(dados) {
  # colnames() retorna os nomes das colunas como character.
  # sapply percorre cada nome e aplica o pipeline de limpeza diretamente
  # — sem passar por limpar_texto(), pois nomes são sempre character e
  # não precisam de conversão para factor no final.
  colnames(dados) <- sapply(colnames(dados), function(col) {
    # Passo 1 — Remove acentos do nome da coluna.
    col <- iconv(col, from = "UTF-8", to = "ASCII//TRANSLIT")
    # Passo 2 — Remove espaços nas bordas.
    col <- trimws(col)
    # Passo 3 — Substitui espaços internos por underline.
    col <- gsub(" ", "_", col)
    # Passo 4 — Converte para minúsculas.
    col <- tolower(col)
    # Retorna o nome limpo como character (colnames() exige character).
    return(col)
  })
  return(dados)
}

# FUNÇÃO 4: limpar_dados()
# Objetivo: aplicar limpar_texto() a CADA COLUNA DE VALORES
# do dataframe (não aos nomes, apenas ao conteúdo).
limpar_dados <- function(dados) {
  # dados[] mantém a estrutura de dataframe ao fazer a atribuição.
  # (Sem os colchetes, dados viraria uma lista simples.)
  # lapply aplica uma função em cada elemento de uma lista.
  # Aqui usamos names(dados) para percorrer os nomes das colunas —
  # cada iteração recebe o nome da coluna na variável "col".
  dados[] <- lapply(names(dados), function(col) {
    # dados[[col]] retorna o vetor de valores da coluna atual.
    # Passamos também o nome "col" para limpar_texto() tratar
    # os casos especiais (ANO, MES, ANO_MES) corretamente.
    limpar_texto(dados[[col]], col)
  })
  return(dados)
}

# FUNÇÃO 5: limpar_dataframe()
# Objetivo: orquestrar tudo — é a função principal que o
# usuário chama. Aplica limpeza de colunas e de dados.
limpar_dataframe <- function(dados) {
  # Primeiro limpa os nomes das colunas (cabeçalho).
  dados <- limpar_colunas(dados)
  # Depois limpa os valores dentro de cada coluna.
  dados <- limpar_dados(dados)
  return(dados)
}

# ── Execução ─────────────────────────────────────────────────────
# Lê o CSV do disco.
# stringsAsFactors = FALSE → por padrão o R moderno já não
# converte strings em factors; aqui é explícito por segurança.
# Atenção: limpar_texto() trata character e factor, então colunas
# de texto serão limpas independentemente do modo de leitura.
# dados <- read.csv("seu_arquivo.csv", stringsAsFactors = FALSE)

# Aplica o pipeline completo de limpeza ao dataframe.
dados <- limpar_dataframe(dados)

# Exibe as primeiras 6 linhas para conferência rápida.
head(dados)
# Mostra total de colunas convertidas para factor.
cat("Total convertido para factor:", total_asfactor, "\n")
# Mostra total de colunas convertidas para Date.
cat("Total convertido para date:  ", total_asdate,   "\n")
# Exibe a estrutura completa do dataframe (tipos de cada coluna).
str(dados)

