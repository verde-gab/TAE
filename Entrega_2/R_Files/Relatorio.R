list.files()
getwd ()
# carrega funções do outro script
source("Modifica_dados.R")
str(dados)
# FUNÇÃO: report_data()
# Objetivo: gerar um mini-relatório do dataset com estrutura,
# primeiros/últimos valores, resumo estatístico de colunas numéricas
# e frequência de categorias de colunas factor.
report_data <- function(dados) {
  
  # ── 1. DATAFRAMES DE RESULTADO ─────────────────────────────────
  # Cria dataframe vazio para armazenar o resumo estatístico
  # das colunas numéricas. Cada linha será uma coluna do dataset.
  resultado_numero <- data.frame(
    coluna       = character(),
    min          = numeric(),
    q1           = numeric(),
    mediana      = numeric(),
    media        = numeric(),
    q3           = numeric(),
    max          = numeric(),
    variancia    = numeric(),
    desvio_padrao = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Cria dataframe vazio para armazenar a frequência das categorias
  # das colunas factor. Cada linha será uma categoria de uma coluna.
  resultado_factor <- data.frame(
    coluna     = character(),
    categoria  = character(),
    frequencia = numeric(),
    stringsAsFactors = FALSE
  )
  
  # ── 2. ESTRUTURA GERAL ─────────────────────────────────────────
  # Exibe número de linhas e colunas do dataset.
  cat("ESTRUTURA DO DATASET\n")
  cat("Linhas:", nrow(dados), " | Colunas:", ncol(dados), "\n")
  
  # ── 3. PRIMEIROS E ÚLTIMOS VALORES POR COLUNA ──────────────────
  cat("\nPRIMEIRAS/ULTIMAS LINHAS E TIPO DE VARIAVEL\n")
  # Percorre cada coluna do dataframe pelo nome.
  for (col in names(dados)) {
    cat("COLUNA:", col, "\n")
    # class() retorna o tipo da coluna (numeric, factor, Date, etc.).
    cat("CLASSE:", class(dados[[col]]), "\n")
    # head() retorna os primeiros 10 valores da coluna.
    cat("PRIMEIROS 10:\n")
    # capture.output + writeLines garante impressão limpa no console.
    writeLines(capture.output(print(head(dados[[col]], 10))))
    # tail() retorna os últimos 10 valores da coluna.
    cat("ULTIMOS 10:\n")
    writeLines(capture.output(print(tail(dados[[col]], 10))))
    # Separador visual entre colunas.
    cat(rep("-", 30), "\n", sep = "")
  }
  
  # ── 4. RESUMO ESTATÍSTICO — COLUNAS NUMÉRICAS ─────────────────
  cat("\nRESUMO ESTATISTICO\n")
  # Percorre cada coluna do dataframe pelo nome.
  for (col in names(dados)) {
    # Armazena o vetor de valores da coluna atual.
    x <- dados[[col]]
    # Só calcula estatísticas para colunas numéricas.
    if (is.numeric(x)) {
      # summary() retorna min, Q1, mediana, média, Q3 e max.
      # na.rm = TRUE ignora valores ausentes (NA) no cálculo.
      cal       <- summary(x, na.rm = TRUE)
      variancia <- var(x, na.rm = TRUE)
      desvio    <- sd(x,  na.rm = TRUE)
      # rbind() adiciona uma nova linha ao dataframe de resultados.
      # Cada campo é extraído pelo nome do vetor retornado por summary().
      resultado_numero <- rbind(
        resultado_numero,
        data.frame(
          coluna        = col,
          min           = as.numeric(cal["Min."]),
          q1            = as.numeric(cal["1st Qu."]),
          mediana       = as.numeric(cal["Median"]),
          media         = as.numeric(cal["Mean"]),
          q3            = as.numeric(cal["3rd Qu."]),
          max           = as.numeric(cal["Max."]),
          variancia     = variancia,
          desvio_padrao = desvio,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  # Exibe o dataframe com o resumo estatístico das colunas numéricas.
  print(resultado_numero)
  cat(rep("=", 40), "\n", sep = "")
  
  # ── 5. FREQUÊNCIA DE CATEGORIAS — COLUNAS FACTOR ──────────────
  cat("\nFREQUENCIA DE CATEGORIAS (FACTOR)\n")
  # Percorre cada coluna do dataframe pelo nome.
  for (col in names(dados)) {
    # Armazena o vetor de valores da coluna atual.
    x <- dados[[col]]
    # Só calcula frequência para colunas do tipo factor.
    if (is.factor(x)) {
      # Separador visual com o nome da coluna atual.
      cat(rep("=", 40), "\n", sep = "")
      cat("COLUNA:", col, "\n")
      cat(rep("-", 40), "\n", sep = "")
      # summary() já retorna ordenado por frequência — pegamos só os 10 primeiros.
      # head() limita a exibição e o armazenamento a no máximo 10 categorias.
      freq <- head(summary(x), 10)
      
      # Monta dataframe temporário com no máximo 10 categorias.
      temp <- data.frame(
        categoria  = names(freq),
        frequencia = as.numeric(freq),
        stringsAsFactors = FALSE
      )
      print(temp)
      
      # Acumula no resultado final também com no máximo 10 categorias.
      resultado_factor <- rbind(
        resultado_factor,
        data.frame(
          coluna     = col,
          categoria  = names(freq),
          frequencia = as.numeric(freq),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  cat(rep("=", 40), "\n", sep = "")
  
  # ── 6. RETORNO ─────────────────────────────────────────────────
  # Retorna uma lista com os dois dataframes de resultado,
  # permitindo que o usuário acesse cada um separadamente após a chamada.
  # Exemplo: rel <- report_data(dados); rel$numerico; rel$factor
  return(list(
    numerico = resultado_numero,
    factor   = resultado_factor
  ))

}

#EXECUÇÃO NORMAL
relatorio <- report_data(dados)

# ── Execução ───────────────────────────────────────────────────────
# Chama a função e armazena os resultados em 'relatorio'.
relatorio <- report_data(dados)
str(dados)
