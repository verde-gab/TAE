getwd()
source("Modifica_dados.R")
# Busca especificamente pelo nome Relatorio_dados em qualquer subpasta.
#source("Relatorio.R")
str(dados)
# FUNÇÃO: graficos()
# Objetivo: gerar visualizações gráficas das principais variáveis do dataset.
# Recebe o dataframe completo como parâmetro.
graficos <- function(dados) {
  
  # ── 1. COLUNAS NUMÉRICAS — BOXPLOT ───────────────────────────
  # Percorre cada coluna do dataframe pelo nome.
  for (col in names(dados)) {
    # Armazena o vetor de valores da coluna atual.
    x <- dados[[col]]
    # is.numeric() E is.integer() — garante que integer também seja plotado.
    # No R, integer e numeric são tipos distintos mas ambos são quantitativos.
    if (is.numeric(x) || is.integer(x)) {
      cat(rep("=", 40), "\n", sep = "")
      cat("BOXPLOT:", col, "\n")
      cat(rep("-", 40), "\n", sep = "")
      par(mfrow = c(1, 1),
          mar   = c(3, 3, 2, 1),
          oma   = c(0, 0, 0, 0))
      boxplot(x,
              outline = FALSE,
              main    = paste("Distribuicao de:", col),
              ylab    = col,
              col     = "lightblue",
              border  = "darkblue",
              notch   = FALSE)
    }
  }
  
  # ── 2. COLUNAS FACTOR — BARPLOT ──────────────────────────────
  # Percorre exatamente as colunas identificadas como factor.
  # Usa sapply para garantir que todas as 11 colunas sejam processadas.
  cols_factor <- names(dados)[sapply(dados, is.factor)]
  for (col in cols_factor) {
    # Armazena o vetor de valores da coluna atual.
    x <- dados[[col]]
    # Separador visual com o nome da coluna atual.
    cat(rep("=", 40), "\n", sep = "")
    cat("BARPLOT:", col, "\n")
    cat(rep("-", 40), "\n", sep = "")
    # as.character() garante que levels numéricos como ano e mes
    # sejam tratados corretamente pelo table().
    freq     <- sort(table(as.character(x)), decreasing = TRUE)
    freq_top <- head(freq, 10)
    
    # ── GRÁFICO 1: TOP 10 CATEGORIAS ─────────────────────────
    par(mfrow = c(1, 1),
        mar   = c(12, 4, 4, 2),
        oma   = c(0, 0, 0, 0))
    barplot(freq_top,
            main      = paste("Top 10:", col),
            ylab      = "Frequencia",
            col       = "lightblue",
            border    = "darkblue",
            las       = 2,
            cex.names = 0.8)
    
    # ── GRÁFICO 2: TODOS OS LEVELS ────────────────────────────
    par(mfrow = c(1, 1),
        mar   = c(12, 4, 4, 2),
        oma   = c(0, 0, 0, 0))
    barplot(freq,
            main      = paste("Todos os levels:", col),
            ylab      = "Frequencia",
            col       = "lightblue",
            border    = "darkblue",
            las       = 2,
            cex.names = 0.4)
    
    # Restaura margens padrão após cada coluna.
    par(mfrow = c(1, 1),
        mar   = c(5, 4, 4, 2),
        oma   = c(0, 0, 0, 0))
  }
  # ── 3. COLUNA DATE — SÉRIE TEMPORAL ──────────────────────────
  # Percorre cada coluna do dataframe pelo nome.
  for (col in names(dados)) {
    # Armazena o vetor de valores da coluna atual.
    x <- dados[[col]]
    # Só gera série temporal para colunas do tipo Date.
    if (inherits(x, "Date")) {
      # Separador visual com o nome da coluna atual.
      cat(rep("=", 40), "\n", sep = "")
      cat("SERIE TEMPORAL:", col, "\n")
      cat(rep("-", 40), "\n", sep = "")
      # Reseta margens antes do plot da série temporal.
      par(mfrow = c(1, 1),
          mar   = c(5, 4, 4, 2),
          oma   = c(0, 0, 0, 0))
      # table() conta quantos registros existem por data.
      freq  <- table(x)
      # as.Date() converte os nomes do table (strings) de volta para Date.
      # Necessário para o eixo X do plot reconhecer as datas corretamente.
      datas <- as.Date(names(freq))
      # plot() com type = "l" gera gráfico de linha (série temporal).
      plot(datas, as.numeric(freq),
           type  = "l",
           main  = paste("Serie temporal de:", col),
           xlab  = "Data",
           ylab  = "Frequencia",
           col   = "darkblue",
           lwd   = 2)
      # Adiciona linha de tendência suavizada via lowess().
      # lowess() calcula uma curva de regressão local suavizada.
      # col = "red" → linha de tendência em vermelho para destaque.
      # lwd = 2     → espessura da linha.
      lines(lowess(datas, as.numeric(freq)),
            col = "red",
            lwd = 2)
    }
  }
  return(dados)
}
 #EXECUÇÃO NORMAL
graficos <- graficos(dados)

# ── Execução para gerar PDF ───────────────────────────────────────────────────────
# Abre PDF com tamanho grande para comportar todos os gráficos.
# width e height em polegadas — 16x12 evita erro de margem.
pdf("Graficos_Porto_Santos.pdf", width = 16, height = 12)
graficos(dados)
dev.off()
