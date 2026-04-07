getwd()
source("Modifica_dados.R")
str(dados)
#source("Relatorio.R")
#source("Graficos_dados.R")

# FUNÇÃO: correlacionar()
# Objetivo: calcular e visualizar a associação entre todas as combinações
# de colunas do dataset, seguindo os três casos da aula:
#   1. Factor  × Factor     → Qui-quadrado + V de Cramér
#   2. Numeric × Numeric    → Spearman (dados não normais/Pareto)
#   3. Factor  × Numeric    → Kruskal-Wallis + R²
# A coluna Date (ano_mes) é convertida para numeric (dias desde origem)
# para participar das correlações numéricas.
# Recebe o dataframe completo e retorna uma lista com os três resultados.
correlacionar <- function(dados) {
  
  # ── PREPARAÇÃO ───────────────────────────────────────────────
  # Cria dataframes vazios para acumular os resultados de cada par.
  resultado_factor  <- data.frame(
    col_a      = character(),  # nome da primeira coluna
    col_b      = character(),  # nome da segunda coluna
    chi2       = numeric(),    # estatística qui-quadrado
    p_valor    = numeric(),    # p-valor do teste
    v_cramer   = numeric(),    # força da associação (0 a 1)
    conclusao  = character(),  # interpretação automática
    stringsAsFactors = FALSE
  )
  resultado_numeric <- data.frame(
    col_a      = character(),
    col_b      = character(),
    spearman   = numeric(),    # coeficiente de correlação Spearman
    p_valor    = numeric(),
    conclusao  = character(),
    stringsAsFactors = FALSE
  )
  resultado_misto   <- data.frame(
    col_factor  = character(), # nome da coluna factor
    col_numeric = character(), # nome da coluna numérica
    kruskal_h   = numeric(),   # estatística H do Kruskal-Wallis
    p_valor     = numeric(),
    r2          = numeric(),   # proporção da variância explicada pelo factor
    conclusao   = character(),
    stringsAsFactors = FALSE
  )
  
  # Converte colunas Date para numeric (dias desde 1970-01-01).
  # Isso permite que ano_mes participe das correlações numéricas,
  # revelando tendências temporais com as demais variáveis quantitativas.
  dados_num <- dados
  for (col in names(dados_num)) {
    if (inherits(dados_num[[col]], "Date")) {
      # as.numeric em Date retorna o número de dias desde a origem do R.
      dados_num[[col]] <- as.numeric(dados_num[[col]])
    }
  }
  
  # Separa os nomes das colunas por tipo para montar os pares.
  cols_factor  <- names(dados_num)[sapply(dados_num, is.factor)]
  cols_numeric <- names(dados_num)[sapply(dados_num, is.numeric)]
  
  # Remove colunas que não fazem sentido analítico nas correlações.
  # numero_viagem → identificador sequencial, não é métrica real.
  # metrica       → redundante com total_toneladas (correlação = 1.0).
  # ano           → redundante com ano_mes (ano foi extraído de ano_mes).
  cols_numeric <- cols_numeric[!cols_numeric %in% c("numero_viagem", "metrica")]
  cols_factor  <- cols_factor[!cols_factor   %in% c("ano")]
  
  # ── CASO 1: FACTOR × FACTOR ──────────────────────────────────
  cat(rep("=", 50), "\n", sep = "")
  cat("ASSOCIACAO FACTOR x FACTOR (Qui-quadrado + V de Cramer)\n")
  cat(rep("=", 50), "\n", sep = "")
  
  # combn() gera todas as combinações únicas de 2 colunas factor.
  # Evita repetir o par (A,B) e (B,A) — cada par é testado uma vez.
  if (length(cols_factor) >= 2) {
    pares_factor <- combn(cols_factor, 2, simplify = FALSE)
    for (par in pares_factor) {
      col_a <- par[1]
      col_b <- par[2]
      # Monta a tabela de contingência entre as duas colunas factor.
      tabela <- table(dados_num[[col_a]], dados_num[[col_b]])
      
      # Verifica se a tabela tem pelo menos 2 linhas e 2 colunas.
      # Tabelas menores que 2x2 não permitem calcular o V de Cramér.
      if (nrow(tabela) < 2 || ncol(tabela) < 2) {
        cat("PAR:", col_a, "x", col_b, "\n")
        cat("  Ignorado — tabela menor que 2x2\n")
        cat(rep("-", 50), "\n", sep = "")
        next
      }
      
      # chisq.test() testa se as duas variáveis são independentes.
      # suppressWarnings evita alertas quando células têm freq < 5.
      teste <- suppressWarnings(chisq.test(tabela))
      # V de Cramér — mede a força da associação entre 0 e 1.
      # Fórmula: sqrt(chi2 / (n * (min(linhas,colunas) - 1)))
      n        <- sum(tabela)
      chi2     <- as.numeric(teste$statistic)
      k        <- min(nrow(tabela), ncol(tabela)) - 1
      v_cramer <- sqrt(chi2 / (n * k))
      p_valor  <- teste$p.value
      
      # Interpreta automaticamente o resultado.
      # p < 0.05 → rejeita independência → há associação.
      if (p_valor < 0.05) {
        forca     <- ifelse(v_cramer >= 0.3, "forte", "fraca")
        conclusao <- paste("Associacao", forca, "- rejeita H0")
      } else {
        conclusao <- "Sem associacao - nao rejeita H0"
      }
      cat("PAR:", col_a, "x", col_b, "\n")
      cat("  Chi2:", round(chi2, 4), "| p-valor:", round(p_valor, 4),
          "| V de Cramer:", round(v_cramer, 4), "\n")
      cat("  Conclusao:", conclusao, "\n")
      cat(rep("-", 50), "\n", sep = "")
      
      # Acumula o resultado no dataframe.
      resultado_factor <- rbind(resultado_factor, data.frame(
        col_a     = col_a,
        col_b     = col_b,
        chi2      = chi2,
        p_valor   = p_valor,
        v_cramer  = round(v_cramer, 4),
        conclusao = conclusao,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # ── CASO 2: NUMERIC × NUMERIC ────────────────────────────────
  cat(rep("=", 50), "\n", sep = "")
  cat("ASSOCIACAO NUMERIC x NUMERIC (Spearman)\n")
  cat(rep("=", 50), "\n", sep = "")
  
  # Spearman é usado no lugar de Pearson pois os dados seguem
  # distribuição assimétrica (Pareto/log-normal), não normal.
  # Spearman trabalha com rankings e não exige normalidade.
  if (length(cols_numeric) >= 2) {
    pares_numeric <- combn(cols_numeric, 2, simplify = FALSE)
    for (par in pares_numeric) {
      col_a <- par[1]
      col_b <- par[2]
      x <- dados_num[[col_a]]
      y <- dados_num[[col_b]]
      # Remove NAs para o cálculo — pares com NA em qualquer coluna
      # são excluídos da correlação (use = "complete.obs").
      teste <- suppressWarnings(
        cor.test(x, y, method = "spearman", use = "complete.obs")
      )
      spearman <- as.numeric(teste$estimate)
      p_valor  <- teste$p.value
      # Interpreta o coeficiente: >0.3 fraca, >0.6 moderada, >0.8 forte.
      if (p_valor < 0.05) {
        forca <- ifelse(abs(spearman) >= 0.6, "moderada/forte",
                        ifelse(abs(spearman) >= 0.3, "fraca", "muito fraca"))
        direcao  <- ifelse(spearman > 0, "positiva", "negativa")
        conclusao <- paste("Correlacao", forca, direcao)
      } else {
        conclusao <- "Sem correlacao - nao rejeita H0"
      }
      cat("PAR:", col_a, "x", col_b, "\n")
      cat("  Spearman:", round(spearman, 4), "| p-valor:", round(p_valor, 4), "\n")
      cat("  Conclusao:", conclusao, "\n")
      cat(rep("-", 50), "\n", sep = "")
      resultado_numeric <- rbind(resultado_numeric, data.frame(
        col_a = col_a, col_b = col_b,
        spearman = round(spearman, 4),
        p_valor  = round(p_valor, 4),
        conclusao = conclusao,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # ── CASO 3: FACTOR × NUMERIC ─────────────────────────────────
  cat(rep("=", 50), "\n", sep = "")
  cat("ASSOCIACAO FACTOR x NUMERIC (Kruskal-Wallis + R2)\n")
  cat(rep("=", 50), "\n", sep = "")
  
  for (col_f in cols_factor) {
    for (col_n in cols_numeric) {
      x <- dados_num[[col_f]]  # factor
      y <- dados_num[[col_n]]  # numérico
      
      # Remove NAs antes do teste.
      validos <- !is.na(x) & !is.na(y)
      x <- x[validos]
      y <- y[validos]
      
      # Verifica se há pelo menos 2 grupos com observações distintas.
      # Kruskal-Wallis exige variação entre grupos — se todos os valores
      # estiverem no mesmo grupo ou só existir 1 grupo, o teste quebra.
      niveis_presentes <- unique(x)
      grupos_validos   <- tapply(y, x, function(g) length(unique(g)) > 1)
      n_grupos_validos <- sum(grupos_validos, na.rm = TRUE)
      
      if (length(niveis_presentes) < 2 || n_grupos_validos < 2) {
        cat("PAR:", col_f, "x", col_n, "\n")
        cat("  Ignorado — observacoes no mesmo grupo ou sem variancia\n")
        cat(rep("-", 50), "\n", sep = "")
        next
      }
      
      # Roda o Kruskal-Wallis normalmente após a validação.
      teste     <- suppressWarnings(kruskal.test(y ~ x))
      p_valor   <- teste$p.value
      kruskal_h <- as.numeric(teste$statistic)
      
      # R² — proporção da variância total explicada pelo factor.
      # Calcula a variância dentro de cada categoria (média ponderada)
      # e compara com a variância total, conforme slide 18 da aula.
      var_total  <- var(y, na.rm = TRUE)
      # tapply aplica var() dentro de cada nível do factor.
      vars_grupo <- tapply(y, x, var, na.rm = TRUE)
      ns_grupo   <- tapply(y, x, length)
      # Média ponderada das variâncias por grupo (fórmula da aula).
      var_media  <- sum(ns_grupo * vars_grupo, na.rm = TRUE) /
        sum(ns_grupo)
      # R² = 1 - var_media / var_total (ganho relativo na variância).
      r2 <- 1 - (var_media / var_total)
      
      if (p_valor < 0.05) {
        forca     <- ifelse(r2 >= 0.3, "forte", "fraca")
        conclusao <- paste("Efeito", forca, "- rejeita H0 | R2:",
                           round(r2, 4))
      } else {
        conclusao <- "Sem efeito - nao rejeita H0"
      }
      cat("PAR:", col_f, "x", col_n, "\n")
      cat("  Kruskal H:", round(kruskal_h, 4), "| p-valor:",
          round(p_valor, 4), "| R2:", round(r2, 4), "\n")
      cat("  Conclusao:", conclusao, "\n")
      cat(rep("-", 50), "\n", sep = "")
      
      # Acumula o resultado no dataframe.
      resultado_misto <- rbind(resultado_misto, data.frame(
        col_factor  = col_f,
        col_numeric = col_n,
        kruskal_h   = round(kruskal_h, 4),
        p_valor     = round(p_valor,   4),
        r2          = round(r2,        4),
        conclusao   = conclusao,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # ── GRÁFICOS ─────────────────────────────────────────────────
  # Gráfico 1 — Mapa de calor da correlação Spearman entre numéricas.
  if (length(cols_numeric) >= 2) {
    mat_cor <- cor(dados_num[, cols_numeric],
                   method = "spearman",
                   use    = "complete.obs")
    n <- ncol(mat_cor)
    # mar grande o suficiente para os labels nos dois eixos.
    par(mfrow = c(1, 1),
        mar   = c(12, 12, 4, 2),
        oma   = c(0, 0, 0, 0))
    # Força a diagonal para NA antes de plotar.
    # Assim a correlação de cada variável com ela mesma
    # fica branca — destacando apenas as correlações entre variáveis diferentes.
    diag(mat_cor) <- NA
    image(1:n, 1:n, t(mat_cor[n:1, ]),
          col   = colorRampPalette(c("darkblue", "white", "darkred"))(100),
          xaxt  = "n",
          yaxt  = "n",
          main  = "Mapa de Calor - Correlacao Spearman",
          xlab  = "",
          ylab  = "")
    # Restaura a diagonal para 1 antes de adicionar os textos.
    diag(mat_cor) <- 1
    # Eixo X — nomes das colunas na vertical.
    axis(1, at = 1:n, labels = colnames(mat_cor),
         las = 2, cex.axis = 0.9, tick = FALSE)
    # Eixo Y — nomes das linhas na horizontal.
    axis(2, at = 1:n, labels = rev(rownames(mat_cor)),
         las = 2, cex.axis = 0.9, tick = FALSE)
    # Adiciona o valor numérico dentro de cada célula.
    for (i in 1:n) {
      for (j in 1:n) {
        text(i, n - j + 1, round(mat_cor[j, i], 2),
             cex = 0.9, col = "black")
      }
    }
    # Restaura margens padrão.
    par(mfrow = c(1, 1),
        mar   = c(5, 4, 4, 2),
        oma   = c(0, 0, 0, 0))
  }
  
  # ── GRÁFICO 2: MAPA DE CALOR — V DE CRAMÉR (FACTOR × FACTOR) ─
  # Equivalente visual ao mapa Spearman, mas para variáveis categóricas.
  # V de Cramér varia entre 0 (sem associação) e 1 (associação perfeita).
  # Permite visualizar todas as associações entre factors de uma vez.
  if (length(cols_factor) >= 2) {
    # Cria matriz quadrada vazia com dimensões = número de factors.
    # dimnames define os nomes das linhas e colunas da matriz.
    n_f        <- length(cols_factor)
    mat_cramer <- matrix(NA, n_f, n_f,
                         dimnames = list(cols_factor, cols_factor))
    
    # Percorre todos os pares de factors para preencher a matriz.
    for (i in seq_along(cols_factor)) {
      for (j in seq_along(cols_factor)) {
        # Diagonal principal → correlação de uma variável com ela mesma = 1.
        if (i == j) {
          mat_cramer[i, j] <- 1
        } else {
          # Monta tabela de contingência entre os dois factors.
          tab  <- table(dados_num[[cols_factor[i]]],
                        dados_num[[cols_factor[j]]])
          # Verifica se a tabela tem pelo menos 2 linhas e 2 colunas.
          # Tabelas menores não permitem calcular o V de Cramér.
          if (nrow(tab) < 2 || ncol(tab) < 2) {
            mat_cramer[i, j] <- NA
            next
          }
          # Calcula o qui-quadrado e extrai a estatística.
          chi2 <- as.numeric(
            suppressWarnings(chisq.test(tab))$statistic
          )
          # n → total de observações da tabela.
          n    <- sum(tab)
          # k → menor dimensão menos 1 — normaliza o V de Cramér.
          k    <- min(nrow(tab), ncol(tab)) - 1
          # V de Cramér — fórmula: sqrt(chi2 / (n * k)).
          mat_cramer[i, j] <- sqrt(chi2 / (n * k))
        }
      }
    }
    
    # Plota o mapa de calor do V de Cramér.
    # Cores mais escuras (azul) → associação mais forte.
    # Cores mais claras (branco) → sem associação.
    par(mar = c(8, 8, 4, 2))
    image(1:n_f, 1:n_f, t(mat_cramer),
          col  = colorRampPalette(c("white", "darkblue"))(100),
          xaxt = "n", yaxt = "n",
          main = "Mapa de Calor - V de Cramer (Factor x Factor)",
          xlab = "", ylab = "")
    # Adiciona rótulos nos eixos com os nomes dos factors.
    # las = 2 → rótulos na vertical para não sobrepor.
    # cex.axis = 0.7 → reduz tamanho dos rótulos para caber.
    axis(1, at = 1:n_f, labels = cols_factor, las = 2, cex.axis = 0.7)
    axis(2, at = 1:n_f, labels = cols_factor, las = 2, cex.axis = 0.7)
    # Adiciona o valor numérico do V de Cramér dentro de cada célula.
    for (i in 1:n_f) {
      for (j in 1:n_f) {
        text(j, i, round(mat_cramer[i, j], 2), cex = 0.7)
      }
    }
    par(mar = c(5, 4, 4, 2))
  }
  
  # Gráfico 3 — Boxplot numérica por categoria (factor × numeric).
  # Reproduz o mesmo estilo da aula (slides 16 e 17).
  # Exibe os 3 pares com maior R² — os mais informativos.
  resultado_misto_ord <- resultado_misto[order(-resultado_misto$r2), ]
  top3 <- head(resultado_misto_ord, 3)
  for (i in seq_len(nrow(top3))) {
    col_f <- top3$col_factor[i]
    col_n <- top3$col_numeric[i]
    # Fórmula numérica ~ factor para o boxplot agrupado.
    boxplot(dados_num[[col_n]] ~ dados_num[[col_f]],
            col     = "lightblue",
            border  = "darkblue",
            main    = paste("Distribuicao de", col_n, "por", col_f),
            xlab    = col_f,
            ylab    = col_n,
            las     = 2,
            outline = FALSE)
  }
  
  # ── RETORNO ──────────────────────────────────────────────────
  # Retorna lista com os três dataframes de resultado.
  # Acesse com: cor$factor, cor$numeric, cor$misto
  return(list(
    factor  = resultado_factor,
    numeric = resultado_numeric,
    misto   = resultado_misto
  ))
}

# ── Execução ───────────────────────────────────────────────────────
# Armazena os resultados para análise posterior.
# cor$factor  → associações factor x factor
# cor$numeric → correlações numeric x numeric
# cor$misto   → efeito factor x numeric
resultados_correlacao <- correlacionar(dados)

#ARQUIVOS EM PDF DAS ANALISES 
# pdf() abre um dispositivo gráfico que salva tudo em PDF.
pdf("Correlacao.pdf")
#pdf("Correlacao.pdf", width = 24, height = 20)
# Chama a função — todas as análises e gráficos vão direto para o PDF.
resultados_correlacao <- correlacionar(dados)
# dev.off() fecha o dispositivo gráfico e finaliza o arquivo PDF.
dev.off()

pdf("Relatorio_Completo.pdf")
#pdf("Relatorio_Completo.pdf", width = 20, height = 16)
report_data(dados)
graficos(dados)
resultados_correlacao <- correlacionar(dados)
dev.off()

# pdf() para os gráficos + sink() para o texto — roda os dois juntos.
sink("Analise_Correlacao.txt")
resultados_correlacao <- correlacionar(dados)
dev.off()
sink()