list.files()
getwd ()
source("Modifica_dados.R")

#   As variáveis numéricas relevantes identificadas na análise exploratória são:
#     - total_toneladas : peso total da carga movimentada (variável principal)
#     - total_teu       : volume em contêineres de 20 pés
#     - total_unid      : quantidade de unidades físicas
#   IMPORTANTE (conforme ETL descrito na análise):
#     - Os zeros em total_teu e total_unid foram convertidos para NA,
#       pois representam inaplicabilidade de métrica (não valor nulo real).
#     - A correlação de Spearman entre essas 3 variáveis é próxima de 1,
#       confirmando alta redundância — a regressão ajuda a quantificar
#       essa relação numericamente.
#     - total_toneladastem distribuição assimétrica à direita (mediana ~200t),
#       com outliers de alto volume (embarques de commodities).
# Modela a relação: total_toneladas ~ total_teu 
#   - São as duas métricas quantitativas principais do dataset.
#   - A análise de Spearman mostrou correlação de 0.92 entre elas.
#   - Ambas medem "volume movimentado", mas em unidades diferentes.
#   - Total_toneladas ~ total_teu → "Dado o volume em contêineres, quanto de peso isso representa?" — faz sentido porque TEU é uma unidade padronizada e bem definida, e toneladas é a grandeza física resultante.
regressao_toneladas_teu <- function(dados) {
  
  # ── 2.1 Selecionar apenas linhas com ambas as variáveis preenchidas ───────
  # Como total_teu tem muitos NAs (operações não containerizadas),
  # usamos na.omit para trabalhar só com registros válidos para ambas.
  dados_modelo <- dados[, c("total_toneladas", "total_teu")]
  dados_modelo <- na.omit(dados_modelo)  # remove linhas com NA em qualquer coluna
  
  # ── 2.2 Ajuste do modelo linear com lm() ─────────────────────────────────
  # Sintaxe: lm(Y ~ X, data = ...)
  # Y = total_toneladas (variável resposta — o que queremos prever)
  # X = total_teu       (variável preditora — o que usamos para prever)
  # O modelo estima: total_toneladas = α + β * total_teu + ε
  #   α (intercepto): valor esperado de total_toneladas quando teu = 0
  #   β (inclinação) : quanto total_toneladas aumenta por unidade de teu
  modelo <- lm(
    formula   = total_toneladas ~ total_teu,
    data      = dados_modelo,
    na.action = na.omit
  )
  
  # ── 2.3 Exibição dos coeficientes estimados ───────────────────────────────
  coef_modelo <- coef(modelo)
  alpha <- coef_modelo["(Intercept)"]
  beta  <- coef_modelo["total_teu"]
  
  cat("Coeficientes estimados (sem correção)\n")
  cat(sprintf("  α (intercepto) = %.4f\n", alpha))
  cat(sprintf("  β (inclinação) = %.4f\n\n", beta))
  cat(sprintf("  Equação: total_toneladas = %.4f + %.4f * total_teu\n\n",
              alpha, beta))
  
  # ── 2.4 Resumo estatístico completo do modelo ─────────────────────────────
  cat("Resumo estatístico (sem correção)\n")
  print(summary(modelo))
  
  # ── 2.5 Verificação de heterocedasticidade (Teste de Breusch-Pagan) ───────
  # A análise mostrou distribuição assimétrica à direita em total_toneladas,
  # o que sugere possível heterocedasticidade (variância não constante dos erros).
  # Se o p-valor for < 0.05, há evidências de heterocedasticidade.
  cat("Teste de Breusch-Pagan (heterocedasticidade)\n")
  library(lmtest)
  bp <- bptest(modelo)
  print(bp)
  
  if (bp$p.value < 0.05) {
    cat("p-valor < 0.05: há heterocedasticidade\n")
    cat("Aplicando correção com weights\n\n")
    
    # ── 2.5.1 Estimação da variância dos erros ──────────────────────────────
    # Seguindo a aula 7: ajustamos um modelo linear sobre o log dos resíduos
    # quadráticos para estimar como a variância cresce com total_teu.
    # Usamos log para estabilizar a escala, já que os resíduos quadráticos
    # têm distribuição muito assimétrica (outliers de alto volume).
    res <- residuals(modelo)
    modelo_var <- lm(log(res^2) ~ total_teu, data = dados_modelo)
    
    # ── 2.5.2 Cálculo dos pesos ─────────────────────────────────────────────
    # A variância estimada é obtida revertendo o log com exp().
    # Os pesos são o inverso da variância: observações com maior variância
    # recebem menos peso, e observações mais estáveis recebem mais peso.
    var_estimada <- exp(fitted(modelo_var))
    weights      <- 1 / var_estimada
    
    # ── 2.5.3 Modelo corrigido com weights ──────────────────────────────────
    # Agora o lm() pondera cada observação pelo inverso da sua variância,
    # corrigindo o efeito da heterocedasticidade no ajuste da reta.
    modelo_corrigido <- lm(
      formula = total_toneladas ~ total_teu,
      data    = dados_modelo,
      weights = weights
    )
    
    # ── 2.5.4 Exibição dos coeficientes corrigidos ──────────────────────────
    coef_corrigido <- coef(modelo_corrigido)
    alpha_c <- coef_corrigido["(Intercept)"]
    beta_c  <- coef_corrigido["total_teu"]
    
    cat("Coeficientes estimados (com correção)\n")
    cat(sprintf("  α (intercepto) = %.4f\n", alpha_c))
    cat(sprintf("  β (inclinação) = %.4f\n\n", beta_c))
    cat(sprintf("  Equação corrigida: total_toneladas = %.4f + %.4f * total_teu\n\n",
                alpha_c, beta_c))
    
    cat("Resumo estatístico (com correção)\n")
    print(summary(modelo_corrigido))
    
    # ── 2.6 Gráficos diagnósticos comparando os dois modelos ────────────────
    par(mfrow = c(2, 2))
    
    # Gráfico 1: Dispersão com reta SEM correção
    plot(
      x    = dados_modelo$total_teu,
      y    = dados_modelo$total_toneladas,
      main = "Sem correção",
      xlab = "total_teu",
      ylab = "total_toneladas",
      pch  = 15, col = "steelblue", cex = 0.2
    )
    abline(modelo, col = "red", lwd = 2)
    
    # Gráfico 2: Dispersão com reta COM correção (weights)
    plot(
      x    = dados_modelo$total_teu,
      y    = dados_modelo$total_toneladas,
      main = "weights (corrigido)",
      xlab = "total_teu",
      ylab = "total_toneladas",
      pch  = 15, col = "steelblue", cex = 0.2
    )
    abline(modelo_corrigido, col = "blue", lwd = 2)
    
    # Gráfico 3: Resíduos do modelo SEM correção
    # Se houver padrão de "cone" (funil), confirma heterocedasticidade.
    plot(
      x    = fitted(modelo),
      y    = residuals(modelo),
      main = "Resíduos — Sem correção",
      xlab = "Valores ajustados (Ŷ)",
      ylab = "Resíduos",
      pch  = 15, col = "gray40", cex = 0.2
    )
    abline(h = 0, col = "red", lty = 2)
    
    # Gráfico 4: Resíduos do modelo COM correção
    # Esperamos que o padrão de cone tenha diminuído após os weights.
    plot(
      x    = fitted(modelo_corrigido),
      y    = residuals(modelo_corrigido),
      main = "Resíduos — weights (corrigido)",
      xlab = "Valores ajustados (Ŷ)",
      ylab = "Resíduos",
      pch  = 15, col = "gray40", cex = 0.2
    )
    abline(h = 0, col = "blue", lty = 2)
    
    par(mfrow = c(1, 1))
    
    # Retorna o modelo corrigido, que é o mais adequado
    return(modelo_corrigido)
    
  } else {
    cat("Sem evidências de heterocedasticidade.\n\n")
    
    # ── 2.6 Gráficos diagnósticos (modelo sem correção necessária) ───────────
    par(mfrow = c(1, 2))
    
    plot(
      x    = dados_modelo$total_teu,
      y    = dados_modelo$total_toneladas,
      main = "Dispersão: total_teu vs total_toneladas",
      xlab = "total_teu (contêineres de 20 pés)",
      ylab = "total_toneladas (peso da carga)",
      pch  = 15, col = "steelblue", cex = 0.3
    )
    abline(modelo, col = "red", lwd = 2)
    legend("topleft",
           legend = c("Observações", "Reta ajustada"),
           col    = c("steelblue", "red"),
           pch    = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))
    
    plot(
      x    = fitted(modelo),
      y    = residuals(modelo),
      main = "Resíduos vs. Valores Ajustados",
      xlab = "Valores ajustados (Ŷ)",
      ylab = "Resíduos",
      pch  = 19, col = "gray40", cex = 0.5
    )
    abline(h = 0, col = "red", lty = 2)
    
    par(mfrow = c(1, 1))
    
    return(modelo)
  }
}
modelo <- regressao_toneladas_teu(dados)