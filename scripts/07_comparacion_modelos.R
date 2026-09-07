# =============================================================================
# SCRIPT 07 - COMPARACIÓN LEE-CARTER vs CBD Y SELECCIÓN DEL MODELO
#
# Decide, con criterios explícitos, cuál de los dos modelos actúa como caso
# base y cuál como contraste.
#
# CRITERIOS QUE SE APLICAN (en este orden de importancia):
#   1. BIC  - penaliza fuerte la complejidad. Es el criterio principal en la
#             literatura actuarial de mortalidad.
#   2. AIC  - penaliza menos. Se reporta como complemento.
#   3. Capacidad predictiva fuera de muestra (backtesting).
#   4. Comportamiento de los residuos.
#   5. Plausibilidad biológica de las proyecciones.
#
# AVISO METODOLÓGICO IMPORTANTE:
#   AIC y BIC de LC y CBD NO son directamente comparables sin cuidado, porque
#   LC modeliza m(x,t) con enlace log sobre exposición central, y CBD modeliza
#   q(x,t) con enlace logit sobre exposición inicial. Son verosimilitudes
#   sobre parametrizaciones distintas del mismo fenómeno.
#   La literatura los compara igualmente (Cairns et al., 2009), pero la
#   salvedad se declara en la memoria (apartado 4.2). Por ello este guion
#   otorga más peso al backtesting, que sí es directamente comparable: ambos
#   modelos predicen defunciones que se contrastan con las observadas.
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(StMoMo)
library(ggplot2)
library(dplyr)

cat("=== SCRIPT 07: COMPARACIÓN DE MODELOS ===\n\n")

prep <- readRDS(file.path(config$ruta_limpios, "datos_preparados.rds"))
lc   <- readRDS(file.path(config$ruta_resultados, "modelo_lee_carter.rds"))
cbd  <- readRDS(file.path(config$ruta_resultados, "modelo_cbd.rds"))
diag <- readRDS(file.path(config$ruta_resultados, "diagnosticos_lc.rds"))

tema_tfm <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10,
                                     colour = "grey30"),
        plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

# Los datos de partida proceden de la Human Mortality Database. El generador
# de series de prueba se elimino del guion 01, de modo que la comprobacion
# siguiente no deberia activarse nunca; si lo hiciera, es preferible detener
# la ejecucion a incorporar al trabajo una figura con una advertencia impresa.
if (grepl("SINT", prep$tipo_datos)) {
  stop("Los datos cargados no son los definitivos. Ejecute el guion 01 con ",
       "los ficheros de la Human Mortality Database antes de continuar.")
}
nota_fuente <- "Fuente: Elaboraci\u00f3n propia a partir de la Human Mortality Database."


# -----------------------------------------------------------------------------
# PASO 7.1 - Tabla comparativa de criterios de información
# -----------------------------------------------------------------------------
tabla_comp <- data.frame(
  Modelo         = c("Lee-Carter", "CBD"),
  Parametros     = c(lc$fit$npar, cbd$fit$npar),
  LogVerosim     = round(c(lc$fit$loglik, cbd$fit$loglik), 1),
  Desviacion     = round(c(lc$fit$deviance, cbd$fit$deviance), 1),
  AIC            = round(c(lc$aic, cbd$aic), 1),
  BIC            = round(c(lc$bic, cbd$bic), 1),
  R2_log         = round(c(lc$r2, cbd$r2), 4),
  stringsAsFactors = FALSE
)

cat("PASO 7.1 - Criterios de información:\n\n")
print(tabla_comp, row.names = FALSE)

mejor_aic <- tabla_comp$Modelo[which.min(tabla_comp$AIC)]
mejor_bic <- tabla_comp$Modelo[which.min(tabla_comp$BIC)]

cat("\n  Mejor según AIC:", mejor_aic, "\n")
cat("  Mejor según BIC:", mejor_bic, "\n")

dif_bic <- abs(diff(tabla_comp$BIC))
cat("  Diferencia de BIC:", round(dif_bic, 1), "\n")
cat("  Regla de Kass y Raftery (1995) para diferencias de BIC:\n")
cat("     0-2   -> evidencia inapreciable\n")
cat("     2-6   -> evidencia positiva\n")
cat("     6-10  -> evidencia fuerte\n")
cat("     >10   -> evidencia muy fuerte\n")

if (dif_bic > 10) {
  cat("  --> Evidencia MUY FUERTE a favor de", mejor_bic, "\n")
} else if (dif_bic > 6) {
  cat("  --> Evidencia fuerte a favor de", mejor_bic, "\n")
} else if (dif_bic > 2) {
  cat("  --> Evidencia positiva a favor de", mejor_bic, "\n")
} else {
  cat("  --> Los modelos son prácticamente equivalentes en ajuste\n")
}
cat("\n  Salvedad: las verosimilitudes de un modelo Poisson (enlace log) y de\n")
cat("  uno binomial (enlace logit) no son directamente comparables (apartado 4.2).\n\n")


# -----------------------------------------------------------------------------
# PASO 7.2 - Backtesting comparado (el criterio que más pesa)
# -----------------------------------------------------------------------------
# Se reajustan ambos modelos sin los últimos años y se compara su capacidad
# de predecir el número de defunciones observado.

H_TEST <- 5

anios_train <- prep$anios[prep$anios <= max(prep$anios) - H_TEST]
anios_test  <- prep$anios[prep$anios >  max(prep$anios) - H_TEST]
pesos_train <- prep$pesos[, prep$anios %in% anios_train, drop = FALSE]

cat("PASO 7.2 - Backtesting comparado (", H_TEST, "años )\n")

# --- Lee-Carter ---
lc_tr   <- fit(lc(link = "log"), data = prep$datos,
               ages.fit = prep$edades, years.fit = anios_train,
               wxt = pesos_train)
m_lc    <- forecast(lc_tr, h = H_TEST)$rates

# --- CBD ---
cbd_tr  <- fit(cbd(link = "logit"), data = cbd$datos_inicial,
               ages.fit = prep$edades, years.fit = anios_train,
               wxt = pesos_train)
q_cbd   <- forecast(cbd_tr, h = H_TEST)$rates

# Para comparar en la misma escala, pasamos q del CBD a tasa central m:
#   q = 1 - exp(-m)   =>   m = -log(1 - q)
m_cbd <- -log(1 - q_cbd)

# --- Valores observados ---
m_real <- (prep$datos$Dxt / prep$datos$Ext)[, prep$anios %in% anios_test, drop = FALSE]
E_test <- prep$datos$Ext[, prep$anios %in% anios_test, drop = FALSE]
D_real <- prep$datos$Dxt[, prep$anios %in% anios_test, drop = FALSE]

# --- Métricas de error ---
calc_metricas <- function(m_pred, nombre) {
  err <- (m_pred - m_real) / m_real
  D_pred <- m_pred * E_test
  data.frame(
    Modelo       = nombre,
    Sesgo_medio  = round(100 * mean(err), 2),
    EAM_pct      = round(100 * mean(abs(err)), 2),
    RECM_log     = round(sqrt(mean((log(m_pred) - log(m_real))^2)), 4),
    Def_pred     = round(sum(D_pred)),
    Def_real     = round(sum(D_real)),
    Error_def_pct = round(100 * (sum(D_pred) / sum(D_real) - 1), 2),
    stringsAsFactors = FALSE
  )
}

bt <- rbind(
  calc_metricas(m_lc,  "Lee-Carter"),
  calc_metricas(m_cbd, "CBD")
)

cat("\n")
print(bt, row.names = FALSE)

# La ventana de validacion (2019-2023) contiene 2020, el anio excluido de la
# calibracion por el exceso de mortalidad de la pandemia. Ningun modelo de
# tendencia puede anticipar ese shock, de modo que las metricas anteriores
# penalizan a ambos por igual por un suceso ajeno a su especificacion. Se
# repite el calculo sin 2020, que es la comparacion limpia de capacidad
# predictiva (apartado 4.2 de la memoria); la version completa se conserva
# porque ilustra la magnitud del shock.
excl <- !(anios_test %in% config$anios_excluidos)
if (any(!excl)) {
  m_real_s <- m_real[, excl, drop = FALSE]
  E_test_s <- E_test[, excl, drop = FALSE]
  D_real_s <- D_real[, excl, drop = FALSE]
  calc_metricas_sin <- function(m_pred, nombre) {
    m_pred <- m_pred[, excl, drop = FALSE]
    err <- (m_pred - m_real_s) / m_real_s
    D_pred <- m_pred * E_test_s
    data.frame(
      Modelo       = nombre,
      Sesgo_medio  = round(100 * mean(err), 2),
      EAM_pct      = round(100 * mean(abs(err)), 2),
      RECM_log     = round(sqrt(mean((log(m_pred) - log(m_real_s))^2)), 4),
      Def_pred     = round(sum(D_pred)),
      Def_real     = round(sum(D_real_s)),
      Error_def_pct = round(100 * (sum(D_pred) / sum(D_real_s) - 1), 2),
      stringsAsFactors = FALSE)
  }
  bt_sin2020 <- rbind(calc_metricas_sin(m_lc, "Lee-Carter"),
                      calc_metricas_sin(m_cbd, "CBD"))
  cat("\n  Mismas metricas excluyendo", paste(config$anios_excluidos, collapse = ", "),
      "de la ventana de validacion:\n")
  print(bt_sin2020, row.names = FALSE)
} else {
  bt_sin2020 <- bt
}

cat("\n  EAM_pct       = error absoluto medio en las tasas (menor es mejor)\n")
cat("  RECM_log      = raíz del error cuadrático medio en escala log\n")
cat("  Error_def_pct = error en el TOTAL de defunciones predichas\n")
cat("                  (esta es la métrica más relevante para el gasto)\n\n")

# La seleccion se decide sobre la ventana sin el anio atipico.
mejor_bt <- bt_sin2020$Modelo[which.min(bt_sin2020$EAM_pct)]
cat("  Mejor capacidad predictiva (sin 2020):", mejor_bt, "\n")
cat("  Mejor capacidad predictiva (con 2020):", bt$Modelo[which.min(bt$EAM_pct)], "\n\n")


# -----------------------------------------------------------------------------
# PASO 7.3 - Comparación de residuos
# -----------------------------------------------------------------------------
res_lc  <- diag$residuos$residuo
res_cbd <- cbd$residuos$residuo

tabla_res <- data.frame(
  Modelo    = c("Lee-Carter", "CBD"),
  Media     = round(c(mean(res_lc), mean(res_cbd)), 4),
  Desv_tip  = round(c(sd(res_lc), sd(res_cbd)), 4),
  Pct_atipicos = round(c(100 * mean(abs(res_lc) > 3),
                         100 * mean(abs(res_cbd) > 3)), 2),
  stringsAsFactors = FALSE
)

cat("PASO 7.3 - Comparación de residuos:\n\n")
print(tabla_res, row.names = FALSE)
cat("\n  Lo ideal: media ~0, desviación ~1, atípicos ~0,3%\n\n")


# -----------------------------------------------------------------------------
# FIGURA 11 - Backtesting de los dos modelos frente a lo observado
# -----------------------------------------------------------------------------
edades_bt <- intersect(c(70, 80, 90), prep$edades)

df_fig <- do.call(rbind, lapply(edades_bt, function(e) {
  i <- which(prep$edades == e)
  rbind(
    data.frame(edad = e, anio = anios_test, m = m_real[i, ], Serie = "Observado"),
    data.frame(edad = e, anio = anios_test, m = m_lc[i, ],   Serie = "Lee-Carter"),
    data.frame(edad = e, anio = anios_test, m = m_cbd[i, ],  Serie = "CBD")
  )
}))

fig11 <- ggplot(df_fig, aes(anio, m, colour = Serie, linetype = Serie)) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.4) +
  facet_wrap(~ paste("Edad", edad), scales = "free_y") +
  scale_colour_manual(values = c("Observado" = "black",
                                 "Lee-Carter" = "#D95F02",
                                 "CBD" = "#1B9E77")) +
  scale_linetype_manual(values = c("Observado" = "solid",
                                   "Lee-Carter" = "dashed",
                                   "CBD" = "dotdash")) +
  labs(title = "Capacidad predictiva comparada: Lee-Carter frente a CBD",
       subtitle = paste("Proyecci\u00f3n de", H_TEST, "a\u00f1os fuera de muestra"),
       x = "A\u00f1o", y = "m(x,t)", caption = nota_fuente) +
  tema_tfm + theme(legend.position = "bottom")

ggsave(file.path(config$ruta_graficos, "fig11_comparacion_backtest.png"),
       fig11, width = 8.5, height = 4.2, dpi = 300)
cat("FIGURA 11 guardada: fig11_comparacion_backtest.png\n\n")


# -----------------------------------------------------------------------------
# PASO 7.4 - DECISIÓN: qué modelo es el caso base
# -----------------------------------------------------------------------------
# Sistema de puntuación simple y transparente, declarado en la memoria.

puntos <- c("Lee-Carter" = 0, "CBD" = 0)
puntos[mejor_bic] <- puntos[mejor_bic] + 2   # BIC pesa doble
puntos[mejor_aic] <- puntos[mejor_aic] + 1
puntos[mejor_bt]  <- puntos[mejor_bt]  + 3   # predicción pesa triple

mejor_res <- tabla_res$Modelo[which.min(abs(tabla_res$Pct_atipicos - 0.3))]
puntos[mejor_res] <- puntos[mejor_res] + 1

cat("PASO 7.4 - Puntuación final:\n")
cat("  BIC (peso 2)          ->", mejor_bic, "\n")
cat("  AIC (peso 1)          ->", mejor_aic, "\n")
cat("  Backtesting (peso 3)  ->", mejor_bt, "\n")
cat("  Residuos (peso 1)     ->", mejor_res, "\n\n")
cat("  Lee-Carter:", puntos["Lee-Carter"], "puntos\n")
cat("  CBD       :", puntos["CBD"], "puntos\n\n")

MODELO_BASE <- names(puntos)[which.max(puntos)]
MODELO_CONTRASTE <- setdiff(names(puntos), MODELO_BASE)

cat("  *** MODELO BASE      :", MODELO_BASE, "***\n")
cat("  *** MODELO CONTRASTE :", MODELO_CONTRASTE, "***\n\n")

cat("  El modelo no seleccionado no se descarta: se mantiene como contraste\n")
cat("  en los guiones 08 a 11 y las proyecciones de gasto se reportan con ambos.\n\n")


# -----------------------------------------------------------------------------
# PASO 7.5 - Guardar
# -----------------------------------------------------------------------------
saveRDS(
  list(
    tabla_criterios = tabla_comp,
    tabla_backtest  = bt,
    tabla_backtest_sin2020 = bt_sin2020,
    tabla_residuos  = tabla_res,
    puntuacion      = puntos,
    modelo_base      = MODELO_BASE,
    modelo_contraste = MODELO_CONTRASTE,
    dif_bic = dif_bic
  ),
  file.path(config$ruta_resultados, "comparacion_modelos.rds")
)

# Exportación de tablas a Excel
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    list(Criterios = tabla_comp, Backtesting = bt,
         Backtesting_sin2020 = bt_sin2020, Residuos = tabla_res),
    file.path(config$ruta_resultados, "tabla_comparacion_modelos.xlsx")
  )
  cat("Tablas exportadas a: tabla_comparacion_modelos.xlsx\n\n")
}

cat("=====================================================\n")
cat("  SCRIPT 07 COMPLETADO\n")
cat("=====================================================\n")
cat("  Modelo base:", MODELO_BASE, "\n")
cat("=====================================================\n\n")
cat("Siguiente guion: 08_proyeccion_montecarlo.R\n")
