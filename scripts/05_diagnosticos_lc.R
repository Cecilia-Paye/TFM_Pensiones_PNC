# =============================================================================
# SCRIPT 05 - DIAGNÓSTICOS DEL MODELO LEE-CARTER
#
# Comprueba, con evidencia gráfica y numérica, si se cumplen los supuestos
# del modelo de Lee-Carter ajustado en el guion 04.
#
# QUÉ SE COMPRUEBA:
#   1. Residuos de desviación: ¿están sin estructura?
#   2. Efecto cohorte: ¿hay bandas diagonales en el mapa de residuos?
#   3. Normalidad de los residuos
#   4. Estabilidad temporal: ¿b(x) es realmente constante en el tiempo?
#   5. Backtesting: ¿el modelo habría acertado el pasado reciente?
#
# QUÉ ES UN "RESIDUO DE DESVIACIÓN":
#   No podemos usar residuos normales porque los datos son recuentos Poisson.
#   El residuo de desviación mide la contribución de cada celda a la falta de
#   ajuste total, en una escala en la que, si el modelo es correcto, se
#   comporta aproximadamente como una normal estándar.
#   Regla práctica: valores fuera de (-3, 3) son celdas problemáticas.
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(StMoMo)
library(ggplot2)
library(dplyr)

cat("=== SCRIPT 05: DIAGNÓSTICOS LEE-CARTER ===\n\n")

prep <- readRDS(file.path(config$ruta_limpios, "datos_preparados.rds"))
lc   <- readRDS(file.path(config$ruta_resultados, "modelo_lee_carter.rds"))

LCfit <- lc$fit

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
# PASO 5.1 - Calcular los residuos
# -----------------------------------------------------------------------------
res <- residuals(LCfit, type = "deviance")

# Los pasamos a formato largo para poder graficarlos
res_df <- expand.grid(edad = res$ages, anio = res$years)
res_df$residuo <- as.vector(res$residuals)
res_df$cohorte <- res_df$anio - res_df$edad   # año de nacimiento

# Se eliminan las celdas excluidas del ajuste (2020), cuyo residuo es vacío
res_df <- res_df %>% filter(!is.na(residuo), !anio %in% config$anios_excluidos)

cat("PASO 5.1 - Residuos calculados:", nrow(res_df), "celdas\n\n")


# -----------------------------------------------------------------------------
# PASO 5.2 - Estadísticos de los residuos
# -----------------------------------------------------------------------------
r <- res_df$residuo

cat("PASO 5.2 - Estadísticos de los residuos de desviación:\n")
cat("  Media              :", round(mean(r), 4), " (debería ser ~0)\n")
cat("  Desviación típica  :", round(sd(r), 4), " (debería ser ~1)\n")
cat("  Mínimo             :", round(min(r), 2), "\n")
cat("  Máximo             :", round(max(r), 2), "\n")

fuera <- sum(abs(r) > 3)
cat("  |residuo| > 3      :", fuera, "celdas (",
    round(100 * fuera / length(r), 2), "% )\n")
cat("  Esperado si el modelo es correcto: en torno al 0,3%\n")

if (100 * fuera / length(r) > 2) {
  cat("  [AVISO] Demasiadas celdas atipicas para la especificacion adoptada.\n")
  cat("          Cabria considerar un modelo con efecto cohorte (Renshaw-Haberman).\n")
}

# Test de normalidad (con muchos datos tiende a rechazar; se interpreta con
# cautela y en conjunción con los gráficos)
if (length(r) >= 3 && length(r) <= 5000) {
  sw <- shapiro.test(r)
  cat("  Shapiro-Wilk p-valor:", format.pval(sw$p.value, digits = 3), "\n")
}
cat("\n")


# -----------------------------------------------------------------------------
# FIGURA 6 - Mapa de calor de residuos
# -----------------------------------------------------------------------------
# Lectura:
#   - Colores sin patrón            -> el modelo captura la estructura
#   - Bandas diagonales             -> efecto cohorte no capturado (limitación
#                                      conocida de Lee-Carter)
#   - Bandas verticales             -> años atípicos no tratados
#   - Bandas horizontales           -> fallo sistemático a ciertas edades

fig6 <- ggplot(res_df, aes(x = anio, y = edad, fill = residuo)) +
  geom_tile() +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-4, 4),
                       oob = scales::squish, name = "Residuo") +
  labs(
    title = "Mapa de residuos de desviaci\u00f3n, Lee-Carter",
    subtitle = "Bandas diagonales indicar\u00edan efecto cohorte no capturado",
    x = "A\u00f1o", y = "Edad", caption = nota_fuente
  ) + tema_tfm

ggsave(file.path(config$ruta_graficos, "fig06_residuos_mapa.png"),
       fig6, width = 7, height = 5, dpi = 300)
cat("FIGURA 6 guardada: fig06_residuos_mapa.png\n")


# -----------------------------------------------------------------------------
# FIGURA 7 - Residuos por edad, por año y por cohorte
# -----------------------------------------------------------------------------
g_edad <- ggplot(res_df, aes(factor(edad), residuo)) +
  geom_boxplot(outlier.size = 0.5, fill = "grey90") +
  geom_hline(yintercept = c(-3, 0, 3), linetype = c("dashed","solid","dashed"),
             colour = c("red","black","red")) +
  labs(title = "Residuos por edad", x = "Edad", y = "Residuo") +
  tema_tfm +
  theme(axis.text.x = element_text(angle = 90, size = 6))

g_anio <- ggplot(res_df, aes(factor(anio), residuo)) +
  geom_boxplot(outlier.size = 0.5, fill = "grey90") +
  geom_hline(yintercept = c(-3, 0, 3), linetype = c("dashed","solid","dashed"),
             colour = c("red","black","red")) +
  labs(title = "Residuos por a\u00f1o", x = "A\u00f1o", y = "Residuo") +
  tema_tfm +
  theme(axis.text.x = element_text(angle = 90, size = 6))

# El gráfico por cohorte es el que revela el efecto generación
g_coh <- res_df %>%
  group_by(cohorte) %>%
  summarise(media = mean(residuo), n = n(), .groups = "drop") %>%
  filter(n >= 5) %>%          # solo cohortes con datos suficientes
  ggplot(aes(cohorte, media)) +
  geom_col(fill = "#4575B4") +
  geom_hline(yintercept = 0) +
  labs(title = "Residuo medio por cohorte de nacimiento",
       subtitle = "Un patr\u00f3n sistem\u00e1tico aqu\u00ed indica efecto cohorte",
       x = "A\u00f1o de nacimiento", y = "Residuo medio",
       caption = nota_fuente) +
  tema_tfm

ggsave(file.path(config$ruta_graficos, "fig07a_res_edad.png"), g_edad,
       width = 7, height = 4, dpi = 300)
ggsave(file.path(config$ruta_graficos, "fig07b_res_anio.png"), g_anio,
       width = 7, height = 4, dpi = 300)
ggsave(file.path(config$ruta_graficos, "fig07c_res_cohorte.png"), g_coh,
       width = 7, height = 4, dpi = 300)

cat("FIGURAS 7a, 7b, 7c guardadas.\n\n")


# -----------------------------------------------------------------------------
# PASO 5.3 - Test formal de efecto cohorte
# -----------------------------------------------------------------------------
# Idea: si no hay efecto cohorte, el residuo medio de cada cohorte debería
# ser cero y no debería haber autocorrelación entre cohortes consecutivas.

coh_medias <- res_df %>%
  group_by(cohorte) %>%
  summarise(media = mean(residuo), n = n(), .groups = "drop") %>%
  filter(n >= 5)

cat("PASO 5.3 - Diagnóstico de efecto cohorte:\n")

# Autocorrelación de orden 1 de las medias por cohorte
if (nrow(coh_medias) > 10) {
  acf1 <- acf(coh_medias$media, lag.max = 1, plot = FALSE)$acf[2]
  cat("  Autocorrelación de orden 1 :", round(acf1, 3), "\n")
  if (abs(acf1) > 0.5) {
    cat("  [AVISO] Autocorrelacion alta: hay estructura de cohorte no capturada.\n")
    cat("          Se declara en las limitaciones (apartado 3.6.2); Lee-Carter se\n")
    cat("          mantiene por parsimonia y comparabilidad, y su efecto sobre el\n")
    cat("          gasto se acota con el contraste CBD (guion 11).\n")
  } else {
    cat("  Autocorrelación moderada. Lee-Carter es defendible.\n")
  }
}

# Proporción de la varianza de los residuos explicada por la cohorte
aov_coh <- summary(aov(residuo ~ factor(cohorte), data = res_df))
ss <- aov_coh[[1]][["Sum Sq"]]
prop_coh <- ss[1] / sum(ss)
cat("  Varianza explicada por cohorte:", round(100 * prop_coh, 1), "%\n\n")


# -----------------------------------------------------------------------------
# PASO 5.4 - BACKTESTING (validación fuera de muestra)
# -----------------------------------------------------------------------------
# Se reajusta el modelo sin los últimos años, se proyecta y se compara con lo
# observado. La ventana de validación 2019-2023 contiene el ejercicio 2020 y
# los dos siguientes, de mortalidad superior a la tendencia; el guion 07
# calcula además las métricas excluido 2020.

H_TEST <- 5   # nº de años que dejamos fuera

anios_train <- prep$anios[prep$anios <= max(prep$anios) - H_TEST]
anios_test  <- prep$anios[prep$anios >  max(prep$anios) - H_TEST]

cat("PASO 5.4 - Backtesting:\n")
cat("  Entrenamiento:", min(anios_train), "-", max(anios_train), "\n")
cat("  Validación   :", min(anios_test), "-", max(anios_test), "\n")

pesos_train <- prep$pesos[, prep$anios %in% anios_train, drop = FALSE]

LCfit_train <- fit(lc(link = "log"), data = prep$datos,
                   ages.fit = prep$edades, years.fit = anios_train,
                   wxt = pesos_train)

pred <- forecast(LCfit_train, h = H_TEST)
m_pred <- pred$rates

m_real <- (prep$datos$Dxt / prep$datos$Ext)[, prep$anios %in% anios_test, drop = FALSE]

err <- (m_pred - m_real) / m_real

cat("  Error relativo medio  :", round(100 * mean(err), 2), "%\n")
cat("  Error absoluto medio  :", round(100 * mean(abs(err)), 2), "%\n")
cat("  Error máximo          :", round(100 * max(abs(err)), 2), "%\n")
cat("  Referencia habitual: error absoluto medio inferior al 5 % en ventanas\n")
cat("  sin shocks de mortalidad. La ventana 2019-2023 incluye la pandemia.\n")

if (mean(err) > 0.02) {
  cat("  [NOTA] El modelo SOBREESTIMA la mortalidad -> subestimaría el gasto.\n")
} else if (mean(err) < -0.02) {
  cat("  [NOTA] El modelo INFRAESTIMA la mortalidad -> sobreestimaría el gasto.\n")
}
cat("\n")

# Gráfico del backtesting a edades clave
edades_bt <- intersect(c(70, 80, 90), prep$edades)

bt_df <- do.call(rbind, lapply(edades_bt, function(e) {
  i <- which(prep$edades == e)
  rbind(
    data.frame(edad = e, anio = anios_test, m = m_real[i, ], serie = "Observado"),
    data.frame(edad = e, anio = anios_test, m = m_pred[i, ], serie = "Proyectado")
  )
}))

fig8 <- ggplot(bt_df, aes(anio, m, colour = serie, linetype = serie)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  facet_wrap(~ paste("Edad", edad), scales = "free_y") +
  scale_colour_manual(values = c("Observado" = "black", "Proyectado" = "#D95F02"),
                      name = NULL) +
  scale_linetype_manual(values = c("Observado" = "solid", "Proyectado" = "dashed"),
                        name = NULL) +
  labs(title = "Backtesting del modelo Lee-Carter",
       subtitle = paste("Proyecci\u00f3n de", H_TEST, "a\u00f1os frente a lo observado"),
       x = "A\u00f1o", y = "m(x,t)", caption = nota_fuente) +
  tema_tfm + theme(legend.position = "bottom")

ggsave(file.path(config$ruta_graficos, "fig08_backtesting.png"),
       fig8, width = 8, height = 4, dpi = 300)
cat("FIGURA 8 guardada: fig08_backtesting.png\n\n")


# -----------------------------------------------------------------------------
# PASO 5.5 - Guardar
# -----------------------------------------------------------------------------
saveRDS(
  list(
    residuos = res_df,
    media_res = mean(r), sd_res = sd(r),
    pct_atipicos = 100 * fuera / length(r),
    prop_var_cohorte = prop_coh,
    backtest = list(
      h = H_TEST,
      anios_test = anios_test,
      error_medio = mean(err),
      error_abs_medio = mean(abs(err)),
      error_max = max(abs(err)),
      datos = bt_df
    )
  ),
  file.path(config$ruta_resultados, "diagnosticos_lc.rds")
)

cat("=====================================================\n")
cat("  SCRIPT 05 COMPLETADO\n")
cat("=====================================================\n")
cat("  Residuos atípicos:", round(100 * fuera / length(r), 2), "%\n")
cat("  Error backtesting:", round(100 * mean(abs(err)), 2), "%\n")
cat("=====================================================\n\n")
cat("Siguiente guion: 06_cairns_blake_dowd.R\n")
