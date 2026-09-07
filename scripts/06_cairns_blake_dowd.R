# =============================================================================
# SCRIPT 06 - MODELO CAIRNS-BLAKE-DOWD (CBD)
#
# EL MODELO, EN UNA LÍNEA:
#     logit q(x,t) = k1(t) + k2(t) · (x - x̄)
#
# QUÉ SIGNIFICA CADA PIEZA:
#   k1(t)  NIVEL general de mortalidad en el año t. Si baja, todo el mundo
#          muere menos. Es el análogo del k(t) de Lee-Carter.
#   k2(t)  PENDIENTE de la mortalidad respecto a la edad en el año t.
#          Controla cómo de rápido crece la mortalidad al envejecer.
#          Si k2 crece, la brecha entre viejos y muy viejos se ensancha.
#   x̄      Edad media del rango ajustado. Solo sirve para centrar.
#
# DIFERENCIAS CLAVE FRENTE A LEE-CARTER (apartado 3.4.1 de la memoria):
#
#   | Aspecto           | Lee-Carter          | CBD                        |
#   |-------------------|---------------------|----------------------------|
#   | Variable          | m(x,t) tasa central | q(x,t) probabilidad        |
#   | Enlace            | log                 | logit                      |
#   | Exposición        | Central             | Inicial                    |
#   | Factores temporales| 1  (k)             | 2  (k1, k2)                |
#   | Parámetros de edad| a(x), b(x) libres   | Ninguno libre: forma lineal|
#   | Diseñado para     | Todo el rango       | EDADES ALTAS (60+)         |
#   | Nº de parámetros  | Muchos              | Pocos (más parsimonioso)   |
#
#   El CBD impone que logit q sea LINEAL en la edad. Es una restricción
#   fuerte, pero empíricamente muy razonable por encima de los 60 años, que
#   es justo el tramo relevante para pensiones de jubilación. A cambio de
#   esa restricción gana parsimonia y estabilidad en la proyección.
#
# FUNCIÓN EN EL TRABAJO:
#   Contraste de especificación. Si dos modelos distintos producen
#   proyecciones de gasto similares, la conclusión es robusta a la elección
#   del modelo de mortalidad (apartado 5.4.5). Referencia: Cairns, Blake y
#   Dowd (2006).
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(StMoMo)
library(ggplot2)
library(dplyr)
library(tidyr)

cat("=== SCRIPT 06: MODELO CAIRNS-BLAKE-DOWD ===\n\n")

prep <- readRDS(file.path(config$ruta_limpios, "datos_preparados.rds"))

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
# PASO 6.1 - Convertir exposiciones centrales en iniciales
# -----------------------------------------------------------------------------
# El CBD modeliza q(x,t) = probabilidad de que alguien VIVO al inicio del año
# muera durante ese año. El denominador tiene que ser la población INICIAL,
# no las personas-año vividas.
#
# La conversión estándar es:  E_inicial = E_central + D/2
# (los fallecidos viven, en promedio, medio año). Sin esta conversión el CBD
# produciría tasas sesgadas y no sería comparable con Lee-Carter.

datos_inicial <- central2initial(prep$datos)

cat("PASO 6.1 - Exposiciones convertidas a iniciales\n")
cat("  Exposición central total:",
    format(round(sum(prep$datos$Ext)), big.mark = "."), "\n")
cat("  Exposición inicial total:",
    format(round(sum(datos_inicial$Ext)), big.mark = "."), "\n")
cat("  Tipo del objeto:", datos_inicial$type, "\n\n")


# -----------------------------------------------------------------------------
# PASO 6.2 - Definir y ajustar el modelo CBD
# -----------------------------------------------------------------------------
# cbd() con link = "logit" es la formulación original del artículo de 2006.

CBD <- cbd(link = "logit")

cat("PASO 6.2 - Especificación del modelo:\n")
print(CBD)

cat("\nAjustando CBD (2020 excluido)...\n")

CBDfit <- fit(
  CBD,
  data      = datos_inicial,
  ages.fit  = prep$edades,
  years.fit = prep$anios,
  wxt       = prep$pesos
)

cat("Ajuste completado.\n\n")


# -----------------------------------------------------------------------------
# PASO 6.3 - Resultados del ajuste
# -----------------------------------------------------------------------------
aic_cbd <- AIC(CBDfit)
bic_cbd <- BIC(CBDfit)

cat("PASO 6.3 - Resumen del ajuste CBD:\n")
cat("  Log-verosimilitud :", round(CBDfit$loglik, 1), "\n")
cat("  Desviación        :", round(CBDfit$deviance, 1), "\n")
cat("  Parámetros libres :", CBDfit$npar, "\n")
cat("  AIC               :", round(aic_cbd, 1), "\n")
cat("  BIC               :", round(bic_cbd, 1), "\n\n")


# -----------------------------------------------------------------------------
# PASO 6.4 - Extraer k1(t) y k2(t)
# -----------------------------------------------------------------------------
# CBDfit$kt es una matriz de 2 filas: la primera es k1, la segunda k2.

k1 <- as.vector(CBDfit$kt[1, ])
k2 <- as.vector(CBDfit$kt[2, ])

kt_cbd <- data.frame(
  anio = prep$anios,
  k1   = k1,
  k2   = k2,
  usado = !(prep$anios %in% config$anios_excluidos)
)

cat("PASO 6.4 - Parámetros temporales:\n")
cat("  k1(t) [nivel]    : de", round(k1[1], 3), "a", round(k1[length(k1)], 3), "\n")
cat("  k2(t) [pendiente]: de", round(k2[1], 4), "a", round(k2[length(k2)], 4), "\n\n")

# Tendencias
kt_usado <- kt_cbd %>% filter(usado)
pend_k1 <- coef(lm(k1 ~ seq_len(nrow(kt_usado)), data = kt_usado))[2]
pend_k2 <- coef(lm(k2 ~ seq_len(nrow(kt_usado)), data = kt_usado))[2]

cat("  Pendiente de k1(t):", round(pend_k1, 5), "por año\n")
if (pend_k1 < 0) {
  cat("    -> El nivel general de mortalidad DESCIENDE. Correcto.\n")
} else {
  cat("    [AVISO] El nivel de mortalidad no desciende. Revisar los datos.\n")
}

cat("  Pendiente de k2(t):", round(pend_k2, 6), "por año\n")
if (pend_k2 > 0) {
  cat("    -> La mortalidad se hace MÁS empinada con la edad: las mejoras\n")
  cat("       se concentran en los más jóvenes del rango. Esto ENCARECE\n")
  cat("       las pensiones, porque más gente llega a edades muy altas.\n")
} else {
  cat("    -> La curva se aplana: las mejoras llegan también a los más viejos.\n")
}
cat("\n")


# -----------------------------------------------------------------------------
# FIGURA 9 - Los dos factores temporales del CBD
# -----------------------------------------------------------------------------
kt_largo <- kt_cbd %>%
  pivot_longer(c(k1, k2), names_to = "factor", values_to = "valor") %>%
  mutate(factor = recode(factor,
                         k1 = "k1(t) - Nivel",
                         k2 = "k2(t) - Pendiente por edad"))

fig9 <- ggplot(kt_largo, aes(anio, valor)) +
  geom_line(colour = "#7570B3", linewidth = 0.9) +
  geom_point(aes(shape = usado, colour = usado), size = 1.6) +
  facet_wrap(~ factor, scales = "free_y", ncol = 1) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 4),
                     labels = c("Excluido", "Usado"), name = NULL) +
  scale_colour_manual(values = c(`TRUE` = "#7570B3", `FALSE` = "red"),
                      labels = c("Excluido", "Usado"), name = NULL) +
  labs(title = "Factores temporales del modelo CBD",
       x = "A\u00f1o", y = NULL, caption = nota_fuente) +
  tema_tfm + theme(legend.position = "bottom")

ggsave(file.path(config$ruta_graficos, "fig09_cbd_kt.png"),
       fig9, width = 7, height = 6, dpi = 300)
cat("FIGURA 9 guardada: fig09_cbd_kt.png\n\n")


# -----------------------------------------------------------------------------
# PASO 6.5 - Residuos del CBD
# -----------------------------------------------------------------------------
res_cbd <- residuals(CBDfit, type = "deviance")

res_cbd_df <- expand.grid(edad = res_cbd$ages, anio = res_cbd$years)
res_cbd_df$residuo <- as.vector(res_cbd$residuals)
res_cbd_df <- res_cbd_df %>%
  filter(!is.na(residuo), !anio %in% config$anios_excluidos)

r <- res_cbd_df$residuo

cat("PASO 6.5 - Residuos del CBD:\n")
cat("  Media             :", round(mean(r), 4), "\n")
cat("  Desviación típica :", round(sd(r), 4), "\n")
cat("  |residuo| > 3     :", sum(abs(r) > 3), "celdas (",
    round(100 * mean(abs(r) > 3), 2), "% )\n\n")

fig10 <- ggplot(res_cbd_df, aes(anio, edad, fill = residuo)) +
  geom_tile() +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-4, 4),
                       oob = scales::squish, name = "Residuo") +
  labs(title = "Mapa de residuos de desviaci\u00f3n, CBD",
       x = "A\u00f1o", y = "Edad", caption = nota_fuente) +
  tema_tfm

ggsave(file.path(config$ruta_graficos, "fig10_residuos_cbd.png"),
       fig10, width = 7, height = 5, dpi = 300)
cat("FIGURA 10 guardada: fig10_residuos_cbd.png\n\n")


# -----------------------------------------------------------------------------
# PASO 6.6 - Bondad del ajuste
# -----------------------------------------------------------------------------
q_ajustado  <- fitted(CBDfit, type = "rates")   # aquí "rates" son q(x,t)
q_observado <- prep$datos$Dxt / datos_inicial$Ext

usadas <- prep$pesos == 1
err_rel <- (q_ajustado[usadas] - q_observado[usadas]) / q_observado[usadas]

r2_cbd <- 1 - sum((log(q_ajustado[usadas]) - log(q_observado[usadas]))^2) /
              sum((log(q_observado[usadas]) - mean(log(q_observado[usadas])))^2)

cat("PASO 6.6 - Bondad del ajuste CBD:\n")
cat("  Error absoluto medio:", round(100 * mean(abs(err_rel)), 2), "%\n")
cat("  R2 (escala log)     :", round(r2_cbd, 4), "\n\n")


# -----------------------------------------------------------------------------
# PASO 6.7 - Guardar
# -----------------------------------------------------------------------------
saveRDS(
  list(
    fit = CBDfit,
    datos_inicial = datos_inicial,
    k1 = k1, k2 = k2,
    kt_df = kt_cbd,
    aic = aic_cbd, bic = bic_cbd, r2 = r2_cbd,
    pendiente_k1 = pend_k1,
    pendiente_k2 = pend_k2,
    residuos = res_cbd_df,
    q_ajustado = q_ajustado
  ),
  file.path(config$ruta_resultados, "modelo_cbd.rds")
)

cat("=====================================================\n")
cat("  SCRIPT 06 COMPLETADO\n")
cat("=====================================================\n")
cat("  AIC:", round(aic_cbd, 1), "  BIC:", round(bic_cbd, 1),
    "  R2:", round(r2_cbd, 4), "\n")
cat("=====================================================\n\n")
cat("Siguiente guion: 07_comparacion_modelos.R\n")
