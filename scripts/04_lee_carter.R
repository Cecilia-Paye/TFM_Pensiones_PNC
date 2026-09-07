# =============================================================================
# SCRIPT 04 - MODELO LEE-CARTER (estimado por máxima verosimilitud de Poisson)
#
# EL MODELO, EN UNA LÍNEA:
#     log m(x,t) = a(x) + b(x) · k(t)
#
# QUÉ SIGNIFICA CADA PIEZA:
#   a(x)  Nivel medio de mortalidad a la edad x. Es el perfil "de fondo":
#         crece casi linealmente con la edad. NO cambia con el tiempo.
#   k(t)  Índice temporal de mortalidad. Un único número por año que resume
#         "cómo de alta está la mortalidad ese año". Baja con el tiempo.
#         Es la única serie que se proyecta (guion 08).
#   b(x)  Sensibilidad de cada edad a los cambios en k(t). Si b(x) es grande,
#         esa edad se beneficia mucho de las mejoras; si es pequeña, poco.
#
# POR QUÉ POISSON Y NO SVD (apartado 3.4.1 de la memoria):
#   El artículo original de Lee y Carter (1992) estima el modelo por
#   descomposición en valores singulares (SVD) sobre log m(x,t). Ese enfoque
#   supone errores homocedásticos en escala logarítmica, lo cual es
#   inconsistente con la naturaleza de los datos: el número de defunciones es
#   un recuento, y su varianza crece con su media. A edades muy altas, donde
#   hay pocas muertes, el SVD sobrepondera celdas muy ruidosas.
#   Brouhns, Denuit y Vermunt (2002) reformulan el modelo suponiendo
#       D(x,t) ~ Poisson( E(x,t) · m(x,t) )
#   y lo estiman por máxima verosimilitud. Es el estándar actual y es lo que
#   implementa StMoMo.
#
# RESTRICCIONES DE IDENTIFICACIÓN:
#   El modelo no está identificado tal cual: si multiplicas b(x) por 2 y
#   divides k(t) por 2, obtienes exactamente las mismas tasas. Hay que fijar
#   una normalización. StMoMo usa la habitual:
#       suma de b(x) = 1     y     suma de k(t) = 0
#   Consecuencia práctica: k(t) se interpreta como DESVIACIÓN respecto a la
#   media del periodo, no como nivel absoluto.
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(StMoMo)
library(ggplot2)
library(dplyr)

cat("=== SCRIPT 04: MODELO LEE-CARTER ===\n\n")

prep <- readRDS(file.path(config$ruta_limpios, "datos_preparados.rds"))

tema_tfm <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10,
                                     colour = "grey30"),
        plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))


# -----------------------------------------------------------------------------
# PASO 4.1 - Definir la especificación del modelo
# -----------------------------------------------------------------------------
# lc() construye la especificación Lee-Carter.
# link = "log" -> enlace logarítmico -> familia Poisson sobre tasas centrales.
# (La alternativa link = "logit" daría un modelo binomial sobre q(x), que no
#  es lo que queremos aquí.)

LC <- lc(link = "log")

cat("PASO 4.1 - Especificación del modelo:\n")
print(LC)
cat("\n")


# -----------------------------------------------------------------------------
# PASO 4.2 - Ajustar el modelo (AJUSTE PRIMARIO, sin 2020)
# -----------------------------------------------------------------------------
# fit() estima a(x), b(x) y k(t) maximizando la verosimilitud de Poisson.
# Internamente usa el paquete gnm (generalized nonlinear models).

cat("PASO 4.2 - Ajustando Lee-Carter (2020 excluido)...\n")

t_inicio <- Sys.time()

LCfit <- fit(
  LC,
  data      = prep$datos,
  ages.fit  = prep$edades,
  years.fit = prep$anios,
  wxt       = prep$pesos      # <-- aquí es donde 2020 queda fuera
)

t_fin <- Sys.time()

cat("  Ajuste completado en",
    round(as.numeric(difftime(t_fin, t_inicio, units = "secs")), 1),
    "segundos\n\n")


# -----------------------------------------------------------------------------
# PASO 4.3 - Leer los resultados del ajuste
# -----------------------------------------------------------------------------
cat("PASO 4.3 - Resumen del ajuste:\n")
cat("  Log-verosimilitud :", round(LCfit$loglik, 1), "\n")
cat("  Desviación        :", round(LCfit$deviance, 1), "\n")
cat("  Parámetros libres :", LCfit$npar, "\n")
cat("  Nº observaciones  :", LCfit$nobs, "\n")

# Criterios de información: penalizan la complejidad del modelo (menor es
# mejor). Se emplean en el guion 07 para la comparación con CBD.
aic_lc <- AIC(LCfit)
bic_lc <- BIC(LCfit)

cat("  AIC               :", round(aic_lc, 1), "\n")
cat("  BIC               :", round(bic_lc, 1), "\n\n")


# -----------------------------------------------------------------------------
# PASO 4.4 - Extraer y examinar los parámetros
# -----------------------------------------------------------------------------
ax <- LCfit$ax
bx <- as.vector(LCfit$bx)
kt <- as.vector(LCfit$kt)

param <- data.frame(edad = prep$edades, ax = ax, bx = bx)
kt_df <- data.frame(anio = prep$anios, kt = kt)

# Los años con peso nulo no tienen k(t) identificado: StMoMo lo devuelve como
# NA. El guion 08 lo sustituye por interpolación lineal antes de proyectar.
kt_df$usado <- !(kt_df$anio %in% config$anios_excluidos)

cat("PASO 4.4 - Comprobación de las restricciones de identificación:\n")
cat("  suma de b(x) =", round(sum(bx), 6), " (debe ser 1)\n")
# k(2020) se devuelve como NA por tener peso nulo; se excluye de la suma.
cat("  suma de k(t) =", round(sum(kt, na.rm = TRUE), 6), " (debe ser 0)\n\n")

cat("  Rango de a(x): ", round(min(ax), 3), " a ", round(max(ax), 3), "\n", sep = "")
cat("  Rango de b(x): ", round(min(bx), 5), " a ", round(max(bx), 5), "\n", sep = "")
cat("  Rango de k(t): ", round(min(kt, na.rm = TRUE), 2), " a ", round(max(kt, na.rm = TRUE), 2), "\n\n", sep = "")

# --- Diagnóstico crítico: ¿hay b(x) negativos? ---
# Un b(x) negativo significa que esa edad EMPEORA cuando el resto mejora.
# Suele ser un artefacto en edades muy altas con pocos datos.
if (any(bx < 0)) {
  edades_neg <- prep$edades[bx < 0]
  cat("  [AVISO] b(x) negativo en las edades:", paste(edades_neg, collapse = ", "), "\n")
  cat("          Implica mortalidad creciente a esas edades. Suele deberse a\n")
  cat("          escasez de datos. Cabria reducir la edad maxima.\n\n")
} else {
  cat("  Todos los b(x) son positivos. Correcto.\n\n")
}

# --- Diagnóstico crítico: ¿k(t) es monótonamente decreciente? ---
kt_usado <- kt_df$kt[kt_df$usado]
pend_kt <- coef(lm(kt_usado ~ seq_along(kt_usado)))[2]

cat("  Pendiente media de k(t):", round(pend_kt, 3), "por año\n")
if (pend_kt < 0) {
  cat("  k(t) desciende -> la mortalidad mejora. Es lo esperado.\n\n")
} else {
  cat("  [AVISO] k(t) no desciende. Revisar los datos antes de continuar.\n\n")
}


# -----------------------------------------------------------------------------
# FIGURA 5 - Los tres parámetros del modelo
# -----------------------------------------------------------------------------
# Los datos de partida proceden de la Human Mortality Database. El generador
# de series de prueba se elimino del guion 01, de modo que la comprobacion
# siguiente no deberia activarse nunca; si lo hiciera, es preferible detener
# la ejecucion a incorporar al trabajo una figura con una advertencia impresa.
if (grepl("SINT", prep$tipo_datos)) {
  stop("Los datos cargados no son los definitivos. Ejecute el guion 01 con ",
       "los ficheros de la Human Mortality Database antes de continuar.")
}
nota_fuente <- "Fuente: Elaboraci\u00f3n propia a partir de la Human Mortality Database."

g_ax <- ggplot(param, aes(edad, ax)) +
  geom_line(colour = "#D95F02", linewidth = 0.9) +
  labs(title = expression(a(x)~": nivel medio de mortalidad"),
       x = "Edad", y = expression(a(x))) + tema_tfm

g_bx <- ggplot(param, aes(edad, bx)) +
  geom_line(colour = "#1B9E77", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = expression(b(x)~": sensibilidad a la mejora"),
       x = "Edad", y = expression(b(x))) + tema_tfm

g_kt <- ggplot(kt_df, aes(anio, kt)) +
  geom_line(colour = "#7570B3", linewidth = 0.9) +
  geom_point(aes(shape = usado, colour = usado), size = 1.8) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 4),
                     labels = c(`TRUE` = "Usado", `FALSE` = "Excluido"),
                     name = NULL) +
  scale_colour_manual(values = c(`TRUE` = "#7570B3", `FALSE` = "red"),
                      labels = c(`TRUE` = "Usado", `FALSE` = "Excluido"),
                      name = NULL) +
  labs(title = expression(k(t)~": \u00edndice temporal de mortalidad"),
       subtitle = "Es la \u00fanica serie que se proyecta al futuro",
       x = "A\u00f1o", y = expression(k(t)),
       caption = nota_fuente) +
  tema_tfm + theme(legend.position = "bottom")

ggsave(file.path(config$ruta_graficos, "fig05a_lc_ax.png"), g_ax,
       width = 6, height = 4, dpi = 300)
ggsave(file.path(config$ruta_graficos, "fig05b_lc_bx.png"), g_bx,
       width = 6, height = 4, dpi = 300)
ggsave(file.path(config$ruta_graficos, "fig05c_lc_kt.png"), g_kt,
       width = 7, height = 4.5, dpi = 300)

cat("FIGURAS 5a, 5b, 5c guardadas.\n\n")


# -----------------------------------------------------------------------------
# PASO 4.5 - Bondad del ajuste: observado frente a ajustado
# -----------------------------------------------------------------------------
# fitted() devuelve las tasas que el modelo predice para el periodo histórico.
# Si el modelo es bueno, deben parecerse mucho a las observadas.

m_ajustado  <- fitted(LCfit, type = "rates")
m_observado <- prep$datos$Dxt / prep$datos$Ext

# Comparamos solo en las celdas que se usaron para el ajuste
usadas <- prep$pesos == 1

err_rel <- (m_ajustado[usadas] - m_observado[usadas]) / m_observado[usadas]

cat("PASO 4.5 - Bondad del ajuste (solo celdas usadas):\n")
cat("  Error relativo medio  :", round(100 * mean(err_rel), 2), "%\n")
cat("  Error absoluto medio  :", round(100 * mean(abs(err_rel)), 2), "%\n")
cat("  Error máximo          :", round(100 * max(abs(err_rel)), 2), "%\n")

# R2 en escala logarítmica
r2 <- 1 - sum((log(m_ajustado[usadas]) - log(m_observado[usadas]))^2) /
          sum((log(m_observado[usadas]) - mean(log(m_observado[usadas])))^2)
cat("  R2 (escala log)       :", round(r2, 4), "\n")
cat("  Un R2 por encima de 0,98 es lo habitual en Lee-Carter.\n\n")


# -----------------------------------------------------------------------------
# PASO 4.6 - Ajuste alternativo INCLUYENDO 2020 (análisis de sensibilidad)
# -----------------------------------------------------------------------------
# Cuantifica el efecto de la exclusión de 2020 sobre la tendencia estimada
# (apartado 3.6.2 de la memoria).

cat("PASO 4.6 - Ajuste alternativo incluyendo 2020...\n")

LCfit_con2020 <- fit(
  LC,
  data      = prep$datos,
  ages.fit  = prep$edades,
  years.fit = prep$anios,
  wxt       = prep$pesos_todo
)

kt_con <- as.vector(LCfit_con2020$kt)
pend_con <- coef(lm(kt_con ~ seq_along(kt_con)))[2]

cat("  Pendiente k(t) SIN 2020:", round(pend_kt, 4), "\n")
cat("  Pendiente k(t) CON 2020:", round(pend_con, 4), "\n")
cat("  Diferencia relativa    :",
    round(100 * (pend_con / pend_kt - 1), 1), "%\n")
cat("  --> Cifra empleada en el analisis de sensibilidad (apartado 3.6.2).\n\n")


# -----------------------------------------------------------------------------
# PASO 4.7 - Guardar
# -----------------------------------------------------------------------------
saveRDS(
  list(
    fit          = LCfit,
    fit_con2020  = LCfit_con2020,
    ax = ax, bx = bx, kt = kt,
    kt_df        = kt_df,
    param        = param,
    aic = aic_lc, bic = bic_lc,
    r2 = r2,
    pendiente_kt = pend_kt,
    pendiente_kt_con2020 = pend_con,
    m_ajustado   = m_ajustado
  ),
  file.path(config$ruta_resultados, "modelo_lee_carter.rds")
)

cat("=====================================================\n")
cat("  SCRIPT 04 COMPLETADO\n")
cat("=====================================================\n")
cat("  AIC:", round(aic_lc, 1), "  BIC:", round(bic_lc, 1),
    "  R2:", round(r2, 4), "\n")
cat("=====================================================\n\n")
cat("Siguiente guion: 05_diagnosticos_lc.R\n")
