# =============================================================================
# SCRIPT 03 - ANÁLISIS EXPLORATORIO DE LA MORTALIDAD
#
# Análisis descriptivo previo a la modelización. Genera para la memoria:
#   - Figura 1: log-tasas de mortalidad por edad (varios años)
#   - Figura 2: evolución temporal de log m(x,t) para edades seleccionadas
#   - Figura 3: mapa de calor de la superficie de mortalidad
#   - Tabla 1: tasas y esperanza de vida en años clave
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

cat("=== SCRIPT 03: ANÁLISIS EXPLORATORIO ===\n\n")

prep <- readRDS(file.path(config$ruta_limpios, "datos_preparados.rds"))

Dxt <- prep$datos$Dxt
Ext <- prep$datos$Ext
edades <- prep$edades
anios  <- prep$anios

# Tasa central de mortalidad
Mxt <- Dxt / Ext

# Tema visual común para todas las figuras del TFM
tema_tfm <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10,
                                     colour = "grey30"),
    plot.caption     = element_text(size = 8, colour = "grey40", hjust = 0)
  )

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
# PASO 3.1 - Pasar la matriz a formato largo (una fila por edad-año)
# -----------------------------------------------------------------------------
# ggplot2 necesita los datos en formato "largo", no en matriz.

df <- expand.grid(edad = edades, anio = anios) %>%
  mutate(
    Dxt    = as.vector(Dxt),
    Ext    = as.vector(Ext),
    mxt    = Dxt / Ext,
    log_mxt = log(mxt),
    # q(x) = probabilidad de morir en el año, con aproximación de fuerza
    # de mortalidad constante dentro del año de edad
    qxt    = 1 - exp(-mxt)
  )

cat("PASO 3.1 - Formato largo:", nrow(df), "observaciones\n\n")


# -----------------------------------------------------------------------------
# FIGURA 1 - Perfil por edad en años seleccionados
# -----------------------------------------------------------------------------
# Lectura esperada: líneas casi rectas y paralelas que descienden con el tiempo.
#   - Rectas  -> la mortalidad crece exponencialmente con la edad (Gompertz)
#   - Bajando -> hay mejora de la longevidad
#   - Paralelas -> la mejora es parecida a todas las edades (supuesto de LC)

anios_muestra <- round(seq(min(anios), max(anios), length.out = 5))

fig1 <- df %>%
  filter(anio %in% anios_muestra) %>%
  ggplot(aes(x = edad, y = log_mxt, colour = factor(anio))) +
  geom_line(linewidth = 0.8) +
  scale_colour_brewer(palette = "RdYlBu", name = "Año") +
  labs(
    title    = "Perfil de mortalidad por edad, Espa\u00f1a",
    subtitle = "Logaritmo de la tasa central de mortalidad",
    x = "Edad", y = expression(log~m(x,t)),
    caption  = nota_fuente
  ) +
  tema_tfm

ggsave(file.path(config$ruta_graficos, "fig01_perfil_edad.png"),
       fig1, width = 7, height = 5, dpi = 300)
cat("FIGURA 1 guardada: fig01_perfil_edad.png\n")


# -----------------------------------------------------------------------------
# FIGURA 2 - Evolución temporal en edades clave
# -----------------------------------------------------------------------------
# Lectura esperada: tendencias descendentes y un pico en 2020, que
#   justifica la exclusión de ese año de la calibración.

edades_clave <- c(65, 70, 75, 80, 85, 90)
edades_clave <- edades_clave[edades_clave %in% edades]

fig2 <- df %>%
  filter(edad %in% edades_clave) %>%
  ggplot(aes(x = anio, y = log_mxt, colour = factor(edad))) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 0.9) +
  { if (2020 %in% anios)
      geom_vline(xintercept = 2020, linetype = "dashed",
                 colour = "grey30", linewidth = 0.5) } +
  { if (2020 %in% anios)
      annotate("text", x = 2020, y = max(df$log_mxt, na.rm = TRUE),
               label = " 2020 (excluido)", hjust = 0, size = 3,
               colour = "grey30") } +
  scale_colour_brewer(palette = "Dark2", name = "Edad") +
  labs(
    title    = "Evoluci\u00f3n temporal de la mortalidad por edad",
    subtitle = "El pico de 2020 corresponde al shock de COVID-19",
    x = "A\u00f1o", y = expression(log~m(x,t)),
    caption  = nota_fuente
  ) +
  tema_tfm

ggsave(file.path(config$ruta_graficos, "fig02_evolucion_temporal.png"),
       fig2, width = 7, height = 5, dpi = 300)
cat("FIGURA 2 guardada: fig02_evolucion_temporal.png\n")


# -----------------------------------------------------------------------------
# FIGURA 3 - Mapa de calor de la superficie de mortalidad
# -----------------------------------------------------------------------------
fig3 <- ggplot(df, aes(x = anio, y = edad, fill = log_mxt)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", name = expression(log~m(x,t))) +
  labs(
    title    = "Superficie de mortalidad, Espa\u00f1a",
    x = "A\u00f1o", y = "Edad",
    caption  = nota_fuente
  ) +
  tema_tfm

ggsave(file.path(config$ruta_graficos, "fig03_superficie.png"),
       fig3, width = 7, height = 5, dpi = 300)
cat("FIGURA 3 guardada: fig03_superficie.png\n\n")


# -----------------------------------------------------------------------------
# PASO 3.2 - Tasa anual media de mejora de la mortalidad
# -----------------------------------------------------------------------------
# Se estima ajustando una recta a log m(x,t) contra el año, edad por edad.
# La pendiente, cambiada de signo, es la mejora anual media.
# Se excluye 2020 también aquí, por coherencia con el ajuste del modelo.

df_sin_atipicos <- df %>% filter(!anio %in% config$anios_excluidos)

mejora <- df_sin_atipicos %>%
  group_by(edad) %>%
  summarise(
    pendiente = coef(lm(log_mxt ~ anio))[2],
    mejora_anual_pct = -100 * pendiente,
    .groups = "drop"
  )

cat("PASO 3.2 - Mejora anual media de la mortalidad (2020 excluido):\n")
print(as.data.frame(
  mejora %>%
    filter(edad %in% edades_clave) %>%
    mutate(mejora_anual_pct = round(mejora_anual_pct, 2))
), row.names = FALSE)

cat("\n  Media global:", round(mean(mejora$mejora_anual_pct), 2), "% anual\n")
cat("  INTERPRETACIÓN: la mortalidad cae en torno a ese porcentaje cada año.\n")
cat("  Valores típicos en países desarrollados: 1% - 2,5% anual.\n\n")


# -----------------------------------------------------------------------------
# PASO 3.3 - Esperanza de vida a los 65 años
# -----------------------------------------------------------------------------
# Indicador que conecta directamente con el coste de una prestación vitalicia.
#
# Cálculo: tabla de vida de periodo, aplicando a una cohorte ficticia las
# tasas observadas en cada año.

calcular_e65 <- function(m_col, edades) {
  # Edades desde los 65
  idx <- which(edades >= 65)
  if (length(idx) == 0) return(NA_real_)

  m <- m_col[idx]
  q <- 1 - exp(-m)             # probabilidad de muerte
  q[length(q)] <- 1            # el último grupo es cerrado: todos mueren

  p <- 1 - q                   # probabilidad de supervivencia
  l <- c(1, cumprod(p))        # supervivientes (l[1] = 1 a los 65)
  l <- l[1:length(q)]

  # Años vividos en cada edad (aproximación de media edad)
  L <- l * (1 - q / 2)

  sum(L) / l[1]
}

e65 <- sapply(seq_along(anios), function(j) calcular_e65(Mxt[, j], edades))

df_e65 <- data.frame(anio = anios, e65 = e65)

cat("PASO 3.3 - Esperanza de vida a los 65 años:\n")
print(df_e65 %>%
        filter(anio %in% anios_muestra) %>%
        mutate(e65 = round(e65, 2)),
      row.names = FALSE)

ganancia <- (max(e65, na.rm = TRUE) - e65[1])
cat("\n  Ganancia total en el periodo:", round(ganancia, 2), "años\n")
cat("  Ganancia media anual:", round(ganancia / (length(anios) - 1), 3), "años\n\n")

fig4 <- ggplot(df_e65, aes(x = anio, y = e65)) +
  geom_line(colour = "#2C7FB8", linewidth = 1) +
  geom_point(size = 1.2, colour = "#2C7FB8") +
  labs(
    title    = "Esperanza de vida a los 65 a\u00f1os, Espa\u00f1a",
    subtitle = "Tabla de vida de periodo",
    x = "A\u00f1o", y = "A\u00f1os restantes esperados",
    caption  = nota_fuente
  ) +
  tema_tfm

ggsave(file.path(config$ruta_graficos, "fig04_e65.png"),
       fig4, width = 7, height = 4.5, dpi = 300)
cat("FIGURA 4 guardada: fig04_e65.png\n\n")


# -----------------------------------------------------------------------------
# PASO 3.4 - Guardar resultados del exploratorio
# -----------------------------------------------------------------------------
saveRDS(
  list(df = df, mejora = mejora, e65 = df_e65, Mxt = Mxt),
  file.path(config$ruta_resultados, "exploratorio.rds")
)

cat("=====================================================\n")
cat("  SCRIPT 03 COMPLETADO\n")
cat("=====================================================\n")
cat("  4 figuras en:", config$ruta_graficos, "\n")
cat("=====================================================\n\n")
cat("Comprobacion visual recomendada: perfiles por edad casi lineales y\n")
cat("descendentes (fig. 1), pico de 2020 (fig. 2), superficie suave (fig. 3)\n")
cat("y esperanza de vida creciente (fig. 4).\n\n")
cat("Siguiente guion: 04_lee_carter.R\n")
