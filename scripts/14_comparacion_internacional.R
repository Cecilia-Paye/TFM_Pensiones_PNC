# =============================================================================
# SCRIPT 14 - COMPARACION INTERNACIONAL CON INDICADORES RELATIVOS
#
# Construye los tres indicadores del apartado 3.3 para los tres paises, cada uno
# referido a las magnitudes de su propia economia. La comparacion de cuantias
# convertidas a euros a tipo de cambio de mercado describe el esfuerzo unitario
# pero no mide la suficiencia: una prestacion de trescientos euros anuales en
# Bolivia y otra de siete mil ochocientos en Espana no ocupan la misma posicion
# en la distribucion de renta de sus respectivos paises. Los indicadores que se
# calculan aqui son adimensionales y no dependen del tipo de cambio.
#
#   Cobertura        = beneficiarios / poblacion de referencia
#   Suficiencia (a)  = cuantia anual / producto por habitante
#   Suficiencia (b)  = cuantia anual / umbral nacional de pobreza
#   Esfuerzo fiscal  = gasto del programa / producto interior bruto nominal
#
# ENTRADA: datos_paises_comparacion.csv en datos/limpios
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(ggplot2)
library(scales)

cat("=== SCRIPT 14: COMPARACION INTERNACIONAL ===\n\n")

f <- file.path(RUTA_PROYECTO, "datos/limpios", "datos_paises_comparacion.csv")
if (!file.exists(f)) {
  stop("Falta ", f,
       "\nDebe copiarse la plantilla a datos/limpios y completar las celdas vacias.")
}
# Lectura robusta: admite el formato anglosajon (coma separadora, punto decimal)
# y el formato local de Excel en espanol (punto y coma separador, coma decimal).
primera <- readLines(f, n = 1, warn = FALSE)
if (grepl(";", primera)) {
  cat("  Formato detectado: separador punto y coma, decimal coma.\n")
  d <- read.csv2(f, stringsAsFactors = FALSE)
} else {
  cat("  Formato detectado: separador coma, decimal punto.\n")
  d <- read.csv(f, stringsAsFactors = FALSE)
}

# Si algun campo numerico ha quedado como texto por llevar coma decimal o
# separadores de millar, se convierte aqui.
numericas <- c("beneficiarios", "poblacion_referencia", "cuantia_anual",
               "gasto_programa_anual", "pib_nominal", "poblacion_total",
               "umbral_pobreza_anual", "pobreza_65mas")
for (cc in intersect(numericas, names(d))) {
  if (is.character(d[[cc]])) {
    x <- gsub("\\s|\\u00a0", "", d[[cc]])
    x <- gsub("\\.", "", x)      # separador de millar
    x <- gsub(",", ".", x)       # decimal
    d[[cc]] <- suppressWarnings(as.numeric(x))
  }
}

# -----------------------------------------------------------------------------
# 14.1 COMPROBACION DE INTEGRIDAD
# -----------------------------------------------------------------------------

cat("14.1 Integridad y plausibilidad de los datos\n")

obligatorias <- c("beneficiarios", "gasto_programa_anual", "pib_nominal",
                  "poblacion_total", "cuantia_anual", "umbral_pobreza_anual",
                  "poblacion_referencia")
faltan <- list()
for (i in seq_len(nrow(d))) {
  aus <- obligatorias[is.na(d[i, obligatorias])]
  if (length(aus)) faltan[[d$pais[i]]] <- aus
}

if (length(faltan)) {
  cat("  Datos ausentes:\n")
  for (p in names(faltan)) cat("   ", p, ":", paste(faltan[[p]], collapse = ", "), "\n")
  cat("  Los indicadores que dependan de ellos no se calcularan.\n\n")
} else {
  cat("  Todos los campos obligatorios estan cubiertos.\n\n")
}

# Comprobacion de plausibilidad: el gasto declarado debe guardar relacion con el
# producto de beneficiarios por cuantia. Una desviacion grande indica que alguna
# de las tres magnitudes no corresponde al concepto que se le supone.
cat("\n  Coherencia entre gasto, beneficiarios y cuantia:\n")
for (i in seq_len(nrow(d))) {
  if (is.na(d$gasto_programa_anual[i]) || is.na(d$cuantia_anual[i])) next
  teorico <- d$beneficiarios[i] * d$cuantia_anual[i]
  razon <- d$gasto_programa_anual[i] / teorico
  aviso <- if (razon > 1.25 || razon < 0.8) "  <-- REVISAR" else ""
  cat(sprintf("    %-8s gasto declarado / (beneficiarios x cuantia) = %.2f%s\n",
              d$pais[i], razon, aviso))
  if (razon > 1.25 || razon < 0.8) {
    cat(sprintf("             cuantia implicita = %.2f %s por beneficiario y anio\n",
                d$gasto_programa_anual[i] / d$beneficiarios[i], d$moneda[i]))
  }
}
cat("\n")

# Si el gasto del programa no se publica, puede aproximarse por el producto
# entre beneficiarios y cuantia media. Se marca como estimado.
d$gasto_estimado <- FALSE
sin_gasto <- is.na(d$gasto_programa_anual) & !is.na(d$cuantia_anual)
if (any(sin_gasto)) {
  d$gasto_programa_anual[sin_gasto] <- d$beneficiarios[sin_gasto] * d$cuantia_anual[sin_gasto]
  d$gasto_estimado[sin_gasto] <- TRUE
  cat("  Gasto estimado como beneficiarios por cuantia en:",
      paste(d$pais[sin_gasto], collapse = ", "), "\n")
  cat("  Se declara como estimacion propia en la memoria (Tabla 6).\n\n")
}

# -----------------------------------------------------------------------------
# 14.2 INDICADORES
# -----------------------------------------------------------------------------

d$pib_per_capita   <- d$pib_nominal / d$poblacion_total
d$cobertura_pct    <- d$beneficiarios / d$poblacion_referencia * 100
d$suf_pib_pc_pct   <- d$cuantia_anual / d$pib_per_capita * 100
d$suf_pobreza_pct  <- d$cuantia_anual / d$umbral_pobreza_anual * 100
d$esfuerzo_pib_pct <- d$gasto_programa_anual / d$pib_nominal * 100

cat("14.2 Indicadores comparados\n\n")
tab <- data.frame(
  Pais = d$pais,
  Anio_cobertura = d$anio_cobertura,
  Anio_fiscal = d$anio_fiscal,
  Cobertura = round(d$cobertura_pct, 2),
  Suf_sobre_PIBpc = round(d$suf_pib_pc_pct, 1),
  Suf_sobre_pobreza = round(d$suf_pobreza_pct, 1),
  Esfuerzo_fiscal = round(d$esfuerzo_pib_pct, 3))
print(tab, row.names = FALSE)

cat("\n  Cobertura        : porcentaje de la poblacion de referencia\n")
cat("  Suf. sobre PIBpc : cuantia anual como porcentaje del producto por habitante\n")
cat("  Suf. sobre pobreza: cuantia anual como porcentaje del umbral de pobreza\n")
cat("  Esfuerzo fiscal  : gasto del programa como porcentaje del producto\n\n")

# -----------------------------------------------------------------------------
# 14.3 LECTURA
# -----------------------------------------------------------------------------

cat("14.3 Lectura de los resultados\n\n")

iE <- match("Espana", d$pais)
for (i in seq_len(nrow(d))) {
  cat("  ", d$pais[i], "\n", sep = "")
  cat("    Cubre al", sprintf("%.2f %%", d$cobertura_pct[i]),
      "de su poblacion de referencia.\n")
  if (!is.na(d$suf_pib_pc_pct[i])) {
    cat("    La prestacion equivale al", sprintf("%.1f %%", d$suf_pib_pc_pct[i]),
        "del producto por habitante.\n")
  }
  if (!is.na(d$suf_pobreza_pct[i])) {
    cat("    Representa el", sprintf("%.1f %%", d$suf_pobreza_pct[i]),
        "del umbral nacional de pobreza",
        ifelse(d$suf_pobreza_pct[i] >= 100,
               "y basta por si sola para superarlo.",
               "y no basta por si sola para superarlo."), "\n")
  }
  if (!is.na(d$esfuerzo_pib_pct[i])) {
    cat("    Destina el", sprintf("%.3f %%", d$esfuerzo_pib_pct[i]),
        "de su producto al programa.\n")
  }
  cat("\n")
}

if (!any(is.na(d$suf_pib_pc_pct))) {
  r <- range(d$suf_pib_pc_pct)
  cat("  En terminos relativos a la riqueza de cada pais, la generosidad de las\n")
  cat("  tres prestaciones se situa entre el", sprintf("%.1f", r[1]), "y el",
      sprintf("%.1f %%", r[2]), "del producto por habitante.\n")
  # Razon entre cuantias en euros a tipo de cambio de mercado (config.rds).
  razon_eur <- config$cuantia_anual_espana / config$cuantia_anual_bolivia
  cat("  La razon entre el maximo y el minimo es de",
      sprintf("%.1f", r[2] / r[1]), "veces, frente a las",
      sprintf("%.1f", razon_eur), "que resultan de\n")
  cat("  comparar las cuantias convertidas a euros.\n\n")
}

# -----------------------------------------------------------------------------
# 14.4 GUARDAR Y REPRESENTAR
# -----------------------------------------------------------------------------

comp <- list(datos = d, tabla = tab, fecha = Sys.Date(),
             nota = paste("Indicadores relativos a la economia de cada pais.",
                          "No dependen del tipo de cambio."))
saveRDS(comp, file.path(config$ruta_resultados, "comparacion_internacional.rds"))

dir.create(file.path(RUTA_PROYECTO, "tablas"), showWarnings = FALSE)
write.csv(tab, file.path(RUTA_PROYECTO, "tablas", "T13_comparacion_internacional.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("14.4 Guardado comparacion_internacional.rds y T13\n")

tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10,
                                     colour = "grey30"),
        plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

if (!any(is.na(d$suf_pib_pc_pct)) && !any(is.na(d$esfuerzo_pib_pct))) {
  dg <- rbind(
    data.frame(pais = d$pais, ind = "Cobertura (%)", val = d$cobertura_pct),
    data.frame(pais = d$pais, ind = "Suficiencia sobre PIB per capita (%)",
               val = d$suf_pib_pc_pct),
    data.frame(pais = d$pais, ind = "Esfuerzo fiscal (% del PIB)",
               val = d$esfuerzo_pib_pct))
  dg$pais <- factor(dg$pais, levels = c("Espana", "Chile", "Bolivia"))

  f23 <- ggplot(dg, aes(pais, val, fill = pais)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = sprintf("%.2f", val)), vjust = -0.4, size = 3.2) +
    facet_wrap(~ind, scales = "free_y") +
    scale_fill_manual(values = c(Espana = "#1A237E", Chile = "#4A7FA5",
                                 Bolivia = "#C1440E"), guide = "none") +
    labs(title = "Los tres sistemas en indicadores relativos a cada econom\u00eda",
         subtitle = "Ninguno depende del tipo de cambio",
         x = NULL, y = NULL,
         caption = "Fuente: Elaboraci\u00f3n propia.") +
    tema + theme(panel.spacing = unit(1.2, "lines"))

  ggsave(file.path(config$ruta_graficos, "fig23_comparacion_relativa.png"),
         f23, width = 9.5, height = 4.5, dpi = 300)
  cat("  Figura guardada: fig23\n")
} else {
  cat("  Figura omitida: faltan datos para alguno de los indicadores.\n")
}
cat("\n=====================================================\n")
cat("  SCRIPT 14 COMPLETADO\n")
cat("=====================================================\n\n")