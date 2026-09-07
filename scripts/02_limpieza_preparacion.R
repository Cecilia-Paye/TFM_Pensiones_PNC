# =============================================================================
# SCRIPT 02 - LIMPIEZA Y PREPARACIÓN DE LOS DATOS
#
# QUÉ HACE:
#   1. Recorta los datos al rango de edades y años que interesan
#   2. Aplica la DECISIÓN METODOLÓGICA de excluir 2020 (shock COVID-19)
#   3. Trata ceros y valores imposibles
#   4. Construye el objeto "StMoMoData" que necesitan Lee-Carter y CBD
#   5. Guarda una versión CON 2020 para el análisis de sensibilidad
#
# JUSTIFICACIÓN DE LA EXCLUSIÓN DE 2020 (apartado 3.4.1 de la memoria):
#   El año 2020 presenta un exceso de mortalidad atípico y no recurrente
#   asociado a la pandemia de COVID-19. Incluirlo en la calibración de un
#   modelo cuya finalidad es proyectar la TENDENCIA de largo plazo sesgaría
#   al alza el nivel proyectado de mortalidad y, por tanto, subestimaría el
#   gasto futuro en pensiones. Se excluye del ajuste primario y se conserva
#   para un análisis de sensibilidad (Fase 07).
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(StMoMo)
library(dplyr)

cat("=== SCRIPT 02: LIMPIEZA Y PREPARACIÓN ===\n\n")


# -----------------------------------------------------------------------------
# PASO 2.1 - Cargar los datos crudos del script 01
# -----------------------------------------------------------------------------
dd <- readRDS(file.path(config$ruta_crudos, "datos_crudos.rds"))

cat("Datos de entrada:", dd$tipo, "\n")
cat("Dimensión original:", nrow(dd$Dxt), "edades x", ncol(dd$Dxt), "años\n\n")


# -----------------------------------------------------------------------------
# PASO 2.2 - Recortar al rango de edades del modelo
# -----------------------------------------------------------------------------
# Ajustamos el modelo de mortalidad sobre edades 60-100.
#
# POR QUÉ 60 Y NO 0: el TFM estudia pensiones de jubilación no contributivas,
# cuyo hecho causante está en los 65 años. La mortalidad infantil y adulta
# joven es irrelevante para el gasto y además tiene una dinámica temporal
# muy distinta que empeoraría el ajuste del modelo en el tramo que sí importa.
# Empezamos en 60 (y no en 65) para dar margen al modelo y poder construir
# después el grupo 65-69 sin efectos de borde.
#
# POR QUÉ 100 Y NO 110: por encima de 100 los datos son escasos y ruidosos.
# El grupo 100+ se tratará como grupo abierto en el módulo de pensiones.

filtro_edad <- dd$ages >= config$edad_min & dd$ages <= config$edad_max

Dxt <- dd$Dxt[filtro_edad, , drop = FALSE]
Ext <- dd$Ext[filtro_edad, , drop = FALSE]
edades <- dd$ages[filtro_edad]

cat("PASO 2.2 - Edades recortadas a", config$edad_min, "-", config$edad_max,
    "(", length(edades), "edades )\n")


# -----------------------------------------------------------------------------
# PASO 2.3 - Recortar a la ventana temporal de calibración
# -----------------------------------------------------------------------------
filtro_anio <- dd$years >= config$anio_inicio_calib &
               dd$years <= config$anio_fin_calib

Dxt <- Dxt[, filtro_anio, drop = FALSE]
Ext <- Ext[, filtro_anio, drop = FALSE]
anios <- dd$years[filtro_anio]

cat("PASO 2.3 - Años recortados a", min(anios), "-", max(anios),
    "(", length(anios), "años )\n")


# -----------------------------------------------------------------------------
# PASO 2.4 - Detectar y tratar problemas de calidad
# -----------------------------------------------------------------------------
cat("\nPASO 2.4 - Control de calidad:\n")

n_exp_cero <- sum(Ext <= 0, na.rm = TRUE)
n_def_na   <- sum(is.na(Dxt))
n_exp_na   <- sum(is.na(Ext))

cat("  Exposiciones <= 0 :", n_exp_cero, "\n")
cat("  Defunciones NA    :", n_def_na, "\n")
cat("  Exposiciones NA   :", n_exp_na, "\n")

# TRATAMIENTO: una exposición cero hace que la tasa m = D/E sea infinita.
# Sustituimos por un valor mínimo positivo. El modelo de Poisson ponderará
# esas celdas con peso casi nulo, así que no distorsiona el ajuste.
if (n_exp_cero > 0) {
  Ext[Ext <= 0] <- 1e-8
  cat("  -> Exposiciones cero sustituidas por 1e-8\n")
}

if (n_def_na > 0) {
  Dxt[is.na(Dxt)] <- 0
  cat("  -> Defunciones NA sustituidas por 0\n")
}

if (n_exp_na > 0) {
  Ext[is.na(Ext)] <- 1e-8
  cat("  -> Exposiciones NA sustituidas por 1e-8\n")
}

# Tasas de mortalidad implausibles (m > 1 significa más muertes que personas)
M_check <- Dxt / Ext
n_implausible <- sum(M_check > 1, na.rm = TRUE)
if (n_implausible > 0) {
  cat("  [AVISO]", n_implausible, "celdas con m(x,t) > 1. Revísalas.\n")
} else {
  cat("  Ninguna tasa implausible (todas m < 1). Correcto.\n")
}


# -----------------------------------------------------------------------------
# PASO 2.5 - Matriz de pesos: aquí es donde EXCLUIMOS 2020
# -----------------------------------------------------------------------------
# StMoMo no exige borrar los datos: acepta una matriz de PESOS con la misma
# forma que Dxt, donde 1 = "usa esta celda" y 0 = "ignora esta celda".
# Esto es más elegante que borrar la columna, porque conserva la estructura
# temporal y permite comparar ajustes con y sin 2020 sin rehacer los datos.

pesos <- genWeightMat(ages = edades, years = anios, clip = 0)
# clip = 0: no se recorta ninguna cohorte de los bordes (un modelo con efecto
# cohorte exigiría clip = 3).

cat("\nPASO 2.5 - Exclusión de años atípicos:\n")

for (a in config$anios_excluidos) {
  if (a %in% anios) {
    col <- which(anios == a)
    pesos[, col] <- 0
    cat("  Año", a, "EXCLUIDO del ajuste primario (peso = 0)\n")
  } else {
    cat("  Año", a, "no está en la ventana de calibración\n")
  }
}

n_celdas_usadas <- sum(pesos)
n_celdas_total  <- length(pesos)
cat("  Celdas usadas:", n_celdas_usadas, "de", n_celdas_total,
    "(", round(100 * n_celdas_usadas / n_celdas_total, 1), "% )\n")


# -----------------------------------------------------------------------------
# PASO 2.6 - Cuantificar el exceso de mortalidad de 2020
# -----------------------------------------------------------------------------
# Cifra que cuantifica la excepcionalidad de 2020 (apartado 3.4.1).

if (2020 %in% anios && 2019 %in% anios && 2021 %in% anios) {

  i19 <- which(anios == 2019)
  i20 <- which(anios == 2020)
  i21 <- which(anios == 2021)

  # Tasa estandarizada simple: defunciones totales / exposición total
  tasa_global <- colSums(Dxt) / colSums(Ext)

  esperada_2020 <- mean(c(tasa_global[i19], tasa_global[i21]))
  exceso_pct <- 100 * (tasa_global[i20] / esperada_2020 - 1)

  cat("\nPASO 2.6 - Exceso de mortalidad en 2020 (edades", config$edad_min,
      "-", config$edad_max, "):\n")
  cat("  Tasa 2019          :", round(tasa_global[i19], 5), "\n")
  cat("  Tasa 2020 OBSERVADA:", round(tasa_global[i20], 5), "\n")
  cat("  Tasa 2021          :", round(tasa_global[i21], 5), "\n")
  cat("  Exceso 2020        : +", round(exceso_pct, 1), "%\n", sep = "")
  cat("  --> Cifra que justifica la exclusion de 2020 del ajuste primario.\n")

  exceso_2020 <- list(
    tasa_2019 = unname(tasa_global[i19]),
    tasa_2020 = unname(tasa_global[i20]),
    tasa_2021 = unname(tasa_global[i21]),
    exceso_pct = unname(exceso_pct)
  )
} else {
  exceso_2020 <- NULL
  cat("\nPASO 2.6 - No se puede calcular el exceso de 2020 (faltan años).\n")
}


# -----------------------------------------------------------------------------
# PASO 2.7 - Construir el objeto StMoMoData
# -----------------------------------------------------------------------------
# StMoMoData es la "caja" estandarizada que entienden las funciones de ajuste.
#
# type = "central": nuestras exposiciones son CENTRALES (personas-año vividas).
# Es lo que da la HMD y lo que necesita Lee-Carter (que usa enlace log).
#
# El modelo CBD necesita exposiciones INICIALES (personas vivas al comienzo
# del año) porque usa enlace logit sobre q(x). La conversión se hace en la
# Fase 06 con la función central2initial().

datos_stmomo <- structure(
  list(
    Dxt    = Dxt,
    Ext    = Ext,
    ages   = edades,
    years  = anios,
    type   = "central",
    series = "total",
    label  = "Espana"
  ),
  class = "StMoMoData"
)

cat("\nPASO 2.7 - Objeto StMoMoData construido:\n")
print(datos_stmomo)


# -----------------------------------------------------------------------------
# PASO 2.8 - Guardar todo
# -----------------------------------------------------------------------------
salida <- list(
  datos      = datos_stmomo,
  pesos      = pesos,               # con 2020 excluido -> AJUSTE PRIMARIO
  pesos_todo = genWeightMat(edades, anios, clip = 0),  # todo -> SENSIBILIDAD
  edades     = edades,
  anios      = anios,
  exceso_2020 = exceso_2020,
  tipo_datos = dd$tipo
)

ruta_salida <- file.path(config$ruta_limpios, "datos_preparados.rds")
saveRDS(salida, ruta_salida)

cat("\n=====================================================\n")
cat("  SCRIPT 02 COMPLETADO\n")
cat("=====================================================\n")
cat("  Guardado en:", ruta_salida, "\n")
cat("  Ajuste primario   : sin", paste(config$anios_excluidos, collapse = ", "), "\n")
cat("  Ajuste sensibilidad: con todos los años\n")
cat("=====================================================\n\n")

if (grepl("SINT", dd$tipo)) {
  stop("Los datos cargados no son los definitivos. Ejecute el guion 01 con ",
       "los ficheros de la Human Mortality Database antes de continuar.")
}

cat("Siguiente guion: 03_exploratorio.R\n")
