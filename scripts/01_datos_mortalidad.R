# =============================================================================
# SCRIPT 01 - OBTENCIÓN DE LOS DATOS DE MORTALIDAD DE ESPAÑA
#
# Lectura de los ficheros 1x1 de la Human Mortality Database descargados
# manualmente. La descarga automática mediante hmd.mx() (paquete demography)
# no se emplea porque la HMD modificó su sistema de acceso y la función deja
# de ser fiable. El lector detecta la línea de cabecera del fichero y emite
# mensajes de diagnóstico si el formato no es el esperado.
#
# INSUMOS DE UN MODELO DE MORTALIDAD:
#   Dos matrices, ambas con edades en filas y años en columnas:
#     (a) DEFUNCIONES  D[x,t] = nº de muertes de personas de edad x en el año t
#     (b) EXPOSICIÓN   E[x,t] = nº de personas-año vividas a edad x en el año t
#   La tasa central de mortalidad es  m[x,t] = D[x,t] / E[x,t]
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(dplyr)

cat("=== SCRIPT 01: DATOS DE MORTALIDAD ===\n\n")


# =============================================================================
# FUENTE DE DATOS
# =============================================================================
# Ficheros 1x1 de la Human Mortality Database descargados manualmente.
# El generador de datos sinteticos que incluian versiones anteriores de este
# guion se ha eliminado el 10-08-2026: el trabajo opera unicamente sobre datos
# observados o sobre parametros estimados y declarados como tales.
# =============================================================================


# -----------------------------------------------------------------------------
# OBTENCIÓN DE LOS FICHEROS
# -----------------------------------------------------------------------------
# 1. Acceder a https://www.mortality.org con un usuario registrado.
# 2. Seleccionar España (Spain).
# 3. Descargar los dos ficheros de la columna 1x1:
#       Deaths            ->  Deaths_1x1.txt
#       Exposure-to-risk  ->  Exposures_1x1.txt
# 4. Copiarlos a ~/TFM_Pensiones/datos/crudos/ con los nombres
#       defunciones.txt  y  exposicion.txt
#
# ESTRUCTURA DE UN FICHERO DE LA HMD:
#
#   Spain, Death counts (period 1x1), Last modified: ...
#
#     Year          Age             Female            Male           Total
#     1908           0             12345.20         13456.70        25801.90
#     1908           1              2345.10          2456.70         4801.80
#     ...
#
# La primera línea es un título, sigue una línea en blanco y después la
# cabecera. El lector localiza la línea que contiene "Year" y "Age", de modo
# que el número de líneas de preámbulo es irrelevante.
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# FUNCIÓN AUXILIAR: lector de ficheros HMD
# -----------------------------------------------------------------------------
leer_hmd <- function(ruta, nombre_amigable) {

  cat("  Leyendo", nombre_amigable, "...\n")

  if (!file.exists(ruta)) {
    stop("\n\nNo se encuentra el fichero:\n  ", ruta,
         "\n\nComprobaciones:\n",
         "  (a) descargado de mortality.org (columna 1x1)\n",
         "  (b) situado en la carpeta datos/crudos\n",
         "  (c) nombre exacto, en minusculas, sin extension duplicada\n")
  }

  # Leemos las primeras lineas como texto plano para localizar la cabecera
  lineas <- readLines(ruta, n = 20, warn = FALSE)

  fila_cabecera <- which(
    grepl("Year", lineas, fixed = TRUE) &
    grepl("Age",  lineas, fixed = TRUE)
  )[1]

  if (is.na(fila_cabecera)) {
    cat("\n  PRIMERAS LINEAS DEL ARCHIVO:\n")
    cat(paste0("    ", head(lineas, 8), collapse = "\n"), "\n\n")
    stop("\nNo se encuentra la cabecera con 'Year' y 'Age' en este fichero.\n",
         "Probablemente se ha guardado una pagina web en lugar del fichero de\n",
         "datos. Debe descargarse desde el enlace directo de la columna 1x1\n",
         "tras iniciar sesion en mortality.org.\n")
  }

  cat("    Cabecera detectada en la linea", fila_cabecera, "\n")

  df <- read.table(ruta,
                   header = TRUE,
                   skip = fila_cabecera - 1,
                   na.strings = ".",
                   stringsAsFactors = FALSE)

  cat("    Filas leidas:", format(nrow(df), big.mark = "."), "\n")
  cat("    Columnas    :", paste(names(df), collapse = ", "), "\n")

  if (!all(c("Year", "Age") %in% names(df))) {
    stop("\nEl archivo no tiene las columnas Year y Age. Revisa la descarga.\n")
  }
  if (!"Total" %in% names(df)) {
    stop("\nEl fichero no tiene columna Total. Debe emplearse el fichero 1x1\n",
         "completo, no una version separada por sexo.\n")
  }

  df
}


# -----------------------------------------------------------------------------
# LECTURA DE LOS FICHEROS
# -----------------------------------------------------------------------------
{

  archivo_def <- file.path(config$ruta_crudos, "defunciones.txt")
  archivo_exp <- file.path(config$ruta_crudos, "exposicion.txt")

  cat("Carpeta de datos crudos:\n  ", config$ruta_crudos, "\n\n")

  # Contenido de la carpeta, a efectos de diagnostico
  archivos_hay <- list.files(config$ruta_crudos)
  if (length(archivos_hay) == 0) {
    cat("  [AVISO] La carpeta esta vacia. Deben copiarse en ella los dos ficheros.\n\n")
  } else {
    cat("  Archivos encontrados en la carpeta:\n")
    cat(paste0("    - ", archivos_hay, collapse = "\n"), "\n\n")
  }

  def <- leer_hmd(archivo_def, "defunciones")
  cat("\n")
  exp <- leer_hmd(archivo_exp, "exposicion")
  cat("\n")

  # --- Limpiar la columna de edad -------------------------------------------
  # Viene como texto porque la ultima categoria es "110+"
  limpiar_edad <- function(x) {
    as.integer(gsub("\\+", "", as.character(x)))
  }

  def$Age <- limpiar_edad(def$Age)
  exp$Age <- limpiar_edad(exp$Age)

  # --- Comprobar que ambos archivos cubren el mismo periodo ------------------
  anios_comunes  <- intersect(unique(def$Year), unique(exp$Year))
  edades_comunes <- intersect(unique(def$Age),  unique(exp$Age))

  cat("  Anios en defunciones :", min(def$Year), "-", max(def$Year), "\n")
  cat("  Anios en exposicion  :", min(exp$Year), "-", max(exp$Year), "\n")
  cat("  Anios comunes        :", min(anios_comunes), "-", max(anios_comunes),
      "(", length(anios_comunes), "anios )\n")
  cat("  Edades comunes       :", min(edades_comunes), "-", max(edades_comunes),
      "(", length(edades_comunes), "edades )\n\n")

  if (length(anios_comunes) < 20) {
    cat("  [AVISO] Muy pocos anios comunes. Ambos ficheros deben ser de Espana\n")
    cat("          y de la misma version de la base de datos.\n\n")
  }

  # Nos quedamos solo con lo comun a ambos
  def <- def %>% filter(Year %in% anios_comunes, Age %in% edades_comunes)
  exp <- exp %>% filter(Year %in% anios_comunes, Age %in% edades_comunes)

  # --- Pasar de formato largo a matriz edad x anio ---------------------------
  a_matriz <- function(df, col) {
    m <- tapply(df[[col]], list(df$Age, df$Year), function(v) v[1])
    m[is.na(m)] <- 0
    # Ordenar filas y columnas numericamente, no alfabeticamente
    m <- m[order(as.numeric(rownames(m))), order(as.numeric(colnames(m)))]
    m
  }

  D_total <- a_matriz(def, "Total")
  E_total <- a_matriz(exp, "Total")

  if (!all(dim(D_total) == dim(E_total))) {
    stop("\nLas matrices de defunciones y exposicion no coinciden en tamano.\n",
         "Defunciones: ", paste(dim(D_total), collapse = " x "), "\n",
         "Exposicion : ", paste(dim(E_total), collapse = " x "), "\n")
  }

  datos_crudos <- list(
    Dxt   = D_total,
    Ext   = E_total,
    ages  = as.integer(rownames(D_total)),
    years = as.integer(colnames(D_total)),
    tipo  = "Human Mortality Database - Espana - datos reales"
  )

  saveRDS(datos_crudos, file.path(config$ruta_crudos, "datos_crudos.rds"))
  cat("Guardado en:", file.path(config$ruta_crudos, "datos_crudos.rds"), "\n")
}


ruta_datos <- file.path(config$ruta_crudos, "datos_crudos.rds")

if (file.exists(ruta_datos)) {

  dd <- readRDS(ruta_datos)

  cat("\n--- RESUMEN DE LOS DATOS CARGADOS ---\n")
  cat("Tipo de datos  :", dd$tipo, "\n")
  cat("Edades         :", min(dd$ages), "-", max(dd$ages),
      "(", length(dd$ages), "edades )\n")
  cat("Anios          :", min(dd$years), "-", max(dd$years),
      "(", length(dd$years), "anios )\n")
  cat("Defunciones tot:", format(round(sum(dd$Dxt)), big.mark = "."), "\n")

  if (80 %in% dd$ages) {
    i80 <- which(dd$ages == 80)
    m_ini <- dd$Dxt[i80, 1] / dd$Ext[i80, 1]
    m_fin <- dd$Dxt[i80, ncol(dd$Dxt)] / dd$Ext[i80, ncol(dd$Ext)]
    cat("m(80,", dd$years[1], ") =", round(m_ini, 5), "\n")
    cat("m(80,", dd$years[length(dd$years)], ") =", round(m_fin, 5), "\n")
    cat("Reduccion      :", round(100 * (1 - m_fin / m_ini), 1), "%\n")
  }

  # --- Avisos de calidad ---
  if (any(dd$Ext <= 0)) {
    cat("\n[AVISO] Hay exposiciones cero o negativas. Se tratan en el script 02.\n")
  }
  if (any(dd$Dxt < 0, na.rm = TRUE)) {
    cat("\n[AVISO] Hay defunciones negativas. Revisa la fuente.\n")
  }

  # --- Control de plausibilidad ---
  {
    if (65 %in% dd$ages && max(dd$years) >= 2015) {
      i65 <- which(dd$ages == 65)
      jfin <- ncol(dd$Dxt)
      m65 <- dd$Dxt[i65, jfin] / dd$Ext[i65, jfin]
      cat("\nCONTROL DE PLAUSIBILIDAD:\n")
      cat("  m(65,", dd$years[jfin], ") =", round(m65, 5), "\n")
      if (m65 > 0.003 && m65 < 0.020) {
        cat("  Valor dentro del rango esperado para Espana. Correcto.\n")
      } else {
        cat("  [AVISO] Valor fuera del rango habitual (0,003 - 0,020).\n")
        cat("          Revisar que los ficheros descargados son los correctos.\n")
      }
    }
    cat("\n*** DATOS REALES DE LA HMD CARGADOS CORRECTAMENTE ***\n")
  }

  cat("\nSiguiente guion: 02_limpieza_preparacion.R\n")

} else {
  stop("No se ha generado ningun fichero de datos. Deben existir defunciones.txt y\n",
       "exposicion.txt en ", config$ruta_crudos)
}
