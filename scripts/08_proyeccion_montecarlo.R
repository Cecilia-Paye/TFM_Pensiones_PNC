# =============================================================================
# SCRIPT 08 - PROYECCIÓN ESTOCÁSTICA Y SIMULACIÓN DE MONTE CARLO
#
# PROCEDIMIENTO:
#
#   1. El índice temporal k(t) estimado en el guion 04 se modeliza como un
#      paseo aleatorio con deriva (Lee y Carter, 1992):
#           k(t) = k(t-1) + mu + e(t),     e(t) ~ N(0, sigma²)
#      donde mu es la deriva (mejora media anual) y e(t) la innovación.
#   2. Se generan 5.000 trayectorias futuras de k(t), cada una con su propia
#      secuencia de innovaciones.
#   3. Cada trayectoria genera una superficie de mortalidad completa y, a
#      través de los guiones 09 a 11, una senda de gasto.
#   4. La distribución de los resultados permite cuantificar el riesgo de
#      longevidad mediante percentiles y medidas de cola.
#
# FUENTES DE INCERTIDUMBRE (apartado 3.4.4 de la memoria):
#   La simulación básica de StMoMo recoge la incertidumbre del proceso. El
#   paso 8.4 añade la de los parámetros de edad (bootstrap semiparamétrico) y
#   el paso 8.4ter la de la deriva y la volatilidad, muestreadas de sus
#   distribuciones muestrales. Omitir estas últimas produce intervalos
#   demasiado estrechos (Lee y Carter, 1992).
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(StMoMo)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)

cat("=== SCRIPT 08: PROYECCIÓN Y MONTE CARLO ===\n\n")

prep <- readRDS(file.path(config$ruta_limpios, "datos_preparados.rds"))
lc   <- readRDS(file.path(config$ruta_resultados, "modelo_lee_carter.rds"))
cbd  <- readRDS(file.path(config$ruta_resultados, "modelo_cbd.rds"))
comp <- readRDS(file.path(config$ruta_resultados, "comparacion_modelos.rds"))

H <- config$anio_fin_proy - config$anio_fin_calib   # horizonte en años
anios_proy <- (config$anio_fin_calib + 1):config$anio_fin_proy

cat("Horizonte de proyección:", H, "años (",
    min(anios_proy), "-", max(anios_proy), ")\n")
cat("Simulaciones por modelo:", format(config$n_simulaciones, big.mark = "."), "\n\n")

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
# PASO 8.1 - Justificar el modelo de series temporales para k(t)
# -----------------------------------------------------------------------------
# Se contrasta la especificación de paseo aleatorio con deriva con la que
# sugiere la selección automática por criterios de información.

# -----------------------------------------------------------------------------
# TRATAMIENTO DEL ANIO EXCLUIDO EN LA SERIE k(t)
# -----------------------------------------------------------------------------
# El ajuste excluye 2020 mediante la matriz de pesos, de modo que k(2020) no
# queda identificado por dato alguno: StMoMo lo devuelve como valor ausente
# (NA). Sin tratamiento, forecast() y simulate() no pueden ajustar el proceso
# de k(t) sobre una serie con huecos, o lo hacen omitiendo el anio.
#
# Ademas, si se elimina 2020 de la serie sin mas, la diferencia 2019-2021 pasa
# a tratarse como si fuera de un anio cuando abarca dos, lo que sesga al alza
# la deriva estimada.
#
# Ambos problemas se resuelven sustituyendo k(2020) por la interpolacion lineal
# entre 2019 y 2021: la serie queda regular y el valor no identificado
# desaparece. El criterio se declara en el apartado 3.6 de la memoria.

anio_int <- config$anios_excluidos
for (a in anio_int) {
  i <- match(a, lc$kt_df$anio)
  if (is.na(i) || i == 1 || i == nrow(lc$kt_df)) next
  kt_previo <- lc$kt_df$kt[i]
  kt_nuevo  <- (lc$kt_df$kt[i - 1] + lc$kt_df$kt[i + 1]) / 2
  cat("  k(", a, ") ajustado por interpolacion lineal:\n", sep = "")
  cat("    valor devuelto por el ajuste :", round(kt_previo, 4), "\n")
  cat("    valor interpolado            :", round(kt_nuevo, 4), "\n")
  cat("    diferencia                   :", round(kt_nuevo - kt_previo, 4), "\n")
  lc$kt_df$kt[i]    <- kt_nuevo
  lc$fit$kt[1, i]   <- kt_nuevo
}

# Serie completa y regular, ya con 2020 interpolado
kt_serie <- lc$kt_df$kt

cat("PASO 8.1 - Selección del modelo de serie temporal para k(t):\n")

ajuste_auto <- auto.arima(kt_serie, max.p = 3, max.q = 3, max.d = 2,
                          seasonal = FALSE, stepwise = FALSE,
                          approximation = FALSE)

cat("  auto.arima() sugiere:", paste(arimaorder(ajuste_auto), collapse = ","), "\n")
print(ajuste_auto)

orden <- arimaorder(ajuste_auto)
if (all(orden == c(0, 1, 0))) {
  cat("\n  --> ARIMA(0,1,0) con deriva: coincide con la especificacion adoptada.\n")
} else {
  cat("\n  --> auto.arima sugiere una especificacion distinta.\n")
  cat("      Se mantiene el paseo aleatorio con deriva porque (a) es la\n")
  cat("      especificacion de Lee y Carter (1992) y la estandar, lo que\n")
  cat("      preserva la comparabilidad; (b) especificaciones mas complejas\n")
  cat("      ajustadas sobre pocas observaciones tienden a proyectar peor a\n")
  cat("      largo plazo; (c) el efecto se cuantifica en el paso 8.4bis y se\n")
  cat("      declara en el apartado 5.4.1 de la memoria.\n")
}

# Estimación explícita de la deriva
d_kt <- diff(kt_serie)
mu    <- mean(d_kt)
sigma <- sd(d_kt)

cat("\n  Deriva estimada (mu)      :", round(mu, 4), "por año\n")
cat("  Volatilidad (sigma)       :", round(sigma, 4), "\n")
cat("  Error típico de mu        :", round(sigma / sqrt(length(d_kt)), 4), "\n")
cat("  IC 95% de mu              : [",
    round(mu - 1.96 * sigma / sqrt(length(d_kt)), 4), ",",
    round(mu + 1.96 * sigma / sqrt(length(d_kt)), 4), "]\n\n")


# -----------------------------------------------------------------------------
# PASO 8.2 - Proyección CENTRAL (determinista) de Lee-Carter
# -----------------------------------------------------------------------------
cat("PASO 8.2 - Proyección central Lee-Carter...\n")

set.seed(config$semilla)

lc_fore <- forecast(lc$fit, h = H, kt.method = "iarima",
                    kt.order = c(0, 1, 0))

m_lc_central <- lc_fore$rates   # matriz edades x años proyectados

cat("  Proyección central obtenida:", nrow(m_lc_central), "edades x",
    ncol(m_lc_central), "años\n\n")


# -----------------------------------------------------------------------------
# PASO 8.3 - SIMULACIÓN DE MONTE CARLO (Lee-Carter)
# -----------------------------------------------------------------------------
# simulate() genera nsim trayectorias completas del proceso de k(t)
# manteniendo a(x) y b(x) fijos en sus valores estimados.

cat("PASO 8.3 - Simulación de Monte Carlo, Lee-Carter...\n")

set.seed(config$semilla)
t0 <- Sys.time()

lc_sim <- simulate(lc$fit, nsim = config$n_simulaciones, h = H,
                   kt.method = "iarima", kt.order = c(0, 1, 0))

cat("  Completado en",
    round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), "minutos\n")

# lc_sim$rates es un array de dimensión [edades, años, simulaciones]
dim_sim <- dim(lc_sim$rates)
cat("  Dimensión del array:", paste(dim_sim, collapse = " x "), "\n\n")


# -----------------------------------------------------------------------------
# PASO 8.4 - Simulación con incertidumbre de parámetros (bootstrap)
# -----------------------------------------------------------------------------
# bootstrap() remuestrea las defunciones para generar conjuntos de
# parámetros alternativos, sobre los que después se simula, de modo que los
# intervalos incorporen también el error de estimación de a(x) y b(x).
# Si HACER_BOOTSTRAP fuera FALSE, los intervalos recogerían únicamente la
# incertidumbre del proceso y de la deriva.

HACER_BOOTSTRAP <- TRUE
N_BOOT <- 500                # número de réplicas bootstrap

lc_boot_sim <- NULL

if (HACER_BOOTSTRAP) {
  cat("PASO 8.4 - Bootstrap semiparamétrico...\n")

  set.seed(config$semilla)
  t0 <- Sys.time()

  lc_boot <- bootstrap(lc$fit, nBoot = N_BOOT, type = "semiparametric")

  lc_boot_sim <- simulate(lc_boot, nsim = ceiling(config$n_simulaciones / N_BOOT),
                          h = H, kt.method = "iarima", kt.order = c(0, 1, 0))

  cat("  Completado en",
      round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), "minutos\n\n")
} else {
  cat("PASO 8.4 - Bootstrap omitido: los intervalos no recogen la\n")
  cat("  incertidumbre de los parametros de edad.\n\n")
}



# -----------------------------------------------------------------------------
# PASO 8.4ter - SIMULACION CON INCERTIDUMBRE COMPLETA
# -----------------------------------------------------------------------------
# Las llamadas a simulate() de los pasos 8.3 y 8.4 fijan la deriva de k(t) en
# su valor estimado, de modo que las trayectorias recogen la variabilidad del
# proceso pero no el hecho de que esa deriva se estima sobre poco mas de
# treinta observaciones anuales. El error tipico de la deriva es del orden de
# la tercera parte de su valor, asi que esa fuente no es despreciable: sobre un
# horizonte de veintiseis anios su contribucion a la varianza de k(T+h) es del
# mismo orden que la del proceso.
#
# Lee y Carter (1992) ya senalaron que los intervalos que ignoran la
# incertidumbre de la deriva son demasiado estrechos. Se incorporan aqui tres
# fuentes:
#
#   (1) Parametros de edad a(x) y b(x), y nivel final de k(t), por el
#       bootstrap semiparametrico del paso 8.4.
#   (2) Deriva y volatilidad del paseo aleatorio, por sus distribuciones
#       muestrales:
#           sigma^2 ~ (n-1) s^2 / chi^2_(n-1)
#           mu | sigma ~ N( mu_gorro , sigma^2 / n )
#   (3) Innovaciones del propio proceso, N(0, sigma^2).
#
# La trayectoria simulada es:
#   k(T+h) = k(T) + suma_{i=1..h} ( mu_s + eps_i ),  eps_i ~ N(0, sigma_s^2)
#   m(x, T+h) = exp( a_s(x) + b_s(x) * k(T+h) )

cat("PASO 8.4ter - Simulacion con incertidumbre completa...\n")

set.seed(config$semilla)

n_d      <- length(d_kt)                 # numero de diferencias de k(t)
kt_final <- lc$kt_df$kt[nrow(lc$kt_df)]  # ultimo k(t) ajustado
NSIM     <- config$n_simulaciones

# Parametros de edad procedentes del bootstrap, si esta disponible
usar_boot <- HACER_BOOTSTRAP && !is.null(lc_boot) &&
             !is.null(lc_boot$bootParameters) && length(lc_boot$bootParameters) > 0

if (usar_boot) {
  nB <- length(lc_boot$bootParameters)
  cat("  Parametros de edad: ", nB, " replicas bootstrap.\n", sep = "")
} else {
  nB <- 1
  cat("  Parametros de edad: estimacion puntual (sin bootstrap).\n")
}

extraer_par <- function(b) {
  if (!usar_boot) {
    return(list(ax = as.numeric(lc$fit$ax),
                bx = as.numeric(lc$fit$bx),
                ktT = kt_final))
  }
  p  <- lc_boot$bootParameters[[b]]
  kt <- as.numeric(p$kt)
  kt <- kt[!is.na(kt)]
  list(ax  = as.numeric(p$ax),
       bx  = as.numeric(p$bx),
       ktT = kt[length(kt)])
}

n_edades  <- length(prep$edades)
rates_full <- array(NA_real_, dim = c(n_edades, H, NSIM),
                    dimnames = list(prep$edades, anios_proy, NULL))
kt_full    <- matrix(NA_real_, nrow = H, ncol = NSIM)
mu_draw    <- numeric(NSIM)

t0 <- Sys.time()
for (s in seq_len(NSIM)) {
  par_s <- extraer_par(((s - 1) %% nB) + 1)

  # (2) deriva y volatilidad muestreadas de su distribucion
  sigma_s <- sqrt((n_d - 1) * sigma^2 / rchisq(1, n_d - 1))
  mu_s    <- rnorm(1, mu, sigma_s / sqrt(n_d))
  mu_draw[s] <- mu_s

  # (3) innovaciones del proceso
  kt_s <- par_s$ktT + cumsum(mu_s + rnorm(H, 0, sigma_s))
  kt_full[, s] <- kt_s

  rates_full[, , s] <- exp(outer(par_s$ax, rep(1, H)) + outer(par_s$bx, kt_s))

  if (s %% 1000 == 0) cat("    ", s, "de", NSIM, "\n")
}
cat("  Completado en",
    round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "segundos\n")

# Descomposicion de la varianza de k(T+h) al final del horizonte
var_proceso <- H * sigma^2
var_deriva  <- H^2 * sigma^2 / n_d
cat("\n  Descomposicion de la varianza de k(", max(anios_proy), "):\n", sep = "")
cat("    proceso :", sprintf("%8.2f", var_proceso),
    sprintf(" (%.0f %%)", 100 * var_proceso / (var_proceso + var_deriva)), "\n")
cat("    deriva  :", sprintf("%8.2f", var_deriva),
    sprintf(" (%.0f %%)", 100 * var_deriva / (var_proceso + var_deriva)), "\n")
cat("    dispersion empirica de la deriva simulada:",
    round(sd(mu_draw), 4), "frente a error tipico teorico",
    round(sigma / sqrt(n_d), 4), "\n\n")

# Comparacion de amplitudes del intervalo
i65c <- match(65, prep$edades)
if (!is.na(i65c)) {
  amp <- function(x) {
    q <- quantile(x, c(0.025, 0.5, 0.975))
    as.numeric((q[3] - q[1]) / q[2] * 100)
  }
  a1 <- amp(lc_sim$rates[i65c, H, ])
  a3 <- amp(rates_full[i65c, H, ])
  cat("  Amplitud del IC95 de m(65,", max(anios_proy), "):\n", sep = "")
  cat("    solo proceso                    :", sprintf("%.1f %%", a1), "\n")
  if (HACER_BOOTSTRAP && !is.null(lc_boot_sim)) {
    cat("    proceso mas parametros de edad  :",
        sprintf("%.1f %%", amp(lc_boot_sim$rates[i65c, H, ])), "\n")
  }
  cat("    incertidumbre completa          :", sprintf("%.1f %%", a3), "\n")
  cat("    ensanchamiento sobre solo proceso:",
      sprintf("%+.1f %%", (a3 / a1 - 1) * 100), "\n\n")
}

# A partir de aqui, el array de referencia del guion es el de incertidumbre
# completa. Se conservan los otros dos para la comparacion de la memoria.
lc_sim_solo_proceso <- lc_sim$rates
lc_sim <- list(rates = rates_full,
               kt.s  = list(sim = array(kt_full, dim = c(1, H, NSIM))))


# -----------------------------------------------------------------------------
# PASO 8.4bis - ROBUSTEZ: especificacion alternativa para k(t)
# -----------------------------------------------------------------------------
# El escenario base extrapola k(t) mediante un paseo aleatorio con deriva,
# que es la especificacion propuesta por Lee y Carter (1992) y la habitual en
# la practica actuarial: garantiza proyecciones coherentes a largo plazo y
# evita que un termino de media movil ajustado sobre poco mas de treinta
# observaciones domine un horizonte de veintiseis anios.
#
# auto.arima() sugiere una especificacion distinta, de modo que la eleccion no
# queda avalada por el criterio automatico y debe cuantificarse. Se proyecta
# aqui la especificacion sugerida y se compara la esperanza de vida a los 65
# anios al final del horizonte. La diferencia se reporta en el analisis de
# robustez de la memoria.

cat("PASO 8.4bis - Robustez de la especificacion de k(t)...\n")

# Tabla de vida de periodo desde los 65 anios, identica a la del guion 03.
# Se define aqui porque la usan los pasos 8.4bis y 8.7.
calcular_e65 <- function(m_col, edades) {
  idx <- which(edades >= 65)
  if (length(idx) == 0) return(NA_real_)
  m <- m_col[idx]
  q <- 1 - exp(-m); q[length(q)] <- 1
  l <- c(1, cumprod(1 - q))[1:length(q)]
  sum(l * (1 - q / 2)) / l[1]
}

orden_alt <- orden
if (identical(as.integer(orden_alt), c(0L, 1L, 0L))) {
  cat("  auto.arima coincide con el paseo aleatorio con deriva.\n")
  cat("  No procede contraste alternativo.\n\n")
  robustez_kt <- NULL
} else {
  set.seed(config$semilla)
  lc_fore_alt <- forecast(lc$fit, h = H, kt.method = "iarima",
                          kt.order = as.integer(orden_alt))

  # Se emplea la misma tabla de vida de periodo que en el paso 8.7 y en el
  # guion 03 (calcular_e65), de modo que las esperanzas de vida del trabajo
  # sean comparables entre si.
  e65 <- function(m) calcular_e65(m[, ncol(m)], prep$edades)
  e_base <- e65(m_lc_central)
  e_alt  <- e65(lc_fore_alt$rates)

  cat("  Especificacion base : ARIMA(0,1,0) con deriva\n")
  cat("  Especificacion alt. : ARIMA(", paste(orden_alt, collapse = ","), ")\n", sep = "")
  cat("  e(65) en", max(anios_proy), "- base:", round(e_base, 2), "anios\n")
  cat("  e(65) en", max(anios_proy), "- alt. :", round(e_alt, 2), "anios\n")
  cat("  Diferencia:", sprintf("%+.2f anios", e_alt - e_base), "\n\n")

  robustez_kt <- list(orden_base = c(0, 1, 0), orden_alt = orden_alt,
                      e65_base = e_base, e65_alt = e_alt,
                      dif = e_alt - e_base,
                      tasas_alt = lc_fore_alt$rates)
}


# -----------------------------------------------------------------------------
# PASO 8.5 - Simulación del modelo CBD (contraste)
# -----------------------------------------------------------------------------
# En CBD hay DOS series temporales, k1 y k2, que están correlacionadas.
# Se proyectan conjuntamente con un paseo aleatorio multivariante con deriva.
# StMoMo lo hace por defecto con kt.method = "mrwd".

cat("PASO 8.5 - Simulación de Monte Carlo, CBD...\n")

set.seed(config$semilla)

cbd_sim <- simulate(cbd$fit, nsim = config$n_simulaciones, h = H,
                    kt.method = "mrwd")

# Los "rates" del CBD son q(x,t). Los pasamos a m(x,t) para poder comparar:
#   m = -log(1 - q)
m_cbd_sim <- -log(1 - cbd_sim$rates)
m_cbd_sim[!is.finite(m_cbd_sim)] <- NA

cat("  Completado.\n\n")


# -----------------------------------------------------------------------------
# PASO 8.6 - Extraer percentiles de las simulaciones
# -----------------------------------------------------------------------------
# Percentiles de las N simulaciones para cada edad y año; alimentan los
# gráficos de abanico y las tablas de la memoria.

percentiles <- c(0.025, 0.05, 0.25, 0.50, 0.75, 0.95, 0.975)

extraer_pct <- function(array_sim, edades, anios, etiqueta) {
  res <- list()
  for (p in percentiles) {
    mat <- apply(array_sim, c(1, 2), quantile, probs = p, na.rm = TRUE)
    res[[paste0("p", p * 1000)]] <- mat
  }
  res$etiqueta <- etiqueta
  res$edades <- edades
  res$anios <- anios
  res
}

pct_lc  <- extraer_pct(lc_sim$rates,  prep$edades, anios_proy, "Lee-Carter")
pct_cbd <- extraer_pct(m_cbd_sim,     prep$edades, anios_proy, "CBD")

cat("PASO 8.6 - Percentiles extraídos.\n")

# Ejemplo ilustrativo: mortalidad a los 80 años al final del horizonte
if (80 %in% prep$edades) {
  i80 <- which(prep$edades == 80)
  jfin <- length(anios_proy)

  cat("\n  m(80,", max(anios_proy), ") según Lee-Carter:\n", sep = "")
  cat("    Percentil  2,5%:", round(pct_lc$p25[i80, jfin], 5), "\n")
  cat("    Percentil   50%:", round(pct_lc$p500[i80, jfin], 5), "\n")
  cat("    Percentil 97,5%:", round(pct_lc$p975[i80, jfin], 5), "\n")

  amplitud <- pct_lc$p975[i80, jfin] / pct_lc$p25[i80, jfin]
  cat("    Amplitud del IC 95%:", round(amplitud, 2), "veces\n")
  cat("    INTERPRETACIÓN: cuanto mayor sea, más incierta es la proyección\n")
  cat("    y más riesgo de longevidad soporta el sistema.\n\n")
}


# -----------------------------------------------------------------------------
# FIGURA 12 - Fan chart de k(t)
# -----------------------------------------------------------------------------
kt_sim <- lc_sim$kt.s$sim      # array [1, años, simulaciones]
kt_mat <- kt_sim[1, , ]

kt_pct <- data.frame(
  anio = anios_proy,
  p025 = apply(kt_mat, 1, quantile, 0.025),
  p05  = apply(kt_mat, 1, quantile, 0.05),
  p25  = apply(kt_mat, 1, quantile, 0.25),
  p50  = apply(kt_mat, 1, quantile, 0.50),
  p75  = apply(kt_mat, 1, quantile, 0.75),
  p95  = apply(kt_mat, 1, quantile, 0.95),
  p975 = apply(kt_mat, 1, quantile, 0.975)
)

# Se representa la serie con k(2020) interpolado (lc$kt_df), que es la que
# alimenta la proyeccion.
kt_hist <- data.frame(anio = lc$kt_df$anio, kt = lc$kt_df$kt)

fig12 <- ggplot() +
  geom_ribbon(data = kt_pct, aes(anio, ymin = p025, ymax = p975),
              fill = "#4575B4", alpha = 0.18) +
  geom_ribbon(data = kt_pct, aes(anio, ymin = p05, ymax = p95),
              fill = "#4575B4", alpha = 0.25) +
  geom_ribbon(data = kt_pct, aes(anio, ymin = p25, ymax = p75),
              fill = "#4575B4", alpha = 0.35) +
  geom_line(data = kt_pct, aes(anio, p50), colour = "#08519C", linewidth = 0.9) +
  geom_line(data = kt_hist, aes(anio, kt), colour = "black", linewidth = 0.8) +
  geom_vline(xintercept = config$anio_fin_calib + 0.5,
             linetype = "dashed", colour = "grey40") +
  annotate("text", x = config$anio_fin_calib + 0.7,
           y = max(kt_hist$kt), label = " Proyecci\u00f3n",
           hjust = 0, size = 3, colour = "grey30") +
  labs(title = "Proyecci\u00f3n estoc\u00e1stica del \u00edndice de mortalidad k(t)",
       subtitle = paste0("Bandas: intervalos al 50%, 90% y 95%. ",
                         format(config$n_simulaciones, big.mark = "."),
                         " simulaciones"),
       x = "A\u00f1o", y = expression(k(t)), caption = nota_fuente) +
  tema_tfm

ggsave(file.path(config$ruta_graficos, "fig12_fanchart_kt.png"),
       fig12, width = 7.5, height = 5, dpi = 300)
cat("FIGURA 12 guardada: fig12_fanchart_kt.png\n")


# -----------------------------------------------------------------------------
# FIGURA 13 - Fan chart de la mortalidad a edades clave
# -----------------------------------------------------------------------------
edades_fig <- intersect(c(70, 80, 90), prep$edades)

df13 <- do.call(rbind, lapply(edades_fig, function(e) {
  i <- which(prep$edades == e)
  hist <- data.frame(
    edad = e, anio = prep$anios,
    p50 = (prep$datos$Dxt / prep$datos$Ext)[i, ],
    p025 = NA, p975 = NA, tipo = "Observado"
  )
  proy <- data.frame(
    edad = e, anio = anios_proy,
    p50  = pct_lc$p500[i, ],
    p025 = pct_lc$p25[i, ],
    p975 = pct_lc$p975[i, ],
    tipo = "Proyectado"
  )
  rbind(hist, proy)
}))

fig13 <- ggplot(df13, aes(anio)) +
  geom_ribbon(aes(ymin = p025, ymax = p975), fill = "#4575B4", alpha = 0.25) +
  geom_line(aes(y = p50, colour = tipo), linewidth = 0.85) +
  facet_wrap(~ paste("Edad", edad), scales = "free_y") +
  scale_colour_manual(values = c("Observado" = "black",
                                 "Proyectado" = "#08519C"), name = NULL) +
  scale_y_log10() +
  labs(title = "Proyecci\u00f3n estoc\u00e1stica de la mortalidad por edad",
       subtitle = "Banda: intervalo de confianza al 95%. Escala logar\u00edtmica",
       x = "A\u00f1o", y = "m(x,t)", caption = nota_fuente) +
  tema_tfm + theme(legend.position = "bottom")

ggsave(file.path(config$ruta_graficos, "fig13_fanchart_mortalidad.png"),
       fig13, width = 8.5, height = 4.2, dpi = 300)
cat("FIGURA 13 guardada: fig13_fanchart_mortalidad.png\n\n")


# -----------------------------------------------------------------------------
# PASO 8.7 - Esperanza de vida proyectada a los 65 años, con incertidumbre
# -----------------------------------------------------------------------------
# Indicador que conecta el modelo de mortalidad con el coste de las
# prestaciones vitalicias.

cat("PASO 8.7 - Esperanza de vida a los 65 proyectada...\n")
cat("  (tabla de vida de periodo, funcion calcular_e65 definida en 8.4bis)\n")

n_usar <- min(1000, dim(lc_sim$rates)[3])   # submuestra de 1.000 trayectorias
idx_sim <- sample(dim(lc_sim$rates)[3], n_usar)

e65_sim <- matrix(NA, length(anios_proy), n_usar)
for (s in seq_len(n_usar)) {
  for (j in seq_along(anios_proy)) {
    e65_sim[j, s] <- calcular_e65(lc_sim$rates[, j, idx_sim[s]], prep$edades)
  }
}

e65_proy <- data.frame(
  anio = anios_proy,
  p025 = apply(e65_sim, 1, quantile, 0.025, na.rm = TRUE),
  p50  = apply(e65_sim, 1, quantile, 0.50,  na.rm = TRUE),
  p975 = apply(e65_sim, 1, quantile, 0.975, na.rm = TRUE)
)

cat("\n  Esperanza de vida a los 65 años:\n")
print(e65_proy %>%
        filter(anio %in% c(min(anio),
                           round(mean(range(anio))),
                           max(anio))) %>%
        mutate(across(-anio, ~round(., 2))),
      row.names = FALSE)

ganancia <- e65_proy$p50[nrow(e65_proy)] - e65_proy$p50[1]
cat("\n  Ganancia mediana en el horizonte:", round(ganancia, 2), "años\n")
cat("  Rango de incertidumbre al final  : [",
    round(e65_proy$p025[nrow(e65_proy)], 2), ",",
    round(e65_proy$p975[nrow(e65_proy)], 2), "]\n")
cat("  Cada año adicional de esperanza de vida a los 65 supone, en\n")
cat("  terminos aproximados, un año más de prestación por beneficiario;\n")
cat("  el efecto sobre el gasto agregado se cuantifica en el guion 11.\n\n")


# -----------------------------------------------------------------------------
# PASO 8.8 - Guardar (ficheros de gran tamaño)
# -----------------------------------------------------------------------------
cat("PASO 8.8 - Guardando resultados...\n")

# Se guardan las tasas con incertidumbre completa (proceso, parametros de
# edad, deriva y volatilidad), que son las que emplean los guiones 09 a 11.

lc_rates_final <- lc_sim$rates
cat("  Tasas guardadas con incertidumbre completa:\n")
cat("    parametros de edad, deriva, volatilidad y proceso.\n")

cat("  Dimension de las tasas guardadas:",
    paste(dim(lc_rates_final), collapse = " x "), "\n")

if (dim(lc_rates_final)[3] != config$n_simulaciones) {
  warning("Trayectorias obtenidas: ", dim(lc_rates_final)[3],
          ". Esperadas: ", config$n_simulaciones,
          ". Ajuste N_BOOT o nsim para que el producto coincida.")
}


# Las simulaciones completas se guardan por separado por su tamaño
saveRDS(
  list(lc_rates = lc_rates_final, cbd_rates = m_cbd_sim,
       kt_sim = kt_mat, anios_proy = anios_proy, edades = prep$edades),
  file.path(config$ruta_resultados, "simulaciones_completas.rds")
)

saveRDS(
  list(
    horizonte  = H,
    anios_proy = anios_proy,
    edades     = prep$edades,
    mu = mu, sigma = sigma,
    arima_sugerido = orden,
    m_lc_central = m_lc_central,
    pct_lc  = pct_lc,
    pct_cbd = pct_cbd,
    kt_pct  = kt_pct,
    e65_proy = e65_proy,
    n_simulaciones = config$n_simulaciones,
    bootstrap_hecho = HACER_BOOTSTRAP,
    robustez_kt    = robustez_kt,
    lc_rates_solo_proceso = lc_sim_solo_proceso,
    mu_simulada    = mu_draw,
    n_dif_kt       = n_d,
    kt_interpolado = config$anios_excluidos
  ),
  file.path(config$ruta_resultados, "proyeccion_mortalidad.rds")
)

tam <- file.size(file.path(config$ruta_resultados, "simulaciones_completas.rds"))
cat("  simulaciones_completas.rds :", round(tam / 1024^2, 1), "MB\n")

cat("\n=====================================================\n")
cat("  SCRIPT 08 COMPLETADO\n")
cat("=====================================================\n")
cat("  Horizonte     :", min(anios_proy), "-", max(anios_proy), "\n")
cat("  Simulaciones  :", format(config$n_simulaciones, big.mark = "."), "\n")
cat("  Deriva de k(t):", round(mu, 4), "\n")
cat("  Ganancia e65  :", round(ganancia, 2), "años\n")
cat("=====================================================\n\n")
cat("Siguiente guion: 09_modulo_demografico.R\n")
