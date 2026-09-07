# =============================================================================
# SCRIPT 10 - MODULO DE ELEGIBILIDAD
# Tasa de percepcion por sexo y tramo de edad, con dos especificaciones
# temporales y los escenarios contrafactuales de Chile y Bolivia.
#
# DEFINICION (apartado 3.4.2 de la memoria)
#   tau(g, s, t) = perceptores(g, s, t) / [ N(g, s, t) * pobreza(s) ]
# El denominador aproxima la poblacion elegible por la poblacion en riesgo de
# pobreza, de modo que tau es una COTA INFERIOR de la percepcion efectiva.
#
#   beneficiarios(g, s, t) = N(g, s, t) * pobreza(s) * tau(g, s, t)
#
# El factor de pobreza NO se aplica en los escenarios contrafactuales de Chile
# y Bolivia, cuyos sistemas no someten el acceso a prueba de recursos.
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(ggplot2)
library(scales)

cat("=== SCRIPT 10: MODULO DE ELEGIBILIDAD ===\n\n")

demo <- readRDS(file.path(config$ruta_resultados, "modulo_demografico.rds"))

leer <- function(f) {
  p <- file.path(RUTA_PROYECTO, "datos/limpios", f)
  if (!file.exists(p)) stop("Falta ", p)
  read.csv(p, stringsAsFactors = FALSE)
}
tau_hist <- leer("tau_historico_sexo_tramo.csv")
perc     <- leer("perceptores_jubilacion_sexo_tramo_2017_2025.csv")

anios  <- demo$anios
H      <- length(anios)
N_SIM  <- demo$n_sim
GR     <- demo$grupos                      # 65-69 ... 85+
NG     <- length(GR)
SEXOS  <- c("H", "M")
pobreza <- c(H = config$tasa_pobreza_65_h, M = config$tasa_pobreza_65_m)

ANIO_CAL <- 2024   # ultimo anio con perceptores y poblacion alineados

# -----------------------------------------------------------------------------
# 10.1 NIVEL DE PARTIDA DE LA TASA DE PERCEPCION
# -----------------------------------------------------------------------------

cat("10.1 Nivel de partida, diciembre de", ANIO_CAL, "\n")

tau0 <- matrix(NA_real_, NG, 2, dimnames = list(GR, SEXOS))
for (g in GR) for (s in SEXOS) {
  sx <- if (s == "H") "hombres" else "mujeres"
  v <- tau_hist$tau[tau_hist$anio == ANIO_CAL & tau_hist$sexo == sx &
                    tau_hist$tramo == g]
  if (length(v) != 1) stop("No se encuentra tau para ", g, " ", sx)
  tau0[g, s] <- v
}
print(round(tau0, 4))
cat("\n")

# -----------------------------------------------------------------------------
# 10.2 ESPECIFICACION TENDENCIAL
# -----------------------------------------------------------------------------
# La serie 2017-2024 presenta dos regimenes. Entre 2017 y 2022 domina la
# maduracion contributiva: la percepcion femenina desciende, sobre todo en el
# tramo 65-69, que recoge las cohortes con carreras de cotizacion mas completas.
# A partir de 2023 se superpone un efecto normativo: al elevarse la cuantia de
# la prestacion se eleva tambien el limite de ingresos que da acceso a ella,
# porque en Espana ambos son la misma magnitud.
#
# La especificacion tendencial aisla el primer regimen. Se estima la tasa de
# variacion logaritmica anual sobre 2017-2022 y se proyecta con amortiguacion
# geometrica, de modo que el cambio acumulado converge a un limite finito:
#
#   tau(t) = tau(2024) * exp( -lambda * (1 - phi^h) / (1 - phi) ),  h = t - 2024
#
# Con phi = 0,9 el cambio logaritmico total converge a -10*lambda. Sin
# amortiguacion la extrapolacion de veintiseis anios careceria de sentido.

PHI <- 0.90
cat("10.2 Especificacion tendencial\n")
cat("  Ventana de estimacion: 2017-2022 (antes del efecto normativo)\n")
cat("  Factor de amortiguacion phi =", PHI, "\n\n")

lambda    <- matrix(NA_real_, NG, 2, dimnames = list(GR, SEXOS))
se_lambda <- matrix(NA_real_, NG, 2, dimnames = list(GR, SEXOS))
for (g in GR) for (s in SEXOS) {
  sx <- if (s == "H") "hombres" else "mujeres"
  d <- tau_hist[tau_hist$sexo == sx & tau_hist$tramo == g &
                tau_hist$anio >= 2017 & tau_hist$anio <= 2022, ]
  d <- d[order(d$anio), ]
  aj <- lm(log(d$tau) ~ I(d$anio - 2017))
  lambda[g, s]    <- -coef(aj)[2]
  se_lambda[g, s] <- summary(aj)$coefficients[2, 2]
}

cat("  Tasa anual de variacion (positiva = descendente):\n")
print(round(lambda, 4))
cat("\n  Error tipico de la estimacion:\n")
print(round(se_lambda, 4))

cat("\n  Cambio acumulado en 2050 respecto de", ANIO_CAL, ":\n")
h_max <- max(anios) - ANIO_CAL
fac_max <- exp(-lambda * (1 - PHI^h_max) / (1 - PHI))
print(round(fac_max, 3))
cat("\n")

# -----------------------------------------------------------------------------
# 10.3 SENDAS DE LA TASA DE PERCEPCION
# -----------------------------------------------------------------------------

h <- anios - ANIO_CAL
amort <- (1 - PHI^h) / (1 - PHI)

tau_path <- array(NA_real_, dim = c(NG, 2, H, 2),
                  dimnames = list(GR, SEXOS, anios, c("constante", "tendencial")))
for (g in GR) for (s in SEXOS) {
  tau_path[g, s, , "constante"]  <- tau0[g, s]
  tau_path[g, s, , "tendencial"] <- pmin(1, pmax(0,
      tau0[g, s] * exp(-lambda[g, s] * amort)))
}

# -----------------------------------------------------------------------------
# 10.3bis INCERTIDUMBRE SOBRE LA TASA DE PERCEPCION
# -----------------------------------------------------------------------------
# Las trayectorias simuladas del modelo recogen la incertidumbre de mortalidad,
# pero tratan la tasa de percepcion como conocida con certeza. El trabajo
# concluye, sin embargo, que la mortalidad es la fuente de variacion menos
# relevante para el gasto, de modo que resulta incoherente medir con precision
# aquello que menos pesa y dar por cierto lo demas.
#
# Se introduce aqui la incertidumbre de estimacion de la tendencia: para cada
# trayectoria se muestrea la tasa anual de variacion de su distribucion
# muestral, lambda_s ~ N(lambda_gorro, se^2), estimada sobre las seis
# observaciones anuales del primer regimen. La dispersion resultante mide
# cuanto puede desviarse la senda de percepcion por el solo hecho de que la
# tendencia se estime sobre una serie corta.

set.seed(config$semilla)
tau_sim <- array(NA_real_, dim = c(NG, 2, H, N_SIM))
for (gi in seq_len(NG)) for (si in seq_along(SEXOS)) {
  lam_s <- rnorm(N_SIM, lambda[gi, si], se_lambda[gi, si])
  tau_sim[gi, si, , ] <- pmin(1, pmax(0,
      tau0[gi, si] * exp(-outer(amort, lam_s))))
}
cat("  Incertidumbre de la tendencia incorporada sobre", N_SIM, "trayectorias.\n\n")

# -----------------------------------------------------------------------------
# 10.4 EFECTO DE LA CUANTIA SOBRE LA ELEGIBILIDAD
# -----------------------------------------------------------------------------
# En Espana el limite de ingresos que da acceso a la prestacion coincide con su
# cuantia anual. Elevar la cuantia amplia por tanto el colectivo elegible.
# La serie reciente permite una estimacion indicativa de esa elasticidad:
#
#   cuantia 2023 +15,0 %  ->  perceptores 2024 +10,3 %   -> 0,69
#   cuantia 2024  +6,9 %  ->  perceptores 2025  +3,7 %   -> 0,54
#
# Son solo dos observaciones y el parametro no esta identificado
# econometricamente. Se adopta el valor central y se somete a sensibilidad.
# La elasticidad unicamente actua en el escenario de suficiencia, que es el que
# altera la senda de la cuantia respecto de la base.

ELASTICIDAD <- 0.60

g_base <- config$tasa_indexacion_precios
g_suf  <- config$tasa_indexacion_suficiencia
ratio_cuantia <- ((1 + g_suf) / (1 + g_base)) ^ (anios - config$anio_base)
fac_suf <- ratio_cuantia ^ ELASTICIDAD

cat("10.4 Elasticidad de la elegibilidad a la cuantia\n")
cat("  Valor adoptado:", ELASTICIDAD, "\n")
cat("  Ampliacion del colectivo en 2050 bajo suficiencia:",
    sprintf("%+.1f %%", (fac_suf[H] - 1) * 100), "\n\n")

# -----------------------------------------------------------------------------
# 10.5 BENEFICIARIOS
# -----------------------------------------------------------------------------
# Espana:  N * pobreza(s) * tau(g, s, t)
# Chile y Bolivia: N * tau, sin factor de pobreza, porque ninguno de los dos
# sistemas somete el acceso a prueba de recursos. Son contrafactuales: aplican
# los parametros institucionales de cada pais a la demografia espanola.

cat("10.5 Calculo de beneficiarios\n")

especificaciones <- c("constante", "tendencial", "tendencial_estoc", "suficiencia",
                      "chile_total", "chile_nc", "bolivia_total", "bolivia_nc")
NE <- length(especificaciones)

calcular <- function(pobl, guardar_sexo = FALSE) {
  b <- array(0, dim = c(H, N_SIM, NE), dimnames = list(anios, NULL, especificaciones))
  bs <- array(0, dim = c(H, N_SIM, 2), dimnames = list(anios, NULL, SEXOS))
  for (s in SEXOS) {
    si <- match(s, SEXOS)
    for (gi in seq_len(NG)) {
      N <- pobl[gi, , , s]                          # [H, N_SIM]
      for (e in c("constante", "tendencial")) {
        b[, , e] <- b[, , e] + N * pobreza[[s]] * tau_path[gi, s, , e]
      }
      b[, , "tendencial_estoc"] <- b[, , "tendencial_estoc"] +
        N * pobreza[[s]] * tau_sim[gi, si, , ]
      bs[, , s] <- bs[, , s] + N * pobreza[[s]] * tau_path[gi, s, , "constante"]
      b[, , "suficiencia"] <- b[, , "suficiencia"] +
        N * pobreza[[s]] * tau_path[gi, s, , "constante"] * fac_suf
      b[, , "chile_total"]   <- b[, , "chile_total"]   + N * config$tau_chile_total
      b[, , "chile_nc"]      <- b[, , "chile_nc"]      + N * config$tau_chile_nc
      b[, , "bolivia_total"] <- b[, , "bolivia_total"] + N * config$tau_bolivia_total
      b[, , "bolivia_nc"]    <- b[, , "bolivia_nc"]    + N * config$tau_bolivia_nc
    }
  }
  if (guardar_sexo) attr(b, "por_sexo") <- bs
  b
}

benef <- calcular(demo$grupo_sim, guardar_sexo = TRUE)
benef_sexo <- attr(benef, "por_sexo")

# Modelo de contraste: se repite el calculo sobre la poblacion proyectada con
# las tasas de Cairns, Blake y Dowd, para acreditar que la eleccion del modelo
# de mortalidad no determina los resultados de gasto.
benef_cbd <- NULL
if (!is.null(demo$grupo_sim_cbd)) {
  benef_cbd <- calcular(demo$grupo_sim_cbd)
  d50 <- (median(benef_cbd[H, , "constante"]) /
          median(benef[H, , "constante"]) - 1) * 100
  cat("  Beneficiarios en", max(anios), "bajo Lee-Carter:",
      format(round(median(benef[H, , "constante"])), big.mark = "."), "\n")
  cat("  Beneficiarios en", max(anios), "bajo CBD       :",
      format(round(median(benef_cbd[H, , "constante"])), big.mark = "."), "\n")
  cat("  Desviacion del modelo de contraste:", sprintf("%+.2f %%", d50), "\n")
}

# -----------------------------------------------------------------------------
# 10.6 VALIDACION
# -----------------------------------------------------------------------------
# El modelo debe reproducir el numero de perceptores observado en el anio de
# calibracion. La poblacion del modelo a 1 de enero de 2025 se empareja con los
# perceptores de diciembre de 2024, que distan una semana.

cat("\n10.6 Validacion frente al dato observado\n")

obs <- sum(perc$perceptores[perc$anio_dic == ANIO_CAL & perc$tramo != "no_consta"])
i25 <- match(config$anio_base, anios)
mod <- median(benef[i25, , "constante"])
err <- (mod - obs) / obs * 100

cat("  Perceptores observados en diciembre de", ANIO_CAL, ":",
    format(obs, big.mark = "."), "\n")
cat("  Beneficiarios del modelo en", config$anio_base, "  :",
    format(round(mod), big.mark = "."), "\n")
cat("  Error relativo:", sprintf("%+.3f %%", err), "\n")

if (abs(err) > 1) {
  stop("El modelo no reproduce los perceptores observados. Revisar tau0.")
}
cat("  Comprobacion superada.\n\n")

# -----------------------------------------------------------------------------
# 10.7 RESULTADOS
# -----------------------------------------------------------------------------

pct <- function(e) {
  data.frame(anio = anios,
             p025 = apply(benef[, , e], 1, quantile, 0.025),
             p50  = apply(benef[, , e], 1, quantile, 0.50),
             p975 = apply(benef[, , e], 1, quantile, 0.975))
}
resultados <- lapply(setNames(especificaciones, especificaciones), pct)

pobtot <- apply(demo$grupo_sim, c(2, 3), sum)
cobertura <- sapply(especificaciones, function(e)
  apply(benef[, , e] / pobtot, 1, median) * 100)
cobertura <- data.frame(anio = anios, cobertura)

cat("10.7 Beneficiarios proyectados, medianas\n")
idx <- match(c(2025, 2035, 2050), anios)
tb <- data.frame(anio = anios[idx])
for (e in especificaciones) tb[[e]] <- round(resultados[[e]]$p50[idx])
print(tb, row.names = FALSE)

cat("\n  Cobertura sobre la poblacion de 65 y mas, en porcentaje\n")
print(round(cobertura[idx, ], 2), row.names = FALSE)

# ---------------------------------------------------------------------------
cat("\n  DESGLOSE POR SEXO, especificacion constante\n")
cat(sprintf("  %6s %12s %12s %10s %10s\n", "Anio", "Hombres", "Mujeres",
            "% mujeres", "razon M/H"))
for (a in c(config$anio_base, 2035, 2050)) {
  i <- match(a, anios)
  h <- median(benef_sexo[i, , "H"]); m <- median(benef_sexo[i, , "M"])
  cat(sprintf("  %6d %12s %12s %9.1f %% %9.2f\n", a,
              format(round(h), big.mark = "."), format(round(m), big.mark = "."),
              m / (h + m) * 100, m / h))
}

cat("\n  INCERTIDUMBRE DE LA TASA DE PERCEPCION\n")
q_e <- quantile(benef[H, , "tendencial_estoc"], c(0.025, 0.50, 0.975))
q_m <- quantile(benef[H, , "constante"], c(0.025, 0.50, 0.975))
cat("  Beneficiarios en", max(anios), ":\n")
cat("    solo incertidumbre de mortalidad :",
    format(round(q_m[2]), big.mark = "."), " IC95%",
    format(round(q_m[1]), big.mark = "."), "-", format(round(q_m[3]), big.mark = "."), "\n")
cat("    con incertidumbre de percepcion  :",
    format(round(q_e[2]), big.mark = "."), " IC95%",
    format(round(q_e[1]), big.mark = "."), "-", format(round(q_e[3]), big.mark = "."), "\n")
amp_m <- (q_m[3] - q_m[1]) / q_m[2] * 100
amp_e <- (q_e[3] - q_e[1]) / q_e[2] * 100
cat("    amplitud relativa:", sprintf("%.1f %% frente a %.1f %%", amp_m, amp_e), "\n")
cat("    ensanchamiento:", sprintf("%+.0f %%", (amp_e / amp_m - 1) * 100), "\n")

cat("\n  Espana bajo la especificacion tendencial:\n")
cat("    2025:", format(round(resultados$tendencial$p50[i25]), big.mark = "."), "\n")
cat("    2050:", format(round(resultados$tendencial$p50[H]), big.mark = "."),
    " | IC95%", format(round(resultados$tendencial$p025[H]), big.mark = "."),
    "-", format(round(resultados$tendencial$p975[H]), big.mark = "."), "\n")
cat("    Diferencia frente a percepcion constante en 2050:",
    sprintf("%+.1f %%",
            (resultados$tendencial$p50[H] / resultados$constante$p50[H] - 1) * 100),
    "\n\n")

# -----------------------------------------------------------------------------
# 10.8 GUARDAR
# -----------------------------------------------------------------------------

modulo_eleg <- list(
  anios = anios, grupos = GR, sexos = SEXOS,
  anio_calibracion = ANIO_CAL,
  tau0 = tau0, lambda = lambda, se_lambda = se_lambda, phi = PHI,
  tau_sim = NULL,
  beneficiarios_sexo = benef_sexo,
  tau_path = tau_path,
  elasticidad = ELASTICIDAD, factor_suficiencia = fac_suf,
  especificaciones = especificaciones,
  beneficiarios = benef,
  beneficiarios_cbd = benef_cbd,
  resultados = resultados,
  cobertura_pct = cobertura,
  validacion = list(observado = obs, modelo = mod, error_pct = err),
  nota = paste("tau se define sobre la poblacion en riesgo de pobreza y es",
               "por tanto una cota inferior de la percepcion efectiva. Los",
               "escenarios de Chile y Bolivia son contrafactuales: aplican sus",
               "parametros institucionales a la demografia espanola, sin",
               "factor de pobreza, ya que no someten el acceso a prueba de",
               "recursos."),
  fecha = Sys.Date()
)
saveRDS(modulo_eleg, file.path(config$ruta_resultados, "modulo_elegibilidad.rds"))
cat("10.8 Guardado modulo_elegibilidad.rds\n\n")

# -----------------------------------------------------------------------------
# FIGURAS
# -----------------------------------------------------------------------------

tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10,
                                     colour = "grey30"),
        plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

d_hist <- tau_hist
d_hist$sexo <- factor(d_hist$sexo, c("hombres", "mujeres"),
                      c("Hombres", "Mujeres"))
f16 <- ggplot(d_hist, aes(anio, tau, colour = tramo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.4) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", colour = "grey50") +
  facet_wrap(~sexo) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "Tasa de percepci\u00f3n observada por sexo y tramo de edad",
       subtitle = "La l\u00ednea vertical marca el cambio de r\u00e9gimen de 2023",
       x = NULL, y = "Tasa de percepci\u00f3n (\u03c4)", colour = "Tramo",
       caption = paste("La tasa de percepci\u00f3n se calcula sobre la poblaci\u00f3n en riesgo de",
                       "pobreza. Fuente: Elaboraci\u00f3n propia a partir del Imserso y del INE.")) +
  tema
ggsave(file.path(config$ruta_graficos, "fig16_tau_historico.png"),
       f16, width = 8, height = 4.5, dpi = 300)

d17 <- do.call(rbind, lapply(c("constante", "tendencial"), function(e)
  data.frame(anio = anios, esp = e, p50 = resultados[[e]]$p50 / 1000,
             p025 = resultados[[e]]$p025 / 1000,
             p975 = resultados[[e]]$p975 / 1000)))
f17 <- ggplot(d17, aes(anio, p50, colour = esp, fill = esp)) +
  geom_ribbon(aes(ymin = p025, ymax = p975), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c(constante = "#2166AC", tendencial = "#B2182B")) +
  scale_fill_manual(values = c(constante = "#2166AC", tendencial = "#B2182B")) +
  labs(title = "Beneficiarios proyectados de la pensi\u00f3n no contributiva",
       subtitle = "Medianas e intervalos del 95 %, en miles",
       x = NULL, y = "Miles de beneficiarios", colour = NULL, fill = NULL,
       caption = "Fuente: Elaboraci\u00f3n propia.") +
  tema
ggsave(file.path(config$ruta_graficos, "fig17_beneficiarios.png"),
       f17, width = 7.5, height = 4.5, dpi = 300)

d18 <- do.call(rbind, lapply(
  c("constante", "chile_nc", "chile_total", "bolivia_nc", "bolivia_total"),
  function(e) data.frame(anio = anios, esc = e, cob = cobertura[[e]])))
f18 <- ggplot(d18, aes(anio, cob, colour = esc)) +
  geom_line(linewidth = 1) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Cobertura bajo distintos dise\u00f1os institucionales",
       subtitle = "Par\u00e1metros de cada pa\u00eds aplicados a la demograf\u00eda espa\u00f1ola",
       x = NULL, y = "Cobertura sobre la poblaci\u00f3n de 65 y m\u00e1s (%)",
       colour = NULL,
       caption = "Ejercicio contrafactual. Fuente: Elaboraci\u00f3n propia.") +
  tema
ggsave(file.path(config$ruta_graficos, "fig18_cobertura_contrafactual.png"),
       f18, width = 7.5, height = 4.5, dpi = 300)

cat("Figuras guardadas: fig16, fig17, fig18\n\n")
cat("=====================================================\n")
cat("  SCRIPT 10 COMPLETADO\n")
cat("=====================================================\n")
cat("  Siguiente: 11_modulo_financiero.R\n\n")
