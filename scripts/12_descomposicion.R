# =============================================================================
# SCRIPT 12 - DESCOMPOSICION DEL CRECIMIENTO DEL GASTO
# Indice de Divisia de media logaritmica (LMDI), forma aditiva.
# Referencia: Ang, B. W. (2005), "The LMDI approach to decomposition analysis:
# a practical guide", Energy Policy, 33(7), 867-871.
#
# IDENTIDAD
#   G(t) = suma_{g,s} [ N(t) * w(g,s,t) * p(s) * tau(g,s,t) ] * Q(t)
#
#   N : poblacion total de 65 y mas anios          -> efecto ESCALA
#   w : participacion de cada grupo y sexo         -> efecto ESTRUCTURA
#   p : tasa de riesgo de pobreza, constante       -> no genera efecto
#   tau: tasa de percepcion                        -> efecto PERCEPCION
#   Q : cuantia anual media                        -> efecto CUANTIA
#
# La descomposicion aditiva de Ang reparte la variacion total sin residuo:
#   G(T) - G(0) = D_escala + D_estructura + D_percepcion + D_cuantia
# donde cada termino es
#   D_X = suma_{g,s} L( G(g,s,T), G(g,s,0) ) * ln( X(g,s,T) / X(g,s,0) )
# y L(a,b) = (a - b) / (ln a - ln b) es la media logaritmica, con L(a,a) = a.
#
# Se emplea la forma aditiva y no la multiplicativa porque el interes esta en
# atribuir euros de gasto, no razones. En niveles la identidad dejaria un
# termino de interaccion no atribuible a ningun factor; en la formulacion de
# Ang ese residuo es nulo por construccion, lo que se comprueba mas abajo.
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(ggplot2)

cat("=== SCRIPT 12: DESCOMPOSICION LMDI ===\n\n")

demo <- readRDS(file.path(config$ruta_resultados, "modulo_demografico.rds"))
eleg <- readRDS(file.path(config$ruta_resultados, "modulo_elegibilidad.rds"))
fin  <- readRDS(file.path(config$ruta_resultados, "modulo_financiero.rds"))

anios <- demo$anios
H     <- length(anios)
GR    <- demo$grupos
NG    <- length(GR)
SEXOS <- c("H", "M")
pobreza <- c(H = config$tasa_pobreza_65_h, M = config$tasa_pobreza_65_m)

# -----------------------------------------------------------------------------
# 12.1 MEDIA LOGARITMICA
# -----------------------------------------------------------------------------

L <- function(a, b) {
  ifelse(a == b, a, (a - b) / (log(a) - log(b)))
}

# -----------------------------------------------------------------------------
# 12.2 CONSTRUCCION DE LOS COMPONENTES
# -----------------------------------------------------------------------------

descomponer <- function(especificacion, Q) {

  # Poblacion mediana por grupo y sexo
  N <- array(0, dim = c(NG, 2, H), dimnames = list(GR, SEXOS, anios))
  for (si in seq_along(SEXOS)) N[, si, ] <- demo$pob_grupo_med[, , SEXOS[si]]

  N_tot <- apply(N, 3, sum)                     # escala
  w     <- sweep(N, 3, N_tot, "/")              # estructura

  tau <- array(0, dim = c(NG, 2, H))
  for (si in seq_along(SEXOS))
    tau[, si, ] <- eleg$tau_path[, SEXOS[si], , especificacion]

  # Gasto por celda
  G <- array(0, dim = c(NG, 2, H))
  for (si in seq_along(SEXOS))
    for (gi in seq_len(NG))
      G[gi, si, ] <- N_tot * w[gi, si, ] * pobreza[[si]] * tau[gi, si, ] * Q

  list(N_tot = N_tot, w = w, tau = tau, Q = Q, G = G,
       G_tot = apply(G, 3, sum))
}

lmdi <- function(d, i0, iT) {
  ef <- c(escala = 0, estructura = 0, percepcion = 0, cuantia = 0)
  for (si in 1:2) for (gi in seq_len(NG)) {
    g0 <- d$G[gi, si, i0]; gT <- d$G[gi, si, iT]
    if (g0 <= 0 || gT <= 0) next
    peso <- L(gT, g0)
    ef["escala"]     <- ef["escala"]     + peso * log(d$N_tot[iT] / d$N_tot[i0])
    ef["estructura"] <- ef["estructura"] + peso * log(d$w[gi, si, iT] / d$w[gi, si, i0])
    ef["percepcion"] <- ef["percepcion"] + peso * log(d$tau[gi, si, iT] / d$tau[gi, si, i0])
    ef["cuantia"]    <- ef["cuantia"]    + peso * log(d$Q[iT] / d$Q[i0])
  }
  ef
}

# -----------------------------------------------------------------------------
# 12.3 ESCENARIO BASE
# -----------------------------------------------------------------------------

cat("12.3 Escenario base: percepcion constante e indexacion de precios\n\n")

d_base <- descomponer("constante", fin$Q_precios)
i0 <- 1; iT <- H

G0 <- d_base$G_tot[i0]; GT <- d_base$G_tot[iT]
ef <- lmdi(d_base, i0, iT)
residuo <- (GT - G0) - sum(ef)

cat("  Gasto en", anios[i0], ":", round(G0 / 1e9, 3), "mil M EUR\n")
cat("  Gasto en", anios[iT], ":", round(GT / 1e9, 3), "mil M EUR\n")
cat("  Variacion         :", round((GT - G0) / 1e9, 3), "mil M EUR\n\n")

tab <- data.frame(
  factor = c("Escala demografica", "Estructura por edad y sexo",
             "Tasa de percepcion", "Cuantia"),
  mil_M_EUR = round(as.numeric(ef) / 1e9, 3),
  pct = round(as.numeric(ef) / (GT - G0) * 100, 1))
print(tab, row.names = FALSE)
cat("  --------------------------------------------------\n")
cat("  Suma de efectos:", round(sum(ef) / 1e9, 3), "mil M EUR\n")
cat("  Variacion observada:", round((GT - G0) / 1e9, 3), "mil M EUR\n")
cat("  Residuo:", format(residuo, scientific = TRUE, digits = 3), "EUR\n")

if (abs(residuo) / abs(GT - G0) > 1e-8) {
  warning("La descomposicion deja residuo. Revisar la implementacion.")
} else {
  cat("  Descomposicion exacta: el residuo es nulo, como corresponde a LMDI.\n\n")
}

# -----------------------------------------------------------------------------
# 12.4 ESCENARIO TENDENCIAL
# -----------------------------------------------------------------------------

cat("12.4 Escenario tendencial\n\n")

d_tend <- descomponer("tendencial", fin$Q_precios)
ef_t <- lmdi(d_tend, i0, iT)
GT_t <- d_tend$G_tot[iT]

tab_t <- data.frame(
  factor = tab$factor,
  mil_M_EUR = round(as.numeric(ef_t) / 1e9, 3),
  pct = round(as.numeric(ef_t) / (GT_t - G0) * 100, 1))
print(tab_t, row.names = FALSE)

cat("\n  Contraste del efecto de la percepcion:\n")
cat("    constante :", round(ef["percepcion"] / 1e9, 3), "mil M EUR\n")
cat("    tendencial:", round(ef_t["percepcion"] / 1e9, 3), "mil M EUR\n")
cat("    diferencia:", round((ef_t["percepcion"] - ef["percepcion"]) / 1e9, 3),
    "mil M EUR\n")
cat("  Esa diferencia es el ahorro atribuible a la maduracion contributiva.\n\n")

# -----------------------------------------------------------------------------
# 12.5 DESCOMPOSICION POR SUBPERIODOS
# -----------------------------------------------------------------------------

cat("12.5 Descomposicion por subperiodos, escenario base\n\n")

cortes <- c(2025, 2030, 2040, 2050)
sub <- data.frame()
for (k in seq_len(length(cortes) - 1)) {
  a <- match(cortes[k], anios); b <- match(cortes[k + 1], anios)
  e <- lmdi(d_base, a, b)
  dg <- d_base$G_tot[b] - d_base$G_tot[a]
  sub <- rbind(sub, data.frame(
    periodo = paste0(cortes[k], "-", cortes[k + 1]),
    variacion = round(dg / 1e9, 3),
    escala = round(e["escala"] / dg * 100, 1),
    estructura = round(e["estructura"] / dg * 100, 1),
    percepcion = round(e["percepcion"] / dg * 100, 1),
    cuantia = round(e["cuantia"] / dg * 100, 1), row.names = NULL))
}
print(sub, row.names = FALSE)
cat("  Porcentajes sobre la variacion de cada subperiodo.\n\n")

# -----------------------------------------------------------------------------
# 12.6 LECTURA
# -----------------------------------------------------------------------------

cat("12.6 Lectura de los resultados\n\n")
pr <- as.numeric(ef) / (GT - G0) * 100
cat("  El crecimiento del gasto entre", anios[i0], "y", anios[iT], "se atribuye a:\n")
cat("    escala demografica  :", sprintf("%5.1f %%", pr[1]), "\n")
cat("    estructura por edad :", sprintf("%5.1f %%", pr[2]), "\n")
cat("    tasa de percepcion  :", sprintf("%5.1f %%", pr[3]), "\n")
cat("    cuantia             :", sprintf("%5.1f %%", pr[4]), "\n\n")
if (pr[2] < 0) {
  cat("  El efecto de la estructura es negativo: el envejecimiento interno\n")
  cat("  desplaza poblacion hacia los tramos de mayor edad, que presentan\n")
  cat("  menor tasa de percepcion, de modo que contiene el gasto.\n\n")
}

# -----------------------------------------------------------------------------
# 12.7 GUARDAR
# -----------------------------------------------------------------------------

descomposicion <- list(
  metodo = "LMDI aditivo. Ang (2005).",
  identidad = "G = suma_{g,s} N * w(g,s) * p(s) * tau(g,s) * Q",
  anios = c(anios[i0], anios[iT]),
  gasto_inicial = G0, gasto_final = GT,
  efectos_base = ef, efectos_tendencial = ef_t,
  tabla_base = tab, tabla_tendencial = tab_t,
  subperiodos = sub,
  residuo = residuo,
  fecha = Sys.Date()
)
saveRDS(descomposicion, file.path(config$ruta_resultados, "descomposicion.rds"))
cat("12.7 Guardado descomposicion.rds\n\n")

# -----------------------------------------------------------------------------
# FIGURA
# -----------------------------------------------------------------------------

d22 <- data.frame(
  factor = factor(tab$factor, levels = rev(tab$factor)),
  valor = as.numeric(ef) / 1e9)

f22 <- ggplot(d22, aes(factor, valor, fill = valor > 0)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, colour = "grey30") +
  geom_text(aes(label = sprintf("%+.2f", valor),
                hjust = ifelse(valor > 0, -0.15, 1.15)), size = 3.4) +
  scale_fill_manual(values = c(`TRUE` = "#B2182B", `FALSE` = "#2166AC"),
                    guide = "none") +
  coord_flip() +
  labs(title = "Descomposici\u00f3n del crecimiento del gasto, 2025-2050",
       subtitle = paste0("\u00cdndice de Divisia de media logar\u00edtmica. Variaci\u00f3n total: ",
                         round((GT - G0) / 1e9, 2), " mil M EUR"),
       x = NULL, y = "Miles de millones de euros",
       caption = "M\u00e9todo LMDI aditivo, Ang (2005). Fuente: Elaboraci\u00f3n propia.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10,
                                     colour = "grey30"),
        plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

ggsave(file.path(config$ruta_graficos, "fig22_descomposicion.png"),
       f22, width = 7.5, height = 4, dpi = 300)

cat("Figura guardada: fig22\n\n")
cat("=====================================================\n")
cat("  SCRIPT 12 COMPLETADO\n")
cat("=====================================================\n")
cat("  Siguiente: 13_resultados_finales.R\n\n")
