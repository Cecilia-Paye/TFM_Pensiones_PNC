# =============================================================================
# SCRIPT 11 - MODULO FINANCIERO
# Gasto proyectado, peso sobre el producto interior bruto y valor actual
# actuarial, bajo la matriz completa de escenarios.
#
#   G(t) = B(t) * Q(t)
#   Q(t) = Q(2025) * (1 + g)^(t - 2025)
#   PIB(t) = PIB(2025) * (1 + gpib)^(t - 2025)     [PIB observado de 2025, INE]
#   VAA = suma_t G(t) / (1 + r)^(t - 2025)
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(ggplot2)
library(scales)

cat("=== SCRIPT 11: MODULO FINANCIERO ===\n\n")

eleg <- readRDS(file.path(config$ruta_resultados, "modulo_elegibilidad.rds"))

anios <- eleg$anios
H     <- length(anios)
N_SIM <- dim(eleg$beneficiarios)[2]
AB    <- config$anio_base           # 2025

benef <- eleg$beneficiarios

# -----------------------------------------------------------------------------
# 11.1 SENDAS DE CUANTIA Y DE PRODUCTO
# -----------------------------------------------------------------------------

Q0      <- config$cuantia_anual_espana      # 7.803,60 EUR, media real de 2025
Q0_chi  <- config$cuantia_anual_chile
Q0_bol  <- config$cuantia_anual_bolivia
g_prec  <- config$tasa_indexacion_precios
g_suf   <- config$tasa_indexacion_suficiencia
gpib    <- config$crec_pib

Q_precios     <- Q0 * (1 + g_prec)^(anios - AB)
Q_suficiencia <- Q0 * (1 + g_suf)^(anios - AB)
# Sendas de cuantia de Chile y Bolivia. No intervienen en los escenarios de la
# senda espanola; se conservan porque el guion 14 las emplea para el indicador
# de suficiencia relativa.
Q_chile       <- Q0_chi * (1 + g_prec)^(anios - AB)
Q_bolivia     <- Q0_bol * (1 + g_prec)^(anios - AB)

PIB <- config$PIB_BASE * (1 + gpib)^(anios - config$PIB_BASE_ANIO)

cat("11.1 Parametros financieros\n")
cat("  Tipos de descuento         :",
    paste0(sprintf("%.1f", config$tasas_descuento * 100), " %", collapse = " / "), "\n")
cat("  Cuantia anual 2025, Espana :", format(round(Q0, 2), big.mark = "."), "EUR\n")
cat("  Indexacion base            :", sprintf("%.1f %%", g_prec * 100), "\n")
cat("  Indexacion por suficiencia :", sprintf("%.1f %%", g_suf * 100), "\n")
cat("  Crecimiento nominal del PIB:", sprintf("%.1f %%", gpib * 100), "\n")
cat("  Cuantia en 2050, base      :",
    format(round(Q_precios[H], 2), big.mark = "."), "EUR\n\n")

# -----------------------------------------------------------------------------
# 11.2 MATRIZ DE ESCENARIOS
# -----------------------------------------------------------------------------
# Los escenarios E0, E1 y E2 comparten la misma distribucion simulada y se
# diferencian unicamente en el percentil que se reporta: E0 la mediana, E1 el
# percentil 95 y E2 el percentil 5. La distribucion completa de esa misma
# simulacion es la que alimenta los intervalos, las medidas de riesgo (11.3bis)
# y la figura 19; no constituye un escenario aparte. Los demas alteran la
# especificacion de la percepcion (E4, R1-R3) o la senda de la cuantia (E3 y
# E5, este ultimo definido en 11.4).
#
# En los contrafactuales de Chile y Bolivia se presentan dos variantes. La
# primera mantiene la cuantia espanola y aisla, por tanto, el efecto puro del
# diseno de elegibilidad. La segunda adopta tambien la cuantia del pais de
# origen y ofrece la lectura comparada completa.

# ESCENARIOS
#
# E0 a E4 y E5 describen la senda del sistema espanol bajo distintos supuestos.
#
# R1 y R2 son ESCENARIOS DE REFORMA, no comparaciones entre paises. Responden a
# la pregunta de cuanto costaria a Espana ampliar SU PROPIA prestacion hasta la
# cobertura que alcanzan Chile o Bolivia. La cuantia que se mantiene es por ello
# la espanola: lo que se simula es la extension de la pension no contributiva
# espanola, no la importacion de la prestacion de otro pais. El parametro que se
# toma del sistema extranjero es unicamente el criterio de acceso, esto es, la
# proporcion de la poblacion mayor que resulta elegible.
#
# La comparacion internacional propiamente dicha -que sistema es mas generoso o
# mas costoso- no se realiza aqui sino en el guion 14, con cada pais referido a
# su propia economia, porque el coste de un sistema solo es interpretable en
# relacion con la riqueza del pais que lo financia.

esc <- list(
  E0 = list(nom = "Base",            b = "constante",     q = Q_precios,     pct = 0.50),
  E1 = list(nom = "Longevidad alta", b = "constante",     q = Q_precios,     pct = 0.95),
  E2 = list(nom = "Longevidad baja", b = "constante",     q = Q_precios,     pct = 0.05),
  E3 = list(nom = "Suficiencia",     b = "suficiencia",   q = Q_suficiencia, pct = 0.50),
  E4 = list(nom = "Tendencial",      b = "tendencial",    q = Q_precios,     pct = 0.50),
  R1 = list(nom = "Reforma: cobertura de nivel chileno",   b = "chile_total",   q = Q_precios, pct = 0.50),
  R2 = list(nom = "Reforma: cobertura de nivel boliviano", b = "bolivia_total", q = Q_precios, pct = 0.50),
  R3 = list(nom = "Reforma: cobertura no contributiva chilena", b = "chile_nc", q = Q_precios, pct = 0.50)
)

cat("11.2 Calculo del gasto por escenario\n")

gasto_sim <- list()
for (k in names(esc)) {
  gasto_sim[[k]] <- benef[, , esc[[k]]$b] * esc[[k]]$q
}

resumen <- function(k) {
  G <- gasto_sim[[k]]
  data.frame(anio = anios,
             gasto  = apply(G, 1, quantile, esc[[k]]$pct),
             pib_pct = apply(G / PIB, 1, quantile, esc[[k]]$pct) * 100)
}
res <- lapply(setNames(names(esc), names(esc)), resumen)

# -----------------------------------------------------------------------------
# 11.3 VALOR ACTUAL ACTUARIAL
# -----------------------------------------------------------------------------

cat("11.3 Valor actual actuarial\n")

vaa <- function(G, r) {
  desc <- (1 + r)^-(anios - AB)
  apply(G, 2, function(x) sum(x * desc))
}

# Cada escenario reporta su propio percentil de la distribucion: E1 el 95,
# E2 el 5 y los demas la mediana.
tab_vaa <- data.frame()
for (k in names(esc)) {
  for (r in config$tasas_descuento) {
    v <- vaa(gasto_sim[[k]], r)
    tab_vaa <- rbind(tab_vaa, data.frame(
      escenario = k, nombre = esc[[k]]$nom, descuento = r,
      vaa      = as.numeric(quantile(v, esc[[k]]$pct)),
      vaa_p025 = as.numeric(quantile(v, 0.025)),
      vaa_p50  = as.numeric(quantile(v, 0.50)),
      vaa_p975 = as.numeric(quantile(v, 0.975)),
      row.names = NULL))
  }
}

cat("  Miles de millones de euros, horizonte", AB, "-", max(anios), "\n")
piv <- reshape(tab_vaa[, c("escenario", "descuento", "vaa")],
               idvar = "escenario", timevar = "descuento", direction = "wide")
names(piv) <- c("escenario", paste0("r=", config$tasas_descuento * 100, "%"))
piv[, -1] <- round(piv[, -1] / 1e9, 1)
print(piv, row.names = FALSE)
cat("  Cada escenario se evalua en su propio percentil: E1 en el 95,\n")
cat("  E2 en el 5 y los demas en la mediana.\n")

rc <- config$tasas_descuento[2]
vc <- tab_vaa[tab_vaa$descuento == rc, ]
cat("\n  Escenario base al", sprintf("%.1f %%", rc * 100), "de descuento:\n")
b <- vc[vc$escenario == "E0", ]
cat("    VAA:", round(b$vaa / 1e9, 1), "mil M EUR",
    "| IC95%", round(b$vaa_p025 / 1e9, 1), "-", round(b$vaa_p975 / 1e9, 1), "\n")
# PIB_BASE es el PIB observado de 2025, que coincide con el anio base, de
# modo que se usa directamente.
stopifnot(config$PIB_BASE_ANIO == AB)
cat("    Sobre el PIB de", config$anio_base, ":",
    sprintf("%.2f %%", b$vaa / PIB[1] * 100), "\n\n")

# -----------------------------------------------------------------------------
# 11.3bis MEDIDAS DE RIESGO
# -----------------------------------------------------------------------------
# El apartado 3.4.4 de la memoria compromete el calculo del valor en riesgo y
# del valor en riesgo condicional sobre la distribucion simulada. Se obtienen
# aqui sobre el gasto del ultimo ejercicio y sobre el valor actual actuarial del
# compromiso, ambos en el escenario base.
#
#   VaR(alfa) : nivel que solo se supera con probabilidad 1 - alfa
#   TVaR(alfa): media de la cola situada por encima del VaR

cat("11.3bis Medidas de riesgo, escenario base\n")

rc <- config$tasa_descuento_central
g_fin <- gasto_sim$E0[H, ]
v_e0  <- vaa(gasto_sim$E0, rc)

medida <- function(x, alfa = 0.95) {
  q <- as.numeric(quantile(x, alfa))
  c(mediana = median(x), VaR = q, TVaR = mean(x[x >= q]))
}
m_g <- medida(g_fin)
m_v <- medida(v_e0)

riesgo <- data.frame(
  magnitud = c(paste("Gasto en", max(anios)), "Valor actual actuarial"),
  mediana = c(m_g["mediana"], m_v["mediana"]),
  VaR95   = c(m_g["VaR"],     m_v["VaR"]),
  TVaR95  = c(m_g["TVaR"],    m_v["TVaR"]),
  recargo_VaR_pct  = c((m_g["VaR"]/m_g["mediana"]-1)*100,
                       (m_v["VaR"]/m_v["mediana"]-1)*100),
  recargo_TVaR_pct = c((m_g["TVaR"]/m_g["mediana"]-1)*100,
                       (m_v["TVaR"]/m_v["mediana"]-1)*100),
  row.names = NULL)

cat(sprintf("  %-24s %10s %10s %10s\n", "", "Mediana", "VaR 95%", "TVaR 95%"))
for (i in 1:2) {
  cat(sprintf("  %-24s %10.2f %10.2f %10.2f  (mil M EUR)\n",
              riesgo$magnitud[i], riesgo$mediana[i]/1e9,
              riesgo$VaR95[i]/1e9, riesgo$TVaR95[i]/1e9))
}
cat("  Recargo sobre la mediana del gasto:",
    sprintf("VaR %+.1f %%, TVaR %+.1f %%", riesgo$recargo_VaR_pct[1],
            riesgo$recargo_TVaR_pct[1]), "\n")
cat("  La estrechez de estos recargos refleja que la incertidumbre de\n")
cat("  longevidad se traslada de forma atenuada al gasto agregado.\n\n")

# ESTABILIDAD DE MONTE CARLO (apartado 3.4.4 de la memoria). Se comparan los
# percentiles del gasto de 2050 y del valor actual actuarial obtenidos en dos
# submuestras independientes de N_SIM/2 trayectorias. Una diferencia pequena
# acredita que 5.000 trayectorias bastan para estabilizar los percentiles
# extremos que alimentan las medidas de riesgo.
cat("  Estabilidad de los percentiles: dos mitades independientes de la muestra\n")
mitad <- seq_len(N_SIM) <= N_SIM / 2
estab <- data.frame()
for (p in c(0.05, 0.50, 0.95)) {
  qa <- quantile(g_fin[mitad], p); qb <- quantile(g_fin[!mitad], p)
  va <- quantile(v_e0[mitad], p);  vb <- quantile(v_e0[!mitad], p)
  estab <- rbind(estab, data.frame(
    percentil = p * 100,
    gasto_2050_dif_pct = as.numeric((qb / qa - 1) * 100),
    vaa_dif_pct        = as.numeric((vb / va - 1) * 100)))
  cat(sprintf("    p%02.0f  gasto 2050: %+.2f %%   VAA: %+.2f %%\n", p * 100,
              (qb / qa - 1) * 100, (vb / va - 1) * 100))
}
cat("  (diferencia relativa entre mitades; valores muy inferiores al 1 %\n")
cat("   indican que el error de Monte Carlo es despreciable)\n\n")

# -----------------------------------------------------------------------------
# 11.3ter SENSIBILIDAD AL TIPO DE DESCUENTO
# -----------------------------------------------------------------------------
# El tipo de descuento no es un parametro actuarial sino financiero, y la
# valoracion del compromiso es sensible a el. Se evalua sobre una rejilla y se
# reporta la variacion por cada cien puntos basicos, magnitud analoga a la
# duracion de una cartera de pasivos.

cat("11.3ter Sensibilidad del valor actual actuarial al tipo de descuento\n")

sens_r <- data.frame(tipo = config$rejilla_descuento)
sens_r$vaa <- sapply(sens_r$tipo, function(r) median(vaa(gasto_sim$E0, r)))
sens_r$var_pct <- c(NA, diff(sens_r$vaa) / head(sens_r$vaa, -1) * 100)

cat(sprintf("  %8s %14s %12s\n", "Tipo", "VAA (mil M)", "Variacion"))
for (i in seq_len(nrow(sens_r))) {
  cat(sprintf("  %7.1f %% %14.1f %11s\n", sens_r$tipo[i]*100, sens_r$vaa[i]/1e9,
      ifelse(is.na(sens_r$var_pct[i]), "-", sprintf("%+.1f %%", sens_r$var_pct[i]))))
}

i_c <- which.min(abs(sens_r$tipo - rc))
i_lo <- which.min(abs(sens_r$tipo - (rc - 0.005)))
i_hi <- which.min(abs(sens_r$tipo - (rc + 0.005)))
dur <- (sens_r$vaa[i_lo] - sens_r$vaa[i_hi]) / (2 * sens_r$vaa[i_c] * 0.005)
cat("\n  Duracion implicita del compromiso:", sprintf("%.1f anios", dur), "\n")
cat("  Cien puntos basicos de variacion del tipo modifican el valor actual\n")
cat("  actuarial en torno a", sprintf("%.1f %%", dur), "\n\n")

# -----------------------------------------------------------------------------
# 11.4 ESCENARIO E5: PRESUPUESTO CONSTANTE
# -----------------------------------------------------------------------------
# Se despeja la tasa de indexacion que mantiene invariante el peso del gasto
# sobre el producto a lo largo del horizonte. De
#   B(T) Q0 (1+g)^(T-AB) / [PIB(AB) (1+gpib)^(T-AB)] = B(AB) Q0 / PIB(AB)
# se obtiene
#   g* = (1 + gpib) * ( B(AB) / B(T) )^(1/(T-AB)) - 1
# B(.) es la mediana de beneficiarios bajo percepcion constante.

cat("11.4 Escenario de presupuesto constante\n")

B <- apply(benef[, , "constante"], 1, median)
nT <- max(anios) - AB
g_est <- (1 + gpib) * (B[1] / B[H])^(1 / nT) - 1

cat("  Indexacion compatible con un peso constante sobre el PIB:",
    sprintf("%.2f %%", g_est * 100), "\n")
cat("  Frente a la indexacion base de", sprintf("%.2f %%", g_prec * 100), "\n")
cat("  Diferencia:", sprintf("%+.2f puntos", (g_est - g_prec) * 100), "\n")

Q_e8 <- Q0 * (1 + g_est)^(anios - AB)
cat("  Cuantia en 2050 bajo E5:", format(round(Q_e8[H], 2), big.mark = "."), "EUR\n")
cat("  Frente a la base       :", format(round(Q_precios[H], 2), big.mark = "."), "EUR\n")
cat("  Perdida de poder adquisitivo acumulada:",
    sprintf("%.1f %%", (Q_e8[H] / Q_precios[H] - 1) * 100), "\n\n")

gasto_sim$E5 <- benef[, , "constante"] * Q_e8
esc$E5 <- list(nom = "Presupuesto constante", b = "constante", q = Q_e8, pct = 0.50)
res$E5 <- resumen("E5")

# -----------------------------------------------------------------------------
# 11.4bis ESCENARIOS DE REFORMA DE LA COBERTURA
# -----------------------------------------------------------------------------
# Se cuantifica el coste de ampliar la pension no contributiva espanola hasta
# tres niveles de cobertura de referencia, manteniendo la cuantia espanola. El
# coste se expresa en tres magnitudes: gasto anual, peso sobre el producto y
# valor actual actuarial del compromiso adicional.

cat("11.4bis Escenarios de reforma de la cobertura\n\n")

reformas <- c("E0", "R3", "R1", "R2")
tab_ref <- data.frame()
for (k in reformas) {
  b50 <- median(benef[H, , esc[[k]]$b])
  g50 <- res[[k]]$gasto[H]
  p50 <- res[[k]]$pib_pct[H]
  v   <- median(vaa(gasto_sim[[k]], rc))
  tab_ref <- rbind(tab_ref, data.frame(
    escenario = k, nombre = esc[[k]]$nom,
    beneficiarios_2050 = round(b50),
    gasto_2050_milM = round(g50 / 1e9, 2),
    pct_pib_2050 = round(p50, 3),
    coste_incremental_milM = round((g50 - res$E0$gasto[H]) / 1e9, 2),
    incremento_pib_pp = round(p50 - res$E0$pib_pct[H], 3),
    vaa_milM = round(v / 1e9, 1),
    row.names = NULL))
}
print(tab_ref[, c("escenario", "beneficiarios_2050", "gasto_2050_milM",
                  "pct_pib_2050", "incremento_pib_pp", "vaa_milM")],
      row.names = FALSE)

cat("\n  Lectura:\n")
for (i in 2:nrow(tab_ref)) {
  cat("   ", tab_ref$nombre[i], "\n")
  cat("      Multiplica los beneficiarios por",
      sprintf("%.1f", tab_ref$beneficiarios_2050[i] / tab_ref$beneficiarios_2050[1]), "\n")
  cat("      Anade", sprintf("%.3f puntos", tab_ref$incremento_pib_pp[i]),
      "de producto al gasto de 2050\n")
  cat("      Eleva el compromiso de", tab_ref$vaa_milM[1], "a", tab_ref$vaa_milM[i],
      "mil M EUR\n")
}
cat("\n  Estos escenarios no describen los sistemas chileno o boliviano, sino el\n")
cat("  coste de ampliar el sistema espanol conservando su nivel de prestacion.\n\n")


# -----------------------------------------------------------------------------
# 11.5 SENSIBILIDAD A LA AMORTIGUACION DE LA TENDENCIA
# -----------------------------------------------------------------------------
# El escenario tendencial depende del factor de amortiguacion phi, fijado en
# 0,9. Se recalcula el gasto de 2050 para valores alternativos, reconstruyendo
# las sendas de percepcion con los mismos parametros lambda.

cat("11.5 Sensibilidad al factor de amortiguacion\n")

demo <- readRDS(file.path(config$ruta_resultados, "modulo_demografico.rds"))
pobreza <- c(H = config$tasa_pobreza_65_h, M = config$tasa_pobreza_65_m)
hh <- anios - eleg$anio_calibracion

sens <- data.frame()
for (phi in c(0.80, 0.85, 0.90, 0.95)) {
  am <- (1 - phi^hh) / (1 - phi)
  B_phi <- matrix(0, H, N_SIM)
  for (s in c("H", "M")) for (gi in seq_along(eleg$grupos)) {
    tau_p <- pmin(1, pmax(0, eleg$tau0[gi, s] * exp(-eleg$lambda[gi, s] * am)))
    B_phi <- B_phi + demo$grupo_sim[gi, , , s] * pobreza[[s]] * tau_p
  }
  G <- median(B_phi[H, ] * Q_precios[H])
  sens <- rbind(sens, data.frame(phi = phi,
                                 benef_2050 = median(B_phi[H, ]),
                                 gasto_2050 = G,
                                 dif_vs_constante = (median(B_phi[H, ]) /
                                   median(benef[H, , "constante"]) - 1) * 100))
}
sens$benef_2050 <- round(sens$benef_2050)
sens$gasto_2050 <- round(sens$gasto_2050 / 1e9, 2)
sens$dif_vs_constante <- round(sens$dif_vs_constante, 1)
print(sens, row.names = FALSE)
cat("  La columna final mide el efecto de la maduracion contributiva sobre el\n")
cat("  numero de beneficiarios en 2050, en porcentaje.\n\n")

# -----------------------------------------------------------------------------
# 11.5bis CONTRASTE DEL MODELO DE MORTALIDAD
# -----------------------------------------------------------------------------
# El apartado 4.2 selecciona Lee-Carter como modelo base sobre la base de su
# capacidad predictiva fuera de muestra. Cairns, Blake y Dowd se conserva como
# contraste de especificacion. Se recalcula aqui el gasto del escenario base
# con la poblacion proyectada bajo ese segundo modelo, de modo que la
# independencia de las conclusiones respecto de la eleccion quede acreditada
# con la magnitud que interesa, que es el gasto y no la tasa de mortalidad.

cat("11.5bis Contraste del modelo de mortalidad\n")

contraste <- NULL
if (!is.null(eleg$beneficiarios_cbd)) {
  G_cbd <- eleg$beneficiarios_cbd[, , "constante"] * Q_precios
  g_lc  <- apply(gasto_sim$E0, 1, quantile, 0.50)
  g_cb  <- apply(G_cbd, 1, quantile, 0.50)
  pib_lc <- g_lc / PIB * 100
  pib_cb <- g_cb / PIB * 100
  vaa_lc <- median(vaa(gasto_sim$E0, config$tasas_descuento[2]))
  vaa_cb <- median(vaa(G_cbd, config$tasas_descuento[2]))

  contraste <- data.frame(
    anio = anios,
    gasto_lee_carter = g_lc, gasto_cbd = g_cb,
    desv_pct = (g_cb / g_lc - 1) * 100,
    pib_lee_carter = pib_lc, pib_cbd = pib_cb)

  cat(sprintf("  %6s %14s %14s %10s\n", "Anio", "Lee-Carter", "CBD", "Desv."))
  for (a in c(AB, 2035, 2050)) {
    i <- match(a, anios)
    cat(sprintf("  %6d %11.2f mM %11.2f mM %9.2f %%\n", a,
                g_lc[i] / 1e9, g_cb[i] / 1e9, contraste$desv_pct[i]))
  }
  cat("\n  Peso sobre el PIB en 2050:",
      sprintf("%.3f %% frente a %.3f %%", pib_lc[H], pib_cb[H]), "\n")
  cat("  Valor actual actuarial al", sprintf("%.1f %%", config$tasas_descuento[2]*100),
      ":", round(vaa_lc / 1e9, 1), "frente a", round(vaa_cb / 1e9, 1), "mil M EUR\n")
  cat("  Desviacion del valor actual actuarial:",
      sprintf("%+.2f %%", (vaa_cb / vaa_lc - 1) * 100), "\n")
  cat("  La eleccion del modelo de mortalidad no altera las conclusiones si\n")
  cat("  esta desviacion resulta pequena frente a la de los escenarios.\n\n")
} else {
  cat("  No hay proyeccion de contraste disponible.\n\n")
}


# -----------------------------------------------------------------------------
# 11.6 RESULTADOS PRINCIPALES
# -----------------------------------------------------------------------------

idx <- match(c(AB, 2035, 2050), anios)
cat("11.6 Gasto en miles de millones de euros corrientes\n")
tb <- data.frame(anio = anios[idx])
for (k in names(esc)) tb[[k]] <- round(res[[k]]$gasto[idx] / 1e9, 2)
print(tb, row.names = FALSE)

cat("\n  Peso sobre el producto interior bruto, en porcentaje\n")
tp <- data.frame(anio = anios[idx])
for (k in names(esc)) tp[[k]] <- round(res[[k]]$pib_pct[idx], 3)
print(tp, row.names = FALSE)

cat("\n  Escenario base, detalle:\n")
cat("   ", AB, ":", format(round(res$E0$gasto[1] / 1e9, 2), big.mark = "."),
    "mil M EUR |", sprintf("%.3f %% del PIB", res$E0$pib_pct[1]), "\n")
cat("    2050:", format(round(res$E0$gasto[H] / 1e9, 2), big.mark = "."),
    "mil M EUR |", sprintf("%.3f %% del PIB", res$E0$pib_pct[H]), "\n")
cat("    Crecimiento del gasto:",
    sprintf("%+.1f %%", (res$E0$gasto[H] / res$E0$gasto[1] - 1) * 100), "\n")
cat("    Variacion del peso sobre el PIB:",
    sprintf("%+.3f puntos", res$E0$pib_pct[H] - res$E0$pib_pct[1]), "\n")

cat("\n  Riesgo de longevidad, gasto en 2050:\n")
cat("    E2 longevidad baja :", round(res$E2$gasto[H] / 1e9, 2), "mil M EUR\n")
cat("    E0 base            :", round(res$E0$gasto[H] / 1e9, 2), "mil M EUR\n")
cat("    E1 longevidad alta :", round(res$E1$gasto[H] / 1e9, 2), "mil M EUR\n")
cat("    Amplitud:",
    sprintf("%.1f %% del valor central",
            (res$E1$gasto[H] - res$E2$gasto[H]) / res$E0$gasto[H] * 100), "\n\n")

# -----------------------------------------------------------------------------
# 11.7 GUARDAR
# -----------------------------------------------------------------------------

modulo_fin <- list(
  anios = anios, anio_base = AB,
  Q_precios = Q_precios, Q_suficiencia = Q_suficiencia, Q_e8 = Q_e8,
  PIB = PIB,
  escenarios = esc,
  gasto_sim = gasto_sim,
  resultados = res,
  vaa = tab_vaa,
  indexacion_presupuesto_constante = g_est,
  sensibilidad_phi = sens,
  contraste_modelo = contraste,
  medidas_riesgo = riesgo,
  estabilidad_montecarlo = estab,
  sensibilidad_descuento = sens_r,
  duracion = dur,
  escenarios_reforma = tab_ref,
  parametros = list(Q0 = Q0, g_precios = g_prec, g_suficiencia = g_suf,
                    gpib = gpib, PIB_base = config$PIB_BASE, PIB_base_anio = config$PIB_BASE_ANIO,
                    tasas_descuento = config$tasas_descuento),
  fecha = Sys.Date()
)
saveRDS(modulo_fin, file.path(config$ruta_resultados, "modulo_financiero.rds"))
cat("11.7 Guardado modulo_financiero.rds\n\n")

# -----------------------------------------------------------------------------
# FIGURAS
# -----------------------------------------------------------------------------

tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10,
                                     colour = "grey30"),
        plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

G0 <- gasto_sim$E0
d19 <- data.frame(anio = anios,
                  p50  = apply(G0, 1, quantile, 0.50) / 1e9,
                  p05  = apply(G0, 1, quantile, 0.05) / 1e9,
                  p95  = apply(G0, 1, quantile, 0.95) / 1e9,
                  p025 = apply(G0, 1, quantile, 0.025) / 1e9,
                  p975 = apply(G0, 1, quantile, 0.975) / 1e9)
f19 <- ggplot(d19, aes(anio)) +
  geom_ribbon(aes(ymin = p025, ymax = p975), fill = "#4575B4", alpha = 0.15) +
  geom_ribbon(aes(ymin = p05, ymax = p95), fill = "#4575B4", alpha = 0.25) +
  geom_line(aes(y = p50), colour = "#1A237E", linewidth = 1) +
  labs(title = "Gasto proyectado en pensiones no contributivas de jubilaci\u00f3n",
       subtitle = "Escenario base. Bandas del 90 % y del 95 %",
       x = NULL, y = "Miles de millones de euros corrientes",
       caption = "Fuente: Elaboraci\u00f3n propia.") + tema
ggsave(file.path(config$ruta_graficos, "fig19_gasto_base.png"),
       f19, width = 7.5, height = 4.5, dpi = 300)

sel <- c("E0", "E3", "E4", "E5")
d20 <- do.call(rbind, lapply(sel, function(k)
  data.frame(anio = anios, esc = esc[[k]]$nom, pib = res[[k]]$pib_pct)))
f20 <- ggplot(d20, aes(anio, pib, colour = esc)) +
  geom_line(linewidth = 1) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "Peso del gasto sobre el producto interior bruto",
       subtitle = "Escenarios de la senda espa\u00f1ola",
       x = NULL, y = "Porcentaje del PIB", colour = NULL,
       caption = "Fuente: Elaboraci\u00f3n propia.") + tema
ggsave(file.path(config$ruta_graficos, "fig20_gasto_pib.png"),
       f20, width = 7.5, height = 4.5, dpi = 300)

d21 <- do.call(rbind, lapply(c("E0", "R3", "R1", "R2"), function(k)
  data.frame(anio = anios, esc = esc[[k]]$nom, pib = res[[k]]$pib_pct)))
d21$esc <- factor(d21$esc, levels = sapply(c("E0","R3","R1","R2"), function(k) esc[[k]]$nom))
f21 <- ggplot(d21, aes(anio, pib, colour = esc)) +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = label_number(accuracy = 0.01)) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Coste de ampliar la cobertura de la prestaci\u00f3n espa\u00f1ola",
       subtitle = "Cuant\u00eda espa\u00f1ola en todos los escenarios. Escala logar\u00edtmica",
       x = NULL, y = "Porcentaje del PIB", colour = NULL,
       caption = "Escenarios de reforma del sistema espa\u00f1ol. Fuente: Elaboraci\u00f3n propia.") + tema
ggsave(file.path(config$ruta_graficos, "fig21_escenarios_reforma.png"),
       f21, width = 8, height = 4.5, dpi = 300)

cat("Figuras guardadas: fig19, fig20, fig21\n\n")
cat("=====================================================\n")
cat("  SCRIPT 11 COMPLETADO\n")
cat("=====================================================\n")
cat("  Siguiente: 12_descomposicion.R\n\n")
