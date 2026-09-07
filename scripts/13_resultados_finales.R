# =============================================================================
# SCRIPT 13 - RESULTADOS FINALES
# Consolida la salida de los modulos, exporta las tablas de los capitulos 4 y 5
# y formaliza el contraste de las tres hipotesis.
#
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

cat("=== SCRIPT 13: RESULTADOS FINALES ===\n\n")

demo <- readRDS(file.path(config$ruta_resultados, "modulo_demografico.rds"))
eleg <- readRDS(file.path(config$ruta_resultados, "modulo_elegibilidad.rds"))
fin  <- readRDS(file.path(config$ruta_resultados, "modulo_financiero.rds"))
desc <- readRDS(file.path(config$ruta_resultados, "descomposicion.rds"))

ruta_tab <- file.path(RUTA_PROYECTO, "tablas")
dir.create(ruta_tab, showWarnings = FALSE)

exportar <- function(x, nombre) {
  f <- file.path(ruta_tab, paste0(nombre, ".csv"))
  write.csv(x, f, row.names = FALSE, fileEncoding = "UTF-8")
  cat("  ", nombre, ".csv\n", sep = "")
}

anios <- fin$anios
H     <- length(anios)
AB    <- config$anio_base
i50   <- H

# =============================================================================
# TABLA 1 - COMPARATIVA INSTITUCIONAL AL CIERRE DE 2025
# =============================================================================

cat("TABLA 1 - Comparativa institucional\n\n")

# Las cifras se derivan de config.rds, de modo que la tabla no pueda divergir
# de los parametros.
# Espana: perceptores de diciembre de 2025 sobre poblacion a 1 de enero de
# 2025 (indicador descriptivo, con doce meses de desfase declarado). Chile y
# Bolivia: junio de 2026.
fmt_n   <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")
fmt_pct <- function(x) format(round(x, 2), nsmall = 2, decimal.mark = ",")
fmt_eur <- function(x) format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2)

pob_es  <- config$poblacion_65_2025
ben_es  <- config$perceptores_jub_2025
pob_cl  <- 3034428; ben_cl_tot <- 2287912; ben_cl_nc <- 616284
pob_bo  <- 1306428; ben_bo_tot <- 1239996
ben_bo_nc <- ben_bo_tot * config$prop_bolivia_no_rentista
stopifnot(abs(ben_cl_tot / pob_cl - config$tau_chile_total) < 1e-6,
          abs(ben_bo_tot / pob_bo - config$tau_bolivia_total) < 1e-6)

T1 <- data.frame(
  indicador = c("Edad de acceso", "Prueba de recursos",
                "Poblacion de referencia", "Beneficiarios, pilar cero ampliado",
                "Cobertura ampliada (%)",
                "Beneficiarios no contributivos estrictos",
                "Cobertura estricta (%)",
                "Cuantia anual (moneda nacional)",
                "Cuantia anual (EUR)"),
  Espana = c("65 anios", "Si", fmt_n(pob_es), fmt_n(ben_es),
             fmt_pct(ben_es / pob_es * 100),
             fmt_n(ben_es), fmt_pct(ben_es / pob_es * 100),
             paste(fmt_eur(config$cuantia_anual_espana), "EUR"),
             fmt_eur(config$cuantia_anual_espana)),
  Chile = c("65 anios", "No, excluye decil superior", fmt_n(pob_cl), fmt_n(ben_cl_tot),
            fmt_pct(config$tau_chile_total * 100), fmt_n(ben_cl_nc),
            fmt_pct(config$tau_chile_nc * 100),
            paste(fmt_eur(config$cuantia_anual_chile_clp), "CLP"),
            fmt_eur(config$cuantia_anual_chile)),
  Bolivia = c("60 anios", "No", fmt_n(pob_bo), fmt_n(ben_bo_tot),
              fmt_pct(config$tau_bolivia_total * 100), fmt_n(ben_bo_nc),
              fmt_pct(config$tau_bolivia_nc * 100), "4.168,20 BOB",
              fmt_eur(config$cuantia_anual_bolivia)),
  stringsAsFactors = FALSE)
print(T1, row.names = FALSE)
cat("\n")

cob_es_desc <- ben_es / pob_es * 100          # 2,39
cob_cl_desc <- config$tau_chile_total * 100   # 75,40
cob_bo_desc <- config$tau_bolivia_total * 100 # 94,91

# =============================================================================
# TABLA 2 - PROYECCION DEMOGRAFICA
# =============================================================================

sel <- match(c(AB, 2030, 2035, 2040, 2045, 2050), anios)
T2 <- data.frame(
  anio = anios[sel],
  p025 = round(demo$pob65_pct$p025[sel]),
  mediana = round(demo$pob65_pct$p50[sel]),
  p975 = round(demo$pob65_pct$p975[sel]),
  prop_80_mas = round(demo$prop_80_plus[sel] * 100, 2))

# =============================================================================
# TABLA 3 - BENEFICIARIOS
# =============================================================================

T3 <- data.frame(anio = anios[sel])
for (e in c("constante", "tendencial", "suficiencia"))
  T3[[e]] <- round(eleg$resultados[[e]]$p50[sel])
T3$IC95_inf <- round(eleg$resultados$constante$p025[sel])
T3$IC95_sup <- round(eleg$resultados$constante$p975[sel])

# =============================================================================
# TABLA 4 - GASTO Y PESO SOBRE EL PIB
# =============================================================================

esc_es <- c("E0", "E1", "E2", "E3", "E4", "E5")
T4 <- data.frame(anio = anios[sel])
for (k in esc_es) T4[[paste0(k, "_milM")]] <- round(fin$resultados[[k]]$gasto[sel] / 1e9, 3)
for (k in esc_es) T4[[paste0(k, "_pctPIB")]] <- round(fin$resultados[[k]]$pib_pct[sel], 3)

# Efecto del diseno de elegibilidad sobre la cobertura
T5 <- data.frame(anio = anios[sel])
for (e in c("constante", "chile_nc", "chile_total", "bolivia_nc", "bolivia_total"))
  T5[[e]] <- round(eleg$cobertura_pct[[e]][sel], 2)

# Escenarios de reforma del sistema espanol
T5b <- if (!is.null(fin$escenarios_reforma)) fin$escenarios_reforma else NULL

# =============================================================================
# TABLA 6 - VALOR ACTUAL ACTUARIAL
# =============================================================================

T6 <- fin$vaa[, c("escenario", "nombre", "descuento", "vaa", "vaa_p025", "vaa_p975")]
T6$vaa      <- round(T6$vaa / 1e9, 2)
T6$vaa_p025 <- round(T6$vaa_p025 / 1e9, 2)
T6$vaa_p975 <- round(T6$vaa_p975 / 1e9, 2)
names(T6) <- c("escenario", "nombre", "descuento", "VAA_milM",
               "IC95_inf", "IC95_sup")

# =============================================================================
# TABLA 7 - DESCOMPOSICION
# =============================================================================

T7 <- data.frame(
  factor = desc$tabla_base$factor,
  base_milM = desc$tabla_base$mil_M_EUR,
  base_pct = desc$tabla_base$pct,
  tendencial_milM = desc$tabla_tendencial$mil_M_EUR,
  tendencial_pct = desc$tabla_tendencial$pct)

# =============================================================================
# TABLA 8 - TASA DE PERCEPCION
# =============================================================================

T8 <- data.frame(tramo = eleg$grupos,
                 tau_hombres = round(eleg$tau0[, "H"], 4),
                 tau_mujeres = round(eleg$tau0[, "M"], 4),
                 lambda_hombres = round(eleg$lambda[, "H"], 4),
                 lambda_mujeres = round(eleg$lambda[, "M"], 4))

# =============================================================================
# CONTRASTE DE HIPOTESIS
# =============================================================================

cat("CONTRASTE DE HIPOTESIS\n")
cat("=====================================================\n\n")

# --- H1 -----------------------------------------------------------------
cat("H1. Convergencia en cobertura y divergencia en suficiencia.\n\n")
brecha_cob <- cob_bo_desc - cob_es_desc
ratio_cuant <- config$cuantia_anual_espana / config$cuantia_anual_bolivia
cat(sprintf("  Cobertura al cierre del periodo: Espana %.2f %%, Chile %.2f %%,\n",
            cob_es_desc, cob_cl_desc))
cat(sprintf("  Bolivia %.2f %%. Brecha maxima: %.1f puntos\n", cob_bo_desc, brecha_cob))
cat("  Razon entre cuantias, Espana frente a Bolivia:",
    sprintf("%.1f veces", ratio_cuant), "\n\n")
cat("  RESULTADO: parcialmente refutada.\n")
cat("  La divergencia en suficiencia se confirma, pero no hay convergencia\n")
cat("  en cobertura: la distancia entre el sistema espanol y el boliviano\n")
cat("  supera los noventa puntos porcentuales al final del periodo. El\n")
cat("  contraste de la evolucion 2000-2025 requiere la serie historica de\n")
cat("  los tres paises, pendiente de recopilacion.\n\n")

# --- H2 -----------------------------------------------------------------
cat("H2. Gasto acotado en porcentaje del PIB por la caida de la percepcion.\n\n")
pib_0  <- fin$resultados$E0$pib_pct[1]
pib_50 <- fin$resultados$E0$pib_pct[i50]
pib_e1 <- fin$resultados$E1$pib_pct[i50]
# El efecto de la percepcion procede de la descomposicion del escenario
# TENDENCIAL, de modo que se relativiza sobre la variacion del gasto de ese
# mismo escenario (en el base la percepcion es constante y su efecto es nulo
# por definicion).
ef_per  <- desc$efectos_tendencial["percepcion"] / 1e9
var_tot <- sum(desc$efectos_tendencial) / 1e9      # = G_tend(2050) - G(2025)
cat("  Peso sobre el PIB:", sprintf("%.3f %%", pib_0), "en", AB,
    "y", sprintf("%.3f %%", pib_50), "en 2050.\n")
cat("  En el escenario de longevidad alta:", sprintf("%.3f %%", pib_e1), "\n")
cat("  Efecto de la percepcion en la descomposicion (escenario tendencial):",
    sprintf("%.3f mil M EUR", ef_per), "sobre",
    sprintf("%.3f", var_tot), "de variacion total de ese escenario\n")
cat("  Contribucion de la percepcion:",
    sprintf("%.1f %%", ef_per / var_tot * 100), "\n\n")
cat("  RESULTADO: la conclusion se confirma, el mecanismo no.\n")
cat("  El gasto permanece acotado incluso bajo longevidad adversa, pero no\n")
cat("  por la caida de la tasa de percepcion. Esta desciende en las mujeres\n")
cat("  y asciende en los hombres, de modo que su efecto neto sobre el gasto\n")
cat("  de 2050 es de apenas el", sprintf("%.1f %%", abs(ef_per / var_tot * 100)),
    "de la variacion. La contencion procede\n")
cat("  de que el producto crece mas deprisa que el gasto.\n\n")

# --- H3 -----------------------------------------------------------------
cat("H3. La indexacion pesa mas que la incertidumbre de longevidad.\n\n")
g0 <- fin$resultados$E0$gasto[i50]
efecto_index <- (fin$resultados$E3$gasto[i50] - g0) / g0 * 100
efecto_long  <- (fin$resultados$E1$gasto[i50] -
                 fin$resultados$E2$gasto[i50]) / g0 * 100
cat("  Efecto de la regla de indexacion, E3 frente a E0:",
    sprintf("%+.1f %%", efecto_index), "\n")
# E1 y E2 son los percentiles 95 y 5 de la distribucion simulada, de modo que
# su diferencia es la amplitud del intervalo del 90 %, no del 95 %.
cat("  Amplitud del riesgo de longevidad, E1 menos E2 (intervalo del 90 %):",
    sprintf("%.1f %%", efecto_long), "\n")
cat("  Razon entre ambos:", sprintf("%.1f a 1", efecto_index / efecto_long), "\n\n")
cat("  RESULTADO: confirmada.\n")
cat("  La regla de indexacion desplaza el gasto de 2050 en una magnitud\n")
cat("  del orden de", sprintf("%.0f", efecto_index / efecto_long),
    "veces la amplitud del riesgo de longevidad. A ello se\n")
cat("  suma que en Espana el limite de ingresos coincide con la cuantia, de\n")
cat("  modo que la indexacion actua por dos vias: eleva la prestacion y\n")
cat("  amplia el colectivo elegible.\n\n")

T9 <- data.frame(
  hipotesis = c("H1", "H2", "H3"),
  resultado = c("Parcialmente refutada", "Confirmada con mecanismo distinto",
                "Confirmada"),
  evidencia = c(
    sprintf("Brecha de cobertura de %.1f puntos; razon de cuantias de %.1f veces",
            brecha_cob, ratio_cuant),
    sprintf("Peso sobre el PIB de %.3f a %.3f por ciento; la percepcion aporta %.1f por ciento de la variacion",
            pib_0, pib_50, ef_per / var_tot * 100),
    sprintf("Indexacion %+.1f por ciento frente a longevidad %.1f por ciento",
            efecto_index, efecto_long)),
  stringsAsFactors = FALSE)

# =============================================================================
# EXPORTACION
# =============================================================================

cat("Exportando tablas a", ruta_tab, "\n")
exportar(T1, "T01_comparativa_institucional")
exportar(T2, "T02_proyeccion_demografica")
exportar(T3, "T03_beneficiarios")
exportar(T4, "T04_gasto_y_pib")
exportar(T5, "T05_efecto_diseno_elegibilidad")
if (!is.null(T5b)) exportar(T5b, "T16_escenarios_reforma")

if (!is.null(eleg$beneficiarios_sexo)) {
  bs <- eleg$beneficiarios_sexo
  T17 <- data.frame(anio = anios[sel])
  T17$hombres <- round(apply(bs[sel, , "H"], 1, median))
  T17$mujeres <- round(apply(bs[sel, , "M"], 1, median))
  T17$total   <- T17$hombres + T17$mujeres
  T17$pct_mujeres <- round(T17$mujeres / T17$total * 100, 1)
  T17$razon_M_H   <- round(T17$mujeres / T17$hombres, 2)
  exportar(T17, "T17_beneficiarios_por_sexo")
}
exportar(T6, "T06_valor_actual_actuarial")
exportar(T7, "T07_descomposicion_lmdi")
exportar(T8, "T08_tasa_percepcion")
exportar(T9, "T09_contraste_hipotesis")
exportar(fin$sensibilidad_phi, "T10_sensibilidad_phi")
if (!is.null(fin$medidas_riesgo)) {
  mr <- fin$medidas_riesgo
  for (cc in c("mediana","VaR95","TVaR95")) mr[[cc]] <- round(mr[[cc]]/1e9, 3)
  mr$recargo_VaR_pct  <- round(mr$recargo_VaR_pct, 2)
  mr$recargo_TVaR_pct <- round(mr$recargo_TVaR_pct, 2)
  exportar(mr, "T14_medidas_de_riesgo")
}
if (!is.null(fin$sensibilidad_descuento)) {
  sd <- fin$sensibilidad_descuento
  sd$tipo <- sd$tipo * 100
  sd$vaa  <- round(sd$vaa / 1e9, 2)
  sd$var_pct <- round(sd$var_pct, 2)
  exportar(sd, "T15_sensibilidad_descuento")
}
if (!is.null(fin$contraste_modelo)) {
  ct <- fin$contraste_modelo
  ct$gasto_lee_carter <- round(ct$gasto_lee_carter / 1e9, 3)
  ct$gasto_cbd <- round(ct$gasto_cbd / 1e9, 3)
  ct$desv_pct <- round(ct$desv_pct, 2)
  ct$pib_lee_carter <- round(ct$pib_lee_carter, 4)
  ct$pib_cbd <- round(ct$pib_cbd, 4)
  exportar(ct, "T12_contraste_modelo_mortalidad")
}
exportar(desc$subperiodos, "T11_descomposicion_subperiodos")

# =============================================================================
# RESUMEN
# =============================================================================

cat("\n=====================================================\n")
cat("  RESUMEN DE RESULTADOS\n")
cat("=====================================================\n\n")

cat("DEMOGRAFIA\n")
cat("  Poblacion de 65 y mas:", format(round(demo$pob65_pct$p50[1]), big.mark = "."),
    "en", AB, "y", format(round(demo$pob65_pct$p50[i50]), big.mark = "."), "en 2050\n")
cat("  Crecimiento:", sprintf("%+.1f %%",
    (demo$pob65_pct$p50[i50] / demo$pob65_pct$p50[1] - 1) * 100), "\n")
cat("  Proporcion de 80 y mas:", sprintf("%.2f %% a %.2f %%",
    demo$prop_80_plus[1] * 100, demo$prop_80_plus[i50] * 100), "\n\n")

cat("BENEFICIARIOS\n")
cat("  ", AB, ":", format(round(eleg$resultados$constante$p50[1]), big.mark = "."), "\n")
cat("   2050:", format(round(eleg$resultados$constante$p50[i50]), big.mark = "."),
    "| IC95%", format(round(eleg$resultados$constante$p025[i50]), big.mark = "."),
    "-", format(round(eleg$resultados$constante$p975[i50]), big.mark = "."), "\n")
cat("  Cobertura:", sprintf("%.2f %% a %.2f %%",
    eleg$cobertura_pct$constante[1], eleg$cobertura_pct$constante[i50]), "\n\n")

cat("GASTO\n")
cat("  ", AB, ":", round(fin$resultados$E0$gasto[1] / 1e9, 2), "mil M EUR |",
    sprintf("%.3f %% del PIB", pib_0), "\n")
cat("   2050:", round(g0 / 1e9, 2), "mil M EUR |",
    sprintf("%.3f %% del PIB", pib_50), "\n")
rc <- config$tasas_descuento[2]
vb <- fin$vaa[fin$vaa$escenario == "E0" & fin$vaa$descuento == rc, ]
cat("  Valor actual actuarial al", sprintf("%.1f %%", rc * 100), ":",
    round(vb$vaa / 1e9, 1), "mil M EUR\n\n")

cat("DESCOMPOSICION 2025-2050\n")
for (i in seq_len(nrow(desc$tabla_base)))
  cat("  ", format(desc$tabla_base$factor[i], width = 28), ":",
      sprintf("%6.1f %%", desc$tabla_base$pct[i]), "\n")

if (!is.null(fin$contraste_modelo)) {
  i50c <- nrow(fin$contraste_modelo)
  cat("\nCONTRASTE DEL MODELO DE MORTALIDAD\n")
  cat("  Gasto en 2050, Lee-Carter:",
      round(fin$contraste_modelo$gasto_lee_carter[i50c] / 1e9, 2), "mil M EUR\n")
  cat("  Gasto en 2050, CBD       :",
      round(fin$contraste_modelo$gasto_cbd[i50c] / 1e9, 2), "mil M EUR\n")
  cat("  Desviacion:",
      sprintf("%+.2f %%", fin$contraste_modelo$desv_pct[i50c]), "\n")
}

# Tres bloques independientes.
if (!is.null(eleg$beneficiarios_sexo)) {
  bs <- eleg$beneficiarios_sexo
  cat("\nCOMPOSICION POR SEXO\n")
  for (a in c(AB, 2050)) {
    i <- match(a, anios)
    h <- median(bs[i, , "H"]); m <- median(bs[i, , "M"])
    cat("  ", a, ": mujeres", sprintf("%.1f %%", m/(h+m)*100),
        "| razon mujeres por hombre", sprintf("%.2f", m/h), "\n")
  }
}

if (!is.null(fin$escenarios_reforma)) {
  tr <- fin$escenarios_reforma
  cat("\nESCENARIOS DE REFORMA DE LA COBERTURA, 2050\n")
  for (i in seq_len(nrow(tr))) {
    cat("  ", format(tr$escenario[i], width = 3), format(tr$nombre[i], width = 44),
        sprintf("%7.3f %% PIB", tr$pct_pib_2050[i]),
        sprintf(" | VAA %6.1f mil M", tr$vaa_milM[i]), "\n")
  }
}

if (!is.null(fin$duracion)) {
  cat("\nSENSIBILIDAD AL TIPO DE DESCUENTO\n")
  cat("  Duracion implicita:", sprintf("%.1f anios", fin$duracion), "\n")
  cat("  Cien puntos basicos modifican el compromiso en torno a",
      sprintf("%.1f %%", fin$duracion), "\n")
}
if (!is.null(fin$medidas_riesgo)) {
  cat("\nMEDIDAS DE RIESGO, ESCENARIO BASE\n")
  for (i in 1:nrow(fin$medidas_riesgo)) {
    cat("  ", fin$medidas_riesgo$magnitud[i], ": VaR 95%",
        round(fin$medidas_riesgo$VaR95[i]/1e9, 2), "| TVaR 95%",
        round(fin$medidas_riesgo$TVaR95[i]/1e9, 2), "mil M EUR\n")
  }
}

cat("\nHIPOTESIS\n")
for (i in 1:3) cat("  ", T9$hipotesis[i], ":", T9$resultado[i], "\n")

resultados_finales <- list(
  T1 = T1, T2 = T2, T3 = T3, T4 = T4, T5 = T5,
  T6 = T6, T7 = T7, T8 = T8, T9 = T9,
  ruta_tablas = ruta_tab, fecha = Sys.Date())
saveRDS(resultados_finales, file.path(config$ruta_resultados, "resultados_finales.rds"))

cat("\n=====================================================\n")
cat("  MODELO COMPLETO. Tablas en", ruta_tab, "\n")
cat("=====================================================\n\n")
