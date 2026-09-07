# =============================================================================
# SCRIPT 09 - MODULO DEMOGRAFICO
# Proyeccion por componentes de cohorte, edad simple, desagregada por sexo,
# alimentada por las tasas de mortalidad proyectadas de Lee-Carter.
#
# Caracteristicas:
#   1. La mortalidad proyectada por Lee-Carter alimenta la proyeccion.
#   2. Flujo de cohortes: los supervivientes de la edad x pasan a x+1.
#   3. La dispersion procede de las 5.000 trayectorias de longevidad.
#   4. Desagregacion por sexo, necesaria para contrastar H2.
#
# Poblacion de arranque: INE, tabla 56934, 1 de enero de 2025, edad simple.
# Entradas a los 65 anios: INE, tabla 36643, proyeccion 2026-2076.
# Desagregacion por sexo: NO se ajusta el modelo de factor comun de Li y Lee
# (2005) en su formulacion completa, que exigiria estimar de forma conjunta un
# factor comun y factores especificos de cada poblacion. Se aplica un
# diferencial proporcional por edad, obtenido como razon entre la tasa central
# de cada sexo y la del total sobre las medias 2014-2023 excluido 2020, sobre
# el indice temporal comun ya proyectado. Fuente del diferencial: HMD.
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
config <- readRDS(file.path(RUTA_PROYECTO, "config.rds"))

library(ggplot2)
library(scales)

cat("=== SCRIPT 09: MODULO DEMOGRAFICO (COHORTES) ===\n\n")

ruta_datos <- file.path(RUTA_PROYECTO, "datos/limpios")

# -----------------------------------------------------------------------------
# 9.1 CARGA DE INSUMOS
# -----------------------------------------------------------------------------

cat("9.1 Carga de insumos\n")

sim <- readRDS(file.path(config$ruta_resultados, "simulaciones_completas.rds"))
lc_rates   <- sim$lc_rates          # [edad, anio, simulacion]
cbd_rates  <- sim$cbd_rates         # modelo de contraste
edades_lc  <- sim$edades            # 60 a 100
anios_lc   <- sim$anios_proy        # 2024 a 2050

proy <- readRDS(file.path(config$ruta_resultados, "proyeccion_mortalidad.rds"))
m_central <- proy$m_lc_central       # [edad, anio]

leer <- function(f) {
  p <- file.path(ruta_datos, f)
  if (!file.exists(p)) stop("Falta el fichero ", p,
      "\nLos CSV deben estar en ~/TFM_Pensiones/datos/limpios/")
  read.csv(p, stringsAsFactors = FALSE)
}

arranque <- leer("poblacion_arranque_2025_edad_simple.csv")   # edad, hombres, mujeres
phi      <- leer("razon_mortalidad_sexo.csv")                 # edad, phi_H, phi_M
entradas <- leer("entradas_edad65_INE_2026_2050.csv")         # anio, hombres, mujeres
ine_ref  <- leer("ine_proyeccion_65plus_2026_2050.csv")       # anio, ine_*

cat("  Tasas Lee-Carter:", paste(dim(lc_rates), collapse = " x "),
    "(edad x anio x simulacion)\n")
if (!is.null(cbd_rates)) {
  cat("  Tasas Cairns-Blake-Dowd:", paste(dim(cbd_rates), collapse = " x "), "\n")
} else {
  cat("  AVISO: no hay tasas de CBD. Se omite el contraste de modelo.\n")
}
cat("  Edades disponibles:", min(edades_lc), "-", max(edades_lc), "\n")
cat("  Anios disponibles :", min(anios_lc), "-", max(anios_lc), "\n\n")

# -----------------------------------------------------------------------------
# 9.2 CONFIGURACION DE LA PROYECCION
# -----------------------------------------------------------------------------

ANIO_BASE <- 2025
ANIO_FIN  <- 2050
anios     <- ANIO_BASE:ANIO_FIN
H         <- length(anios)

EDADES <- 65:100          # 100 es grupo abierto
NE     <- length(EDADES)
N_SIM  <- dim(lc_rates)[3]

# Indices de las edades 65-100 dentro del array de mortalidad
idx_edad <- match(EDADES, edades_lc)
stopifnot(!any(is.na(idx_edad)))

# Diferencial de mortalidad por sexo, ordenado como EDADES
phi <- phi[match(EDADES, phi$edad), ]
stopifnot(!any(is.na(phi$phi_H)), !any(is.na(phi$phi_M)))

# Grupos de agregacion. Coinciden con los tramos que publica el Imserso,
# de modo que la tasa de percepcion se calcula sobre poblaciones homogeneas.
grupos <- list("65-69" = 1:5, "70-74" = 6:10, "75-79" = 11:15,
               "80-84" = 16:20, "85+" = 21:NE)
NG <- length(grupos)

cat("9.2 Configuracion\n")
cat("  Horizonte:", ANIO_BASE, "-", ANIO_FIN, "(", H, "anios )\n")
cat("  Edades   :", min(EDADES), "-", max(EDADES), "(", NE, ", ultima abierta )\n")
cat("  Grupos   :", paste(names(grupos), collapse = ", "), "\n")
cat("  Simulaciones:", N_SIM, "\n\n")

# -----------------------------------------------------------------------------
# 9.3 VALIDACION DE LA POBLACION DE ARRANQUE
# -----------------------------------------------------------------------------

arranque <- arranque[match(EDADES, arranque$edad), ]
stopifnot(!any(is.na(arranque$hombres)))

P0_H <- arranque$hombres
P0_M <- arranque$mujeres
tot0 <- sum(P0_H) + sum(P0_M)

cat("9.3 Validacion de la poblacion de arranque (1 de enero de 2025)\n")
cat("  Hombres:", format(sum(P0_H), big.mark = "."), "\n")
cat("  Mujeres:", format(sum(P0_M), big.mark = "."), "\n")
cat("  Total 65+:", format(tot0, big.mark = "."),
    " | INE tabla 56937:", format(config$poblacion_65_2025, big.mark = "."), "\n")

if (abs(tot0 - config$poblacion_65_2025) > 1) {
  stop("La poblacion de arranque no coincide con la tabla 56937.")
}
cat("  Coincidencia exacta.\n\n")

# -----------------------------------------------------------------------------
# 9.4 FUNCION DE PROYECCION POR COHORTES
# -----------------------------------------------------------------------------
# Ecuacion de supervivencia, con m la tasa central de mortalidad:
#   q(x,t) = 1 - exp(-m(x,t))
#   P(x+1, t+1) = P(x,t) * (1 - q(x,t))
# La ultima edad es un grupo abierto y recibe tanto a los supervivientes de la
# edad 99 como a los del propio grupo.
# Las entradas a los 65 anios proceden de la proyeccion oficial del INE, que
# incorpora los supuestos de fecundidad y migracion por debajo de esa edad.

proyectar <- function(P0, tasas, phi_sexo, entradas_sexo) {
  # P0        : vector de poblacion inicial por edad
  # tasas     : matriz [edad, anio] de tasas centrales de mortalidad del total
  # phi_sexo  : vector de razon de mortalidad del sexo respecto del total
  # entradas_sexo : vector con nombres de anio, entradas a los 65
  out <- matrix(0, nrow = NE, ncol = H)
  out[, 1] <- P0
  for (i in 2:H) {
    anio_trans <- anios[i - 1]
    j <- match(anio_trans, anios_lc)
    m <- tasas[, j] * phi_sexo
    superv <- out[, i - 1] * exp(-m)
    out[2:(NE - 1), i] <- superv[1:(NE - 2)]
    out[NE, i]         <- superv[NE - 1] + superv[NE]
    out[1, i]          <- entradas_sexo[as.character(anios[i])]
  }
  out
}

ent_H <- setNames(entradas$hombres, entradas$anio)
ent_M <- setNames(entradas$mujeres, entradas$anio)

# -----------------------------------------------------------------------------
# 9.5 PROYECCION CENTRAL
# -----------------------------------------------------------------------------

cat("9.5 Proyeccion central\n")

tasas_c <- m_central[idx_edad, , drop = FALSE]
central_H <- proyectar(P0_H, tasas_c, phi$phi_H, ent_H)
central_M <- proyectar(P0_M, tasas_c, phi$phi_M, ent_M)
central_T <- central_H + central_M

for (a in c(2025, 2030, 2040, 2050)) {
  i <- match(a, anios)
  cat("  ", a, ":", format(round(sum(central_T[, i])), big.mark = "."), "\n")
}
cat("\n")

# -----------------------------------------------------------------------------
# 9.6 PROYECCION ESTOCASTICA
# -----------------------------------------------------------------------------
# La incertidumbre procede integramente de las 5.000 trayectorias del indice
# temporal de Lee-Carter, es decir, del riesgo de longevidad.

cat("9.6 Proyeccion estocastica sobre", N_SIM, "trayectorias\n")

agregar <- function(P) vapply(grupos, function(k) colSums(P[k, , drop = FALSE]),
                              numeric(H))           # [H, NG]

simular <- function(rates, etiqueta) {
  g <- array(0, dim = c(NG, H, N_SIM, 2),
             dimnames = list(names(grupos), anios, NULL, c("H", "M")))
  tas_sim <- rates[idx_edad, , , drop = FALSE]
  t0 <- Sys.time()
  for (s in seq_len(N_SIM)) {
    tas <- tas_sim[, , s]
    ph <- proyectar(P0_H, tas, phi$phi_H, ent_H)
    pm <- proyectar(P0_M, tas, phi$phi_M, ent_M)
    g[, , s, "H"] <- t(agregar(ph))
    g[, , s, "M"] <- t(agregar(pm))
  }
  cat("  ", etiqueta, ":", round(difftime(Sys.time(), t0, units = "secs")),
      "segundos\n")
  g
}

grupo_sim <- simular(lc_rates, "Lee-Carter")

grupo_sim_cbd <- NULL
if (!is.null(cbd_rates)) {
  grupo_sim_cbd <- simular(cbd_rates, "Cairns-Blake-Dowd")
}
cat("\n")

# -----------------------------------------------------------------------------
# 9.7 PERCENTILES
# -----------------------------------------------------------------------------

total_sim <- apply(grupo_sim, c(2, 3), sum)          # [H, N_SIM]

pob65_pct <- data.frame(
  anio = anios,
  p025 = apply(total_sim, 1, quantile, 0.025),
  p50  = apply(total_sim, 1, quantile, 0.50),
  p975 = apply(total_sim, 1, quantile, 0.975)
)

pct_por_sexo <- lapply(c(H = "H", M = "M"), function(sx) {
  ts <- apply(grupo_sim[, , , sx], c(2, 3), sum)
  data.frame(anio = anios,
             p025 = apply(ts, 1, quantile, 0.025),
             p50  = apply(ts, 1, quantile, 0.50),
             p975 = apply(ts, 1, quantile, 0.975))
})

pob_grupo_med <- apply(grupo_sim, c(1, 2, 4), median)   # [NG, H, sexo]

pob65_pct_cbd <- NULL
if (!is.null(grupo_sim_cbd)) {
  tc <- apply(grupo_sim_cbd, c(2, 3), sum)
  pob65_pct_cbd <- data.frame(
    anio = anios,
    p025 = apply(tc, 1, quantile, 0.025),
    p50  = apply(tc, 1, quantile, 0.50),
    p975 = apply(tc, 1, quantile, 0.975))
}

cat("9.7 Poblacion 65+ proyectada\n")
for (a in c(2030, 2040, 2050)) {
  i <- match(a, anios)
  cat("  ", a, ": mediana", format(round(pob65_pct$p50[i]), big.mark = "."),
      " | IC95%", format(round(pob65_pct$p025[i]), big.mark = "."),
      "-", format(round(pob65_pct$p975[i]), big.mark = "."), "\n")
}
cat("\n")

# -----------------------------------------------------------------------------
# 9.8 VALIDACION FRENTE A LA PROYECCION OFICIAL DEL INE
# -----------------------------------------------------------------------------
# El modelo aplica mortalidad estocastica propia a la poblacion de 65 y mas y
# toma del INE unicamente las entradas a los 65 anios. Dos diferencias de
# supuestos separan ambas proyecciones y actuan en sentidos contrarios:
#
#   (a) La extrapolacion lineal del indice de Lee-Carter proyecta una mejora
#       de la mortalidad mas rapida que los supuestos oficiales del INE, lo
#       que eleva la poblacion superviviente.
#   (b) El modelo no incorpora saldo migratorio por encima de los 65 anios,
#       que el INE si proyecta, lo que la reduce.
#
# En la practica domina (a) y el modelo queda por encima de la referencia
# oficial. Para la proyeccion del gasto ese sesgo es prudente, ya que una
# poblacion mayor implica mas beneficiarios. La desviacion se reporta en el
# apartado de limitaciones de la memoria (3.6.2).

if (!is.null(pob65_pct_cbd)) {
  cat("  Contraste de modelo de mortalidad, medianas:\n")
  for (a in c(2030, 2040, 2050)) {
    i <- match(a, anios)
    d <- (pob65_pct_cbd$p50[i] / pob65_pct$p50[i] - 1) * 100
    cat(sprintf("    %d  Lee-Carter %s  CBD %s  desviacion %+.2f %%\n", a,
                format(round(pob65_pct$p50[i]), big.mark = "."),
                format(round(pob65_pct_cbd$p50[i]), big.mark = "."), d))
  }
  cat("\n")
}

cat("9.8 Validacion frente a la proyeccion oficial del INE (tabla 36643)\n")
cat(sprintf("  %6s %14s %14s %9s\n", "Anio", "Modelo", "INE", "Desv."))
val <- data.frame()
for (a in c(2030, 2040, 2050)) {
  i <- match(a, anios)
  mo <- pob65_pct$p50[i]
  re <- ine_ref$ine_total[match(a, ine_ref$anio)]
  d  <- (mo / re - 1) * 100
  cat(sprintf("  %6d %14s %14s %8.2f%%\n", a,
              format(round(mo), big.mark = "."),
              format(round(re), big.mark = "."), d))
  val <- rbind(val, data.frame(anio = a, modelo = mo, ine = re, desv_pct = d))
}
d50 <- val$desv_pct[val$anio == 2050]
cat("  Desviacion en 2050:", sprintf("%+.2f %%", d50), "\n")
if (d50 > 0) {
  cat("  El modelo proyecta por encima del INE: la mejora de mortalidad de\n")
  cat("  Lee-Carter supera a la de los supuestos oficiales. Sesgo prudente\n")
  cat("  para el gasto. Declarado en el apartado de limitaciones.\n\n")
} else {
  cat("  El modelo proyecta por debajo del INE, principalmente por no\n")
  cat("  incorporar saldo migratorio de 65 y mas anios.\n\n")
}
if (abs(d50) > 15) {
  warning("Desviacion frente al INE superior al 15 %. Revisar los supuestos.")
}

# -----------------------------------------------------------------------------
# 9.9 ESTRUCTURA INTERNA
# -----------------------------------------------------------------------------

prop_80 <- numeric(H)
for (i in 1:H) {
  tot <- sum(central_T[, i])
  prop_80[i] <- sum(central_T[16:NE, i]) / tot
}

cat("9.9 Envejecimiento interno del colectivo\n")
cat("  Proporcion de 80 y mas sobre el total de 65 y mas:\n")
cat("    ", anios[1], ":", sprintf("%.2f %%", prop_80[1] * 100), "\n")
cat("    ", anios[H], ":", sprintf("%.2f %%", prop_80[H] * 100), "\n")
cat("    Variacion:", sprintf("%+.2f puntos porcentuales",
                              (prop_80[H] - prop_80[1]) * 100), "\n\n")

# -----------------------------------------------------------------------------
# 9.10 GUARDAR
# -----------------------------------------------------------------------------

modulo_demo <- list(
  anios = anios,
  edades = EDADES,
  grupos = names(grupos),
  n_sim = N_SIM,

  central_H = central_H, central_M = central_M, central_T = central_T,
  grupo_sim = grupo_sim,
  grupo_sim_cbd = grupo_sim_cbd,
  pob65_pct_cbd = pob65_pct_cbd,
  pob_grupo_med = pob_grupo_med,
  pob65_pct = pob65_pct,
  pct_por_sexo = pct_por_sexo,
  prop_80_plus = prop_80,
  validacion_ine = val,

  metodo = paste("Proyeccion por componentes de cohorte a edad simple,",
                 "por sexo, con mortalidad estocastica de Lee-Carter",
                 "(5.000 trayectorias) y entradas a los 65 anios tomadas",
                 "de la proyeccion oficial del INE."),
  fuentes = c("INE tabla 56934 (poblacion de arranque)",
              "INE tabla 36643 (entradas y referencia de validacion)",
              "Human Mortality Database (diferencial de mortalidad por sexo)"),
  supuestos = c("Diferencial de mortalidad por sexo constante en el horizonte.",
                "Sin saldo migratorio por encima de los 65 anios.",
                "Mejora de la mortalidad segun Lee-Carter, mas rapida que la del INE."),
  fecha = Sys.Date()
)

saveRDS(modulo_demo, file.path(config$ruta_resultados, "modulo_demografico.rds"))
cat("9.10 Guardado modulo_demografico.rds\n\n")

# -----------------------------------------------------------------------------
# FIGURAS
# -----------------------------------------------------------------------------

tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10,
                                     colour = "grey30"),
        plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

d1 <- data.frame(anio = anios,
                 p025 = pob65_pct$p025 / 1e6,
                 p50  = pob65_pct$p50 / 1e6,
                 p975 = pob65_pct$p975 / 1e6)
d1$ine <- ine_ref$ine_total[match(anios, ine_ref$anio)] / 1e6

f1 <- ggplot(d1, aes(anio)) +
  geom_ribbon(aes(ymin = p025, ymax = p975), fill = "#4575B4", alpha = 0.20) +
  geom_line(aes(y = p50), colour = "#1A237E", linewidth = 1) +
  geom_line(aes(y = ine), colour = "#B2182B", linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(labels = label_number(suffix = " M", accuracy = 0.1)) +
  labs(title = "Proyecci\u00f3n de la poblaci\u00f3n de 65 y m\u00e1s a\u00f1os",
       subtitle = "Mediana e intervalo del 95 %. En rojo discontinuo, proyecci\u00f3n oficial del INE",
       x = NULL, y = "Poblaci\u00f3n",
       caption = "Cohortes con mortalidad estoc\u00e1stica de Lee-Carter. Fuente: Elaboraci\u00f3n propia a partir del INE.") +
  tema

ggsave(file.path(config$ruta_graficos, "fig14_poblacion65.png"),
       f1, width = 7.5, height = 4.5, dpi = 300)

d2 <- do.call(rbind, lapply(seq_len(NG), function(g) {
  data.frame(anio = anios, grupo = names(grupos)[g],
             pob = pob_grupo_med[g, , "H"] + pob_grupo_med[g, , "M"])
}))
d2$grupo <- factor(d2$grupo, levels = names(grupos))

f2 <- ggplot(d2, aes(anio, pob, fill = grupo)) +
  geom_area(alpha = 0.85) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1)) +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(title = "Estructura por edad de la poblaci\u00f3n de 65 y m\u00e1s a\u00f1os",
       subtitle = "Medianas por grupo de edad",
       x = NULL, y = "Poblaci\u00f3n", fill = NULL,
       caption = "Fuente: Elaboraci\u00f3n propia.") +
  tema + theme(legend.position = "right")

ggsave(file.path(config$ruta_graficos, "fig15_estructura_edad.png"),
       f2, width = 7.5, height = 4.5, dpi = 300)

d3 <- rbind(
  data.frame(anio = anios, sexo = "Hombres", p50 = pct_por_sexo$H$p50 / 1e6),
  data.frame(anio = anios, sexo = "Mujeres", p50 = pct_por_sexo$M$p50 / 1e6))

f3 <- ggplot(d3, aes(anio, p50, colour = sexo)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = label_number(suffix = " M", accuracy = 0.1)) +
  scale_colour_manual(values = c("Hombres" = "#2166AC", "Mujeres" = "#B2182B")) +
  labs(title = "Poblaci\u00f3n de 65 y m\u00e1s a\u00f1os por sexo",
       subtitle = "Medianas de la distribuci\u00f3n simulada",
       x = NULL, y = "Poblaci\u00f3n", colour = NULL,
       caption = "Fuente: Elaboraci\u00f3n propia.") +
  tema

ggsave(file.path(config$ruta_graficos, "fig15b_poblacion_sexo.png"),
       f3, width = 7.5, height = 4.5, dpi = 300)

cat("Figuras guardadas: fig14, fig15, fig15b\n\n")
cat("=====================================================\n")
cat("  SCRIPT 09 COMPLETADO\n")
cat("=====================================================\n")
cat("  Siguiente: 10_modulo_elegibilidad.R\n\n")
