# =============================================================================
# SCRIPT 00 - CONFIGURACION
# TFM: Pensiones no contributivas en Bolivia, Chile y Espana
# Cecilia Paye Larico - UAH - Master en Ciencias Actuariales y Financieras
# =============================================================================

rm(list = ls())
cat("\014")

RUTA_PROYECTO <- path.expand("~/TFM_Pensiones")
for (d in c("datos/crudos", "datos/limpios", "resultados", "graficos")) {
  dir.create(file.path(RUTA_PROYECTO, d), recursive = TRUE, showWarnings = FALSE)
}

# =============================================================================
# 1. DEMOGRAFIA ESPANA
#    Fuente: INE, Cifras de Poblacion, tabla 56937.
#    Obtenido como suma de los seis grupos quinquenales 65-69 a 90+.
# =============================================================================

# ATENCION: se usa POBLACION RESIDENTE TOTAL, no la filtrada por pais de
# nacimiento. La PNC exige residencia legal de diez anos, no nacimiento en
# Espana, de modo que los residentes nacidos fuera forman parte del colectivo
# elegible. La serie filtrada por nacimiento excluia 871.098 personas (8,6 %).
poblacion_65_2024 <- 9928368    # 1 de enero de 2024
poblacion_65_2025 <- 10178625   # 1 de enero de 2025
poblacion_65_2025_h <- 4442965
poblacion_65_2025_m <- 5735660

# Tasas de riesgo de pobreza 65+. Fuente: INE, ECV 2025.
tasa_pobreza_65   <- 0.164
tasa_pobreza_65_h <- 0.137
tasa_pobreza_65_m <- 0.185

# =============================================================================
# 2. PERCEPTORES OBSERVADOS. MODALIDAD DE JUBILACION.
#    Fuente: IMSERSO, Perfil del pensionista de PNC, diciembre de 2025.
# =============================================================================

perceptores_jub_2025   <- 243449
perceptores_jub_2025_m <- 174630
perceptores_jub_2025_h <- 68819

stopifnot(perceptores_jub_2025_m + perceptores_jub_2025_h == perceptores_jub_2025)

# =============================================================================
# 3. TASA DE PERCEPCION (tau)
#
#    Definicion (apartado 3.4.2 del TFM):
#      tau = perceptores observados / poblacion elegible aproximada
#    donde la poblacion elegible se aproxima por la poblacion 65+ en riesgo
#    de pobreza. tau es por tanto una COTA INFERIOR de la percepcion efectiva.
#
#    IMPLICACION PARA EL MODULO DE ELEGIBILIDAD (Espana):
#      beneficiarios = poblacion * tasa_pobreza * tau
#    NO   beneficiarios = poblacion * tau
# =============================================================================

# ESPANA. Calibrado sobre 2025, que es el ultimo ano con observacion conjunta
# de perceptores y poblacion.
# Se distinguen dos valores del parametro, que responden a preguntas distintas:
#
#   (a) DESCRIPTIVO. Perceptores del ultimo ejercicio publicado, diciembre de
#       2025, sobre la poblacion a 1 de enero de 2025. Media un desfase de doce
#       meses entre numerador y denominador, de modo que sobrestima ligeramente
#       el parametro. Se emplea solo como indicador del cierre del periodo.
#
#   (b) DE CALIBRACION. Perceptores de diciembre de 2024 sobre la poblacion a 1
#       de enero de 2025: una semana de desfase. Es el unico par con alineacion
#       temporal, y es el que emplea el guion 10 para calibrar el modelo, con
#       desagregacion por sexo y tramo de edad.
#
# La memoria cita (b) cuando se refiere al modelo y (a) solo como dato
# descriptivo, declarando en ese caso el desfase.

poblacion_pobre_65_2025 <- poblacion_65_2025 * tasa_pobreza_65   # 1.669.294

# (a) descriptivo, diciembre de 2025
tau_espana_desc <- perceptores_jub_2025 / poblacion_pobre_65_2025          # 0,14584

# (b) de calibracion, diciembre de 2024
perceptores_jub_2024   <- 234756
perceptores_jub_2024_h <- 65678
perceptores_jub_2024_m <- 169078
tau_espana <- perceptores_jub_2024 / poblacion_pobre_65_2025               # 0,14063

# Desagregacion por sexo. Cada sexo se calibra contra su propia tasa de
# riesgo de pobreza, porque la incidencia es sensiblemente mayor en mujeres.
tau_espana_h <- perceptores_jub_2024_h / (poblacion_65_2025_h * tasa_pobreza_65_h)  # 0,10790
tau_espana_m <- perceptores_jub_2024_m / (poblacion_65_2025_m * tasa_pobreza_65_m)  # 0,15934

# CHILE. Fuente: Superintendencia de Pensiones, junio de 2026.
# Poblacion 65+: INE Chile, proyecciones base 2024, junio de 2026.
# Sin factor de pobreza: la PGU es universal con exclusion del decil superior
# (Ley 21.419 de 29-01-2022, ampliada por la Ley 21.538 de abril de 2023).
pgu_no_contributiva <- 616284
pgu_contributiva    <- 1671628
pgu_total           <- pgu_no_contributiva + pgu_contributiva   # 2.287.912
poblacion_65_chile  <- 3034428

tau_chile_total <- pgu_total / poblacion_65_chile               # 0,75398
tau_chile_nc    <- pgu_no_contributiva / poblacion_65_chile     # 0,20310

# BOLIVIA. Fuente: Gestora Publica y Boletin Economico del Viceministerio de
# Pensiones y Servicios Financieros (MEFP), junio de 2026.
# Poblacion 60+: INE Bolivia, Estimaciones y proyecciones, revision 2025.
# Edad de acceso: 60 anos (Ley 3.791 de 28-11-2007).
beneficiarios_bolivia <- 1239996
poblacion_60_bolivia  <- 1306428
tau_bolivia_total <- beneficiarios_bolivia / poblacion_60_bolivia  # 0,94915

# Descomposicion contributivo / no contributivo.
# El importe medio anual (4.168,20 Bs) es la media ponderada entre no
# rentistas (4.550 Bs) y rentistas (3.900 Bs). Se despeja la proporcion:
importe_bolivia_no_rentista <- 4550
importe_bolivia_rentista    <- 3900
importe_bolivia_medio       <- 4168.20
prop_bolivia_no_rentista <- (importe_bolivia_medio - importe_bolivia_rentista) /
                            (importe_bolivia_no_rentista - importe_bolivia_rentista)
tau_bolivia_nc <- tau_bolivia_total * prop_bolivia_no_rentista     # 0,39163

# =============================================================================
# 4. CUANTIAS ANUALES POR PERCEPTOR, EN EUROS
# =============================================================================

# Espana. IMSERSO: importe medio 557,40 EUR/mes, 14 pagas.
cuantia_mensual_espana <- 557.40
n_pagas <- 14
cuantia_anual_espana <- cuantia_mensual_espana * n_pagas          # 7.803,60

# Chile. Importe medio efectivamente percibido en la modalidad de vejez, junio
# de 2026, obtenido como media ponderada de las dos modalidades de la Pension
# Garantizada Universal con el peso de sus respectivos beneficiarios. Se excluyen
# invalidez y los aportes previsionales solidarios, para mantener la homogeneidad
# con Espana, cuyo analisis se restringe asimismo a la modalidad de jubilacion.
# Fuente: Superintendencia de Pensiones, informe 8 (montos promedio).
#
# Nota: la cifra de 5.780.010,41 (informe 2 de la Superintendencia) es el
# monto TOTAL pagado por el programa, en millones de pesos, y no una cuantia
# individual; se emplea en el guion 14 como gasto del programa.
pgu_nc_mensual <- 234794   # PGU No Contributiva, CLP/mes
pgu_c_mensual  <- 228878   # PGU Contributiva, CLP/mes
cuantia_mensual_chile <- (pgu_no_contributiva * pgu_nc_mensual +
                          pgu_contributiva   * pgu_c_mensual) / pgu_total
n_pagas_chile <- 12
cuantia_anual_chile_clp <- cuantia_mensual_chile * n_pagas_chile   # 2.765.658,78
tc_eur_clp <- 1055.93
cuantia_anual_chile <- cuantia_anual_chile_clp / tc_eur_clp        # 2.619,17

# Bolivia. 4.168,20 BOB anuales (2025). TC: 1 EUR = 13,70 BOB.
cuantia_anual_bolivia_bob <- 4168.20
tc_eur_bob <- 13.70
cuantia_anual_bolivia <- cuantia_anual_bolivia_bob / tc_eur_bob   # 304,25

# =============================================================================
# 5. PARAMETROS MACROECONOMICOS Y FINANCIEROS
# =============================================================================

# Producto interior bruto nominal observado en 2025. INE, Contabilidad Nacional.
# La proyeccion se ancla en el ultimo dato observado (1.687.152 millones en
# 2025), de modo que el peso del gasto sobre el producto sea identico en todos
# los modulos que lo calculan.
PIB_BASE      <- 1.687152e12   # EUR, 2025
PIB_BASE_ANIO <- 2025
crec_pib  <- 0.034         # Nominal anual.

tasa_indexacion_precios <- 0.020   # Escenarios E0-E2, E4-E7.
tasa_indexacion_suficiencia <- 0.035  # Escenario E3, convergencia legal.

# Tipos de descuento para el valor actual actuarial (apartado 3.4.3).
# Los flujos del modelo son NOMINALES: la cuantia se indexa al 2 % anual, que
# es el objetivo de inflacion del Banco Central Europeo. El descuento debe ser
# por tanto tambien nominal.
# El caso central se ancla en el rendimiento de la deuda soberana espanola a
# largo plazo y en la curva libre de riesgo en euros de EIOPA, ambas en el
# entorno del 3 % para los vencimientos largos. Los otros dos tipos equivalen
# a rendimientos reales del 0 % y del 2 % sobre una inflacion del 2 %.
tasas_descuento <- c(0.020, 0.030, 0.040)

# Rejilla para el analisis de sensibilidad del apartado 5.4.4. Permite medir la
# variacion del valor actual actuarial por cada cien puntos basicos, magnitud
# analoga a la duracion de una cartera de pasivos.
rejilla_descuento <- seq(0.010, 0.050, by = 0.005)
tasa_descuento_central <- 0.030

# =============================================================================
# 6. HORIZONTE, CALIBRACION Y SIMULACION
# =============================================================================

anio_base       <- 2025
anio_inicio_proy <- 2024
anio_fin_proy    <- 2050

anio_inicio_calib <- 1991
anio_fin_calib    <- 2023
anios_excluidos   <- 2020     # COVID-19

# Rango de edades del ajuste de mortalidad. Lo usan los guiones 02, 03 y 08.
edad_min <- 60
edad_max <- 100

# Tramos de agregacion. Coinciden con los que publica el Imserso en el perfil
# del pensionista y con los que emplean los guiones 09 a 13 (el ultimo tramo,
# 85 y mas, es abierto).
grupos_quinq    <- c(65, 70, 75, 80, 85)
etiquetas_grupo <- c("65-69", "70-74", "75-79", "80-84", "85+")

n_simulaciones <- 5000
semilla        <- 12345

# =============================================================================
# 7. MATRIZ DE ESCENARIOS (apartado 3.4.8)
# =============================================================================

# Matriz de escenarios (Tabla 11 de la memoria): seis escenarios de la senda
# espanola (E0-E5) y tres escenarios de reforma de la cobertura (R1-R3), tal
# como los calcula el guion 11. La distribucion simulada completa de E0 alimenta
# los intervalos y las medidas de riesgo y no constituye un escenario aparte.
escenarios <- data.frame(
  id          = c("E0","E1","E2","E3","E4","E5","R1","R2","R3"),
  nombre      = c("Base", "Longevidad alta", "Longevidad baja", "Suficiencia",
                  "Tendencial", "Presupuesto constante",
                  "Reforma: cobertura de nivel chileno",
                  "Reforma: cobertura de nivel boliviano",
                  "Reforma: cobertura no contributiva chilena"),
  mortalidad  = c("mediana","p95","p05","mediana","mediana","mediana",
                  "mediana","mediana","mediana"),
  percepcion  = c("constante","constante","constante","constante (x elasticidad)",
                  "tendencial","constante","chile_total","bolivia_total","chile_nc"),
  indexacion  = c("precios 2 %","precios 2 %","precios 2 %","suficiencia 3,5 %",
                  "precios 2 %","endogena (peso constante sobre PIB)",
                  "precios 2 %","precios 2 %","precios 2 %"),
  stringsAsFactors = FALSE
)

# =============================================================================
# 8. RUTAS
# =============================================================================

ruta_proyecto   <- RUTA_PROYECTO
ruta_crudos     <- file.path(RUTA_PROYECTO, "datos/crudos")
ruta_limpios    <- file.path(RUTA_PROYECTO, "datos/limpios")
ruta_resultados <- file.path(RUTA_PROYECTO, "resultados")
ruta_graficos   <- file.path(RUTA_PROYECTO, "graficos")

# =============================================================================
# 9. COMPROBACION DE COHERENCIA INTERNA
#    Como tau se calibra despejando de la identidad
#      perceptores = poblacion * tasa de pobreza * tau,
#    la reconstruccion reproduce el dato observado POR CONSTRUCCION. Esta
#    comprobacion no es una validacion del modelo (no hay dato fuera de la
#    calibracion que contrastar), sino una salvaguarda contra ediciones
#    inconsistentes de los parametros. La memoria la presenta como comprobacion
#    de coherencia (apartado 3.5); la contrastacion externa es la del guion 09
#    frente a la proyeccion oficial del INE y el backtesting de los guiones 05
#    y 07.
# =============================================================================

benef_reconstruidos <- poblacion_65_2025 * tasa_pobreza_65 * tau_espana
error_relativo <- abs(benef_reconstruidos - perceptores_jub_2024) / perceptores_jub_2024

cat("=====================================================\n")
cat("  COMPROBACION DE COHERENCIA INTERNA\n")
cat("=====================================================\n")
cat("Perceptores observados (IMSERSO dic-2024):",
    format(perceptores_jub_2024, big.mark = "."), "\n")
cat("Perceptores reconstruidos por el modelo:  ",
    format(round(benef_reconstruidos), big.mark = "."), "\n")
cat("Error relativo:", sprintf("%.6f %%", error_relativo * 100), "\n")

if (error_relativo > 1e-6) {
  stop("CONFIGURACION INCOHERENTE: el modelo no reproduce los perceptores ",
       "observados. Revisar tau_espana, la poblacion o la tasa de pobreza ",
       "antes de continuar.")
}
cat("Comprobacion superada (reproduccion por construccion; vease el bloque 9).\n\n")

# =============================================================================
# 10. GUARDAR
# =============================================================================

config <- list(
  poblacion_65_2024 = poblacion_65_2024,
  poblacion_65_2025 = poblacion_65_2025,
  tasa_pobreza_65 = tasa_pobreza_65,
  tasa_pobreza_65_h = tasa_pobreza_65_h,
  tasa_pobreza_65_m = tasa_pobreza_65_m,

  perceptores_jub_2025 = perceptores_jub_2025,
  perceptores_jub_2024 = perceptores_jub_2024,
  perceptores_jub_2024_h = perceptores_jub_2024_h,
  perceptores_jub_2024_m = perceptores_jub_2024_m,
  tau_espana_desc = tau_espana_desc,
  perceptores_jub_2025_h = perceptores_jub_2025_h,
  perceptores_jub_2025_m = perceptores_jub_2025_m,

  tau_espana = tau_espana,
  tau_espana_h = tau_espana_h,
  tau_espana_m = tau_espana_m,
  poblacion_65_2025_h = poblacion_65_2025_h,
  poblacion_65_2025_m = poblacion_65_2025_m,
  tau_chile_total = tau_chile_total,
  tau_chile_nc = tau_chile_nc,
  tau_bolivia_total = tau_bolivia_total,
  tau_bolivia_nc = tau_bolivia_nc,
  prop_bolivia_no_rentista = prop_bolivia_no_rentista,

  cuantia_anual_espana = cuantia_anual_espana,
  cuantia_anual_chile = cuantia_anual_chile,
  cuantia_anual_chile_clp = cuantia_anual_chile_clp,
  cuantia_mensual_chile = cuantia_mensual_chile,
  cuantia_anual_bolivia = cuantia_anual_bolivia,
  cuantia_mensual_espana = cuantia_mensual_espana,
  n_pagas = n_pagas,

  PIB_BASE = PIB_BASE,
  PIB_BASE_ANIO = PIB_BASE_ANIO,
  crec_pib = crec_pib,
  tasa_indexacion_precios = tasa_indexacion_precios,
  tasa_indexacion_suficiencia = tasa_indexacion_suficiencia,
  tasas_descuento = tasas_descuento,
  rejilla_descuento = rejilla_descuento,
  tasa_descuento_central = tasa_descuento_central,

  anio_base = anio_base,
  anio_inicio_proy = anio_inicio_proy,
  anio_fin_proy = anio_fin_proy,
  anio_inicio_calib = anio_inicio_calib,
  anio_fin_calib = anio_fin_calib,
  anios_excluidos = anios_excluidos,
  edad_min = edad_min,
  edad_max = edad_max,
  grupos_quinq = grupos_quinq,
  etiquetas_grupo = etiquetas_grupo,
  n_simulaciones = n_simulaciones,
  semilla = semilla,
  escenarios = escenarios,

  ruta_proyecto = ruta_proyecto,
  ruta_crudos = ruta_crudos,
  ruta_limpios = ruta_limpios,
  ruta_resultados = ruta_resultados,
  ruta_graficos = ruta_graficos,

  version = "01-09-2026 rev.9",
  nota = "Parametros verificados contra INE (56937, ECV 2025), IMSERSO (Perfil dic-2025), Superintendencia de Pensiones de Chile (jun-2026) y MEFP Bolivia (jun-2026)."
)

saveRDS(config, file.path(ruta_proyecto, "config.rds"))

cat("PARAMETROS DE ELEGIBILIDAD\n")
cat("  tau Espana calibracion (dic-2024):", sprintf("%.5f", tau_espana), "\n")
cat("  tau Espana descriptivo (dic-2025):", sprintf("%.5f", tau_espana_desc), "\n")
cat("  tau Chile total   :", sprintf("%.5f", tau_chile_total), "\n")
cat("  tau Chile no contr:", sprintf("%.5f", tau_chile_nc), "\n")
cat("  tau Bolivia total :", sprintf("%.5f", tau_bolivia_total), "\n")
cat("  tau Bolivia no rent:", sprintf("%.5f", tau_bolivia_nc), "\n\n")

cat("CUANTIAS ANUALES (EUR)\n")
cat("  Espana :", sprintf("%10.2f", cuantia_anual_espana), "\n")
cat("  Chile  :", sprintf("%10.2f", cuantia_anual_chile), "\n")
cat("  Bolivia:", sprintf("%10.2f", cuantia_anual_bolivia), "\n")
cat("  (Chile: media ponderada de PGU no contributiva y contributiva,",
    sprintf("%.0f CLP/mes)", cuantia_mensual_chile), "\n\n")

cat("  tau Espana hombres:", sprintf("%.5f", tau_espana_h), "\n")
cat("  tau Espana mujeres:", sprintf("%.5f", tau_espana_m), "\n\n")
cat("config.rds guardado en", ruta_proyecto, "\n")
