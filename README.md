# Modelo actuarial de proyección del gasto en pensiones no contributivas de jubilación en España, 2025-2050

Código, datos y resultados del Trabajo Fin de Máster **«Análisis comparativo de los sistemas de pensiones no contributivas en Bolivia, Chile y España y formulación de un modelo actuarial de proyección del gasto para el caso español, 2025-2050»**.

**Autora:** Cecilia Ely Paye Larico
**Director:** Iván de la Fuente Merencio
**Titulación:** Máster Universitario en Ciencias Actuariales y Financieras
**Centro:** Facultad de Ciencias Económicas y Empresariales, Universidad de Alcalá
**Fecha:** septiembre de 2026

---

## Qué contiene este repositorio

El modelo se organiza en **quince guiones de R encadenados**, numerados de `00` a `14`. Cada uno lee los resultados que el anterior guardó en disco y escribe los suyos, de modo que la numeración no es orientativa sino una dependencia real de ejecución.

| Carpeta o fichero | Contenido |
|---|---|
| `scripts/` | Los quince guiones de R, de `00_setup.R` a `14_comparacion_internacional.R` |
| `TFM_Pensiones.Rproj` | Proyecto de RStudio |
| `datos/fuentes/` | Tablas originales de INE, IMSERSO y AIReF, tal como se descargaron. Con `FUENTES.txt`, que indica de qué fichero de entrada procede cada una |
| `datos/crudos/` | Ficheros de mortalidad de la Human Mortality Database. **Se entrega vacía**, con solo un `LEEME.txt`; véase el aviso más abajo |
| `datos/limpios/` | Ficheros CSV de entrada de los guiones 09, 10 y 14: población de arranque a edad simple, diferencial de mortalidad por sexo, entradas a los 65 años, proyección de referencia del INE, series del IMSERSO y datos de la comparación internacional |
| `tablas/` | Tablas de resultados exportadas por los guiones 13 y 14 (T01 a T17) |
| `graficos/` | Las 23 figuras del trabajo (`fig01` a `fig23`) |
| `salidas_consola.txt` | Transcripción completa de la consola en una ejecución íntegra de la cadena |
| `sessionInfo.txt` | Versiones de R y de los paquetes con las que se obtuvieron las cifras de la memoria |

El modelo se estructura en tres módulos encadenados —demográfico, de beneficiarios y financiero— sobre los que actúa una capa transversal de simulación de Monte Carlo con 5.000 trayectorias.

Los resultados intermedios (`resultados/`, ficheros `.rds`) no se incluyen: son regenerables ejecutando la cadena, y el mayor de ellos ocupa 80,7 MB.

---

## Aviso importante sobre los datos de mortalidad

**Los ficheros de defunciones y exposición al riesgo de España no se redistribuyen en este repositorio**, conforme a las condiciones de uso de la Human Mortality Database. La carpeta `datos/crudos/` contiene únicamente un `LEEME.txt` con estas mismas instrucciones, y el guion `01_datos_mortalidad.R` se detendrá con un mensaje de error si no encuentra los ficheros.

Para ejecutar el modelo deben descargarse manualmente:

1. Acceder a [www.mortality.org](https://www.mortality.org) con un usuario registrado. El registro es gratuito, pero **es obligatorio**: sin iniciar sesión, el enlace de descarga devuelve una página web en lugar del fichero de datos, y el guion 01 lo detectará y avisará de ello expresamente.
2. Seleccionar España (*Spain*).
3. Descargar los **dos ficheros de la columna 1x1**:
   - *Deaths* → `Deaths_1x1.txt`
   - *Exposure-to-risk* → `Exposures_1x1.txt`
4. Copiarlos en `datos/crudos/` **renombrados** como:
   - `defunciones.txt`
   - `exposicion.txt`

Los nombres deben escribirse exactamente así, en minúsculas y sin duplicar la extensión. La descarga automática mediante `hmd.mx()` del paquete `demography` **no se emplea**, porque la HMD modificó su sistema de acceso y la función dejó de ser fiable.

El resto de las fuentes sí se incluyen. Las tablas originales del INE, el IMSERSO y la AIReF están en `datos/fuentes/`, y los ficheros ya procesados que leen los guiones, en `datos/limpios/`. El fichero `datos/fuentes/FUENTES.txt` establece la correspondencia entre unas y otros, de modo que el paso que va de la estadística oficial al fichero de entrada del modelo pueda auditarse.

---

## Ejecución

**Requisito previo:** R (versión 4.0 o superior) y RStudio. Las versiones exactas con las que se obtuvieron los resultados de la memoria figuran en `sessionInfo.txt`.

**Paquetes necesarios.** Instálense antes de la primera ejecución:

    install.packages(c("StMoMo", "forecast", "ggplot2", "dplyr", "tidyr", "scales", "writexl"))

`writexl` es opcional: solo lo usa el guion 07 para exportar una tabla a Excel, y el propio guion comprueba si está disponible antes de llamarlo.

### Orden de ejecución

1. **Abrir el proyecto.** Doble clic en `TFM_Pensiones.Rproj`, o bien *File → Open Project* en RStudio. Trabajar con el proyecto abierto no es opcional: fija el directorio de trabajo y evita los problemas de rutas relativas. Los guiones asumen además que la carpeta del proyecto es `~/TFM_Pensiones`; si se clona en otra ubicación, debe ajustarse la variable `RUTA_PROYECTO` que aparece al comienzo de cada guion.

2. **Ejecutar los quince guiones en orden, del `00` al `14`, cada uno con Source.** Es decir: abrir el guion en el editor y pulsar el botón **Source** (o Ctrl+Shift+S en Windows y Linux, Cmd+Shift+S en macOS). No basta con ejecutar líneas sueltas con Ctrl+Enter.

   | Orden | Guion | Qué hace |
   |---|---|---|
   | 1 | `00_setup.R` | Parámetros, fuentes y matriz de escenarios. Escribe `config.rds` |
   | 2 | `01_datos_mortalidad.R` | Lectura de los ficheros 1x1 de la HMD |
   | 3 | `02_limpieza_preparacion.R` | Recorte a edades 60-100 y años 1991-2023; exclusión de 2020 por matriz de pesos |
   | 4 | `03_exploratorio.R` | Análisis descriptivo previo a la modelización |
   | 5 | `04_lee_carter.R` | Ajuste de Lee-Carter por máxima verosimilitud de Poisson |
   | 6 | `05_diagnosticos_lc.R` | Residuos, efecto cohorte y validación fuera de muestra |
   | 7 | `06_cairns_blake_dowd.R` | Ajuste del modelo de contraste |
   | 8 | `07_comparacion_modelos.R` | Selección del modelo base con criterios declarados |
   | 9 | `08_proyeccion_montecarlo.R` | Proyección estocástica con 5.000 trayectorias |
   | 10 | `09_modulo_demografico.R` | Proyección por componentes de cohorte, por sexo y edad simple |
   | 11 | `10_modulo_elegibilidad.R` | Tasa de percepción y escenarios de cobertura |
   | 12 | `11_modulo_financiero.R` | Gasto, peso sobre el PIB, valor actual actuarial y medidas de riesgo |
   | 13 | `12_descomposicion.R` | Descomposición del crecimiento del gasto por el índice LMDI |
   | 14 | `13_resultados_finales.R` | Exportación de las tablas T01 a T17 y contraste de hipótesis |
   | 15 | `14_comparacion_internacional.R` | Indicadores comparados de los tres sistemas |

3. **No reiniciar R entre guiones.** Cada guion vuelve a leer de disco lo que necesita, pero la sesión debe mantenerse abierta para que los paquetes permanezcan cargados y para que `sessionInfo.txt` pueda generarse al final con la lista completa.

Ejecutar un guion fuera de orden produce un error de fichero no encontrado, no un resultado silenciosamente equivocado. Es un comportamiento buscado.

La ejecución completa requiere pocos minutos en un ordenador personal corriente. El paso más costoso es el bootstrap semiparamétrico del guion 08, que ronda los cuarenta segundos.

---

## Reproducibilidad

La semilla aleatoria está fijada en `00_setup.R` (`semilla <- 12345`) y se reestablece antes de cada llamada que emplee aleatoriedad, de modo que **una ejecución íntegra de la cadena reproduce exactamente las cifras de la memoria**.

Quien reejecute el modelo puede contrastar su salida con estas cifras de referencia, tomadas de la ejecución que respalda la memoria y transcritas íntegramente en `salidas_consola.txt`:

| Magnitud | Valor de referencia |
|---|---|
| Deriva del índice temporal de Lee-Carter | −0,6226 por año |
| Población de 65 y más años en 2050 | 17.415.426 · IC 95 % [16.009.224 · 18.602.068] |
| Esperanza de vida a los 65 años en 2050 | 24,51 años · IC 95 % [21,87 · 26,94] |
| Beneficiarios en 2050, escenario base | 392.973 |
| Gasto en 2050, escenario base | 5,03 miles de millones de euros · 0,129 % del PIB |
| Valor actual actuarial al 3 % | 56,8 miles de millones de euros |
| Descomposición LMDI 2025-2050 | escala 53,0 % · cuantía 48,9 % · estructura −1,9 % |

El código incorpora además cinco comprobaciones automáticas, tres de ellas programadas como condiciones de parada, para que una incoherencia detenga la ejecución en lugar de propagarse a los resultados:

| Guion | Qué verifica | Tipo |
|---|---|---|
| `00` | Los parámetros reproducen los perceptores observados de diciembre de 2024 | Parada |
| `09` | La población de arranque coincide exactamente con la publicada por el INE | Parada |
| `10` | El módulo de elegibilidad reproduce los perceptores registrados con error inferior al 1 % | Parada |
| `12` | La descomposición del crecimiento del gasto no deja residuo | Aviso |
| `14` | El gasto declarado de cada país es coherente con el producto de beneficiarios por cuantía | Aviso |

A ellas se añaden dos validaciones frente a datos externos que no participaron en la construcción del modelo: el contraste de la proyección demográfica con la proyección oficial del INE (guion 09) y la validación fuera de muestra de ambos modelos de mortalidad (guiones 05 y 07).

---

## Cómo citar

> Paye Larico, C. E. (2026). *Análisis comparativo de los sistemas de pensiones no contributivas en Bolivia, Chile y España y formulación de un modelo actuarial de proyección del gasto para el caso español, 2025-2050* [Trabajo Fin de Máster, Universidad de Alcalá]. Director: Iván de la Fuente Merencio.

El código se publica con fines académicos y de verificación de los resultados de la memoria. Las tablas de `datos/fuentes/` proceden de organismos públicos españoles y se reproducen citando su origen. Los datos de la Human Mortality Database se rigen por las condiciones de uso de dicha base y no forman parte de esta publicación.
