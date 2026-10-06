# MPDH — Biomasa Desovante Centro-Sur

Aplicación Shiny interactiva para estimar y visualizar la biomasa desovante de anchoveta y sardina común en la zona centro-sur de Chile, mediante el **Método de Producción Diaria de Huevos (MPDH)**.

Desarrollado por Luis A. Cubillos - CEPMAR SpA

---

## Estructura del repositorio

```
AnalisisR/
├── app_MPDH_Biomasa.R        # Aplicación Shiny principal
├── Data/
│   └── MPDH_datos.xlsx       # Libro de datos consolidado (lances + parametros)
├── Rscripts/                 # Scripts R de análisis (estimador, CV, etc.)
└──  Figs/                    # Figuras generadas
```

---

## Requisitos

R ≥ 4.2 y los siguientes paquetes:

```r
install.packages(c(
  "shiny", "bslib", "readxl",
  "dplyr", "tidyr", "ggplot2", "scales",
  "officer", "flextable"
))
```

---

## Datos de entrada

La app lee un único archivo Excel: **`Data/MPDH_datos.xlsx`**, con dos hojas obligatorias y una opcional:

| Hoja | Contenido | Columnas clave |
|------|-----------|----------------|
| `lances` | Parámetros reproductivos por lance | `year`, `Especie`, `Zona`, `Haul`, `m`, `F`, `W`, `S`, … |
| `parametros` | Promedios y CV por zona y año | `year`, `Especie`, `Zona`, `Parm`, `Mean`, `CV` |
| `historico` *(opcional)* | Serie histórica de biomasa total (t) | `year`, `Anchoveta`, `Sardina_comun` |

> En `lances` también se acepta el nombre antiguo `Zone`; la app lo renombra a `Zona` al cargar.

### Agregar un año nuevo (ej. 2026)

1. Abrir `Data/MPDH_datos.xlsx`.
2. En la hoja `lances`, agregar las filas del crucero 2026 con `year = 2026`.
3. En la hoja `parametros`, agregar los parámetros estimados con `year = 2026`.
4. Guardar y recargar el archivo en la app — el nuevo año aparece automáticamente.

> La serie histórica se lee desde la hoja `historico`.  
> Los años presentes en `parametros` se calculan y agregan automáticamente al gráfico histórico, destacados con un punto de color.

---

## Ejecutar la app localmente

```r
# Desde RStudio con AnalisisR.Rproj abierto:
shiny::runApp("app_MPDH_Biomasa.R")
```

O desde la consola R:

```r
setwd("ruta/a/AnalisisR")
shiny::runApp("app_MPDH_Biomasa.R")
```

---

## Pestañas de la app

| Pestaña | Descripción |
|---------|-------------|
| **Serie histórica** | Gráfico de barras 2002–presente. Opción de recortar al último año analizado y destacar con puntos los años con análisis completo. |
| **Por zona** | Biomasa Centro vs. Sur con IC 95% (método delta). Value boxes con resumen del último año. |
| **Parámetros** | Tabla de parámetros MPDH (P₀, W, F, S, A) y gráfico de descomposición del CV²(B) por componente. |
| **Reporte** | Genera y descarga la Tabla R4 (biomasa y CV por zona) y Tabla R5 (estructura de varianza-covarianza) en formato `.docx` o `.html`, con interpretación automática. |

---

## Despliegue en shinyapps.io

```r
install.packages("rsconnect")
library(rsconnect)

# Autenticar (solo la primera vez — obtener token en shinyapps.io)
rsconnect::setAccountInfo(name = "tu-cuenta",
                          token = "TU_TOKEN",
                          secret = "TU_SECRET")

# Desplegar
rsconnect::deployApp(
  appDir  = ".",
  appName = "MPDH-BiomassaDesovante",
  appFiles = c("app_MPDH_Biomasa.R", "Data/MPDH_datos.xlsx")
)
```

Una vez desplegada, la app puede embeberse en cualquier página web:

```html
<iframe src="https://tu-cuenta.shinyapps.io/MPDH-BiomassaDesovante/"
        width="100%" height="850px" frameborder="0">
</iframe>
```

---

## Fórmula MPDH

$$B = \frac{P_0 \cdot A_d \cdot W}{F \cdot S \cdot R} \quad \text{[toneladas]}$$

La varianza se descompone mediante el método delta con términos de covarianza entre F, W y S:

$$CV^2(B) = CV^2(P_0) + CV^2(W) + CV^2(F) + CV^2(S) + CV^2(R) - \frac{2\,\text{Cov}(F,W)}{FW} - \frac{2\,\text{Cov}(W,S)}{WS} + \frac{2\,\text{Cov}(F,S)}{FS}$$

Detalle de fórmulas, unidades y configuración del Excel en el **[tutorial de uso](https://luisacubillos.github.io/BiomDesMPHSur/)** (fuente: `Tutorial_App_MPDH.Rmd`).

Para actualizar la versión publicada, compile el tutorial directo en `docs/`:

```r
rmarkdown::render("Tutorial_App_MPDH.Rmd", output_file = "index.html", output_dir = "docs")
```

---

## Referencia

Stauffer, G. & Picquelle, S. (1980). Estimates of the 1980 spawning biomass of the central subpopulation of northern anchovy. *NOAA Admin. Rep.* NMFS SWFC-81-4.

---

*Luis Cubillos — CEPMAR SpA, 2026*
