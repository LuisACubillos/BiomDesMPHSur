# ============================================================
# app_MPDH_Biomasa.R  v4
# Estimador interactivo de biomasa desovante — MPDH Centro-Sur
#
# DATOS: MPDH_datos.xlsx  (hojas: lances, parametros, historico)
#   Seleccione el archivo desde cualquier ubicación con el botón
#   "Abrir Excel…" en la barra superior.
#
# Serie histórica: hoja "historico" (opcional; year, Anchoveta, Sardina_comun).
# Los años presentes en "parametros" se calculan y agregan
# automáticamente, con puntos de color en el gráfico histórico.
#
# Ejecutar desde RStudio con el proyecto AnalisisR.Rproj abierto.
# ============================================================

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(officer)
library(flextable)

# ── Carga de datos desde ruta arbitraria ─────────────────────────────────────
load_data_from_path <- function(path) {
  sheets_found <- tryCatch(excel_sheets(path), error = function(e) character(0))
  out <- list(sheets = sheets_found, errors = character(0), path = path)

  read_sheet <- function(sh) {
    tryCatch(
      read_excel(path, sheet = sh, na = "NA"),
      error = function(e) {
        out$errors <<- c(out$errors, paste(sh, ":", e$message))
        NULL
      }
    )
  }

  for (sh in c("lances", "parametros")) {
    if (sh %in% sheets_found) {
      out[[sh]] <- read_sheet(sh)
    } else {
      out$errors <- c(out$errors, paste("Hoja no encontrada:", sh))
      out[[sh]]  <- NULL
    }
  }

  # Nombre de zona uniforme: se acepta "Zone" (versión antigua) o "Zona"
  for (sh in c("lances", "parametros")) {
    d <- out[[sh]]
    if (!is.null(d) && "Zone" %in% names(d) && !"Zona" %in% names(d)) {
      out[[sh]] <- rename(d, Zona = Zone)
    }
  }

  # Serie histórica (opcional): year, Anchoveta, Sardina_comun
  out$historico <- tibble(year = numeric(0), Anchoveta = numeric(0),
                          `Sardina común` = numeric(0))
  if ("historico" %in% sheets_found) {
    h <- read_sheet("historico")
    if (!is.null(h)) {
      names(h)[names(h) %in% c("Sardina_comun", "Sardina", "Sardina comun")] <- "Sardina común"
      if (all(c("year", "Anchoveta", "Sardina común") %in% names(h))) {
        out$historico <- h %>%
          select(year, Anchoveta, `Sardina común`) %>%
          mutate(across(everything(), as.numeric))
      } else {
        out$errors <- c(out$errors,
                        "historico: se esperan columnas year, Anchoveta, Sardina_comun")
      }
    }
  } else {
    out$errors <- c(out$errors, "Hoja no encontrada: historico (serie histórica vacía)")
  }
  out
}

# ── Cálculo MPDH para una zona (común a todas las pestañas) ──────────────────
# Términos del CV²(B) por método delta, en el orden usado en tablas y gráficos
CV_TERMS <- c("CV²(P₀)", "CV²(W)", "CV²(F)", "CV²(S)", "CV²(R)",
              "-2·Cov(F,W)/(FW)", "-2·Cov(W,S)/(WS)", "2·Cov(F,S)/(FS)")

# d: filas de "parametros" de un año-especie-zona; dl: lances correspondientes.
# Devuelve NULL (con warning) si faltan parámetros o están duplicados.
# Con menos de 2 lances, B se calcula pero terms y CV2 quedan NA.
mpdh_zone <- function(d, dl, label = "") {
  # Cada parámetro requerido debe aparecer exactamente una vez
  req_parms <- c("P0", "AD", "W", "F", "S", "R")
  n_parm    <- sapply(req_parms, function(p) sum(d$Parm == p & !is.na(d$Mean)))
  if (any(n_parm != 1)) {
    warning(sprintf(
      "%s: parámetros faltantes o duplicados (%s); zona omitida",
      label, paste(names(n_parm)[n_parm != 1], collapse = ", ")
    ))
    return(NULL)
  }

  gv  <- function(p) as.numeric(d$Mean[d$Parm == p & !is.na(d$Mean)])
  gcv <- function(p) as.numeric(d$CV[d$Parm == p & !is.na(d$Mean)])

  P0 <- gv("P0"); CVP <- gcv("P0")
  Ad <- gv("AD")
  W  <- gv("W");  cvW <- gcv("W")
  F_ <- gv("F");  cvF <- gcv("F")
  S  <- gv("S");  cvS <- gcv("S")
  R  <- gv("R");  cvR <- gcv("R")
  if (is.na(cvR)) cvR <- 0   # R fijo (sin incertidumbre) si no se informa CV

  B <- P0 * Ad * W / (F_ * S * R)

  terms <- setNames(rep(NA_real_, length(CV_TERMS)), CV_TERMS)
  if (nrow(dl) > 1) {
    n  <- nrow(dl); mp <- mean(dl$m)
    m  <- dl$m; Fv <- dl$F; Wv <- dl$W; Sv <- dl$S
    wt_cov <- function(a, ma, b, mb)
      sum(m^2 * (a - ma) * (b - mb)) / (mp^2 * n * (n - 1))
    cFW <- wt_cov(Fv, F_, Wv, W)
    cFS <- wt_cov(Fv, F_, Sv, S)
    cWS <- wt_cov(Wv, W,  Sv, S)
    terms[] <- c(CVP^2, cvW^2, cvF^2, cvS^2, cvR^2,
                 -2*cFW/(F_*W), -2*cWS/(W*S), 2*cFS/(F_*S))
  }

  list(B = B, terms = terms, CV2 = max(sum(terms), 0))
}

# ── Cálculo de biomasa por zona ───────────────────────────────────────────────
calc_zone_biomass <- function(params_all, lances_all) {
  years <- sort(unique(params_all$year))
  out   <- list()

  for (yr in years) {
    params <- params_all %>% filter(year == yr)
    lances <- lances_all %>% filter(year == yr)

    for (spp in c("Anchoveta", "Sardina")) {
      for (zona in c("Centro", "Sur")) {
        d <- params %>% filter(Especie == spp, Zona == zona)
        if (nrow(d) == 0) next

        dl  <- lances %>% filter(Especie == spp, Zona == zona)
        res <- mpdh_zone(d, dl, paste(yr, spp, zona))
        if (is.null(res)) next

        out[[length(out) + 1]] <- data.frame(
          year    = yr,
          Especie = ifelse(spp == "Sardina", "Sardina común", spp),
          Zona    = zona,
          B_ton   = round(res$B),
          CV      = round(sqrt(res$CV2) * 100, 1),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(out) == 0) return(data.frame())
  bind_rows(out) %>%
    mutate(
      IC_lo = pmax(0, round(B_ton * (1 - 1.96 * CV / 100))),
      IC_hi = round(B_ton * (1 + 1.96 * CV / 100))
    )
}

# ── Paletas ───────────────────────────────────────────────────────────────────
col_spp  <- c("Anchoveta" = "#2166ac", "Sardina común" = "#d6604d")
col_zona <- c("Centro" = "#1a9641", "Sur" = "#e6850e")
fmt_t    <- function(x) format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = tags$span(
    tags$img(src = "Logo_cepmar_final.jpg",
             height = "38px",
             style = "margin-right:10px; vertical-align:middle;"),
    "MPDH — Biomasa Desovante Centro-Sur"
  ),
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2166ac"),
  fillable = FALSE,
  footer = tags$footer(
    style = paste(
      "background:#f0f4f8; border-top:1px solid #d0dbe8;",
      "padding:8px 20px; font-size:12px; color:#555;",
      "display:flex; align-items:center; gap:16px;"
    ),
    tags$img(src = "Logo_cepmar_final.jpg", height = "24px"),
    tags$span(
      "Luis A. Cubillos",
      tags$span(style = "color:#aaa; margin:0 6px", "|"),
      "CEPMAR SpA",
      tags$span(style = "color:#aaa; margin:0 6px", "|"),
      "Contacto: ",
      tags$a(href = "mailto:ad.cepmar@gmail.com",
             style = "color:#2166ac;",
             "ad.cepmar@gmail.com")
    )
  ),

  # Botón de carga en la barra superior (derecha)
  nav_spacer(),
  nav_item(
    fileInput(
      "data_file",
      label    = NULL,
      buttonLabel = tagList(icon("folder-open"), " Abrir Excel…"),
      placeholder = "Ningún archivo seleccionado",
      accept   = c(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
      width    = "320px"
    )
  ),

  # ── Tab 1: Serie histórica ─────────────────────────────────────────────────
  nav_panel(
    title = "Serie histórica",
    uiOutput("tab_hist_ui")
  ),

  # ── Tab 2: Por zona ────────────────────────────────────────────────────────
  nav_panel(
    title = "Por zona",
    uiOutput("tab_zona_ui")
  ),

  # ── Tab 3: Parámetros ─────────────────────────────────────────────────────
  nav_panel(
    title = "Parámetros",
    uiOutput("tab_params_ui")
  ),

  # ── Tab 4: Reporte ─────────────────────────────────────────────────────────
  nav_panel(
    title = "Reporte",
    uiOutput("tab_reporte_ui")
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactivos de datos ─────────────────────────────────────────────────────

  data_r <- reactive({
    req(input$data_file)
    load_data_from_path(input$data_file$datapath)
  })

  zone_df_r <- reactive({
    d <- data_r()
    req(!is.null(d$parametros), !is.null(d$lances))
    calc_zone_biomass(d$parametros, d$lances)
  })

  years_analyzed_r <- reactive({
    df <- zone_df_r()
    if (nrow(df) == 0) integer(0) else sort(unique(df$year))
  })

  hist_df_r <- reactive({
    zone_df <- zone_df_r()
    recent <- if (nrow(zone_df) > 0) {
      zone_df %>%
        group_by(year, Especie) %>%
        summarise(B_ton = sum(B_ton, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = Especie, values_from = B_ton)
    } else data.frame()

    base <- data_r()$historico %>% filter(!(year %in% recent$year))
    bind_rows(base, recent) %>% arrange(year)
  })

  year_range_r <- reactive({
    df <- hist_df_r()
    if (nrow(df) == 0) c(2002L, 2025L) else range(df$year)
  })

  # (controles dinámicos poblados directamente en cada renderUI)

  # ── Pantalla de bienvenida (sin archivo) ───────────────────────────────────

  no_data_card <- card(
    class = "mt-5 mx-auto",
    style = "max-width:520px;",
    card_header("Sin datos cargados"),
    card_body(
      p("Use el botón ", tags$b("Abrir Excel…"), " de la barra superior para seleccionar el archivo ",
        tags$code("MPDH_datos.xlsx"), " desde cualquier ubicación."),
      p("El archivo debe contener las hojas ", tags$code("lances"), " y ",
        tags$code("parametros"), " con columna ", tags$code("year"),
        ", y opcionalmente ", tags$code("historico"), " con la serie histórica.")
    )
  )

  # ── Tab 1 UI / outputs ─────────────────────────────────────────────────────

  output$tab_hist_ui <- renderUI({
    if (is.null(input$data_file)) return(no_data_card)

    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h6("Filtros", class = "text-muted fw-bold"),
        checkboxGroupInput("h_spp", "Especie:",
                           choices  = c("Anchoveta", "Sardina común"),
                           selected = c("Anchoveta", "Sardina común")),
        sliderInput("h_years", "Rango de años:",
                    min   = year_range_r()[1], max = year_range_r()[2],
                    value = year_range_r(), sep = "", step = 1),
        checkboxInput("h_log",   "Escala logarítmica (eje Y)",          value = FALSE),
        checkboxInput("h_trim",  "Recortar al último año analizado",     value = FALSE),
        checkboxInput("h_dots",  "Destacar años analizados (punto)",     value = TRUE),
        hr(),
        p(class = "text-muted small",
          "Años sin datos (NA) no se grafican.",
          br(), "Puntos = años calculados desde ", tags$code("parametros"), ".")
      ),
      card(
        card_header("Biomasa desovante total por especie (toneladas)"),
        plotOutput("hist_plot", height = "440px")
      ),
      card(
        card_header("Tabla de valores"),
        div(style = "overflow-x:auto", tableOutput("hist_table"))
      )
    )
  })

  hist_long_r <- reactive({
    req(input$data_file)
    yrs      <- years_analyzed_r()
    yr_max   <- if (isTRUE(input$h_trim) && length(yrs) > 0) max(yrs) else input$h_years[2]
    hist_df_r() %>%
      filter(year >= input$h_years[1], year <= yr_max) %>%
      pivot_longer(c("Anchoveta", "Sardina común"),
                   names_to = "Especie", values_to = "B_ton") %>%
      filter(Especie %in% input$h_spp, !is.na(B_ton))
  })

  output$hist_plot <- renderPlot({
    df   <- hist_long_r()
    validate(need(nrow(df) > 0, "Sin datos para los filtros seleccionados."))
    yrs_an <- years_analyzed_r()
    df_dots <- df %>% filter(year %in% yrs_an)
    yr_rng  <- range(df$year)

    p <- ggplot(df, aes(x = year, y = B_ton, fill = Especie)) +
      geom_col(position = position_dodge2(width = 0.9, preserve = "single"),
               width = 0.75, alpha = 0.9, color = "white", linewidth = 0.2) +
      scale_fill_manual(values = col_spp) +
      scale_x_continuous(breaks = seq(yr_rng[1], yr_rng[2], 2)) +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                         expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "Biomasa desovante (t)", fill = NULL,
           caption = "Fuente: IFOP–UdeC, MPDH 2002–presente") +
      theme_minimal(base_size = 13) +
      theme(legend.position   = "top",
            axis.text.x       = element_text(angle = 45, hjust = 1),
            panel.grid.minor  = element_blank(),
            plot.caption      = element_text(color = "gray50"))

    if (isTRUE(input$h_dots) && nrow(df_dots) > 0) {
      p <- p +
        geom_point(
          data     = df_dots,
          aes(x = year, y = B_ton, color = Especie,
              group = Especie, shape = "Año analizado"),
          position = position_dodge2(width = 0.9, preserve = "single"),
          size = 3.8, stroke = 1.3
        ) +
        scale_color_manual(
          name   = NULL,
          values = c("Anchoveta" = "#08306b", "Sardina común" = "#67000d"),
          guide  = guide_legend(order = 2)
        ) +
        scale_shape_manual(
          name   = NULL,
          values = c("Año analizado" = 21),
          guide  = guide_legend(order = 3,
                                override.aes = list(fill = "white", color = "gray30", size = 3.8))
        ) +
        guides(fill = guide_legend(order = 1))
    }

    if (isTRUE(input$h_log))
      p <- p + scale_y_log10(labels = label_number(big.mark = ".", decimal.mark = ","),
                              expand = expansion(mult = c(0.02, 0.12)))
    p
  }, res = 110)

  output$hist_table <- renderTable({
    req(input$data_file)
    hist_df_r() %>%
      filter(year >= input$h_years[1], year <= input$h_years[2]) %>%
      rename(Año = year) %>%
      mutate(
        Anchoveta       = ifelse(is.na(Anchoveta),       "—", fmt_t(Anchoveta)),
        `Sardina común` = ifelse(is.na(`Sardina común`), "—", fmt_t(`Sardina común`))
      )
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "lrr")

  # ── Tab 2 UI / outputs ─────────────────────────────────────────────────────

  output$tab_zona_ui <- renderUI({
    if (is.null(input$data_file)) return(no_data_card)

    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h6("Filtros", class = "text-muted fw-bold"),
        selectInput("z_spp", "Especie:",
                    choices  = c("Anchoveta", "Sardina común"),
                    selected = "Anchoveta"),
        checkboxGroupInput("z_years", "Año(s):",
                           choices  = years_analyzed_r(),
                           selected = years_analyzed_r()),
        checkboxGroupInput("z_zones", "Zona:",
                           choices  = c("Centro", "Sur"),
                           selected = c("Centro", "Sur")),
        hr(),
        p(class = "text-muted small",
          "Barras de error: IC 95% (método delta).", br(), "R = 0,5 (fijo).")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(title = "Zona Centro (último año)",
                  value = textOutput("vb_centro"), theme = "primary"),
        value_box(title = "Zona Sur (último año)",
                  value = textOutput("vb_sur"),
                  theme = value_box_theme(bg = "#e6850e", fg = "white")),
        value_box(title = "Total Centro + Sur",
                  value = textOutput("vb_total"), theme = "success")
      ),
      card(card_header(textOutput("zone_title")),
           plotOutput("zone_plot", height = "420px")),
      card(card_header("Detalle por zona y año"),
           div(style = "overflow-x:auto", tableOutput("zone_table")))
    )
  })

  zone_sel_r <- reactive({
    req(input$data_file, input$z_spp, input$z_years, input$z_zones)
    zone_df_r() %>%
      filter(Especie %in% input$z_spp,
             year    %in% as.integer(input$z_years),
             Zona    %in% input$z_zones)
  })

  last_year_r <- reactive({
    df <- zone_sel_r()
    if (nrow(df) == 0) return(NULL)
    df %>% filter(year == max(year))
  })

  output$vb_centro <- renderText({
    d <- last_year_r(); if (is.null(d)) return("—")
    v <- d %>% filter(Zona == "Centro") %>% pull(B_ton)
    if (length(v) == 0) "—" else paste0(fmt_t(v), " t")
  })
  output$vb_sur <- renderText({
    d <- last_year_r(); if (is.null(d)) return("—")
    v <- d %>% filter(Zona == "Sur") %>% pull(B_ton)
    if (length(v) == 0) "—" else paste0(fmt_t(v), " t")
  })
  output$vb_total <- renderText({
    d <- last_year_r(); if (is.null(d)) return("—")
    paste0(fmt_t(sum(d$B_ton)), " t  (", unique(d$year), ")")
  })

  output$zone_title <- renderText({
    req(input$z_spp)
    paste("Biomasa por zona —", input$z_spp)
  })

  output$zone_plot <- renderPlot({
    df <- zone_sel_r()
    validate(need(nrow(df) > 0, "Sin datos para los filtros seleccionados."))
    df <- df %>% mutate(yr_lbl = as.factor(year))
    totals <- df %>%
      group_by(yr_lbl) %>%
      summarise(total = sum(B_ton), top_ci = max(IC_hi, na.rm = TRUE), .groups = "drop")

    p <- ggplot(df, aes(x = yr_lbl, y = B_ton, fill = Zona)) +
      geom_col(position = position_dodge(width = 0.75),
               width = 0.65, alpha = 0.9, color = "white", linewidth = 0.2) +
      geom_errorbar(aes(ymin = IC_lo, ymax = IC_hi),
                    position = position_dodge(width = 0.75),
                    width = 0.28, color = "gray30", linewidth = 0.6) +
      geom_text(data = totals,
                aes(x = yr_lbl, y = top_ci * 1.05,
                    label = paste0("Total: ", fmt_t(total), " t")),
                inherit.aes = FALSE, size = 3.6, fontface = "bold", color = "gray25") +
      scale_fill_manual(values = col_zona) +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                         expand = expansion(mult = c(0, 0.18))) +
      labs(x = "Año", y = "Biomasa desovante (t)", fill = "Zona",
           caption = "Barras de error: IC 95% (método delta). R = 0,5 (fijo).") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 12),
            plot.caption = element_text(color = "gray50"))

    if (length(unique(df$year)) == 1)
      p <- p + geom_text(aes(label = paste0(fmt_t(B_ton), " t\nCV: ", CV, "%"), group = Zona),
                         position = position_dodge(width = 0.75),
                         vjust = -0.8, size = 3.5, color = "gray20")
    p
  }, res = 110)

  output$zone_table <- renderTable({
    df <- zone_sel_r()
    validate(need(nrow(df) > 0, "Sin datos."))
    detail <- df %>%
      transmute(Año = year, Zona,
                `Biomasa (t)` = fmt_t(B_ton),
                `CV (%)`      = ifelse(is.na(CV), "—", as.character(CV)),
                `IC95 inf.`   = ifelse(is.na(IC_lo), "—", fmt_t(IC_lo)),
                `IC95 sup.`   = ifelse(is.na(IC_hi), "—", fmt_t(IC_hi)))
    totals <- df %>%
      group_by(year) %>%
      summarise(B = sum(B_ton), .groups = "drop") %>%
      transmute(Año = year, Zona = "TOTAL",
                `Biomasa (t)` = fmt_t(B),
                `CV (%)` = "—", `IC95 inf.` = "—", `IC95 sup.` = "—")
    bind_rows(detail, totals) %>% arrange(Año, Zona)
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "llrrrr")

  # ── Tab 3 UI / outputs ─────────────────────────────────────────────────────

  output$tab_params_ui <- renderUI({
    if (is.null(input$data_file)) return(no_data_card)

    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h6("Selección", class = "text-muted fw-bold"),
        selectInput("p_year", "Año:",
                    choices  = rev(years_analyzed_r()),
                    selected = max(years_analyzed_r())),
        selectInput("p_spp",  "Especie:",
                    choices  = c("Anchoveta", "Sardina común"),
                    selected = "Anchoveta")
      ),
      card(card_header(textOutput("par_title")),
           div(style = "overflow-x:auto", tableOutput("par_table"))),
      card(card_header("Contribución al CV²(B) por componente"),
           plotOutput("cv_decomp_plot", height = "340px"))
    )
  })

  output$par_title <- renderText({
    req(input$p_spp, input$p_year)
    paste("Parámetros del MPDH —", input$p_spp, "—", input$p_year)
  })

  par_data_r <- reactive({
    req(input$data_file, input$p_year, input$p_spp)
    yr   <- as.integer(input$p_year)
    spp0 <- ifelse(input$p_spp == "Sardina común", "Sardina", input$p_spp)
    d    <- data_r()
    validate(need(!is.null(d$parametros), "Sin datos de parámetros."))
    d$parametros %>%
      filter(year == yr, Especie == spp0) %>%
      mutate(
        Parámetro = case_when(
          Parm == "A"  ~ "Área total (mn²)",
          Parm == "AD" ~ "Área de desove (mn²)",
          Parm == "P0" ~ "Producción diaria de huevos P₀ (huevos/m²/día)",
          Parm == "W"  ~ "Peso promedio hembras maduras W (g)",
          Parm == "R"  ~ "Proporción hembras en peso R",
          Parm == "F"  ~ "Fecundidad parcial F (huevos/hembra)",
          Parm == "S"  ~ "Fracción diaria hembras desovantes S",
          TRUE ~ Parm
        ),
        Media    = round(as.numeric(Mean), 3),
        `CV (%)` = ifelse(is.na(CV) | CV == "NA", "—",
                          as.character(round(as.numeric(CV) * 100, 1)))
      ) %>%
      select(Zona, Parámetro, Media, `CV (%)`)
  })

  output$par_table <- renderTable({
    par_data_r()
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "llrl")

  output$cv_decomp_plot <- renderPlot({
    req(input$data_file, input$p_year, input$p_spp)
    yr   <- as.integer(input$p_year)
    spp0 <- ifelse(input$p_spp == "Sardina común", "Sardina", input$p_spp)
    d    <- data_r()
    validate(
      need(!is.null(d$parametros), "Sin datos de parámetros."),
      need(!is.null(d$lances),     "Sin datos de lances para descomponer la varianza.")
    )
    params <- d$parametros %>% filter(year == yr)
    lances <- d$lances     %>% filter(year == yr)

    comp_short <- c("CV²(P₀)", "CV²(W)", "CV²(F)", "CV²(S)", "CV²(R)",
                    "Cov(F,W)", "Cov(W,S)", "Cov(F,S)")
    rows <- list()
    for (zona in c("Centro", "Sur")) {
      dp <- params %>% filter(Especie == spp0, Zona == zona)
      if (nrow(dp) == 0) next
      dl  <- lances %>% filter(Especie == spp0, Zona == zona)
      res <- mpdh_zone(dp, dl, paste(yr, spp0, zona))
      if (is.null(res) || is.na(res$CV2)) next

      rows[[zona]] <- data.frame(
        Zona       = zona,
        Componente = comp_short,
        Pct        = round(res$terms / res$CV2 * 100, 1),
        CV_total   = round(sqrt(res$CV2) * 100, 1)
      )
    }

    validate(need(length(rows) > 0, "Sin datos de lances."))
    df_cv <- bind_rows(rows)
    df_cv$Componente <- factor(df_cv$Componente, levels = comp_short)
    df_cv$positivo <- df_cv$Pct >= 0
    labels_cv <- df_cv %>% group_by(Zona) %>% slice(1) %>%
      mutate(lab = paste0("CV total = ", CV_total, "%"))

    ggplot(df_cv, aes(x = Componente, y = Pct, fill = positivo)) +
      geom_col(width = 0.65, alpha = 0.88) +
      geom_hline(yintercept = 0, color = "gray40") +
      geom_text(data = labels_cv,
                aes(x = Inf, y = Inf, label = lab),
                inherit.aes = FALSE,
                hjust = 1.1, vjust = 1.5, size = 3.8, fontface = "bold", color = "gray25") +
      scale_fill_manual(values = c("TRUE" = "#2166ac", "FALSE" = "#d6604d"), guide = "none") +
      scale_y_continuous(labels = label_number(suffix = "%", decimal.mark = ","),
                         expand = expansion(mult = c(0.12, 0.15))) +
      facet_wrap(~Zona) +
      labs(x = NULL, y = "Contribución al CV²(B) (%)",
           caption = "Azul: término positivo (aumenta varianza). Rojo: término negativo (reduce varianza).") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 10),
            panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold", size = 12),
            plot.caption = element_text(color = "gray50", size = 9))
  }, res = 110)

  # ── Tab 4: Reporte ────────────────────────────────────────────────────────

  # Helper: descomposición CV² para una especie, todas las zonas, un año
  compute_cv_table <- function(params_yr, lances_yr, spp0) {
    rows <- list()
    for (zona in c("Centro", "Sur")) {
      dp <- params_yr %>% filter(Especie == spp0, Zona == zona)
      dl <- lances_yr %>% filter(Especie == spp0, Zona == zona)
      if (nrow(dp) == 0) next
      res <- mpdh_zone(dp, dl, paste(dp$year[1], spp0, zona))
      if (is.null(res) || is.na(res$CV2)) next

      rows[[zona]] <- data.frame(
        Zona     = zona,
        as.list(round(res$terms / res$CV2 * 100, 1)),
        CV_total = round(sqrt(res$CV2) * 100, 1),
        check.names = FALSE
      )
    }
    bind_rows(rows)
  }

  # Helper: texto de interpretación automática
  interpreta <- function(r4, r5, spp_label, yr) {
    if (is.null(r4) || nrow(r4) == 0) return("")
    comp_cols <- CV_TERMS
    lbl_map   <- c("CV²(P₀)" = "la producción diaria de huevos (P₀)",
                   "CV²(W)"  = "el peso promedio de hembras (W)",
                   "CV²(F)"  = "la fecundidad parcial (F)",
                   "CV²(S)"  = "la fracción diaria de hembras desovantes (S)",
                   "CV²(R)"  = "la proporción sexual en peso (R)",
                   "-2·Cov(F,W)/(FW)" = "la covarianza negativa F–W",
                   "-2·Cov(W,S)/(WS)" = "la covarianza negativa W–S",
                   "2·Cov(F,S)/(FS)"  = "la covarianza positiva F–S")
    total_row <- r4 %>% summarise(B = sum(B_ton, na.rm=TRUE), .groups="drop")
    total_b   <- fmt_t(sum(r4$B_ton, na.rm=TRUE))

    parrafos <- lapply(seq_len(nrow(r4)), function(i) {
      zona  <- r4$Zona[i]
      b     <- fmt_t(r4$B_ton[i])
      cv    <- r4$CV[i]
      nivel <- if (!is.na(cv) && cv > 35) "alta" else if (!is.na(cv) && cv > 20) "moderada" else "baja"

      dom_comp <- if (!is.null(r5) && nrow(r5) > 0) {
        row_r5 <- r5 %>% filter(Zona == zona)
        if (nrow(row_r5) > 0) {
          vals <- as.numeric(row_r5[1, comp_cols])
          idx  <- which.max(vals)
          paste0("El componente dominante es ", lbl_map[comp_cols[idx]],
                 " (", round(vals[idx], 1), "% del CV²(B)).")
        } else ""
      } else ""

      paste0("En la zona ", zona, ", la biomasa estimada de ", spp_label,
             " es ", b, " t (CV = ", ifelse(is.na(cv), "N/D", paste0(cv, "%")),
             ", incertidumbre ", nivel, "). ", dom_comp)
    })

    total_cv  <- r4 %>% summarise(
      B  = sum(B_ton, na.rm=TRUE),
      IC_lo = sum(IC_lo, na.rm=TRUE),
      IC_hi = sum(IC_hi, na.rm=TRUE)
    )
    resumen <- paste0(
      "La biomasa total de ", spp_label, " en ", yr, " es ",
      total_b, " t (IC 95%: ",
      fmt_t(total_cv$IC_lo), "–", fmt_t(total_cv$IC_hi), " t)."
    )

    paste(c(resumen, "", unlist(parrafos)), collapse = "\n")
  }

  # Reactivos del reporte
  rep_r4_r <- reactive({
    req(input$data_file, input$rep_year, input$rep_spp)
    spp_label <- input$rep_spp
    zone_df_r() %>%
      filter(Especie == spp_label,
             year    == as.integer(input$rep_year))
  })

  rep_r5_r <- reactive({
    req(input$data_file, input$rep_year, input$rep_spp)
    yr   <- as.integer(input$rep_year)
    spp0 <- ifelse(input$rep_spp == "Sardina común", "Sardina", input$rep_spp)
    d    <- data_r()
    req(!is.null(d$parametros), !is.null(d$lances))
    compute_cv_table(
      d$parametros %>% filter(year == yr),
      d$lances     %>% filter(year == yr),
      spp0
    )
  })

  rep_interp_r <- reactive({
    interpreta(rep_r4_r(), rep_r5_r(), input$rep_spp, input$rep_year)
  })

  # UI del tab reporte
  output$tab_reporte_ui <- renderUI({
    if (is.null(input$data_file)) return(no_data_card)
    yrs <- years_analyzed_r()

    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h6("Selección", class = "text-muted fw-bold"),
        selectInput("rep_year", "Año:",
                    choices  = rev(yrs),
                    selected = if (length(yrs) > 0) max(yrs) else NULL),
        selectInput("rep_spp", "Especie:",
                    choices  = c("Anchoveta", "Sardina común"),
                    selected = "Anchoveta"),
        hr(),
        h6("Descargar reporte", class = "text-muted fw-bold"),
        downloadButton("dl_docx", "Descargar .docx",
                       class = "btn btn-primary btn-sm w-100 mb-2"),
        downloadButton("dl_html", "Descargar .html",
                       class = "btn btn-outline-secondary btn-sm w-100"),
        hr(),
        p(class = "text-muted small",
          "El reporte incluye Tabla R4 (biomasa por zona) y",
          "Tabla R5 (estructura de varianza-covarianza) con interpretación.")
      ),

      card(
        card_header(textOutput("rep_titulo_r4")),
        div(style = "overflow-x:auto", tableOutput("rep_tabla_r4"))
      ),
      card(
        card_header(textOutput("rep_titulo_r5")),
        div(style = "overflow-x:auto", tableOutput("rep_tabla_r5"))
      ),
      card(
        card_header("Interpretación"),
        verbatimTextOutput("rep_interp")
      )
    )
  })

  output$rep_titulo_r4 <- renderText({
    req(input$rep_spp, input$rep_year)
    paste0("Tabla R4. Biomasa desovante estimada y CV total (método delta) — ",
           input$rep_spp, " — ", input$rep_year)
  })

  output$rep_titulo_r5 <- renderText({
    req(input$rep_spp, input$rep_year)
    paste0("Tabla R5. Estructura de varianza-covarianza — ",
           input$rep_spp, " — ", input$rep_year)
  })

  output$rep_tabla_r4 <- renderTable({
    r4 <- rep_r4_r()
    validate(need(nrow(r4) > 0, "Sin datos para esta selección."))
    totales <- r4 %>% summarise(
      Zona = "TOTAL",
      B_ton = sum(B_ton), CV = NA_real_, IC_lo = sum(IC_lo), IC_hi = sum(IC_hi)
    )
    bind_rows(r4, totales) %>%
      transmute(
        Zona,
        `Biomasa (t)` = fmt_t(B_ton),
        `CV total (%)` = ifelse(is.na(CV), "—", as.character(CV)),
        `IC95 inf. (t)` = fmt_t(IC_lo),
        `IC95 sup. (t)` = fmt_t(IC_hi)
      )
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "lrrrr")

  output$rep_tabla_r5 <- renderTable({
    r5 <- rep_r5_r()
    validate(need(nrow(r5) > 0, "Sin datos de lances para descomponer la varianza."))
    r5 %>% rename(`CV total (%)` = CV_total)
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "lrrrrrrrr")

  # ── Generadores de archivo ─────────────────────────────────────────────────

  # Flextable auxiliar para tabla R4
  make_ft_r4 <- function(r4, spp_label, yr) {
    totales <- r4 %>% summarise(
      Zona = "TOTAL", B_ton = sum(B_ton), CV = NA_real_, IC_lo = sum(IC_lo), IC_hi = sum(IC_hi)
    )
    df <- bind_rows(r4, totales) %>%
      transmute(
        Zona,
        `Biomasa (t)`   = fmt_t(B_ton),
        `CV total (%)`  = ifelse(is.na(CV), "—", paste0(CV, "%")),
        `IC95 inf. (t)` = fmt_t(IC_lo),
        `IC95 sup. (t)` = fmt_t(IC_hi)
      )
    ft <- flextable(df) %>%
      set_caption(paste0("Tabla R4. Biomasa desovante estimada y CV total (método delta) — ",
                         spp_label, " — ", yr,
                         ".\nFuente: MPDH Centro-Sur ", yr, ". R = 0,5 (fijo).")) %>%
      bold(part = "header") %>%
      bold(i = nrow(df)) %>%
      bg(i = nrow(df), bg = "#F0F0F0") %>%
      align(align = "right", j = 2:5) %>%
      align(align = "left",  j = 1) %>%
      fontsize(size = 10) %>%
      font(fontname = "Calibri", part = "all") %>%
      autofit()
    ft
  }

  # Flextable auxiliar para tabla R5 (negrita en mayor contribución por fila)
  make_ft_r5 <- function(r5, spp_label, yr) {
    comp_cols <- CV_TERMS
    df <- r5 %>% select(Zona, all_of(comp_cols), CV_total)
    ft <- flextable(df) %>%
      set_caption(paste0("Tabla R5. Estructura de varianza-covarianza — ",
                         spp_label, " — ", yr,
                         ".\nEn negrita, la contribución más alta de incertidumbre por zona.")) %>%
      bold(part = "header") %>%
      align(align = "right", j = 2:ncol(df)) %>%
      align(align = "left",  j = 1) %>%
      fontsize(size = 9) %>%
      font(fontname = "Calibri", part = "all") %>%
      color(j = which(names(df) %in% c("-2·Cov(F,W)/(FW)","-2·Cov(W,S)/(WS)")),
            color = "#d6604d") %>%
      color(j = which(names(df) == "2·Cov(F,S)/(FS)"), color = "#d6604d") %>%
      autofit()

    # Negrita en máximo positivo de cada fila
    for (i in seq_len(nrow(df))) {
      vals   <- as.numeric(df[i, comp_cols])
      j_max  <- which.max(vals) + 1L   # +1 por columna Zona
      if (!is.na(j_max)) ft <- bold(ft, i = i, j = j_max)
    }
    ft
  }

  # DOCX download
  output$dl_docx <- downloadHandler(
    filename = function() {
      paste0("Reporte_MPDH_", gsub(" ", "_", input$rep_spp), "_", input$rep_year, ".docx")
    },
    content = function(file) {
      r4  <- rep_r4_r()
      r5  <- rep_r5_r()
      txt <- rep_interp_r()
      yr  <- input$rep_year
      spp <- input$rep_spp

      ft4 <- make_ft_r4(r4, spp, yr)
      ft5 <- make_ft_r5(r5, spp, yr)

      doc <- read_docx() %>%
        body_add_par(
          paste0("Reporte de Biomasa Desovante — MPDH Centro-Sur ", yr),
          style = "heading 1"
        ) %>%
        body_add_par(paste0("Especie: ", spp, "   |   Año: ", yr), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_flextable(ft4) %>%
        body_add_par("", style = "Normal") %>%
        body_add_flextable(ft5) %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("Interpretación", style = "heading 2")

      for (linea in strsplit(txt, "\n")[[1]]) {
        doc <- body_add_par(doc, linea, style = "Normal")
      }

      print(doc, target = file)
    }
  )

  # HTML download
  output$dl_html <- downloadHandler(
    filename = function() {
      paste0("Reporte_MPDH_", gsub(" ", "_", input$rep_spp), "_", input$rep_year, ".html")
    },
    content = function(file) {
      r4  <- rep_r4_r()
      r5  <- rep_r5_r()
      txt <- rep_interp_r()
      yr  <- input$rep_year
      spp <- input$rep_spp

      # Table R4 html
      totales <- r4 %>% summarise(
        Zona = "TOTAL", B_ton = sum(B_ton), CV = NA_real_,
        IC_lo = sum(IC_lo), IC_hi = sum(IC_hi)
      )
      df4 <- bind_rows(r4, totales) %>%
        transmute(Zona,
                  `Biomasa (t)` = fmt_t(B_ton),
                  `CV total (%)` = ifelse(is.na(CV), "—", paste0(CV, "%")),
                  `IC95 inf. (t)` = fmt_t(IC_lo),
                  `IC95 sup. (t)` = fmt_t(IC_hi))

      comp_cols <- CV_TERMS
      df5 <- r5 %>% select(Zona, all_of(comp_cols), CV_total)

      tbl_to_html <- function(df, bold_last = FALSE, bold_max_cols = NULL) {
        th  <- paste0("<th>", names(df), "</th>", collapse = "")
        rows <- apply(df, 1, function(row) {
          cells <- mapply(function(val, col) {
            is_bold <- bold_last && col == nrow(df)
            is_cov  <- !is.null(bold_max_cols) && col %in% bold_max_cols
            style   <- if (is_bold) " style='font-weight:bold;background:#f0f0f0'" else ""
            paste0("<td", style, ">", val, "</td>")
          }, row, seq_along(row))
          paste0("<tr>", paste(cells, collapse=""), "</tr>")
        })
        # Bold max per row in R5
        if (!is.null(bold_max_cols)) {
          num_cols <- which(names(df) %in% comp_cols)
          rows <- sapply(seq_len(nrow(df)), function(i) {
            j_max <- which.max(as.numeric(df[i, comp_cols])) + 1L
            cells <- sapply(seq_along(df[i,]), function(j) {
              val <- df[i, j]
              b   <- if (j == j_max) " style='font-weight:bold'" else ""
              paste0("<td", b, ">", val, "</td>")
            })
            paste0("<tr>", paste(cells, collapse=""), "</tr>")
          })
        }
        paste0(
          "<table style='border-collapse:collapse;width:100%;font-size:13px'>",
          "<thead style='background:#2166ac;color:white'><tr>", th, "</tr></thead>",
          "<tbody>", paste(rows, collapse=""), "</tbody></table>"
        )
      }

      interp_html <- paste0("<p>", gsub("\n\n", "</p><p>",
                                        gsub("\n", " ", txt)), "</p>")

      html <- paste0(
        "<!DOCTYPE html><html lang='es'><head><meta charset='UTF-8'>",
        "<title>Reporte MPDH ", yr, "</title>",
        "<style>body{font-family:Calibri,Arial,sans-serif;max-width:960px;margin:40px auto;padding:0 20px;color:#222}",
        "h1{color:#1a3f6f}h2{color:#2166ac;margin-top:2em}",
        "table{margin-bottom:1.5em}td,th{padding:6px 12px;border:1px solid #ccc;text-align:right}",
        "td:first-child,th:first-child{text-align:left}",
        "p{line-height:1.6}</style></head><body>",
        "<h1>Reporte Biomasa Desovante — MPDH Centro-Sur ", yr, "</h1>",
        "<p><strong>Especie:</strong> ", spp, " &nbsp;|&nbsp; <strong>Año:</strong> ", yr, "</p>",
        "<h2>Tabla R4. Biomasa desovante estimada y CV total (método delta)</h2>",
        tbl_to_html(df4, bold_last = TRUE),
        "<p style='font-size:11px;color:#666'>Fuente: MPDH Centro-Sur ", yr,
        ". R&nbsp;=&nbsp;0,5 (fijo).</p>",
        "<h2>Tabla R5. Estructura de varianza-covarianza</h2>",
        tbl_to_html(df5, bold_max_cols = comp_cols),
        "<p style='font-size:11px;color:#666'>En negrita, la contribución más alta",
        " de incertidumbre por zona. Valores en % del CV²(B) total.</p>",
        "<h2>Interpretación</h2>", interp_html,
        "</body></html>"
      )
      writeLines(html, file, useBytes = TRUE)
    }
  )
}

# ── Arranque ──────────────────────────────────────────────────────────────────
shinyApp(ui, server)
