# =============================================================================
# SIMULADOR MONTECARLO DE JUGUETE — SHINY APP
# Doodles de estadística | LGN aplicada al fútbol
# =============================================================================
# Paquetes: shiny, ggplot2, dplyr, DT
# install.packages(c("shiny", "ggplot2", "dplyr", "DT"))
# =============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# -----------------------------------------------------------------------------
# FUNCIONES DEL SIMULADOR
# -----------------------------------------------------------------------------

sortear_posicion <- function(grilla, fila_prob, col_prob) {
  fila <- sample(1:2, size = 1, prob = fila_prob)
  col  <- sample(1:3, size = 1, prob = col_prob)
  grilla[fila, col]
}

simular_situacion <- function(p_med_base, p_del_base, grilla_med, grilla_del,
                               fila_prob_med, col_prob_med,
                               fila_prob_del, col_prob_del, mult) {
  mult_med <- sortear_posicion(grilla_med, fila_prob_med, col_prob_med)
  mult_del <- sortear_posicion(grilla_del, fila_prob_del, col_prob_del)
  p_gol    <- min(p_med_base * mult_med * mult * p_del_base * mult_del * mult, 1)
  rbinom(1, 1, prob = p_gol)
}

simular_partido <- function(p_med_base, p_del_base, grilla_med, grilla_del,
                             fila_prob_med, col_prob_med,
                             fila_prob_del, col_prob_del,
                             lambda, es_local, mult_local, mult_visitante) {
  mult          <- ifelse(es_local, mult_local, mult_visitante)
  n_situaciones <- rpois(1, lambda)
  if (n_situaciones == 0) return(0L)
  sum(sapply(seq_len(n_situaciones), function(i)
    simular_situacion(p_med_base, p_del_base, grilla_med, grilla_del,
                      fila_prob_med, col_prob_med,
                      fila_prob_del, col_prob_del, mult)))
}

armar_calendario <- function(n_candidatos, n_mitad, n_descenso,
                              lambda_cand, lambda_mitad, lambda_desc) {
  rivales <- data.frame(
    lambda = c(rep(lambda_cand,  n_candidatos),
               rep(lambda_mitad, n_mitad),
               rep(lambda_desc,  n_descenso))
  )
  rbind(cbind(rivales, es_local = TRUE),
        cbind(rivales, es_local = FALSE))
}

simular_temporada <- function(p_med_base, p_del_base, grilla_med, grilla_del,
                               fila_prob_med, col_prob_med,
                               fila_prob_del, col_prob_del,
                               n_candidatos, n_mitad, n_descenso,
                               lambda_cand, lambda_mitad, lambda_desc,
                               mult_local, mult_visitante) {
  calendario <- armar_calendario(n_candidatos, n_mitad, n_descenso,
                                  lambda_cand, lambda_mitad, lambda_desc)
  sum(sapply(seq_len(nrow(calendario)), function(i)
    simular_partido(p_med_base, p_del_base, grilla_med, grilla_del,
                    fila_prob_med, col_prob_med,
                    fila_prob_del, col_prob_del,
                    calendario$lambda[i], calendario$es_local[i],
                    mult_local, mult_visitante)))
}

correr_escenario <- function(nombre, p_med, p_del, grilla_med, grilla_del,
                              fila_prob_med, col_prob_med,
                              fila_prob_del, col_prob_del,
                              n_temporadas, n_candidatos, n_mitad, n_descenso,
                              lambda_cand, lambda_mitad, lambda_desc,
                              mult_local, mult_visitante) {
  goles <- replicate(n_temporadas,
    simular_temporada(p_med, p_del, grilla_med, grilla_del,
                      fila_prob_med, col_prob_med,
                      fila_prob_del, col_prob_del,
                      n_candidatos, n_mitad, n_descenso,
                      lambda_cand, lambda_mitad, lambda_desc,
                      mult_local, mult_visitante))
  data.frame(escenario = nombre, goles = goles)
}

# -----------------------------------------------------------------------------
# HELPERS UI
# -----------------------------------------------------------------------------

grilla_inputs <- function(prefijo, label, defaults) {
  # defaults: vector de 6 valores, orden byrow: A_izq, A_cen, A_der, B_izq, B_cen, B_der
  tagList(
    tags$p(tags$b(label), style = "margin-bottom:4px; margin-top:10px;"),
    tags$table(style = "width:100%; font-size:12px;",
      tags$thead(tags$tr(
        tags$th(""),
        tags$th("Banda izq.", style = "text-align:center;"),
        tags$th("Centro",     style = "text-align:center;"),
        tags$th("Banda der.", style = "text-align:center;")
      )),
      tags$tbody(
        tags$tr(
          tags$td("Último cuarto", style = "font-weight:bold; padding-right:4px;"),
          tags$td(numericInput(paste0(prefijo, "_A1"), NULL, defaults[1], 0, 1, 0.05,  width = "70px")),
          tags$td(numericInput(paste0(prefijo, "_A2"), NULL, defaults[2], 0, 1, 0.05,  width = "70px")),
          tags$td(numericInput(paste0(prefijo, "_A3"), NULL, defaults[3], 0, 1, 0.05,  width = "70px"))
        ),
        tags$tr(
          tags$td("Tres cuartos", style = "font-weight:bold; padding-right:4px;"),
          tags$td(numericInput(paste0(prefijo, "_B1"), NULL, defaults[4], 0, 1, 0.05, width = "70px")),
          tags$td(numericInput(paste0(prefijo, "_B2"), NULL, defaults[5], 0, 1, 0.05, width = "70px")),
          tags$td(numericInput(paste0(prefijo, "_B3"), NULL, defaults[6], 0, 1, 0.05, width = "70px"))
        )
      )
    )
  )
}

leer_grilla <- function(input, prefijo) {
  matrix(c(
    input[[paste0(prefijo, "_A1")]], input[[paste0(prefijo, "_A2")]], input[[paste0(prefijo, "_A3")]],
    input[[paste0(prefijo, "_B1")]], input[[paste0(prefijo, "_B2")]], input[[paste0(prefijo, "_B3")]]
  ), nrow = 2, byrow = TRUE)
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Simulador Montecarlo de juguete — Doodles de estadística"),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # ── Parámetros generales ──────────────────────────────────────────────
      tags$h4("Parámetros generales", style = "border-bottom:1px solid #ccc; padding-bottom:4px;"),

      sliderInput("n_temporadas", "Temporadas a simular", 100, 1000, 200, step = 100),

      fluidRow(
        column(6, numericInput("lambda_cand",  "λ vs candidatos", 7,  1, 30, 1)),
        column(6, numericInput("lambda_mitad", "λ vs mitad tabla", 10, 1, 30, 1))
      ),
      fluidRow(
        column(6, numericInput("lambda_desc",  "λ vs descenso",  14,  1, 30, 1)),
        column(6, numericInput("mult_visit",   "Mult. visitante", 0.85, 0.5, 1, 0.05))
      ),
      fluidRow(
        column(4, numericInput("n_cand",    "Candidatos", 4,  1, 10, 1)),
        column(4, numericInput("n_mitad",   "Mitad",     10,  1, 15, 1)),
        column(4, numericInput("n_desc",    "Descenso",   5,  1, 10, 1))
      ),

      # ── Mediocampista bueno ───────────────────────────────────────────────
      tags$h4("Mediocampista bueno", style = "border-bottom:1px solid #ccc; padding-bottom:4px; margin-top:16px;"),
      sliderInput("p_med_bueno", "Probabilidad base", 0.01, 1, 0.35, 0.01),
      grilla_inputs("gmed_b", "Grilla multiplicadores",
                    c(0.70, 1.00, 0.80, 0.60, 1.00, 0.60)),

      # ── Mediocampista medio pelo ──────────────────────────────────────────
      tags$h4("Mediocampista medio pelo", style = "border-bottom:1px solid #ccc; padding-bottom:4px; margin-top:16px;"),
      sliderInput("p_med_malo", "Probabilidad base", 0.01, 1, 0.15, 0.01),
      grilla_inputs("gmed_m", "Grilla multiplicadores",
                    c(0.40, 1.00, 0.40, 0.30, 0.70, 0.30)),

      # ── Delantero bueno ───────────────────────────────────────────────────
      tags$h4("Delantero bueno", style = "border-bottom:1px solid #ccc; padding-bottom:4px; margin-top:16px;"),
      sliderInput("p_del_bueno", "Probabilidad base", 0.01, 1, 0.35, 0.01),
      grilla_inputs("gdel_b", "Grilla multiplicadores",
                    c(0.60, 1.00, 0.60, 0.20, 0.60, 0.20)),

      # ── Delantero medio pelo ──────────────────────────────────────────────
      tags$h4("Delantero medio pelo", style = "border-bottom:1px solid #ccc; padding-bottom:4px; margin-top:16px;"),
      sliderInput("p_del_malo", "Probabilidad base", 0.01, 1, 0.15, 0.01),
      grilla_inputs("gdel_m", "Grilla multiplicadores",
                    c(0.10, 1.00, 0.10, 0.10, 0.60, 0.10)),

      tags$br(),
      actionButton("simular", "▶  Simular", class = "btn btn-primary btn-lg",
                   style = "width:100%;")
    ),

    mainPanel(
      width = 8,
      plotOutput("boxplot", height = "420px"),
      tags$br(),
      DTOutput("tabla")
    )
  )
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  # Probabilidades de posición — fijas (no expuestas al usuario por simplicidad)
  fila_prob_del <- c(0.80, 0.20)
  col_prob_del  <- c(0.15, 0.70, 0.15)
  fila_prob_med <- c(0.50, 0.50)
  col_prob_med  <- c(0.20, 0.60, 0.20)

  resultados <- eventReactive(input$simular, {
    withProgress(message = "Simulando temporadas...", value = 0, {

      params_comunes <- list(
        fila_prob_med  = fila_prob_med,
        col_prob_med   = col_prob_med,
        fila_prob_del  = fila_prob_del,
        col_prob_del   = col_prob_del,
        n_temporadas   = input$n_temporadas,
        n_candidatos   = input$n_cand,
        n_mitad        = input$n_mitad,
        n_descenso     = input$n_desc,
        lambda_cand    = input$lambda_cand,
        lambda_mitad   = input$lambda_mitad,
        lambda_desc    = input$lambda_desc,
        mult_local     = 1.00,
        mult_visitante = input$mult_visit
      )

      grilla_med_b <- leer_grilla(input, "gmed_b")
      grilla_med_m <- leer_grilla(input, "gmed_m")
      grilla_del_b <- leer_grilla(input, "gdel_b")
      grilla_del_m <- leer_grilla(input, "gdel_m")

      escenarios <- list(
        list(nombre = "Del. bueno + Med. bueno",       p_med = input$p_med_bueno, p_del = input$p_del_bueno, gm = grilla_med_b, gd = grilla_del_b),
        list(nombre = "Del. bueno + Med. medio pelo",  p_med = input$p_med_malo,  p_del = input$p_del_bueno, gm = grilla_med_m, gd = grilla_del_b),
        list(nombre = "Del. medio pelo + Med. bueno",  p_med = input$p_med_bueno, p_del = input$p_del_malo,  gm = grilla_med_b, gd = grilla_del_m),
        list(nombre = "Del. medio pelo + Med. medio pelo", p_med = input$p_med_malo, p_del = input$p_del_malo, gm = grilla_med_m, gd = grilla_del_m)
      )

      dfs <- lapply(seq_along(escenarios), function(i) {
        setProgress(i / length(escenarios),
                    detail = paste0("Escenario ", i, " de ", length(escenarios)))
        esc <- escenarios[[i]]
        do.call(correr_escenario,
                c(list(nombre     = esc$nombre,
                       p_med      = esc$p_med,
                       p_del      = esc$p_del,
                       grilla_med = esc$gm,
                       grilla_del = esc$gd),
                  params_comunes))
      })

      df <- do.call(rbind, dfs)
      df$escenario <- factor(df$escenario, levels = sapply(escenarios, `[[`, "nombre"))
      df
    })
  })

  output$boxplot <- renderPlot({
    req(resultados())
    df <- resultados()
    ggplot(df, aes(x = escenario, y = goles, fill = escenario)) +
      geom_boxplot(width = 0.5, outlier.shape = 21, outlier.size = 2, alpha = 0.85) +
      scale_fill_manual(values = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D")) +
      labs(
        title    = "Goles por temporada según combinación de jugadores",
        subtitle = paste0("Montecarlo — ", input$n_temporadas, " temporadas por escenario"),
        x        = NULL,
        y        = "Goles en la temporada",
        caption  = "Simulador de juguete | Doodles de estadística"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position  = "none",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold"),
        axis.text.x      = element_text(size = 10)
      )
  })

  output$tabla <- renderDT({
    req(resultados())
    df <- resultados()

    resumen <- df %>%
      group_by(escenario) %>%
      summarise(
        Min     = min(goles),
        Q1      = quantile(goles, 0.25),
        Mediana = median(goles),
        Media   = round(mean(goles), 1),
        Q3      = quantile(goles, 0.75),
        Max     = max(goles),
        DE      = round(sd(goles), 2),
        .groups = "drop"
      )

    # Transponer
    resumen_t <- as.data.frame(t(resumen[, -1]))
    colnames(resumen_t) <- as.character(resumen$escenario)
    resumen_t <- cbind(Estadístico = rownames(resumen_t), resumen_t)
    rownames(resumen_t) <- NULL

    datatable(
      resumen_t,
      rownames  = FALSE,
      options   = list(dom = "t", ordering = FALSE, pageLength = 10),
      caption   = paste0("Resumen — ", input$n_temporadas, " temporadas simuladas por escenario")
    )
  })
}

# -----------------------------------------------------------------------------
# LANZAR
# -----------------------------------------------------------------------------

shinyApp(ui, server)
