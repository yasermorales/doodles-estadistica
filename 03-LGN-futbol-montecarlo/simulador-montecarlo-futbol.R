# =============================================================================
# MONTECARLO DE JUGUETE - 4 ESCENARIOS
# Doodles de estadística - LGN aplicada al fútbol
# =============================================================================

library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

# -----------------------------------------------------------------------------
# PARÁMETROS FIJOS (iguales en todos los escenarios)
# -----------------------------------------------------------------------------

mult_local     <- 1.00
mult_visitante <- 0.85

lambda_vs_descenso   <- 14
lambda_vs_mitad      <- 10
lambda_vs_candidatos <- 7

n_candidatos <- 4
n_mitad      <- 10
n_descenso   <- 5

n_temporadas <- 1000

fila_prob_del   <- c(0.80, 0.20)
col_prob_del    <- c(0.15, 0.70, 0.15)
fila_prob_med   <- c(0.50, 0.50)
col_prob_med    <- c(0.20, 0.60, 0.20)

# -----------------------------------------------------------------------------
# PARÁMETROS POR ESCENARIO
# -----------------------------------------------------------------------------

escenarios <- list(
  list(
    nombre   = "Del. bueno +\nMed. bueno",
    p_med    = 0.35,
    p_del    = 0.35,
    grilla_med = matrix(c(0.80, 1.00, 0.80,
                          0.60, 1.00, 0.60), nrow = 2, byrow = TRUE),
    grilla_del = matrix(c(0.60, 1.00, 0.60,
                          0.20, 0.60, 0.20), nrow = 2, byrow = TRUE)
  ),
  list(
    nombre   = "Del. bueno +\nMed. medio pelo",
    p_med    = 0.20,
    p_del    = 0.35,
    grilla_med = matrix(c(0.60, 1.00, 0.60,
                          0.40, 0.70, 0.40), nrow = 2, byrow = TRUE),
    grilla_del = matrix(c(0.60, 1.00, 0.60,
                          0.20, 0.60, 0.20), nrow = 2, byrow = TRUE)
  ),
  list(
    nombre   = "Del. medio pelo +\nMed. bueno",
    p_med    = 0.35,
    p_del    = 0.20,
    grilla_med = matrix(c(0.80, 1.00, 0.80,
                          0.60, 1.00, 0.60), nrow = 2, byrow = TRUE),
    grilla_del = matrix(c(0.20, 1.00, 0.20,
                          0.10, 0.60, 0.10), nrow = 2, byrow = TRUE)
  ),
  list(
    nombre   = "Del. medio pelo +\nMed. medio pelo",
    p_med    = 0.20,
    p_del    = 0.20,
    grilla_med = matrix(c(0.40, 1.00, 0.40,
                          0.30, 0.70, 0.30), nrow = 2, byrow = TRUE),
    grilla_del = matrix(c(0.20, 1.00, 0.20,
                          0.10, 0.60, 0.10), nrow = 2, byrow = TRUE)
  )
)

# -----------------------------------------------------------------------------
# FUNCIONES
# -----------------------------------------------------------------------------

sortear_posicion <- function(grilla, fila_prob, col_prob) {
  fila <- sample(1:2, size = 1, prob = fila_prob)
  col  <- sample(1:3, size = 1, prob = col_prob)
  grilla[fila, col]
}

simular_situacion <- function(p_med_base, p_del_base, grilla_med, grilla_del, mult) {
  mult_med <- sortear_posicion(grilla_med, fila_prob_med, col_prob_med)
  mult_del <- sortear_posicion(grilla_del, fila_prob_del, col_prob_del)
  
  p_med <- p_med_base * mult_med * mult
  p_del <- p_del_base * mult_del * mult
  p_gol <- min(p_med * p_del, 1)
  
  rbinom(1, 1, prob = p_gol)
}

simular_partido <- function(p_med_base, p_del_base, grilla_med, grilla_del, lambda, es_local) {
  mult          <- ifelse(es_local, mult_local, mult_visitante)
  n_situaciones <- rpois(1, lambda)
  if (n_situaciones == 0) return(0L)
  sum(sapply(seq_len(n_situaciones), function(i)
    simular_situacion(p_med_base, p_del_base, grilla_med, grilla_del, mult)))
}

armar_calendario <- function() {
  rivales <- data.frame(
    tipo   = c(rep("candidato", n_candidatos),
               rep("mitad",     n_mitad),
               rep("descenso",  n_descenso)),
    lambda = c(rep(lambda_vs_candidatos, n_candidatos),
               rep(lambda_vs_mitad,      n_mitad),
               rep(lambda_vs_descenso,   n_descenso))
  )
  rbind(cbind(rivales, es_local = TRUE),
        cbind(rivales, es_local = FALSE))
}

simular_temporada <- function(p_med_base, p_del_base, grilla_med, grilla_del) {
  calendario <- armar_calendario()
  goles <- sapply(seq_len(nrow(calendario)), function(i)
    simular_partido(p_med_base, p_del_base, grilla_med, grilla_del,
                    calendario$lambda[i], calendario$es_local[i]))
  sum(goles)
}

# -----------------------------------------------------------------------------
# SIMULACIÓN
# -----------------------------------------------------------------------------

set.seed(42)

resultados <- lapply(escenarios, function(esc) {
  goles <- replicate(n_temporadas,
                     simular_temporada(esc$p_med, esc$p_del,
                                       esc$grilla_med, esc$grilla_del))
  data.frame(escenario = esc$nombre, goles = goles)
})

df_resultados <- do.call(rbind, resultados)

# Factor con orden lógico
df_resultados$escenario <- factor(df_resultados$escenario,
                                  levels = sapply(escenarios, `[[`, "nombre"))

# -----------------------------------------------------------------------------
# BOXPLOT
# -----------------------------------------------------------------------------

p <- ggplot(df_resultados, aes(x = escenario, y = goles, fill = escenario)) +
  geom_boxplot(width = 0.5, outlier.shape = 21, outlier.size = 2, alpha = 0.8) +
  scale_fill_manual(values = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D")) +
  labs(
    title    = "Goles por temporada según combinación de jugadores",
    subtitle = paste0("Montecarlo — ", n_temporadas, " temporadas simuladas por escenario"),
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

ggsave("boxplot_escenarios.png", plot = p, width = 9, height = 6, dpi = 150)
cat("Boxplot guardado: boxplot_escenarios.png\n")

# -----------------------------------------------------------------------------
# TABLA RESUMEN (transpuesta: estadísticos = filas, escenarios = columnas)
# -----------------------------------------------------------------------------

resumen <- df_resultados %>%
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

# Transponer: escenarios pasan a ser nombres de columnas
resumen_t <- as.data.frame(t(resumen[, -1]))
colnames(resumen_t) <- gsub("\n", " ", as.character(resumen$escenario))
resumen_t <- cbind(Estadístico = rownames(resumen_t), resumen_t)
rownames(resumen_t) <- NULL

# Tabla con kable
tabla <- kable(resumen_t, format = "html", align = "lrrrr",
               caption = paste0("Resumen de goles por temporada — ",
                                n_temporadas, " simulaciones por escenario")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "left") %>%
  column_spec(1, bold = TRUE)

# Guardar tabla como HTML standalone
writeLines(
  c("<!DOCTYPE html><html><head>",
    "<meta charset='utf-8'>",
    "<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css'>",
    "</head><body style='padding:20px'>",
    as.character(tabla),
    "</body></html>"),
  "tabla_resumen.html"
)
cat("Tabla guardada: tabla_resumen.html\n")

# También imprime en consola
print(resumen)


# -----------------------------------------------------------------------------
# TABLAS DE PARÁMETROS POR ESCENARIO
# -----------------------------------------------------------------------------

tabla_parametros <- function(esc) {
  t1 <- kable(
    data.frame(Jugador = c("MC", "DEL"), Prob = c(esc$p_med, esc$p_del)),
    format = "html", align = "lr"
  ) %>%
    kable_styling(bootstrap_options = c("striped", "condensed"),
                  full_width = FALSE, position = "left") %>%
    column_spec(1, bold = TRUE)
  
  df_del <- as.data.frame(esc$grilla_del)
  colnames(df_del) <- c("Banda izquierda", "Centro", "Banda derecha")
  df_del <- cbind(`Mult. DEL` = c("Último cuarto", "Tres cuartos"), df_del)
  t2 <- kable(df_del, format = "html", align = "lrrr") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"),
                  full_width = FALSE, position = "left") %>%
    column_spec(1, bold = TRUE)
  
  df_med <- as.data.frame(esc$grilla_med)
  colnames(df_med) <- c("Banda izquierda", "Centro", "Banda derecha")
  df_med <- cbind(`Mult. MC` = c("Último cuarto", "Tres cuartos"), df_med)
  t3 <- kable(df_med, format = "html", align = "lrrr") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"),
                  full_width = FALSE, position = "left") %>%
    column_spec(1, bold = TRUE)
  
  list(probs = t1, grilla_del = t2, grilla_med = t3)
}

tablas_escenarios <- lapply(escenarios, tabla_parametros)
names(tablas_escenarios) <- sapply(escenarios, function(e) gsub("\n", " ", e$nombre))

# Uso en Rmarkdown:
# tablas_escenarios[["Del. bueno + Med. bueno"]]$probs
# tablas_escenarios[["Del. bueno + Med. bueno"]]$grilla_del
# tablas_escenarios[["Del. bueno + Med. bueno"]]$grilla_med
# -----------------------------------------------------------------------------
# IMAGEN CON LAS 3 TABLAS DE PARÁMETROS POR ESCENARIO
# Requiere: ggplot2, gridExtra (install.packages("gridExtra"))
# -----------------------------------------------------------------------------

library(gridExtra)

tabla_como_grob <- function(df, titulo) {
  tt <- ttheme_minimal(
    core    = list(fg_params = list(hjust = 1, x = 0.95)),
    rowhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "bold")),
    colhead = list(fg_params = list(fontface = "bold"))
  )
  tableGrob(df, rows = NULL, theme = tt)
}

imagen_parametros <- function(esc) {
  nombre_limpio <- gsub("\n", " ", esc$nombre)
  
  # Tabla 1: probs base
  df1 <- data.frame(Jugador = c("MC", "DEL"), Prob = c(esc$p_med, esc$p_del))
  
  # Tabla 2: grilla DEL
  df2 <- as.data.frame(esc$grilla_del)
  colnames(df2) <- c("Banda izquierda", "Centro", "Banda derecha")
  df2 <- cbind(`Mult. DEL` = c("Último cuarto", "Tres cuartos"), df2)
  
  # Tabla 3: grilla MC
  df3 <- as.data.frame(esc$grilla_med)
  colnames(df3) <- c("Banda izquierda", "Centro", "Banda derecha")
  df3 <- cbind(`Mult. MC` = c("Último cuarto", "Tres cuartos"), df3)
  
  g1 <- tabla_como_grob(df1, "Probabilidades base")
  g2 <- tabla_como_grob(df2, "Multiplicadores DEL")
  g3 <- tabla_como_grob(df3, "Multiplicadores MC")
  
  nombre_archivo <- file.path(dirname(rstudioapi::getSourceEditorContext()$path),
                              paste0("params_", gsub("[^a-zA-Z0-9]", "_", nombre_limpio), ".png"))
  png(nombre_archivo, width = 800, height = 400, res = 120)
  grid.arrange(
    g1, g2, g3,
    ncol = 1,
    top  = grid::textGrob(nombre_limpio, gp = grid::gpar(fontsize = 13, fontface = "bold"))
  )
  dev.off()
  
  cat("Imagen guardada:", nombre_archivo, "\n")
}

invisible(lapply(escenarios, imagen_parametros))

p
tabla

