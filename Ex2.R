library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(MASS)

app_theme <- bs_theme(
  
  bg = "#FFFFFF",
  fg = "#2B2B2B",
  primary = "#E7131A",
  secondary = "#F8F8F8",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Montserrat")
)

ui <- page_navbar(
  title = "Transformări de Variabile Aleatoare",
  theme = app_theme,
  
  nav_panel(title = "1D: Unidimensional",
            fluidPage(
              card(
                card_header("Configurare Simulare 1D", style = "background-color: #F8F8F8; color: #E7131A; font-weight: bold; border-bottom: 2px solid #E7131A;"),
                fluidRow(
                  column(3, selectInput("dist_x", "Repartiția lui X:", choices = c("Normală" = "norm", "Exponențială" = "exp", "Uniformă" = "unif", "Gamma" = "gamma"))),
                  column(3, numericInput("n_samples", "Eșantion (n):", 1000, min = 10, max = 50000)),
                  column(3, selectInput("trans_g", "Funcția g(x):", choices = c("x^2" = "sq", "|x|" = "abs", "log(x)" = "log", "e^x" = "exp_t", "1 / (1 + e^-x)" = "sigmoid"))),
                  column(3, div(actionButton("sim_1d", "Simulează", class = "btn-primary w-100", style = "background-color: #E7131A; border-color: #E7131A; color: white; font-weight: bold; padding: 12px; font-size: 16px;"), style = "margin-top: 24px;"))
                ),
                hr(style = "border-top: 1px solid #ddd; margin-top: 10px; margin-bottom: 20px;"),
                div(style = "background-color: #fcfcfc; padding: 15px; border-radius: 8px; border: 1px solid #eee;",
                    strong("Parametrii Repartiției:", style = "color: #E7131A; display: block; margin-bottom: 10px;"),
                    fluidRow(
                      conditionalPanel("input.dist_x == 'norm'", column(3, numericInput("norm_mu", "Media (\u03bc):", 0)), column(3, numericInput("norm_sd", "Deviație (\u03c3):", 1, min = 0.001))),
                      conditionalPanel("input.dist_x == 'exp'", column(3, numericInput("exp_rate", "Rata (\u03bb):", 1, min = 0.001))),
                      conditionalPanel("input.dist_x == 'unif'", column(3, numericInput("unif_a", "Minim (a):", 0)), column(3, numericInput("unif_b", "Maxim (b):", 1))),
                      conditionalPanel("input.dist_x == 'gamma'", column(3, numericInput("gamma_shape", "Forma (k):", 2, min = 0.001)), column(3, numericInput("gamma_scale", "Scala (\u03b8):", 1, min = 0.001)))
                    )
                )
              ),
              br(),
              uiOutput("interp_msg"),
              br(),
              layout_columns(
                card(card_header("Distribuția X", style = "background-color: #F8F8F8; color: #E7131A; font-weight: bold; border-top: 3px solid #E7131A;"), plotOutput("plot_x")),
                card(card_header("Distribuția Y", style = "background-color: #F8F8F8; color: #E7131A; font-weight: bold; border-top: 3px solid #E7131A;"), plotOutput("plot_y"))
              ),
              layout_columns(
                card(card_header("Indicatori Statistici X", style = "background-color: #E7131A; color: white;"), tableOutput("stat_x")),
                card(card_header("Indicatori Statistici Y", style = "background-color: #E7131A; color: white;"), tableOutput("stat_y"))
              )
            )
  ),
  
  nav_panel(title = "2D: Bidimensional",
            fluidPage(
              card(
                card_header("Configurare Simulare 2D", style = "background-color: #F8F8F8; color: #E7131A; font-weight: bold; border-bottom: 2px solid #E7131A;"),
                fluidRow(
                  column(3, radioButtons("gen_type", "Tip generare:", choices = c("Independente" = "indep", "Normală 2D" = "bivnorm"))),
                  column(3, numericInput("n_samples_2d", "Eșantion (n):", 1000, min = 10)),
                  column(3, selectInput("trans_h", "Funcția h(X,Y):", choices = c("X + Y" = "add", "X - Y" = "sub", "sqrt(X^2 + Y^2)" = "dist", "X * Y" = "mul"))),
                  column(3, div(actionButton("sim_2d", "Simulează", class = "btn-primary w-100", style = "background-color: #E7131A; border-color: #E7131A; color: white; font-weight: bold; padding: 12px; font-size: 16px;"), style = "margin-top: 24px;"))
                ),
                hr(style = "border-top: 1px solid #ddd; margin-top: 10px; margin-bottom: 20px;"),
                conditionalPanel("input.gen_type == 'indep'",
                                 fluidRow(
                                   column(6,
                                          div(style = "background-color: #fcfcfc; padding: 15px; border-radius: 8px; border: 1px solid #eee;",
                                              strong("Variabila X", style = "color: #E7131A; display: block; margin-bottom: 10px;"),
                                              selectInput("dist_x_2d", "Repartiție X:", choices = c("Normală" = "norm", "Exponențială" = "exp", "Uniformă" = "unif", "Gamma" = "gamma")),
                                              fluidRow(
                                                conditionalPanel("input.dist_x_2d == 'norm'", column(6, numericInput("norm_mu_x2", "\u03bc X:", 0)), column(6, numericInput("norm_sd_x2", "\u03c3 X:", 1, min = 0.001))),
                                                conditionalPanel("input.dist_x_2d == 'exp'", column(12, numericInput("exp_rate_x2", "Rata (\u03bb) X:", 1, min = 0.001))),
                                                conditionalPanel("input.dist_x_2d == 'unif'", column(6, numericInput("unif_a_x2", "Min (a) X:", 0)), column(6, numericInput("unif_b_x2", "Max (b) X:", 1))),
                                                conditionalPanel("input.dist_x_2d == 'gamma'", column(6, numericInput("gamma_shape_x2", "Forma (k) X:", 2, min = 0.001)), column(6, numericInput("gamma_scale_x2", "Scala (\u03b8) X:", 1, min = 0.001)))
                                              )
                                          )
                                   ),
                                   column(6,
                                          div(style = "background-color: #fcfcfc; padding: 15px; border-radius: 8px; border: 1px solid #eee;",
                                              strong("Variabila Y", style = "color: #E7131A; display: block; margin-bottom: 10px;"),
                                              selectInput("dist_y_2d", "Repartiție Y:", choices = c("Normală" = "norm", "Exponențială" = "exp", "Uniformă" = "unif", "Gamma" = "gamma")),
                                              fluidRow(
                                                conditionalPanel("input.dist_y_2d == 'norm'", column(6, numericInput("norm_mu_y2", "\u03bc Y:", 0)), column(6, numericInput("norm_sd_y2", "\u03c3 Y:", 1, min = 0.001))),
                                                conditionalPanel("input.dist_y_2d == 'exp'", column(12, numericInput("exp_rate_y2", "Rata (\u03bb) Y:", 1, min = 0.001))),
                                                conditionalPanel("input.dist_y_2d == 'unif'", column(6, numericInput("unif_a_y2", "Min (a) Y:", 0)), column(6, numericInput("unif_b_y2", "Max (b) Y:", 1))),
                                                conditionalPanel("input.dist_y_2d == 'gamma'", column(6, numericInput("gamma_shape_y2", "Forma (k) Y:", 2, min = 0.001)), column(6, numericInput("gamma_scale_y2", "Scala (\u03b8) Y:", 1, min = 0.001)))
                                              )
                                          )
                                   )
                                 )
                ),
                conditionalPanel("input.gen_type == 'bivnorm'",
                                 div(style = "background-color: #fcfcfc; padding: 15px; border-radius: 8px; border: 1px solid #eee;",
                                     strong("Parametrii Repartiției Normale Bidimensionale", style = "color: #E7131A; display: block; margin-bottom: 10px;"),
                                     fluidRow(
                                       column(4, numericInput("biv_mu_x", "\u03bc_X (Media X):", 15), numericInput("biv_mu_y", "\u03bc_Y (Media Y):", 65)),
                                       column(4, numericInput("biv_sd_x", "\u03c3_X (Deviație X):", 40, min = 0.001), numericInput("biv_sd_y", "\u03c3_Y (Deviație Y):", 69, min = 0.001)),
                                       column(4, sliderInput("biv_rho", "Corelație (\u03c1):", min = -0.99, max = 0.99, value = 0.5, step = 0.01))
                                     )
                                 )
                )
              ),
              br(),
              layout_columns(
                card(card_header("Scatterplot (X, Y)", style = "background-color: #F8F8F8; color: #E7131A; font-weight: bold; border-top: 3px solid #E7131A;"), plotOutput("plot_scatter")),
                card(card_header("Matricea de Analiză a Dispersiei", style = "background-color: #E7131A; color: white;"), tableOutput("stat_2d"))
              ),
              layout_columns(
                card(card_header("Histogramă X"), plotOutput("plot_x_2d")),
                card(card_header("Histogramă Y"), plotOutput("plot_y_2d")),
                card(card_header("Histogramă Z"), plotOutput("plot_z_2d"))
              )
            )
  )
)
server <- function(input, output, session) {
  
  data_1d <- eventReactive(input$sim_1d, {
    validate(need(input$n_samples > 0, "Eșantionul trebuie să fie > 0!"))
    n <- input$n_samples
    x <- numeric(n)
    
    if(input$dist_x == "norm") {
      validate(need(input$norm_sd > 0, "Deviația standard trebuie să fie strict pozitivă!"))
      x <- rnorm(n, input$norm_mu, input$norm_sd)
    } else if(input$dist_x == "exp") {
      validate(need(input$exp_rate > 0, "Rata (\u03bb) trebuie să fie strict pozitivă!"))
      x <- rexp(n, input$exp_rate)
    } else if(input$dist_x == "unif") {
      validate(need(input$unif_a < input$unif_b, "Minimul trebuie să fie mai mic decât maximul!"))
      x <- runif(n, input$unif_a, input$unif_b)
    } else if(input$dist_x == "gamma") {
      validate(need(input$gamma_shape > 0 && input$gamma_scale > 0, "Parametrii Gamma trebuie să fie pozitivi!"))
      x <- rgamma(n, shape = input$gamma_shape, scale = input$gamma_scale)
    }
    
    y <- numeric(n)
    valid_mask <- rep(TRUE, n)
    msg <- ""
    
    if(input$trans_g == "sq") {
      y <- x^2
      msg <- "Transformarea a produs valori strict pozitive sau nule."
    } else if(input$trans_g == "abs") {
      y <- abs(x)
      msg <- "Transformarea a pliat distribuția producând valori strict pozitive sau nule."
    } else if(input$trans_g == "log") {
      valid_mask <- x > 0
      y[valid_mask] <- log(x[valid_mask])
      if(sum(!valid_mask) > 0) {
        msg <- paste("Atenție: s-au exclus", sum(!valid_mask), "valori \u2264 0 pentru care log(X) nu este definit.")
      } else {
        msg <- "Transformarea a comprimat valorile mari și a creat cozi lungi pentru valorile subunitare."
      }
    } else if(input$trans_g == "exp_t") {
      y <- exp(x)
      msg <- "Transformarea a accentuat puternic valorile extreme pozitive."
    } else if(input$trans_g == "sigmoid") {
      y <- 1 / (1 + exp(-x))
      msg <- "Transformarea a comprimat toate valorile în intervalul (0, 1)."
    }
    
    inf_mask <- is.finite(y)
    valid_mask <- valid_mask & inf_mask
    if(sum(!inf_mask) > 0) {
      msg <- paste(msg, "Atenție: S-au exclus", sum(!inf_mask), "valori rezultate care sunt prea mari (depășesc limita de calcul a programului).")
    }
    
    list(x = x[valid_mask], y = y[valid_mask], msg = msg, og_x = x)
  }, ignoreNULL = FALSE)
  
  output$interp_msg <- renderUI({
    res <- data_1d()
    sym_msg <- ""
    
    if(length(res$y) > 2) {
      sk_x <- (sum((res$og_x - mean(res$og_x))^3) / length(res$og_x)) / (sd(res$og_x)^3)
      sk_y <- (sum((res$y - mean(res$y))^3) / length(res$y)) / (sd(res$y)^3)
      
      if(!is.na(sk_x) && !is.na(sk_y)) {
        if(abs(sk_x) < 0.3 && abs(sk_y) > 0.5) sym_msg <- "Transformarea a modificat simetria distribuției (a devenit asimetrică)."
        if(abs(sk_x) > 0.5 && abs(sk_y) < 0.3) sym_msg <- "Transformarea a modificat simetria distribuției (a devenit mai simetrică)."
      }
    }
    
    div(class = "alert alert-danger", style = "background-color: #FFF0F0; color: #E7131A; border-left: 5px solid #E7131A;",
        strong("Interpretare Automată: "), res$msg, br(), sym_msg)
  })
  
  # Funcție standard pentru grafice (cum era înainte pentru Y și 2D)
  plot_hist_dens <- function(data_vec, title, fill_col) {
    ggplot(data.frame(val = data_vec), aes(x = val)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, fill = fill_col, color = "white", alpha = 0.8) +
      geom_density(color = "#2B2B2B", linewidth = 1) +
      theme_minimal() +
      labs(title = title, x = "Valoare", y = "Densitate") +
      theme(plot.title = element_text(face = "bold", color = "#2B2B2B"), panel.grid.minor = element_blank())
  }
  
  # REZOLVARE PUNCT 1: Graficul X cu Densitatea TEORETICĂ matematică
  output$plot_x <- renderPlot({ 
    res <- data_1d()
    p <- ggplot(data.frame(val = res$og_x), aes(x = val)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#8A8A8A", color = "white", alpha = 0.8) +
      theme_minimal() +
      labs(title = "Histogramă și Densitate TEORETICĂ X", x = "Valoare", y = "Densitate") +
      theme(plot.title = element_text(face = "bold", color = "#2B2B2B"), panel.grid.minor = element_blank())
    
    # Adăugăm curba perfectă (teoretică) în funcție de ce a ales utilizatorul
    if(input$dist_x == "norm") {
      p <- p + stat_function(fun = dnorm, args = list(mean = input$norm_mu, sd = input$norm_sd), color = "#E7131A", linewidth = 1.2)
    } else if(input$dist_x == "exp") {
      p <- p + stat_function(fun = dexp, args = list(rate = input$exp_rate), color = "#E7131A", linewidth = 1.2)
    } else if(input$dist_x == "unif") {
      p <- p + stat_function(fun = dunif, args = list(min = input$unif_a, max = input$unif_b), color = "#E7131A", linewidth = 1.2)
    } else if(input$dist_x == "gamma") {
      p <- p + stat_function(fun = dgamma, args = list(shape = input$gamma_shape, scale = input$gamma_scale), color = "#E7131A", linewidth = 1.2)
    }
    return(p)
  })
  
  output$plot_y <- renderPlot({ 
    validate(need(length(data_1d()$y) > 0, "Toate valorile au depășit limita de calcul și nu pot fi reprezentate grafic."))
    plot_hist_dens(data_1d()$y, "Histogramă și Densitate Y", "#E7131A") 
  })
  
  calc_stats <- function(v) {
    data.frame(
      Indicator = c("Media Empirică", "Dispersia Empirică", "Deviația Standard", "Minim", "Cuartila 1 (25%)", "Mediana", "Cuartila 3 (75%)", "Maxim"),
      Valoare = format(round(c(mean(v), var(v), sd(v), min(v), quantile(v, 0.25), median(v), quantile(v, 0.75), max(v)), 4), nsmall=4)
    )
  }
  
  output$stat_x <- renderTable({ 
    calc_stats(data_1d()$og_x) 
  }, align = "lr", width = "100%")
  
  output$stat_y <- renderTable({ 
    validate(need(length(data_1d()$y) > 0, "Fără date valide."))
    calc_stats(data_1d()$y) 
  }, align = "lr", width = "100%")
  
  data_2d <- eventReactive(input$sim_2d, {
    validate(need(input$n_samples_2d > 0, "Eșantion invalid!"))
    n <- input$n_samples_2d
    
    if(input$gen_type == "indep") {
      
      if(input$dist_x_2d == "norm") {
        validate(need(input$norm_sd_x2 > 0, "\u03c3_X > 0 !"))
        x <- rnorm(n, input$norm_mu_x2, input$norm_sd_x2)
      } else if(input$dist_x_2d == "exp") {
        validate(need(input$exp_rate_x2 > 0, "\u03bb_X > 0 !"))
        x <- rexp(n, input$exp_rate_x2)
      } else if(input$dist_x_2d == "unif") {
        validate(need(input$unif_a_x2 < input$unif_b_x2, "Min < Max pentru X!"))
        x <- runif(n, input$unif_a_x2, input$unif_b_x2)
      } else if(input$dist_x_2d == "gamma") {
        validate(need(input$gamma_shape_x2 > 0 && input$gamma_scale_x2 > 0, "Parametrii Gamma X trebuie să fie pozitivi!"))
        x <- rgamma(n, shape = input$gamma_shape_x2, scale = input$gamma_scale_x2)
      }
      
      if(input$dist_y_2d == "norm") {
        validate(need(input$norm_sd_y2 > 0, "\u03c3_Y > 0 !"))
        y <- rnorm(n, input$norm_mu_y2, input$norm_sd_y2)
      } else if(input$dist_y_2d == "exp") {
        validate(need(input$exp_rate_y2 > 0, "\u03bb_Y > 0 !"))
        y <- rexp(n, input$exp_rate_y2)
      } else if(input$dist_y_2d == "unif") {
        validate(need(input$unif_a_y2 < input$unif_b_y2, "Min < Max pentru Y!"))
        y <- runif(n, input$unif_a_y2, input$unif_b_y2)
      } else if(input$dist_y_2d == "gamma") {
        validate(need(input$gamma_shape_y2 > 0 && input$gamma_scale_y2 > 0, "Parametrii Gamma Y trebuie să fie pozitivi!"))
        y <- rgamma(n, shape = input$gamma_shape_y2, scale = input$gamma_scale_y2)
      }
      
    } else {
      validate(need(input$biv_sd_x > 0 && input$biv_sd_y > 0, "Deviațiile standard trebuie să fie strict pozitive!"))
      mu <- c(input$biv_mu_x, input$biv_mu_y)
      cov_xy <- input$biv_rho * input$biv_sd_x * input$biv_sd_y
      sigma <- matrix(c(input$biv_sd_x^2, cov_xy, cov_xy, input$biv_sd_y^2), 2, 2)
      biv_data <- mvrnorm(n, mu, sigma)
      x <- biv_data[, 1]
      y <- biv_data[, 2]
    }
    
    z <- switch(input$trans_h, "add" = x + y, "sub" = x - y, "dist" = sqrt(x^2 + y^2), "mul" = x * y)
    
    valid <- is.finite(x) & is.finite(y) & is.finite(z)
    list(x = x[valid], y = y[valid], z = z[valid])
  }, ignoreNULL = FALSE)
  
  output$plot_scatter <- renderPlot({
    res <- data_2d()
    validate(need(length(res$x) > 0, "Valori prea mari detectate. Nu se poate genera graficul."))
    ggplot(data.frame(x = res$x, y = res$y), aes(x = x, y = y)) +
      geom_point(alpha = 0.6, color = "#E7131A", size = 2) +
      theme_minimal() +
      labs(x = "Axa X", y = "Axa Y") +
      theme(panel.grid.minor = element_blank())
  })
  
  output$plot_x_2d <- renderPlot({ 
    validate(need(length(data_2d()$x) > 0, "Fără date."))
    plot_hist_dens(data_2d()$x, "Distribuție X", "#8A8A8A") 
  })
  
  output$plot_y_2d <- renderPlot({ 
    validate(need(length(data_2d()$y) > 0, "Fără date."))
    plot_hist_dens(data_2d()$y, "Distribuție Y", "#8A8A8A") 
  })
  
  output$plot_z_2d <- renderPlot({ 
    validate(need(length(data_2d()$z) > 0, "Valori care depășesc limita de calcul detectate."))
    plot_hist_dens(data_2d()$z, "Z = h(X, Y)", "#E7131A") 
  })
  
  
  output$stat_2d <- renderTable({
    res <- data_2d()
    validate(need(length(res$x) > 0, "Fără date valide."))
    
     
    format_num <- function(val) format(round(val, 4), nsmall=4)
    
    data.frame(
      Metrică = c("Media", "Dispersia Empirică", "Covarianța Empirică (X,Y)", "Corelația Empirică (X,Y)"),
      `Variabila X` = c(format_num(mean(res$x)), format_num(var(res$x)), format_num(cov(res$x, res$y)), format_num(cor(res$x, res$y))),
      `Variabila Y` = c(format_num(mean(res$y)), format_num(var(res$y)), "-", "-"),
      `Variabila Z` = c(format_num(mean(res$z)), format_num(var(res$z)), "-", "-")
    )
  }, align = "lccc", width = "100%")
}

shinyApp(ui, server)