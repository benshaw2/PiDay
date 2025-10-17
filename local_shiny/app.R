# app.R - Local Shiny app for Pi Estimation
# Depends on pi_core.R in parent folder
# Run locally with: shiny::runApp("local_shiny")

library(shiny)
library(DT)
library(ggplot2)
source("../web_version/pi_core.R", local = TRUE)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Estimate π from Circle Measurements"),

  sidebarLayout(
    sidebarPanel(
      textInput("name", "Name:", placeholder = "e.g., Alice"),
      numericInput("diameter", "Diameter:", value = NA, min = 0, step = 0.01),
      numericInput("circumference", "Circumference:", value = NA, min = 0, step = 0.01),
      actionButton("add", "Add row"),
      actionButton("clear", "Clear data"),
      tags$hr(),
      selectInput(
        "model",
        "Model type:",
        choices = list(
          "Fixed effects only (lm)" = 1,
          "Random intercept (lme4)" = 2,
          "Random intercept + slope (lme4)" = 3
        ),
        selected = 1
      ),
      radioButtons(
        "format",
        "Download plot as:",
        choices = c("PNG" = "png", "SVG" = "svg"),
        inline = TRUE
      ),
      tags$hr(),
      actionButton("fit", "Estimate π", class = "btn-primary"),
      downloadButton("downloadData", "Download Data"),
      downloadButton("downloadPlot", "Download Plot")
    ),

    mainPanel(
      h3("Results"),
      verbatimTextOutput("message"),
      textOutput("piText"),
      plotOutput("plot", height = "400px"),
      br(),
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {

  # reactive dataset
  df <- reactiveVal(
    data.frame(Name = character(0), Diameter = numeric(0), Circumference = numeric(0))
  )

  observeEvent(input$add, {
    if (!is.na(input$diameter) && !is.na(input$circumference) && nzchar(input$name)) {
      new_row <- data.frame(
        Name = input$name,
        Diameter = input$diameter,
        Circumference = input$circumference
      )
      df(rbind(df(), new_row))
    } else {
      showNotification("Please fill in all fields before adding.", type = "warning")
    }
  })

  observeEvent(input$clear, {
    df(data.frame(Name = character(0), Diameter = numeric(0), Circumference = numeric(0)))
  })

  output$table <- renderDT({
    datatable(df(), options = list(pageLength = 5, autoWidth = TRUE))
  })

  model_result <- eventReactive(input$fit, {
    d <- df()
    if (nrow(d) < 2) {
      return(list(ok = FALSE, message = "Need at least two data points."))
    }

    # Safe=FALSE so we use full lme4 when available
    res <- fit_pi_model(d, model_type = as.integer(input$model), safe = FALSE)
    res$df <- d
    res
  })

  output$message <- renderText({
    req(model_result())
    model_result()$message
  })

  output$piText <- renderText({
    req(model_result())
    if (isTRUE(model_result()$ok)) {
      paste("Estimated π ≈", round(model_result()$slope, 5))
    }
  })

  output$plot <- renderPlot({
    res <- model_result()
    req(res$ok)
    d <- res$df
    slope <- res$slope
    intercept <- res$intercept

    ggplot(d, aes(x = Diameter, y = Circumference, color = Name)) +
      geom_point(size = 3, alpha = 0.8) +
      theme_minimal(base_size = 14) +
      labs(
        title = "Circumference vs Diameter",
        subtitle = paste("Estimated π ≈", round(slope, 4)),
        x = "Diameter",
        y = "Circumference"
      ) +
      geom_abline(slope = slope, intercept = intercept, color = "firebrick", size = 1)
  })

  # Download handlers
  output$downloadData <- downloadHandler(
    filename = function() "pi_measurements.csv",
    content = function(file) {
      write.csv(df(), file, row.names = FALSE)
    }
  )

  output$downloadPlot <- downloadHandler(
    filename = function() paste0("pi_plot.", input$format),
    content = function(file) {
      res <- model_result()
      req(res$ok)
      d <- res$df
      slope <- res$slope
      intercept <- res$intercept

      library(ggplot2)
      p <- ggplot(d, aes(x = Diameter, y = Circumference, color = Name)) +
        geom_point(size = 3, alpha = 0.8) +
        theme_minimal(base_size = 14) +
        labs(
          title = "Circumference vs Diameter",
          subtitle = paste("Estimated π ≈", round(slope, 4)),
          x = "Diameter",
          y = "Circumference"
        ) +
        geom_abline(slope = slope, intercept = intercept, color = "firebrick", size = 1)

      if (input$format == "svg") {
        ggsave(file, plot = p, width = 8, height = 6, device = "svg")
      } else {
        ggsave(file, plot = p, width = 8, height = 6, dpi = 100)
      }
    }
  )
}

shinyApp(ui, server)

