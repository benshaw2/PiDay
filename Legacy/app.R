library(DT)
library(shiny)
library(lme4)
library(ggplot2)

ui <- fluidPage(
  # Open navbar page and customize the theme
  navbarPage(
    title = "Estimate pi from Circumference/Diameter Measurements",
  ),
  sidebarLayout(
    sidebarPanel(
      # inputs
      textInput("name", label = h3('Enter the Name')),
      textInput("diameter", label = h3('Enter the Diameter'), value = "0.00"),
      textInput("circumference", label = h3('Enter the Circumference'), value="0.00"),
      # Radio
      radioButtons("radio",
                   label = h3("Choose an option"),
                   choices = list("Fixed Effects Only"=1, "Fixed + Random intercept"=2, "Fixed + Random slope/intercept"=3),
                   selected = 1
      ),
      # Action Buttons
      fluidRow(
        # Update table
        actionButton("submit", label = "Update Table"),
        # Clear table
        actionButton("reset", label = "Reset Table"),
        # Run Prediction
        actionButton("model", label = "Get pi"),
      ),
      textOutput("pi")
    ),
    #fluidPage(
    #  plotOutput("plot"),
    #  fluidRow(
    #    DTOutput("table")
    #  )
    #),
    mainPanel(
      fluidRow(
        plotOutput("plot"),
        DTOutput("table")
      )
    ),
  ), # Close sidebar
  
) # Close fluidPage

server <- function(input, output) {
  
  df <- reactiveVal(
    data.frame(
      Name = character(0),
      Diameter = numeric(0),
      Circumference = numeric(0)
    )
  )
  
  output$plot<-renderPlot({
    ggplot(data=df(), aes(x=Diameter, y=Circumference, color=Name)) +
    geom_point() +
    xlim(0,35) +
    ylim(0,35)
    
    #try(abline(GLM))
  })
  
  observeEvent(input$submit, {
    newdat <- data.frame(
      Name = input$name,
      Diameter = as.numeric(strsplit(input$diameter, split = ";")[[1]]),
      Circumference = as.numeric(strsplit(input$circumference, split = ";")[[1]])
    ) 
    df(dplyr::bind_rows(df(), newdat))
  })
  
  observeEvent(input$reset, {
    newdat2 <- data.frame(
      Name = character(0),
      Diameter = numeric(0), 
      Circumference = numeric(0)
    ) 
    df(dplyr::bind_rows(data.frame(), newdat2))
  })
  
  
  observeEvent(input$model, {
    if(input$radio=="1"){
      try(GLM <- lm(Circumference ~ Diameter, df()))
    }
    if(input$radio=="2"){
      try(GLM <- lme4::lmer(Circumference ~ Diameter + (1|Name), df()))
    }
    if(input$radio=="3"){
      try(GLM <- lme4::lmer(Circumference ~ Diameter + (1+Diameter|Name), df()))
    }
    
    pie <- "NULL"
    #coefs <- c(0,0)
    try(coefs <- summary(GLM)$coefficients)
    try(pie <- coefs[2]) #coef(GLM)
    msg <- paste("The estimate for pi is: ", pie)
    output$pi <- renderText(msg)
    
    output$plot<-renderPlot({
      p1 <- ggplot(data=df(), aes(x=Diameter, y=Circumference, color=Name)) +
        geom_point() +
        xlim(0,35) +
        ylim(0,35) 
        try(p1 + geom_abline(intercept = coefs[1], slope = coefs[2]) +annotate("text",x=20,y=5,label=(paste0("slope==",coefs[2]))) )
    })
    
  })
  
  output$table <- renderDataTable({
    df()
  })
}

shinyApp(ui = ui, server = server)