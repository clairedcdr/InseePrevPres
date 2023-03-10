#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


x <- models
button <- names(x)
# as.list(names(x))
# button <- as.list(seq_along(x))
# names(button) <- names(x)


# Define UI for application that draws a histogram
ui <- fluidPage(verticalLayout(
  selectInput("models",
              "Choix du modele",
              choices = button,
              multiple = FALSE,
              selected = button[1]),
  sliderInput("bw",
              "Bandwidth",
              min = 0.1,
              max = 3,
              value = 1,
              step = 0.05),
  tabsetPanel(

    tabPanel("Fitted",
             dygraphOutput("distPlot")),

    tabPanel("Out of sample",
             dygraphOutput("oosPlot")),

    tabPanel("Coefficients",
             uiOutput("coefserver"),
             dygraphOutput("coefPlot"),
             hr(),
             fluidRow(column(3, verbatimTextOutput("value"))))
  )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  # r <- reactiveValues(tvlm = {
  #   data_shiny <- x[[input$models]]
  #   formule <- sprintf("%s ~ .", colnames(data_shiny$model)[1])
  #   tvlm <- tvLM(formula(formule), data = get_data(data_shiny), bw = input$bw)
  #   })
  # # reactive()
  # observeEvent({
  #   input$models
  #   input$bw
  # },{
  #   data_shiny <- x[[input$models]]
  #   formule <- sprintf("%s ~ .", colnames(data_shiny$model)[1])
  #   tvlm <- tvLM(formula(formule), data = get_data(data_shiny), bw = input$bw)
  #   r$tvlm <- tvlm
  # })
  tvlm <- eventReactive({
    input$models
    input$bw
  }, {
    data_shiny <- x[[input$models]]
    formule <- sprintf("%s ~ .", colnames(data_shiny$model)[1])
    tvLM(formula(formule), data = get_data(data_shiny), bw = input$bw)
  })

  output$distPlot <- renderDygraph({
    data_shiny <- x[[input$models]]
    tvlm_fitted <- ts(tvlm()$fitted, end = c(2019,4), frequency = 4)
    data_plot <- do.call(cbind, list(get_data(data_shiny)[,1], data_shiny$fitted.values, tvlm_fitted))
    colnames(data_plot) = c("Data", "Fitted lm", "Fitted tvlm")
    dygraph(data_plot, main = "Fitted values against data") %>%
      dyOptions(colors = c("gray", "darkred", "darkgreen"))

  })

  output$oosPlot <- renderDygraph({
    data_shiny <- x[[input$models]]
    prevision <- oos_prev(tvlm(), end = c(2019,4), frequency = 4, date = 28, fixed_bw = TRUE, bw = input$bw)
    data_plot <- do.call(cbind, list(get_data(data_shiny)[,1], prevision$prevision))
    colnames(data_plot) = c("Data", "Oos tvlm")
    dygraph(data_plot, main = "Out of sample previsions against data") %>%
      dyOptions(colors = c("gray", "blueviolet"))
  })

  output$coefserver <- renderUI({
    choices <- colnames(coef(tvlm()))
    checkboxGroupInput("coef",
                       "Coefficients",
                       choices = choices)
  })

  output$coefPlot <- renderDygraph({
    if (!is.null(input$coef)){
      tvlm_coef <- ts(coef(tvlm())[,input$coef], end = c(2019,4), frequency = 4)

      dygraph(tvlm_coef, main = "Coef")
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
