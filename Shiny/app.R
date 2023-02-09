#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
x <- test
names(x)
as.list(names(x))
button <- as.list(seq_along(x))
names(button) <- names(x)

# interactif :
library(dygraphs)

ui <- fluidPage(

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("models",
                  "Choix du modele",
                  choices = button,
                  multiple = TRUE,
                  selected = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      dygraphOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderDygraph({
    if (is.null(input$models)) {
      # plot(1,1)
    } else {
      data_shiny <- x[as.numeric(input$models)]
      data_plot <- lapply(data_shiny, get_tvlm_bw)
      data_plot <- lapply(data_plot, ts, end = c(2019,4), frequency = 4)
      data_plot <- do.call(cbind, data_plot)
      dygraph(data_plot) |>
        dyRangeSelector()
    }
  })
}
# Run the application
shinyApp(ui = ui, server = server)
