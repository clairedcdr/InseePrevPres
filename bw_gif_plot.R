# triweight
triweight <- function(x) {
  ifelse(abs(x) >= 1, 0, 35/32 * (1-x^2)^3)
}
epanechnikov <- function(x) {
  ifelse(abs(x) >= 1, 0, 3/4 * (1-x^2))
}

triweight.bw <- function(x, bw = 1){
  triweight(x/bw)/bw
}
epanechnikov.bw <- function(x, bw = 1){
  epanechnikov(x/bw)/bw
}


# library(highcharter)
# library(shiny)
#
# ui <- fluidPage(
#   # Application title
#   titlePanel("Poids"),
#
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
#       selectInput('kernel', 'Select kernel',
#                   c("Triweight", "Epanechnikov"),
#                   multiple=FALSE, selectize=TRUE,
#                   selected = "Triweight"),
#       sliderInput("bw", label = h3("Bandwidth"), min = 0.1,
#                   max = 40, value = 1),
#       sliderInput("points", label = h3("Nombre de points"), min = 2,
#                   max = 400, value = 120, step = 1),
#       width = 3.
#     ),
#     # Show a plot of the generated distribution
#     mainPanel(
#       tabsetPanel(type = "tabs",
#                   tabPanel("Coefficients",
#                            highchartOutput("plot_coef", height = "80vh")))
#     )
#   ))
# server <- function(input, output) {
#   output$plot_coef <- renderHighchart({
#     x = seq(-trunc(input$points/2), length.out = input$points, by = 1) / input$points
#     if (input$kernel == "Triweight") {
#       fun <- triweight.bw
#     } else {
#       fun <- epanechnikov.bw
#     }
#     y = ts(fun(x, bw = input$bw), start = -input$points)
#     y = y /sum(y)
#     AQLTools::hc_lines(y,digits = 4)
#   })
# }
# shinyApp(ui = ui, server = server)


nb_points <- 20 * 4 + 1 # 20 annes trim
x_graph = seq(-trunc(nb_points/2), length.out = nb_points, by = 1)
x_asym = seq(-nb_points + 1, length.out = nb_points, by = 1) / nb_points
x_graph_asym = seq(-nb_points + 1, length.out = nb_points, by = 1)
data_p <- lapply(c(seq(0.05,1, by = 0.05),
                   seq(2,5, by = 1)), function(bw){
                     y = triweight.bw(x_graph / nb_points, bw = bw)
                     y = y / sum(y)
                     data.frame(x = x_graph, y = y, bw = bw)
                   })
data_p <- do.call(rbind, data_p)
data_p <- data_p[data_p$y > 0,]

data_p_asym <- lapply(c(seq(0.05,1, by = 0.05),
                        seq(2,5, by = 1)), function(bw){
                          y = triweight.bw(x_graph_asym / nb_points, bw = bw)
                          y = y / sum(y)
                          data.frame(x = x_graph_asym, y = y, bw = bw)
                        })
data_p_asym <- do.call(rbind, data_p_asym)
data_p_asym <- data_p_asym[data_p_asym$y > 0,]
library(ggplot2)
library(gganimate)
plot_anim <- ggplot(data = data_p, aes(x = x, y = y)) +
  geom_line() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) +
  labs(title = "Pondérations en fonction de b",
       subtitle = 'b = {closest_state}', y = 'Poids', x = 'Distance à t')+
  transition_states(bw)
plot_anim
anim_save("graphs_atelier/bw_anim.gif",plot_anim)

plot_anim_asym <- ggplot(data = data_p_asym, aes(x = x, y = y)) +
  geom_line() +
  theme_bw() +
  labs(title = 'bw = {closest_state}', y = 'Poids', x = 'Distance à t')+
  transition_states(bw)
plot_anim_asym
