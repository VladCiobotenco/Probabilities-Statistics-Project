library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Prima mea aplicație Shiny"),
  h3("Bine ați venit!"),
  p("În această aplicație vom analiza vizual un set de date.")
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)