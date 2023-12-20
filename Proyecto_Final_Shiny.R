library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(tidyr)

imdb <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-02-19/ranking_imdb.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Explorador de Películas IMDB",titleWidth = 300),
  
  dashboardSidebar(width = 300,
                   selectInput("genre", "Género:", choices = unique(imdb$genero)),
                   sliderInput("rating", "Puntaje:", min = 1, max = 10, value = c(1, 10)),
                   sliderInput("votes", "Cantidad de Votos:", min = 0, max = max(imdb$votos), value = c(0, max(imdb$votos))),
                   sliderInput("year", "Año de Estreno:", min = min(imdb$anio), max = max(imdb$anio), value = c(min(imdb$anio), max(imdb$anio))),
                   
                   fluidRow(
                     column(width = 12, offset = 2,
                            downloadButton("descargarDatos", "Descargar Datos"),
                            useShinyjs()
                     )
                   )
  ),
  dashboardBody(
    column(12, plotlyOutput("scatter_plot"))
  )
)


server <- function(input, output, session) {
  filtered_data <- reactive({
    filter(imdb,
           genero == input$genre,
           puntaje >= input$rating[1] & puntaje <= input$rating[2],
           votos >= input$votes[1] & votos <= input$votes[2],
           anio >= input$year[1] & anio <= input$year[2])
  })
  
  output$scatter_plot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~ranking, y = ~puntaje, color = ~anio, type = "scatter", mode = "markers", text = ~titulo) %>%
      layout(title = "Ranking vs.Puntaje de Películas IMDB",
             xaxis = list(title = "Posición en el Ranking"),
             yaxis = list(title = "Puntaje"))
  })
  
  observe({
    if (nrow(filtered_data()) == 0) {
      shinyjs::disable("descargarDatos")
    } else {
      shinyjs::enable("descargarDatos")
    }
  })
  
  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("películas_seleccionadas/", "_", input$genre, ".csv", sep = "")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    } )
}
shinyApp(ui = ui, server = server)