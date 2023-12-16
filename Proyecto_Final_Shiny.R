library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(readr)
library(shinyjs)
library(readr)
library(shinyalert)
library(tidyr)



imdb <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-02-19/ranking_imdb.csv")
head(imdb)
View(imdb)
summary(imdb)
str(imdb)



  distinct(titulo, .keep_all = TRUE) |> 
  separate_rows(genero, sep = ", ")


scatterPlotModule <- function(input, output, session, data) {
  output$scatter_plot <- renderPlotly({
    plot_ly(data = data(), x = ~ranking, y = ~puntaje, color = ~anio, type = "scatter", mode = "markers", text = ~titulo) %>%
      layout(title = "Ranking vs. Puntaje de Películas IMDB",
             xaxis = list(title = "Posición en el Ranking"),
             yaxis = list(title = "Puntaje"))
  })
}


downloadDataModule <- function(input, output, session, data) {
  output$download_data <- downloadHandler(
    filename = function() {
      paste("datos_filtrados_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(data(), file)
    }
  )
}


ui <- dashboardPage(
  dashboardHeader(title = "Explorador de Películas IMDB"),
  dashboardSidebar(
    selectInput("genre", "Género:", choices = unique(imdb$genero)),
    sliderInput("rating", "Puntaje:", min = 1, max = 10, value = c(1, 10)),
    sliderInput("votes", "Cantidad de Votos:", min = 0, max = max(imdb$votos), value = c(0, max(imdb$votos))),
    sliderInput("year", "Año de Estreno:", min = min(imdb$anio), max = max(imdb$anio), value = c(min(imdb$anio), max(imdb$anio))),
    shinyalert("download_alert", "Descarga Exitosa", "Los datos han sido descargados exitosamente.", type = "success")
  ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("scatter_plot")),
      box(downloadButton("download_data", "Descargar Datos"))
    )
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
  

  callModule(scatterPlotModule, "scatter_plot", filtered_data)
  callModule(downloadDataModule, "download_data", filtered_data)
  
 
  observeEvent(input$download_data, {
    shinyalert("download_alert")
  })
}


shinyApp(ui, server)





