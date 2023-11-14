library(shiny)
library(shinydashboard)
library(PGVendasVG)
library(highcharter)


ui <- fluidPage(
  titlePanel("Vendas de Jogos"),

  sliderInput("data", "Qual o intervalo do Ano",
              min = 1980,
              max = 2020,
              value = c(2000, 2018),
              sep = "",
              dragRange = T),

  selectInput("Gen",
              "Gênero:",
              c("Todos", vet_obs$gen)),

  selectInput("Plat",
              "Plataforma:",
              c("Todos", vet_obs$plat)),

  highchartOutput("st")
)

server <- function(input, output) {


  output$st <- renderHighchart({
    st_jogos(input$Gen, input$Plat, input$data[1], input$data[2])
  })


}

# Run the application
shinyApp(ui = ui,
         server = server)

