library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)
library(highcharter)
 dados = read.csv("Video_Games.csv")
 dados_n <- dados |> dplyr::select("Platform", "Genre", "Rating", "NA_Sales", "EU_Sales",
                                   "JP_Sales", "Other_Sales", "Global_Sales")
 #grafico de barras
 densidade = function(data, x_var, y_var) {
   dados_teste = data |> group_by(.data[[x_var]]) |> summarise(vendas_total = sum(.data[[y_var]]))
   hchart(dados_teste, "bar", hcaes(x = .data[[x_var]], y = vendas_total)) |>
     hc_add_theme(hc_theme_darkunica())
 }
 
 
 ui <- fluidPage(
   titlePanel("Vendas de Jogos"),
   
   selectInput("categoria", "Selecione uma categoria:", c("Platform",
                                                          "Genre",
                                                          "Rating")),
   selectInput("vendas", "Selecione o local de venda:", c("JP_Sales",
                                                          "Global_Sales",
                                                          "EU_Sales", 
                                                          "Other_Sales", 
                                                          "NA_Sales")),
   highchartOutput("Grafico_1")  
 )
 
 
server <- function(input, output) {
   
   output$Grafico_1 <- renderHighchart({
     densidade(dados_n,input$categoria, input$vendas)
   })
 }
# Run the application
shinyApp(ui = ui, server = server)
