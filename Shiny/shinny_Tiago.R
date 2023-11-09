library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)
library(highcharter)
library(shinydashboard)

Dados <-  read_csv(file.path(getwd(), "Dados","Video_Games.csv"))

St<- function(db, Gen, Plat, Data_inicio, Data_final) {

db |> select(Year_of_Release, Genre, Platform,
                NA_Sales, EU_Sales, JP_Sales, Other_Sales,
             Global_Sales)  |>
  pivot_longer(!c(Year_of_Release, Genre, Platform), names_to = "Continentes",
               values_to = "Vendas") |>
  na.omit() |>
  mutate(Year_of_Release = as.integer(Year_of_Release),
         Continentes = ifelse(Continentes %in% "NA_Sales", "América do Norte",
                              ifelse(Continentes %in% "EU_Sales", "União Europeia",
                                     ifelse(Continentes %in% "JP_Sales", "Japão",
                                            ifelse(Continentes %in% "Other_Sales", "Outros",
                                                   "Total"))))) |>
  #filter(Year_of_Release %in% NA)
  rename("Ano" = "Year_of_Release",
         "Gênero" = "Genre",
          "Plataforma" = "Platform") %>%
  {if (`Gen` %in% "Todos") select(.,-`Gênero`) else filter(.,`Gênero` %in% `Gen`)} %>%
  {if (`Plat` %in% "Todos") select(.,-`Plataforma`) else filter(.,`Plataforma` %in% `Plat`)} %>%
  filter(Ano >= Data_inicio & Ano <= Data_final) |>
  group_by(Ano,  Continentes) |>
  summarise(Vendas = sum(Vendas)) |>
  hchart('line', hcaes(x = Ano, y = Vendas, group = Continentes)) |>
  hc_title(text = "Series de vendas de jogos por continentes/País") |>
  hc_add_theme(hc_theme_darkunica()) |>
  hc_subtitle(text = paste0("Gênero: ","'",Gen,"'", ", Plataforma: ","'",Plat,"'")) |>
  hc_yAxis(labels = list(format = "{value} M"))
}


ui <- fluidPage(
  titlePanel("Vendas de Jogos"),

  sliderInput("data", "Qual o intervalo do Ano", min = 1980, max = 2020, value = c(2000, 2018),
              sep = "", dragRange = T),

  selectInput("Gen", "Gênero:", c("Todos", Dados |>
                                   select(Genre) |>
                                   distinct() |>
                                   na.omit() %>%
                                   .$Genre)),

  selectInput("Plat", "Plataforma:", c("Todos", Dados |>
                                    select(Platform) |>
                                    distinct() |>
                                    na.omit() %>%
                                    .$Platform)),

  highchartOutput("st")
)

server <- function(input, output) {


  output$st <- renderHighchart({
    St(Dados, input$Gen, input$Plat, input$data[1], input$data[2])
  })


}

# Run the application
shinyApp(ui = ui,
         server = server)

