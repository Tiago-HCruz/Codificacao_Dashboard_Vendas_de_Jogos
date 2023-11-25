library(shiny)
library(shinydashboard)
library(highcharter)
library(fresh)

if(!require("PGVendasVG", quietly = TRUE)){
  devtools::install_github("ti-cruz/PGVendasVG", force = TRUE)
}

#if(!require("PGVendasVG", quietly = TRUE)){
#  install.packages(file.path(getwd(),"PGVendasVG_0.1.0.tar.gz"),
#                   repos = NULL, type = "source")
#}
library(PGVendasVG)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),

  adminlte_sidebar(
    dark_bg = "#353537",
    dark_hover_bg = "#606165",
    dark_color = "#ffffff7d"
  ),

  adminlte_global(
    content_bg = "#f1f3f5",
    box_bg = "#bdc1c6",
    info_box_bg = "#ffffff"
  )

)

ui <- dashboardPage(

  dashboardHeader(title = "Vendas de Jogos"),
  dashboardSidebar(
    sidebarMenu(menuItem("Serie", tabName = "Serie",
                         icon = icon("chart-line")),
                menuItem("Gráfico de Barras", tabName = "Grafico_Barras",
                         icon = icon("chart-bar")),
                menuItem("Intervalo Boostrap", tabName = "Intervalo_Boostrap",
                         icon = icon("clipboard")),
                menuItem(text = "Grupo D", icon = icon("users"),
                         menuSubItem(text = HTML("<b>  Nome:</b> João, <b>RA:</b> 199910")),
                         menuSubItem(text = HTML("<b>  Nome:</b> Ricardo, <b>RA:</b> 243887")),
                         menuSubItem(text = HTML("<b>  Nome:</b> Samuel, <b>RA:</b> 193819")),
                         menuSubItem(text = HTML("<b>  Nome:</b> Tiago, <b>RA:</b> 206333")))
                )
    ),

  dashboardBody(

    tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    use_theme(mytheme),

    tabItems(
      tabItem(tabName = "Serie",
              fluidRow(box(status = "primary", solidHeader=TRUE,
                           sliderInput("data", "Qual o intervalo do ano desejavél:",
                                min = 1980,
                                max = 2020,
                                value = c(2000, 2018),
                                sep = "",
                                dragRange = T)),

                       box(status = "primary", solidHeader = TRUE,
                           width = 3, selectInput("Gen",
                              "Gênero:",
                              c("Todos", vet_obs$gen))),

                       box(status = "primary", solidHeader = TRUE,
                           width = 3, selectInput("Plat",
                                   "Plataforma:",
                                   c("Todos", vet_obs$plat)))),

              highchartOutput("st")),
      tabItem(tabName = "Grafico_Barras",
              fluidRow(box(status = "primary", solidHeader = TRUE,
                           width = 3, selectInput("Cat",
                                                  "Categoria:",
                                                  c(vet_obs$Categoria)
                                                  )
                           ),
                       box(status = "primary", solidHeader = TRUE,
                           width = 3, selectInput("Pais",
                                                  "Vendas pelo País:",
                                                  c(vet_obs$Vendas)
                                                  )
                           ),
                       valueBox(vet_obs$plat |>
                                  length(),
                                subtitle =  HTML("Número de Plataforma"),
                                icon = icon("gamepad"), width = 3),
                       valueBox(vet_obs$gen |>
                                  length()
                                , subtitle =  HTML("Número de Gêneros"),
                                icon = icon("layer-group"), width = 3)
                       ),
              highchartOutput("Graf_Barras")),
      tabItem(tabName = "Intervalo_Boostrap",
              fluidRow(box(status = "primary", solidHeader = TRUE,
                           numericInput("n",
                                        "Quantas amostras devem ser feitas no método bootstrap",
                                        100, min = 10, max = 50000)),
                       infoBoxOutput("Int_sup_inf")),
             dataTableOutput("Intervalo_Boostrap"))
      )
    )
  )

server <- function(input, output) {


  output$st <- renderHighchart({
    st_jogos(input$Gen, input$Plat, input$data[1], input$data[2])
  })


  output$Graf_Barras <- renderHighchart({
    Graf_barras(input$Cat, input$Pais)
  })

  output$Intervalo_Boostrap <- renderDataTable({
    IBoots_Vendas(input$n)},
    options = list(searching = FALSE, paging = FALSE, info = FALSE))

  output$Int_sup_inf <- renderInfoBox({
    infoBox(
      HTML("Intervalo <br> Superior e Inferior"), "95%",
      icon = icon("down-left-and-up-right-to-center", lib = "font-awesome"),
      fill = F
    )
  })
}

# Run the application
shinyApp(ui = ui,
         server = server)

