library(shiny)
library(shinydashboard)
library(highcharter)
library(fresh)

if(!require("PGVendasVG", quietly = TRUE)){
  devtools::install_github("ti-cruz/PGVendasVG", force = TRUE)
}

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

title <- tags$main(tags$img(src = "https://cdn-icons-png.flaticon.com/512/2780/2780137.png",
                           height = '35', width = '35'),
                "Vendas de Jogos")

ui <- dashboardPage(
  title="Vendas de Jogos | Dashboard",

  dashboardHeader(title = title,
                  tags$li(class = 'dropdown',
                          tags$a(href = "https://github.com/Tiago-HCruz/PGVendasVG/tree/main",
                                 icon('box-open'),
                                 "Pacote Utilizado",
                                 target="_black")),
                  tags$li(class = 'dropdown',
                          tags$a(icon("github"),
                                 "Codificação",
                                 target="_black"))),
  dashboardSidebar(
    sidebarMenu(
                h6(HTML("<center>Para fazer esse dashboard, <br>
                        foi utilizado o software <b>linguagem R</b> <br>
                        com o banco de dados <a href='https://www.kaggle.com/datasets/ghassenkhaled/video-games-data'target='_blank'><font color='#52deff'>Video Games Data</font></a><br>
                        e o pacote <a href='https://github.com/Tiago-HCruz/PGVendasVG' target='_blank'><font color='#52deff'>PGVendasVG</font></a> <center/>
                        (feito pelo proprio <b>grupo</b>)")),
                menuItem("Serie", tabName = "Serie",
                         icon = icon("chart-line")),
                menuItem("Gráfico de Barras", tabName = "Grafico_Barras",
                         icon = icon("chart-bar")),
                menuItem("Intervalo Boostrap", tabName = "Intervalo_Boostrap",
                         icon = icon("clipboard")),
                menuItem(text = "Grupo D", icon = icon("users"),
                         menuSubItem(text = HTML("<b>João Matovani</b> "),
                                     icon = icon("user")),
                         menuSubItem(text = HTML("<b>Ricardo Shiraishi</b>"),
                                     href = "https://www.linkedin.com/in/ricardo-shiraishi-567883208/",
                                     icon = icon("linkedin")),
                         menuSubItem(text = HTML("<b>Samuel Volpe</b> "),
                                     icon = icon("user")),
                         menuSubItem(text = HTML("<b>Tiago Cruz</b>"),
                                     href = "https://www.linkedin.com/in/tiago-cruz-b65721284/",
                                     icon = icon("linkedin")))
                )
    ),

  dashboardBody(
    tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
       tags$link(rel = "icon", type = "image/png", href = "https://cdn-icons-png.flaticon.com/512/2780/2780137.png"),
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

