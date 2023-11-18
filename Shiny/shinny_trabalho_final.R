library(shiny)
library(shinydashboard)
library(PGVendasVG)
library(highcharter)
library(fresh)

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
    info_box_bg = "#D8DEE9"
  )

)

ui <- dashboardPage(

  dashboardHeader(title = "Vendas de Jogos"),
  dashboardSidebar(
    sidebarMenu(menuItem("Serie", tabName = "Serie",
                         icon = icon("chart-line")),
                menuItem("Densidade", tabName = "Densidade",
                         icon = icon("chart-bar")) ,
                menuItem(text = "Membros do Grupo", icon = icon("users"),
                         menuSubItem(text = HTML("<b>  Nome:</b> Tiago, <b>RA:</b> 206333")),
                         menuSubItem(text = HTML("<b>  Nome:</b> Nome, <b>RA:</b> ******")),
                         menuSubItem(text = HTML("<b>  Nome:</b> Nome, <b>RA:</b> ******")),
                         menuSubItem(text = HTML("<b>  Nome:</b> Nome, <b>RA:</b> ******")))
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
      tabItem(tabName = "Densidade",
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
              highchartOutput("den"))
      )
    )
  )

server <- function(input, output) {


  output$st <- renderHighchart({
    st_jogos(input$Gen, input$Plat, input$data[1], input$data[2])
  })


  output$den <- renderHighchart({
    densidade(Dados, input$Cat, input$Pais)
  })

}

# Run the application
shinyApp(ui = ui,
         server = server)

