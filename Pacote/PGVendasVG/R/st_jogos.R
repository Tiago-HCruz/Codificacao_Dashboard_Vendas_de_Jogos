#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select summarise mutate rename filter group_by `%>%`
#' @importFrom highcharter hchart hcaes hc_title hc_add_theme hc_subtitle hc_yAxis hc_theme_darkunica hc_tooltip
#' @export
#'
#' @description
#' Uma função que retorna um gráfico da série sobre as vendas de jogos,
#' acumalado para cada localidade por Ano, filtrados por gênero e plataforma.
#'
#' @title Série de Vendas de jogos por localidade.
#'
#' @param Gen: Gênero de preferência, se for todos, o argumento deve ser "Todos".
#' @param Plat: Plataforma de preferência, se for todos, o argumento deve ser "Todos".
#' @param Data_inicio: Data inicial do intervalo da série.
#' @param Data_final:  Data final do intervalo da série.
#'
#' @examples
#' # Segue abaixo, alguns valores dos demais parâmetros de entradas como exemplo:
#' st_jogos("Todos", "Todos", 2000, 2010)
#' # outro Exemplo em comentario:
#' # st_jogos("Action", "PS2", 2000, 2010)

st_jogos <- function(Gen, Plat, Data_inicio, Data_final) {
db_st %>%
  {if (`Gen` %in% "Todos") select(.,-`Gênero`) else filter(.,`Gênero` %in% `Gen`)} %>%
  {if (`Plat` %in% "Todos") select(.,-`Plataforma`) else filter(.,`Plataforma` %in% `Plat`)} %>%
  filter(Ano >= Data_inicio & Ano <= Data_final) |>
  group_by(Ano,  Continentes) |>
  summarise(Vendas = sum(Vendas)) |>
  hchart('line', hcaes(x = Ano, y = Vendas, group = Continentes)) |>
  hc_title(text = "Series de vendas de jogos por continentes/País") |>
  hc_add_theme(hc_theme_darkunica()) |>
  hc_subtitle(text = paste0("Gênero: ","'",Gen,"'", ", Plataforma: ","'",Plat,"'")) |>
  hc_yAxis(labels = list(format = "{value} M")) |>
  hc_tooltip(crosshairs = TRUE, shared = TRUE, headerFormat = "<b>{point.name}</b>",
             valueSuffix = " M")
  }
