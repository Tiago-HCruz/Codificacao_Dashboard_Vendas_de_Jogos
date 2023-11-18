#' @importFrom dplyr group_by summarise
#' @importFrom highcharter hchart hcaes hc_add_theme hc_theme_darkunica hc_yAxis hc_xAxis  hc_tooltip hc_plotOptions
#' @export
#'
#' @title Gráfico de barras
#'
#' @description
#' Esta função retorna um gráfico de barras de acordo com o data frame
#' passado e duas variáveis escolhidas desse mesmo data frame
#'
#' @param data data frame
#' @param x_var variável do data frame que ficará no eixo x
#' @param y_var variável do data frame que ficará no eixo y
#' @return um gráfico de barras
#'
#' @examples
#' densidade(Dados, "Genre", "EU_Sales")

densidade = function(data, x_var, y_var) {
  data |> group_by(.data[[x_var]]) |>
    summarise(`Vendas Totais` = sum(.data[[y_var]])) |>
    na.omit() |>
    hchart("bar", hcaes(x = .data[[x_var]], y = `Vendas Totais`),
           name = "Vendas",
           tooltip = list(pointFormat = "Vendas: <b>{point.y} M</b>")
           ) |>
    hc_title(text = paste0("Densidade das vendas totais")) |>
    hc_subtitle(text = paste0("Localidade: ", ifelse(y_var %in% vet_obs$Vendas[1],
                                                     vet_obs$Vendas_br[1],
                                                     ifelse(y_var %in% vet_obs$Vendas[2],
                                                            vet_obs$Vendas_br[2],
                                                            ifelse(y_var %in% vet_obs$Vendas[3],
                                                                   vet_obs$Vendas_br[3],
                                                                   ifelse(y_var %in% vet_obs$Vendas[4],
                                                                          vet_obs$Vendas_br[4],
                                                                          vet_obs$Vendas_br[5])))))) |>
    hc_yAxis(labels = list(format = "{value} M")) |>
    hc_xAxis(title = list(text = ifelse(x_var %in% "Genre",
                                        "Gênero", "Plataforma"))) |>
    hc_add_theme(hc_theme_darkunica())
}
