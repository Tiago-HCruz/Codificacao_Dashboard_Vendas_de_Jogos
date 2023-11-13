#' Gráfico de barras
#'
#'Esta função retorna um gráfico de barras de acordo com o data frame
#'passado e duas variáveis escolhidas desse mesmo data frame
#'
#' @param data data frame
#' @param x_var variável do data frame que ficará no eixo x
#' @param y_var variável do data frame que ficará no eixo y
#' @return um gráfico de barras
#' @importFrom dplyr group_by summarise
#' @importFrom highcharter hchart hcaes hc_add_theme hc_theme_darkunica
#' @export
densidade = function(data, x_var, y_var) {
  dados_teste = data |> group_by(.data[[x_var]]) |> summarise(vendas_total = sum(.data[[y_var]]))
  hchart(dados_teste, "bar", hcaes(x = .data[[x_var]], y = vendas_total)) |>
    hc_add_theme(hc_theme_darkunica())
}