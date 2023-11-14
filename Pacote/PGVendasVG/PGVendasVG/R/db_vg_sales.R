#' @importFrom readr read_csv cols_only
#' @export
#'
#' @description
#' Este conjunto de dados é sobre as vendas de jogos ao longo
#' dos anos em diversos países/continentes, e demais detalhes.
#'
#' @title Dados de vendas sobre os jogos de video games
#'
#' @param Name: Nome do jogo
#' @param Platform: Plataforma do jogo
#' @param Year_of_Release: Ano de lançamento
#' @param Genre: Gênero do jogo
#' @param Publisher: Editora
#' @param NA_Sales: Vendas na América do Norte
#' @param EU_Sales: Vendas na União Europeia
#' @param JP_Sales: Vendas no Japão
#' @param Other_Sales: Vendas em outras localidades.
#' @param Global_Sales: Total de vendas entre os Países
#'
#' @format Um objeto classificado como data frame, contendo 16719 observações e 10 variáveis.
#'
#' @return Retorna um banco de dados com os nome dos jogos,
#' as plataformas, o ano de lançamento, gênero do jogos
#' as editoras de jogos e as Vendas nas localidades.
#'
#' @examples
#' # Vizualização geral dos dados
#' dplyr::glimpse(Dados)
#'

Dados <- read_csv(file.path(getwd(),
                            "Dados",
                            "Video_Games.csv"),
                  col_types = cols_only(
                    Name = "c",
                    Platform = "c",
                    Year_of_Release = "c",
                    Genre = "c",
                    Publisher = "c",
                    NA_Sales = "d",
                    EU_Sales = "d",
                    JP_Sales = "d",
                    Other_Sales = "d",
                    Global_Sales = "d"))

#save(Dados, file = file.path(getwd(),
#                             "Dados",
#                             "VG_vendas.RData"))


