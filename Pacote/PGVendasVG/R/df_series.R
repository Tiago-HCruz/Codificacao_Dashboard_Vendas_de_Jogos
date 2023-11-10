#' @import tidyverse
#' @export
#'
#' @description
#' Uma função que retorna um conjunto de dados da série sobre as vendas de jogos
#' acumalado para cada localidade por Ano. Pode-se restringir a Plataforma e o
#' Gênero de preferencia do úsuario.
#'
#' @title Vendas de jogos por ano e coninentes.
#'
#' @param db: Banco de dados, funcional para `Dados`.
#' @param Gen: Gênero, se prefirir todos, o argumento deve ser "Todos".
#' @param Plat: Plataforma, se prefirir todos, o argumento deve ser "Todos".
#' @param Data_inicio: Data inicial do intervalo da serie do banco de dados.
#' @param Data_final:  Data final do intervalo da serie do banco de dados.
#'
#' @examples
#' # Banco de dados `Dados` do pacote recomendado para utilizar nessa função.
#' # Segue abaixo, alguns valores dos demais parametros de entradas:
#' df_series(Dados, "Todos", "Todos", 2000, 2010)


df_series <- function(db, Gen, Plat, Data_inicio, Data_final) {
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
                                                   "Global"))))) |>
  #filter(Year_of_Release %in% NA)
  rename("Ano" = "Year_of_Release",
         "Gênero" = "Genre",
         "Plataforma" = "Platform") %>%
  {if (`Gen` %in% "Todos") select(.,-`Gênero`) else filter(.,`Gênero` %in% `Gen`)} %>%
  {if (`Plat` %in% "Todos") select(.,-`Plataforma`) else filter(.,`Plataforma` %in% `Plat`)} %>%
  filter(Ano >= Data_inicio & Ano <= Data_final) |>
  group_by(Ano,  Continentes) |>
  summarise(Vendas = sum(Vendas))}
