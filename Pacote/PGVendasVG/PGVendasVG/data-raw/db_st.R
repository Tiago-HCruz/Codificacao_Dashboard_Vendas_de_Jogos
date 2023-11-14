library(tidyverse)
db_st <- Dados |> select(Year_of_Release, Genre, Platform,
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
   rename("Ano" = "Year_of_Release",
          "Gênero" = "Genre",
          "Plataforma" = "Platform")

# usethis::use_data_raw("db_st")
usethis::use_data(db_st, overwrite = TRUE)
