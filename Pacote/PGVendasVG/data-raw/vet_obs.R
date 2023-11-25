## code to prepare `vet_obs` dataset goes here
library(dplyr)

vet_obs <- list(gen = (Dados |>
              select(Genre) |>
              distinct() |>
              na.omit() %>%
              .$Genre),
     plat = (Dados |>
               select(Platform) |>
               distinct() |>
               na.omit() %>%
               .$Platform),
     Vendas = (Dados |>
                 names() |>
                 stringr::str_subset("Sales")),
     Vendas_br = c("América do Norte", "União Europeia",
                   "Japão", "Outros", "Global"),
     Categoria = c("Platform",
                   "Genre")
     )

# usethis::use_data_raw("vet_obs")
usethis::use_data(vet_obs, overwrite = TRUE)
