## code to prepare `vet_obs` dataset goes here

list(gen = (Dados |>
              select(Genre) |>
              distinct() |>
              na.omit() %>%
              .$Genre),
     plat = (Dados |>
               select(Platform) |>
               distinct() |>
               na.omit() %>%
               .$Platform))

# usethis::use_data_raw("vet_obs")
usethis::use_data(vet_obs, overwrite = TRUE)
