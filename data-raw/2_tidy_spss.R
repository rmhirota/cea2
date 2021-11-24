library(magrittr)

da_spss <- haven::read_sav("data-raw/spss/v3 completo 3 dias variaveis base ime.sav")

#da_spss %>% dplyr::filter(Nome == "Bernard") %>% dplyr::glimpse()

#dplyr::glimpse(da_spss)

da_spss_tidy <- da_spss %>%
  janitor::clean_names() %>%
  tidyr::pivot_longer(-c(nome, grupo)) %>%
  dplyr::mutate(
    dia = dplyr::case_when(
      stringr::str_detect(name, "x2") ~ 2,
      stringr::str_detect(name, "x3") ~ 3,
      TRUE ~ 1
    ),
    condicao = stringr::str_remove(stringr::str_extract(name, ".+?(?=_)"), "x."),
    name = stringr::str_remove_all(name, "(c|bas|nc|pos)_"),
    name = stringr::str_remove_all(name, "x[23]"),
    name = stringr::str_replace_all(name, "x", "media"),
    nome = stringr::str_squish(nome)
  ) %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  dplyr::mutate(
    grupo = as.factor(grupo),
    dia = as.factor(dia),
    condicao = as.factor(condicao)
  )

dplyr::glimpse(da_spss_tidy)
View(da_spss_tidy)

readr::write_rds(da_spss_tidy, "data-raw/v2_da_spss_tidy.rds")
usethis::use_data(da_spss, overwrite = TRUE)

# Verificação de inconsistências ------------------------------------------

da_spss_tidy %>%
  dplyr::mutate(inco = dplyr::case_when(
    media_pressao > pico ~ "media_pressao maior que pico",
    media_pressao_pico < media_pressao ~ "media_pressao_pico menor que media_pressao"
  )) %>%
  dplyr::filter(!is.na(inco))


saveRDS(da_spss_tidy, file = "data-raw/2_tidy_spss.rds")

da_spss <- readr::read_rds("data-raw/v2_da_spss_tidy.rds")
dplyr::glimpse(da_spss)
