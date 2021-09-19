library(magrittr)

# da_spss <- foreign::read.spss("data-raw/spss/Completo 3 dias variaveis base.sav") %>%
#   dplyr::as_tibble()

da_spss <- readxl::read_excel("data-raw/spss/Dados-Completos_IME.xlsx")
da_spss %>% dplyr::filter(Nome == "Bernard") %>% dplyr::glimpse()

dplyr::glimpse(da_spss)

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

da_spss_tidy %>%
  dplyr::filter(nome == "Bernard", dia == 3)



readr::write_rds(da_spss_tidy, "data-raw/da_spss_tidy.rds")
