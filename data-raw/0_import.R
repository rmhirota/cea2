library(magrittr)

ler_arquivos_brutos <- function(arq) {
  nome <- arq %>%
    basename() %>%
    stringr::str_extract("[:alpha:]+") %>%
    tolower()
  dia <- arq %>%
    basename() %>%
    stringr::str_extract("[0-9]")
  grupo <- stringr::str_extract(arq, "B.")
  arq %>%
    readr::read_delim(
      delim = "\t",
      col_names = c("excluir", "video", "tempo", "pressao")
    ) %>%
    dplyr::select(-excluir) %>%
    dplyr::mutate(
      nome = nome,
      dia = dia,
      grupo = grupo
    )
}

arquivos <- fs::dir_ls(
  "data-raw/", type = "file", recurse = TRUE,
  regexp = "(res|R)$", invert = TRUE
)

purrr::map_df(arquivos, ler_arquivos_brutos)


