library(magrittr)

# arrumar regex do dia pq não tem dia 1 em alguns casos
ler_arquivos_brutos <- function(arq) {
  nome <- arq %>%
    basename() %>%
    stringr::str_extract("[:alpha:]+") %>%
    tolower()
  dia <- arq %>%
    basename() %>%
    stringr::str_extract("[0-9]")
  grupo <- stringr::str_extract(arq, "B.")
  condicao <- arq %>%
    basename() %>%
    stringr::str_extract("(?<=[0-9]).*")
  arq %>%
    readr::read_delim(
      delim = "\t",
      col_names = c("excluir", "video", "tempo", "pressao")
    ) %>%
    dplyr::select(-excluir) %>%
    dplyr::mutate(
      nome = nome,
      dia = dia,
      grupo = grupo,
      condicao = condicao
    )
}

arquivos %>%
  stringr::str_detect(stringr::regex("usar", TRUE)) %>%
  sum()

arquivos <- fs::dir_ls(
  "data-raw/", type = "file", recurse = TRUE,
  regexp = "(res|R)$", invert = TRUE
)

"data-raw/brutos/B3Bruna_Fernanda/bruna1basa2" %>%
  ler_arquivos_brutos()

da <- purrr::map_df(arquivos, ler_arquivos_brutos)


# SPSS --------------------------------------------------------------------

spss1 <- foreign::read.spss("data-raw/spss/Completo 3 dias variaveis base.sav") %>%
  dplyr::as_tibble()
dplyr::glimpse(spss1)
spss2 <- foreign::read.spss("data-raw/spss/gatilho_janelada.sav") %>%
  dplyr::as_tibble()
dplyr::glimpse(spss2)
spss3 <- foreign::read.spss("data-raw/spss/Tempo Video.sav") %>%
  dplyr::as_tibble()
dplyr::glimpse(spss3)

# variáveis
# - idade
# - dia
# - condicao
# - n_pertos
# - freq_apertos
# - pressao_media
# - sd_pressao
# - max_pressao (pico maximo)
# - media_picos
# - sd_picos
# - duracao_picos
# - sd_duracao_picos






