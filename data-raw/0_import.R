library(magrittr)

arquivos <- fs::dir_ls(
  "data-raw/", type = "file", recurse = TRUE,
  regexp = "(res|R|sav|Icon_|lnk)$", invert = TRUE
)

tbl_arquivos_brutos <- function(dir) {
  dir %>%
    fs::dir_ls(
      type = "file", recurse = TRUE,
      regexp = "(res|R|sav|Icon_|lnk)$", invert = TRUE
    ) %>%
    purrr::discard(~stringr::str_detect(
      .x, stringr::regex("nao.*usar|cortar|mamando", TRUE)
    )) %>%
    dplyr::tibble(arq = .) %>%
    dplyr::mutate(
      basename = tolower(basename(arq)),
      grupo = stringr::str_extract(arq, "B."),
      nome = tolower(basename(dirname(arq))),
      nome = stringr::str_remove(stringr::str_extract(nome, "(?<=b.).+"), "_.+"),
      dia = stringr::str_extract(basename, "[0-9](?=[\\s\\S])"),
      condicao = dplyr::case_when(
        stringr::str_detect(basename, "ba[ds]a.*2") ~ "basal2",
        stringr::str_detect(basename, "ba[ds]a.*1?") ~ "basal1",
        stringr::str_detect(basename, "con[tr]") ~ "contingente",
        stringr::str_detect(basename, "nc") ~ "não contingente",
        stringr::str_detect(basename, "c$") ~ "contingente"
      ),
      condicao = stringr::str_squish(stringr::str_remove_all(condicao, "[-_]"))
    )
}

da_brutos <- tbl_arquivos_brutos(dir = "data-raw/brutos/")

# nomes ok
da_brutos %>%
  dplyr::count(nome) %>%
  print(n = 40)

# condição
da_brutos %>%
  dplyr::count(condicao) %>%
  print(n = 40)
da_brutos %>%
  dplyr::filter(is.na(condicao))

# dia
da_brutos %>%
  dplyr::count(dia)
da_brutos %>%
  dplyr::filter(is.na(dia))


ler_arquivos_brutos <- function(arq) {
  arq %>%
    readr::read_delim(
      delim = "\t",
      col_names = c("excluir", "video", "tempo", "pressao")
    ) %>%
    dplyr::select(-excluir) %>%
    dplyr::mutate(arq = arq)
}

da <- purrr::map_dfr(da_brutos$arq, ler_arquivos_brutos)
da <- da_brutos %>%
  dplyr::left_join(da)


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






