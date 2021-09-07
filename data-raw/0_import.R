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
      nome = tolower(stringr::str_extract(basename, "[:alpha:]+")),
      dia = stringr::str_extract(basename, "[0-9](?=[\\s\\S])"),
      condicao = dplyr::case_when(
        stringr::str_detect(basename, "ba[ds]a.*2") ~ "basal2",
        stringr::str_detect(basename, "ba[ds]a.*1?") ~ "basal1",
        stringr::str_detect(basename, "con[tr]") ~ "contingente",
        stringr::str_detect(basename, "nc") ~ "não contingente",
        stringr::str_detect(basename, "c$") ~ "contingente"
      ),
      # correções manuais
      nome = dplyr::case_when(
        nome == "bernado" | nome == "bernando" ~ "bernardo",
        nome == "duardo" ~ "eduardo",
        stringr::str_detect(nome, "luizmat") ~ "luizmateus",
        stringr::str_detect(nome, "hen") ~ "henriqueluiz",
        nome == "m" ~ "meduarda",
        stringr::str_detect(nome, "gabri") ~ "gabriel",
        stringr::str_detect(nome, "mlaura") ~ "mlaura",
        stringr::str_detect(nome, "nic") ~ "nicolas",
        stringr::str_detect(nome, "^ph") ~ "ph",
        stringr::str_detect(nome, "rafaell") ~ "rafaella",
        stringr::str_detect(nome, "stel") ~ " stella",
        TRUE ~ nome
      ),
      condicao = stringr::str_squish(stringr::str_remove_all(condicao, "[-_]"))
    )
}

da_brutos <- tbl_arquivos_brutos("data-raw/brutos/")

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






