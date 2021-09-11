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
    readr::read_tsv(
      col_names = c("excluir", "video", "tempo", "pressao"),
      col_types = "cccc"
    ) %>%
    dplyr::select(-excluir) %>%
    dplyr::mutate(
      arq = arq,
      video = as.logical(as.numeric(stringr::str_replace(video, ",", "."))),
      tempo = as.numeric(stringr::str_replace(tempo, ",", ".")),
      pressao = as.numeric(stringr::str_replace(pressao, ",", ".")),
    )
}

da <- purrr::map_dfr(da_brutos$arq, ler_arquivos_brutos)
da <- dplyr::left_join(da_brutos, da, "arq")

dplyr::glimpse(da)

readr::write_rds(da, "data-raw/da_tidy.rds", compress = "xz")


# Arquivos novos ----------------------------------------------------------

# arquivos <- fs::dir_ls(
#   "data-raw/", type = "file", recurse = TRUE,
#   regexp = "(R|sav)$", invert = TRUE
# )
# arquivos_novos <- fs::dir_ls(
#   "Dados Brutos/", type = "file", recurse = TRUE,
#   regexp = "(R|sav)$", invert = TRUE
# )
# arquivos <- arquivos %>% stringr::str_remove_all("data-raw/brutos")
# arquivos_novos <- arquivos_novos %>% stringr::str_remove_all("Dados Brutos")
# length(arquivos_novos)
# length(arquivos)
# setdiff(arquivos_novos, arquivos)
# setdiff(arquivos, arquivos_novos)
# arquivos_novos %>%
#   purrr::keep(~stringr::str_detect(.x, "mamando"))




