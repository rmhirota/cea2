library(magrittr)

arquivos <- fs::dir_ls(
  "data-raw/copia_dados_brutos/",
  # "dados brutos novo",
  type = "file", recurse = TRUE,
  regexp = "(res|R|sav|Icon_|lnk|xlsx)$", invert = TRUE
)

# Junta arquivos com mais de uma parte ------------------------------------

parte2 <- purrr::keep(arquivos, ~stringr::str_detect(.x, "parte"))
parte1 <- stringr::str_remove_all(parte2, "([23] ?parte|\\.txt)")
setdiff(parte1, arquivos) # confirma se arquivos da parte1 existem

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

merge_partes <- function(file1, file2) {
  parte1 <- ler_arquivos_brutos(file1)
  parte2 <- ler_arquivos_brutos(file2)
  parte2 %>%
    dplyr::mutate(tempo = tempo - min(tempo) + max(parte1$tempo)) %>%
    dplyr::bind_rows(parte1, .)
}
arquivos_ok <- purrr::discard(arquivos, ~.x %in% c(parte1, parte2))

da_arquivos_ok <- purrr::map_dfr(arquivos_ok, ler_arquivos_brutos)
da_arquivos_merged <- purrr::map2_dfr(parte1, parte2, merge_partes)
da_brutos <- dplyr::bind_rows(da_arquivos_ok, da_arquivos_merged)

# Adiciona variáveis a partir do nome do arquivo --------------------------

tbl_arquivos_brutos <- function(dir) {
  dir %>%
    fs::dir_ls(
      type = "file", recurse = TRUE,
      regexp = "(res|R|sav|Icon_|lnk|xlsx)$", invert = TRUE
    ) %>%
    purrr::discard(~stringr::str_detect(
      .x, stringr::regex("nao.*usar|cortar|mamando", TRUE)
    )) %>%
    dplyr::tibble(arq = .) %>%
    dplyr::mutate(
      basename = tolower(basename(arq)),
      grupo = stringr::str_extract(arq, "b[123]"),
      nome = tolower(basename(dirname(arq))),
      nome = stringr::str_remove(stringr::str_extract(nome, "(?<=b.).+"), "_.+"),
      dia = stringr::str_extract(basename, "[0-9](?=[\\s\\S])"),
      condicao = dplyr::case_when(
        stringr::str_detect(basename, "ba[ds]a.*2") ~ "basal2",
        stringr::str_detect(basename, "ba[ds]a.*1?") ~ "basal1",
        stringr::str_detect(basename, "con[tr]") ~ "contingente",
        stringr::str_detect(basename, "nc") ~ "não contingente",
        stringr::str_detect(basename, "c$") ~ "contingente",
        stringr::str_detect(basename, "(?<=[123]\\-?)c") ~ "contingente"
      ),
      condicao = stringr::str_squish(stringr::str_remove_all(condicao, "[-_]"))
    )
}

da_metadados <- tbl_arquivos_brutos(dir = "data-raw/copia_dados_brutos/")

# nome ok (total = 21; lembrar que tem 2 nicolas)
da_metadados %>%
  dplyr::count(nome) %>%
  print(n = 40)

# grupo ok
da_metadados %>%
  dplyr::count(grupo) %>%
  print(n = 40)

# condição
da_metadados %>%
  dplyr::count(condicao) %>%
  print(n = 40)

# dia ok
da_metadados %>%
  dplyr::count(dia)

da_tidy <- da_brutos %>%
  dplyr::inner_join(da_metadados, "arq") %>%
  dplyr::relocate(video, tempo, pressao, .after = condicao)


# readr::write_rds(da_tidy, "data-raw/da_tidy.rds", compress = "xz")
usethis::use_data(da_tidy, overwrite = TRUE, compress = "xz")
usethis::use_data(da_metadados, overwrite = TRUE)

