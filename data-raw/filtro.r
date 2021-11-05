library(magrittr)
cea2::da_tidy %>% dplyr::glimpse()

teste <- cea2::da_tidy %>%
  dplyr::filter(nome == "eduarda", dia == "1", condicao == "contingente") %>%
  dplyr::mutate()

teste_filtrado <- teste$pressao[1:10] %>%
  mFilter::bwfilter(freq = 5)

teste_filtrado$trend[, 1]

# Filtro para toda a base ----

filtrar <- function(da, nome, gr, dia, cond) {
  pressao <- da %>%
    dplyr::filter(nome == nome, grupo == gr, dia == dia, condicao == cond) %>%
    dplyr::pull(pressao)
  pressao_filtrada <- mFilter::bwfilter(pressao, freq = 5) %>%
    dplyr::pull(trend)
  da %>%
    dplyr::mutate(pressao_filtrada = pressao_filtrada)
}

combinacoes <- cea2::da_tidy %>%
  dplyr::distinct(nome, grupo, dia, condicao)

for (i in 1:3) {
  filtrar(
    cea2::da_tidy, combinacoes$nome[i], combinacoes$grupo[i],
    combinacoes$dia[i], combinacoes$condicao[i]
  )
}

i <- 1
filtrar(
    cea2::da_tidy, "eduarda", "b1",
    "1", "basal1"
  )

pressao <- cea2::da_tidy %>%
    dplyr::filter(nome == "eduarda", grupo == "b1", dia == "1", condicao == "basal1") %>%
    dplyr::pull(pressao)
