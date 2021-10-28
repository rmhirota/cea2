library(magrittr)
library(patchwork)

nomes_b1 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b1", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b2 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b2", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b3 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b3", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()

# Fixamos o dia 1 para análise

graf_exp <- function(da, bebe, d, gr){
  da_bebe <- da %>%
    dplyr::filter(
      !is.null(diff_video), dia == d,
      grupo == gr, nome == bebe
    )
  tempo <- da_bebe %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::select(id,diff_video)
  da_bebe %>%
    ggplot2::ggplot(ggplot2::aes(x = diff_video, y=..density..)) +
    ggplot2::geom_histogram()+
    ggplot2::geom_density(ggplot2::aes(x=diff_video,y=..density..))
}

# Teste da exponencialização da variavel tempo entre videos

# Dia 1

b1_d1 <- purrr::map(nomes_b1, ~graf_exp(da_diff_tempo, .x, 1, "b1"))
b2_d1 <- purrr::map(nomes_b2, ~graf_exp(da_diff_tempo, .x, 1, "b2"))
b3_d1 <- purrr::map(nomes_b3, ~graf_exp(da_diff_tempo, .x, 1, "b3"))

grupo1 <- purrr::reduce(b1_d1, `/`)
grupo2 <- purrr::reduce(b2_d1, `/`)
grupo3 <- purrr::reduce(b3_d1, `/`)

# Dia 2

b1_d2 <- purrr::map(nomes_b1, ~graf_exp(cea2::da_tidy, .x, 2, "b1"))
b2_d2 <- purrr::map(nomes_b2, ~graf_exp(cea2::da_tidy, .x, 2, "b2"))
b3_d2 <- purrr::map(nomes_b3, ~graf_exp(cea2::da_tidy, .x, 2, "b3"))

grupo1_2 <- purrr::reduce(b1_d2, `/`)
grupo2_2 <- purrr::reduce(b2_d2, `/`)
grupo3_2 <- purrr::reduce(b3_d2, `/`)

# Dia 3

b1_d3 <- purrr::map(nomes_b1, ~graf_exp(cea2::da_tidy, .x, 3, "b1"))
b2_d3 <- purrr::map(nomes_b2, ~graf_exp(cea2::da_tidy, .x, 3, "b2"))
b3_d3 <- purrr::map(nomes_b3, ~graf_exp(cea2::da_tidy, .x, 3, "b3"))

grupo1_3 <- purrr::reduce(b1_d3, `/`)
grupo2_3 <- purrr::reduce(b2_d3, `/`)
grupo3_3 <- purrr::reduce(b3_d3, `/`)

