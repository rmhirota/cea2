library(magrittr)
library(patchwork)

# Tempo entre vídeos ------------------------------------------------------

da_diff_tempo <- cea2::da_tidy %>%
  dplyr::group_by(grupo, nome, dia, condicao) %>%
  dplyr::arrange(grupo, nome, dia, condicao, tempo) %>%
  dplyr::filter(status_video %in% c("inicio", "fim")) %>%
  dplyr::mutate(diff_video = dplyr::case_when(
    status_video == "inicio" & dplyr::lag(status_video) == "fim"
    ~ (tempo - dplyr::lag(tempo))
  )) %>%
  dplyr::ungroup()

nomes_b1 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b1", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b2 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b2", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b3 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b3", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()

# Fixamos o dia 1 para análise --------------------------------------------------------------

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

grupo1 <- purrr::reduce(b1_d1, `+`)
grupo2 <- purrr::reduce(b2_d1, `+`)
grupo3 <- purrr::reduce(b3_d1, `+`)

# Dia 2

b1_d2 <- purrr::map(nomes_b1, ~graf_exp(cea2::da_tidy, .x, 2, "b1"))
b2_d2 <- purrr::map(nomes_b2, ~graf_exp(cea2::da_tidy, .x, 2, "b2"))
b3_d2 <- purrr::map(nomes_b3, ~graf_exp(cea2::da_tidy, .x, 2, "b3"))

grupo1_2 <- purrr::reduce(b1_d2, `+`)
grupo2_2 <- purrr::reduce(b2_d2, `+`)
grupo3_2 <- purrr::reduce(b3_d2, `+`)

# Dia 3

b1_d3 <- purrr::map(nomes_b1, ~graf_exp(cea2::da_tidy, .x, 3, "b1"))
b2_d3 <- purrr::map(nomes_b2, ~graf_exp(cea2::da_tidy, .x, 3, "b2"))
b3_d3 <- purrr::map(nomes_b3, ~graf_exp(cea2::da_tidy, .x, 3, "b3"))

grupo1_3 <- purrr::reduce(b1_d3, `/`)
grupo2_3 <- purrr::reduce(b2_d3, `/`)
grupo3_3 <- purrr::reduce(b3_d3, `/`)

# Sem abertura por dia

graf_exp_all <- function(da, bebe, gr){
  da_bebe <- da %>%
    dplyr::filter(
      !is.null(diff_video),
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


g1 <- purrr::map(nomes_b1, ~graf_exp_all(da_diff_tempo, .x, "b1"))
g2 <- purrr::map(nomes_b2, ~graf_exp_all(da_diff_tempo, .x, "b2"))
g3 <- purrr::map(nomes_b3, ~graf_exp_all(da_diff_tempo, .x, "b3"))

grupo1 <- purrr::reduce(g1, `+`)
grupo2 <- purrr::reduce(g2, `+`)
grupo3 <- purrr::reduce(g3, `+`)

# Teste de KS da variavel tempo entre videos

require(vcd)
require(MASS)

# Grupo 1

baby_11 <- as.data.frame(da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[1]) %>%
  dplyr::select(diff_video))
fit1 <- fitdistr(baby_11$diff_video,"exponential")
ks.test(baby_11$diff_video,"pexp", fit1$estimate) # p-value < 0.05 -> distribution refused

baby_12 <- da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[2]) %>%
  dplyr::select(diff_video)
fit2 <- fitdistr(baby_12$diff_video,"exponential")
ks.test(baby_12$diff_video,"pexp", fit2$estimate) # p-value < 0.05 -> distribution refused
#  ties should not be present for the Kolmogorov-Smirnov test


