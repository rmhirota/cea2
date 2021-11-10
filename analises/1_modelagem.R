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



##Teste de Friedman entre dias do mesmo bebê
# Grupo 1

baby_11 <- as.data.frame(da_diff_tempo %>%
                           dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[1], dia == 1) %>%
                           dplyr::select(diff_video))


baby_12 <- da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[1], dia == 2) %>%
  dplyr::select(diff_video)

baby_13 <- da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[1], dia == 3) %>%
  dplyr::select(diff_video)

df1 = matrix(c(baby_11$diff_video[1:18],baby_12$diff_video[1:18], baby_13$diff_video[1:18]), 18, 3)

friedman.test(df1)
#Friedman rank sum test

#data:  df1
#Friedman chi-squared = 1.4444, df = 2, p-value = 0.4857


baby_21 <- as.data.frame(da_diff_tempo %>%
                           dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[2], dia == 1) %>%
                           dplyr::select(diff_video))


baby_22 <- da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[2], dia == 2) %>%
  dplyr::select(diff_video)

baby_23 <- da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[2], dia == 3) %>%
  dplyr::select(diff_video)

df2 = matrix(c(baby_21$diff_video[1:18],baby_22$diff_video[1:26], baby_23$diff_video[1:26]), 26, 3)

friedman.test(df2)
#Friedman rank sum test

#data:  df2
#Friedman chi-squared = 13, df = 2, p-value = 0.001503

baby_31 <- as.data.frame(da_diff_tempo %>%
                           dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[3], dia == 1) %>%
                           dplyr::select(diff_video))


baby_32 <- da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[3], dia == 2) %>%
  dplyr::select(diff_video)

baby_33 <- da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[3], dia == 3) %>%
  dplyr::select(diff_video)

df3 = matrix(c(baby_31$diff_video[1:18],baby_32$diff_video[1:18], baby_33$diff_video[1:18]), 18, 3)

friedman.test(df3)
#Friedman chi-squared = NaN, df = 2, p-value = NA


baby_41 <- as.data.frame(da_diff_tempo %>%
                           dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[4], dia == 1) %>%
                           dplyr::select(diff_video))


baby_42 <- da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[4], dia == 2) %>%
  dplyr::select(diff_video)

baby_43 <- da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[4], dia == 3) %>%
  dplyr::select(diff_video)

df4 = matrix(c(baby_41$diff_video[1:63],baby_42$diff_video[1:63], baby_43$diff_video[1:63]), 63, 3)

friedman.test(df4)
#Friedman rank sum test

#data:  df4
#Friedman chi-squared = 17.568, df = 2, p-value = 0.0001532



baby_51 <- as.data.frame(da_diff_tempo %>%
                           dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[5], dia == 1) %>%
                           dplyr::select(diff_video))


baby_52 <- da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[5], dia == 2) %>%
  dplyr::select(diff_video)

baby_53 <- da_diff_tempo %>%
  dplyr::filter(!is.na(diff_video),condicao == 'contingente',nome==nomes_b1[5], dia == 3) %>%
  dplyr::select(diff_video)

df5 = matrix(c(baby_51$diff_video[1:5],baby_52$diff_video[1:5], baby_53$diff_video[1:5]), 5, 3)

friedman.test(df5)

#Friedman rank sum test

#data:  df5
#Friedman chi-squared = 2.8, df = 2, p-value = 0.2466


