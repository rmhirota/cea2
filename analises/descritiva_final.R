# Pacotes ----------------------------------------------------------------------------

library(magrittr)
library(patchwork)
library(htmlwidgets)
library(webshot)
library(readr)

# Leitura das bases -------------------------------------------------------------------

da_tidy <- read.csv("DADOS/BASES INTERMEDIÁRIAS/da_tidy") # inserir o caminho do arquivo csv da_tidy (base bruta)
da_spss <- readr::read_rds("DADOS/BASES INTERMEDIÁRIAS/2_tidy_spss.rds") # inserir o caminho do arquivo rds (base spss)

# Análise descritiva

# Perfil -------------------------------------------------------------------------------

cea2::da_tidy %>%
    dplyr::distinct(grupo, nome) %>%
    dplyr::count(grupo) %>%
    dplyr::mutate(grupo = dplyr::case_when(
      grupo == "b1" ~ "1 mês",
      grupo == "b2" ~ "2 meses",
      grupo == "b3" ~ "3 meses"
    )) %>%
    dplyr::mutate(prop = formattable::percent(n/sum(n))) %>%
    janitor::adorn_totals() %>%
    knitr::kable(col.names = c("Grupo", "N", "%")) %>%
    kableExtra::kable_styling(full_width = TRUE)

# Pressão Média-------------------------------------------------------------------------------

media_grupo <- function(da, variavel, ...) {
  da %>%  dplyr::mutate(grupo = dplyr::case_when(grupo == 'b1' ~ 1,
                                                 grupo == 'b2' ~ 2,
                                                 grupo == 'b3' ~ 3)) %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      "media_{{variavel}}" := mean({{variavel}}),
      "dp_{{variavel}}" := sd({{variavel}}),
      "mediana_{{variavel}}" := median({{variavel}}),
      .groups = "drop"
    )
}

## Por Dia
media_grupo(cea2::da_tidy, pressao, dia) %>%
  dplyr::mutate(dplyr::across(2:4, ~round(.x, 3))) %>%
  knitr::kable(col.names = c("Dia", "Pressão Média", "Desvio Padrão", "Pressão Mediana")) %>%
  kableExtra::kable_styling(full_width = TRUE)

## Por Grupo
media_grupo(cea2::da_tidy, pressao, grupo)  %>%
  dplyr::mutate(dplyr::across(2:4, ~round(.x, 3))) %>%
  knitr::kable(col.names = c("Grupo", "Pressão Média", "Desvio Padrão", "Pressão Mediana")) %>%
  kableExtra::kable_styling(full_width = TRUE)

## Por Condição
media_grupo(cea2::da_tidy, pressao, condicao)  %>%
  dplyr::mutate(dplyr::across(2:4, ~round(.x, 3))) %>%
  knitr::kable(col.names = c("Condição", "Pressão Média", "Desvio Padrão", "Pressão Mediana")) %>%
  kableExtra::kable_styling(full_width = TRUE)

# Pressão Média ------------------------------------------------------------------------

cea2::da_tidy %>%
  dplyr::mutate(condicao = dplyr::case_when(
    condicao == "basal1" ~ "Basal 1",
    condicao == "contingente" ~ "Contingente",
    condicao == "não contingente" ~ "Não contingente",
    condicao == "basal2" ~ "Basal 2"),
    grupo = dplyr::case_when(
      grupo == 'b1' ~ '1',
      grupo == 'b2' ~ '2',
      grupo == 'b3' ~ '3',
    )) %>%
  dplyr::group_by(grupo, condicao, dia) %>%
  dplyr::summarise(media = mean(pressao, na.rm = TRUE)) %>%
  ggplot2::ggplot(ggplot2::aes(x = dia, y = media, fill = grupo)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::facet_wrap(~condicao)+
  ggplot2::labs(
    x = "Dia",
    y = "Pressão Média"
  ) +
  ggplot2::theme_minimal(14) +
  ggplot2::scale_fill_manual(values = c("#440154FF", "#21908CFF", "#FDE725FF"))

# Pressão Média ao longo do tempo ------------------------------------------------------
## qual o código da função hc_grupo?

g1 = cea2::hc_grupo("b1") %>%
  highcharter::hc_title(text = "Pressão média por segundo - Grupo 1")

htmlwidgets::saveWidget(widget=g1,file="analises/bas_c_interativo_g1.html")
webshot::webshot(url="analises/bas_c_interativo_g1.html",file="analises/grafico.png",delay=20)

g2 = cea2::hc_grupo("b2") %>%
  highcharter::hc_title(text = "Pressão média por segundo - Grupo 2")

htmlwidgets::saveWidget(widget=g2,file="analises/bas_c_interativo_g2.html")
webshot::webshot(url="analises/bas_c_interativo_g2.html",file="analises/grafico2.png",delay=20)

g3 = cea2::hc_grupo("b3") %>%
  highcharter::hc_title(text = "Pressão média por segundo - Grupo 3")

htmlwidgets::saveWidget(widget=g3,file="analises/bas_c_interativo_g3.html") # salva os graficos no caminho passado
webshot::webshot(url="analises/bas_c_interativo_g3.html",file="analises/grafico3.png",delay=20)


# Perfil dos participantes -----------------------------------------------------------

da <- cea2::da_tidy %>% dplyr::filter(condicao=='contingente')  #filtra contingente

nomes_b1 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b1", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b2 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b2", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b3 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b3", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()

b1_d1 <- purrr::map(nomes_b1, ~cea2::p_video_bebe(da, .x, 1, "b1"))
b1_d2 <- purrr::map(nomes_b1, ~cea2::p_video_bebe(da, .x, 2, "b1"))
b1_d3 <- purrr::map(nomes_b1, ~cea2::p_video_bebe(da, .x, 3, "b1"))
b2_d1 <- purrr::map(nomes_b2, ~cea2::p_video_bebe(da, .x, 1, "b2"))
b2_d2 <- purrr::map(nomes_b2, ~cea2::p_video_bebe(da, .x, 2, "b2"))
b2_d3 <- purrr::map(nomes_b2, ~cea2::p_video_bebe(da, .x, 3, "b2"))
b3_d1 <- purrr::map(nomes_b3, ~cea2::p_video_bebe(da, .x, 1, "b3"))
b3_d2 <- purrr::map(nomes_b3, ~cea2::p_video_bebe(da, .x, 2, "b3"))
b3_d3 <- purrr::map(nomes_b3, ~cea2::p_video_bebe(da, .x, 3, "b3"))

## Grupo 1
dia1 <- purrr::reduce(b1_d1, `/`)
dia2 <- purrr::reduce(b1_d2, `/`)
dia3 <- purrr::reduce(b1_d3, `/`)
grupo1 <- dia1 | dia2 | dia3
ggplot2::ggsave("analises/perfis_grupo1.png", grupo1,width = 7, height = 7) # salva imagem
ggplot2::ggsave("pres/plots/perfis_grupo1.png", grupo1,width = 7, height = 7)

## Grupo 2
dia1 <- purrr::reduce(b2_d1, `/`)
dia2 <- purrr::reduce(b2_d2, `/`)
dia3 <- purrr::reduce(b2_d3, `/`)
grupo2 <- dia1 | dia2 | dia3
ggplot2::ggsave("analises/perfis_grupo2.png", grupo2,width = 7, height = 7)
ggplot2::ggsave("pres/plots/perfis_grupo2.png", grupo2,width = 7, height = 7)

## Grupo 3
dia1 <- purrr::reduce(b3_d1, `/`)
dia2 <- purrr::reduce(b3_d2, `/`)
dia3 <- purrr::reduce(b3_d3, `/`)
grupo3 <- dia1 | dia2 | dia3
ggplot2::ggsave("analises/perfis_grupo3.png", grupo3,width = 7, height = 7)
ggplot2::ggsave("pres/plots/perfis_grupo3.png", grupo3,width = 7, height = 7)


# Frequencia de apertos boxplot -------------------------------------------------------

da_spss <- readr::read_rds("../data-raw/2_tidy_spss.rds")
boxplot_freq <- function(cond) {
  label_cond <- switch(
    cond,
    bas = "Basal 1", pos = "Basal 2",
    c = "Contingente", nc = "Não contingente"
  )
  da_spss %>%
    dplyr::filter(condicao == cond) %>%
    ggplot2::ggplot(ggplot2::aes(x = grupo, y = freq_apertos, fill = dia)) +
    ggplot2::geom_boxplot(colour = "grey", outlier.colour = "red") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "Grupo", y = glue::glue("Freq - {label_cond}")) +
    ggplot2::theme_minimal(18) +
    ggplot2::facet_wrap(~dia)
}

basal1 <- boxplot_freq("bas")
contingente <- boxplot_freq("c")
naocont <- boxplot_freq("nc")
basal2 <- boxplot_freq("pos")
bass = basal1+basal2
conting = contingente+naocont

# Frequencia de apertos (Basal 2) ----------------------------------------------------
da_spss <- readr::read_rds("data-raw/2_tidy_spss.rds") #base agrupada

da_apertos <- da_spss %>%
  dplyr::filter(condicao == "pos") %>%
  dplyr::transmute(
    nome, grupo = as.character(grupo),
    dia = stringr::str_c("dia", dia), freq_apertos
  ) %>%
  tidyr::pivot_wider(names_from = dia, values_from = freq_apertos)

d12 = da_apertos %>%
  ggplot2::ggplot(ggplot2::aes(x = dia1, y = dia2, colour = grupo)) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Frequência de apertos no dia 1", y = "Frequência de apertos no dia 2") +
  ggplot2::scale_colour_viridis_d() +
  ggplot2::geom_abline(slope = 1, intercept = 0, alpha = .5) +
  ggplot2::geom_hline(
    linetype = "dotted", alpha = .3,
    yintercept = median(da_apertos$dia2, na.rm = TRUE)
  ) +
  ggplot2::geom_vline(
    linetype = "dotted", alpha = .3,
    xintercept = median(da_apertos$dia1, na.rm = TRUE)
  )

d23 = da_apertos %>%
  ggplot2::ggplot(ggplot2::aes(x = dia2, y = dia3, colour = grupo)) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Frequência de apertos no dia 2", y = "Frequência de apertos no dia 3") +
  ggplot2::scale_colour_viridis_d() +
  ggplot2::geom_abline(slope = 1, intercept = 0, alpha = .5) +
  ggplot2::geom_hline(
    linetype = "dotted", alpha = .3,
    yintercept = median(da_apertos$dia2, na.rm = TRUE)
  ) +
  ggplot2::geom_vline(
    linetype = "dotted", alpha = .3,
    xintercept = median(da_apertos$dia1, na.rm = TRUE)
  )

d13 = da_apertos %>%
  ggplot2::ggplot(ggplot2::aes(x = dia1, y = dia3, colour = grupo)) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Frequência de apertos no dia 1", y = "Frequência de apertos no dia 3") +
  ggplot2::scale_colour_viridis_d() +
  ggplot2::geom_abline(slope = 1, intercept = 0, alpha = .5) +
  ggplot2::geom_hline(
    linetype = "dotted", alpha = .3,
    yintercept = median(da_apertos$dia2, na.rm = TRUE)
  ) +
  ggplot2::geom_vline(
    linetype = "dotted", alpha = .3,
    xintercept = median(da_apertos$dia1, na.rm = TRUE)
  )

d12+d23+d13+plot_layout(ncol=1,)

# Frequencia de apertos (Basal 1 x Basal 2) ----------------------------------------------------

da_dia <- da_spss %>%
  dplyr::filter(condicao %in% c("bas", "pos")) %>%
  dplyr::mutate(condicao = ifelse(condicao == "pos", "Basal 2", "Basal 1")) %>%
  dplyr::select(nome,condicao, grupo, dia, freq_apertos) %>%
  dplyr::mutate(condicao = dplyr::case_when(
    condicao == "Basal 1" ~ "Basal_1",
    TRUE ~ "Basal_2"
  )) %>%
  tidyr::pivot_wider(names_from = condicao, values_from = freq_apertos)

#Dia 1
d1 = da_dia %>%
  dplyr::filter(dia == 1)%>%
  ggplot2::ggplot(ggplot2::aes(x = Basal_1, y = Basal_2, colour = grupo)) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::geom_abline(slope = 1, intercept = 0)+
  ggplot2::labs(x = "Frequência de apertos na Basal 1", y = "Frequência de apertos na Basal 2") +
  ggplot2::scale_colour_viridis_d()

#Dia 2
d2 = da_dia %>%
  dplyr::filter(dia == 2)%>%
  ggplot2::ggplot(ggplot2::aes(x = Basal_1, y = Basal_2, colour = grupo)) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::geom_abline(slope = 1, intercept = 0)+
  ggplot2::labs(x = "Frequência de apertos na Basal 1", y = "Frequência de apertos na Basal 2") +
  ggplot2::scale_colour_viridis_d()

#Dia 3
d3 = da_dia %>%
  dplyr::filter(dia == 3)%>%
  ggplot2::ggplot(ggplot2::aes(x = Basal_1, y = Basal_2, colour = grupo)) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::geom_abline(slope = 1, intercept = 0)+
  ggplot2::labs(x = "Frequência de apertos na Basal 1", y = "Frequência de apertos na Basal 2") +
  ggplot2::scale_colour_viridis_d()

d1+d2+d3+plot_layout(ncol=1)

# Tempo entre vídeos -------------------------------------------------------------------------

da_diff_tempo <- cea2::da_tidy %>%
  dplyr::group_by(grupo, nome, dia, condicao) %>%
  dplyr::arrange(grupo, nome, dia, condicao, tempo) %>%
  dplyr::filter(status_video %in% c("inicio", "fim")) %>%
  dplyr::mutate(diff_video = dplyr::case_when(
    status_video == "inicio" & dplyr::lag(status_video) == "fim"
    ~ (tempo - dplyr::lag(tempo))
  )) %>%
  dplyr::ungroup()

tempo_video <- function(da, gr,c) {
  da %>%
    dplyr::filter(grupo == gr,condicao==c) %>%
    dplyr::mutate(
      nome = as.factor(nome),
      label = stringr::str_c("bebê ", as.numeric(nome))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = dia, y = diff_video, fill = dia)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_viridis_d(alpha = .4) +
    ggplot2::facet_wrap(~label) +
    ggplot2::theme_bw(14) +
    ggplot2::theme(
      legend.position = "none"
    ) +
    ggplot2::labs(
      y = "Tempo entre vídeos (log10)",
      x = "Dia"
    ) + ggplot2::scale_y_continuous(trans='log10')
}

# Boxplot Tempo entre vídeos ---------------------------------------------------------

## Grupo 1
tempo_grupo1 <- tempo_video(da_diff_tempo, "b1","contingente")
ggplot2::ggsave("analises/tempo_video_g1.jpeg", tempo_grupo1)
ggplot2::ggsave("pres/plots/tempo_video_g1.jpeg", tempo_grupo1)

## Grupo 2
tempo_grupo2 <- tempo_video(da_diff_tempo, "b2","contingente")
ggplot2::ggsave("analises/tempo_video_g2.jpeg", tempo_grupo2)
ggplot2::ggsave("pres/plots/tempo_video_g2.jpeg", tempo_grupo2)

## Grupo 3
tempo_grupo3 <- tempo_video(da_diff_tempo, "b3","contingente")
ggplot2::ggsave("analises/tempo_video_g3.jpeg", tempo_grupo3)
ggplot2::ggsave("pres/plots/tempo_video_g3.jpeg", tempo_grupo3)

# Análise Inferencial

# Kruskal-Wallis ----------------------------------------------------------

da_spss <- readr::read_rds("data-raw/2_tidy_spss.rds")

teste_kw <- function(cond, d) {
  da <- da_spss %>%
    dplyr::filter(condicao == cond, dia == d) %>%
    dplyr::select(grupo, freq_apertos)
  g1 <- dplyr::filter(da, grupo == 1)
  g2 <- dplyr::filter(da, grupo == 2)
  g3 <- dplyr::filter(da, grupo == 3)
  kruskal.test(list(
    g1$freq_apertos, g2$freq_apertos, g3$freq_apertos
  ))
}

# Contingente
d1 <- teste_kw("c", 1)
d2 <- teste_kw("c", 2)
d3 <- teste_kw("c", 3)

dplyr::bind_cols(
  Dia = c("Dia 1", "Dia 2","Dia 3"),
  purrr::map_df(list(d1,d2,d3), broom::tidy)
) %>%
  dplyr::select(-parameter, -method) %>%
  knitr::kable()

# Não Contingente

n1 <- teste_kw("nc", 1)
n2 <- teste_kw("nc", 2)
n3 <- teste_kw("nc", 3)

dplyr::bind_cols(
  Dia = c("Dia 1", "Dia 2", "Dia 3"),
  purrr::map_df(list(n1, n2, n3), broom::tidy)
) %>%
  dplyr::select(-parameter, -method) %>%
  knitr::kable()

# Basal 1
b1 <- teste_kw("bas", 1)
b2 <- teste_kw("bas", 2)
b3 <- teste_kw("bas", 3)

dplyr::bind_cols(
  Dia = c("Dia 1", "Dia 2", "Dia 3"),
  purrr::map_df(list(b1, b2, b3), broom::tidy)
) %>%
  dplyr::select(-parameter, -method) %>%
  knitr::kable()

# Basal 2
p1 <- teste_kw("pos", 1)
p2 <- teste_kw("pos", 2)
p3 <- teste_kw("pos", 3)

dplyr::bind_cols(
  Dia = c("Dia 1", "Dia 2", "Dia 3"),
  purrr::map_df(list(p1, p2, p3), broom::tidy)
) %>%
  dplyr::select(-parameter, -method) %>%
  knitr::kable()

# Wilcoxon --------------------------------------------------------------------------

# Wilcoxon: Comparação entre dias (basal 2 - basal 1)
da_spss <- readr::read_rds("data-raw/2_tidy_spss.rds")

da_wilcoxon <- da_spss %>%
  dplyr::filter(condicao %in% c("pos")) %>%
  dplyr::mutate(id = paste0(nome, grupo)) %>%
  dplyr::select(id, dia, condicao, freq_apertos) %>%
  tidyr::pivot_wider(names_from = condicao, values_from = freq_apertos)

teste_w2 <- function(dia1, dia2, alt = "greater") {
  d1 <- da_wilcoxon %>%
    dplyr::filter(dia == dia1) %>%
    dplyr::pull(pos)
  d2 <- da_wilcoxon %>%
    dplyr::filter(dia == dia2) %>%
    dplyr::pull(pos)
  wilcox.test(d1, d2, paired = TRUE, alternative = alt)
}

w1 <- teste_w2(1, 2)  # diferença entre dias 1 e 2
w2 <- teste_w2(1, 3)  # diferença entre dias 1 e 3
w3 <- teste_w2(2, 3)  # diferença entre dias 2 e 3

dplyr::bind_cols(
  Dia = c("Dia 1 x Dia 2", "Dia 1 x Dia 3", "Dia 2 x Dia 3"),
  purrr::map_df(list(w1, w2, w3), broom::tidy)
) %>%
  knitr::kable()

# Teste da exponencialização da variavel tempo até aperto ---------------------------

da_diff_aperto <- cea2::da_tidy %>%
  dplyr::group_by(grupo, nome, dia, condicao) %>%
  dplyr::arrange(grupo, nome, dia, condicao, tempo) %>%
  dplyr::filter(status_aperto %in% c("inicio", "fim")) %>%
  dplyr::mutate(diff_aperto = dplyr::case_when(
    status_aperto == "inicio" & dplyr::lag(status_aperto) == "fim"
    ~ (tempo - dplyr::lag(tempo))
  )) %>%
  dplyr::ungroup()

nomes_b1 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b1") %>%
  dplyr::pull(nome) %>% unique()
nomes_b2 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b2") %>%
  dplyr::pull(nome) %>% unique()
nomes_b3 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b3") %>%
  dplyr::pull(nome) %>% unique()


graf_exp <- function(da, d, gr,c){
  da_bebe <- da %>%
    dplyr::filter(
      !is.null(diff_aperto), dia == d,
      grupo == gr,  condicao == c
    )
  da_bebe %>%
    ggplot2::ggplot(ggplot2::aes(x = diff_aperto, y=..density..)) +
    ggplot2::geom_histogram()
}

# Fixamos o dia 1 para análise
# Grupo 1

graf_exp(da_diff_aperto, 1, "b1","basal1") +
  graf_exp(da_diff_aperto, 1, "b1","basal2") +
  graf_exp(da_diff_aperto, 1, "b1","contingente") +
  graf_exp(da_diff_aperto, 1, "b1","não contingente")

graf_exp(da_diff_aperto, 2, "b1","basal1") +
  graf_exp(da_diff_aperto, 2, "b1","basal2") +
  graf_exp(da_diff_aperto, 2, "b1","contingente") +
  graf_exp(da_diff_aperto, 2, "b1","não contingente")

graf_exp(da_diff_aperto, 3, "b1","basal1") +
  graf_exp(da_diff_aperto, 3, "b1","basal2") +
  graf_exp(da_diff_aperto, 3, "b1","contingente") +
  graf_exp(da_diff_aperto, 3, "b1","não contingente")

# Grupo 2

graf_exp(da_diff_aperto, 1, "b2","basal1") +
  graf_exp(da_diff_aperto, 1, "b2","basal2") +
  graf_exp(da_diff_aperto, 1, "b2","contingente") +
  graf_exp(da_diff_aperto, 1, "b2","não contingente")

graf_exp(da_diff_aperto, 2, "b2","basal1") +
  graf_exp(da_diff_aperto, 2, "b2","basal2") +
  graf_exp(da_diff_aperto, 2, "b2","contingente") +
  graf_exp(da_diff_aperto, 2, "b2","não contingente")

graf_exp(da_diff_aperto, 3, "b2","basal1") +
  graf_exp(da_diff_aperto, 3, "b2","basal2") +
  graf_exp(da_diff_aperto, 3, "b2","contingente") +
  graf_exp(da_diff_aperto, 3, "b2","não contingente")

# Grupo 3

graf_exp(da_diff_aperto, 1, "b3","basal1") +
  graf_exp(da_diff_aperto, 1, "b3","basal2") +
  graf_exp(da_diff_aperto, 1, "b3","contingente") +
  graf_exp(da_diff_aperto, 1, "b3","não contingente")

graf_exp(da_diff_aperto, 2, "b3","basal1") +
  graf_exp(da_diff_aperto, 2, "b3","basal2") +
  graf_exp(da_diff_aperto, 2, "b3","contingente") +
  graf_exp(da_diff_aperto, 2, "b3","não contingente")

graf_exp(da_diff_aperto, 3, "b3","basal1") +
  graf_exp(da_diff_aperto, 3, "b3","basal2") +
  graf_exp(da_diff_aperto, 3, "b3","contingente") +
  graf_exp(da_diff_aperto, 3, "b3","não contingente")

# Teste de KS da variavel tempo entre apertos

require(vcd)
require(MASS)

teste <- function(c,d,g){
  base = as.data.frame(da_diff_aperto %>%
                         dplyr::filter(!is.na(diff_aperto),condicao == c,dia== d, grupo == g) %>%
                         dplyr::select(diff_aperto))
  fit = fitdistr(base$diff_aperto,"exponential")
  ks.test(base$diff_aperto,"pexp", fit$estimate)
}

# Grupo 1

#todos os testes foram rejeitados

teste('basal1',1,'b1')
teste('basal2',1,'b1')
teste('contingente',1,'b1')
teste('não contingente',1,'b1')

teste('basal1',2,'b1')
teste('basal2',2,'b1')
teste('contingente',2,'b1')
teste('não contingente',2,'b1')

teste('basal1',3,'b1')
teste('basal2',3,'b1')
teste('contingente',3,'b1')
teste('não contingente',3,'b1')

# Grupo 2

#todos os testes foram rejeitados

teste('basal1',1,'b2')
teste('basal2',1,'b2')
teste('contingente',1,'b2')
teste('não contingente',1,'b2')

teste('basal1',2,'b2')
teste('basal2',2,'b2')
teste('contingente',2,'b2')
teste('não contingente',2,'b2')

teste('basal1',3,'b2')
teste('basal2',3,'b2')
teste('contingente',3,'b2')
teste('não contingente',3,'b2')

# Grupo 3

#todos os testes foram rejeitados

teste('basal1',1,'b3')
teste('basal2',1,'b3')
teste('contingente',1,'b3')
teste('não contingente',1,'b3')

teste('basal1',2,'b3')
teste('basal2',2,'b3')
teste('contingente',2,'b3')
teste('não contingente',2,'b3')

teste('basal1',3,'b3')
teste('basal2',3,'b3')
teste('contingente',3,'b3')
teste('não contingente',3,'b3')



