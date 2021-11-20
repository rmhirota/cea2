library(magrittr)
library(patchwork)

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

htmlwidgets::saveWidget(widget=g3,file="analises/bas_c_interativo_g3.html")
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
  #dplyr::filter(grupo == 1) %>%
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
