# Análise descritiva dos dados brutos
library(magrittr)
library(patchwork)
library(dplyr)

# dados arrumados
da <- cea2::da_tidy %>% filter(condicao=='contingente')


# Perfis ------------------------------------------------------------------
# Perfil de todos os bebês por grupo/dia para condição contingente

nomes_b1 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b1", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b2 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b2", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()
nomes_b3 <- cea2::da_tidy %>%
  dplyr::filter(grupo == "b3", condicao == "contingente") %>%
  dplyr::pull(nome) %>% unique()

b1_d1 <- purrr::map(cea2::filtra_nomes(da, "b1", 1), ~cea2::p_video_bebe(da, .x, 1, "b1"))
b1_d2 <- purrr::map(cea2::filtra_nomes(da, "b1", 2), ~cea2::p_video_bebe(da, .x, 2, "b1"))
b1_d3 <- purrr::map(cea2::filtra_nomes(da, "b1", 3), ~cea2::p_video_bebe(da, .x, 3, "b1"))
b2_d1 <- purrr::map(cea2::filtra_nomes(da, "b2", 1), ~cea2::p_video_bebe(da, .x, 1, "b2"))
b2_d2 <- purrr::map(cea2::filtra_nomes(da, "b2", 2), ~cea2::p_video_bebe(da, .x, 2, "b2"))
b2_d3 <- purrr::map(cea2::filtra_nomes(da, "b2", 3), ~cea2::p_video_bebe(da, .x, 3, "b2"))
b3_d1 <- purrr::map(nomes_b3, ~cea2::p_video_bebe(da, .x, 1, "b3"))
b3_d2 <- purrr::map(nomes_b3, ~cea2::p_video_bebe(da, .x, 2, "b3"))
b3_d3 <- purrr::map(nomes_b3, ~cea2::p_video_bebe(da, .x, 3, "b3"))

# grupo 1
dia1 <- purrr::reduce(b1_d1, `/`)
dia2 <- purrr::reduce(b1_d2, `/`)
dia3 <- purrr::reduce(b1_d3, `/`)
grupo1 <- dia1 | dia2 | dia3
ggplot2::ggsave("analises/perfis_grupo1.png", grupo1,width = 7, height = 7)
ggplot2::ggsave("pres/plots/perfis_grupo1.png", grupo1,width = 7, height = 7)

# grupo 2
dia1 <- purrr::reduce(b2_d1, `/`)
dia2 <- purrr::reduce(b2_d2, `/`)
dia3 <- purrr::reduce(b2_d3, `/`)
grupo2 <- dia1 | dia2 | dia3
ggplot2::ggsave("analises/perfis_grupo2.png", grupo2,width = 7, height = 7)
ggplot2::ggsave("pres/plots/perfis_grupo2.png", grupo2,width = 7, height = 7)

# grupo 3
dia1 <- purrr::reduce(b3_d1, `/`)
dia2 <- purrr::reduce(b3_d2, `/`)
dia3 <- purrr::reduce(b3_d3, `/`)
grupo3 <- dia1 | dia2 | dia3
ggplot2::ggsave("analises/perfis_grupo3.png", grupo3,width = 7, height = 7)
ggplot2::ggsave("pres/plots/perfis_grupo3.png", grupo3,width = 7, height = 7)


# Pressão média por condição ----------------------------------------------

p_media <- da %>%
  dplyr::group_by(grupo, condicao, dia) %>%
  dplyr::summarise(media = mean(pressao)) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = dia, y = media, fill = grupo
  ),fill=condicao) +
  ggplot2::geom_bar(stat="identity",position="dodge") +
  ggplot2::facet_wrap(~condicao)+
  #ggplot2::ggtitle("Pressão Média") +
  ggplot2::labs(x = "Dia")+
  ggplot2::labs(y = "Pressão Média")+
  ggplot2::theme_minimal()+
  ggplot2::scale_fill_manual(values = c("#440154FF", "#21908CFF", "#FDE725FF"))

ggplot2::ggsave("analises/pressao_media.jpeg", p_media)

# Pressão Média Resumos ---------------------------------------------------
# Média por agrupamento
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

media_dia = media_grupo(cea2::da_tidy, pressao, dia) %>%
  #janitor::adorn_totals() %>%
  dplyr::mutate(dplyr::across(2:4, ~round(.x, 3))) %>%
  knitr::kable(col.names = c("Dia", "Pressão Média", "Desvio Padrão", "Pressão Mediana"))

media_grupo = media_grupo(cea2::da_tidy, pressao, grupo)  %>%
  #janitor::adorn_totals() %>%
  dplyr::mutate(dplyr::across(2:4, ~round(.x, 3))) %>%
  knitr::kable(col.names = c("Grupo", "Pressão Média", "Desvio Padrão", "Pressão Mediana"))

media_condicao = media_grupo(cea2::da_tidy, pressao, condicao)  %>%
  #janitor::adorn_totals() %>%
  dplyr::mutate(dplyr::across(2:4, ~round(.x, 3))) %>%
  knitr::kable(col.names = c("Condição", "Pressão Média", "Desvio Padrão", "Pressão Mediana"))

# Pressão media ao longo do tempo -----------------------------------------

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
      y = "Tempo entre vídeos",
      x = "Dia"
    )
}

# grupo 1
tempo_grupo1 <- tempo_video(da_diff_tempo, "b1","contingente")
ggplot2::ggsave("analises/tempo_video_g1.jpeg", tempo_grupo1)
# grupo 2
tempo_grupo2 <- tempo_video(da_diff_tempo, "b2","contingente")
ggplot2::ggsave("analises/tempo_video_g2.jpeg", tempo_grupo2)
# grupo 3
tempo_grupo3 <- tempo_video(da_diff_tempo, "b3","contingente")
ggplot2::ggsave("analises/tempo_video_g3.jpeg", tempo_grupo3)

## grupo 1
# t_g1_d1 = purrr::map(nomes_b1, ~tempo_video(cea2::da_tidy,1, "b1",.x))
# purrr::reduce(t_g1_d1,`+`)
# t_g1_d2 = purrr::map(nomes_b1, ~tempo_video(cea2::da_tidy,2, "b1",.x))
# purrr::reduce(t_g1_d2,`+`)
# t_g1_d3 = purrr::map(nomes_b1, ~tempo_video(cea2::da_tidy,3, "b1",.x))
# purrr::reduce(t_g1_d3,`+`)
#
# ggplot2::ggsave("analises/tempo_video_g1_d1.jpeg", purrr::reduce(t_g1_d1,`+`))
# ggplot2::ggsave("analises/tempo_video_g1_d2.jpeg", purrr::reduce(t_g1_d2,`+`))
# ggplot2::ggsave("analises/tempo_video_g1_d3.jpeg", purrr::reduce(t_g1_d3,`+`))
#
#
# ## grupo 2
# t_g2_d1 = purrr::map(nomes_b2, ~tempo_video(cea2::da_tidy,1, "b2",.x))
# purrr::reduce(t_g2_d1,`+`)
# t_g2_d2 = purrr::map(nomes_b2, ~tempo_video(cea2::da_tidy,2,"b2",.x))
# purrr::reduce(t_g2_d2,`+`)
# t_g2_d3 = purrr::map(nomes_b2, ~tempo_video(cea2::da_tidy,3,"b2",.x))
# purrr::reduce(t_g2_d3,`+`)
#
# ggplot2::ggsave("analises/tempo_video_g2_d1.jpeg", purrr::reduce(t_g2_d1,`+`))
# ggplot2::ggsave("analises/tempo_video_g2_d2.jpeg", purrr::reduce(t_g2_d2,`+`))
# ggplot2::ggsave("analises/tempo_video_g2_d3.jpeg", purrr::reduce(t_g2_d3,`+`))
#
# ## grupo 3
# t_g3_d1 = purrr::map(nomes_b3, ~tempo_video(cea2::da_tidy,1, "b3",.x))
# purrr::reduce(t_g3_d1,`/`)
# t_g3_d2 = purrr::map(nomes_b3, ~tempo_video(cea2::da_tidy,2,"b3",.x))
# purrr::reduce(t_g3_d2,`+`)
# t_g3_d3 = purrr::map(nomes_b3, ~tempo_video(cea2::da_tidy,3,"b3",.x))
# purrr::reduce(t_g3_d3,`+`)
#
# ggplot2::ggsave("analises/tempo_video_g3_d1.jpeg", purrr::reduce(t_g3_d1,`+`))
# ggplot2::ggsave("analises/tempo_video_g3_d2.jpeg", purrr::reduce(t_g3_d2,`+`))
# ggplot2::ggsave("analises/tempo_video_g3_d3.jpeg", purrr::reduce(t_g3_d3,`+`))

# variavel tempo entre disparo do video - por dia

  da %>% dplyr::filter(condicao == "contingente") %>%
    dplyr::mutate(rank = dplyr::row_number(tempo)) %>%
    dplyr::group_by(grupo, nome, dia, condicao) %>%
    dplyr::mutate(status_video = dplyr::case_when(video == FALSE & dplyr::lag(video) == TRUE  ~ "fim",
                                                  video == TRUE & dplyr::lag(video) == FALSE ~ "inicio",
                                                  video == TRUE & dplyr::lag(video) == TRUE ~ "meio")) %>%
    dplyr::filter(status_video %in% c("inicio","fim")) %>%
    dplyr::mutate(diff_video = dplyr::case_when(status_video == "inicio" &
                                                  dplyr::lag(status_video) == "fim" ~ (tempo-dplyr::lag(tempo)))) %>%

    ggplot2::ggplot(ggplot2::aes(
      x = dia, y = diff_video, fill = dia)) +
    ggplot2::geom_boxplot() +
    #ggplot2::xlab("Bebê") +
    #ggplot2::ylab("Tempo")+
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position="none"
    ) +
    ggplot2::labs(
      #   title = "Pressão ao longo do tempo",
      #   subtitle = glue::glue("Grupo {gr} / Dia {d}"),
      y = ggplot2::element_blank(),
      x = ggplot2::element_blank(),
      axis.title.x= ggplot2::element_blank()
    ) + ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values=c("#440154FF", "#21908CFF", "#FDE725FF"))

#pressão média por segundo basal1 x basal2 por bebe por dia

  da_basal <- da %>%
    dplyr::filter(
      condicao == "basal1"| condicao == "basal2", dia == d,
      grupo == gr, nome == bebe
    )

  gganim1 = ggplot2::ggplot(da_basal,ggplot2::aes(x = tempo, y = pressao, color = condicao)) +
    ggplot2::geom_line()+
    gganimate::transition_time(tempo)+
    labs(title = 'Pressão média no segundo {frame_time}', x = 'Segundos', y = 'Pressão média de aperto')+
    ease_aes('linear')
  gganimate::anim_save("analises/gganimado_eduarda_dia1.gif",gganim1)

  animate(gganim1)

