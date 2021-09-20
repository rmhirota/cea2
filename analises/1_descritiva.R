library(magrittr)
library(patchwork)

da_spss <- readr::read_rds("data-raw/da_spss_tidy.rds")
dplyr::glimpse(da_spss)

# Funções -----------------------------------------------------------------

# Média por agrupamento
media_grupo <- function(da, variavel, ...) {
  da %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      "media_{{variavel}}" := mean({{variavel}}),
      "dp_{{variavel}}" := sd({{variavel}}),
      .groups = "drop"
    )
}
media_grupo(da_spss, freq_apertos, dia, grupo)
media_grupo(da_spss, n_apertos, dia)
media_grupo(da_spss, n_apertos, grupo)
media_grupo(da_spss, n_apertos, grupo)

# boxplot
boxplot_uni <- function(da, variavel, grupo) {
  da %>%
    ggplot2::ggplot(ggplot2::aes(x = {{grupo}}, y = {{variavel}})) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_light() +
    ggplot2::labs(title = glue::glue(
      "Boxplot de {rlang::enexpr(variavel)} por {rlang::enexpr(grupo)}"
    ))
}

col_dia_grupo_cond <- function(da, variavel) {
  da %>%
    ggplot2::ggplot(ggplot2::aes(x = dia, y = {{variavel}}, fill = grupo)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~condicao) +
    ggplot2::theme_light()
}

# Base SPSS ---------------------------------------------------------------

# n_apertos ---------------------------------------------------------------

# Boxplots univariados
boxplot_uni(da_spss, n_apertos, dia) # n_apertos por dia
boxplot_uni(da_spss, n_apertos, grupo) # n_apertos por grupo
boxplot_uni(da_spss, n_apertos, condicao) # n_apertos por condicao

# Média por grupo, condição e dia
da_spss %>%
  media_grupo(n_apertos, grupo, condicao, dia) %>%
  col_dia_grupo_cond(media_n_apertos)

da_spss %>%
  media_grupo(n_apertos, nome, grupo, condicao, dia) %>%
  tidyr::pivot_longer(dia, values_to = "dia")
  ggplot2::ggplot(ggplot2::aes(
    x = dia, y = media_n_apertos, colour = dia
  )) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~condicao)

da_spss %>%
  dplyr::mutate(
    nome = stringr::str_c(nome, grupo),
    grupo = stringr::str_c("B", grupo),
    dia = as.factor(dia)
  ) %>%
  dplyr::group_by(nome, grupo, condicao, dia) %>%
  dplyr::summarise(media = mean(n_apertos)) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = grupo, y = media, colour = dia
  )) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~condicao)


#  freq_apertos -----------------------------------------------------------

# Boxplots univariados
boxplot_uni(da_spss, freq_apertos, dia) # freq_apertos por dia
boxplot_uni(da_spss, freq_apertos, grupo) # freq_apertos por grupo
boxplot_uni(da_spss, freq_apertos, condicao) # freq_apertos por condicao

# Média por grupo, condição e dia
da_spss %>%
  media_grupo(freq_apertos, grupo, condicao, dia) %>%
  col_dia_grupo_cond(media_freq_apertos)


# media_pressao -----------------------------------------------------------

# Boxplots univariados
boxplot_uni(da_spss, media_pressao, dia) # media_pressao por dia
boxplot_uni(da_spss, media_pressao, grupo) # media_pressao por grupo
boxplot_uni(da_spss, media_pressao, condicao) # media_pressao por condicao

# Média por grupo, condição e dia
da_spss %>%
  media_grupo(media_pressao, grupo, condicao, dia) %>%
  col_dia_grupo_cond(media_media_pressao)

da_spss %>%
  dplyr::filter(grupo == 3) %>%
  dplyr::mutate(
    nome = stringr::str_c(nome, grupo),
    grupo = stringr::str_c("B", grupo),
    dia = as.factor(dia)
  ) %>%
  dplyr::group_by(nome, grupo, condicao, dia) %>%
  dplyr::summarise(media = mean(media_pressao)) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = dia, y = media, colour = condicao
  )) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~condicao)

# da_spss %>%
#   dplyr::mutate(
#     nome = stringr::str_c(nome, grupo),
#     grupo = stringr::str_c("B", grupo),
#     dia = as.factor(dia)
#   ) %>%
#   dplyr::group_by(nome, grupo, condicao, dia) %>%
#   dplyr::summarise(media = mean(media_pressao)) %>%
#   ggplot2::ggplot(ggplot2::aes(
#     x = dia, y = grupo, colour = condicao, size = media
#   )) +
#   ggbeeswarm::geom_beeswarm(grouponX = TRUE) +
#   ggplot2::facet_wrap(~condicao)


# pico --------------------------------------------------------------------
# confirmar definição da variável

# Boxplots univariados
boxplot_uni(da_spss, pico, dia) # pico por dia
boxplot_uni(da_spss, pico, grupo) # pico por grupo
boxplot_uni(da_spss, pico, condicao) # pico por condicao

# Média por grupo, condição e dia
da_spss %>%
  media_grupo(pico, grupo, condicao, dia) %>%
  col_dia_grupo_cond(media_pico)

# media_duracao_pico ------------------------------------------------------

# Boxplots univariados
boxplot_uni(da_spss, media_duracao_pico, dia) # media_duracao_pico por dia
boxplot_uni(da_spss, media_duracao_pico, grupo) # media_duracao_pico por grupo
boxplot_uni(da_spss, media_duracao_pico, condicao) # media_duracao_pico por condicao

# Média por grupo, condição e dia
da_spss %>%
  media_grupo(media_duracao_pico, grupo, condicao, dia) %>%
  col_dia_grupo_cond(media_media_duracao_pico)

## tentativa de grafico 3d - apertos
library(plotly)
dia1 = da_spss %>% filter(dia==1) %>% select(grupo,n_apertos)
dia2 = da_spss %>% filter(dia==2) %>% select(n_apertos)
dia3 = da_spss %>% filter(dia==3) %>% select(n_apertos)
data = data.frame(dia1,dia2,dia3)
colnames(data) = c("grupo","dia1","dia2","dia3")

fig <- plot_ly(data,x=data$dia1, y=data$dia2, z=data$dia3,color=data$grupo,colors = "Set1",mode="markers")
fig <- add_markers(fig)
fig <- layout(fig,scene = list(xaxis = list(title = 'Apertos: Dia 1'),
                                   yaxis = list(title = 'Apertos: Dia 2'),
                                   zaxis = list(title = 'Apertos: Dia 3')))
fig

## 2 x 2
data %>%  plot_ly(x=data$dia1, y=data$dia2,color=data$grupo,mode="markers",colors = "Set1") %>%
  add_markers() %>%
  layout(title = 'Número de apertos: Dia 1 x Dia 2',xaxis = list(title = 'Dia 1'),
                               yaxis = list(title = 'Dia 2'))

data %>%  plot_ly(x=data$dia2, y=data$dia3,color=data$grupo,mode="markers",colors = "Set1") %>%
  add_markers() %>%
  layout(title= 'Número de apertos: Dia 2 x Dia 3',xaxis = list(title = 'Dia 2'),
                      yaxis = list(title = 'Dia 3'))


## tentativa de grafico 3d - pressão média
dia1_ = da_spss %>% filter(dia==1) %>% select(grupo,media_pressao)
dia2_ = da_spss %>% filter(dia==2) %>% select(media_pressao)
dia3_ = da_spss %>% filter(dia==3) %>% select(media_pressao)
data_ = data.frame(dia1_,dia2_,dia3_)
colnames(data_) = c("grupo","dia1","dia2","dia3")

fig2 <- plot_ly(data_,x=data_$dia1, y=data_$dia2, z=data_$dia3,colors = "Set1",color=data_$grupo,mode="markers")
fig2 <- add_markers(fig2)
fig2 <- layout(fig2,scene = list(xaxis = list(title = 'Dia 1'),
                               yaxis = list(title = 'Dia 2'),
                               zaxis = list(title = 'Dia 3')))
fig2

## 2 x 2
data_ %>%  plot_ly(x=data_$dia1, y=data_$dia2,color=data_$grupo,mode="markers",colors = "Set1") %>%
  add_markers() %>%
  layout(title = 'Pressão: Dia 1 x Dia 2',xaxis = list(title = 'Dia 1'),
         yaxis = list(title = 'Dia 2'))

data_ %>%  plot_ly(x=data_$dia2, y=data_$dia3,color=data_$grupo,mode="markers",colors = "Set1") %>%
  add_markers() %>%
  layout(title= 'Pressão: Dia 2 x Dia 3',xaxis = list(title = 'Dia 2'),
         yaxis = list(title = 'Dia 3'))

# Média de tempo de vídeo por grupo na condição contingente
# valores referentes a porcentagem do tempo da tentativa em que o vídeo esteve ativado.

spss3 <- foreign::read.spss("data-raw/spss/Tempo Video.sav") %>%
  +   dplyr::as_tibble()

graf_cont = spss3 %>% group_by(Grupo) %>% summarise(media_cont = mean(Cont), media_nc = mean(NC)) %>%
  ggplot() +
  geom_col(aes(x = Grupo, y = media_cont),fill="purple") +
  ggtitle("Média do tempo de vídeo na condição contingente") +
  labs(y = "Média de tempo")+theme_minimal()+
  geom_label(aes(x = Grupo, y = media_cont, label = round(media_cont,2)))
graf_cont

# No geral, no primeiro dia do experimento a porcentagem do tempo média em que o vídeo esteve ativado.
# é superior nos grupos 1 e 2 (parece q bebes nesses grupos tendem a atingir mais o valor gatilho)
# intrigante pq vimos anteriormente que o o número médio de apertos nesse dia era maior nos grupos 2 e 3
# ou seja, bebes de 3 meses apertaram mais, porém, atingiram em média menos vezes o valor de gatilho


# Perfis ------------------------------------------------------------------
dados1 <- readr::read_rds( "data-raw/da_tidy.rds")
dados2 <- readr::read_rds( "data-raw/da_brutos.rds")
da = dplyr::inner_join(dados1,dados2,by="arq",suffix = c("",""))


### pressão média por dia-condicao-grupo

da %>% dplyr::group_by(grupo,condicao,dia) %>%
  dplyr::summarise(media=mean(pressao)) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = dia, y = media, fill = grupo
  ),fill=condicao) +
  ggplot2::geom_bar(stat="identity",position="dodge") +
  ggplot2::facet_wrap(~condicao)+
  #ggplot2::ggtitle("Pressão Média") +
  ggplot2::labs(x = "Dia")+
  ggplot2::labs(y = "Pressão Média")+
  ggplot2::theme_minimal()+
  ggplot2::scale_fill_manual(values=c("#CCCCCC", "#999999", "#666666"))




# por bebê com vídeo (contingente)
p_video_bebe <- function(da, bebe, d, gr) {
  da_bebe <- da %>%
    dplyr::filter(
      condicao == "contingente", dia == d,
      grupo == gr, nome == bebe
    )
  video <- da_bebe %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::select(id, video, tempo, pressao) %>%
    dplyr::filter(video)
  # pressao_min <- min(video$id)
  # gatilho <- da_bebe %>%
  #   dplyr::mutate(id = dplyr::row_number()) %>%
  #   dplyr::filter(id < pressao_min) %>%
  #   dplyr::slice_max(id) %>%
  #   dplyr::pull(pressao)
  da_bebe %>%
    ggplot2::ggplot(ggplot2::aes(x = tempo, y = pressao)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(
      data = video, ggplot2::aes(xintercept = tempo),
      alpha = .01, colour = "blue"
    ) +
    # ggplot2::geom_hline(yintercept = gatilho, colour = "red") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank() ,
      panel.grid.major.y = ggplot2::element_line(size=.1, color="black"),
      panel.grid.minor.x = ggplot2::element_blank() ,
      panel.grid.minor.y = ggplot2::element_line(size=.01, color="black")
    )
}

nomes_b1 <- da %>%
  dplyr::filter(grupo == "b1") %>%
  dplyr::pull(nome) %>% unique()
nomes_b2 <- da %>%
  dplyr::filter(grupo == "b2") %>%
  dplyr::pull(nome) %>% unique()
nomes_b3 <- da %>%
  dplyr::filter(grupo == "b3") %>%
  dplyr::pull(nome) %>% unique()

# dia 1
p1 = purrr::map(nomes_b1, ~p_video_bebe(da, .x, 1, "b1"))
p2 = purrr::map(nomes_b2, ~p_video_bebe(da, .x, 1, "b2"))
p3 = purrr::map(nomes_b3, ~p_video_bebe(da, .x, 1, "b3"))

p1[[1]]+p1[[2]]+p1[[3]]+p1[[4]]+p1[[5]]+p1[[6]]
p2[[1]]+p2[[2]]+p2[[3]]+p2[[4]]+p2[[5]]+p2[[6]]
p3[[1]] + p3[[2]] + p3[[3]] + p3[[4]] + p3[[6]]
#p_video_bebe(da, nomes_b1[2], 1, "b2")

# dia 2

purrr::map(nomes_b1, ~p_video_bebe(da, .x, 2, "b1"))
purrr::map(nomes_b2, ~p_video_bebe(da, .x, 2, "b2"))
purrr::map(nomes_b3, ~p_video_bebe(da, .x, 2, "b3"))

# dia 3

purrr::map(nomes_b1, ~p_video_bebe(da, .x, 3, "b1"))
purrr::map(nomes_b2, ~p_video_bebe(da, .x, 3, "b2"))
purrr::map(nomes_b3, ~p_video_bebe(da, .x, 3, "b3"))

perfil <- function(da, d, gr, cond) {
  da %>%
    dplyr::filter(condicao == cond, dia == d, grupo == gr) %>%
    dplyr::mutate(tempo_trunc = floor(tempo*10)/10) %>%
    dplyr::group_by(nome, tempo_trunc) %>%
    dplyr::summarise(media_pressao = mean(pressao), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(
      x = tempo_trunc, y = media_pressao, group = nome, colour = nome
    )) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "Média de pressão por segundo")+
    ggplot2::xlab("Segundos") +
    ggplot2::ylab("Pressão Média")
     # subtitle = glue::glue("Dia {d} / Grupo {gr} / {cond}")
}

# dia 1
# grupo 1
perfil(da, 1, "b1", "basal1")  #bebe nicolas com uma media de pressão mto alta em detrminado momento
perfil(da, 1, "b1", "contingente")  #media de pressão maior do que na condição basal
perfil(da, 1, "b1", "não contingente")  #media de pressão mto menor (pico do bebe renan)
                                       #aqui, o comportamento deles é mto parecido
perfil(da, 1, "b1", "basal2")

perfil(da, 3, "b2", "contingente")

# agrupado por condição
# grupo 1, dia 1
da %>%
  dplyr::filter(dia == 1, grupo == "b1") %>%
  dplyr::mutate(tempo_trunc = floor(tempo*10)/10) %>%
  dplyr::group_by(condicao, tempo_trunc) %>%
  dplyr::summarise(media_pressao = mean(pressao), .groups = "drop") %>%
  highcharter::hchart(
    "line",
    highcharter::hcaes(tempo_trunc, media_pressao, group = condicao)
  ) %>%
  highcharter::hc_tooltip(
    pointFormat = paste0(
      "<b>Tempo</b>: {point.tempo_trunc}<br>",
      "<b>Pressão</b>: {point.media_pressao}<br>",
      "<b>Condição</b>: {point.condicao}<br>"
    )
  )

# grupo 1, dia 2
da %>%
  dplyr::filter(dia == 2, grupo == "b1") %>%
  dplyr::mutate(tempo_trunc = floor(tempo*10)/10) %>%
  dplyr::group_by(condicao, tempo_trunc) %>%
  dplyr::summarise(media_pressao = mean(pressao), .groups = "drop") %>%
  highcharter::hchart(
    "line",
    highcharter::hcaes(tempo_trunc, media_pressao, group = condicao)
  ) %>%
  highcharter::hc_tooltip(
    pointFormat = paste0(
      "<b>Tempo</b>: {point.tempo_trunc}<br>",
      "<b>Pressão</b>: {point.media_pressao}<br>",
      "<b>Condição</b>: {point.condicao}<br>"
    )
  )

# grupo 1, dia 3
da %>%
  dplyr::filter(dia == 3, grupo == "b1") %>%
  dplyr::mutate(tempo_trunc = floor(tempo*10)/10) %>%
  dplyr::group_by(condicao, tempo_trunc) %>%
  dplyr::summarise(media_pressao = mean(pressao), .groups = "drop") %>%
  highcharter::hchart(
    "line",
    highcharter::hcaes(tempo_trunc, media_pressao, group = condicao)
  ) %>%
  highcharter::hc_tooltip(
    pointFormat = paste0(
      "<b>Tempo</b>: {point.tempo_trunc}<br>",
      "<b>Pressão</b>: {point.media_pressao}<br>",
      "<b>Condição</b>: {point.condicao}<br>"
    )
  )

# grupo 2, dia 1
da %>%
  dplyr::filter(dia == 1, grupo == "b2") %>%
  dplyr::mutate(tempo_trunc = floor(tempo*10)/10) %>%
  dplyr::group_by(condicao, tempo_trunc) %>%
  dplyr::summarise(media_pressao = mean(pressao), .groups = "drop") %>%
  highcharter::hchart(
    "line",
    highcharter::hcaes(tempo_trunc, media_pressao, group = condicao)
  ) %>%
  highcharter::hc_tooltip(
    pointFormat = paste0(
      "<b>Tempo</b>: {point.tempo_trunc}<br>",
      "<b>Pressão</b>: {point.media_pressao}<br>",
      "<b>Condição</b>: {point.condicao}<br>"
    )
  )

# grupo 2, dia 2
da %>%
  dplyr::filter(dia == 2, grupo == "b2") %>%
  dplyr::mutate(tempo_trunc = floor(tempo*10)/10) %>%
  dplyr::group_by(condicao, tempo_trunc) %>%
  dplyr::summarise(media_pressao = mean(pressao), .groups = "drop") %>%
  highcharter::hchart(
    "line",
    highcharter::hcaes(tempo_trunc, media_pressao, group = condicao)
  ) %>%
  highcharter::hc_tooltip(
    pointFormat = paste0(
      "<b>Tempo</b>: {point.tempo_trunc}<br>",
      "<b>Pressão</b>: {point.media_pressao}<br>",
      "<b>Condição</b>: {point.condicao}<br>"
    )
  )

# grupo 2, dia 3
da %>%
  dplyr::filter(dia == 3, grupo == "b2") %>%
  dplyr::mutate(tempo_trunc = floor(tempo*10)/10) %>%
  dplyr::group_by(condicao, tempo_trunc) %>%
  dplyr::summarise(media_pressao = mean(pressao), .groups = "drop") %>%
  highcharter::hchart(
    "line",
    highcharter::hcaes(tempo_trunc, media_pressao, group = condicao)
  ) %>%
  highcharter::hc_tooltip(
    pointFormat = paste0(
      "<b>Tempo</b>: {point.tempo_trunc}<br>",
      "<b>Pressão</b>: {point.media_pressao}<br>",
      "<b>Condição</b>: {point.condicao}<br>"
    )
  )

# grupo 3, dia 1
da %>%
  dplyr::filter(dia == 1, grupo == "b3") %>%
  dplyr::mutate(tempo_trunc = floor(tempo*10)/10) %>%
  dplyr::group_by(condicao, tempo_trunc) %>%
  dplyr::summarise(media_pressao = mean(pressao), .groups = "drop") %>%
  highcharter::hchart(
    "line",
    highcharter::hcaes(tempo_trunc, media_pressao, group = condicao)
  ) %>%
  highcharter::hc_tooltip(
    pointFormat = paste0(
      "<b>Tempo</b>: {point.tempo_trunc}<br>",
      "<b>Pressão</b>: {point.media_pressao}<br>",
      "<b>Condição</b>: {point.condicao}<br>"
    )
  )

# grupo 3, dia 2
da %>%
  dplyr::filter(dia == 2, grupo == "b3") %>%
  dplyr::mutate(tempo_trunc = floor(tempo*10)/10) %>%
  dplyr::group_by(condicao, tempo_trunc) %>%
  dplyr::summarise(media_pressao = mean(pressao), .groups = "drop") %>%
  highcharter::hchart(
    "line",
    highcharter::hcaes(tempo_trunc, media_pressao, group = condicao)
  ) %>%
  highcharter::hc_tooltip(
    pointFormat = paste0(
      "<b>Tempo</b>: {point.tempo_trunc}<br>",
      "<b>Pressão</b>: {point.media_pressao}<br>",
      "<b>Condição</b>: {point.condicao}<br>"
    )
  )

# grupo 3, dia 3
da %>%
  dplyr::filter(dia == 3, grupo == "b3") %>%
  dplyr::mutate(tempo_trunc = floor(tempo*10)/10) %>%
  dplyr::group_by(condicao, tempo_trunc) %>%
  dplyr::summarise(media_pressao = mean(pressao), .groups = "drop") %>%
  highcharter::hchart(
    "line",
    highcharter::hcaes(tempo_trunc, media_pressao, group = condicao)
  ) %>%
  highcharter::hc_tooltip(
    pointFormat = paste0(
      "<b>Tempo</b>: {point.tempo_trunc}<br>",
      "<b>Pressão</b>: {point.media_pressao}<br>",
      "<b>Condição</b>: {point.condicao}<br>"
    )
  )

# variavel tempo entre disparo do video

tempo_video <- function(da, d, gr, b) {
  da %>%
    dplyr::filter(condicao == "contingente", dia == d, grupo == gr, nome == b) %>%
    dplyr::mutate(ate_video = dplyr::case_when(video == FALSE ~ 0,
                                               video == TRUE ~ dplyr::lag(tempo))) %>%
    dplyr::mutate(diff_video = dplyr::case_when(video == FALSE ~ 0,
                                                video == TRUE ~tempo - ate_video ))%>%
    ggplot2::ggplot(ggplot2::aes(
      x = nome, y = diff_video)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Bebê") +
    ggplot2::ylab("Tempo")
    ##ggplot2::labs(
      ##title = "Tempo entre ativação do video")
  #subtitle = glue::glue("Dia {d} / Grupo {gr} / Bebê{b}")
}

## grupo 1
a = purrr::map(nomes_b1, ~tempo_video(da,1, "b1",.x))
a[[1]]+a[[2]]+a[[3]]+a[[4]]+a[[5]]+a[[6]]

b3 = purrr::map(nomes_b1, ~tempo_video(da,3,"b1",.x))
b3[[1]]+b3[[2]]+b3[[4]]+b3[[5]]+b3[[6]]

## grupo 2
a = purrr::map(nomes_b2, ~tempo_video(da,1, "b2",.x))
a[[1]]+a[[2]]+a[[3]]+a[[4]]+a[[5]]+a[[6]]

b3 = purrr::map(nomes_b2, ~tempo_video(da,3,"b2",.x))
b3[[1]]+b3[[2]]+b3[[3]]+b3[[4]]+b3[[5]]+b3[[6]]

## grupo 3
a = purrr::map(nomes_b3, ~tempo_video(da,1, "b3",.x))
a[[1]]+a[[2]]+a[[3]]+a[[4]]+a[[6]]

b3 = purrr::map(nomes_b3, ~tempo_video(da,3,"b3",.x))
b3[[2]]+b3[[3]]+b3[[4]]+b3[[5]]+b3[[6]]

## tempo entre video ao longo da serie (scatterplot)

tempo_video2 <- function(da, d, gr, b) {
  da %>%
    dplyr::filter(condicao == "contingente", dia == d, grupo == gr, nome == b) %>%
    dplyr::mutate(ate_video = dplyr::case_when(video == FALSE ~ 0,
                                               video == TRUE ~ dplyr::lag(tempo))) %>%
    dplyr::mutate(diff_video = dplyr::case_when(video == FALSE ~ 0,
                                                video == TRUE ~tempo - ate_video ))%>%
    dplyr::filter(video == TRUE) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = tempo, y = diff_video)) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "Tempo entre ativação do video",
      ylab= "Segundos")
  #subtitle = glue::glue("Dia {d} / Grupo {gr} / Bebê{b}")
}

b = purrr::map(nomes_b1, ~tempo_video2(da,1, "b1",.x))
b[[1]]+b[[2]]+b[[3]]+b[[4]]+b[[5]]+b[[6]]



## tempo de video ativo por bebe

## acho q essa base esta errada (o lag n esta difinido por bebe)

tempo_ativo1 = da %>% dplyr::filter(!condicao %in% c("basal1","basal2")) %>%
  dplyr::mutate(ate_video = dplyr::case_when(video == FALSE ~ 0,
                                             video == TRUE ~ dplyr::lag(tempo))) %>%
  dplyr::mutate(diff_video = dplyr::case_when(video == FALSE ~ 0,
                                              video == TRUE ~tempo - ate_video ))%>%
  dplyr::group_by(dia,grupo,nome,condicao) %>% dplyr::summarise(total=sum(diff_video))


# Média de tempo de vídeo por grupo na condição contingente

graf_cont1 = tempo_ativo1 %>% dplyr::filter(dia==1,condicao=="contingente") %>%
  dplyr::group_by(grupo) %>% dplyr::summarise(media_cont = mean(total)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = grupo, y = media_cont),fill="purple") +
  ggplot2::ggtitle("Média do tempo de vídeo na condição contingente") +
  ggplot2::labs(y = "Média de tempo (Dia 1)")+ggplot2::theme_minimal()+
  ggplot2::geom_label(ggplot2::aes(x = grupo, y = media_cont, label = round(media_cont,2)))

graf_cont2 = tempo_ativo1 %>% dplyr::filter(dia==2,condicao=="contingente") %>%
  dplyr::group_by(grupo) %>% dplyr::summarise(media_cont = mean(total)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = grupo, y = media_cont),fill="purple") +
 # ggplot2::ggtitle("Média do tempo de vídeo na condição contingente") +
  ggplot2::labs(y = "Média de tempo (Dia 2)")+ggplot2::theme_minimal()+
  ggplot2::geom_label(ggplot2::aes(x = grupo, y = media_cont, label = round(media_cont,2)))

graf_cont3 = tempo_ativo1 %>% dplyr::filter(dia==3,condicao=="contingente") %>%
  dplyr::group_by(grupo) %>% dplyr::summarise(media_cont = mean(total)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = grupo, y = media_cont),fill="purple") +
  #ggplot2::ggtitle("Média do tempo de vídeo na condição contingente") +
  ggplot2::labs(y = "Média de tempo (Dia 3)")+ggplot2::theme_minimal()+
  ggplot2::geom_label(ggplot2::aes(x = grupo, y = media_cont, label = round(media_cont,2)))

graf_cont1+graf_cont2+graf_cont3
