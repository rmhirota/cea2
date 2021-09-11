library(magrittr)

da_spss <- readr::read_rds("data-raw/da_spss_tidy.rds")
dplyr::glimpse(da_spss)

# Funções -----------------------------------------------------------------

# Média por agrupamento
media_grupo <- function(da, variavel, ...) {
  da %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      "media_{{variavel}}" := mean({{variavel}}),
      .groups = "drop"
    )
}
media_grupo(da_spss, n_apertos, dia, grupo)
media_grupo(da_spss, n_apertos, dia)
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
