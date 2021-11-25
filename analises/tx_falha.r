library(magrittr)

# tempo entre vídeos
da_diff_tempo <- cea2::da_tidy %>%
  dplyr::group_by(grupo, nome, dia, condicao) %>%
  dplyr::arrange(grupo, nome, dia, condicao, tempo) %>%
  dplyr::filter(status_video %in% c("inicio", "fim")) %>%
  dplyr::mutate(diff_video = dplyr::case_when(
    status_video == "inicio" & dplyr::lag(status_video) == "fim"
    ~ (tempo - dplyr::lag(tempo))
  )) %>%
  dplyr::ungroup()

da_diff_tempo %>%
  dplyr::filter(condicao == "contingente", !is.na(diff_video)) %>%
  dplyr::filter(nome == "eduarda", grupo == "b1", dia == "1") %>%
  dplyr::mutate(id_video = dplyr::row_number()) %>%
  ggplot2::ggplot(ggplot2::aes(y = diff_video, x = id_video)) +
  ggplot2::geom_point()


# tempo até primeiro vídeo
da_1o_video <- da_diff_tempo %>%
  dplyr::filter(condicao == "contingente", video == TRUE) %>%
  dplyr::group_by(nome, grupo, dia) %>%
  dplyr::summarise(
    t_1o_video = dplyr::first(tempo),
    video = dplyr::first(video),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dia = as.factor(dia),
    grupo = as.factor(grupo),
    video = ifelse(video, 1, 0)
  )

# KM
ajuste <- survival::survfit(
  survival::Surv(time = t_1o_video, event = video) ~  dia,
  data = da_1o_video
)
survminer::ggsurvplot(ajuste)

# Por grupo

da_1o_video_grupo <- function(grupo){
  da_diff_tempo %>%
  dplyr::filter(grupo == grupo,condicao == "contingente", video == TRUE) %>%
  dplyr::group_by(nome, dia) %>%
  dplyr::summarise(
    t_1o_video = dplyr::first(tempo),
    video = dplyr::first(video),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dia = as.factor(dia),
    video = ifelse(video, 1, 0)
  )
}

ajuste1 <- survival::survfit(
  survival::Surv(time = t_1o_video, event = video) ~  dia,
  data = da_1o_video_grupo('b1')
)
survminer::ggsurvplot(ajuste1)

ajuste2 <- survival::survfit(
  survival::Surv(time = t_1o_video, event = video) ~  dia,
  data = da_1o_video_grupo('b2')
)
survminer::ggsurvplot(ajuste2)

ajuste3 <- survival::survfit(
  survival::Surv(time = t_1o_video, event = video) ~  dia,
  data = da_1o_video_grupo('b3')
)
survminer::ggsurvplot(ajuste3)

# bshazard
fit <- bshazard::bshazard(
  survival::Surv(t_1o_video, video) ~ dia + grupo, data = da_1o_video
)
plot(fit)


# weibulltools
dados <- weibulltools::reliability_data(
  data = da_1o_video,
  x = t_1o_video, status = video
)
weibulltools::estimate_cdf(dados)
