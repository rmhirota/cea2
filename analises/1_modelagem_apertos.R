# Tempo até aperto ------------------------------------------------------

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

# Fixamos o dia 1 para análise --------------------------------------------------------------

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

# Teste da exponencialização da variavel tempo até aperto

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
