library(tidyverse)
library(patchwork)
# Número de crianças em cada grupo

spss1 %>% group_by(Grupo) %>% count() %>% knitr::kable()

# Média de apertos na condição basal de cada grupo

spss1 %>% group_by(Grupo) %>% summarise(media = mean(bas_n_apertos)) %>% knitr::kable()
# Grupos 2 e 3 (2 e 3 meses) com médias de apertos maiores (18, 32, 39)

# Média de apertos na condição basal 2 de cada grupo

spss1 %>% group_by(Grupo) %>% summarise(media = mean(`@2bas_n_apertos`)) %>% knitr::kable()
# Grupos 2 e 3 (2 e 3 meses) com médias de apertos maiores (23, 35, 43)
# Aumento do número médio de apertos em relação a condição

# Média de tempo de vídeo por grupo e condiçaõ
# valores referentes a porcentagem do tempo da tentativa em que o vídeo esteve ativado.

graf_cont = spss3 %>% group_by(Grupo) %>% summarise(media_cont = mean(Cont), media_nc = mean(NC)) %>%
  ggplot() +
  geom_col(aes(x = Grupo, y = media_cont),fill="purple") +
  ggtitle("Média do tempo de vídeo na condição contingente") +
  labs(y = "Média de tempo")+theme_minimal()+
  geom_label(aes(x = Grupo, y = media_cont, label = round(media_cont,2)))

graf_nc = spss3 %>% group_by(Grupo) %>% summarise(media_cont = mean(Cont), media_nc = mean(NC)) %>%
  ggplot() +
  geom_col(aes(x = Grupo, y = media_nc),fill="gray") +
  ggtitle("Média do tempo de vídeo na condição não contingente") +
  labs(y = "Média de tempo")+theme_minimal()+
  geom_label(aes(x = Grupo, y = media_nc, label = round(media_nc,2)))

graf_cont + graf_nc

# No geral, na condição contingente a porcentagem do tempo média em que o vídeo esteve ativado.
# é superior à mesma métrica na condição não contingente

spss3 %>%  ggplot() + geom_point(aes(x = Cont, y = NC, colour = factor(Grupo), shape = factor(Grupo))) +
  theme_minimal() +
  ggtitle("Porcentagem do tempo média para cada condição") +
  labs(y = "Não Contingente", x = "Contingente")+theme_minimal()



# Variação da qtd de apertos por dia

# Dia 1

serie1 = spss1 %>% select(Grupo, bas_n_apertos,c_n_apertos,nc_n_apertos,pos_bas_n_apertos) %>%
  group_by(Grupo) %>%
  mutate(Grp = row_number()) %>%
  pivot_wider(names_from = "Grupo", values_from = c("bas_n_apertos","c_n_apertos","nc_n_apertos","pos_bas_n_apertos")) %>%
  select(-Grp)

xyplot(ts(serie1 %>% select(bas_n_apertos_1,c_n_apertos_1,nc_n_apertos_1,pos_bas_n_apertos_1)),superpose = TRUE,
       title(main = "Condições no dia 1 (Grupo 1)"))
xyplot(ts(serie1 %>% select(bas_n_apertos_2,c_n_apertos_2,nc_n_apertos_2,pos_bas_n_apertos_2)),superpose = TRUE,
       title(main = "Condições no dia 1 (Grupo 2)"))
xyplot(ts(serie1 %>% select(bas_n_apertos_3,c_n_apertos_3,nc_n_apertos_3,pos_bas_n_apertos_3)),superpose = TRUE,
                       title(main = "Condições no dia 1 (Grupo 3)"))

# Dia 2

serie2 = spss1 %>% select(Grupo,`@2bas_n_apertos`,`@2c_n_apertos`, `@2nc_n_apertos`,`@2pos_bas_n_apertos`) %>%
  group_by(Grupo) %>%
  mutate(Grp = row_number()) %>%
  pivot_wider(names_from = "Grupo", values_from = c("@2bas_n_apertos","@2c_n_apertos", "@2nc_n_apertos","@2pos_bas_n_apertos")) %>%
  select(-Grp)

xyplot(ts(serie2 %>% select(`@2bas_n_apertos_1`,`@2c_n_apertos_1`, `@2nc_n_apertos_1`,`@2pos_bas_n_apertos_1`)),superpose = TRUE,
       title(main = "Condições no dia 2 (Grupo 1)"))
xyplot(ts(serie2 %>% select(`@2bas_n_apertos_2`,`@2c_n_apertos_2`, `@2nc_n_apertos_2`,`@2pos_bas_n_apertos_2`)),superpose = TRUE,
          title(main = "Condições no dia 2 (Grupo 2)"))
xyplot(ts(serie2 %>% select(`@2bas_n_apertos_3`,`@2c_n_apertos_3`, `@2nc_n_apertos_3`,`@2pos_bas_n_apertos_3`)),superpose = TRUE,
          title(main = "Condições no dia 2 (Grupo 3)"))

# Dia 3

serie3 = spss1 %>% select(Grupo,`@3bas_n_apertos`,`@3c_n_apertos`, `@3nc_n_apertos`,`@3pos_bas_n_apertos`) %>%
  group_by(Grupo) %>%
  mutate(Grp = row_number()) %>%
  pivot_wider(names_from = "Grupo", values_from = c("@3bas_n_apertos","@3c_n_apertos", "@3nc_n_apertos","@3pos_bas_n_apertos")) %>%
  select(-Grp)

xyplot(ts(serie3 %>% select(`@3bas_n_apertos_1`,`@3c_n_apertos_1`, `@3nc_n_apertos_1`,`@3pos_bas_n_apertos_1`)),superpose = TRUE,
       title(main = "Condições no dia 3 (Grupo 1)"))
xyplot(ts(serie3 %>% select(`@3bas_n_apertos_2`,`@3c_n_apertos_2`, `@3nc_n_apertos_2`,`@3pos_bas_n_apertos_2`)),superpose = TRUE,
       title(main = "Condições no dia 3 (Grupo 2)"))
xyplot(ts(serie3 %>% select(`@3bas_n_apertos_3`,`@3c_n_apertos_3`, `@3nc_n_apertos_3`,`@3pos_bas_n_apertos_3`)),superpose = TRUE,
       title(main = "Condições no dia 3 (Grupo 3)"))
#

# Variação do pico de aperto por dia

# Dia 1

pico1 = spss1 %>% select(Grupo, bas_pico,c_pico,nc_pico,pos_bas_pico) %>%
  group_by(Grupo) %>%
  mutate(Grp = row_number()) %>%
  pivot_wider(names_from = "Grupo", values_from = c("bas_pico","c_pico","nc_pico","pos_bas_pico")) %>%
  select(-Grp)

xyplot(ts(pico1 %>% select(bas_pico_1,c_pico_1,nc_pico_1,pos_bas_pico_1)),superpose = TRUE,
       title(main = "Condições no dia 1 (Grupo 1)"))
xyplot(ts(pico1 %>% select(bas_pico_2,c_pico_2,nc_pico_2,pos_bas_pico_2)),superpose = TRUE,
       title(main = "Condições no dia 1 (Grupo 2)"))
xyplot(ts(pico1 %>% select(bas_pico_3,c_pico_3,nc_pico_3,pos_bas_pico_3)),superpose = TRUE,
       title(main = "Condições no dia 1 (Grupo 3)"))

# Dia 2

pico2 = spss1 %>% select(Grupo,`@2bas_pico`,`@2c_pico`, `@2nc_pico`,`@2pos_bas_pico`) %>%
  group_by(Grupo) %>%
  mutate(Grp = row_number()) %>%
  pivot_wider(names_from = "Grupo", values_from = c("@2bas_pico","@2c_pico", "@2nc_pico","@2pos_bas_pico")) %>%
  select(-Grp)

xyplot(ts(pico2 %>% select(`@2bas_pico_1`,`@2c_pico_1`, `@2nc_pico_1`,`@2pos_bas_pico_1`)),superpose = TRUE,
       title(main = "Condições no dia 2 (Grupo 1)"))
xyplot(ts(pico2 %>% select(`@2bas_pico_2`,`@2c_pico_2`, `@2nc_pico_2`,`@2pos_bas_pico_2`)),superpose = TRUE,
       title(main = "Condições no dia 2 (Grupo 2)"))
xyplot(ts(pico2 %>% select(`@2bas_pico_3`,`@2c_pico_3`, `@2nc_pico_3`,`@2pos_bas_pico_3`)),superpose = TRUE,
       title(main = "Condições no dia 2 (Grupo 3)"))

# Dia 3

pico3 = spss1 %>% select(Grupo,`@3bas_pico`,`@3c_pico`, `@3nc_pico`,`@3pos_bas_pico`) %>%
  group_by(Grupo) %>%
  mutate(Grp = row_number()) %>%
  pivot_wider(names_from = "Grupo", values_from = c("@3bas_pico","@3c_pico", "@3nc_pico","@3pos_bas_pico")) %>%
  select(-Grp)

xyplot(ts(pico3 %>% select(`@3bas_pico_1`,`@3c_pico_1`, `@3nc_pico_1`,`@3pos_bas_pico_1`)),superpose = TRUE,
       title(main = "Condições no dia 3 (Grupo 1)"))
xyplot(ts(pico3 %>% select(`@3bas_pico_2`,`@3c_pico_2`, `@3nc_pico_2`,`@3pos_bas_pico_2`)),superpose = TRUE,
       title(main = "Condições no dia 3 (Grupo 2)"))
xyplot(ts(pico3 %>% select(`@3bas_pico_3`,`@3c_pico_3`, `@3nc_pico_3`,`@3pos_bas_pico_3`)),superpose = TRUE,
       title(main = "Condições no dia 3 (Grupo 3)"))
#
