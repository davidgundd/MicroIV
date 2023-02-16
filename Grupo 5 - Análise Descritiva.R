#Importando as bibliotecas

library(tidyverse)
library(dplyr)
library(readxl)
library(plotly)
library(ggridges)

#Importando as bases de dados

dados <- read_excel("Dados_UPP.xlsx")
dados_pop <- read_excel("PopulacaoResidenteUpp2010.xlsx")

view(dados)
view(dados_pop)

#Adicionando a população das UPPs de "dados_pop" para a base "dados":

dados <- dados %>% 
  left_join(dados_pop)

view(dados)

#Filtrando apenas as UPPs fechadas 

upps_fechadas <- dados %>% 
  filter(cod_upp %in% c("2", "3", "14","16","32","33","36","37")) %>% 
  mutate(A_F="fechada")

#Filtrando apenas as UPPs abertas

`%not_in%` <- purrr::negate(`%in%`)

upps_abertas <- dados %>% 
  filter(cod_upp %not_in% c("2", "3", "14","16","32","33","36","37")) %>% 
  mutate(A_F="aberta")

#Agregando novamente as duas tabelas

dados1 <- full_join(upps_abertas,upps_fechadas)

dados1 <- dados1 %>% 
  arrange(cod_upp)

#Rearranjando as colunas das tabelas

dados1 <- dados1 %>% 
  select(cod_upp,upp,A_F,pop_2010,ano:registro_ocorrencias)

#Registro de ocorrencias totais registradas em todas as UPPs no período

ocorrencias_tot <- dados1 %>% 
  filter(ano<2020) %>% 
  group_by(ano) %>% 
  summarise(ocorrencias_tot_ano = sum(registro_ocorrencias)) %>% 
  ggplot() +
  geom_line(mapping = aes(ano,ocorrencias_tot_ano),color = "darkred",
            size = 1) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 12) +
  labs(x = "",
       y = "Número de ocorrências",
       title = 
       "Evolução das ocorrências totais nas comunidades com UPPs") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA))

show(ocorrencias_tot)

ggplotly(ocorrencias_tot, tooltip = "ocorrencias_tot_ano")

#Registro dos roubos de veículos totais em todas as UPPs no período

roubos_veic_tot <- dados1 %>% 
  filter(ano<2020) %>% 
  group_by(ano) %>% 
  summarise(roubos_veic_tot_ano = sum(roubo_veiculo)) %>% 
  ggplot() +
  geom_line(mapping = aes(ano,roubos_veic_tot_ano),color = "darkblue", 
            size = 1) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 12) +
  labs(x = "",
       y = "Número de roubos de veículos",
       title = 
       "Evolução dos roubos de veículos totais nas comunidades com UPPs") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

show(roubos_veic_tot)

ggplotly(roubos_veic_tot,tooltip = "roubos_veic_tot_ano")

#Gráfico de linha das ocorrências nas UPPs abertas controlado pela população

graf_abertas <- upps_abertas %>%
  filter(ano<2020) %>% 
  group_by(ano,upp) %>% 
  summarise(ocorrencias_tot_ano_upp = sum(registro_ocorrencias)
            /pop_2010*1000) %>% 
  ggplot(mapping = aes(ano, ocorrencias_tot_ano_upp, group=upp, color=upp)) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 12) +
  labs(x = "",
       y = "Ocorrências totais / população (por mil habitantes)",
       title = 
       "Evolução das ocorrências totais nas comunidades com UPPs abertas") +
  geom_line() +
  geom_vline(xintercept = 2014, linetype = "dashed",
             color = "blue") +
  geom_vline(xintercept = 2018, linetype = "dotdash",
             color = "red") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent",
                                       color = NA),
        panel.background = element_rect(fill = "transparent", 
                                        color = NA),
        legend.background = element_rect(fill = "transparent", 
                                         color = NA))

show(graf_abertas)

ggplotly(graf_abertas,tooltip = "ocorrencias_tot_ano_upp")

#Gráfico de linha dos roubos de carro total nas UPPs fechadas 
#controlado por população

graf_fechadas_carro <- upps_fechadas %>% 
  filter(ano<2020) %>% 
  group_by(ano,upp) %>% 
  summarise(rouboscarro_tot_ano = sum(roubo_veiculo)/pop_2010*1000) %>% 
  ggplot(mapping = aes(ano,rouboscarro_tot_ano,group=upp,color=upp)) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 12) +
  geom_line(size = 0.8) +
  labs(x = "",
       y = "Roubos de veículos (por mil habitantes)",
       title = 
       "Evolução dos roubos de veículos nas comunidades com UPPs fechadas") +
  geom_vline(xintercept = c(2014,2018), linetype = c("dashed","dotdash"),
             color = c("blue","red")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

ggplotly(graf_fechadas_carro, tooltip = c("rouboscarro_tot_ano","upp"))

show(graf_fechadas_carro)

#Gráfico de linha dos roubos de carros totais nas UPPs abertas 
#controlado por população

graf_abertas_carro <- upps_abertas %>%
  filter(ano<2020) %>% 
  group_by(ano,upp) %>% 
  summarise(rouboscarro_tot_ano = sum(roubo_veiculo)/pop_2010*1000) %>% 
  ggplot(mapping = aes(ano,rouboscarro_tot_ano,group=upp,color=upp)) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 12) +
  labs(x = "",
       y = "Roubos de veículo (por mil habitantes)",
       title = 
       "Evolução dos roubos de veículos nas comunidades com UPPs abertas") +
  geom_line() +
  geom_vline(xintercept = 2014, linetype = "dashed",
             color = "blue") +
  geom_vline(xintercept = 2018, linetype = "dotdash",
             color = "red") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent",
                                       color = NA),
        panel.background = element_rect(fill = "transparent", 
                                        color = NA),
        legend.background = element_rect(fill = "transparent", 
                                         color = NA))

show(graf_abertas_carro)

ggplotly(graf_abertas_carro,tooltip = c("rouboscarro_tot_ano","upp"))

#Gráfico de linha das ocorrências totais das UPPs abertas versus UPPs 
#fechadas

graf_compar_ocorr <- dados1 %>%
  filter(ano < 2020) %>% 
  group_by(ano, A_F) %>%
  summarise(ocorrencias_tot_af = 
              sum(registro_ocorrencias)/sum(pop_2010)*1000) %>%
  ggplot(mapping = aes(ano, ocorrencias_tot_af, color=A_F)) +
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  scale_x_continuous(n.breaks = 12) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "",x = , y = "Ocorrências totais (por mil habitantes)",
       title = "Evolução das ocorrências totais nas comunidades com UPPs") +
  theme_bw() +
  geom_vline(xintercept = 2014, linetype = "dashed",
             color = "blue") +
  geom_vline(xintercept = 2018, linetype = "dotdash",
             color = "red") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent",
                                       color = NA),
        panel.background = element_rect(fill = "transparent", 
                                        color = NA),
        legend.background = element_rect(fill = "transparent", 
                                         color = NA))
show(graf_compar_ocorr)

ggplotly(graf_compar_ocorr,tooltip = "ocorrencias_tot_af")

#Gráfico de linhas das apreensões de drogas das UPPs abertas versus UPPs 
#fechadas

graf_compar_drog <- dados1 %>%
  filter(ano < 2020) %>% 
  group_by(ano, A_F) %>%
  summarise(apreensao_drogas_tot = 
              sum(apreensao_drogas)/sum(pop_2010)*1000) %>%
  ggplot(mapping = aes(ano, apreensao_drogas_tot, color=A_F)) +
  theme(legend.position = "top") %>% 
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  scale_x_continuous(n.breaks = 12) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = "", y = "Apreensões de drogas totais (por mil habitantes)",
       title = 
       "Evolução das apreensões de drogas totais nas comunidades com UPPs") +
  theme_bw() +
  geom_vline(xintercept = 2014, linetype = "dashed",
             color = "blue") +
  geom_vline(xintercept = 2018, linetype = "dotdashed",
             color = "red") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent",
                                       color = NA),
        panel.background = element_rect(fill = "transparent", 
                                        color = NA),
        legend.background = element_rect(fill = "transparent", 
                                         color = NA))
show(graf_compar_drog)

ggplotly(graf_compar_drog,tooltip = "graf_compar_drog")

#Gráfico de linha dos roubos de veículos totais das UPPs abertas versus
#UPPs fechadas

graf_compar_veic <- dados1 %>%
  filter(ano < 2020) %>% 
   group_by(ano, A_F) %>%
   summarise(roubo_veic_tot = sum(roubo_veiculo)/sum(pop_2010)*1000) %>%
   ggplot(mapping = aes(ano, roubo_veic_tot, color=A_F, group = A_F)) +
   geom_point(size = 3) +
   geom_line(size = 1) +
   scale_x_continuous(n.breaks = 12) +
   scale_y_continuous(n.breaks = 10) +
   labs(x = "",
        y = "Roubos de veículo (por mil habitantes)",
        title = "Evolução dos roubos de veículos nas comunidades com UPPs") +
   theme_bw() +
  geom_vline(xintercept = 2014, linetype = "dashed",
             color = "blue") +
  geom_vline(xintercept = 2018, linetype = "dotdash",
             color = "red") +
   theme(axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 14),
         panel.grid = element_blank(), 
         plot.background = element_rect(fill = "transparent",
                                        color = NA),
         panel.background = element_rect(fill = "transparent", 
                                         color = NA),
         legend.background = element_rect(fill = "transparent", 
                                          color = NA))

show(graf_compar_veic)

ggplotly(graf_compar_veic,tooltip = "roubo_veic_tot")

