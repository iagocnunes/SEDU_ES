library(Hmisc)
library(tidyverse)
library(openxlsx)
library(haven)
library(data.table)
library(lubridate)

options(scipen=999)
memory.limit(size = 999999999999)
memory.limit()
rm(list = ls())

########## MATRICULA_20`xx` ############
censo_2020_1<-read.xlsx('./censo_2020/MATRICULAS_ES_2020_2.xlsx', sheet = 1, colNames = T, skipEmptyRows = T, skipEmptyCols = T)

colnames(censo_2020_1)

censo_2020_1 <- subset(censo_2020_1, TP_ETAPA_ENSINO==4|TP_ETAPA_ENSINO==5|TP_ETAPA_ENSINO==6|TP_ETAPA_ENSINO==7|TP_ETAPA_ENSINO==8|
                         TP_ETAPA_ENSINO==9|TP_ETAPA_ENSINO==10|TP_ETAPA_ENSINO==11|TP_ETAPA_ENSINO==14|TP_ETAPA_ENSINO==15|TP_ETAPA_ENSINO==16|
                         TP_ETAPA_ENSINO==17|TP_ETAPA_ENSINO==18|TP_ETAPA_ENSINO==19|TP_ETAPA_ENSINO==20|TP_ETAPA_ENSINO==21|TP_ETAPA_ENSINO==25|
                         TP_ETAPA_ENSINO==26|TP_ETAPA_ENSINO==27|TP_ETAPA_ENSINO==28|TP_ETAPA_ENSINO==29|TP_ETAPA_ENSINO==30|TP_ETAPA_ENSINO==31|
                         TP_ETAPA_ENSINO==32|TP_ETAPA_ENSINO==33|TP_ETAPA_ENSINO==34|TP_ETAPA_ENSINO==35|TP_ETAPA_ENSINO==36|TP_ETAPA_ENSINO==37|
                         TP_ETAPA_ENSINO==38|TP_ETAPA_ENSINO==41)
censo_2020_1 <- subset(censo_2020_1, TP_DEPENDENCIA==2)
describe(censo_2020_1$CO_PESSOA_FISICA)

colnames(censo_2020_1)

dup_MATRICULA_2020 <- censo_2020_1[duplicated(censo_2020_1[, 4]) | duplicated(censo_2020_1[, 4], fromLast=T),]
describe(dup_MATRICULA_2020$CO_PESSOA_FISICA)

NOdup_MATRICULA_2020 <- censo_2020_1 %>%  filter(!CO_PESSOA_FISICA %in% dup_MATRICULA_2020$CO_PESSOA_FISICA)
describe(NOdup_MATRICULA_2020$CO_PESSOA_FISICA)

######## todas as duplicações ocorrem duas vezes, mudança de escola, munic ou turma, que geram outras mudanças em outras colunas

dup_MATRICULA_2020 <- dup_MATRICULA_2020 %>% group_by(CO_PESSOA_FISICA) %>% mutate(id = row_number()) %>% 
  ungroup

dup1_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(CO_ENTIDADE, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CO_ENTIDADE) %>% 
  fill('1_CO_ENTIDADE', '2_CO_ENTIDADE', .direction = "downup") %>% 
  rename(escola1='1_CO_ENTIDADE',escola2='2_CO_ENTIDADE') %>% 
  ungroup

dup2_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(CO_MUNICIPIO, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CO_MUNICIPIO) %>% 
  fill('1_CO_MUNICIPIO', '2_CO_MUNICIPIO', .direction = "downup") %>% 
  rename(munic1='1_CO_MUNICIPIO',munic2='2_CO_MUNICIPIO') %>% 
  ungroup

dup3_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(ID_TURMA, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURMA) %>% 
  fill('1_ID_TURMA', '2_ID_TURMA', .direction = "downup") %>% 
  rename(turma1='1_ID_TURMA',turma2='2_ID_TURMA') %>% 
  ungroup

dup4_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(CO_UF_END, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CO_UF_END) %>% 
  fill('1_CO_UF_END', '2_CO_UF_END', .direction = "downup") %>% 
  rename(uf_end1='1_CO_UF_END',uf_end2='2_CO_UF_END') %>% 
  ungroup

dup5_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(CO_MUNICIPIO_END, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CO_MUNICIPIO_END) %>% 
  fill('1_CO_MUNICIPIO_END', '2_CO_MUNICIPIO_END', .direction = "downup") %>% 
  rename(munic_end1='1_CO_MUNICIPIO_END',munic_end2='2_CO_MUNICIPIO_END') %>% 
  ungroup

dup6_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(TP_ZONA_RESIDENCIAL, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = TP_ZONA_RESIDENCIAL) %>% 
  fill('1_TP_ZONA_RESIDENCIAL', '2_TP_ZONA_RESIDENCIAL', .direction = "downup") %>% 
  rename(res_zon1='1_TP_ZONA_RESIDENCIAL',res_zon2='2_TP_ZONA_RESIDENCIAL') %>% 
  ungroup

dup7_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(TP_LOCAL_RESID_DIFERENCIADA, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = TP_LOCAL_RESID_DIFERENCIADA) %>% 
  fill('1_TP_LOCAL_RESID_DIFERENCIADA', '2_TP_LOCAL_RESID_DIFERENCIADA', .direction = "downup") %>% 
  rename(res_dif1='1_TP_LOCAL_RESID_DIFERENCIADA',res_dif2='2_TP_LOCAL_RESID_DIFERENCIADA') %>% 
  ungroup

dup8_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(TP_OUTRO_LOCAL_AULA, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = TP_OUTRO_LOCAL_AULA) %>% 
  fill('1_TP_OUTRO_LOCAL_AULA', '2_TP_OUTRO_LOCAL_AULA', .direction = "downup") %>% 
  rename(loc_aula1='1_TP_OUTRO_LOCAL_AULA',loc_aula2='2_TP_OUTRO_LOCAL_AULA') %>% 
  ungroup

dup9_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(IN_TRANSPORTE_PUBLICO, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = IN_TRANSPORTE_PUBLICO) %>% 
  fill('1_IN_TRANSPORTE_PUBLICO', '2_IN_TRANSPORTE_PUBLICO', .direction = "downup") %>% 
  rename(transp1='1_IN_TRANSPORTE_PUBLICO',transp2='2_IN_TRANSPORTE_PUBLICO') %>% 
  ungroup

dup10_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(IN_REGULAR, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = IN_REGULAR) %>% 
  fill('1_IN_REGULAR', '2_IN_REGULAR', .direction = "downup") %>% 
  rename(regular1='1_IN_REGULAR',regular2='2_IN_REGULAR') %>% 
  ungroup

dup11_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(IN_EJA, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = IN_EJA) %>% 
  fill('1_IN_EJA', '2_IN_EJA', .direction = "downup") %>% 
  rename(eja1='1_IN_EJA',eja2='2_IN_EJA') %>% 
  ungroup

dup12_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(IN_PROFISSIONALIZANTE, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = IN_PROFISSIONALIZANTE) %>% 
  fill('1_IN_PROFISSIONALIZANTE', '2_IN_PROFISSIONALIZANTE', .direction = "downup") %>% 
  rename(profiss1='1_IN_PROFISSIONALIZANTE',profiss2='2_IN_PROFISSIONALIZANTE') %>% 
  ungroup

dup13_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(TP_MEDIACAO_DIDATICO_PEDAGO, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = TP_MEDIACAO_DIDATICO_PEDAGO) %>% 
  fill('1_TP_MEDIACAO_DIDATICO_PEDAGO', '2_TP_MEDIACAO_DIDATICO_PEDAGO', .direction = "downup") %>% 
  rename(mediacao1='1_TP_MEDIACAO_DIDATICO_PEDAGO',mediacao2='2_TP_MEDIACAO_DIDATICO_PEDAGO') %>% 
  ungroup

dup14_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(NU_DURACAO_TURMA, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NU_DURACAO_TURMA) %>% 
  fill('1_NU_DURACAO_TURMA', '2_NU_DURACAO_TURMA', .direction = "downup") %>% 
  rename(duracao1='1_NU_DURACAO_TURMA',duracao2='2_NU_DURACAO_TURMA') %>% 
  ungroup

dup15_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(NU_DUR_ATIV_COMP_MESMA_REDE, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NU_DUR_ATIV_COMP_MESMA_REDE) %>% 
  fill('1_NU_DUR_ATIV_COMP_MESMA_REDE', '2_NU_DUR_ATIV_COMP_MESMA_REDE', .direction = "downup") %>% 
  rename(dur_atvmsm1='1_NU_DUR_ATIV_COMP_MESMA_REDE',dur_atvmsm2='2_NU_DUR_ATIV_COMP_MESMA_REDE') %>% 
  ungroup

dup16_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(NU_DUR_ATIV_COMP_OUTRAS_REDES, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NU_DUR_ATIV_COMP_OUTRAS_REDES) %>% 
  fill('1_NU_DUR_ATIV_COMP_OUTRAS_REDES', '2_NU_DUR_ATIV_COMP_OUTRAS_REDES', .direction = "downup") %>% 
  rename(dur_atvotr1='1_NU_DUR_ATIV_COMP_OUTRAS_REDES',dur_atvotr2='2_NU_DUR_ATIV_COMP_OUTRAS_REDES') %>% 
  ungroup

dup17_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(NU_DUR_AEE_MESMA_REDE, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NU_DUR_AEE_MESMA_REDE) %>% 
  fill('1_NU_DUR_AEE_MESMA_REDE', '2_NU_DUR_AEE_MESMA_REDE', .direction = "downup") %>% 
  rename(dur_AEEmsm1='1_NU_DUR_AEE_MESMA_REDE',dur_AEEmsm2='2_NU_DUR_AEE_MESMA_REDE') %>% 
  ungroup

dup18_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(NU_DUR_AEE_OUTRAS_REDES, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NU_DUR_AEE_OUTRAS_REDES) %>% 
  fill('1_NU_DUR_AEE_OUTRAS_REDES', '2_NU_DUR_AEE_OUTRAS_REDES', .direction = "downup") %>% 
  rename(dur_AEEotr1='1_NU_DUR_AEE_OUTRAS_REDES',dur_AEEotr2='2_NU_DUR_AEE_OUTRAS_REDES') %>% 
  ungroup

dup19_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(NU_DIAS_ATIVIDADE, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NU_DIAS_ATIVIDADE) %>% 
  fill('1_NU_DIAS_ATIVIDADE', '2_NU_DIAS_ATIVIDADE', .direction = "downup") %>% 
  rename(dias_atv1='1_NU_DIAS_ATIVIDADE',dias_atv2='2_NU_DIAS_ATIVIDADE') %>% 
  ungroup

dup20_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(TP_UNIFICADA, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = TP_UNIFICADA) %>% 
  fill('1_TP_UNIFICADA', '2_TP_UNIFICADA', .direction = "downup") %>% 
  rename(unif1='1_TP_UNIFICADA',unif2='2_TP_UNIFICADA') %>% 
  ungroup

dup21_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(TP_TIPO_ATENDIMENTO_TURMA, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = TP_TIPO_ATENDIMENTO_TURMA) %>% 
  fill('1_TP_TIPO_ATENDIMENTO_TURMA', '2_TP_TIPO_ATENDIMENTO_TURMA', .direction = "downup") %>% 
  rename(tipo_atend1='1_TP_TIPO_ATENDIMENTO_TURMA',tipo_atend2='2_TP_TIPO_ATENDIMENTO_TURMA') %>% 
  ungroup

dup22_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(TP_TIPO_LOCAL_TURMA, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = TP_TIPO_LOCAL_TURMA) %>% 
  fill('1_TP_TIPO_LOCAL_TURMA', '2_TP_TIPO_LOCAL_TURMA', .direction = "downup") %>% 
  rename(loc_turm1='1_TP_TIPO_LOCAL_TURMA',loc_turm2='2_TP_TIPO_LOCAL_TURMA') %>% 
  ungroup

dup23_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(CO_REGIAO, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CO_REGIAO) %>% 
  fill('1_CO_REGIAO', '2_CO_REGIAO', .direction = "downup") %>% 
  rename(cod_reg1='1_CO_REGIAO',cod_reg2='2_CO_REGIAO') %>% 
  ungroup

dup24_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(CO_MESORREGIAO, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CO_MESORREGIAO) %>% 
  fill('1_CO_MESORREGIAO', '2_CO_MESORREGIAO', .direction = "downup") %>% 
  rename(cod_meso1='1_CO_MESORREGIAO',cod_meso2='2_CO_MESORREGIAO') %>% 
  ungroup

dup25_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(CO_MICRORREGIAO, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CO_MICRORREGIAO) %>% 
  fill('1_CO_MICRORREGIAO', '2_CO_MICRORREGIAO', .direction = "downup") %>% 
  rename(cod_micr1='1_CO_MICRORREGIAO',cod_micr2='2_CO_MICRORREGIAO') %>% 
  ungroup

dup26_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(CO_DISTRITO, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CO_DISTRITO) %>% 
  fill('1_CO_DISTRITO', '2_CO_DISTRITO', .direction = "downup") %>% 
  rename(cod_dist1='1_CO_DISTRITO',cod_dist2='2_CO_DISTRITO') %>% 
  ungroup

dup27_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(TP_LOCALIZACAO, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = TP_LOCALIZACAO) %>% 
  fill('1_TP_LOCALIZACAO', '2_TP_LOCALIZACAO', .direction = "downup") %>% 
  rename(urb_rur1='1_TP_LOCALIZACAO',urb_rur2='2_TP_LOCALIZACAO') %>% 
  ungroup

dup28_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(TP_LOCALIZACAO_DIFERENCIADA, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = TP_LOCALIZACAO_DIFERENCIADA) %>% 
  fill('1_TP_LOCALIZACAO_DIFERENCIADA', '2_TP_LOCALIZACAO_DIFERENCIADA', .direction = "downup") %>% 
  rename(loc_dif1='1_TP_LOCALIZACAO_DIFERENCIADA',loc_dif2='2_TP_LOCALIZACAO_DIFERENCIADA') %>% 
  ungroup

dup29_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  select(IN_EDUCACAO_INDIGENA, CO_PESSOA_FISICA, id) %>% 
  group_by(CO_PESSOA_FISICA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = IN_EDUCACAO_INDIGENA) %>% 
  fill('1_IN_EDUCACAO_INDIGENA', '2_IN_EDUCACAO_INDIGENA', .direction = "downup") %>% 
  rename(ed_indg1='1_IN_EDUCACAO_INDIGENA',ed_indg2='2_IN_EDUCACAO_INDIGENA') %>% 
  ungroup

dup1_2X_MATRICULA_2020 <- dup1_2X_MATRICULA_2020 %>%
  mutate(switch_escola=ifelse(escola1!=escola2, 1, 0))

dup2_2X_MATRICULA_2020 <- dup2_2X_MATRICULA_2020 %>%
  mutate(switch_munic=ifelse(munic1!=munic2, 1, 0))

dup3_2X_MATRICULA_2020 <- dup3_2X_MATRICULA_2020 %>%
  mutate(switch_turma=ifelse(turma1!=turma2, 1, 0))

dup4_2X_MATRICULA_2020 <- dup4_2X_MATRICULA_2020 %>%
  mutate(switch_uf_end=ifelse(uf_end1!=uf_end2, 1, 0))

dup5_2X_MATRICULA_2020 <- dup5_2X_MATRICULA_2020 %>%
  mutate(switch_munic_end=ifelse(munic_end1!=munic_end2, 1, 0))

dup6_2X_MATRICULA_2020 <- dup6_2X_MATRICULA_2020 %>%
  mutate(switch_res_zon=ifelse(res_zon1!=res_zon2, 1, 0))

dup7_2X_MATRICULA_2020 <- dup7_2X_MATRICULA_2020 %>%
  mutate(switch_res_dif=ifelse(res_dif1!=res_dif2, 1, 0))

dup8_2X_MATRICULA_2020 <- dup8_2X_MATRICULA_2020 %>%
  mutate(switch_loc_aula=ifelse(loc_aula1!=loc_aula2, 1, 0))

dup9_2X_MATRICULA_2020 <- dup9_2X_MATRICULA_2020 %>%
  mutate(switch_transp=ifelse(transp1!=transp2, 1, 0))

dup10_2X_MATRICULA_2020 <- dup10_2X_MATRICULA_2020 %>%
  mutate(switch_regular=ifelse(regular1!=regular2, 1, 0))

dup11_2X_MATRICULA_2020 <- dup11_2X_MATRICULA_2020 %>%
  mutate(switch_eja=ifelse(eja1!=eja2, 1, 0))

dup12_2X_MATRICULA_2020 <- dup12_2X_MATRICULA_2020 %>%
  mutate(switch_profiss=ifelse(profiss1!=profiss2, 1, 0))

dup13_2X_MATRICULA_2020 <- dup13_2X_MATRICULA_2020 %>%
  mutate(switch_mediacao=ifelse(mediacao1!=mediacao2, 1, 0))

dup14_2X_MATRICULA_2020 <- dup14_2X_MATRICULA_2020 %>%
  mutate(switch_duracao=ifelse(duracao1!=duracao2, 1, 0))

dup15_2X_MATRICULA_2020 <- dup15_2X_MATRICULA_2020 %>%
  mutate(switch_dur_atvmsm=ifelse(dur_atvmsm1!=dur_atvmsm2, 1, 0))

dup16_2X_MATRICULA_2020 <- dup16_2X_MATRICULA_2020 %>%
  mutate(switch_dur_atvotr=ifelse(dur_atvotr1!=dur_atvotr2, 1, 0))

dup17_2X_MATRICULA_2020 <- dup17_2X_MATRICULA_2020 %>%
  mutate(switch_dur_AEEmsm=ifelse(dur_AEEmsm1!=dur_AEEmsm2, 1, 0))

dup18_2X_MATRICULA_2020 <- dup18_2X_MATRICULA_2020 %>%
  mutate(switch_dur_AEEotr=ifelse(dur_AEEotr1!=dur_AEEotr2, 1, 0))

dup19_2X_MATRICULA_2020 <- dup19_2X_MATRICULA_2020 %>%
  mutate(switch_dias_atv=ifelse(dias_atv1!=dias_atv2, 1, 0))

dup20_2X_MATRICULA_2020 <- dup20_2X_MATRICULA_2020 %>%
  mutate(switch_unif=ifelse(unif1!=unif2, 1, 0))

dup21_2X_MATRICULA_2020 <- dup21_2X_MATRICULA_2020 %>%
  mutate(switch_tipo_atend=ifelse(tipo_atend1!=tipo_atend2, 1, 0))

dup22_2X_MATRICULA_2020 <- dup22_2X_MATRICULA_2020 %>%
  mutate(switch_loc_turm=ifelse(loc_turm1!=loc_turm2, 1, 0))

dup23_2X_MATRICULA_2020 <- dup23_2X_MATRICULA_2020 %>%
  mutate(switch_cod_reg=ifelse(cod_reg1!=cod_reg2, 1, 0))

dup24_2X_MATRICULA_2020 <- dup24_2X_MATRICULA_2020 %>%
  mutate(switch_cod_meso=ifelse(cod_meso1!=cod_meso2, 1, 0))

dup25_2X_MATRICULA_2020 <- dup25_2X_MATRICULA_2020 %>%
  mutate(switch_cod_micr=ifelse(cod_micr1!=cod_micr2, 1, 0))

dup26_2X_MATRICULA_2020 <- dup26_2X_MATRICULA_2020 %>%
  mutate(switch_cod_dist=ifelse(cod_dist1!=cod_dist2, 1, 0))

dup27_2X_MATRICULA_2020 <- dup27_2X_MATRICULA_2020 %>%
  mutate(switch_urb_rur=ifelse(urb_rur1!=urb_rur2, 1, 0))

dup28_2X_MATRICULA_2020 <- dup28_2X_MATRICULA_2020 %>%
  mutate(switch_loc_dif=ifelse(loc_dif1!=loc_dif2, 1, 0))

dup29_2X_MATRICULA_2020 <- dup29_2X_MATRICULA_2020 %>%
  mutate(switch_ed_indg=ifelse(ed_indg1!=ed_indg2, 1, 0))

dup_2X_MATRICULA_2020 <- dup1_2X_MATRICULA_2020 %>% 
  left_join(dup2_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup3_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup4_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup5_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup6_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup7_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup8_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup9_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup10_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup11_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup12_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup13_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup14_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup15_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup16_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup17_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup18_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup19_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup20_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup21_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup22_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup23_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup24_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup25_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup26_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup27_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup28_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(dup29_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))
dup_2X_MATRICULA_2020 <- dup_MATRICULA_2020 %>% 
  left_join(dup_2X_MATRICULA_2020, by = c("CO_PESSOA_FISICA" = "CO_PESSOA_FISICA"))

rm(dup1_2X_MATRICULA_2020, dup2_2X_MATRICULA_2020, dup3_2X_MATRICULA_2020, dup4_2X_MATRICULA_2020, dup5_2X_MATRICULA_2020, 
   dup6_2X_MATRICULA_2020, dup7_2X_MATRICULA_2020, dup8_2X_MATRICULA_2020, dup9_2X_MATRICULA_2020, dup10_2X_MATRICULA_2020,
   dup11_2X_MATRICULA_2020, dup12_2X_MATRICULA_2020, dup13_2X_MATRICULA_2020,dup14_2X_MATRICULA_2020,dup15_2X_MATRICULA_2020,dup16_2X_MATRICULA_2020,
   dup17_2X_MATRICULA_2020,dup18_2X_MATRICULA_2020,dup19_2X_MATRICULA_2020,dup20_2X_MATRICULA_2020,dup21_2X_MATRICULA_2020,dup22_2X_MATRICULA_2020,
   dup23_2X_MATRICULA_2020,dup24_2X_MATRICULA_2020,dup25_2X_MATRICULA_2020,dup26_2X_MATRICULA_2020,dup27_2X_MATRICULA_2020,dup28_2X_MATRICULA_2020,
   dup29_2X_MATRICULA_2020)

NOdup_MATRICULA_2020 <- NOdup_MATRICULA_2020 %>% group_by(CO_PESSOA_FISICA) %>% mutate(id = row_number())
describe(NOdup_MATRICULA_2020$id)

NOdup_MATRICULA_2020 <- NOdup_MATRICULA_2020 %>%
  mutate(switch_escola=ifelse(!is.na(CO_ENTIDADE), 0, as.numeric(NA)))

NOdup_MATRICULA_2020 <- NOdup_MATRICULA_2020 %>%
  mutate(switch_uf_end=ifelse(!is.na(CO_UF_END), 0, as.numeric(NA)),
         switch_res_dif=ifelse(!is.na(TP_LOCAL_RESID_DIFERENCIADA), 0, as.numeric(NA)),
         switch_munic_end=ifelse(!is.na(CO_MUNICIPIO_END), 0, as.numeric(NA)))

NOdup_MATRICULA_2020 <- NOdup_MATRICULA_2020 %>%
  mutate(switch_munic=ifelse(!is.na(CO_MUNICIPIO), 0, as.numeric(NA)),
         switch_turma=ifelse(!is.na(ID_TURMA), 0, as.numeric(NA)),
         switch_res_zon=ifelse(!is.na(TP_ZONA_RESIDENCIAL), 0, as.numeric(NA)),
         switch_loc_aula=ifelse(!is.na(TP_OUTRO_LOCAL_AULA), 0, as.numeric(NA)),
         switch_transp=ifelse(!is.na(IN_TRANSPORTE_PUBLICO), 0, as.numeric(NA)),
         switch_regular=ifelse(!is.na(IN_REGULAR), 0, as.numeric(NA)),
         switch_eja=ifelse(!is.na(IN_EJA), 0, as.numeric(NA)),
         switch_profiss=ifelse(!is.na(IN_PROFISSIONALIZANTE), 0, as.numeric(NA)),
         switch_mediacao=ifelse(!is.na(TP_MEDIACAO_DIDATICO_PEDAGO), 0, as.numeric(NA)),
         switch_duracao=ifelse(!is.na(NU_DURACAO_TURMA), 0, as.numeric(NA)),
         switch_dur_atvmsm=ifelse(!is.na(NU_DUR_ATIV_COMP_MESMA_REDE), 0, as.numeric(NA)),
         switch_dur_atvotr=ifelse(!is.na(NU_DUR_ATIV_COMP_OUTRAS_REDES), 0, as.numeric(NA)),
         switch_dur_AEEmsm=ifelse(!is.na(NU_DUR_AEE_MESMA_REDE), 0, as.numeric(NA)),
         switch_dur_AEEotr=ifelse(!is.na(NU_DUR_AEE_OUTRAS_REDES), 0, as.numeric(NA)),
         switch_dias_atv=ifelse(!is.na(NU_DIAS_ATIVIDADE), 0, as.numeric(NA)),
         switch_unif=ifelse(!is.na(TP_UNIFICADA), 0, as.numeric(NA)),
         switch_tipo_atend=ifelse(!is.na(TP_TIPO_ATENDIMENTO_TURMA), 0, as.numeric(NA)),
         switch_loc_turm=ifelse(!is.na(TP_TIPO_LOCAL_TURMA), 0, as.numeric(NA)),
         switch_cod_reg=ifelse(!is.na(CO_REGIAO), 0, as.numeric(NA)),
         switch_cod_meso=ifelse(!is.na(CO_MESORREGIAO), 0, as.numeric(NA)),
         switch_cod_micr=ifelse(!is.na(CO_MICRORREGIAO), 0, as.numeric(NA)),
         switch_cod_dist=ifelse(!is.na(CO_DISTRITO), 0, as.numeric(NA)),
         switch_urb_rur=ifelse(!is.na(TP_LOCALIZACAO), 0, as.numeric(NA)),
         switch_loc_dif=ifelse(!is.na(TP_LOCALIZACAO_DIFERENCIADA), 0, as.numeric(NA)),
         switch_ed_indg=ifelse(!is.na(IN_EDUCACAO_INDIGENA), 0, as.numeric(NA)))

colnames(dup_2X_MATRICULA_2020)
colnames(dup_MATRICULA_2020)
colnames(NOdup_MATRICULA_2020)

library(lubridate)
seges_2019 <-readRDS('seges_2020_raw')
colnames(seges_2019)
describe(seges_2019$CD_INEP_ALUNO)

seges1_2019 <- seges_2019 %>%
  select(CENSO_ESCOLA_ENTURMACAO, CD_INEP_ALUNO, DATA_MATRICULA, DATA_ENCERRAMENTO_MATRICULA) %>% 
  group_by(CD_INEP_ALUNO, CENSO_ESCOLA_ENTURMACAO, DATA_MATRICULA, DATA_ENCERRAMENTO_MATRICULA) %>%
  filter(DATA_MATRICULA==unique(DATA_MATRICULA)) %>% 
  filter(DATA_ENCERRAMENTO_MATRICULA== unique(DATA_ENCERRAMENTO_MATRICULA)) %>% 
  distinct(CD_INEP_ALUNO, CENSO_ESCOLA_ENTURMACAO, DATA_MATRICULA, DATA_ENCERRAMENTO_MATRICULA, .keep_all= TRUE) %>% 
  ungroup
describe(seges1_2019$CD_INEP_ALUNO) # nenhum cd_inep único perdido

dup_MATRICULA_2020 <- dup_2X_MATRICULA_2020 %>% 
  left_join(seges1_2019, by = c("CO_PESSOA_FISICA" = "CD_INEP_ALUNO", "CO_ENTIDADE" = "CENSO_ESCOLA_ENTURMACAO"))

dup_MATRICULA_2020$dt_matric <- parse_date_time(dup_MATRICULA_2020$DATA_MATRICULA, "Ymd HMS")
dup_MATRICULA_2020$dt_encerr <- parse_date_time(dup_MATRICULA_2020$DATA_ENCERRAMENTO_MATRICULA, "Ymd HMS")

dup_MATRICULA_2020$dt_encerr <- as.Date(dup_MATRICULA_2020$dt_encerr, format =  "%Y/%m/%d %H:%M:%S")
dup_MATRICULA_2020$dt_matric <- as.Date(dup_MATRICULA_2020$dt_matric, format =  "%Y/%m/%d %H:%M:%S")

rm(dup_2X_MATRICULA_2020, seges_2019, seges1_2019)

SITUACAO_2020 <-readRDS('SITUACAO_2020')

SITUACAO_2020 <- SITUACAO_2020 %>%
  select(CO_ENTIDADE, CO_ALUNO, SITUACAO)

dup_MATRICULA_2020 <- dup_MATRICULA_2020 %>% 
  left_join(SITUACAO_2020, by = c("CO_PESSOA_FISICA" = "CO_ALUNO", "CO_ENTIDADE" = "CO_ENTIDADE"))

dup_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  mutate(CO_SITUACAO=ifelse(SITUACAO=="SIR", 1,
                            ifelse(SITUACAO=="Abandono", 2,
                                   ifelse(SITUACAO=="Reprovado", 3,
                                          ifelse(SITUACAO=="Aprovado", 4,
                                                 ifelse(SITUACAO=="Falecido", 5, 0))))))

dup_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  group_by(CO_PESSOA_FISICA) %>%
  filter(if (all(is.na(dt_matric))) TRUE else dt_matric == max(dt_matric, na.rm = TRUE)) %>% 
  ungroup

describe(dup_MATRICULA_2020$CO_PESSOA_FISICA)

dup_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  group_by(CO_PESSOA_FISICA) %>%
  filter(if (all(is.na(dt_encerr))) TRUE else dt_encerr == max(dt_encerr, na.rm = TRUE)) %>% 
  ungroup

describe(dup_MATRICULA_2020$CO_PESSOA_FISICA)

dup_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  group_by(CO_PESSOA_FISICA) %>%
  filter(if (all(is.na(CO_SITUACAO))) TRUE else CO_SITUACAO == max(CO_SITUACAO, na.rm = TRUE)) %>% 
  ungroup

describe(dup_MATRICULA_2020$CO_PESSOA_FISICA)

dup_MATRICULA_2020 <- dup_MATRICULA_2020 %>%
  group_by(CO_PESSOA_FISICA) %>%
  slice_sample(1) %>% 
  ungroup

describe(dup_MATRICULA_2020$CO_PESSOA_FISICA)

rm(SITUACAO_2020)

colnames(dup_MATRICULA_2020)
colnames(NOdup_MATRICULA_2020)

dup_MATRICULA_2020 <- dup_MATRICULA_2020[, c("NU_ANO_CENSO","CO_PESSOA_FISICA","ID_MATRICULA","NU_DIA",
                                             "NU_MES","NU_ANO","NU_IDADE_REFERENCIA","NU_IDADE","TP_SEXO","TP_COR_RACA","TP_NACIONALIDADE",
                                             "CO_PAIS_ORIGEM","CO_UF_NASC","CO_MUNICIPIO_NASC","CO_PAIS_RESIDENCIA","CO_UF_END",
                                             "CO_MUNICIPIO_END","TP_ZONA_RESIDENCIAL","TP_LOCAL_RESID_DIFERENCIADA",
                                             "IN_NECESSIDADE_ESPECIAL","IN_BAIXA_VISAO","IN_CEGUEIRA","IN_DEF_AUDITIVA","IN_DEF_FISICA",
                                             "IN_DEF_INTELECTUAL","IN_SURDEZ","IN_SURDOCEGUEIRA","IN_DEF_MULTIPLA","IN_AUTISMO",
                                             "IN_SUPERDOTACAO","IN_RECURSO_LEDOR","IN_RECURSO_TRANSCRICAO","IN_RECURSO_INTERPRETE",
                                             "IN_RECURSO_LIBRAS","IN_RECURSO_LABIAL","IN_RECURSO_AMPLIADA_18","IN_RECURSO_AMPLIADA_24",
                                             "IN_RECURSO_CD_AUDIO","IN_RECURSO_PROVA_PORTUGUES","IN_RECURSO_VIDEO_LIBRAS","IN_RECURSO_BRAILLE",
                                             "IN_RECURSO_NENHUM","IN_AEE_LIBRAS","IN_AEE_LINGUA_PORTUGUESA","IN_AEE_INFORMATICA_ACESSIVEL",
                                             "IN_AEE_BRAILLE","IN_AEE_CAA","IN_AEE_SOROBAN","IN_AEE_VIDA_AUTONOMA","IN_AEE_OPTICOS_NAO_OPTICOS",
                                             "IN_AEE_ENRIQ_CURRICULAR","IN_AEE_DESEN_COGNITIVO","IN_AEE_MOBILIDADE","TP_OUTRO_LOCAL_AULA",
                                             "IN_TRANSPORTE_PUBLICO","TP_RESPONSAVEL_TRANSPORTE","IN_TRANSP_BICICLETA","IN_TRANSP_MICRO_ONIBUS",
                                             "IN_TRANSP_ONIBUS","IN_TRANSP_TR_ANIMAL","IN_TRANSP_VANS_KOMBI","IN_TRANSP_OUTRO_VEICULO",
                                             "IN_TRANSP_EMBAR_ATE5","IN_TRANSP_EMBAR_5A15","IN_TRANSP_EMBAR_15A35","IN_TRANSP_EMBAR_35",
                                             "TP_ETAPA_ENSINO","IN_ESPECIAL_EXCLUSIVA","IN_REGULAR","IN_EJA","IN_PROFISSIONALIZANTE",
                                             "ID_TURMA","CO_CURSO_EDUC_PROFISSIONAL","TP_MEDIACAO_DIDATICO_PEDAGO","NU_DURACAO_TURMA",
                                             "NU_DUR_ATIV_COMP_MESMA_REDE","NU_DUR_ATIV_COMP_OUTRAS_REDES","NU_DUR_AEE_MESMA_REDE",
                                             "NU_DUR_AEE_OUTRAS_REDES","NU_DIAS_ATIVIDADE","TP_UNIFICADA","TP_TIPO_ATENDIMENTO_TURMA",
                                             "TP_TIPO_LOCAL_TURMA","CO_ENTIDADE","CO_REGIAO","CO_MESORREGIAO","CO_MICRORREGIAO",
                                             "CO_UF","CO_MUNICIPIO","CO_DISTRITO","TP_DEPENDENCIA","TP_LOCALIZACAO","TP_LOCALIZACAO_DIFERENCIADA",
                                             "IN_EDUCACAO_INDIGENA","switch_escola","switch_munic","switch_turma","switch_uf_end","switch_munic_end",
                                             "switch_res_zon","switch_res_dif","switch_loc_aula","switch_transp","switch_regular","switch_eja",
                                             "switch_profiss","switch_mediacao","switch_duracao","switch_dur_atvmsm","switch_dur_atvotr","switch_dur_AEEmsm",
                                             "switch_dur_AEEotr","switch_dias_atv","switch_unif","switch_tipo_atend","switch_loc_turm","switch_cod_reg",
                                             "switch_cod_meso","switch_cod_micr","switch_cod_dist","switch_urb_rur","switch_loc_dif","switch_ed_indg")]
NOdup_MATRICULA_2020 <- NOdup_MATRICULA_2020[, c("NU_ANO_CENSO","CO_PESSOA_FISICA","ID_MATRICULA","NU_DIA",
                                                 "NU_MES","NU_ANO","NU_IDADE_REFERENCIA","NU_IDADE","TP_SEXO","TP_COR_RACA","TP_NACIONALIDADE",
                                                 "CO_PAIS_ORIGEM","CO_UF_NASC","CO_MUNICIPIO_NASC","CO_PAIS_RESIDENCIA","CO_UF_END",
                                                 "CO_MUNICIPIO_END","TP_ZONA_RESIDENCIAL","TP_LOCAL_RESID_DIFERENCIADA",
                                                 "IN_NECESSIDADE_ESPECIAL","IN_BAIXA_VISAO","IN_CEGUEIRA","IN_DEF_AUDITIVA","IN_DEF_FISICA",
                                                 "IN_DEF_INTELECTUAL","IN_SURDEZ","IN_SURDOCEGUEIRA","IN_DEF_MULTIPLA","IN_AUTISMO",
                                                 "IN_SUPERDOTACAO","IN_RECURSO_LEDOR","IN_RECURSO_TRANSCRICAO","IN_RECURSO_INTERPRETE",
                                                 "IN_RECURSO_LIBRAS","IN_RECURSO_LABIAL","IN_RECURSO_AMPLIADA_18","IN_RECURSO_AMPLIADA_24",
                                                 "IN_RECURSO_CD_AUDIO","IN_RECURSO_PROVA_PORTUGUES","IN_RECURSO_VIDEO_LIBRAS","IN_RECURSO_BRAILLE",
                                                 "IN_RECURSO_NENHUM","IN_AEE_LIBRAS","IN_AEE_LINGUA_PORTUGUESA","IN_AEE_INFORMATICA_ACESSIVEL",
                                                 "IN_AEE_BRAILLE","IN_AEE_CAA","IN_AEE_SOROBAN","IN_AEE_VIDA_AUTONOMA","IN_AEE_OPTICOS_NAO_OPTICOS",
                                                 "IN_AEE_ENRIQ_CURRICULAR","IN_AEE_DESEN_COGNITIVO","IN_AEE_MOBILIDADE","TP_OUTRO_LOCAL_AULA",
                                                 "IN_TRANSPORTE_PUBLICO","TP_RESPONSAVEL_TRANSPORTE","IN_TRANSP_BICICLETA","IN_TRANSP_MICRO_ONIBUS",
                                                 "IN_TRANSP_ONIBUS","IN_TRANSP_TR_ANIMAL","IN_TRANSP_VANS_KOMBI","IN_TRANSP_OUTRO_VEICULO",
                                                 "IN_TRANSP_EMBAR_ATE5","IN_TRANSP_EMBAR_5A15","IN_TRANSP_EMBAR_15A35","IN_TRANSP_EMBAR_35",
                                                 "TP_ETAPA_ENSINO","IN_ESPECIAL_EXCLUSIVA","IN_REGULAR","IN_EJA","IN_PROFISSIONALIZANTE",
                                                 "ID_TURMA","CO_CURSO_EDUC_PROFISSIONAL","TP_MEDIACAO_DIDATICO_PEDAGO","NU_DURACAO_TURMA",
                                                 "NU_DUR_ATIV_COMP_MESMA_REDE","NU_DUR_ATIV_COMP_OUTRAS_REDES","NU_DUR_AEE_MESMA_REDE",
                                                 "NU_DUR_AEE_OUTRAS_REDES","NU_DIAS_ATIVIDADE","TP_UNIFICADA","TP_TIPO_ATENDIMENTO_TURMA",
                                                 "TP_TIPO_LOCAL_TURMA","CO_ENTIDADE","CO_REGIAO","CO_MESORREGIAO","CO_MICRORREGIAO",
                                                 "CO_UF","CO_MUNICIPIO","CO_DISTRITO","TP_DEPENDENCIA","TP_LOCALIZACAO","TP_LOCALIZACAO_DIFERENCIADA",
                                                 "IN_EDUCACAO_INDIGENA","switch_escola","switch_munic","switch_turma","switch_uf_end","switch_munic_end",
                                                 "switch_res_zon","switch_res_dif","switch_loc_aula","switch_transp","switch_regular","switch_eja",
                                                 "switch_profiss","switch_mediacao","switch_duracao","switch_dur_atvmsm","switch_dur_atvotr","switch_dur_AEEmsm",
                                                 "switch_dur_AEEotr","switch_dias_atv","switch_unif","switch_tipo_atend","switch_loc_turm","switch_cod_reg",
                                                 "switch_cod_meso","switch_cod_micr","switch_cod_dist","switch_urb_rur","switch_loc_dif","switch_ed_indg")]
dup_MATRICULA_2020 <- arrange(dup_MATRICULA_2020, CO_PESSOA_FISICA)
NOdup_MATRICULA_2020 <- arrange(NOdup_MATRICULA_2020, CO_PESSOA_FISICA)

MATRICULA_2020 <- rbind(dup_MATRICULA_2020, NOdup_MATRICULA_2020)
describe(MATRICULA_2020$CO_PESSOA_FISICA)
# sem duplicações

saveRDS(MATRICULA_2020, 'MATRICULA_2020')
rm(list = ls())
