library(Hmisc)
library(tidyverse)
library(openxlsx)
library(data.table)

options(scipen=999)
memory.limit(size = 999999999999)
memory.limit()
rm(list = ls())

###### QUANDO AS DUPLICAÇÕES SE ENCERRAM COM O CRITÉRIO INTERNO AO BANCO  ######

censo_2020_2<-read.xlsx('Situação_Estadual_2020.xlsx', sheet = 1, colNames = T, skipEmptyRows = T, skipEmptyCols = T)

colnames(censo_2020_2)

censo_2020_2 <- subset(censo_2020_2, ETAPA_ENSINO==4|ETAPA_ENSINO==5|ETAPA_ENSINO==6|ETAPA_ENSINO==7|ETAPA_ENSINO==8|
                         ETAPA_ENSINO==9|ETAPA_ENSINO==10|ETAPA_ENSINO==11|ETAPA_ENSINO==14|ETAPA_ENSINO==15|ETAPA_ENSINO==16|
                         ETAPA_ENSINO==17|ETAPA_ENSINO==18|ETAPA_ENSINO==19|ETAPA_ENSINO==20|ETAPA_ENSINO==21|ETAPA_ENSINO==25|
                         ETAPA_ENSINO==26|ETAPA_ENSINO==27|ETAPA_ENSINO==28|ETAPA_ENSINO==29|ETAPA_ENSINO==30|ETAPA_ENSINO==31|
                         ETAPA_ENSINO==32|ETAPA_ENSINO==33|ETAPA_ENSINO==34|ETAPA_ENSINO==35|ETAPA_ENSINO==36|ETAPA_ENSINO==37|
                         ETAPA_ENSINO==38|ETAPA_ENSINO==41)

describe(censo_2020_2$CO_ALUNO)


dup_SITUACAO_2020 <- censo_2020_2[duplicated(censo_2020_2[, 7]) | duplicated(censo_2020_2[, 7], fromLast=T),]
describe(dup_SITUACAO_2020$CO_ALUNO)

NOdup_SITUACAO_2020 <- censo_2020_2 %>%  filter(!CO_ALUNO %in% dup_SITUACAO_2020$CO_ALUNO)
describe(NOdup_SITUACAO_2020$CO_ALUNO)


dup_SITUACAO_2020 <- dup_SITUACAO_2020 %>%
  mutate(CO_SITUACAO=ifelse(SITUACAO=="SIR", 1,
                            ifelse(SITUACAO=="Abandono", 2,
                                   ifelse(SITUACAO=="Reprovado", 3,
                                          ifelse(SITUACAO=="Aprovado", 4,
                                                 ifelse(SITUACAO=="Falecido", 5, 0))))))

dup_SITUACAO_2020 <- dup_SITUACAO_2020 %>% group_by(CO_ALUNO) %>% mutate(id = row_number()) %>% 
  ungroup()

dup1_2X_SITUACAO_2020 <- dup_SITUACAO_2020 %>%
  select(CO_ENTIDADE, CO_ALUNO, id) %>% 
  group_by(CO_ALUNO) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CO_ENTIDADE) %>% 
  fill('1_CO_ENTIDADE', '2_CO_ENTIDADE', .direction = "downup") %>% 
  rename(escola1='1_CO_ENTIDADE',escola2='2_CO_ENTIDADE') %>% 
  ungroup()

dup2_2X_SITUACAO_2020 <- dup_SITUACAO_2020 %>%
  select(CO_MUNICIPIO, CO_ALUNO, id) %>% 
  group_by(CO_ALUNO) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CO_MUNICIPIO) %>% 
  fill('1_CO_MUNICIPIO', '2_CO_MUNICIPIO', .direction = "downup") %>% 
  rename(munic1='1_CO_MUNICIPIO',munic2='2_CO_MUNICIPIO') %>% 
  ungroup()

dup1_2X_SITUACAO_2020 <- dup1_2X_SITUACAO_2020 %>%
  mutate(switch_escola=ifelse(escola1!=escola2, 1, 0))

dup2_2X_SITUACAO_2020 <- dup2_2X_SITUACAO_2020 %>%
  mutate(switch_munic=ifelse(munic1!=munic2, 1, 0))

dup1_2X_SITUACAO_2020$qt_escolas <- apply(
  subset(dup1_2X_SITUACAO_2020, select = escola1:escola2), 1,
  function(z) length(unique(z)))
dup2_2X_SITUACAO_2020$qt_munic <- apply(
  subset(dup2_2X_SITUACAO_2020, select = munic1:munic2), 1,
  function(z) length(unique(z)))

# numero de mudanças
changes <- function(z) if (length(z)) sum(z[-1] != z[-length(z)]) else 0L
dup1_2X_SITUACAO_2020$qt_switch_escola <- apply(subset(dup1_2X_SITUACAO_2020, select = escola1:escola2), 1, changes)
dup2_2X_SITUACAO_2020$qt_switch_munic <- apply(subset(dup2_2X_SITUACAO_2020, select = munic1:munic2), 1, changes)

dup_2XSITUACAO_2020 <- dup1_2X_SITUACAO_2020 %>% 
  left_join(dup2_2X_SITUACAO_2020, by = c("CO_ALUNO" = "CO_ALUNO"))
dup_SITUACAO_2020 <- dup_SITUACAO_2020 %>% 
  left_join(dup_2XSITUACAO_2020, by = c("CO_ALUNO" = "CO_ALUNO"))

rm(dup1_2X_SITUACAO_2020, dup2_2X_SITUACAO_2020, dup_2XSITUACAO_2020, changes)

NOdup_SITUACAO_2020 <- NOdup_SITUACAO_2020 %>% group_by(CO_ALUNO) %>% mutate(id = row_number())
describe(NOdup_SITUACAO_2020$id)

NOdup_SITUACAO_2020 <- NOdup_SITUACAO_2020 %>%
  mutate(switch_escola=ifelse(!is.na(CO_ENTIDADE), 0, is.na),
         qt_escolas=ifelse(!is.na(CO_ENTIDADE), 1, is.na),
         qt_switch_escola=ifelse(!is.na(CO_ENTIDADE), 0, is.na))
NOdup_SITUACAO_2020 <- NOdup_SITUACAO_2020 %>%
  mutate(switch_munic=ifelse(!is.na(MUNICIPIO), 0, is.na),
         qt_munic=ifelse(!is.na(MUNICIPIO), 1, is.na),
         qt_switch_munic=ifelse(!is.na(MUNICIPIO), 0, is.na))
NOdup_SITUACAO_2020 <- NOdup_SITUACAO_2020 %>%
  mutate(CO_SITUACAO=ifelse(SITUACAO=="SIR", 1,
                            ifelse(SITUACAO=="Abandono", 2,
                                   ifelse(SITUACAO=="Reprovado", 3,
                                          ifelse(SITUACAO=="Aprovado", 4,
                                                 ifelse(SITUACAO=="Falecido", 5, 0))))))
colnames(dup_SITUACAO_2020)
colnames(NOdup_SITUACAO_2020)

dup_SITUACAO_2020 <- dup_SITUACAO_2020 %>%
  group_by(CO_ALUNO) %>%
  filter(if (all(is.na(CO_SITUACAO))) TRUE else CO_SITUACAO == max(CO_SITUACAO, na.rm = TRUE)) %>%
  ungroup()

############# nao ha outras duplicacoes #################

dup_SITUACAO_2020 <- dup_SITUACAO_2020[, c("ANO_CENSO","DEPENDENCIA","CO_MUNICIPIO","MUNICIPIO","CO_ENTIDADE","ESCOLA",
                                           "CO_ALUNO","ETAPA_ENSINO","Nome_ETAPA","SITUACAO","CO_SITUACAO","id","switch_escola",
                                           "qt_escolas","qt_switch_escola","switch_munic","qt_munic","qt_switch_munic")]
NOdup_SITUACAO_2020 <- NOdup_SITUACAO_2020[, c("ANO_CENSO","DEPENDENCIA","CO_MUNICIPIO","MUNICIPIO","CO_ENTIDADE","ESCOLA",
                                               "CO_ALUNO","ETAPA_ENSINO","Nome_ETAPA","SITUACAO","CO_SITUACAO","id","switch_escola",
                                               "qt_escolas","qt_switch_escola","switch_munic","qt_munic","qt_switch_munic")]
dup_SITUACAO_2020 <- arrange(dup_SITUACAO_2020, CO_ALUNO)
NOdup_SITUACAO_2020 <- arrange(NOdup_SITUACAO_2020, CO_ALUNO)

SITUACAO_2020 <- rbind(dup_SITUACAO_2020, NOdup_SITUACAO_2020)
describe(SITUACAO_2020$CO_ALUNO)
# sem duplicaÃ§Ãµes

saveRDS(SITUACAO_2020, 'SITUACAO_2020')
rm(list = ls())

##################################################################################################################
############ se ainda sobram duplicacoes com "situacao" iguais, eh preciso saber qual eh a ultima linha ##########
##################################################################################################################
##################################################################################################################
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
  ungroup()
describe(seges1_2019$CD_INEP_ALUNO) # nenhum cd_inep unico perdido

dup1_SITUACAO_2020 <- dup_SITUACAO_2020 %>% 
  left_join(seges1_2019, by = c("CO_ALUNO" = "CD_INEP_ALUNO", "CO_ENTIDADE" = "CENSO_ESCOLA_ENTURMACAO"))

dup1_SITUACAO_2020$dt_matric <- parse_date_time(dup1_SITUACAO_2020$DATA_MATRICULA, "Ymd HMS")
dup1_SITUACAO_2020$dt_encerr <- parse_date_time(dup1_SITUACAO_2020$DATA_ENCERRAMENTO_MATRICULA, "Ymd HMS")

dup1_SITUACAO_2020$dt_encerr <- as.Date(dup1_SITUACAO_2020$dt_encerr, format =  "%Y/%m/%d %H:%M:%S")
dup1_SITUACAO_2020$dt_matric <- as.Date(dup1_SITUACAO_2020$dt_matric, format =  "%Y/%m/%d %H:%M:%S")

dup2_SITUACAO_2020 <- dup1_SITUACAO_2020 %>%
  group_by(CO_ALUNO) %>%
  filter(if (all(is.na(dt_matric))) TRUE else dt_matric == max(dt_matric, na.rm = TRUE)) %>%
  ungroup()

dup2_SITUACAO_2020 <- dup2_SITUACAO_2020 %>%
  group_by(CO_ALUNO) %>%
  filter(if (all(is.na(dt_encerr))) TRUE else dt_encerr == max(dt_encerr, na.rm = TRUE)) %>%
  ungroup()

dup2_SITUACAO_2020 <- dup2_SITUACAO_2020 %>%
  group_by(CO_ALUNO) %>%
  slice_sample(n=1) %>%
  ungroup()

#############
colnames(dup2_SITUACAO_2020)
colnames(NOdup_SITUACAO_2020)

dup2_SITUACAO_2020 <- dup2_SITUACAO_2020[, c("ANO_CENSO","DEPENDENCIA","CO_MUNICIPIO","MUNICIPIO","CO_ENTIDADE","ESCOLA",
                                             "CO_ALUNO","ETAPA_ENSINO","Nome_ETAPA","SITUACAO","CO_SITUACAO","id","switch_escola",
                                             "qt_escolas","qt_switch_escola","switch_munic","qt_munic","qt_switch_munic")]
NOdup_SITUACAO_2020 <- NOdup_SITUACAO_2020[, c("ANO_CENSO","DEPENDENCIA","CO_MUNICIPIO","MUNICIPIO","CO_ENTIDADE","ESCOLA",
                                               "CO_ALUNO","ETAPA_ENSINO","Nome_ETAPA","SITUACAO","CO_SITUACAO","id","switch_escola",
                                               "qt_escolas","qt_switch_escola","switch_munic","qt_munic","qt_switch_munic")]
dup2_SITUACAO_2020 <- arrange(dup2_SITUACAO_2020, CO_ALUNO)
NOdup_SITUACAO_2020 <- arrange(NOdup_SITUACAO_2020, CO_ALUNO)

SITUACAO_2020 <- rbind(dup2_SITUACAO_2020, NOdup_SITUACAO_2020)
describe(SITUACAO_2020$CO_ALUNO)
# sem duplicacoes

saveRDS(SITUACAO_2020, 'SITUACAO_2020')
rm(list = ls())
