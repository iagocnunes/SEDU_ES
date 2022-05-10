#############################################################################################
######## ANALISE DOS DADOS DA SEGES E CENSO; LIMPEZA, UNIAO E PREPARACAO PARA O PREDITOR ####
#############################################################################################
library(Hmisc) #comando describe 
library(tidyverse)
library(openxlsx)
library(data.table)
library(hablar)

options(scipen=999)
memory.limit(size = 999999999999)
memory.limit()
rm(list = ls())

# ALGUMAS ETAPAS FORAM SUPRIMIDAS PARA MANTER O SIGILO DOS DADOS

################################# ABRINDO BASES SEGES ########################################
##############################################################################################

seges_2016<-read.csv('DADOS_SEGES_2020.CSV',sep = ';', header = T)

saveRDS(seges_2016,'seges_2020_raw')

seges_2016 <-readRDS('seges_2019_raw')

#### removendo recuperacao final, nota final, EJAs, Ed. Prof. e MEPES
seges_2016 <- subset(seges_2016, DC_PERIODO_ENTURMACAO!="2020 MEPES")
seges_2016 <- subset(seges_2016, DIVISAO!="RECUPERAÇÃO FINAL"&DIVISAO!="NOTA FINAL")
seges_2016 <- subset(seges_2016, ID_NIVEL_MATRICULA!=19&ID_NIVEL_MATRICULA!=20&ID_NIVEL_MATRICULA!=21)
###################### ANALISANDO VARIAVEIS ######################
colnames(seges_2016)
str(seges_2016)

# suprimido

####################### PIVOTAR BANCO LONGO PARA BANCO LARGO ######################
################ PREENCHENDO COLUNAS VAZIAS DE UM MESMO ESTUDANTE #################
###################################################################################

colnames(seges_2016)
# seges_2016_fill <- seges_2016[,c("x","y",  "z", "w", "j")]
# seges_2016 <- seges_2016[,-(21:23),drop=FALSE]
# seges_2016 <- seges_2016[,-(13),drop=FALSE]


seges_2016_fill1 <- seges_2016 %>%
  select(NM_REGIONAL_ENTURMACAO, NM_MUNICIPIO_ENTURMACAO, CENSO_ESCOLA_ENTURMACAO, LOCALIZACAO,
         ID_ETAPA_MATRICULA, ID_NIVEL_MATRICULA, ID_TURNO, ID_TURMA, NM_ESCOLA_ENTURMACAO, RA, NM_ALUNO, DC_COR_RACA, TP_SEXO,
         DC_PROGRAMA_PEDAGOGICO_ITEM, DIVISAO, VL_NOTA, NU_CPF, CD_INEP_ALUNO) %>%
  group_by(RA,ID_TURNO,ID_TURMA,CENSO_ESCOLA_ENTURMACAO) %>%
  pivot_wider(names_from = c(DC_PROGRAMA_PEDAGOGICO_ITEM, DIVISAO), values_from = VL_NOTA, values_fn = max) %>%
  fill('CD_INEP_ALUNO', 'NU_CPF', 'MATEMÁTICA_3º TRIMESTRE', 'LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 1º TRIMESTRE', 
       'FILOSOFIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'FILOSOFIA_2º TRIMESTRE', 'BIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE',
       'CIÊNCIAS_1º TRIMESTRE', 'HISTÓRIA_2º TRIMESTRE', 'ARTE_REC. TRIMESTRAL - 1º TRIMESTRE', 'LÍNGUA PORTUGUESA_1º TRIMESTRE', 
       'ARTE_3º TRIMESTRE', 'CIÊNCIAS_3º TRIMESTRE', 'QUÍMICA_2º TRIMESTRE', 'MATEMÁTICA_REC. TRIMESTRAL - 1º TRIMESTRE', 
       'EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE', 'GEOGRAFIA_3º TRIMESTRE', 'FILOSOFIA_1º TRIMESTRE', 
       'FILOSOFIA_3º TRIMESTRE', 'FÍSICA_2º TRIMESTRE', 'HISTÓRIA_1º TRIMESTRE', 'ARTE_1º TRIMESTRE', 'MATEMÁTICA_2º TRIMESTRE',
       'FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE', 'HISTÓRIA_REC. TRIMESTRAL - 2º TRIMESTRE', 'FILOSOFIA_REC. TRIMESTRAL - 2º TRIMESTRE',
       'HISTÓRIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'EDUCAÇÃO FÍSICA_2º TRIMESTRE', 'EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE',
       'CIÊNCIAS_2º TRIMESTRE', 'GEOGRAFIA_REC. TRIMESTRAL - 2º TRIMESTRE', 'SOCIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE',
       'ARTE_2º TRIMESTRE', 'LÍNGUA PORTUGUESA_2º TRIMESTRE', 'QUÍMICA_3º TRIMESTRE', 'LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 2º TRIMESTRE',
       'GEOGRAFIA_1º TRIMESTRE', 'EDUCAÇÃO FÍSICA_1º TRIMESTRE', 'ARTE_REC. TRIMESTRAL - 2º TRIMESTRE', 
       'FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE', 'MATEMÁTICA_1º TRIMESTRE', 'BIOLOGIA_3º TRIMESTRE', 
       'MATEMÁTICA_REC. TRIMESTRAL - 2º TRIMESTRE', 'QUÍMICA_REC. TRIMESTRAL - 2º TRIMESTRE', 'HISTÓRIA_3º TRIMESTRE', 
       'CIÊNCIAS_REC. TRIMESTRAL - 1º TRIMESTRE', 'SOCIOLOGIA_2º TRIMESTRE', 'CIÊNCIAS_REC. TRIMESTRAL - 2º TRIMESTRE', 
       'FÍSICA_1º TRIMESTRE', 'GEOGRAFIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'LÍNGUA PORTUGUESA_3º TRIMESTRE', 'BIOLOGIA_2º TRIMESTRE', 
       'EDUCAÇÃO FÍSICA_3º TRIMESTRE', 'QUÍMICA_REC. TRIMESTRAL - 1º TRIMESTRE', 'GEOGRAFIA_2º TRIMESTRE', 'FÍSICA_3º TRIMESTRE', 
       'SOCIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'QUÍMICA_1º TRIMESTRE', 'BIOLOGIA_1º TRIMESTRE', 
       'BIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'SOCIOLOGIA_1º TRIMESTRE', 'SOCIOLOGIA_3º TRIMESTRE',
       .direction = "downup") %>%
  rename(mat_3_tri='MATEMÁTICA_3º TRIMESTRE',lp_rec_1_tri='LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 1º TRIMESTRE', 
         fil_rec_1_tri='FILOSOFIA_REC. TRIMESTRAL - 1º TRIMESTRE',fil_2_tri='FILOSOFIA_2º TRIMESTRE',
         bio_rec_2_tri='BIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE', cien_1_tri='CIÊNCIAS_1º TRIMESTRE',
         hist_2_tri='HISTÓRIA_2º TRIMESTRE', art_rec_1_tri='ARTE_REC. TRIMESTRAL - 1º TRIMESTRE',
         lp_1_tri='LÍNGUA PORTUGUESA_1º TRIMESTRE', art_3_tri='ARTE_3º TRIMESTRE', cien_3_tri='CIÊNCIAS_3º TRIMESTRE',
         qui_2_tri='QUÍMICA_2º TRIMESTRE', mat_rec_1_tri='MATEMÁTICA_REC. TRIMESTRAL - 1º TRIMESTRE', 
         ed_fis_rec_2_tri='EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE',geo_3_tri='GEOGRAFIA_3º TRIMESTRE',
         fil_1_tri='FILOSOFIA_1º TRIMESTRE', fil_3_tri='FILOSOFIA_3º TRIMESTRE',fis_2_tri='FÍSICA_2º TRIMESTRE',
         hist_1_tri='HISTÓRIA_1º TRIMESTRE', art_1_tri='ARTE_1º TRIMESTRE',mat_2_tri='MATEMÁTICA_2º TRIMESTRE',
         fis_rec_2_tri='FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE', hist_rec_2_tri='HISTÓRIA_REC. TRIMESTRAL - 2º TRIMESTRE', 
         fil_rec_2_tri='FILOSOFIA_REC. TRIMESTRAL - 2º TRIMESTRE',hist_rec_1_tri='HISTÓRIA_REC. TRIMESTRAL - 1º TRIMESTRE',
         ed_fis_2_tri='EDUCAÇÃO FÍSICA_2º TRIMESTRE',ed_fis_rec_1_tri='EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE',
         cien_2_tri='CIÊNCIAS_2º TRIMESTRE',geo_rec_2_tri='GEOGRAFIA_REC. TRIMESTRAL - 2º TRIMESTRE', 
         soc_rec_2_tri='SOCIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE',art_2_tri='ARTE_2º TRIMESTRE',
         lp_2_tri='LÍNGUA PORTUGUESA_2º TRIMESTRE',qui_3_tri='QUÍMICA_3º TRIMESTRE',
         lp_rec_2_tri='LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 2º TRIMESTRE',geo_1_tri='GEOGRAFIA_1º TRIMESTRE', 
         ed_fis_1_tri='EDUCAÇÃO FÍSICA_1º TRIMESTRE', art_rec_2_tri='ARTE_REC. TRIMESTRAL - 2º TRIMESTRE',
         fis_rec_1_tri='FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE', mat_1_tri='MATEMÁTICA_1º TRIMESTRE',
         bio_3_tri='BIOLOGIA_3º TRIMESTRE',mat_rec_2_tri='MATEMÁTICA_REC. TRIMESTRAL - 2º TRIMESTRE',
         qui_rec_2_tri='QUÍMICA_REC. TRIMESTRAL - 2º TRIMESTRE',hist_3_tri='HISTÓRIA_3º TRIMESTRE', 
         cien_rec_1_tri='CIÊNCIAS_REC. TRIMESTRAL - 1º TRIMESTRE',soc_2_tri='SOCIOLOGIA_2º TRIMESTRE',
         cien_rec_2_tri='CIÊNCIAS_REC. TRIMESTRAL - 2º TRIMESTRE',fis_1_tri='FÍSICA_1º TRIMESTRE',
         geo_rec_1_tri='GEOGRAFIA_REC. TRIMESTRAL - 1º TRIMESTRE',lp_3_tri='LÍNGUA PORTUGUESA_3º TRIMESTRE',
         bio_2_tri='BIOLOGIA_2º TRIMESTRE',ed_fis_3_tri='EDUCAÇÃO FÍSICA_3º TRIMESTRE',
         qui_rec_1_tri='QUÍMICA_REC. TRIMESTRAL - 1º TRIMESTRE',geo_2_tri='GEOGRAFIA_2º TRIMESTRE',
         fis_3_tri='FÍSICA_3º TRIMESTRE',soc_rec_1_tri='SOCIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE',
         qui_1_tri='QUÍMICA_1º TRIMESTRE',bio_1_tri='BIOLOGIA_1º TRIMESTRE',bio_rec_1_tri='BIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE',
         soc_1_tri='SOCIOLOGIA_1º TRIMESTRE',soc_3_tri='SOCIOLOGIA_3º TRIMESTRE') %>%
  distinct(RA,ID_TURNO,ID_TURMA,CENSO_ESCOLA_ENTURMACAO, .keep_all= TRUE) %>% 
  ungroup()

seges_2016_fill2 <- seges_2016 %>%
  select(NM_REGIONAL_ENTURMACAO, NM_MUNICIPIO_ENTURMACAO, CENSO_ESCOLA_ENTURMACAO, LOCALIZACAO,
         ID_ETAPA_MATRICULA, ID_NIVEL_MATRICULA, ID_TURNO, ID_TURMA, NM_ESCOLA_ENTURMACAO, RA, NM_ALUNO, DC_COR_RACA, TP_SEXO,
         DC_PROGRAMA_PEDAGOGICO_ITEM, DIVISAO, QT_AULA_PREVISTA, NU_CPF, CD_INEP_ALUNO) %>%
  group_by(RA,ID_TURNO,ID_TURMA,CENSO_ESCOLA_ENTURMACAO) %>%
  pivot_wider(names_from = c(DC_PROGRAMA_PEDAGOGICO_ITEM, DIVISAO), values_from = QT_AULA_PREVISTA, values_fn = max) %>%
  fill('MATEMÁTICA_3º TRIMESTRE', 'LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 1º TRIMESTRE', 
       'FILOSOFIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'FILOSOFIA_2º TRIMESTRE',
       'BIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE', 'CIÊNCIAS_1º TRIMESTRE', 'HISTÓRIA_2º TRIMESTRE', 
       'ARTE_REC. TRIMESTRAL - 1º TRIMESTRE', 'LÍNGUA PORTUGUESA_1º TRIMESTRE',
       'ARTE_3º TRIMESTRE', 'CIÊNCIAS_3º TRIMESTRE', 'QUÍMICA_2º TRIMESTRE', 'MATEMÁTICA_REC. TRIMESTRAL - 1º TRIMESTRE', 
       'EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE', 'GEOGRAFIA_3º TRIMESTRE', 'FILOSOFIA_1º TRIMESTRE', 
       'FILOSOFIA_3º TRIMESTRE', 'FÍSICA_2º TRIMESTRE', 'HISTÓRIA_1º TRIMESTRE', 
       'ARTE_1º TRIMESTRE', 'MATEMÁTICA_2º TRIMESTRE', 'FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE', 
       'HISTÓRIA_REC. TRIMESTRAL - 2º TRIMESTRE',
       'FILOSOFIA_REC. TRIMESTRAL - 2º TRIMESTRE', 'HISTÓRIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'EDUCAÇÃO FÍSICA_2º TRIMESTRE', 
       'EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE', 'CIÊNCIAS_2º TRIMESTRE', 'GEOGRAFIA_REC. TRIMESTRAL - 2º TRIMESTRE', 
       'SOCIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE', 'ARTE_2º TRIMESTRE', 'LÍNGUA PORTUGUESA_2º TRIMESTRE', 
       'QUÍMICA_3º TRIMESTRE', 'LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 2º TRIMESTRE', 'GEOGRAFIA_1º TRIMESTRE', 
       'EDUCAÇÃO FÍSICA_1º TRIMESTRE', 'ARTE_REC. TRIMESTRAL - 2º TRIMESTRE',
       'FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE', 'MATEMÁTICA_1º TRIMESTRE', 'BIOLOGIA_3º TRIMESTRE', 
       'MATEMÁTICA_REC. TRIMESTRAL - 2º TRIMESTRE', 'QUÍMICA_REC. TRIMESTRAL - 2º TRIMESTRE', 'HISTÓRIA_3º TRIMESTRE', 
       'CIÊNCIAS_REC. TRIMESTRAL - 1º TRIMESTRE', 'SOCIOLOGIA_2º TRIMESTRE', 'CIÊNCIAS_REC. TRIMESTRAL - 2º TRIMESTRE', 
       'FÍSICA_1º TRIMESTRE', 'GEOGRAFIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'LÍNGUA PORTUGUESA_3º TRIMESTRE', 'BIOLOGIA_2º TRIMESTRE', 
       'EDUCAÇÃO FÍSICA_3º TRIMESTRE', 'QUÍMICA_REC. TRIMESTRAL - 1º TRIMESTRE', 'GEOGRAFIA_2º TRIMESTRE', 'FÍSICA_3º TRIMESTRE', 
       'SOCIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'QUÍMICA_1º TRIMESTRE', 'BIOLOGIA_1º TRIMESTRE', 
       'BIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'SOCIOLOGIA_1º TRIMESTRE', 'SOCIOLOGIA_3º TRIMESTRE',
       .direction = "downup") %>%
  rename(mat_aula_3_tri='MATEMÁTICA_3º TRIMESTRE',lp_aula_rec_1_tri='LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 1º TRIMESTRE', 
         fil_aula_rec_1_tri='FILOSOFIA_REC. TRIMESTRAL - 1º TRIMESTRE',fil_aula_2_tri='FILOSOFIA_2º TRIMESTRE',
         bio_aula_rec_2_tri='BIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE',
         cien_aula_1_tri='CIÊNCIAS_1º TRIMESTRE',hist_aula_2_tri='HISTÓRIA_2º TRIMESTRE', 
         art_aula_rec_1_tri='ARTE_REC. TRIMESTRAL - 1º TRIMESTRE',lp_aula_1_tri='LÍNGUA PORTUGUESA_1º TRIMESTRE',
         art_aula_3_tri='ARTE_3º TRIMESTRE', cien_aula_3_tri='CIÊNCIAS_3º TRIMESTRE',
         qui_aula_2_tri='QUÍMICA_2º TRIMESTRE', mat_aula_rec_1_tri='MATEMÁTICA_REC. TRIMESTRAL - 1º TRIMESTRE', 
         ed_fis_aula_rec_2_tri='EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE',geo_aula_3_tri='GEOGRAFIA_3º TRIMESTRE',
         fil_aula_1_tri='FILOSOFIA_1º TRIMESTRE',
         fil_aula_3_tri='FILOSOFIA_3º TRIMESTRE',fis_aula_2_tri='FÍSICA_2º TRIMESTRE',hist_aula_1_tri='HISTÓRIA_1º TRIMESTRE', 
         art_aula_1_tri='ARTE_1º TRIMESTRE',mat_aula_2_tri='MATEMÁTICA_2º TRIMESTRE',fis_aula_rec_2_tri='FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE', 
         hist_aula_rec_2_tri='HISTÓRIA_REC. TRIMESTRAL - 2º TRIMESTRE', 
         fil_aula_rec_2_tri='FILOSOFIA_REC. TRIMESTRAL - 2º TRIMESTRE',hist_aula_rec_1_tri='HISTÓRIA_REC. TRIMESTRAL - 1º TRIMESTRE',
         ed_fis_aula_2_tri='EDUCAÇÃO FÍSICA_2º TRIMESTRE',ed_fis_aula_rec_1_tri='EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE',
         cien_aula_2_tri='CIÊNCIAS_2º TRIMESTRE',geo_aula_rec_2_tri='GEOGRAFIA_REC. TRIMESTRAL - 2º TRIMESTRE', 
         soc_aula_rec_2_tri='SOCIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE',art_aula_2_tri='ARTE_2º TRIMESTRE',
         lp_aula_2_tri='LÍNGUA PORTUGUESA_2º TRIMESTRE',qui_aula_3_tri='QUÍMICA_3º TRIMESTRE',
         lp_aula_rec_2_tri='LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 2º TRIMESTRE',geo_aula_1_tri='GEOGRAFIA_1º TRIMESTRE', 
         ed_fis_aula_1_tri='EDUCAÇÃO FÍSICA_1º TRIMESTRE',
         art_aula_rec_2_tri='ARTE_REC. TRIMESTRAL - 2º TRIMESTRE', 
         fis_aula_rec_1_tri='FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE',
         mat_aula_1_tri='MATEMÁTICA_1º TRIMESTRE',bio_aula_3_tri='BIOLOGIA_3º TRIMESTRE',mat_aula_rec_2_tri='MATEMÁTICA_REC. TRIMESTRAL - 2º TRIMESTRE',
         qui_aula_rec_2_tri='QUÍMICA_REC. TRIMESTRAL - 2º TRIMESTRE',hist_aula_3_tri='HISTÓRIA_3º TRIMESTRE', 
         cien_aula_rec_1_tri='CIÊNCIAS_REC. TRIMESTRAL - 1º TRIMESTRE',soc_aula_2_tri='SOCIOLOGIA_2º TRIMESTRE',
         cien_aula_rec_2_tri='CIÊNCIAS_REC. TRIMESTRAL - 2º TRIMESTRE',fis_aula_1_tri='FÍSICA_1º TRIMESTRE',
         geo_aula_rec_1_tri='GEOGRAFIA_REC. TRIMESTRAL - 1º TRIMESTRE',lp_aula_3_tri='LÍNGUA PORTUGUESA_3º TRIMESTRE',
         bio_aula_2_tri='BIOLOGIA_2º TRIMESTRE',ed_fis_aula_3_tri='EDUCAÇÃO FÍSICA_3º TRIMESTRE',
         qui_aula_rec_1_tri='QUÍMICA_REC. TRIMESTRAL - 1º TRIMESTRE',geo_aula_2_tri='GEOGRAFIA_2º TRIMESTRE',
         fis_aula_3_tri='FÍSICA_3º TRIMESTRE',soc_aula_rec_1_tri='SOCIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE',
         qui_aula_1_tri='QUÍMICA_1º TRIMESTRE',bio_aula_1_tri='BIOLOGIA_1º TRIMESTRE',bio_aula_rec_1_tri='BIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE',
         soc_aula_1_tri='SOCIOLOGIA_1º TRIMESTRE',soc_aula_3_tri='SOCIOLOGIA_3º TRIMESTRE') %>%
  distinct(RA,ID_TURNO,ID_TURMA,CENSO_ESCOLA_ENTURMACAO, .keep_all= TRUE) %>% 
  ungroup()

seges_2016_fill3 <- seges_2016 %>%
  select(NM_REGIONAL_ENTURMACAO, NM_MUNICIPIO_ENTURMACAO, CENSO_ESCOLA_ENTURMACAO, LOCALIZACAO,
         ID_ETAPA_MATRICULA, ID_NIVEL_MATRICULA, ID_TURNO, ID_TURMA, NM_ESCOLA_ENTURMACAO, RA, NM_ALUNO, DC_COR_RACA, TP_SEXO,
         DC_PROGRAMA_PEDAGOGICO_ITEM, DIVISAO, QT_FALTA, NU_CPF, CD_INEP_ALUNO) %>%
  group_by(RA,ID_TURNO,ID_TURMA,CENSO_ESCOLA_ENTURMACAO) %>%
  pivot_wider(names_from = c(DC_PROGRAMA_PEDAGOGICO_ITEM, DIVISAO), values_from = QT_FALTA, values_fn = max) %>%
  fill('MATEMÁTICA_3º TRIMESTRE', 'LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 1º TRIMESTRE', 
       'FILOSOFIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'FILOSOFIA_2º TRIMESTRE', 
       'BIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE', 'CIÊNCIAS_1º TRIMESTRE', 'HISTÓRIA_2º TRIMESTRE', 
       'ARTE_REC. TRIMESTRAL - 1º TRIMESTRE', 'LÍNGUA PORTUGUESA_1º TRIMESTRE', 
       'ARTE_3º TRIMESTRE', 'CIÊNCIAS_3º TRIMESTRE', 'QUÍMICA_2º TRIMESTRE', 'MATEMÁTICA_REC. TRIMESTRAL - 1º TRIMESTRE', 
       'EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE', 'GEOGRAFIA_3º TRIMESTRE', 'FILOSOFIA_1º TRIMESTRE', 
       'FILOSOFIA_3º TRIMESTRE', 'FÍSICA_2º TRIMESTRE', 'HISTÓRIA_1º TRIMESTRE', 
       'ARTE_1º TRIMESTRE', 'MATEMÁTICA_2º TRIMESTRE', 'FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE', 
       'HISTÓRIA_REC. TRIMESTRAL - 2º TRIMESTRE', 
       'FILOSOFIA_REC. TRIMESTRAL - 2º TRIMESTRE', 'HISTÓRIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'EDUCAÇÃO FÍSICA_2º TRIMESTRE', 
       'EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE', 'CIÊNCIAS_2º TRIMESTRE', 'GEOGRAFIA_REC. TRIMESTRAL - 2º TRIMESTRE', 
       'SOCIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE', 'ARTE_2º TRIMESTRE', 'LÍNGUA PORTUGUESA_2º TRIMESTRE', 
       'QUÍMICA_3º TRIMESTRE', 'LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 2º TRIMESTRE', 'GEOGRAFIA_1º TRIMESTRE', 
       'EDUCAÇÃO FÍSICA_1º TRIMESTRE', 'ARTE_REC. TRIMESTRAL - 2º TRIMESTRE',
       'FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE', 'MATEMÁTICA_1º TRIMESTRE', 'BIOLOGIA_3º TRIMESTRE', 
       'MATEMÁTICA_REC. TRIMESTRAL - 2º TRIMESTRE', 'QUÍMICA_REC. TRIMESTRAL - 2º TRIMESTRE', 'HISTÓRIA_3º TRIMESTRE', 
       'CIÊNCIAS_REC. TRIMESTRAL - 1º TRIMESTRE', 'SOCIOLOGIA_2º TRIMESTRE', 'CIÊNCIAS_REC. TRIMESTRAL - 2º TRIMESTRE', 
       'FÍSICA_1º TRIMESTRE', 'GEOGRAFIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'LÍNGUA PORTUGUESA_3º TRIMESTRE', 'BIOLOGIA_2º TRIMESTRE', 
       'EDUCAÇÃO FÍSICA_3º TRIMESTRE', 'QUÍMICA_REC. TRIMESTRAL - 1º TRIMESTRE', 'GEOGRAFIA_2º TRIMESTRE', 'FÍSICA_3º TRIMESTRE', 
       'SOCIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'QUÍMICA_1º TRIMESTRE', 'BIOLOGIA_1º TRIMESTRE', 
       'BIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE', 'SOCIOLOGIA_1º TRIMESTRE', 'SOCIOLOGIA_3º TRIMESTRE',
       .direction = "downup") %>%
  rename(mat_falta_3_tri='MATEMÁTICA_3º TRIMESTRE',lp_falta_rec_1_tri='LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 1º TRIMESTRE', 
         fil_falta_rec_1_tri='FILOSOFIA_REC. TRIMESTRAL - 1º TRIMESTRE',fil_falta_2_tri='FILOSOFIA_2º TRIMESTRE',
         bio_falta_rec_2_tri='BIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE',
         cien_falta_1_tri='CIÊNCIAS_1º TRIMESTRE',hist_falta_2_tri='HISTÓRIA_2º TRIMESTRE', 
         art_falta_rec_1_tri='ARTE_REC. TRIMESTRAL - 1º TRIMESTRE',lp_falta_1_tri='LÍNGUA PORTUGUESA_1º TRIMESTRE',
         art_falta_3_tri='ARTE_3º TRIMESTRE', cien_falta_3_tri='CIÊNCIAS_3º TRIMESTRE',
         qui_falta_2_tri='QUÍMICA_2º TRIMESTRE', mat_falta_rec_1_tri='MATEMÁTICA_REC. TRIMESTRAL - 1º TRIMESTRE', 
         ed_fis_falta_rec_2_tri='EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE',geo_falta_3_tri='GEOGRAFIA_3º TRIMESTRE',
         fil_falta_1_tri='FILOSOFIA_1º TRIMESTRE',
         fil_falta_3_tri='FILOSOFIA_3º TRIMESTRE',fis_falta_2_tri='FÍSICA_2º TRIMESTRE',hist_falta_1_tri='HISTÓRIA_1º TRIMESTRE', 
         art_falta_1_tri='ARTE_1º TRIMESTRE',mat_falta_2_tri='MATEMÁTICA_2º TRIMESTRE',fis_falta_rec_2_tri='FÍSICA_REC. TRIMESTRAL - 2º TRIMESTRE', 
         hist_falta_rec_2_tri='HISTÓRIA_REC. TRIMESTRAL - 2º TRIMESTRE',
         fil_falta_rec_2_tri='FILOSOFIA_REC. TRIMESTRAL - 2º TRIMESTRE',hist_falta_rec_1_tri='HISTÓRIA_REC. TRIMESTRAL - 1º TRIMESTRE',
         ed_fis_falta_2_tri='EDUCAÇÃO FÍSICA_2º TRIMESTRE',ed_fis_falta_rec_1_tri='EDUCAÇÃO FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE',
         cien_falta_2_tri='CIÊNCIAS_2º TRIMESTRE',geo_falta_rec_2_tri='GEOGRAFIA_REC. TRIMESTRAL - 2º TRIMESTRE', 
         soc_falta_rec_2_tri='SOCIOLOGIA_REC. TRIMESTRAL - 2º TRIMESTRE',art_falta_2_tri='ARTE_2º TRIMESTRE',
         lp_falta_2_tri='LÍNGUA PORTUGUESA_2º TRIMESTRE',qui_falta_3_tri='QUÍMICA_3º TRIMESTRE',
         lp_falta_rec_2_tri='LÍNGUA PORTUGUESA_REC. TRIMESTRAL - 2º TRIMESTRE',geo_falta_1_tri='GEOGRAFIA_1º TRIMESTRE', 
         ed_fis_falta_1_tri='EDUCAÇÃO FÍSICA_1º TRIMESTRE',
         art_falta_rec_2_tri='ARTE_REC. TRIMESTRAL - 2º TRIMESTRE', fis_falta_rec_1_tri='FÍSICA_REC. TRIMESTRAL - 1º TRIMESTRE',
         mat_falta_1_tri='MATEMÁTICA_1º TRIMESTRE',bio_falta_3_tri='BIOLOGIA_3º TRIMESTRE',mat_falta_rec_2_tri='MATEMÁTICA_REC. TRIMESTRAL - 2º TRIMESTRE',
         qui_falta_rec_2_tri='QUÍMICA_REC. TRIMESTRAL - 2º TRIMESTRE',hist_falta_3_tri='HISTÓRIA_3º TRIMESTRE', 
         cien_falta_rec_1_tri='CIÊNCIAS_REC. TRIMESTRAL - 1º TRIMESTRE',soc_falta_2_tri='SOCIOLOGIA_2º TRIMESTRE',
         cien_falta_rec_2_tri='CIÊNCIAS_REC. TRIMESTRAL - 2º TRIMESTRE',fis_falta_1_tri='FÍSICA_1º TRIMESTRE',
         geo_falta_rec_1_tri='GEOGRAFIA_REC. TRIMESTRAL - 1º TRIMESTRE',lp_falta_3_tri='LÍNGUA PORTUGUESA_3º TRIMESTRE',
         bio_falta_2_tri='BIOLOGIA_2º TRIMESTRE',ed_fis_falta_3_tri='EDUCAÇÃO FÍSICA_3º TRIMESTRE',
         qui_falta_rec_1_tri='QUÍMICA_REC. TRIMESTRAL - 1º TRIMESTRE',geo_falta_2_tri='GEOGRAFIA_2º TRIMESTRE',
         fis_falta_3_tri='FÍSICA_3º TRIMESTRE',soc_falta_rec_1_tri='SOCIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE',
         qui_falta_1_tri='QUÍMICA_1º TRIMESTRE',bio_falta_1_tri='BIOLOGIA_1º TRIMESTRE',bio_falta_rec_1_tri='BIOLOGIA_REC. TRIMESTRAL - 1º TRIMESTRE',
         soc_falta_1_tri='SOCIOLOGIA_1º TRIMESTRE',soc_falta_3_tri='SOCIOLOGIA_3º TRIMESTRE') %>%
  distinct(RA,ID_TURNO,ID_TURMA,CENSO_ESCOLA_ENTURMACAO, .keep_all= TRUE) %>% 
  ungroup()

colnames(seges_2016_fill1)
seges_2016_fill1 <- arrange(seges_2016_fill1, RA)
seges_2016_fill2 <- arrange(seges_2016_fill2, RA)
seges_2016_fill3 <- arrange(seges_2016_fill3, RA)
describe(seges_2016_fill1$RA)

saveRDS(seges_2016_fill1, 'seges_2020_notas')
saveRDS(seges_2016_fill2, 'seges_2020_aulas')
saveRDS(seges_2016_fill3, 'seges_2020_faltas')
rm(seges_2016, seges_2016_fill2, seges_2016_fill3)
###############################################################################
#################### TRATAMENTO DOS DUPLICADOS: NOTAS ################################


dup_seges_2016_fill1 <- seges_2016_fill1[duplicated(seges_2016_fill1[, 10]) | duplicated(seges_2016_fill1[, 10], fromLast=T),]
describe(dup_seges_2016_fill1$RA)

dup_seges_2016_fill2 <- seges_2016_fill2[duplicated(seges_2016_fill2[, 10]) | duplicated(seges_2016_fill2[, 10], fromLast=T),]
describe(dup_seges_2016_fill2$RA)

dup_seges_2016_fill3 <- seges_2016_fill3[duplicated(seges_2016_fill3[, 10]) | duplicated(seges_2016_fill3[, 10], fromLast=T),]
describe(dup_seges_2016_fill3$RA)


NOdup_seges_2016_fill1 <- seges_2016_fill1 %>%  filter(!RA %in% dup_seges_2016_fill1$RA)
describe(NOdup_seges_2016_fill1$RA)

# numerando duplicados
dup_seges_2016_fill1 <- dup_seges_2016_fill1 %>% group_by(RA) %>% mutate(id = row_number()) %>% 
  ungroup()
dup_seges_2016_fill2 <- dup_seges_2016_fill2 %>% group_by(RA) %>% mutate(id = row_number()) %>% 
  ungroup()
dup_seges_2016_fill3 <- dup_seges_2016_fill3 %>% group_by(RA) %>% mutate(id = row_number()) %>% 
  ungroup()
describe(dup_seges_2016_fill1$id)
describe(dup_seges_2016_fill2$id)
describe(dup_seges_2016_fill3$id)

dup_count_seges2016_1 <- as.data.table(table(dup_seges_2016_fill1$RA))
RA_group_3X <- subset(dup_count_seges2016_1, N==3)
RA_group_4X <- subset(dup_count_seges2016_1, N==4)

dup_seges_2016_fill1$dup_count <-
  ifelse(dup_seges_2016_fill1$RA %in% RA_group_3X$V1, 3,
         ifelse(dup_seges_2016_fill1$RA %in% RA_group_4X$V1, 4, 2))
describe(dup_seges_2016_fill1$dup_count)

dup_seges_2016_fill1 <- dup_seges_2016_fill1 %>%
  mutate(EMnotas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_1_tri) |!is.na(geo_1_tri) |!is.na(soc_1_tri) |!is.na(fil_1_tri) |!is.na(lp_1_tri) |
                                           !is.na(ed_fis_1_tri) |!is.na(art_1_tri) |!is.na(fis_1_tri) |!is.na(qui_1_tri) |!is.na(bio_1_tri) |
                                           !is.na(mat_1_tri), 1, 0), 0),
         EMnotas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMnotas1tri_semNA),
         EMnotas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_2_tri) |!is.na(geo_2_tri) |!is.na(soc_2_tri) |!is.na(fil_2_tri) |!is.na(lp_2_tri) |
                                           !is.na(ed_fis_2_tri) |!is.na(art_2_tri) |!is.na(fis_2_tri) |!is.na(qui_2_tri) |!is.na(bio_2_tri) |
                                           !is.na(mat_2_tri), 1, 0), 0),
         EMnotas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMnotas2tri_semNA),
         EMnotas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_3_tri) |!is.na(geo_3_tri) |!is.na(soc_3_tri) |!is.na(fil_3_tri) |!is.na(lp_3_tri) |
                                           !is.na(ed_fis_3_tri) |!is.na(art_3_tri) |!is.na(fis_3_tri) |!is.na(qui_3_tri) |!is.na(bio_3_tri) |
                                           !is.na(mat_3_tri), 1, 0), 0),
         EMnotas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMnotas3tri_semNA),
         EMnotas_semNA=ifelse(EMnotas1tri_semNA==1 & EMnotas2tri_semNA==1 & EMnotas3tri_semNA==1, 1, 0),
         EMnotas_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMnotas_semNA),
         EFnotas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_1_tri) |!is.na(geo_1_tri) |!is.na(lp_1_tri) | !is.na(ed_fis_1_tri) |!is.na(art_1_tri) |
                                           !is.na(mat_1_tri) |!is.na(cien_1_tri) , 1, 0), 0),
         EFnotas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFnotas1tri_semNA),
         EFnotas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_2_tri) |!is.na(geo_2_tri) |!is.na(lp_2_tri) | !is.na(ed_fis_2_tri) |!is.na(art_2_tri) |
                                           !is.na(mat_2_tri) |!is.na(cien_2_tri) , 1, 0), 0),
         EFnotas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFnotas2tri_semNA),
         EFnotas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_3_tri) |!is.na(geo_3_tri) |!is.na(lp_3_tri) | !is.na(ed_fis_3_tri) |!is.na(art_3_tri) |
                                           !is.na(mat_3_tri) |!is.na(cien_3_tri) , 1, 0), 0),
         EFnotas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFnotas3tri_semNA),
         EFnotas_semNA=ifelse(EFnotas1tri_semNA==1 & EFnotas2tri_semNA==1 & EFnotas3tri_semNA==1, 1, 0),
         EFnotas_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFnotas_semNA))

dup_count4X_seges2016_1 <- subset(dup_seges_2016_fill1, dup_count==4)
dup_count3X_seges2016_1 <- subset(dup_seges_2016_fill1, dup_count==3)
dup_count2X_seges2016_1 <- subset(dup_seges_2016_fill1, dup_count==2)
rm(dup_count_seges2016_1, RA_group_3X, RA_group_4X, RA_group_5X, seges_2016)
colnames(dup_count5X_seges2016_1)

#######################################################
######### CRIANDO VARIAVEIS PARA AS DUPLICACOES #######
######## DUP_4X ##############

dup1_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(CENSO_ESCOLA_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CENSO_ESCOLA_ENTURMACAO) %>% 
  fill('1_CENSO_ESCOLA_ENTURMACAO', '2_CENSO_ESCOLA_ENTURMACAO', '3_CENSO_ESCOLA_ENTURMACAO', '4_CENSO_ESCOLA_ENTURMACAO',
       .direction = "downup") %>% 
  rename(escola1='1_CENSO_ESCOLA_ENTURMACAO',escola2='2_CENSO_ESCOLA_ENTURMACAO', escola3='3_CENSO_ESCOLA_ENTURMACAO',
         escola4='4_CENSO_ESCOLA_ENTURMACAO') %>% 
  ungroup()

dup2_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(NM_MUNICIPIO_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NM_MUNICIPIO_ENTURMACAO) %>% 
  fill('1_NM_MUNICIPIO_ENTURMACAO', '2_NM_MUNICIPIO_ENTURMACAO', '3_NM_MUNICIPIO_ENTURMACAO', '4_NM_MUNICIPIO_ENTURMACAO', 
       .direction = "downup") %>% 
  rename(munic1='1_NM_MUNICIPIO_ENTURMACAO',munic2='2_NM_MUNICIPIO_ENTURMACAO', munic3='3_NM_MUNICIPIO_ENTURMACAO',
         munic4='4_NM_MUNICIPIO_ENTURMACAO') %>% 
  ungroup()

dup3_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(ID_TURNO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURNO) %>% 
  fill('1_ID_TURNO', '2_ID_TURNO', '3_ID_TURNO', '4_ID_TURNO', .direction = "downup") %>% 
  rename(turno1='1_ID_TURNO',turno2='2_ID_TURNO', turno3='3_ID_TURNO', turno4='4_ID_TURNO') %>% 
  ungroup()

dup4_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(ID_TURMA, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURMA) %>% 
  fill('1_ID_TURMA', '2_ID_TURMA', '3_ID_TURMA', '4_ID_TURMA', .direction = "downup") %>% 
  rename(turma1='1_ID_TURMA',turma2='2_ID_TURMA', turma3='3_ID_TURMA', turma4='4_ID_TURMA') %>% 
  ungroup()

dup1_4X_seges2016_1 <- dup1_4X_seges2016_1 %>%
  mutate(switch_escola=ifelse(escola1!=escola2 | escola2!=escola3 | escola3!=escola4 | escola4!=escola1, 1, 0))

dup2_4X_seges2016_1 <- dup2_4X_seges2016_1 %>%
  mutate(switch_munic=ifelse(munic1!=munic2 | munic2!=munic3 | munic3!=munic4 | munic4!=munic1, 1, 0))

dup3_4X_seges2016_1 <- dup3_4X_seges2016_1 %>%
  mutate(switch_turno=ifelse(turno1!=turno2 | turno2!=turno3 | turno3!=turno4 | turno4!=turno1, 1, 0))

dup4_4X_seges2016_1 <- dup4_4X_seges2016_1 %>%
  mutate(switch_turma=ifelse(turma1!=turma2 | turma2!=turma3 | turma3!=turma4 | turma4!=turma1, 1, 0))

# numero de escolas, munic, turnos e turmas
dup1_4X_seges2016_1$qt_escolas <- apply(
  subset(dup1_4X_seges2016_1, select = escola1:escola4), 1,
  function(z) length(unique(z)))
dup2_4X_seges2016_1$qt_munic <- apply(
  subset(dup2_4X_seges2016_1, select = munic1:munic4), 1,
  function(z) length(unique(z)))
dup3_4X_seges2016_1$qt_turno <- apply(
  subset(dup3_4X_seges2016_1, select = turno1:turno4), 1,
  function(z) length(unique(z)))
dup4_4X_seges2016_1$qt_turma <- apply(
  subset(dup4_4X_seges2016_1, select = turma1:turma4), 1,
  function(z) length(unique(z)))

# numero de mudancas
changes <- function(z) if (length(z)) sum(z[-1] != z[-length(z)]) else 0L
dup1_4X_seges2016_1$qt_switch_escola <- apply(subset(dup1_4X_seges2016_1, select = escola1:escola4), 1, changes)
dup2_4X_seges2016_1$qt_switch_munic <- apply(subset(dup2_4X_seges2016_1, select = munic1:munic4), 1, changes)
dup3_4X_seges2016_1$qt_switch_turno <- apply(subset(dup3_4X_seges2016_1, select = turno1:turno4), 1, changes)
dup4_4X_seges2016_1$qt_switch_turma <- apply(subset(dup4_4X_seges2016_1, select = turma1:turma4), 1, changes)

dup_4X_seges2016_1 <- dup1_4X_seges2016_1 %>% 
  left_join(dup2_4X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic)
dup_4X_seges2016_1 <- dup_4X_seges2016_1 %>% 
  left_join(dup3_4X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno, qt_switch_turno)
dup_4X_seges2016_1 <- dup_4X_seges2016_1 %>% 
  left_join(dup4_4X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno,
         qt_switch_turno, switch_turma, qt_turma, qt_switch_turma)
dup_count4X_seges2016_1 <- dup_count4X_seges2016_1 %>% 
  left_join(dup_4X_seges2016_1, by = "RA")

rm(dup_4X_seges2016_1, dup1_4X_seges2016_1, dup2_4X_seges2016_1, dup3_4X_seges2016_1, dup4_4X_seges2016_1)

######## DUP_3X ##############

dup1_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(CENSO_ESCOLA_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CENSO_ESCOLA_ENTURMACAO) %>% 
  fill('1_CENSO_ESCOLA_ENTURMACAO', '2_CENSO_ESCOLA_ENTURMACAO', '3_CENSO_ESCOLA_ENTURMACAO',
       .direction = "downup") %>% 
  rename(escola1='1_CENSO_ESCOLA_ENTURMACAO',escola2='2_CENSO_ESCOLA_ENTURMACAO', escola3='3_CENSO_ESCOLA_ENTURMACAO') %>% 
  ungroup()

dup2_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(NM_MUNICIPIO_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NM_MUNICIPIO_ENTURMACAO) %>% 
  fill('1_NM_MUNICIPIO_ENTURMACAO', '2_NM_MUNICIPIO_ENTURMACAO', '3_NM_MUNICIPIO_ENTURMACAO',
       .direction = "downup") %>% 
  rename(munic1='1_NM_MUNICIPIO_ENTURMACAO',munic2='2_NM_MUNICIPIO_ENTURMACAO', munic3='3_NM_MUNICIPIO_ENTURMACAO') %>% 
  ungroup()

dup3_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(ID_TURNO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURNO) %>% 
  fill('1_ID_TURNO', '2_ID_TURNO', '3_ID_TURNO', .direction = "downup") %>% 
  rename(turno1='1_ID_TURNO',turno2='2_ID_TURNO', turno3='3_ID_TURNO') %>% 
  ungroup()

dup4_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(ID_TURMA, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURMA) %>% 
  fill('1_ID_TURMA', '2_ID_TURMA', '3_ID_TURMA', .direction = "downup") %>% 
  rename(turma1='1_ID_TURMA',turma2='2_ID_TURMA', turma3='3_ID_TURMA') %>% 
  ungroup()

dup1_3X_seges2016_1 <- dup1_3X_seges2016_1 %>%
  mutate(switch_escola=ifelse(escola1!=escola2 | escola2!=escola3 | escola3!=escola1, 1, 0))

dup2_3X_seges2016_1 <- dup2_3X_seges2016_1 %>%
  mutate(switch_munic=ifelse(munic1!=munic2 | munic2!=munic3 | munic3!=munic1, 1, 0))

dup3_3X_seges2016_1 <- dup3_3X_seges2016_1 %>%
  mutate(switch_turno=ifelse(turno1!=turno2 | turno2!=turno3 | turno3!=turno1, 1, 0))

dup4_3X_seges2016_1 <- dup4_3X_seges2016_1 %>%
  mutate(switch_turma=ifelse(turma1!=turma2 | turma2!=turma3 | turma3!=turma1, 1, 0))

# numero de escolas, munic, turnos e turmas
dup1_3X_seges2016_1$qt_escolas <- apply(
  subset(dup1_3X_seges2016_1, select = escola1:escola3), 1,
  function(z) length(unique(z)))
dup2_3X_seges2016_1$qt_munic <- apply(
  subset(dup2_3X_seges2016_1, select = munic1:munic3), 1,
  function(z) length(unique(z)))
dup3_3X_seges2016_1$qt_turno <- apply(
  subset(dup3_3X_seges2016_1, select = turno1:turno3), 1,
  function(z) length(unique(z)))
dup4_3X_seges2016_1$qt_turma <- apply(
  subset(dup4_3X_seges2016_1, select = turma1:turma3), 1,
  function(z) length(unique(z)))

# numero de mudancas
changes <- function(z) if (length(z)) sum(z[-1] != z[-length(z)]) else 0L
dup1_3X_seges2016_1$qt_switch_escola <- apply(subset(dup1_3X_seges2016_1, select = escola1:escola3), 1, changes)
dup2_3X_seges2016_1$qt_switch_munic <- apply(subset(dup2_3X_seges2016_1, select = munic1:munic3), 1, changes)
dup3_3X_seges2016_1$qt_switch_turno <- apply(subset(dup3_3X_seges2016_1, select = turno1:turno3), 1, changes)
dup4_3X_seges2016_1$qt_switch_turma <- apply(subset(dup4_3X_seges2016_1, select = turma1:turma3), 1, changes)

dup_3X_seges2016_1 <- dup1_3X_seges2016_1 %>% 
  left_join(dup2_3X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic)
dup_3X_seges2016_1 <- dup_3X_seges2016_1 %>% 
  left_join(dup3_3X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno, qt_switch_turno)
dup_3X_seges2016_1 <- dup_3X_seges2016_1 %>% 
  left_join(dup4_3X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno,
         qt_switch_turno, switch_turma, qt_turma, qt_switch_turma)
dup_count3X_seges2016_1 <- dup_count3X_seges2016_1 %>% 
  left_join(dup_3X_seges2016_1, by = "RA")

rm(dup_3X_seges2016_1, dup1_3X_seges2016_1, dup2_3X_seges2016_1, dup3_3X_seges2016_1, dup4_3X_seges2016_1)

######## DUP_2X ##############

dup1_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(CENSO_ESCOLA_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CENSO_ESCOLA_ENTURMACAO) %>% 
  fill('1_CENSO_ESCOLA_ENTURMACAO', '2_CENSO_ESCOLA_ENTURMACAO',
       .direction = "downup") %>% 
  rename(escola1='1_CENSO_ESCOLA_ENTURMACAO',escola2='2_CENSO_ESCOLA_ENTURMACAO') %>% 
  ungroup()

dup2_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(NM_MUNICIPIO_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NM_MUNICIPIO_ENTURMACAO) %>% 
  fill('1_NM_MUNICIPIO_ENTURMACAO', '2_NM_MUNICIPIO_ENTURMACAO',
       .direction = "downup") %>% 
  rename(munic1='1_NM_MUNICIPIO_ENTURMACAO',munic2='2_NM_MUNICIPIO_ENTURMACAO') %>% 
  ungroup()

dup3_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(ID_TURNO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURNO) %>% 
  fill('1_ID_TURNO', '2_ID_TURNO', .direction = "downup") %>% 
  rename(turno1='1_ID_TURNO',turno2='2_ID_TURNO') %>% 
  ungroup()

dup4_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(ID_TURMA, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURMA) %>% 
  fill('1_ID_TURMA', '2_ID_TURMA', .direction = "downup") %>% 
  rename(turma1='1_ID_TURMA',turma2='2_ID_TURMA') %>% 
  ungroup()

dup1_2X_seges2016_1 <- dup1_2X_seges2016_1 %>%
  mutate(switch_escola=ifelse(escola1!=escola2, 1, 0))

dup2_2X_seges2016_1 <- dup2_2X_seges2016_1 %>%
  mutate(switch_munic=ifelse(munic1!=munic2, 1, 0))

dup3_2X_seges2016_1 <- dup3_2X_seges2016_1 %>%
  mutate(switch_turno=ifelse(turno1!=turno2, 1, 0))

dup4_2X_seges2016_1 <- dup4_2X_seges2016_1 %>%
  mutate(switch_turma=ifelse(turma1!=turma2, 1, 0))

# numero de escolas, munic, turnos e turmas
dup1_2X_seges2016_1$qt_escolas <- apply(
  subset(dup1_2X_seges2016_1, select = escola1:escola2), 1,
  function(z) length(unique(z)))
dup2_2X_seges2016_1$qt_munic <- apply(
  subset(dup2_2X_seges2016_1, select = munic1:munic2), 1,
  function(z) length(unique(z)))
dup3_2X_seges2016_1$qt_turno <- apply(
  subset(dup3_2X_seges2016_1, select = turno1:turno2), 1,
  function(z) length(unique(z)))
dup4_2X_seges2016_1$qt_turma <- apply(
  subset(dup4_2X_seges2016_1, select = turma1:turma2), 1,
  function(z) length(unique(z)))

# numero de mudancas
changes <- function(z) if (length(z)) sum(z[-1] != z[-length(z)]) else 0L
dup1_2X_seges2016_1$qt_switch_escola <- apply(subset(dup1_2X_seges2016_1, select = escola1:escola2), 1, changes)
dup2_2X_seges2016_1$qt_switch_munic <- apply(subset(dup2_2X_seges2016_1, select = munic1:munic2), 1, changes)
dup3_2X_seges2016_1$qt_switch_turno <- apply(subset(dup3_2X_seges2016_1, select = turno1:turno2), 1, changes)
dup4_2X_seges2016_1$qt_switch_turma <- apply(subset(dup4_2X_seges2016_1, select = turma1:turma2), 1, changes)

dup_2X_seges2016_1 <- dup1_2X_seges2016_1 %>% 
  left_join(dup2_2X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic)
dup_2X_seges2016_1 <- dup_2X_seges2016_1 %>% 
  left_join(dup3_2X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno, qt_switch_turno)
dup_2X_seges2016_1 <- dup_2X_seges2016_1 %>% 
  left_join(dup4_2X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno,
         qt_switch_turno, switch_turma, qt_turma, qt_switch_turma)
dup_count2X_seges2016_1 <- dup_count2X_seges2016_1 %>% 
  left_join(dup_2X_seges2016_1, by = "RA")

rm(dup_2X_seges2016_1, dup1_2X_seges2016_1, dup2_2X_seges2016_1, dup3_2X_seges2016_1, dup4_2X_seges2016_1)

################ CRIANDO AS MESMAS VARIAVEIS PARA OS NAO-DUPLICADOS #####################
#########################################################################################

NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1 %>% group_by(RA) %>% mutate(id = row_number())  %>% 
  ungroup()

dup_count_seges2016_1 <- as.data.table(table(dup_seges_2016_fill1$RA))
RA_group_3X <- subset(dup_count_seges2016_1, N==3)
RA_group_4X <- subset(dup_count_seges2016_1, N==4)
RA_group_2X <- subset(dup_count_seges2016_1, N==2)


NOdup_seges_2016_fill1$dup_count <-
  ifelse(!NOdup_seges_2016_fill1$RA %in% RA_group_2X | !NOdup_seges_2016_fill1$RA %in% RA_group_3X | !NOdup_seges_2016_fill1$RA %in% RA_group_4X, 0, 6)
table(NOdup_seges_2016_fill1$dup_count)

NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1 %>%
  mutate(EMnotas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_1_tri) |!is.na(geo_1_tri) |!is.na(soc_1_tri) |!is.na(fil_1_tri) |!is.na(lp_1_tri) |
                                           !is.na(ed_fis_1_tri) |!is.na(art_1_tri) |!is.na(fis_1_tri) |!is.na(qui_1_tri) |!is.na(bio_1_tri) |
                                           !is.na(mat_1_tri), 1, 0), 0),
         EMnotas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMnotas1tri_semNA),
         EMnotas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_2_tri) |!is.na(geo_2_tri) |!is.na(soc_2_tri) |!is.na(fil_2_tri) |!is.na(lp_2_tri) |
                                           !is.na(ed_fis_2_tri) |!is.na(art_2_tri) |!is.na(fis_2_tri) |!is.na(qui_2_tri) |!is.na(bio_2_tri) |
                                           !is.na(mat_2_tri), 1, 0), 0),
         EMnotas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMnotas2tri_semNA),
         EMnotas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_3_tri) |!is.na(geo_3_tri) |!is.na(soc_3_tri) |!is.na(fil_3_tri) |!is.na(lp_3_tri) |
                                           !is.na(ed_fis_3_tri) |!is.na(art_3_tri) |!is.na(fis_3_tri) |!is.na(qui_3_tri) |!is.na(bio_3_tri) |
                                           !is.na(mat_3_tri), 1, 0), 0),
         EMnotas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMnotas3tri_semNA),
         EMnotas_semNA=ifelse(EMnotas1tri_semNA==1 & EMnotas2tri_semNA==1 & EMnotas3tri_semNA==1, 1, 0),
         EMnotas_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMnotas_semNA),
         EFnotas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_1_tri) |!is.na(geo_1_tri) |!is.na(lp_1_tri) | !is.na(ed_fis_1_tri) |!is.na(art_1_tri) |
                                           !is.na(mat_1_tri) |!is.na(cien_1_tri) , 1, 0), 0),
         EFnotas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFnotas1tri_semNA),
         EFnotas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_2_tri) |!is.na(geo_2_tri) |!is.na(lp_2_tri) | !is.na(ed_fis_2_tri) |!is.na(art_2_tri) |
                                           !is.na(mat_2_tri) |!is.na(cien_2_tri) , 1, 0), 0),
         EFnotas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFnotas2tri_semNA),
         EFnotas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_3_tri) |!is.na(geo_3_tri) |!is.na(lp_3_tri) | !is.na(ed_fis_3_tri) |!is.na(art_3_tri) |
                                           !is.na(mat_3_tri) |!is.na(cien_3_tri) , 1, 0), 0),
         EFnotas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFnotas3tri_semNA),
         EFnotas_semNA=ifelse(EFnotas1tri_semNA==1 & EFnotas2tri_semNA==1 & EFnotas3tri_semNA==1, 1, 0),
         EFnotas_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFnotas_semNA))

NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1 %>%
  mutate(switch_escola=ifelse(!is.na(CENSO_ESCOLA_ENTURMACAO), 0, as.numeric(NA)),
         qt_escolas=ifelse(!is.na(CENSO_ESCOLA_ENTURMACAO), 1, as.numeric(NA)),
         qt_switch_escola=ifelse(!is.na(CENSO_ESCOLA_ENTURMACAO), 0, as.numeric(NA)),
         switch_munic=ifelse(!is.na(NM_MUNICIPIO_ENTURMACAO), 0, as.numeric(NA)),
         qt_switch_munic=ifelse(!is.na(NM_MUNICIPIO_ENTURMACAO), 0, as.numeric(NA)),
         qt_munic=ifelse(!is.na(NM_MUNICIPIO_ENTURMACAO), 1, as.numeric(NA)),
         switch_turno=ifelse(!is.na(ID_TURNO), 0, as.numeric(NA)),
         qt_turno=ifelse(!is.na(ID_TURNO), 1, as.numeric(NA)),
         qt_switch_turno=ifelse(!is.na(ID_TURNO), 0, as.numeric(NA)),
         switch_turma=ifelse(!is.na(ID_TURMA), 0, as.numeric(NA)),
         qt_turma=ifelse(!is.na(ID_TURMA), 1, as.numeric(NA)),
         qt_switch_turma=ifelse(!is.na(ID_TURMA), 0, as.numeric(NA)))

############### Unindo duplicados e nao-duplicados ##################
rm(changes, RA_group_2X, RA_group_3X, RA_group_4X, RA_group_5X, RA_group_10X, seges_2016_fill1, dup_seges_2016_fill1)

colnames(dup_count2X_seges2016_1)
colnames(NOdup_seges_2016_fill1)

NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                     "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                     "CD_INEP_ALUNO","hist_2_tri","hist_1_tri","hist_3_tri","geo_3_tri","geo_1_tri","geo_2_tri","soc_2_tri","soc_1_tri",
                                                     "soc_3_tri","fil_3_tri","fil_2_tri","art_rec_2_tri","bio_rec_2_tri","ed_fis_rec_2_tri","ed_fis_rec_1_tri",
                                                     "fil_rec_2_tri","fis_rec_2_tri","fis_rec_1_tri","geo_rec_2_tri","geo_rec_1_tri","hist_rec_1_tri","hist_rec_2_tri",
                                                     "lp_rec_1_tri","mat_rec_2_tri","mat_rec_1_tri","qui_rec_2_tri","qui_rec_1_tri","soc_rec_1_tri","lp_2_tri","lp_1_tri",
                                                     "lp_3_tri","ed_fis_2_tri","art_2_tri","art_1_tri","fis_3_tri","fis_2_tri","fis_1_tri","qui_2_tri","qui_3_tri",
                                                     "qui_1_tri","bio_1_tri","mat_1_tri","mat_3_tri","art_rec_1_tri","bio_rec_1_tri","fil_rec_1_tri","lp_rec_2_tri",
                                                     "soc_rec_2_tri","ed_fis_3_tri","ed_fis_1_tri","art_3_tri","bio_2_tri","bio_3_tri","mat_2_tri","fil_1_tri",
                                                     "cien_3_tri","cien_2_tri","cien_1_tri","cien_rec_2_tri","cien_rec_1_tri","id", "dup_count","EMnotas1tri_semNA",
                                                     "EMnotas2tri_semNA","EMnotas3tri_semNA","EMnotas_semNA","EFnotas1tri_semNA","EFnotas2tri_semNA","EFnotas3tri_semNA",
                                                     "EFnotas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                     "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]

dup_count2X_seges2016_1 <- dup_count2X_seges2016_1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                       "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                       "CD_INEP_ALUNO","hist_2_tri","hist_1_tri","hist_3_tri","geo_3_tri","geo_1_tri","geo_2_tri","soc_2_tri","soc_1_tri",
                                                       "soc_3_tri","fil_3_tri","fil_2_tri","art_rec_2_tri","bio_rec_2_tri","ed_fis_rec_2_tri","ed_fis_rec_1_tri",
                                                       "fil_rec_2_tri","fis_rec_2_tri","fis_rec_1_tri","geo_rec_2_tri","geo_rec_1_tri","hist_rec_1_tri","hist_rec_2_tri",
                                                       "lp_rec_1_tri","mat_rec_2_tri","mat_rec_1_tri","qui_rec_2_tri","qui_rec_1_tri","soc_rec_1_tri","lp_2_tri","lp_1_tri",
                                                       "lp_3_tri","ed_fis_2_tri","art_2_tri","art_1_tri","fis_3_tri","fis_2_tri","fis_1_tri","qui_2_tri","qui_3_tri",
                                                       "qui_1_tri","bio_1_tri","mat_1_tri","mat_3_tri","art_rec_1_tri","bio_rec_1_tri","fil_rec_1_tri","lp_rec_2_tri",
                                                       "soc_rec_2_tri","ed_fis_3_tri","ed_fis_1_tri","art_3_tri","bio_2_tri","bio_3_tri","mat_2_tri","fil_1_tri",
                                                       "cien_3_tri","cien_2_tri","cien_1_tri","cien_rec_2_tri","cien_rec_1_tri","id", "dup_count","EMnotas1tri_semNA",
                                                       "EMnotas2tri_semNA","EMnotas3tri_semNA","EMnotas_semNA","EFnotas1tri_semNA","EFnotas2tri_semNA","EFnotas3tri_semNA",
                                                       "EFnotas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                       "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]

dup_count3X_seges2016_1 <- dup_count3X_seges2016_1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                       "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                       "CD_INEP_ALUNO","hist_2_tri","hist_1_tri","hist_3_tri","geo_3_tri","geo_1_tri","geo_2_tri","soc_2_tri","soc_1_tri",
                                                       "soc_3_tri","fil_3_tri","fil_2_tri","art_rec_2_tri","bio_rec_2_tri","ed_fis_rec_2_tri","ed_fis_rec_1_tri",
                                                       "fil_rec_2_tri","fis_rec_2_tri","fis_rec_1_tri","geo_rec_2_tri","geo_rec_1_tri","hist_rec_1_tri","hist_rec_2_tri",
                                                       "lp_rec_1_tri","mat_rec_2_tri","mat_rec_1_tri","qui_rec_2_tri","qui_rec_1_tri","soc_rec_1_tri","lp_2_tri","lp_1_tri",
                                                       "lp_3_tri","ed_fis_2_tri","art_2_tri","art_1_tri","fis_3_tri","fis_2_tri","fis_1_tri","qui_2_tri","qui_3_tri",
                                                       "qui_1_tri","bio_1_tri","mat_1_tri","mat_3_tri","art_rec_1_tri","bio_rec_1_tri","fil_rec_1_tri","lp_rec_2_tri",
                                                       "soc_rec_2_tri","ed_fis_3_tri","ed_fis_1_tri","art_3_tri","bio_2_tri","bio_3_tri","mat_2_tri","fil_1_tri",
                                                       "cien_3_tri","cien_2_tri","cien_1_tri","cien_rec_2_tri","cien_rec_1_tri","id", "dup_count","EMnotas1tri_semNA",
                                                       "EMnotas2tri_semNA","EMnotas3tri_semNA","EMnotas_semNA","EFnotas1tri_semNA","EFnotas2tri_semNA","EFnotas3tri_semNA",
                                                       "EFnotas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                       "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]

dup_count4X_seges2016_1 <- dup_count4X_seges2016_1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                       "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                       "CD_INEP_ALUNO","hist_2_tri","hist_1_tri","hist_3_tri","geo_3_tri","geo_1_tri","geo_2_tri","soc_2_tri","soc_1_tri",
                                                       "soc_3_tri","fil_3_tri","fil_2_tri","art_rec_2_tri","bio_rec_2_tri","ed_fis_rec_2_tri","ed_fis_rec_1_tri",
                                                       "fil_rec_2_tri","fis_rec_2_tri","fis_rec_1_tri","geo_rec_2_tri","geo_rec_1_tri","hist_rec_1_tri","hist_rec_2_tri",
                                                       "lp_rec_1_tri","mat_rec_2_tri","mat_rec_1_tri","qui_rec_2_tri","qui_rec_1_tri","soc_rec_1_tri","lp_2_tri","lp_1_tri",
                                                       "lp_3_tri","ed_fis_2_tri","art_2_tri","art_1_tri","fis_3_tri","fis_2_tri","fis_1_tri","qui_2_tri","qui_3_tri",
                                                       "qui_1_tri","bio_1_tri","mat_1_tri","mat_3_tri","art_rec_1_tri","bio_rec_1_tri","fil_rec_1_tri","lp_rec_2_tri",
                                                       "soc_rec_2_tri","ed_fis_3_tri","ed_fis_1_tri","art_3_tri","bio_2_tri","bio_3_tri","mat_2_tri","fil_1_tri",
                                                       "cien_3_tri","cien_2_tri","cien_1_tri","cien_rec_2_tri","cien_rec_1_tri","id", "dup_count","EMnotas1tri_semNA",
                                                       "EMnotas2tri_semNA","EMnotas3tri_semNA","EMnotas_semNA","EFnotas1tri_semNA","EFnotas2tri_semNA","EFnotas3tri_semNA",
                                                       "EFnotas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                       "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]


dup_seges_2016_fill1 <- rbind(NOdup_seges_2016_fill1, dup_count2X_seges2016_1, dup_count3X_seges2016_1, dup_count4X_seges2016_1)

saveRDS(dup_seges_2016_fill1, 'seges2020notas_dup_postTRATAMENTO')

rm(NOdup_seges_2016_fill1, dup_count2X_seges2016_1, dup_count3X_seges2016_1, dup_count4X_seges2016_1,dup_seges_2016_fill1,
   seges_2016_fill1, dup_count_seges2016_1)
#################################################################
################# TRATANDO DUPLICADOS: AULAS ###################

seges_2016_fill1 <-readRDS('seges_2020_aulas')

# todos os duplicados
dup_seges_2016_fill1 <- seges_2016_fill1[duplicated(seges_2016_fill1[, 10]) | duplicated(seges_2016_fill1[, 10], fromLast=T),]
describe(dup_seges_2016_fill1$RA)

NOdup_seges_2016_fill1 <- seges_2016_fill1 %>%  filter(!RA %in% dup_seges_2016_fill1$RA)

# numerando duplicados
dup_seges_2016_fill1 <- dup_seges_2016_fill1 %>% group_by(RA) %>% mutate(id = row_number()) %>% 
  ungroup()

dup_count_seges2016_1 <- as.data.table(table(dup_seges_2016_fill1$RA))
RA_group_3X <- subset(dup_count_seges2016_1, N==3)
RA_group_4X <- subset(dup_count_seges2016_1, N==4)
RA_group_2X <- subset(dup_count_seges2016_1, N==2)

dup_seges_2016_fill1$dup_count <-
  ifelse(dup_seges_2016_fill1$RA %in% RA_group_3X$V1, 3,
         ifelse(dup_seges_2016_fill1$RA %in% RA_group_4X$V1, 4, 2))
describe(dup_seges_2016_fill1$dup_count)


dup_seges_2016_fill1 <- dup_seges_2016_fill1 %>%
  mutate(EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(soc_aula_1_tri) |!is.na(fil_aula_1_tri) |!is.na(lp_aula_1_tri) |
                                           !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |!is.na(fis_aula_1_tri) |!is.na(qui_aula_1_tri) |!is.na(bio_aula_1_tri) |
                                           !is.na(mat_aula_1_tri), 1, 0), 0),
         EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas1tri_semNA),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(soc_aula_2_tri) |!is.na(fil_aula_2_tri) |!is.na(lp_aula_2_tri) |
                                           !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |!is.na(fis_aula_2_tri) |!is.na(qui_aula_2_tri) |!is.na(bio_aula_2_tri) |
                                           !is.na(mat_aula_2_tri), 1, 0), 0),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas2tri_semNA),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(soc_aula_3_tri) |!is.na(fil_aula_3_tri) |!is.na(lp_aula_3_tri) |
                                           !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |!is.na(fis_aula_3_tri) |!is.na(qui_aula_3_tri) |!is.na(bio_aula_3_tri) |
                                           !is.na(mat_aula_3_tri), 1, 0), 0),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas3tri_semNA),
         EMaulas_semNA=ifelse(EMaulas1tri_semNA==1 & EMaulas2tri_semNA==1 & EMaulas3tri_semNA==1, 1, 0),
         EMaulas_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas_semNA),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(lp_aula_1_tri) | !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |
                                           !is.na(mat_aula_1_tri) |!is.na(cien_aula_1_tri) , 1, 0), 0),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas1tri_semNA),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(lp_aula_2_tri) | !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |
                                           !is.na(mat_aula_2_tri) |!is.na(cien_aula_2_tri) , 1, 0), 0),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas2tri_semNA),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(lp_aula_3_tri) | !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |
                                           !is.na(mat_aula_3_tri) |!is.na(cien_aula_3_tri) , 1, 0), 0),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas3tri_semNA),
         EFaulas_semNA=ifelse(EFaulas1tri_semNA==1 & EFaulas2tri_semNA==1 & EFaulas3tri_semNA==1, 1, 0),
         EFaulas_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas_semNA))

dup_count4X_seges2016_1 <- subset(dup_seges_2016_fill1, dup_count==4)
dup_count3X_seges2016_1 <- subset(dup_seges_2016_fill1, dup_count==3)
dup_count2X_seges2016_1 <- subset(dup_seges_2016_fill1, dup_count==2)
rm(dup_count_seges2016_1, RA_group_3X, RA_group_4X, RA_group_5X, RA_group_2X, seges_2016)
colnames(dup_count5X_seges2016_1)

#######################################################
######### CRIANDO VARIAVEIS PARA AS DUPLICACOES #######
######## DUP_4X ##############

dup1_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(CENSO_ESCOLA_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CENSO_ESCOLA_ENTURMACAO) %>% 
  fill('1_CENSO_ESCOLA_ENTURMACAO', '2_CENSO_ESCOLA_ENTURMACAO', '3_CENSO_ESCOLA_ENTURMACAO', '4_CENSO_ESCOLA_ENTURMACAO',
       .direction = "downup") %>% 
  rename(escola1='1_CENSO_ESCOLA_ENTURMACAO',escola2='2_CENSO_ESCOLA_ENTURMACAO', escola3='3_CENSO_ESCOLA_ENTURMACAO',
         escola4='4_CENSO_ESCOLA_ENTURMACAO') %>% 
  ungroup()

dup2_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(NM_MUNICIPIO_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NM_MUNICIPIO_ENTURMACAO) %>% 
  fill('1_NM_MUNICIPIO_ENTURMACAO', '2_NM_MUNICIPIO_ENTURMACAO', '3_NM_MUNICIPIO_ENTURMACAO', '4_NM_MUNICIPIO_ENTURMACAO', 
       .direction = "downup") %>% 
  rename(munic1='1_NM_MUNICIPIO_ENTURMACAO',munic2='2_NM_MUNICIPIO_ENTURMACAO', munic3='3_NM_MUNICIPIO_ENTURMACAO',
         munic4='4_NM_MUNICIPIO_ENTURMACAO') %>% 
  ungroup()

dup3_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(ID_TURNO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURNO) %>% 
  fill('1_ID_TURNO', '2_ID_TURNO', '3_ID_TURNO', '4_ID_TURNO', .direction = "downup") %>% 
  rename(turno1='1_ID_TURNO',turno2='2_ID_TURNO', turno3='3_ID_TURNO', turno4='4_ID_TURNO') %>% 
  ungroup()

dup4_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(ID_TURMA, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURMA) %>% 
  fill('1_ID_TURMA', '2_ID_TURMA', '3_ID_TURMA', '4_ID_TURMA', .direction = "downup") %>% 
  rename(turma1='1_ID_TURMA',turma2='2_ID_TURMA', turma3='3_ID_TURMA', turma4='4_ID_TURMA') %>% 
  ungroup()

dup1_4X_seges2016_1 <- dup1_4X_seges2016_1 %>%
  mutate(switch_escola=ifelse(escola1!=escola2 | escola2!=escola3 | escola3!=escola4 | escola4!=escola1, 1, 0))

dup2_4X_seges2016_1 <- dup2_4X_seges2016_1 %>%
  mutate(switch_munic=ifelse(munic1!=munic2 | munic2!=munic3 | munic3!=munic4 | munic4!=munic1, 1, 0))

dup3_4X_seges2016_1 <- dup3_4X_seges2016_1 %>%
  mutate(switch_turno=ifelse(turno1!=turno2 | turno2!=turno3 | turno3!=turno4 | turno4!=turno1, 1, 0))

dup4_4X_seges2016_1 <- dup4_4X_seges2016_1 %>%
  mutate(switch_turma=ifelse(turma1!=turma2 | turma2!=turma3 | turma3!=turma4 | turma4!=turma1, 1, 0))

# numero de escolas, munic, turnos e turmas
dup1_4X_seges2016_1$qt_escolas <- apply(
  subset(dup1_4X_seges2016_1, select = escola1:escola4), 1,
  function(z) length(unique(z)))
dup2_4X_seges2016_1$qt_munic <- apply(
  subset(dup2_4X_seges2016_1, select = munic1:munic4), 1,
  function(z) length(unique(z)))
dup3_4X_seges2016_1$qt_turno <- apply(
  subset(dup3_4X_seges2016_1, select = turno1:turno4), 1,
  function(z) length(unique(z)))
dup4_4X_seges2016_1$qt_turma <- apply(
  subset(dup4_4X_seges2016_1, select = turma1:turma4), 1,
  function(z) length(unique(z)))

# numero de mudancas
changes <- function(z) if (length(z)) sum(z[-1] != z[-length(z)]) else 0L
dup1_4X_seges2016_1$qt_switch_escola <- apply(subset(dup1_4X_seges2016_1, select = escola1:escola4), 1, changes)
dup2_4X_seges2016_1$qt_switch_munic <- apply(subset(dup2_4X_seges2016_1, select = munic1:munic4), 1, changes)
dup3_4X_seges2016_1$qt_switch_turno <- apply(subset(dup3_4X_seges2016_1, select = turno1:turno4), 1, changes)
dup4_4X_seges2016_1$qt_switch_turma <- apply(subset(dup4_4X_seges2016_1, select = turma1:turma4), 1, changes)

dup_4X_seges2016_1 <- dup1_4X_seges2016_1 %>% 
  left_join(dup2_4X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic)
dup_4X_seges2016_1 <- dup_4X_seges2016_1 %>% 
  left_join(dup3_4X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno, qt_switch_turno)
dup_4X_seges2016_1 <- dup_4X_seges2016_1 %>% 
  left_join(dup4_4X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno,
         qt_switch_turno, switch_turma, qt_turma, qt_switch_turma)
dup_count4X_seges2016_1 <- dup_count4X_seges2016_1 %>% 
  left_join(dup_4X_seges2016_1, by = "RA")

rm(dup_4X_seges2016_1, dup1_4X_seges2016_1, dup2_4X_seges2016_1, dup3_4X_seges2016_1, dup4_4X_seges2016_1)

######## DUP_3X ##############

dup1_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(CENSO_ESCOLA_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CENSO_ESCOLA_ENTURMACAO) %>% 
  fill('1_CENSO_ESCOLA_ENTURMACAO', '2_CENSO_ESCOLA_ENTURMACAO', '3_CENSO_ESCOLA_ENTURMACAO',
       .direction = "downup") %>% 
  rename(escola1='1_CENSO_ESCOLA_ENTURMACAO',escola2='2_CENSO_ESCOLA_ENTURMACAO', escola3='3_CENSO_ESCOLA_ENTURMACAO') %>% 
  ungroup()

dup2_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(NM_MUNICIPIO_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NM_MUNICIPIO_ENTURMACAO) %>% 
  fill('1_NM_MUNICIPIO_ENTURMACAO', '2_NM_MUNICIPIO_ENTURMACAO', '3_NM_MUNICIPIO_ENTURMACAO',
       .direction = "downup") %>% 
  rename(munic1='1_NM_MUNICIPIO_ENTURMACAO',munic2='2_NM_MUNICIPIO_ENTURMACAO', munic3='3_NM_MUNICIPIO_ENTURMACAO') %>% 
  ungroup()

dup3_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(ID_TURNO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURNO) %>% 
  fill('1_ID_TURNO', '2_ID_TURNO', '3_ID_TURNO', .direction = "downup") %>% 
  rename(turno1='1_ID_TURNO',turno2='2_ID_TURNO', turno3='3_ID_TURNO') %>% 
  ungroup()

dup4_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(ID_TURMA, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURMA) %>% 
  fill('1_ID_TURMA', '2_ID_TURMA', '3_ID_TURMA', .direction = "downup") %>% 
  rename(turma1='1_ID_TURMA',turma2='2_ID_TURMA', turma3='3_ID_TURMA') %>% 
  ungroup()

dup1_3X_seges2016_1 <- dup1_3X_seges2016_1 %>%
  mutate(switch_escola=ifelse(escola1!=escola2 | escola2!=escola3 | escola3!=escola1, 1, 0))

dup2_3X_seges2016_1 <- dup2_3X_seges2016_1 %>%
  mutate(switch_munic=ifelse(munic1!=munic2 | munic2!=munic3 | munic3!=munic1, 1, 0))

dup3_3X_seges2016_1 <- dup3_3X_seges2016_1 %>%
  mutate(switch_turno=ifelse(turno1!=turno2 | turno2!=turno3 | turno3!=turno1, 1, 0))

dup4_3X_seges2016_1 <- dup4_3X_seges2016_1 %>%
  mutate(switch_turma=ifelse(turma1!=turma2 | turma2!=turma3 | turma3!=turma1, 1, 0))

# numero de escolas, munic, turnos e turmas
dup1_3X_seges2016_1$qt_escolas <- apply(
  subset(dup1_3X_seges2016_1, select = escola1:escola3), 1,
  function(z) length(unique(z)))
dup2_3X_seges2016_1$qt_munic <- apply(
  subset(dup2_3X_seges2016_1, select = munic1:munic3), 1,
  function(z) length(unique(z)))
dup3_3X_seges2016_1$qt_turno <- apply(
  subset(dup3_3X_seges2016_1, select = turno1:turno3), 1,
  function(z) length(unique(z)))
dup4_3X_seges2016_1$qt_turma <- apply(
  subset(dup4_3X_seges2016_1, select = turma1:turma3), 1,
  function(z) length(unique(z)))

# numero de mudancas
changes <- function(z) if (length(z)) sum(z[-1] != z[-length(z)]) else 0L
dup1_3X_seges2016_1$qt_switch_escola <- apply(subset(dup1_3X_seges2016_1, select = escola1:escola3), 1, changes)
dup2_3X_seges2016_1$qt_switch_munic <- apply(subset(dup2_3X_seges2016_1, select = munic1:munic3), 1, changes)
dup3_3X_seges2016_1$qt_switch_turno <- apply(subset(dup3_3X_seges2016_1, select = turno1:turno3), 1, changes)
dup4_3X_seges2016_1$qt_switch_turma <- apply(subset(dup4_3X_seges2016_1, select = turma1:turma3), 1, changes)

dup_3X_seges2016_1 <- dup1_3X_seges2016_1 %>% 
  left_join(dup2_3X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic)
dup_3X_seges2016_1 <- dup_3X_seges2016_1 %>% 
  left_join(dup3_3X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno, qt_switch_turno)
dup_3X_seges2016_1 <- dup_3X_seges2016_1 %>% 
  left_join(dup4_3X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno,
         qt_switch_turno, switch_turma, qt_turma, qt_switch_turma)
dup_count3X_seges2016_1 <- dup_count3X_seges2016_1 %>% 
  left_join(dup_3X_seges2016_1, by = "RA")

rm(dup_3X_seges2016_1, dup1_3X_seges2016_1, dup2_3X_seges2016_1, dup3_3X_seges2016_1, dup4_3X_seges2016_1)

######## DUP_2X ##############

dup1_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(CENSO_ESCOLA_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CENSO_ESCOLA_ENTURMACAO) %>% 
  fill('1_CENSO_ESCOLA_ENTURMACAO', '2_CENSO_ESCOLA_ENTURMACAO',
       .direction = "downup") %>% 
  rename(escola1='1_CENSO_ESCOLA_ENTURMACAO',escola2='2_CENSO_ESCOLA_ENTURMACAO') %>% 
  ungroup()

dup2_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(NM_MUNICIPIO_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NM_MUNICIPIO_ENTURMACAO) %>% 
  fill('1_NM_MUNICIPIO_ENTURMACAO', '2_NM_MUNICIPIO_ENTURMACAO',
       .direction = "downup") %>% 
  rename(munic1='1_NM_MUNICIPIO_ENTURMACAO',munic2='2_NM_MUNICIPIO_ENTURMACAO') %>% 
  ungroup()

dup3_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(ID_TURNO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURNO) %>% 
  fill('1_ID_TURNO', '2_ID_TURNO', .direction = "downup") %>% 
  rename(turno1='1_ID_TURNO',turno2='2_ID_TURNO') %>% 
  ungroup()

dup4_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(ID_TURMA, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURMA) %>% 
  fill('1_ID_TURMA', '2_ID_TURMA', .direction = "downup") %>% 
  rename(turma1='1_ID_TURMA',turma2='2_ID_TURMA') %>% 
  ungroup()

dup1_2X_seges2016_1 <- dup1_2X_seges2016_1 %>%
  mutate(switch_escola=ifelse(escola1!=escola2, 1, 0))

dup2_2X_seges2016_1 <- dup2_2X_seges2016_1 %>%
  mutate(switch_munic=ifelse(munic1!=munic2, 1, 0))

dup3_2X_seges2016_1 <- dup3_2X_seges2016_1 %>%
  mutate(switch_turno=ifelse(turno1!=turno2, 1, 0))

dup4_2X_seges2016_1 <- dup4_2X_seges2016_1 %>%
  mutate(switch_turma=ifelse(turma1!=turma2, 1, 0))

# numero de escolas, munic, turnos e turmas
dup1_2X_seges2016_1$qt_escolas <- apply(
  subset(dup1_2X_seges2016_1, select = escola1:escola2), 1,
  function(z) length(unique(z)))
dup2_2X_seges2016_1$qt_munic <- apply(
  subset(dup2_2X_seges2016_1, select = munic1:munic2), 1,
  function(z) length(unique(z)))
dup3_2X_seges2016_1$qt_turno <- apply(
  subset(dup3_2X_seges2016_1, select = turno1:turno2), 1,
  function(z) length(unique(z)))
dup4_2X_seges2016_1$qt_turma <- apply(
  subset(dup4_2X_seges2016_1, select = turma1:turma2), 1,
  function(z) length(unique(z)))

# numero de mudancas
changes <- function(z) if (length(z)) sum(z[-1] != z[-length(z)]) else 0L
dup1_2X_seges2016_1$qt_switch_escola <- apply(subset(dup1_2X_seges2016_1, select = escola1:escola2), 1, changes)
dup2_2X_seges2016_1$qt_switch_munic <- apply(subset(dup2_2X_seges2016_1, select = munic1:munic2), 1, changes)
dup3_2X_seges2016_1$qt_switch_turno <- apply(subset(dup3_2X_seges2016_1, select = turno1:turno2), 1, changes)
dup4_2X_seges2016_1$qt_switch_turma <- apply(subset(dup4_2X_seges2016_1, select = turma1:turma2), 1, changes)

dup_2X_seges2016_1 <- dup1_2X_seges2016_1 %>% 
  left_join(dup2_2X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic)
dup_2X_seges2016_1 <- dup_2X_seges2016_1 %>% 
  left_join(dup3_2X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno, qt_switch_turno)
dup_2X_seges2016_1 <- dup_2X_seges2016_1 %>% 
  left_join(dup4_2X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno,
         qt_switch_turno, switch_turma, qt_turma, qt_switch_turma)
dup_count2X_seges2016_1 <- dup_count2X_seges2016_1 %>% 
  left_join(dup_2X_seges2016_1, by = "RA")

rm(dup_2X_seges2016_1, dup1_2X_seges2016_1, dup2_2X_seges2016_1, dup3_2X_seges2016_1, dup4_2X_seges2016_1)

################ CRIANDO AS MESMAS VARIAVEIS PARA OS NAO-DUPLICADOS #####################
#########################################################################################

NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1 %>% group_by(RA) %>% mutate(id = row_number()) %>% 
  ungroup()

dup_count_seges2016_1 <- as.data.table(table(dup_seges_2016_fill1$RA))
RA_group_3X <- subset(dup_count_seges2016_1, N==3)
RA_group_4X <- subset(dup_count_seges2016_1, N==4)
RA_group_2X <- subset(dup_count_seges2016_1, N==2)

NOdup_seges_2016_fill1$dup_count <-
  ifelse(!NOdup_seges_2016_fill1$RA %in% RA_group_2X | !NOdup_seges_2016_fill1$RA %in% RA_group_3X | !NOdup_seges_2016_fill1$RA %in% RA_group_4X, 0, 6)

NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1 %>%
  mutate(EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(soc_aula_1_tri) |!is.na(fil_aula_1_tri) |!is.na(lp_aula_1_tri) |
                                           !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |!is.na(fis_aula_1_tri) |!is.na(qui_aula_1_tri) |!is.na(bio_aula_1_tri) |
                                           !is.na(mat_aula_1_tri), 1, 0), 0),
         EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas1tri_semNA),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(soc_aula_2_tri) |!is.na(fil_aula_2_tri) |!is.na(lp_aula_2_tri) |
                                           !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |!is.na(fis_aula_2_tri) |!is.na(qui_aula_2_tri) |!is.na(bio_aula_2_tri) |
                                           !is.na(mat_aula_2_tri), 1, 0), 0),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas2tri_semNA),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(soc_aula_3_tri) |!is.na(fil_aula_3_tri) |!is.na(lp_aula_3_tri) |
                                           !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |!is.na(fis_aula_3_tri) |!is.na(qui_aula_3_tri) |!is.na(bio_aula_3_tri) |
                                           !is.na(mat_aula_3_tri), 1, 0), 0),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas3tri_semNA),
         EMaulas_semNA=ifelse(EMaulas1tri_semNA==1 & EMaulas2tri_semNA==1 & EMaulas3tri_semNA==1, 1, 0),
         EMaulas_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas_semNA),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(lp_aula_1_tri) | !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |
                                           !is.na(mat_aula_1_tri) |!is.na(cien_aula_1_tri) , 1, 0), 0),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas1tri_semNA),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(lp_aula_2_tri) | !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |
                                           !is.na(mat_aula_2_tri) |!is.na(cien_aula_2_tri) , 1, 0), 0),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas2tri_semNA),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(lp_aula_3_tri) | !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |
                                           !is.na(mat_aula_3_tri) |!is.na(cien_aula_3_tri) , 1, 0), 0),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas3tri_semNA),
         EFaulas_semNA=ifelse(EFaulas1tri_semNA==1 & EFaulas2tri_semNA==1 & EFaulas3tri_semNA==1, 1, 0),
         EFaulas_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas_semNA))


NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1 %>%
  mutate(switch_escola=ifelse(!is.na(CENSO_ESCOLA_ENTURMACAO), 0, as.numeric(NA)),
         qt_escolas=ifelse(!is.na(CENSO_ESCOLA_ENTURMACAO), 1, as.numeric(NA)),
         qt_switch_escola=ifelse(!is.na(CENSO_ESCOLA_ENTURMACAO), 0, as.numeric(NA)),
         switch_munic=ifelse(!is.na(NM_MUNICIPIO_ENTURMACAO), 0, as.numeric(NA)),
         qt_switch_munic=ifelse(!is.na(NM_MUNICIPIO_ENTURMACAO), 0, as.numeric(NA)),
         qt_munic=ifelse(!is.na(NM_MUNICIPIO_ENTURMACAO), 1, as.numeric(NA)),
         switch_turno=ifelse(!is.na(ID_TURNO), 0, as.numeric(NA)),
         qt_turno=ifelse(!is.na(ID_TURNO), 1, as.numeric(NA)),
         qt_switch_turno=ifelse(!is.na(ID_TURNO), 0, as.numeric(NA)),
         switch_turma=ifelse(!is.na(ID_TURMA), 0, as.numeric(NA)),
         qt_turma=ifelse(!is.na(ID_TURMA), 1, as.numeric(NA)),
         qt_switch_turma=ifelse(!is.na(ID_TURMA), 0, as.numeric(NA)))

############### Unindo duplicados e naooduplicados ##################
rm(changes, RA_group_2X, RA_group_3X, RA_group_4X, RA_group_5X, seges_2016_fill1, dup_seges_2016_fill1)

colnames(dup_count2X_seges2016_1)
colnames(NOdup_seges_2016_fill1)

NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                     "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                     "CD_INEP_ALUNO","hist_aula_2_tri","hist_aula_1_tri","hist_aula_3_tri","geo_aula_3_tri","geo_aula_1_tri",
                                                     "geo_aula_2_tri","soc_aula_2_tri","soc_aula_1_tri","soc_aula_3_tri","fil_aula_3_tri",
                                                     "fil_aula_2_tri","art_aula_rec_2_tri","bio_aula_rec_2_tri","ed_fis_aula_rec_2_tri","ed_fis_aula_rec_1_tri",
                                                     "fil_aula_rec_2_tri","fis_aula_rec_2_tri","fis_aula_rec_1_tri","geo_aula_rec_2_tri","geo_aula_rec_1_tri",
                                                     "hist_aula_rec_1_tri","hist_aula_rec_2_tri","lp_aula_rec_1_tri","mat_aula_rec_2_tri","mat_aula_rec_1_tri",
                                                     "qui_aula_rec_2_tri","qui_aula_rec_1_tri","soc_aula_rec_1_tri","lp_aula_2_tri","lp_aula_1_tri",
                                                     "lp_aula_3_tri","ed_fis_aula_2_tri","art_aula_2_tri","art_aula_1_tri","fis_aula_3_tri",
                                                     "fis_aula_2_tri","fis_aula_1_tri","qui_aula_2_tri","qui_aula_3_tri","qui_aula_1_tri",
                                                     "bio_aula_1_tri","mat_aula_1_tri","mat_aula_3_tri","art_aula_rec_1_tri","bio_aula_rec_1_tri",
                                                     "fil_aula_rec_1_tri","lp_aula_rec_2_tri","soc_aula_rec_2_tri","ed_fis_aula_3_tri","ed_fis_aula_1_tri",
                                                     "art_aula_3_tri","bio_aula_2_tri","bio_aula_3_tri","mat_aula_2_tri","fil_aula_1_tri",
                                                     "cien_aula_3_tri","cien_aula_2_tri","cien_aula_1_tri","cien_aula_rec_2_tri","cien_aula_rec_1_tri","id","dup_count","EMaulas1tri_semNA",
                                                     "EMaulas2tri_semNA","EMaulas3tri_semNA","EMaulas_semNA","EFaulas1tri_semNA","EFaulas2tri_semNA","EFaulas3tri_semNA",
                                                     "EFaulas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                     "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]

dup_count2X_seges2016_1 <- dup_count2X_seges2016_1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                       "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                       "CD_INEP_ALUNO","hist_aula_2_tri","hist_aula_1_tri","hist_aula_3_tri","geo_aula_3_tri","geo_aula_1_tri",
                                                       "geo_aula_2_tri","soc_aula_2_tri","soc_aula_1_tri","soc_aula_3_tri","fil_aula_3_tri",
                                                       "fil_aula_2_tri","art_aula_rec_2_tri","bio_aula_rec_2_tri","ed_fis_aula_rec_2_tri","ed_fis_aula_rec_1_tri",
                                                       "fil_aula_rec_2_tri","fis_aula_rec_2_tri","fis_aula_rec_1_tri","geo_aula_rec_2_tri","geo_aula_rec_1_tri",
                                                       "hist_aula_rec_1_tri","hist_aula_rec_2_tri","lp_aula_rec_1_tri","mat_aula_rec_2_tri","mat_aula_rec_1_tri",
                                                       "qui_aula_rec_2_tri","qui_aula_rec_1_tri","soc_aula_rec_1_tri","lp_aula_2_tri","lp_aula_1_tri",
                                                       "lp_aula_3_tri","ed_fis_aula_2_tri","art_aula_2_tri","art_aula_1_tri","fis_aula_3_tri",
                                                       "fis_aula_2_tri","fis_aula_1_tri","qui_aula_2_tri","qui_aula_3_tri","qui_aula_1_tri",
                                                       "bio_aula_1_tri","mat_aula_1_tri","mat_aula_3_tri","art_aula_rec_1_tri","bio_aula_rec_1_tri",
                                                       "fil_aula_rec_1_tri","lp_aula_rec_2_tri","soc_aula_rec_2_tri","ed_fis_aula_3_tri","ed_fis_aula_1_tri",
                                                       "art_aula_3_tri","bio_aula_2_tri","bio_aula_3_tri","mat_aula_2_tri","fil_aula_1_tri",
                                                       "cien_aula_3_tri","cien_aula_2_tri","cien_aula_1_tri","cien_aula_rec_2_tri","cien_aula_rec_1_tri","id","dup_count","EMaulas1tri_semNA",
                                                       "EMaulas2tri_semNA","EMaulas3tri_semNA","EMaulas_semNA","EFaulas1tri_semNA","EFaulas2tri_semNA","EFaulas3tri_semNA",
                                                       "EFaulas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                       "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]

dup_count3X_seges2016_1 <- dup_count3X_seges2016_1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                       "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                       "CD_INEP_ALUNO","hist_aula_2_tri","hist_aula_1_tri","hist_aula_3_tri","geo_aula_3_tri","geo_aula_1_tri",
                                                       "geo_aula_2_tri","soc_aula_2_tri","soc_aula_1_tri","soc_aula_3_tri","fil_aula_3_tri",
                                                       "fil_aula_2_tri","art_aula_rec_2_tri","bio_aula_rec_2_tri","ed_fis_aula_rec_2_tri","ed_fis_aula_rec_1_tri",
                                                       "fil_aula_rec_2_tri","fis_aula_rec_2_tri","fis_aula_rec_1_tri","geo_aula_rec_2_tri","geo_aula_rec_1_tri",
                                                       "hist_aula_rec_1_tri","hist_aula_rec_2_tri","lp_aula_rec_1_tri","mat_aula_rec_2_tri","mat_aula_rec_1_tri",
                                                       "qui_aula_rec_2_tri","qui_aula_rec_1_tri","soc_aula_rec_1_tri","lp_aula_2_tri","lp_aula_1_tri",
                                                       "lp_aula_3_tri","ed_fis_aula_2_tri","art_aula_2_tri","art_aula_1_tri","fis_aula_3_tri",
                                                       "fis_aula_2_tri","fis_aula_1_tri","qui_aula_2_tri","qui_aula_3_tri","qui_aula_1_tri",
                                                       "bio_aula_1_tri","mat_aula_1_tri","mat_aula_3_tri","art_aula_rec_1_tri","bio_aula_rec_1_tri",
                                                       "fil_aula_rec_1_tri","lp_aula_rec_2_tri","soc_aula_rec_2_tri","ed_fis_aula_3_tri","ed_fis_aula_1_tri",
                                                       "art_aula_3_tri","bio_aula_2_tri","bio_aula_3_tri","mat_aula_2_tri","fil_aula_1_tri",
                                                       "cien_aula_3_tri","cien_aula_2_tri","cien_aula_1_tri","cien_aula_rec_2_tri","cien_aula_rec_1_tri","id","dup_count","EMaulas1tri_semNA",
                                                       "EMaulas2tri_semNA","EMaulas3tri_semNA","EMaulas_semNA","EFaulas1tri_semNA","EFaulas2tri_semNA","EFaulas3tri_semNA",
                                                       "EFaulas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                       "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]

dup_count4X_seges2016_1 <- dup_count4X_seges2016_1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                       "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                       "CD_INEP_ALUNO","hist_aula_2_tri","hist_aula_1_tri","hist_aula_3_tri","geo_aula_3_tri","geo_aula_1_tri",
                                                       "geo_aula_2_tri","soc_aula_2_tri","soc_aula_1_tri","soc_aula_3_tri","fil_aula_3_tri",
                                                       "fil_aula_2_tri","art_aula_rec_2_tri","bio_aula_rec_2_tri","ed_fis_aula_rec_2_tri","ed_fis_aula_rec_1_tri",
                                                       "fil_aula_rec_2_tri","fis_aula_rec_2_tri","fis_aula_rec_1_tri","geo_aula_rec_2_tri","geo_aula_rec_1_tri",
                                                       "hist_aula_rec_1_tri","hist_aula_rec_2_tri","lp_aula_rec_1_tri","mat_aula_rec_2_tri","mat_aula_rec_1_tri",
                                                       "qui_aula_rec_2_tri","qui_aula_rec_1_tri","soc_aula_rec_1_tri","lp_aula_2_tri","lp_aula_1_tri",
                                                       "lp_aula_3_tri","ed_fis_aula_2_tri","art_aula_2_tri","art_aula_1_tri","fis_aula_3_tri",
                                                       "fis_aula_2_tri","fis_aula_1_tri","qui_aula_2_tri","qui_aula_3_tri","qui_aula_1_tri",
                                                       "bio_aula_1_tri","mat_aula_1_tri","mat_aula_3_tri","art_aula_rec_1_tri","bio_aula_rec_1_tri",
                                                       "fil_aula_rec_1_tri","lp_aula_rec_2_tri","soc_aula_rec_2_tri","ed_fis_aula_3_tri","ed_fis_aula_1_tri",
                                                       "art_aula_3_tri","bio_aula_2_tri","bio_aula_3_tri","mat_aula_2_tri","fil_aula_1_tri",
                                                       "cien_aula_3_tri","cien_aula_2_tri","cien_aula_1_tri","cien_aula_rec_2_tri","cien_aula_rec_1_tri","id","dup_count","EMaulas1tri_semNA",
                                                       "EMaulas2tri_semNA","EMaulas3tri_semNA","EMaulas_semNA","EFaulas1tri_semNA","EFaulas2tri_semNA","EFaulas3tri_semNA",
                                                       "EFaulas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                       "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]


dup_seges_2016_fill1 <- rbind(NOdup_seges_2016_fill1, dup_count2X_seges2016_1, dup_count3X_seges2016_1, dup_count4X_seges2016_1)

saveRDS(dup_seges_2016_fill1, 'seges2020aulas_dup_postTRATAMENTO')

rm(NOdup_seges_2016_fill1, dup_count2X_seges2016_1, dup_count3X_seges2016_1, dup_count4X_seges2016_1,dup_seges_2016_fill1, seges_2016_fill1)

################### TRATANDO DUPLICADOS: FALTAS #####################

seges_2016_fill1 <-readRDS('seges_2020_faltas')

# todos os duplicados
dup_seges_2016_fill1 <- seges_2016_fill1[duplicated(seges_2016_fill1[, 10]) | duplicated(seges_2016_fill1[, 10], fromLast=T),]
describe(dup_seges_2016_fill1$RA)

NOdup_seges_2016_fill1 <- seges_2016_fill1 %>%  filter(!RA %in% dup_seges_2016_fill1$RA)
describe(NOdup_seges_2016_fill1$RA)

# numerando duplicados
dup_seges_2016_fill1 <- dup_seges_2016_fill1 %>% group_by(RA) %>% mutate(id = row_number()) %>% 
  ungroup()

describe(dup_seges_2016_fill1$id)

dup_count_seges2016_1 <- as.data.table(table(dup_seges_2016_fill1$RA))
RA_group_3X <- subset(dup_count_seges2016_1, N==3)
RA_group_4X <- subset(dup_count_seges2016_1, N==4)
RA_group_2X <- subset(dup_count_seges2016_1, N==2)

dup_seges_2016_fill1$dup_count <-
  ifelse(dup_seges_2016_fill1$RA %in% RA_group_3X$V1, 3,
         ifelse(dup_seges_2016_fill1$RA %in% RA_group_4X$V1, 4, 2))

describe(dup_seges_2016_fill1$dup_count)
colnames(dup_seges_2016_fill1)

dup_seges_2016_fill1 <- dup_seges_2016_fill1 %>%
  mutate(EMfaltas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                   ifelse(!is.na(hist_falta_1_tri) |!is.na(geo_falta_1_tri) |!is.na(soc_falta_1_tri) |!is.na(fil_falta_1_tri) |!is.na(lp_falta_1_tri) |
                                            !is.na(ed_fis_falta_1_tri) |!is.na(art_falta_1_tri) |!is.na(fis_falta_1_tri) |!is.na(qui_falta_1_tri) |!is.na(bio_falta_1_tri) |
                                            !is.na(mat_falta_1_tri), 1, 0), 0),
         EMfaltas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMfaltas1tri_semNA),
         EMfaltas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                   ifelse(!is.na(hist_falta_2_tri) |!is.na(geo_falta_2_tri) |!is.na(soc_falta_2_tri) |!is.na(fil_falta_2_tri) |!is.na(lp_falta_2_tri) |
                                            !is.na(ed_fis_falta_2_tri) |!is.na(art_falta_2_tri) |!is.na(fis_falta_2_tri) |!is.na(qui_falta_2_tri) |!is.na(bio_falta_2_tri) |
                                            !is.na(mat_falta_2_tri), 1, 0), 0),
         EMfaltas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMfaltas2tri_semNA),
         EMfaltas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                   ifelse(!is.na(hist_falta_3_tri) |!is.na(geo_falta_3_tri) |!is.na(soc_falta_3_tri) |!is.na(fil_falta_3_tri) |!is.na(lp_falta_3_tri) |
                                            !is.na(ed_fis_falta_3_tri) |!is.na(art_falta_3_tri) |!is.na(fis_falta_3_tri) |!is.na(qui_falta_3_tri) |!is.na(bio_falta_3_tri) |
                                            !is.na(mat_falta_3_tri), 1, 0), 0),
         EMfaltas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMfaltas3tri_semNA),
         EMfaltas_semNA=ifelse(EMfaltas1tri_semNA==1 & EMfaltas2tri_semNA==1 & EMfaltas3tri_semNA==1, 1, 0),
         EMfaltas_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMfaltas_semNA),
         EFfaltas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                   ifelse(!is.na(hist_falta_1_tri) |!is.na(geo_falta_1_tri) |!is.na(lp_falta_1_tri) | !is.na(ed_fis_falta_1_tri) |!is.na(art_falta_1_tri) |
                                            !is.na(mat_falta_1_tri) |!is.na(cien_falta_1_tri) , 1, 0), 0),
         EFfaltas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFfaltas1tri_semNA),
         EFfaltas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                   ifelse(!is.na(hist_falta_2_tri) |!is.na(geo_falta_2_tri) |!is.na(lp_falta_2_tri) | !is.na(ed_fis_falta_2_tri) |!is.na(art_falta_2_tri) |
                                            !is.na(mat_falta_2_tri) |!is.na(cien_falta_2_tri) , 1, 0), 0),
         EFfaltas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFfaltas2tri_semNA),
         EFfaltas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                   ifelse(!is.na(hist_falta_3_tri) |!is.na(geo_falta_3_tri) |!is.na(lp_falta_3_tri) | !is.na(ed_fis_falta_3_tri) |!is.na(art_falta_3_tri) |
                                            !is.na(mat_falta_3_tri) |!is.na(cien_falta_3_tri) , 1, 0), 0),
         EFfaltas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFfaltas3tri_semNA),
         EFfaltas_semNA=ifelse(EFfaltas1tri_semNA==1 & EFfaltas2tri_semNA==1 & EFfaltas3tri_semNA==1, 1, 0),
         EFfaltas_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFfaltas_semNA))

dup_count4X_seges2016_1 <- subset(dup_seges_2016_fill1, dup_count==4)
dup_count3X_seges2016_1 <- subset(dup_seges_2016_fill1, dup_count==3)
dup_count2X_seges2016_1 <- subset(dup_seges_2016_fill1, dup_count==2)
rm(dup_count_seges2016_1, RA_group_3X, RA_group_4X, RA_group_5X, RA_group_2X, seges_2016)
colnames(dup_count5X_seges2016_1)

#######################################################
######### CRIANDO VARIAVEIS PARA AS DUPLICACOES #######
######## DUP_4X ##############

dup1_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(CENSO_ESCOLA_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CENSO_ESCOLA_ENTURMACAO) %>% 
  fill('1_CENSO_ESCOLA_ENTURMACAO', '2_CENSO_ESCOLA_ENTURMACAO', '3_CENSO_ESCOLA_ENTURMACAO', '4_CENSO_ESCOLA_ENTURMACAO',
       .direction = "downup") %>% 
  rename(escola1='1_CENSO_ESCOLA_ENTURMACAO',escola2='2_CENSO_ESCOLA_ENTURMACAO', escola3='3_CENSO_ESCOLA_ENTURMACAO',
         escola4='4_CENSO_ESCOLA_ENTURMACAO') %>% 
  ungroup()

dup2_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(NM_MUNICIPIO_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NM_MUNICIPIO_ENTURMACAO) %>% 
  fill('1_NM_MUNICIPIO_ENTURMACAO', '2_NM_MUNICIPIO_ENTURMACAO', '3_NM_MUNICIPIO_ENTURMACAO', '4_NM_MUNICIPIO_ENTURMACAO', 
       .direction = "downup") %>% 
  rename(munic1='1_NM_MUNICIPIO_ENTURMACAO',munic2='2_NM_MUNICIPIO_ENTURMACAO', munic3='3_NM_MUNICIPIO_ENTURMACAO',
         munic4='4_NM_MUNICIPIO_ENTURMACAO') %>% 
  ungroup()

dup3_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(ID_TURNO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURNO) %>% 
  fill('1_ID_TURNO', '2_ID_TURNO', '3_ID_TURNO', '4_ID_TURNO', .direction = "downup") %>% 
  rename(turno1='1_ID_TURNO',turno2='2_ID_TURNO', turno3='3_ID_TURNO', turno4='4_ID_TURNO') %>% 
  ungroup()

dup4_4X_seges2016_1 <- dup_count4X_seges2016_1 %>%
  select(ID_TURMA, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURMA) %>% 
  fill('1_ID_TURMA', '2_ID_TURMA', '3_ID_TURMA', '4_ID_TURMA', .direction = "downup") %>% 
  rename(turma1='1_ID_TURMA',turma2='2_ID_TURMA', turma3='3_ID_TURMA', turma4='4_ID_TURMA') %>% 
  ungroup()

dup1_4X_seges2016_1 <- dup1_4X_seges2016_1 %>%
  mutate(switch_escola=ifelse(escola1!=escola2 | escola2!=escola3 | escola3!=escola4 | escola4!=escola1, 1, 0))

dup2_4X_seges2016_1 <- dup2_4X_seges2016_1 %>%
  mutate(switch_munic=ifelse(munic1!=munic2 | munic2!=munic3 | munic3!=munic4 | munic4!=munic1, 1, 0))

dup3_4X_seges2016_1 <- dup3_4X_seges2016_1 %>%
  mutate(switch_turno=ifelse(turno1!=turno2 | turno2!=turno3 | turno3!=turno4 | turno4!=turno1, 1, 0))

dup4_4X_seges2016_1 <- dup4_4X_seges2016_1 %>%
  mutate(switch_turma=ifelse(turma1!=turma2 | turma2!=turma3 | turma3!=turma4 | turma4!=turma1, 1, 0))

# numero de escolas, munic, turnos e turmas
dup1_4X_seges2016_1$qt_escolas <- apply(
  subset(dup1_4X_seges2016_1, select = escola1:escola4), 1,
  function(z) length(unique(z)))
dup2_4X_seges2016_1$qt_munic <- apply(
  subset(dup2_4X_seges2016_1, select = munic1:munic4), 1,
  function(z) length(unique(z)))
dup3_4X_seges2016_1$qt_turno <- apply(
  subset(dup3_4X_seges2016_1, select = turno1:turno4), 1,
  function(z) length(unique(z)))
dup4_4X_seges2016_1$qt_turma <- apply(
  subset(dup4_4X_seges2016_1, select = turma1:turma4), 1,
  function(z) length(unique(z)))

# numero de mudancas
changes <- function(z) if (length(z)) sum(z[-1] != z[-length(z)]) else 0L
dup1_4X_seges2016_1$qt_switch_escola <- apply(subset(dup1_4X_seges2016_1, select = escola1:escola4), 1, changes)
dup2_4X_seges2016_1$qt_switch_munic <- apply(subset(dup2_4X_seges2016_1, select = munic1:munic4), 1, changes)
dup3_4X_seges2016_1$qt_switch_turno <- apply(subset(dup3_4X_seges2016_1, select = turno1:turno4), 1, changes)
dup4_4X_seges2016_1$qt_switch_turma <- apply(subset(dup4_4X_seges2016_1, select = turma1:turma4), 1, changes)

dup_4X_seges2016_1 <- dup1_4X_seges2016_1 %>% 
  left_join(dup2_4X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic)
dup_4X_seges2016_1 <- dup_4X_seges2016_1 %>% 
  left_join(dup3_4X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno, qt_switch_turno)
dup_4X_seges2016_1 <- dup_4X_seges2016_1 %>% 
  left_join(dup4_4X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno,
         qt_switch_turno, switch_turma, qt_turma, qt_switch_turma)
dup_count4X_seges2016_1 <- dup_count4X_seges2016_1 %>% 
  left_join(dup_4X_seges2016_1, by = "RA")

rm(dup_4X_seges2016_1, dup1_4X_seges2016_1, dup2_4X_seges2016_1, dup3_4X_seges2016_1, dup4_4X_seges2016_1)

######## DUP_3X ##############

dup1_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(CENSO_ESCOLA_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CENSO_ESCOLA_ENTURMACAO) %>% 
  fill('1_CENSO_ESCOLA_ENTURMACAO', '2_CENSO_ESCOLA_ENTURMACAO', '3_CENSO_ESCOLA_ENTURMACAO',
       .direction = "downup") %>% 
  rename(escola1='1_CENSO_ESCOLA_ENTURMACAO',escola2='2_CENSO_ESCOLA_ENTURMACAO', escola3='3_CENSO_ESCOLA_ENTURMACAO') %>% 
  ungroup()

dup2_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(NM_MUNICIPIO_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NM_MUNICIPIO_ENTURMACAO) %>% 
  fill('1_NM_MUNICIPIO_ENTURMACAO', '2_NM_MUNICIPIO_ENTURMACAO', '3_NM_MUNICIPIO_ENTURMACAO',
       .direction = "downup") %>% 
  rename(munic1='1_NM_MUNICIPIO_ENTURMACAO',munic2='2_NM_MUNICIPIO_ENTURMACAO', munic3='3_NM_MUNICIPIO_ENTURMACAO') %>% 
  ungroup()

dup3_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(ID_TURNO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURNO) %>% 
  fill('1_ID_TURNO', '2_ID_TURNO', '3_ID_TURNO', .direction = "downup") %>% 
  rename(turno1='1_ID_TURNO',turno2='2_ID_TURNO', turno3='3_ID_TURNO') %>% 
  ungroup()

dup4_3X_seges2016_1 <- dup_count3X_seges2016_1 %>%
  select(ID_TURMA, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURMA) %>% 
  fill('1_ID_TURMA', '2_ID_TURMA', '3_ID_TURMA', .direction = "downup") %>% 
  rename(turma1='1_ID_TURMA',turma2='2_ID_TURMA', turma3='3_ID_TURMA') %>% 
  ungroup()

dup1_3X_seges2016_1 <- dup1_3X_seges2016_1 %>%
  mutate(switch_escola=ifelse(escola1!=escola2 | escola2!=escola3 | escola3!=escola1, 1, 0))

dup2_3X_seges2016_1 <- dup2_3X_seges2016_1 %>%
  mutate(switch_munic=ifelse(munic1!=munic2 | munic2!=munic3 | munic3!=munic1, 1, 0))

dup3_3X_seges2016_1 <- dup3_3X_seges2016_1 %>%
  mutate(switch_turno=ifelse(turno1!=turno2 | turno2!=turno3 | turno3!=turno1, 1, 0))

dup4_3X_seges2016_1 <- dup4_3X_seges2016_1 %>%
  mutate(switch_turma=ifelse(turma1!=turma2 | turma2!=turma3 | turma3!=turma1, 1, 0))

# numero de escolas, munic, turnos e turmas
dup1_3X_seges2016_1$qt_escolas <- apply(
  subset(dup1_3X_seges2016_1, select = escola1:escola3), 1,
  function(z) length(unique(z)))
dup2_3X_seges2016_1$qt_munic <- apply(
  subset(dup2_3X_seges2016_1, select = munic1:munic3), 1,
  function(z) length(unique(z)))
dup3_3X_seges2016_1$qt_turno <- apply(
  subset(dup3_3X_seges2016_1, select = turno1:turno3), 1,
  function(z) length(unique(z)))
dup4_3X_seges2016_1$qt_turma <- apply(
  subset(dup4_3X_seges2016_1, select = turma1:turma3), 1,
  function(z) length(unique(z)))

# numero de mudancas
changes <- function(z) if (length(z)) sum(z[-1] != z[-length(z)]) else 0L
dup1_3X_seges2016_1$qt_switch_escola <- apply(subset(dup1_3X_seges2016_1, select = escola1:escola3), 1, changes)
dup2_3X_seges2016_1$qt_switch_munic <- apply(subset(dup2_3X_seges2016_1, select = munic1:munic3), 1, changes)
dup3_3X_seges2016_1$qt_switch_turno <- apply(subset(dup3_3X_seges2016_1, select = turno1:turno3), 1, changes)
dup4_3X_seges2016_1$qt_switch_turma <- apply(subset(dup4_3X_seges2016_1, select = turma1:turma3), 1, changes)

dup_3X_seges2016_1 <- dup1_3X_seges2016_1 %>% 
  left_join(dup2_3X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic)
dup_3X_seges2016_1 <- dup_3X_seges2016_1 %>% 
  left_join(dup3_3X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno, qt_switch_turno)
dup_3X_seges2016_1 <- dup_3X_seges2016_1 %>% 
  left_join(dup4_3X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno,
         qt_switch_turno, switch_turma, qt_turma, qt_switch_turma)
dup_count3X_seges2016_1 <- dup_count3X_seges2016_1 %>% 
  left_join(dup_3X_seges2016_1, by = "RA")

rm(dup_3X_seges2016_1, dup1_3X_seges2016_1, dup2_3X_seges2016_1, dup3_3X_seges2016_1, dup4_3X_seges2016_1)

######## DUP_2X ##############

dup1_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(CENSO_ESCOLA_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = CENSO_ESCOLA_ENTURMACAO) %>% 
  fill('1_CENSO_ESCOLA_ENTURMACAO', '2_CENSO_ESCOLA_ENTURMACAO',
       .direction = "downup") %>% 
  rename(escola1='1_CENSO_ESCOLA_ENTURMACAO',escola2='2_CENSO_ESCOLA_ENTURMACAO') %>% 
  ungroup()

dup2_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(NM_MUNICIPIO_ENTURMACAO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = NM_MUNICIPIO_ENTURMACAO) %>% 
  fill('1_NM_MUNICIPIO_ENTURMACAO', '2_NM_MUNICIPIO_ENTURMACAO',
       .direction = "downup") %>% 
  rename(munic1='1_NM_MUNICIPIO_ENTURMACAO',munic2='2_NM_MUNICIPIO_ENTURMACAO') %>% 
  ungroup()

dup3_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(ID_TURNO, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURNO) %>% 
  fill('1_ID_TURNO', '2_ID_TURNO', .direction = "downup") %>% 
  rename(turno1='1_ID_TURNO',turno2='2_ID_TURNO') %>% 
  ungroup()

dup4_2X_seges2016_1 <- dup_count2X_seges2016_1 %>%
  select(ID_TURMA, RA, id) %>% 
  group_by(RA) %>%
  pivot_wider(names_from = id, names_glue = "{id}_{.value}", values_from = ID_TURMA) %>% 
  fill('1_ID_TURMA', '2_ID_TURMA', .direction = "downup") %>% 
  rename(turma1='1_ID_TURMA',turma2='2_ID_TURMA') %>% 
  ungroup()

dup1_2X_seges2016_1 <- dup1_2X_seges2016_1 %>%
  mutate(switch_escola=ifelse(escola1!=escola2, 1, 0))

dup2_2X_seges2016_1 <- dup2_2X_seges2016_1 %>%
  mutate(switch_munic=ifelse(munic1!=munic2, 1, 0))

dup3_2X_seges2016_1 <- dup3_2X_seges2016_1 %>%
  mutate(switch_turno=ifelse(turno1!=turno2, 1, 0))

dup4_2X_seges2016_1 <- dup4_2X_seges2016_1 %>%
  mutate(switch_turma=ifelse(turma1!=turma2, 1, 0))

# numero de escolas, munic, turnos e turmas
dup1_2X_seges2016_1$qt_escolas <- apply(
  subset(dup1_2X_seges2016_1, select = escola1:escola2), 1,
  function(z) length(unique(z)))
dup2_2X_seges2016_1$qt_munic <- apply(
  subset(dup2_2X_seges2016_1, select = munic1:munic2), 1,
  function(z) length(unique(z)))
dup3_2X_seges2016_1$qt_turno <- apply(
  subset(dup3_2X_seges2016_1, select = turno1:turno2), 1,
  function(z) length(unique(z)))
dup4_2X_seges2016_1$qt_turma <- apply(
  subset(dup4_2X_seges2016_1, select = turma1:turma2), 1,
  function(z) length(unique(z)))

# numero de mudancas
changes <- function(z) if (length(z)) sum(z[-1] != z[-length(z)]) else 0L
dup1_2X_seges2016_1$qt_switch_escola <- apply(subset(dup1_2X_seges2016_1, select = escola1:escola2), 1, changes)
dup2_2X_seges2016_1$qt_switch_munic <- apply(subset(dup2_2X_seges2016_1, select = munic1:munic2), 1, changes)
dup3_2X_seges2016_1$qt_switch_turno <- apply(subset(dup3_2X_seges2016_1, select = turno1:turno2), 1, changes)
dup4_2X_seges2016_1$qt_switch_turma <- apply(subset(dup4_2X_seges2016_1, select = turma1:turma2), 1, changes)

dup_2X_seges2016_1 <- dup1_2X_seges2016_1 %>% 
  left_join(dup2_2X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic)
dup_2X_seges2016_1 <- dup_2X_seges2016_1 %>% 
  left_join(dup3_2X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno, qt_switch_turno)
dup_2X_seges2016_1 <- dup_2X_seges2016_1 %>% 
  left_join(dup4_2X_seges2016_1, by = "RA") %>% 
  select(RA, switch_escola, qt_escolas, qt_switch_escola, switch_munic, qt_switch_munic, qt_munic, switch_turno, qt_turno,
         qt_switch_turno, switch_turma, qt_turma, qt_switch_turma)
dup_count2X_seges2016_1 <- dup_count2X_seges2016_1 %>% 
  left_join(dup_2X_seges2016_1, by = "RA")

rm(dup_2X_seges2016_1, dup1_2X_seges2016_1, dup2_2X_seges2016_1, dup3_2X_seges2016_1, dup4_2X_seges2016_1)

################ CRIANDO AS MESMAS VARIAVEIS PARA OS NAO-DUPLICADOS #####################
#########################################################################################

NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1 %>% group_by(RA) %>% mutate(id = row_number()) %>% 
  ungroup()

dup_count_seges2016_1 <- as.data.table(table(dup_seges_2016_fill1$RA))
RA_group_3X <- subset(dup_count_seges2016_1, N==3)
RA_group_4X <- subset(dup_count_seges2016_1, N==4)
RA_group_2X <- subset(dup_count_seges2016_1, N==2)

NOdup_seges_2016_fill1$dup_count <-
  ifelse(!NOdup_seges_2016_fill1$RA %in% RA_group_2X | !NOdup_seges_2016_fill1$RA %in% RA_group_3X | !NOdup_seges_2016_fill1$RA %in% RA_group_4X, 0, 6)

NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1 %>%
  mutate(EMfaltas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                   ifelse(!is.na(hist_falta_1_tri) |!is.na(geo_falta_1_tri) |!is.na(soc_falta_1_tri) |!is.na(fil_falta_1_tri) |!is.na(lp_falta_1_tri) |
                                            !is.na(ed_fis_falta_1_tri) |!is.na(art_falta_1_tri) |!is.na(fis_falta_1_tri) |!is.na(qui_falta_1_tri) |!is.na(bio_falta_1_tri) |
                                            !is.na(mat_falta_1_tri), 1, 0), 0),
         EMfaltas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMfaltas1tri_semNA),
         EMfaltas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                   ifelse(!is.na(hist_falta_2_tri) |!is.na(geo_falta_2_tri) |!is.na(soc_falta_2_tri) |!is.na(fil_falta_2_tri) |!is.na(lp_falta_2_tri) |
                                            !is.na(ed_fis_falta_2_tri) |!is.na(art_falta_2_tri) |!is.na(fis_falta_2_tri) |!is.na(qui_falta_2_tri) |!is.na(bio_falta_2_tri) |
                                            !is.na(mat_falta_2_tri), 1, 0), 0),
         EMfaltas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMfaltas2tri_semNA),
         EMfaltas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                   ifelse(!is.na(hist_falta_3_tri) |!is.na(geo_falta_3_tri) |!is.na(soc_falta_3_tri) |!is.na(fil_falta_3_tri) |!is.na(lp_falta_3_tri) |
                                            !is.na(ed_fis_falta_3_tri) |!is.na(art_falta_3_tri) |!is.na(fis_falta_3_tri) |!is.na(qui_falta_3_tri) |!is.na(bio_falta_3_tri) |
                                            !is.na(mat_falta_3_tri), 1, 0), 0),
         EMfaltas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMfaltas3tri_semNA),
         EMfaltas_semNA=ifelse(EMfaltas1tri_semNA==1 & EMfaltas2tri_semNA==1 & EMfaltas3tri_semNA==1, 1, 0),
         EMfaltas_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMfaltas_semNA),
         EFfaltas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                   ifelse(!is.na(hist_falta_1_tri) |!is.na(geo_falta_1_tri) |!is.na(lp_falta_1_tri) | !is.na(ed_fis_falta_1_tri) |!is.na(art_falta_1_tri) |
                                            !is.na(mat_falta_1_tri) |!is.na(cien_falta_1_tri) , 1, 0), 0),
         EFfaltas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFfaltas1tri_semNA),
         EFfaltas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                   ifelse(!is.na(hist_falta_2_tri) |!is.na(geo_falta_2_tri) |!is.na(lp_falta_2_tri) | !is.na(ed_fis_falta_2_tri) |!is.na(art_falta_2_tri) |
                                            !is.na(mat_falta_2_tri) |!is.na(cien_falta_2_tri) , 1, 0), 0),
         EFfaltas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFfaltas2tri_semNA),
         EFfaltas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                   ifelse(!is.na(hist_falta_3_tri) |!is.na(geo_falta_3_tri) |!is.na(lp_falta_3_tri) | !is.na(ed_fis_falta_3_tri) |!is.na(art_falta_3_tri) |
                                            !is.na(mat_falta_3_tri) |!is.na(cien_falta_3_tri) , 1, 0), 0),
         EFfaltas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFfaltas3tri_semNA),
         EFfaltas_semNA=ifelse(EFfaltas1tri_semNA==1 & EFfaltas2tri_semNA==1 & EFfaltas3tri_semNA==1, 1, 0),
         EFfaltas_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFfaltas_semNA))


NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1 %>%
  mutate(switch_escola=ifelse(!is.na(CENSO_ESCOLA_ENTURMACAO), 0, as.numeric(NA)),
         qt_escolas=ifelse(!is.na(CENSO_ESCOLA_ENTURMACAO), 1, as.numeric(NA)),
         qt_switch_escola=ifelse(!is.na(CENSO_ESCOLA_ENTURMACAO), 0, as.numeric(NA)),
         switch_munic=ifelse(!is.na(NM_MUNICIPIO_ENTURMACAO), 0, as.numeric(NA)),
         qt_switch_munic=ifelse(!is.na(NM_MUNICIPIO_ENTURMACAO), 0, as.numeric(NA)),
         qt_munic=ifelse(!is.na(NM_MUNICIPIO_ENTURMACAO), 1, as.numeric(NA)),
         switch_turno=ifelse(!is.na(ID_TURNO), 0, as.numeric(NA)),
         qt_turno=ifelse(!is.na(ID_TURNO), 1, as.numeric(NA)),
         qt_switch_turno=ifelse(!is.na(ID_TURNO), 0, as.numeric(NA)),
         switch_turma=ifelse(!is.na(ID_TURMA), 0, as.numeric(NA)),
         qt_turma=ifelse(!is.na(ID_TURMA), 1, as.numeric(NA)),
         qt_switch_turma=ifelse(!is.na(ID_TURMA), 0, as.numeric(NA)))

############### Unindo duplicados e naooduplicados ##################
rm(changes, RA_group_2X, RA_group_3X, RA_group_4X, RA_group_5X, seges_2016_fill1, dup_seges_2016_fill1)

colnames(dup_count2X_seges2016_1)
colnames(NOdup_seges_2016_fill1)

NOdup_seges_2016_fill1 <- NOdup_seges_2016_fill1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                     "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                     "CD_INEP_ALUNO","hist_falta_2_tri","hist_falta_1_tri","hist_falta_3_tri","geo_falta_3_tri","geo_falta_1_tri",
                                                     "geo_falta_2_tri","soc_falta_2_tri","soc_falta_1_tri","soc_falta_3_tri","fil_falta_3_tri",
                                                     "fil_falta_2_tri","art_falta_rec_2_tri","bio_falta_rec_2_tri","ed_fis_falta_rec_2_tri","ed_fis_falta_rec_1_tri",
                                                     "fil_falta_rec_2_tri","fis_falta_rec_2_tri","fis_falta_rec_1_tri","geo_falta_rec_2_tri","geo_falta_rec_1_tri",
                                                     "hist_falta_rec_1_tri","hist_falta_rec_2_tri","lp_falta_rec_1_tri","mat_falta_rec_2_tri","mat_falta_rec_1_tri",
                                                     "qui_falta_rec_2_tri","qui_falta_rec_1_tri","soc_falta_rec_1_tri","lp_falta_2_tri","lp_falta_1_tri",
                                                     "lp_falta_3_tri","ed_fis_falta_2_tri","art_falta_2_tri","art_falta_1_tri","fis_falta_3_tri",
                                                     "fis_falta_2_tri","fis_falta_1_tri","qui_falta_2_tri","qui_falta_3_tri","qui_falta_1_tri",
                                                     "bio_falta_1_tri","mat_falta_1_tri","mat_falta_3_tri","art_falta_rec_1_tri","bio_falta_rec_1_tri",
                                                     "fil_falta_rec_1_tri","lp_falta_rec_2_tri","soc_falta_rec_2_tri","ed_fis_falta_3_tri","ed_fis_falta_1_tri",
                                                     "art_falta_3_tri","bio_falta_2_tri","bio_falta_3_tri","mat_falta_2_tri","fil_falta_1_tri",
                                                     "cien_falta_3_tri","cien_falta_2_tri","cien_falta_1_tri","cien_falta_rec_2_tri","cien_falta_rec_1_tri","id", "dup_count","EMfaltas1tri_semNA",
                                                     "EMfaltas2tri_semNA","EMfaltas3tri_semNA","EMfaltas_semNA","EFfaltas1tri_semNA","EFfaltas2tri_semNA","EFfaltas3tri_semNA",
                                                     "EFfaltas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                     "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]

dup_count2X_seges2016_1 <- dup_count2X_seges2016_1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                       "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                       "CD_INEP_ALUNO","hist_falta_2_tri","hist_falta_1_tri","hist_falta_3_tri","geo_falta_3_tri","geo_falta_1_tri",
                                                       "geo_falta_2_tri","soc_falta_2_tri","soc_falta_1_tri","soc_falta_3_tri","fil_falta_3_tri",
                                                       "fil_falta_2_tri","art_falta_rec_2_tri","bio_falta_rec_2_tri","ed_fis_falta_rec_2_tri","ed_fis_falta_rec_1_tri",
                                                       "fil_falta_rec_2_tri","fis_falta_rec_2_tri","fis_falta_rec_1_tri","geo_falta_rec_2_tri","geo_falta_rec_1_tri",
                                                       "hist_falta_rec_1_tri","hist_falta_rec_2_tri","lp_falta_rec_1_tri","mat_falta_rec_2_tri","mat_falta_rec_1_tri",
                                                       "qui_falta_rec_2_tri","qui_falta_rec_1_tri","soc_falta_rec_1_tri","lp_falta_2_tri","lp_falta_1_tri",
                                                       "lp_falta_3_tri","ed_fis_falta_2_tri","art_falta_2_tri","art_falta_1_tri","fis_falta_3_tri",
                                                       "fis_falta_2_tri","fis_falta_1_tri","qui_falta_2_tri","qui_falta_3_tri","qui_falta_1_tri",
                                                       "bio_falta_1_tri","mat_falta_1_tri","mat_falta_3_tri","art_falta_rec_1_tri","bio_falta_rec_1_tri",
                                                       "fil_falta_rec_1_tri","lp_falta_rec_2_tri","soc_falta_rec_2_tri","ed_fis_falta_3_tri","ed_fis_falta_1_tri",
                                                       "art_falta_3_tri","bio_falta_2_tri","bio_falta_3_tri","mat_falta_2_tri","fil_falta_1_tri",
                                                       "cien_falta_3_tri","cien_falta_2_tri","cien_falta_1_tri","cien_falta_rec_2_tri","cien_falta_rec_1_tri","id","dup_count","EMfaltas1tri_semNA",
                                                       "EMfaltas2tri_semNA","EMfaltas3tri_semNA","EMfaltas_semNA","EFfaltas1tri_semNA","EFfaltas2tri_semNA","EFfaltas3tri_semNA",
                                                       "EFfaltas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                       "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]

dup_count3X_seges2016_1 <- dup_count3X_seges2016_1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                       "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                       "CD_INEP_ALUNO","hist_falta_2_tri","hist_falta_1_tri","hist_falta_3_tri","geo_falta_3_tri","geo_falta_1_tri",
                                                       "geo_falta_2_tri","soc_falta_2_tri","soc_falta_1_tri","soc_falta_3_tri","fil_falta_3_tri",
                                                       "fil_falta_2_tri","art_falta_rec_2_tri","bio_falta_rec_2_tri","ed_fis_falta_rec_2_tri","ed_fis_falta_rec_1_tri",
                                                       "fil_falta_rec_2_tri","fis_falta_rec_2_tri","fis_falta_rec_1_tri","geo_falta_rec_2_tri","geo_falta_rec_1_tri",
                                                       "hist_falta_rec_1_tri","hist_falta_rec_2_tri","lp_falta_rec_1_tri","mat_falta_rec_2_tri","mat_falta_rec_1_tri",
                                                       "qui_falta_rec_2_tri","qui_falta_rec_1_tri","soc_falta_rec_1_tri","lp_falta_2_tri","lp_falta_1_tri",
                                                       "lp_falta_3_tri","ed_fis_falta_2_tri","art_falta_2_tri","art_falta_1_tri","fis_falta_3_tri",
                                                       "fis_falta_2_tri","fis_falta_1_tri","qui_falta_2_tri","qui_falta_3_tri","qui_falta_1_tri",
                                                       "bio_falta_1_tri","mat_falta_1_tri","mat_falta_3_tri","art_falta_rec_1_tri","bio_falta_rec_1_tri",
                                                       "fil_falta_rec_1_tri","lp_falta_rec_2_tri","soc_falta_rec_2_tri","ed_fis_falta_3_tri","ed_fis_falta_1_tri",
                                                       "art_falta_3_tri","bio_falta_2_tri","bio_falta_3_tri","mat_falta_2_tri","fil_falta_1_tri",
                                                       "cien_falta_3_tri","cien_falta_2_tri","cien_falta_1_tri","cien_falta_rec_2_tri","cien_falta_rec_1_tri","id","dup_count","EMfaltas1tri_semNA",
                                                       "EMfaltas2tri_semNA","EMfaltas3tri_semNA","EMfaltas_semNA","EFfaltas1tri_semNA","EFfaltas2tri_semNA","EFfaltas3tri_semNA",
                                                       "EFfaltas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                       "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]

dup_count4X_seges2016_1 <- dup_count4X_seges2016_1[, c("NM_REGIONAL_ENTURMACAO","NM_MUNICIPIO_ENTURMACAO","CENSO_ESCOLA_ENTURMACAO","LOCALIZACAO","ID_ETAPA_MATRICULA",
                                                       "ID_NIVEL_MATRICULA","ID_TURNO","ID_TURMA","NM_ESCOLA_ENTURMACAO","RA","NM_ALUNO","DC_COR_RACA","TP_SEXO","NU_CPF",
                                                       "CD_INEP_ALUNO","hist_falta_2_tri","hist_falta_1_tri","hist_falta_3_tri","geo_falta_3_tri","geo_falta_1_tri",
                                                       "geo_falta_2_tri","soc_falta_2_tri","soc_falta_1_tri","soc_falta_3_tri","fil_falta_3_tri",
                                                       "fil_falta_2_tri","art_falta_rec_2_tri","bio_falta_rec_2_tri","ed_fis_falta_rec_2_tri","ed_fis_falta_rec_1_tri",
                                                       "fil_falta_rec_2_tri","fis_falta_rec_2_tri","fis_falta_rec_1_tri","geo_falta_rec_2_tri","geo_falta_rec_1_tri",
                                                       "hist_falta_rec_1_tri","hist_falta_rec_2_tri","lp_falta_rec_1_tri","mat_falta_rec_2_tri","mat_falta_rec_1_tri",
                                                       "qui_falta_rec_2_tri","qui_falta_rec_1_tri","soc_falta_rec_1_tri","lp_falta_2_tri","lp_falta_1_tri",
                                                       "lp_falta_3_tri","ed_fis_falta_2_tri","art_falta_2_tri","art_falta_1_tri","fis_falta_3_tri",
                                                       "fis_falta_2_tri","fis_falta_1_tri","qui_falta_2_tri","qui_falta_3_tri","qui_falta_1_tri",
                                                       "bio_falta_1_tri","mat_falta_1_tri","mat_falta_3_tri","art_falta_rec_1_tri","bio_falta_rec_1_tri",
                                                       "fil_falta_rec_1_tri","lp_falta_rec_2_tri","soc_falta_rec_2_tri","ed_fis_falta_3_tri","ed_fis_falta_1_tri",
                                                       "art_falta_3_tri","bio_falta_2_tri","bio_falta_3_tri","mat_falta_2_tri","fil_falta_1_tri",
                                                       "cien_falta_3_tri","cien_falta_2_tri","cien_falta_1_tri","cien_falta_rec_2_tri","cien_falta_rec_1_tri","id","dup_count","EMfaltas1tri_semNA",
                                                       "EMfaltas2tri_semNA","EMfaltas3tri_semNA","EMfaltas_semNA","EFfaltas1tri_semNA","EFfaltas2tri_semNA","EFfaltas3tri_semNA",
                                                       "EFfaltas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic","qt_munic",
                                                       "switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma")]

dup_seges_2016_fill1 <- rbind(NOdup_seges_2016_fill1, dup_count2X_seges2016_1, dup_count3X_seges2016_1, dup_count4X_seges2016_1)

saveRDS(dup_seges_2016_fill1, 'seges2020faltas_dup_postTRATAMENTO')

rm(NOdup_seges_2016_fill1, dup_count2X_seges2016_1, dup_count3X_seges2016_1, dup_count4X_seges2016_1,dup_seges_2016_fill1, seges_2016_fill1)


######################################################################

dup_seges_2020_fill1 <-readRDS('seges2016aulas_dup_postTRATAMENTO')
dup_seges_2020_fill2 <-readRDS('seges2016faltas_dup_postTRATAMENTO')
dup_seges_2020_fill3 <-readRDS('seges2016notas_dup_postTRATAMENTO')

###### UNIFICANDO DADOS DE AULAS+NOTAS+FALTAS #######
colnames(dup_seges_2020_fill1)
colnames(dup_seges_2020_fill2)
colnames(dup_seges_2020_fill3)

dup_seges_2020_fill1 <- arrange(dup_seges_2020_fill1, RA)
dup_seges_2020_fill2 <- arrange(dup_seges_2020_fill2, RA)
dup_seges_2020_fill3 <- arrange(dup_seges_2020_fill3, RA)

dup_seges_2020_fill2 <- dup_seges_2020_fill2[, c("hist_falta_2_tri","hist_falta_1_tri","hist_falta_3_tri","geo_falta_3_tri","geo_falta_1_tri",
                                                 "geo_falta_2_tri","soc_falta_2_tri","soc_falta_1_tri","soc_falta_3_tri","fil_falta_3_tri",
                                                 "fil_falta_2_tri","art_falta_rec_2_tri","bio_falta_rec_2_tri","ed_fis_falta_rec_2_tri","ed_fis_falta_rec_1_tri",
                                                 "fil_falta_rec_2_tri","fis_falta_rec_2_tri","fis_falta_rec_1_tri","geo_falta_rec_2_tri","geo_falta_rec_1_tri",
                                                 "hist_falta_rec_1_tri","hist_falta_rec_2_tri","lp_falta_rec_1_tri","mat_falta_rec_2_tri","mat_falta_rec_1_tri",
                                                 "qui_falta_rec_2_tri","qui_falta_rec_1_tri","soc_falta_rec_1_tri","lp_falta_2_tri","lp_falta_1_tri",
                                                 "lp_falta_3_tri","ed_fis_falta_2_tri","art_falta_2_tri","art_falta_1_tri","fis_falta_3_tri",
                                                 "fis_falta_2_tri","fis_falta_1_tri","qui_falta_2_tri","qui_falta_3_tri","qui_falta_1_tri",
                                                 "bio_falta_1_tri","mat_falta_1_tri","mat_falta_3_tri","art_falta_rec_1_tri","bio_falta_rec_1_tri",
                                                 "fil_falta_rec_1_tri","lp_falta_rec_2_tri","soc_falta_rec_2_tri","ed_fis_falta_3_tri","ed_fis_falta_1_tri",
                                                 "art_falta_3_tri","bio_falta_2_tri","bio_falta_3_tri","mat_falta_2_tri","fil_falta_1_tri",
                                                 "cien_falta_3_tri","cien_falta_2_tri","cien_falta_1_tri","cien_falta_rec_2_tri","cien_falta_rec_1_tri","EMfaltas1tri_semNA",
                                                 "EMfaltas2tri_semNA","EMfaltas3tri_semNA","EMfaltas_semNA","EFfaltas1tri_semNA","EFfaltas2tri_semNA","EFfaltas3tri_semNA",
                                                 "EFfaltas_semNA")]

dup_seges_2020_fill1 <- dup_seges_2020_fill1[, c("hist_aula_2_tri","hist_aula_1_tri","hist_aula_3_tri","geo_aula_3_tri","geo_aula_1_tri",
                                                 "geo_aula_2_tri","soc_aula_2_tri","soc_aula_1_tri","soc_aula_3_tri","fil_aula_3_tri",
                                                 "fil_aula_2_tri","art_aula_rec_2_tri","bio_aula_rec_2_tri","ed_fis_aula_rec_2_tri","ed_fis_aula_rec_1_tri",
                                                 "fil_aula_rec_2_tri","fis_aula_rec_2_tri","fis_aula_rec_1_tri","geo_aula_rec_2_tri","geo_aula_rec_1_tri",
                                                 "hist_aula_rec_1_tri","hist_aula_rec_2_tri","lp_aula_rec_1_tri","mat_aula_rec_2_tri","mat_aula_rec_1_tri",
                                                 "qui_aula_rec_2_tri","qui_aula_rec_1_tri","soc_aula_rec_1_tri","lp_aula_2_tri","lp_aula_1_tri",
                                                 "lp_aula_3_tri","ed_fis_aula_2_tri","art_aula_2_tri","art_aula_1_tri","fis_aula_3_tri",
                                                 "fis_aula_2_tri","fis_aula_1_tri","qui_aula_2_tri","qui_aula_3_tri","qui_aula_1_tri",
                                                 "bio_aula_1_tri","mat_aula_1_tri","mat_aula_3_tri","art_aula_rec_1_tri","bio_aula_rec_1_tri",
                                                 "fil_aula_rec_1_tri","lp_aula_rec_2_tri","soc_aula_rec_2_tri","ed_fis_aula_3_tri","ed_fis_aula_1_tri",
                                                 "art_aula_3_tri","bio_aula_2_tri","bio_aula_3_tri","mat_aula_2_tri","fil_aula_1_tri",
                                                 "cien_aula_3_tri","cien_aula_2_tri","cien_aula_1_tri","cien_aula_rec_2_tri","cien_aula_rec_1_tri","EMaulas1tri_semNA",
                                                 "EMaulas2tri_semNA","EMaulas3tri_semNA","EMaulas_semNA","EFaulas1tri_semNA","EFaulas2tri_semNA","EFaulas3tri_semNA",
                                                 "EFaulas_semNA")]

dup_seges_2020 <- cbind(dup_seges_2020_fill3, dup_seges_2020_fill2, dup_seges_2020_fill1)

saveRDS(dup_seges_2020, 'seges2020_postTRATAMENTO')

############# Tratamento de informacoes faltantes #################
########### PRIMEIRA OPCAO PROXY: FILL() POR ID_TURMA #############
dup_seges_2020 <- dup_seges_2020 %>%
  group_by(ID_TURMA) %>%
  fill('hist_aula_2_tri','hist_aula_1_tri','hist_aula_3_tri','geo_aula_3_tri','geo_aula_1_tri','geo_aula_2_tri','soc_aula_2_tri','soc_aula_1_tri',
       'soc_aula_3_tri','fil_aula_3_tri','fil_aula_2_tri','art_aula_rec_2_tri','bio_aula_rec_2_tri','ed_fis_aula_rec_2_tri','ed_fis_aula_rec_1_tri',
       'fil_aula_rec_2_tri','fis_aula_rec_2_tri','fis_aula_rec_1_tri','geo_aula_rec_2_tri','geo_aula_rec_1_tri','hist_aula_rec_1_tri','hist_aula_rec_2_tri',
       'lp_aula_rec_1_tri','mat_aula_rec_2_tri','mat_aula_rec_1_tri','qui_aula_rec_2_tri','qui_aula_rec_1_tri','soc_aula_rec_1_tri','lp_aula_2_tri',
       'lp_aula_1_tri','lp_aula_3_tri','ed_fis_aula_2_tri','art_aula_2_tri','art_aula_1_tri','fis_aula_3_tri','fis_aula_2_tri','fis_aula_1_tri',
       'qui_aula_2_tri','qui_aula_3_tri','qui_aula_1_tri','bio_aula_1_tri','mat_aula_1_tri','mat_aula_3_tri','art_aula_rec_1_tri','bio_aula_rec_1_tri',
       'fil_aula_rec_1_tri','lp_aula_rec_2_tri','soc_aula_rec_2_tri','ed_fis_aula_3_tri','ed_fis_aula_1_tri','art_aula_3_tri','bio_aula_2_tri',
       'bio_aula_3_tri','mat_aula_2_tri','fil_aula_1_tri','cien_aula_3_tri','cien_aula_2_tri','cien_aula_1_tri','cien_aula_rec_2_tri','cien_aula_rec_1_tri',
       .direction = "downup") %>% 
  ungroup()

dup_seges_2020 <- dup_seges_2020 %>%
  mutate(EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(soc_aula_1_tri) |!is.na(fil_aula_1_tri) |!is.na(lp_aula_1_tri) |
                                           !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |!is.na(fis_aula_1_tri) |!is.na(qui_aula_1_tri) |!is.na(bio_aula_1_tri) |
                                           !is.na(mat_aula_1_tri), 1, 0), 0),
         EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas1tri_semNA),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(soc_aula_2_tri) |!is.na(fil_aula_2_tri) |!is.na(lp_aula_2_tri) |
                                           !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |!is.na(fis_aula_2_tri) |!is.na(qui_aula_2_tri) |!is.na(bio_aula_2_tri) |
                                           !is.na(mat_aula_2_tri), 1, 0), 0),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas2tri_semNA),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(soc_aula_3_tri) |!is.na(fil_aula_3_tri) |!is.na(lp_aula_3_tri) |
                                           !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |!is.na(fis_aula_3_tri) |!is.na(qui_aula_3_tri) |!is.na(bio_aula_3_tri) |
                                           !is.na(mat_aula_3_tri), 1, 0), 0),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas3tri_semNA),
         EMaulas_semNA=ifelse(EMaulas1tri_semNA==1 & EMaulas2tri_semNA==1 & EMaulas3tri_semNA==1, 1, 0),
         EMaulas_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas_semNA),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(lp_aula_1_tri) | !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |
                                           !is.na(mat_aula_1_tri) |!is.na(cien_aula_1_tri) , 1, 0), 0),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas1tri_semNA),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(lp_aula_2_tri) | !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |
                                           !is.na(mat_aula_2_tri) |!is.na(cien_aula_2_tri) , 1, 0), 0),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas2tri_semNA),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(lp_aula_3_tri) | !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |
                                           !is.na(mat_aula_3_tri) |!is.na(cien_aula_3_tri) , 1, 0), 0),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas3tri_semNA),
         EFaulas_semNA=ifelse(EFaulas1tri_semNA==1 & EFaulas2tri_semNA==1 & EFaulas3tri_semNA==1, 1, 0),
         EFaulas_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas_semNA))

############## SEGUNDA OPCAO PROXY: FILL() POR ID_TURNO E ESCOLA ####################
dup_seges_2020 <- dup_seges_2020 %>%
  group_by(CENSO_ESCOLA_ENTURMACAO, ID_TURNO) %>%
  fill('hist_aula_2_tri','hist_aula_1_tri','hist_aula_3_tri','geo_aula_3_tri','geo_aula_1_tri','geo_aula_2_tri','soc_aula_2_tri','soc_aula_1_tri',
       'soc_aula_3_tri','fil_aula_3_tri','fil_aula_2_tri','art_aula_rec_2_tri','bio_aula_rec_2_tri','ed_fis_aula_rec_2_tri','ed_fis_aula_rec_1_tri',
       'fil_aula_rec_2_tri','fis_aula_rec_2_tri','fis_aula_rec_1_tri','geo_aula_rec_2_tri','geo_aula_rec_1_tri','hist_aula_rec_1_tri','hist_aula_rec_2_tri',
       'lp_aula_rec_1_tri','mat_aula_rec_2_tri','mat_aula_rec_1_tri','qui_aula_rec_2_tri','qui_aula_rec_1_tri','soc_aula_rec_1_tri','lp_aula_2_tri',
       'lp_aula_1_tri','lp_aula_3_tri','ed_fis_aula_2_tri','art_aula_2_tri','art_aula_1_tri','fis_aula_3_tri','fis_aula_2_tri','fis_aula_1_tri',
       'qui_aula_2_tri','qui_aula_3_tri','qui_aula_1_tri','bio_aula_1_tri','mat_aula_1_tri','mat_aula_3_tri','art_aula_rec_1_tri','bio_aula_rec_1_tri',
       'fil_aula_rec_1_tri','lp_aula_rec_2_tri','soc_aula_rec_2_tri','ed_fis_aula_3_tri','ed_fis_aula_1_tri','art_aula_3_tri','bio_aula_2_tri',
       'bio_aula_3_tri','mat_aula_2_tri','fil_aula_1_tri','cien_aula_3_tri','cien_aula_2_tri','cien_aula_1_tri','cien_aula_rec_2_tri','cien_aula_rec_1_tri',
       .direction = "downup") %>% 
  ungroup()

dup_seges_2020 <- dup_seges_2020 %>%
  mutate(EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(soc_aula_1_tri) |!is.na(fil_aula_1_tri) |!is.na(lp_aula_1_tri) |
                                           !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |!is.na(fis_aula_1_tri) |!is.na(qui_aula_1_tri) |!is.na(bio_aula_1_tri) |
                                           !is.na(mat_aula_1_tri), 1, 0), 0),
         EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas1tri_semNA),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(soc_aula_2_tri) |!is.na(fil_aula_2_tri) |!is.na(lp_aula_2_tri) |
                                           !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |!is.na(fis_aula_2_tri) |!is.na(qui_aula_2_tri) |!is.na(bio_aula_2_tri) |
                                           !is.na(mat_aula_2_tri), 1, 0), 0),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas2tri_semNA),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(soc_aula_3_tri) |!is.na(fil_aula_3_tri) |!is.na(lp_aula_3_tri) |
                                           !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |!is.na(fis_aula_3_tri) |!is.na(qui_aula_3_tri) |!is.na(bio_aula_3_tri) |
                                           !is.na(mat_aula_3_tri), 1, 0), 0),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas3tri_semNA),
         EMaulas_semNA=ifelse(EMaulas1tri_semNA==1 & EMaulas2tri_semNA==1 & EMaulas3tri_semNA==1, 1, 0),
         EMaulas_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas_semNA),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(lp_aula_1_tri) | !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |
                                           !is.na(mat_aula_1_tri) |!is.na(cien_aula_1_tri) , 1, 0), 0),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas1tri_semNA),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(lp_aula_2_tri) | !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |
                                           !is.na(mat_aula_2_tri) |!is.na(cien_aula_2_tri) , 1, 0), 0),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas2tri_semNA),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(lp_aula_3_tri) | !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |
                                           !is.na(mat_aula_3_tri) |!is.na(cien_aula_3_tri) , 1, 0), 0),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas3tri_semNA),
         EFaulas_semNA=ifelse(EFaulas1tri_semNA==1 & EFaulas2tri_semNA==1 & EFaulas3tri_semNA==1, 1, 0),
         EFaulas_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas_semNA))

############## TERCEIRA OPCAO PROXY: FILL() POR NM_MUNICIPIO_ENTURMACAO e ID_TURNO ####################
dup_seges_2020 <- dup_seges_2020 %>%
  group_by(NM_MUNICIPIO_ENTURMACAO, ID_TURNO) %>%
  fill('hist_aula_2_tri','hist_aula_1_tri','hist_aula_3_tri','geo_aula_3_tri','geo_aula_1_tri','geo_aula_2_tri','soc_aula_2_tri','soc_aula_1_tri',
       'soc_aula_3_tri','fil_aula_3_tri','fil_aula_2_tri','art_aula_rec_2_tri','bio_aula_rec_2_tri','ed_fis_aula_rec_2_tri','ed_fis_aula_rec_1_tri',
       'fil_aula_rec_2_tri','fis_aula_rec_2_tri','fis_aula_rec_1_tri','geo_aula_rec_2_tri','geo_aula_rec_1_tri','hist_aula_rec_1_tri','hist_aula_rec_2_tri',
       'lp_aula_rec_1_tri','mat_aula_rec_2_tri','mat_aula_rec_1_tri','qui_aula_rec_2_tri','qui_aula_rec_1_tri','soc_aula_rec_1_tri','lp_aula_2_tri',
       'lp_aula_1_tri','lp_aula_3_tri','ed_fis_aula_2_tri','art_aula_2_tri','art_aula_1_tri','fis_aula_3_tri','fis_aula_2_tri','fis_aula_1_tri',
       'qui_aula_2_tri','qui_aula_3_tri','qui_aula_1_tri','bio_aula_1_tri','mat_aula_1_tri','mat_aula_3_tri','art_aula_rec_1_tri','bio_aula_rec_1_tri',
       'fil_aula_rec_1_tri','lp_aula_rec_2_tri','soc_aula_rec_2_tri','ed_fis_aula_3_tri','ed_fis_aula_1_tri','art_aula_3_tri','bio_aula_2_tri',
       'bio_aula_3_tri','mat_aula_2_tri','fil_aula_1_tri','cien_aula_3_tri','cien_aula_2_tri','cien_aula_1_tri','cien_aula_rec_2_tri','cien_aula_rec_1_tri',
       .direction = "downup") %>% 
  ungroup()

dup_seges_2020 <- dup_seges_2020 %>%
  mutate(EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(soc_aula_1_tri) |!is.na(fil_aula_1_tri) |!is.na(lp_aula_1_tri) |
                                           !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |!is.na(fis_aula_1_tri) |!is.na(qui_aula_1_tri) |!is.na(bio_aula_1_tri) |
                                           !is.na(mat_aula_1_tri), 1, 0), 0),
         EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas1tri_semNA),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(soc_aula_2_tri) |!is.na(fil_aula_2_tri) |!is.na(lp_aula_2_tri) |
                                           !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |!is.na(fis_aula_2_tri) |!is.na(qui_aula_2_tri) |!is.na(bio_aula_2_tri) |
                                           !is.na(mat_aula_2_tri), 1, 0), 0),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas2tri_semNA),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(soc_aula_3_tri) |!is.na(fil_aula_3_tri) |!is.na(lp_aula_3_tri) |
                                           !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |!is.na(fis_aula_3_tri) |!is.na(qui_aula_3_tri) |!is.na(bio_aula_3_tri) |
                                           !is.na(mat_aula_3_tri), 1, 0), 0),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas3tri_semNA),
         EMaulas_semNA=ifelse(EMaulas1tri_semNA==1 & EMaulas2tri_semNA==1 & EMaulas3tri_semNA==1, 1, 0),
         EMaulas_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas_semNA),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(lp_aula_1_tri) | !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |
                                           !is.na(mat_aula_1_tri) |!is.na(cien_aula_1_tri) , 1, 0), 0),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas1tri_semNA),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(lp_aula_2_tri) | !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |
                                           !is.na(mat_aula_2_tri) |!is.na(cien_aula_2_tri) , 1, 0), 0),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas2tri_semNA),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(lp_aula_3_tri) | !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |
                                           !is.na(mat_aula_3_tri) |!is.na(cien_aula_3_tri) , 1, 0), 0),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas3tri_semNA),
         EFaulas_semNA=ifelse(EFaulas1tri_semNA==1 & EFaulas2tri_semNA==1 & EFaulas3tri_semNA==1, 1, 0),
         EFaulas_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas_semNA))

############## QUARTA OPCAO PROXY: FILL() POR NM_MUNICIPIO_ENTURMACAO ####################
dup_seges_2020 <- dup_seges_2020 %>%
  group_by(NM_MUNICIPIO_ENTURMACAO) %>%
  fill('hist_aula_2_tri','hist_aula_1_tri','hist_aula_3_tri','geo_aula_3_tri','geo_aula_1_tri','geo_aula_2_tri','soc_aula_2_tri','soc_aula_1_tri',
       'soc_aula_3_tri','fil_aula_3_tri','fil_aula_2_tri','art_aula_rec_2_tri','bio_aula_rec_2_tri','ed_fis_aula_rec_2_tri','ed_fis_aula_rec_1_tri',
       'fil_aula_rec_2_tri','fis_aula_rec_2_tri','fis_aula_rec_1_tri','geo_aula_rec_2_tri','geo_aula_rec_1_tri','hist_aula_rec_1_tri','hist_aula_rec_2_tri',
       'lp_aula_rec_1_tri','mat_aula_rec_2_tri','mat_aula_rec_1_tri','qui_aula_rec_2_tri','qui_aula_rec_1_tri','soc_aula_rec_1_tri','lp_aula_2_tri',
       'lp_aula_1_tri','lp_aula_3_tri','ed_fis_aula_2_tri','art_aula_2_tri','art_aula_1_tri','fis_aula_3_tri','fis_aula_2_tri','fis_aula_1_tri',
       'qui_aula_2_tri','qui_aula_3_tri','qui_aula_1_tri','bio_aula_1_tri','mat_aula_1_tri','mat_aula_3_tri','art_aula_rec_1_tri','bio_aula_rec_1_tri',
       'fil_aula_rec_1_tri','lp_aula_rec_2_tri','soc_aula_rec_2_tri','ed_fis_aula_3_tri','ed_fis_aula_1_tri','art_aula_3_tri','bio_aula_2_tri',
       'bio_aula_3_tri','mat_aula_2_tri','fil_aula_1_tri','cien_aula_3_tri','cien_aula_2_tri','cien_aula_1_tri','cien_aula_rec_2_tri','cien_aula_rec_1_tri',
       .direction = "downup") %>% 
  ungroup()

dup_seges_2020 <- dup_seges_2020 %>%
  mutate(EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(soc_aula_1_tri) |!is.na(fil_aula_1_tri) |!is.na(lp_aula_1_tri) |
                                           !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |!is.na(fis_aula_1_tri) |!is.na(qui_aula_1_tri) |!is.na(bio_aula_1_tri) |
                                           !is.na(mat_aula_1_tri), 1, 0), 0),
         EMaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas1tri_semNA),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(soc_aula_2_tri) |!is.na(fil_aula_2_tri) |!is.na(lp_aula_2_tri) |
                                           !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |!is.na(fis_aula_2_tri) |!is.na(qui_aula_2_tri) |!is.na(bio_aula_2_tri) |
                                           !is.na(mat_aula_2_tri), 1, 0), 0),
         EMaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas2tri_semNA),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(soc_aula_3_tri) |!is.na(fil_aula_3_tri) |!is.na(lp_aula_3_tri) |
                                           !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |!is.na(fis_aula_3_tri) |!is.na(qui_aula_3_tri) |!is.na(bio_aula_3_tri) |
                                           !is.na(mat_aula_3_tri), 1, 0), 0),
         EMaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas3tri_semNA),
         EMaulas_semNA=ifelse(EMaulas1tri_semNA==1 & EMaulas2tri_semNA==1 & EMaulas3tri_semNA==1, 1, 0),
         EMaulas_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38, 2, EMaulas_semNA),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_1_tri) |!is.na(geo_aula_1_tri) |!is.na(lp_aula_1_tri) | !is.na(ed_fis_aula_1_tri) |!is.na(art_aula_1_tri) |
                                           !is.na(mat_aula_1_tri) |!is.na(cien_aula_1_tri) , 1, 0), 0),
         EFaulas1tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas1tri_semNA),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_2_tri) |!is.na(geo_aula_2_tri) |!is.na(lp_aula_2_tri) | !is.na(ed_fis_aula_2_tri) |!is.na(art_aula_2_tri) |
                                           !is.na(mat_aula_2_tri) |!is.na(cien_aula_2_tri) , 1, 0), 0),
         EFaulas2tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas2tri_semNA),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==4| ID_NIVEL_MATRICULA==38,
                                  ifelse(!is.na(hist_aula_3_tri) |!is.na(geo_aula_3_tri) |!is.na(lp_aula_3_tri) | !is.na(ed_fis_aula_3_tri) |!is.na(art_aula_3_tri) |
                                           !is.na(mat_aula_3_tri) |!is.na(cien_aula_3_tri) , 1, 0), 0),
         EFaulas3tri_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas3tri_semNA),
         EFaulas_semNA=ifelse(EFaulas1tri_semNA==1 & EFaulas2tri_semNA==1 & EFaulas3tri_semNA==1, 1, 0),
         EFaulas_semNA=ifelse(ID_NIVEL_MATRICULA==5| ID_NIVEL_MATRICULA==6, 2, EFaulas_semNA))


saveRDS(dup_seges_2020, 'seges2016_22-02-2022')
rm(list = ls())

#################### LIMPANDO DUPLICADOS E CONSTRUINDO VARIAVEIS #########################

seges_2020 <-readRDS('seges2016_22-02-2022')
seges_2020_raw <-readRDS('seges_2016_raw')
SITUACAO_2020 <-readRDS('SITUACAO_FULL_2016')

colnames(seges_2020_raw)

seges_2020_raw <- seges_2020_raw %>%
  select(CENSO_ESCOLA_ENTURMACAO, CD_INEP_ALUNO, IDADE, DATA_MATRICULA, DATA_ENCERRAMENTO_MATRICULA) %>% 
  group_by(CD_INEP_ALUNO, IDADE, CENSO_ESCOLA_ENTURMACAO, DATA_MATRICULA, DATA_ENCERRAMENTO_MATRICULA) %>%
  filter(DATA_MATRICULA==unique(DATA_MATRICULA)) %>% 
  filter(DATA_ENCERRAMENTO_MATRICULA== unique(DATA_ENCERRAMENTO_MATRICULA)) %>% 
  distinct(CD_INEP_ALUNO, IDADE, CENSO_ESCOLA_ENTURMACAO, DATA_MATRICULA, DATA_ENCERRAMENTO_MATRICULA, .keep_all= TRUE) %>% 
  ungroup()

dup1_seges_2020 <- seges_2020 %>% 
  left_join(seges_2020_raw, by = c("CD_INEP_ALUNO" = "CD_INEP_ALUNO", "CENSO_ESCOLA_ENTURMACAO" = "CENSO_ESCOLA_ENTURMACAO"))

dup1_seges_2020$dt_matric <- parse_date_time(dup1_seges_2020$DATA_MATRICULA, "Ymd HMS")
dup1_seges_2020$dt_encerr <- parse_date_time(dup1_seges_2020$DATA_ENCERRAMENTO_MATRICULA, "Ymd HMS")

dup1_seges_2020$dt_encerr <- as.Date(dup1_seges_2020$dt_encerr, format =  "%Y/%m/%d %H:%M:%S")
dup1_seges_2020$dt_matric <- as.Date(dup1_seges_2020$dt_matric, format =  "%Y/%m/%d %H:%M:%S")

SITUACAO_2020 <- SITUACAO_2020 %>%
  select(CO_ENTIDADE, CO_ALUNO, SITUACAO)

dup1_seges_2020 <- dup1_seges_2020 %>% 
  left_join(SITUACAO_2020, by = c("CD_INEP_ALUNO" = "CO_ALUNO", "CENSO_ESCOLA_ENTURMACAO" = "CO_ENTIDADE"))

dup1_seges_2020 <- dup1_seges_2020 %>%
  mutate(CO_SITUACAO=ifelse(SITUACAO=="SIR", 1,
                            ifelse(SITUACAO=="Abandono", 2,
                                   ifelse(SITUACAO=="Reprovado", 3,
                                          ifelse(SITUACAO=="Aprovado", 4,
                                                 ifelse(SITUACAO=="Falecido", 5, 0))))))

dup1_seges_2020 <- dup1_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  filter(if (all(is.na(dt_matric))) TRUE else dt_matric == max(dt_matric, na.rm = TRUE))

dup1_seges_2020 <- dup1_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  filter(if (all(is.na(dt_encerr))) TRUE else dt_encerr == max(dt_encerr, na.rm = TRUE))

dup1_seges_2020 <- dup1_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  filter(if (all(is.na(CO_SITUACAO))) TRUE else CO_SITUACAO == max(CO_SITUACAO, na.rm = TRUE))

dup1_seges_2020 <- dup1_seges_2020 %>% group_by(CD_INEP_ALUNO) %>% mutate(id = row_number())
describe(dup1_seges_2020$id)

dup1_seges_2020 <- dup1_seges_2020 %>% ungroup()

dup1_seges_2020 <- dup1_seges_2020 %>%
  group_by(RA) %>%
  filter(if (all(is.na(dt_matric))) TRUE else dt_matric == max(dt_matric, na.rm = TRUE))

dup1_seges_2020 <- dup1_seges_2020 %>%
  group_by(RA) %>%
  filter(if (all(is.na(dt_encerr))) TRUE else dt_encerr == max(dt_encerr, na.rm = TRUE))

dup1_seges_2020 <- dup1_seges_2020 %>%
  group_by(RA) %>%
  filter(if (all(is.na(CO_SITUACAO))) TRUE else CO_SITUACAO == max(CO_SITUACAO, na.rm = TRUE))

dup1_seges_2020 <- dup1_seges_2020 %>% group_by(RA) %>% mutate(id = row_number())
describe(dup1_seges_2020$id)

dup1_seges_2020 <- dup1_seges_2020 %>% ungroup()

dup2_seges_2020 <- dup1_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  filter(if (all(EMnotas_semNA==0)) TRUE else EMnotas_semNA == max(EMnotas_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  filter(if (all(EMnotas1tri_semNA==0)) TRUE else EMnotas1tri_semNA == max(EMnotas1tri_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  filter(if (all(EMfaltas_semNA==0)) TRUE else EMfaltas_semNA == max(EMfaltas_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  filter(if (all(EMfaltas1tri_semNA==0)) TRUE else EMfaltas1tri_semNA == max(EMfaltas1tri_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  filter(if (all(EFnotas_semNA==0)) TRUE else EFnotas_semNA == max(EFnotas_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  filter(if (all(EFnotas1tri_semNA==0)) TRUE else EFnotas1tri_semNA == max(EFnotas1tri_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  filter(if (all(EFfaltas_semNA==0)) TRUE else EFfaltas_semNA == max(EFfaltas_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  filter(if (all(EFfaltas1tri_semNA==0)) TRUE else EFfaltas1tri_semNA == max(EFfaltas1tri_semNA, na.rm = TRUE))

dup2_seges_2020 <- dup2_seges_2020 %>% group_by(CD_INEP_ALUNO) %>% mutate(id = row_number())
describe(dup2_seges_2020$id)

dup2_seges_2020 <- dup2_seges_2020 %>% ungroup()

dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(RA) %>%
  filter(if (all(EMnotas_semNA==0)) TRUE else EMnotas_semNA == max(EMnotas_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(RA) %>%
  filter(if (all(EMnotas1tri_semNA==0)) TRUE else EMnotas1tri_semNA == max(EMnotas1tri_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(RA) %>%
  filter(if (all(EMfaltas_semNA==0)) TRUE else EMfaltas_semNA == max(EMfaltas_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(RA) %>%
  filter(if (all(EMfaltas1tri_semNA==0)) TRUE else EMfaltas1tri_semNA == max(EMfaltas1tri_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(RA) %>%
  filter(if (all(EFnotas_semNA==0)) TRUE else EFnotas_semNA == max(EFnotas_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(RA) %>%
  filter(if (all(EFnotas1tri_semNA==0)) TRUE else EFnotas1tri_semNA == max(EFnotas1tri_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(RA) %>%
  filter(if (all(EFfaltas_semNA==0)) TRUE else EFfaltas_semNA == max(EFfaltas_semNA, na.rm = TRUE))
dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(RA) %>%
  filter(if (all(EFfaltas1tri_semNA==0)) TRUE else EFfaltas1tri_semNA == max(EFfaltas1tri_semNA, na.rm = TRUE))

dup2_seges_2020 <- dup2_seges_2020 %>% group_by(RA) %>% mutate(id = row_number())
describe(dup2_seges_2020$id)

dup2_seges_2020 <- dup2_seges_2020 %>% ungroup()

dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(CD_INEP_ALUNO) %>%
  slice_sample(n=1) %>%
  ungroup()

dup2_seges_2020 <- dup2_seges_2020 %>%
  group_by(RA) %>%
  slice_sample(n=1) %>%
  ungroup()

dup2_seges_2020 <- dup2_seges_2020 %>% group_by(CD_INEP_ALUNO) %>% mutate(id = row_number()) %>% ungroup()
describe(dup2_seges_2020$id)
dup2_seges_2020 <- dup2_seges_2020 %>% group_by(RA) %>% mutate(id = row_number()) %>% ungroup()
describe(dup2_seges_2020$id)

colnames(dup2_seges_2020)

dup2_seges_2020 <- dup2_seges_2020[, c("CENSO_ESCOLA_ENTURMACAO","ID_TURMA","RA","NU_CPF","CD_INEP_ALUNO","DC_COR_RACA","IDADE","TP_SEXO","ID_TURNO","ID_NIVEL_MATRICULA",
                                       "ID_ETAPA_MATRICULA","dt_matric","dt_encerr","SITUACAO","CO_SITUACAO","EMnotas1tri_semNA","EMnotas2tri_semNA","EMnotas3tri_semNA",
                                       "EMnotas_semNA","EFnotas1tri_semNA","EFnotas2tri_semNA","EFnotas3tri_semNA","EFnotas_semNA","EMfaltas1tri_semNA",
                                       "EMfaltas2tri_semNA","EMfaltas3tri_semNA","EMfaltas_semNA","EFfaltas1tri_semNA","EFfaltas2tri_semNA",
                                       "EFfaltas3tri_semNA","EFfaltas_semNA","EMaulas1tri_semNA","EMaulas2tri_semNA","EMaulas3tri_semNA","EMaulas_semNA",
                                       "EFaulas1tri_semNA","EFaulas2tri_semNA","EFaulas3tri_semNA","EFaulas_semNA","switch_escola","qt_escolas",
                                       "qt_switch_escola","switch_munic","qt_switch_munic","qt_munic","switch_turno","qt_turno","qt_switch_turno",
                                       "switch_turma","qt_turma","qt_switch_turma","hist_2_tri","hist_1_tri","hist_3_tri","geo_3_tri","geo_1_tri",
                                       "geo_2_tri","soc_2_tri","soc_1_tri","soc_3_tri","fil_3_tri","fil_2_tri","art_rec_2_tri","bio_rec_2_tri",
                                       "ed_fis_rec_2_tri","ed_fis_rec_1_tri","fil_rec_2_tri","fis_rec_2_tri","fis_rec_1_tri","geo_rec_2_tri",
                                       "geo_rec_1_tri","hist_rec_1_tri","hist_rec_2_tri","lp_rec_1_tri","mat_rec_2_tri","mat_rec_1_tri","qui_rec_2_tri",
                                       "qui_rec_1_tri","soc_rec_1_tri","lp_2_tri","lp_1_tri","lp_3_tri","ed_fis_2_tri","art_2_tri","art_1_tri",
                                       "fis_3_tri","fis_2_tri","fis_1_tri","qui_2_tri","qui_3_tri","qui_1_tri","bio_1_tri","mat_1_tri","mat_3_tri",
                                       "art_rec_1_tri","bio_rec_1_tri","fil_rec_1_tri","lp_rec_2_tri","soc_rec_2_tri","ed_fis_3_tri","ed_fis_1_tri",
                                       "art_3_tri","bio_2_tri","bio_3_tri","mat_2_tri","fil_1_tri","cien_3_tri","cien_2_tri","cien_1_tri",
                                       "cien_rec_2_tri","cien_rec_1_tri","hist_falta_2_tri","hist_falta_1_tri","hist_falta_3_tri","geo_falta_3_tri",
                                       "geo_falta_1_tri","geo_falta_2_tri","soc_falta_2_tri","soc_falta_1_tri","soc_falta_3_tri","fil_falta_3_tri",
                                       "fil_falta_2_tri","lp_falta_2_tri","lp_falta_1_tri","lp_falta_3_tri","ed_fis_falta_2_tri","art_falta_2_tri",
                                       "art_falta_1_tri","fis_falta_3_tri","fis_falta_2_tri","fis_falta_1_tri","qui_falta_2_tri","qui_falta_3_tri",
                                       "qui_falta_1_tri","bio_falta_1_tri","mat_falta_1_tri","mat_falta_3_tri","ed_fis_falta_3_tri","ed_fis_falta_1_tri",
                                       "art_falta_3_tri","bio_falta_2_tri","bio_falta_3_tri","mat_falta_2_tri","fil_falta_1_tri","cien_falta_3_tri",
                                       "cien_falta_2_tri","cien_falta_1_tri","hist_aula_2_tri","hist_aula_1_tri","hist_aula_3_tri","geo_aula_3_tri",
                                       "geo_aula_1_tri","geo_aula_2_tri","soc_aula_2_tri","soc_aula_1_tri","soc_aula_3_tri","fil_aula_3_tri",
                                       "fil_aula_2_tri","lp_aula_2_tri","lp_aula_1_tri","lp_aula_3_tri","ed_fis_aula_2_tri","art_aula_2_tri",
                                       "art_aula_1_tri","fis_aula_3_tri","fis_aula_2_tri","fis_aula_1_tri","qui_aula_2_tri","qui_aula_3_tri",
                                       "qui_aula_1_tri","bio_aula_1_tri","mat_aula_1_tri","mat_aula_3_tri","ed_fis_aula_3_tri","ed_fis_aula_1_tri",
                                       "art_aula_3_tri","bio_aula_2_tri","bio_aula_3_tri","mat_aula_2_tri","fil_aula_1_tri","cien_aula_3_tri",
                                       "cien_aula_2_tri","cien_aula_1_tri")]

dup2_seges_2020 <- dup2_seges_2020 %>% ungroup()

dup2_seges_2020 <- dup2_seges_2020 %>% 
  mutate(lp_1_tri=ifelse(lp_1_tri>=31, 30, lp_1_tri),
         mat_1_tri=ifelse(mat_1_tri>=31, 30, mat_1_tri),
         geo_1_tri=ifelse(geo_1_tri>=31, 30, geo_1_tri),
         hist_1_tri=ifelse(hist_1_tri>=31, 30, hist_1_tri),
         soc_1_tri=ifelse(soc_1_tri>=31, 30, soc_1_tri),
         fil_1_tri=ifelse(fil_1_tri>=31, 30, fil_1_tri),
         bio_1_tri=ifelse(bio_1_tri>=31, 30, bio_1_tri),
         qui_1_tri=ifelse(qui_1_tri>=31, 30, qui_1_tri),
         fis_1_tri=ifelse(fis_1_tri>=31, 30, fis_1_tri),
         cien_1_tri=ifelse(cien_1_tri>=31, 30, cien_1_tri),
         art_1_tri=ifelse(art_1_tri>=31, 30, art_1_tri),
         ed_fis_1_tri=ifelse(ed_fis_1_tri>=31, 30, ed_fis_1_tri),
         lp_2_tri=ifelse(lp_2_tri>=31, 30, lp_2_tri),
         mat_2_tri=ifelse(mat_2_tri>=31, 30, mat_2_tri),
         geo_2_tri=ifelse(geo_2_tri>=31, 30, geo_2_tri),
         hist_2_tri=ifelse(hist_2_tri>=31, 30, hist_2_tri),
         soc_2_tri=ifelse(soc_2_tri>=31, 30, soc_2_tri),
         fil_2_tri=ifelse(fil_2_tri>=31, 30, fil_2_tri),
         bio_2_tri=ifelse(bio_2_tri>=31, 30, bio_2_tri),
         qui_2_tri=ifelse(qui_2_tri>=31, 30, qui_2_tri),
         fis_2_tri=ifelse(fis_2_tri>=31, 30, fis_2_tri),
         cien_2_tri=ifelse(cien_2_tri>=31, 30, cien_2_tri),
         art_2_tri=ifelse(art_2_tri>=31, 30, art_2_tri),
         ed_fis_2_tri=ifelse(ed_fis_2_tri>=31, 30, ed_fis_2_tri),
         lp_3_tri=ifelse(lp_3_tri>=41, 40, lp_3_tri),
         mat_3_tri=ifelse(mat_3_tri>=41, 40, mat_3_tri),
         geo_3_tri=ifelse(geo_3_tri>=41, 40, geo_3_tri),
         hist_3_tri=ifelse(hist_3_tri>=41, 40, hist_3_tri),
         soc_3_tri=ifelse(soc_3_tri>=41, 40, soc_3_tri),
         fil_3_tri=ifelse(fil_3_tri>=41, 40, fil_3_tri),
         bio_3_tri=ifelse(bio_3_tri>=41, 40, bio_3_tri),
         qui_3_tri=ifelse(qui_3_tri>=41, 40, qui_3_tri),
         fis_3_tri=ifelse(fis_3_tri>=41, 40, fis_3_tri),
         cien_3_tri=ifelse(cien_3_tri>=41, 40, cien_3_tri),
         art_3_tri=ifelse(art_3_tri>=41, 40, art_3_tri),
         ed_fis_3_tri=ifelse(ed_fis_3_tri>=41, 40, ed_fis_3_tri))

dup2_seges_2020 <- dup2_seges_2020 %>%
  mutate(ID2_NIVEL_MATRICULA=ifelse(ID_NIVEL_MATRICULA==4|ID_NIVEL_MATRICULA==38, 1, 0),
         ID3_NIVEL_MATRICULA=ifelse(ID_NIVEL_MATRICULA==5|ID_NIVEL_MATRICULA==6, 2, 0),
         lp_1_tri=ifelse(is.na(lp_1_tri), as.numeric(NA), lp_1_tri),
         lp_2_tri=ifelse(is.na(lp_2_tri), as.numeric(NA), lp_2_tri),
         lp_3_tri=ifelse(is.na(lp_3_tri), as.numeric(NA), lp_3_tri),
         mat_1_tri=ifelse(is.na(mat_1_tri), as.numeric(NA), mat_1_tri),
         mat_2_tri=ifelse(is.na(mat_2_tri), as.numeric(NA), mat_2_tri),
         mat_3_tri=ifelse(is.na(mat_3_tri), as.numeric(NA), mat_3_tri),
         geo_1_tri=ifelse(is.na(geo_1_tri), as.numeric(NA), geo_1_tri),
         geo_2_tri=ifelse(is.na(geo_2_tri), as.numeric(NA), geo_2_tri),
         geo_3_tri=ifelse(is.na(geo_3_tri), as.numeric(NA), geo_3_tri),
         hist_1_tri=ifelse(is.na(hist_1_tri), as.numeric(NA), hist_1_tri),
         hist_2_tri=ifelse(is.na(hist_2_tri), as.numeric(NA), hist_2_tri),
         hist_3_tri=ifelse(is.na(hist_3_tri), as.numeric(NA), hist_3_tri),
         soc_1_tri=ifelse(is.na(soc_1_tri), as.numeric(NA), soc_1_tri),
         soc_2_tri=ifelse(is.na(soc_2_tri), as.numeric(NA), soc_2_tri),
         soc_3_tri=ifelse(is.na(soc_3_tri), as.numeric(NA), soc_3_tri),
         fil_1_tri=ifelse(is.na(fil_1_tri), as.numeric(NA), fil_1_tri),
         fil_2_tri=ifelse(is.na(fil_2_tri), as.numeric(NA), fil_2_tri),
         fil_3_tri=ifelse(is.na(fil_3_tri), as.numeric(NA), fil_3_tri),
         bio_1_tri=ifelse(is.na(bio_1_tri), as.numeric(NA), bio_1_tri),
         bio_2_tri=ifelse(is.na(bio_2_tri), as.numeric(NA), bio_2_tri),
         bio_3_tri=ifelse(is.na(bio_3_tri), as.numeric(NA), bio_3_tri),
         qui_1_tri=ifelse(is.na(qui_1_tri), as.numeric(NA), qui_1_tri),
         qui_2_tri=ifelse(is.na(qui_2_tri), as.numeric(NA), qui_2_tri),
         qui_3_tri=ifelse(is.na(qui_3_tri), as.numeric(NA), qui_3_tri),
         fis_1_tri=ifelse(is.na(fis_1_tri), as.numeric(NA), fis_1_tri),
         fis_2_tri=ifelse(is.na(fis_2_tri), as.numeric(NA), fis_2_tri),
         fis_3_tri=ifelse(is.na(fis_3_tri), as.numeric(NA), fis_3_tri),
         cien_1_tri=ifelse(is.na(cien_1_tri), as.numeric(NA), cien_1_tri),
         cien_2_tri=ifelse(is.na(cien_2_tri), as.numeric(NA), cien_2_tri),
         cien_3_tri=ifelse(is.na(cien_3_tri), as.numeric(NA), cien_3_tri),
         art_1_tri=ifelse(is.na(art_1_tri), as.numeric(NA), art_1_tri),
         art_2_tri=ifelse(is.na(art_2_tri), as.numeric(NA), art_2_tri),
         art_3_tri=ifelse(is.na(art_3_tri), as.numeric(NA), art_3_tri),
         ed_fis_1_tri=ifelse(is.na(ed_fis_1_tri), as.numeric(NA), ed_fis_1_tri),
         ed_fis_2_tri=ifelse(is.na(ed_fis_2_tri), as.numeric(NA), ed_fis_2_tri),
         ed_fis_3_tri=ifelse(is.na(ed_fis_3_tri), as.numeric(NA), ed_fis_3_tri))

table(dup2_seges_2020$lp_1_tri)
table(dup2_seges_2020$lp_2_tri)
table(dup2_seges_2020$lp_3_tri)
table(dup2_seges_2020$ID2_NIVEL_MATRICULA)
describe(dup2_seges_2020$ID3_NIVEL_MATRICULA)


dup_EM_seges_2020 <- dup2_seges_2020 %>%  filter(ID_NIVEL_MATRICULA==5|ID_NIVEL_MATRICULA==6)
dup_EF_seges_2020 <- dup2_seges_2020 %>%  filter(ID_NIVEL_MATRICULA==4|ID_NIVEL_MATRICULA==38)

dup_EM_seges_2020 <- dup_EM_seges_2020[, c("CENSO_ESCOLA_ENTURMACAO","ID_TURMA","RA","NU_CPF","CD_INEP_ALUNO","DC_COR_RACA","IDADE","TP_SEXO","ID_TURNO",
                                           "ID_NIVEL_MATRICULA","dt_matric","dt_encerr","SITUACAO","CO_SITUACAO","EMnotas1tri_semNA",
                                           "EMnotas2tri_semNA","EMnotas3tri_semNA","EMnotas_semNA","EMfaltas1tri_semNA","EMfaltas2tri_semNA",
                                           "EMfaltas3tri_semNA","EMfaltas_semNA","EMaulas1tri_semNA","EMaulas2tri_semNA","EMaulas3tri_semNA",
                                           "EMaulas_semNA","switch_escola","qt_escolas","qt_switch_escola","switch_munic","qt_switch_munic",
                                           "qt_munic","switch_turno","qt_turno","qt_switch_turno","switch_turma","qt_turma","qt_switch_turma",
                                           "hist_2_tri","hist_1_tri","hist_3_tri","geo_3_tri","geo_1_tri","geo_2_tri","soc_2_tri",
                                           "soc_1_tri","soc_3_tri","fil_3_tri","fil_2_tri","art_rec_2_tri","bio_rec_2_tri","ed_fis_rec_2_tri",
                                           "ed_fis_rec_1_tri","fil_rec_2_tri","fis_rec_2_tri","fis_rec_1_tri","geo_rec_2_tri","geo_rec_1_tri",
                                           "hist_rec_1_tri","hist_rec_2_tri","lp_rec_1_tri","mat_rec_2_tri","mat_rec_1_tri","qui_rec_2_tri",
                                           "qui_rec_1_tri","soc_rec_1_tri","lp_2_tri","lp_1_tri","lp_3_tri","ed_fis_2_tri","art_2_tri",
                                           "art_1_tri","fis_3_tri","fis_2_tri","fis_1_tri","qui_2_tri","qui_3_tri","qui_1_tri","bio_1_tri",
                                           "mat_1_tri","mat_3_tri","art_rec_1_tri","bio_rec_1_tri","fil_rec_1_tri","lp_rec_2_tri",
                                           "soc_rec_2_tri","ed_fis_3_tri","ed_fis_1_tri","art_3_tri","bio_2_tri","bio_3_tri","mat_2_tri",
                                           "fil_1_tri","hist_falta_2_tri","hist_falta_1_tri","hist_falta_3_tri","geo_falta_3_tri",
                                           "geo_falta_1_tri","geo_falta_2_tri","soc_falta_2_tri","soc_falta_1_tri","soc_falta_3_tri",
                                           "fil_falta_3_tri","fil_falta_2_tri","lp_falta_2_tri","lp_falta_1_tri","lp_falta_3_tri",
                                           "ed_fis_falta_2_tri","art_falta_2_tri","art_falta_1_tri","fis_falta_3_tri","fis_falta_2_tri",
                                           "fis_falta_1_tri","qui_falta_2_tri","qui_falta_3_tri","qui_falta_1_tri","bio_falta_1_tri",
                                           "mat_falta_1_tri","mat_falta_3_tri","ed_fis_falta_3_tri","ed_fis_falta_1_tri","art_falta_3_tri",
                                           "bio_falta_2_tri","bio_falta_3_tri","mat_falta_2_tri","fil_falta_1_tri","hist_aula_2_tri",
                                           "hist_aula_1_tri","hist_aula_3_tri","geo_aula_3_tri","geo_aula_1_tri","geo_aula_2_tri",
                                           "soc_aula_2_tri","soc_aula_1_tri","soc_aula_3_tri","fil_aula_3_tri","fil_aula_2_tri",
                                           "lp_aula_2_tri","lp_aula_1_tri","lp_aula_3_tri","ed_fis_aula_2_tri","art_aula_2_tri",
                                           "art_aula_1_tri","fis_aula_3_tri","fis_aula_2_tri","fis_aula_1_tri","qui_aula_2_tri",
                                           "qui_aula_3_tri","qui_aula_1_tri","bio_aula_1_tri","mat_aula_1_tri","mat_aula_3_tri",
                                           "ed_fis_aula_3_tri","ed_fis_aula_1_tri","art_aula_3_tri","bio_aula_2_tri","bio_aula_3_tri",
                                           "mat_aula_2_tri","fil_aula_1_tri","ID_ETAPA_MATRICULA")]

dup_EF_seges_2020 <- dup_EF_seges_2020[, c("CENSO_ESCOLA_ENTURMACAO","ID_TURMA","RA","NU_CPF","CD_INEP_ALUNO","DC_COR_RACA","IDADE","TP_SEXO","ID_TURNO",
                                           "ID_NIVEL_MATRICULA","dt_matric","dt_encerr","SITUACAO","CO_SITUACAO",
                                           "EFnotas1tri_semNA","EFnotas2tri_semNA","EFnotas3tri_semNA","EFnotas_semNA","EFfaltas1tri_semNA",
                                           "EFfaltas2tri_semNA","EFfaltas3tri_semNA","EFfaltas_semNA","EFaulas1tri_semNA","EFaulas2tri_semNA",
                                           "EFaulas3tri_semNA","EFaulas_semNA","switch_escola","qt_escolas","qt_switch_escola",
                                           "switch_munic","qt_switch_munic","qt_munic","switch_turno","qt_turno","qt_switch_turno",
                                           "switch_turma","qt_turma","qt_switch_turma","hist_2_tri","hist_1_tri","hist_3_tri","geo_3_tri",
                                           "geo_1_tri","geo_2_tri","art_rec_2_tri","ed_fis_rec_2_tri","ed_fis_rec_1_tri","geo_rec_2_tri",
                                           "geo_rec_1_tri","hist_rec_1_tri","hist_rec_2_tri","lp_rec_1_tri","mat_rec_2_tri","mat_rec_1_tri",
                                           "lp_2_tri","lp_1_tri","lp_3_tri","ed_fis_2_tri","art_2_tri","art_1_tri","mat_1_tri","mat_3_tri",
                                           "art_rec_1_tri","lp_rec_2_tri","ed_fis_3_tri","ed_fis_1_tri","art_3_tri","mat_2_tri","cien_3_tri",
                                           "cien_2_tri","cien_1_tri","cien_rec_2_tri","cien_rec_1_tri","hist_falta_2_tri","hist_falta_1_tri",
                                           "hist_falta_3_tri","geo_falta_3_tri","geo_falta_1_tri","geo_falta_2_tri","lp_falta_2_tri",
                                           "lp_falta_1_tri","lp_falta_3_tri","ed_fis_falta_2_tri","art_falta_2_tri","art_falta_1_tri",
                                           "mat_falta_1_tri","mat_falta_3_tri","ed_fis_falta_3_tri","ed_fis_falta_1_tri","art_falta_3_tri",
                                           "mat_falta_2_tri","cien_falta_3_tri","cien_falta_2_tri","cien_falta_1_tri","hist_aula_2_tri",
                                           "hist_aula_1_tri","hist_aula_3_tri","geo_aula_3_tri","geo_aula_1_tri","geo_aula_2_tri",
                                           "lp_aula_2_tri","lp_aula_1_tri","lp_aula_3_tri","ed_fis_aula_2_tri","art_aula_2_tri",
                                           "art_aula_1_tri","mat_aula_1_tri","mat_aula_3_tri","ed_fis_aula_3_tri","ed_fis_aula_1_tri",
                                           "art_aula_3_tri","mat_aula_2_tri","cien_aula_3_tri","cien_aula_2_tri","cien_aula_1_tri",
                                           "ID_ETAPA_MATRICULA")]

dup_EM_seges_2020 <- dup_EM_seges_2020 %>%
  group_by(ID_TURMA) %>% 
  mutate(NOTA_TURMA_PT=mean(s(lp_1_tri+lp_2_tri+lp_3_tri)),
         NOTA_TURMA_MT=mean(s(mat_1_tri+mat_2_tri+mat_3_tri)),
         NOTA_TURMA_GEO=mean(s(geo_1_tri+geo_2_tri+geo_3_tri)),
         NOTA_TURMA_HIS=mean(s(hist_1_tri+hist_2_tri+hist_3_tri)),
         NOTA_TURMA_SOC=mean(s(soc_1_tri+soc_2_tri+soc_3_tri)),
         NOTA_TURMA_FIL=mean(s(fil_1_tri+fil_2_tri+fil_3_tri)),
         NOTA_TURMA_BIO=mean(s(bio_1_tri+bio_2_tri+bio_3_tri)),
         NOTA_TURMA_QUI=mean(s(qui_1_tri+qui_2_tri+qui_3_tri)),
         NOTA_TURMA_FIS=mean(s(fis_1_tri+fis_2_tri+fis_3_tri)),
         NOTA_TURMA_ART=mean(s(art_1_tri+art_2_tri+art_3_tri)),
         NOTA_TURMA_EDFIS=mean(s(ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri)),
         NOTA_TURMA_EM_TOTAL=mean(s(lp_1_tri+lp_2_tri+lp_3_tri+ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri+mat_1_tri+mat_2_tri+mat_3_tri+
                                      geo_1_tri+geo_2_tri+geo_3_tri+hist_1_tri+hist_2_tri+hist_3_tri+soc_1_tri+soc_2_tri+soc_3_tri+
                                      fil_1_tri+fil_2_tri+fil_3_tri+bio_1_tri+bio_2_tri+bio_3_tri+qui_1_tri+qui_2_tri+qui_3_tri+
                                      fis_1_tri+fis_2_tri+fis_3_tri+art_1_tri+art_2_tri+art_3_tri)),
         NOTA_TURMA_TRI1PT=mean(s(lp_1_tri)),
         NOTA_TURMA_TRI1MT=mean(s(mat_1_tri)),
         NOTA_TURMA_TRI1GEO=mean(s(geo_1_tri)),
         NOTA_TURMA_TRI1HIS=mean(s(hist_1_tri)),
         NOTA_TURMA_TRI1SOC=mean(s(soc_1_tri)),
         NOTA_TURMA_TRI1FIL=mean(s(fil_1_tri)),
         NOTA_TURMA_TRI1BIO=mean(s(bio_1_tri)),
         NOTA_TURMA_TRI1QUI=mean(s(qui_1_tri)),
         NOTA_TURMA_TRI1FIS=mean(s(fis_1_tri)),
         NOTA_TURMA_TRI1ART=mean(s(art_1_tri)),
         NOTA_TURMA_TRI1EDFIS=mean(s(ed_fis_1_tri)),
         NOTA_TURMA_EM_TRI1TOTAL=mean(s(lp_1_tri+mat_1_tri+geo_1_tri+hist_1_tri+soc_1_tri+fil_1_tri+
                                          bio_1_tri+qui_1_tri+fis_1_tri+art_1_tri+ed_fis_1_tri))) %>% 
  ungroup()

dup_EF_seges_2020 <- dup_EF_seges_2020 %>%
  group_by(ID_TURMA) %>% 
  mutate(NOTA_TURMA_PT=mean(s(lp_1_tri+lp_2_tri+lp_3_tri)),
         NOTA_TURMA_MT=mean(s(mat_1_tri+mat_2_tri+mat_3_tri)),
         NOTA_TURMA_GEO=mean(s(geo_1_tri+geo_2_tri+geo_3_tri)),
         NOTA_TURMA_HIS=mean(s(hist_1_tri+hist_2_tri+hist_3_tri)),
         NOTA_TURMA_CIEN=mean(s(cien_1_tri+cien_2_tri+cien_3_tri)),
         NOTA_TURMA_ART=mean(s(art_1_tri+art_2_tri+art_3_tri)),
         NOTA_TURMA_EDFIS=mean(s(ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri)),
         NOTA_TURMA_EF_TOTAL=mean(s(lp_1_tri+lp_2_tri+lp_3_tri+ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri+mat_1_tri+mat_2_tri+mat_3_tri+
                                      geo_1_tri+geo_2_tri+geo_3_tri+hist_1_tri+hist_2_tri+hist_3_tri+cien_1_tri+cien_2_tri+cien_3_tri+
                                      art_1_tri+art_2_tri+art_3_tri)),
         NOTA_TURMA_TRI1PT=mean(s(lp_1_tri)),
         NOTA_TURMA_TRI1MT=mean(s(mat_1_tri)),
         NOTA_TURMA_TRI1GEO=mean(s(geo_1_tri)),
         NOTA_TURMA_TRI1HIS=mean(s(hist_1_tri)),
         NOTA_TURMA_TRI1CIEN=mean(s(cien_1_tri)),
         NOTA_TURMA_TRI1ART=mean(s(art_1_tri)),
         NOTA_TURMA_TRI1EDFIS=mean(s(ed_fis_1_tri)),
         NOTA_TURMA_EF_TRI1TOTAL=mean(s(lp_1_tri+mat_1_tri+geo_1_tri+hist_1_tri+cien_1_tri+art_1_tri+ed_fis_1_tri))) %>% 
  ungroup()

colnames(dup3_seges_2020)
describe(dup_EF_seges_2020$NOTA_TURMA_EF_TOTAL)
describe(dup_EF_seges_2020$NOTA_TURMA_MT)
describe(dup_EF_seges_2020$NOTA_TURMA_TRI1PT)
describe(dup_EM_seges_2020$NOTA_TURMA_EM_TRI1TOTAL)
describe(dup_EM_seges_2020$NOTA_TURMA_EM_TOTAL)

x49c <- subset(dup_EF_seges_2020, NOTA_TURMA_MT>=100)
x49c <- x49c %>%  select(ID_TURMA, CD_INEP_ALUNO, mat_1_tri, mat_2_tri, mat_3_tri, NOTA_TURMA_MT, NOTA_TURMA_TRI1MT,
                         NOTA_TURMA_EF_TOTAL, SITUACAO)

x49c <- subset(dup_EF_seges_2020, NOTA_TURMA_TRI1PT>=30)
x49c <- x49c %>%  select(ID_TURMA, CD_INEP_ALUNO, lp_1_tri, lp_2_tri, lp_3_tri, NOTA_TURMA_PT, NOTA_TURMA_TRI1PT,
                         NOTA_TURMA_EF_TOTAL, SITUACAO)
rm(x49c)

dup_EM_seges_2020 <- dup_EM_seges_2020 %>%
  group_by(CENSO_ESCOLA_ENTURMACAO) %>% 
  mutate(NOTA_ESCOLA_PT=mean(s(lp_1_tri+lp_2_tri+lp_3_tri)),
         NOTA_ESCOLA_MT=mean(s(mat_1_tri+mat_2_tri+mat_3_tri)),
         NOTA_ESCOLA_GEO=mean(s(geo_1_tri+geo_2_tri+geo_3_tri)),
         NOTA_ESCOLA_HIS=mean(s(hist_1_tri+hist_2_tri+hist_3_tri)),
         NOTA_ESCOLA_SOC=mean(s(soc_1_tri+soc_2_tri+soc_3_tri)),
         NOTA_ESCOLA_FIL=mean(s(fil_1_tri+fil_2_tri+fil_3_tri)),
         NOTA_ESCOLA_BIO=mean(s(bio_1_tri+bio_2_tri+bio_3_tri)),
         NOTA_ESCOLA_QUI=mean(s(qui_1_tri+qui_2_tri+qui_3_tri)),
         NOTA_ESCOLA_FIS=mean(s(fis_1_tri+fis_2_tri+fis_3_tri)),
         NOTA_ESCOLA_ART=mean(s(art_1_tri+art_2_tri+art_3_tri)),
         NOTA_ESCOLA_EDFIS=mean(s(ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri)),
         NOTA_ESCOLA_TOTAL=mean(s(lp_1_tri+lp_2_tri+lp_3_tri+ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri+mat_1_tri+mat_2_tri+mat_3_tri+
                                    geo_1_tri+geo_2_tri+geo_3_tri+hist_1_tri+hist_2_tri+hist_3_tri+soc_1_tri+soc_2_tri+soc_3_tri+
                                    fil_1_tri+fil_2_tri+fil_3_tri+bio_1_tri+bio_2_tri+bio_3_tri+qui_1_tri+qui_2_tri+qui_3_tri+
                                    fis_1_tri+fis_2_tri+fis_3_tri+art_1_tri+art_2_tri+art_3_tri)),
         NOTA_ESCOLA_TRI1PT=mean(s(lp_1_tri)),
         NOTA_ESCOLA_TRI1MT=mean(s(mat_1_tri)),
         NOTA_ESCOLA_TRI1GEO=mean(s(geo_1_tri)),
         NOTA_ESCOLA_TRI1HIS=mean(s(hist_1_tri)),
         NOTA_ESCOLA_TRI1SOC=mean(s(soc_1_tri)),
         NOTA_ESCOLA_TRI1FIL=mean(s(fil_1_tri)),
         NOTA_ESCOLA_TRI1BIO=mean(s(bio_1_tri)),
         NOTA_ESCOLA_TRI1QUI=mean(s(qui_1_tri)),
         NOTA_ESCOLA_TRI1FIS=mean(s(fis_1_tri)),
         NOTA_ESCOLA_TRI1ART=mean(s(art_1_tri)),
         NOTA_ESCOLA_TRI1EDFIS=mean(s(ed_fis_1_tri)),
         NOTA_ESCOLA_TRI1TOTAL=mean(s(lp_1_tri+mat_1_tri+geo_1_tri+hist_1_tri+soc_1_tri+fil_1_tri+bio_1_tri+qui_1_tri+fis_1_tri+
                                        art_1_tri+ed_fis_1_tri))) %>%
  ungroup()

dup_EF_seges_2020 <- dup_EF_seges_2020 %>%
  group_by(CENSO_ESCOLA_ENTURMACAO) %>% 
  mutate(NOTA_ESCOLA_PT=mean(s(lp_1_tri+lp_2_tri+lp_3_tri)),
         NOTA_ESCOLA_MT=mean(s(mat_1_tri+mat_2_tri+mat_3_tri)),
         NOTA_ESCOLA_GEO=mean(s(geo_1_tri+geo_2_tri+geo_3_tri)),
         NOTA_ESCOLA_HIS=mean(s(hist_1_tri+hist_2_tri+hist_3_tri)),
         NOTA_ESCOLA_CIEN=mean(s(cien_1_tri+cien_2_tri+cien_3_tri)),
         NOTA_ESCOLA_ART=mean(s(art_1_tri+art_2_tri+art_3_tri)),
         NOTA_ESCOLA_EDFIS=mean(s(ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri)),
         NOTA_ESCOLA_TOTAL=mean(s(lp_1_tri+lp_2_tri+lp_3_tri+ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri+mat_1_tri+mat_2_tri+mat_3_tri+
                                    geo_1_tri+geo_2_tri+geo_3_tri+hist_1_tri+hist_2_tri+hist_3_tri+cien_1_tri+cien_2_tri+cien_3_tri+
                                    art_1_tri+art_2_tri+art_3_tri)),
         NOTA_ESCOLA_TRI1PT=mean(s(lp_1_tri)),
         NOTA_ESCOLA_TRI1MT=mean(s(mat_1_tri)),
         NOTA_ESCOLA_TRI1GEO=mean(s(geo_1_tri)),
         NOTA_ESCOLA_TRI1HIS=mean(s(hist_1_tri)),
         NOTA_ESCOLA_TRI1CIEN=mean(s(cien_1_tri)),
         NOTA_ESCOLA_TRI1ART=mean(s(art_1_tri)),
         NOTA_ESCOLA_TRI1EDFIS=mean(s(ed_fis_1_tri)),
         NOTA_ESCOLA_TRI1TOTAL=mean(s(lp_1_tri+mat_1_tri+geo_1_tri+hist_1_tri+cien_1_tri+art_1_tri+ed_fis_1_tri))) %>%
  ungroup()

describe(dup_EF_seges_2020$NOTA_ESCOLA_TOTAL)
describe(dup_EF_seges_2020$NOTA_ESCOLA_TRI1TOTAL)
describe(dup_EF_seges_2020$NOTA_ESCOLA_TRI1PT)
describe(dup_EM_seges_2020$NOTA_ESCOLA_TOTAL)
describe(dup_EM_seges_2020$NOTA_ESCOLA_TRI1TOTAL)

x49c <- subset(dup_EF_seges_2020, NOTA_ESCOLA_TOTAL>=700)
x49c <- x49c %>%  select(CENSO_ESCOLA_ENTURMACAO, ID_TURMA, CD_INEP_ALUNO, mat_1_tri, mat_2_tri, mat_3_tri, NOTA_TURMA_MT, NOTA_ESCOLA_TRI1MT,
                         NOTA_ESCOLA_TOTAL, SITUACAO)

x49c <- subset(dup_EF_seges_2020, NOTA_TURMA_TRI1PT>=30)
x49c <- x49c %>%  select(ID_TURMA, CD_INEP_ALUNO, lp_1_tri, lp_2_tri, lp_3_tri, NOTA_TURMA_PT, NOTA_TURMA_TRI1PT,
                         NOTA_TURMA_EF_TOTAL, SITUACAO)
rm(x49c)

dup_EM_seges_2020 <- dup_EM_seges_2020 %>%
  group_by(RA) %>% 
  mutate(PROP_FALTA_PT=sum_(lp_falta_1_tri+lp_falta_2_tri+lp_falta_3_tri)/sum_(lp_aula_1_tri+lp_aula_2_tri+lp_aula_3_tri),
         PROP_FALTA_MT=sum_(mat_falta_1_tri+mat_falta_2_tri+mat_falta_3_tri)/sum_(mat_aula_1_tri+mat_aula_2_tri+mat_aula_3_tri),
         PROP_FALTA_GEO=sum_(geo_falta_1_tri+geo_falta_2_tri+geo_falta_3_tri)/sum_(geo_aula_1_tri+geo_aula_2_tri+geo_aula_3_tri),
         PROP_FALTA_HIS=sum_(hist_falta_1_tri+hist_falta_2_tri+hist_falta_3_tri)/sum_(hist_aula_1_tri+hist_aula_2_tri+hist_aula_3_tri),
         PROP_FALTA_SOC=sum_(soc_falta_1_tri+soc_falta_2_tri+soc_falta_3_tri)/sum_(soc_aula_1_tri+soc_aula_2_tri+soc_aula_3_tri),
         PROP_FALTA_FIL=sum_(fil_falta_1_tri+fil_falta_2_tri+fil_falta_3_tri)/sum_(fil_aula_1_tri+fil_aula_2_tri+fil_aula_3_tri),
         PROP_FALTA_BIO=sum_(bio_falta_1_tri+bio_falta_2_tri+bio_falta_3_tri)/sum_(bio_aula_1_tri+bio_aula_2_tri+bio_aula_3_tri),
         PROP_FALTA_QUI=sum_(qui_falta_1_tri+qui_falta_2_tri+qui_falta_3_tri)/sum_(qui_aula_1_tri+qui_aula_2_tri+qui_aula_3_tri),
         PROP_FALTA_FIS=sum_(fis_falta_1_tri+fis_falta_2_tri+fis_falta_3_tri)/sum_(fis_aula_1_tri+fis_aula_2_tri+fis_aula_3_tri),
         PROP_FALTA_ART=sum_(art_falta_1_tri+art_falta_2_tri+art_falta_3_tri)/sum_(art_aula_1_tri+art_aula_2_tri+art_aula_3_tri),
         PROP_FALTA_EDFIS=sum_(ed_fis_falta_1_tri+ed_fis_falta_2_tri+ed_fis_falta_3_tri)/sum_(ed_fis_aula_1_tri+ed_fis_aula_2_tri+ed_fis_aula_3_tri),
         PROP_FALTA_TOTAL=sum_(lp_falta_1_tri+lp_falta_2_tri+lp_falta_3_tri+mat_falta_1_tri+mat_falta_2_tri+mat_falta_3_tri+
                                 geo_falta_1_tri+geo_falta_2_tri+geo_falta_3_tri+hist_falta_1_tri+hist_falta_2_tri+hist_falta_3_tri+
                                 soc_falta_1_tri+soc_falta_2_tri+soc_falta_3_tri+fil_falta_1_tri+fil_falta_2_tri+fil_falta_3_tri+
                                 bio_falta_1_tri+bio_falta_2_tri+bio_falta_3_tri+qui_falta_1_tri+qui_falta_2_tri+qui_falta_3_tri+
                                 fis_falta_1_tri+fis_falta_2_tri+fis_falta_3_tri+art_falta_1_tri+art_falta_2_tri+art_falta_3_tri+
                                 ed_fis_falta_1_tri+ed_fis_falta_2_tri+ed_fis_falta_3_tri)/sum_(lp_aula_1_tri+lp_aula_2_tri+lp_aula_3_tri+
                                                                                                  mat_aula_1_tri+mat_aula_2_tri+mat_aula_3_tri+
                                                                                                  geo_aula_1_tri+geo_aula_2_tri+geo_aula_3_tri+
                                                                                                  hist_aula_1_tri+hist_aula_2_tri+hist_aula_3_tri+
                                                                                                  soc_aula_1_tri+soc_aula_2_tri+soc_aula_3_tri+
                                                                                                  fil_aula_1_tri+fil_aula_2_tri+fil_aula_3_tri+
                                                                                                  bio_aula_1_tri+bio_aula_2_tri+bio_aula_3_tri+
                                                                                                  qui_aula_1_tri+qui_aula_2_tri+qui_aula_3_tri+
                                                                                                  fis_aula_1_tri+fis_aula_2_tri+fis_aula_3_tri+
                                                                                                  art_aula_1_tri+art_aula_2_tri+art_aula_3_tri+
                                                                                                  ed_fis_aula_1_tri+ed_fis_aula_2_tri+ed_fis_aula_3_tri),
         PROP_FALTA_TRI1PT=(lp_falta_1_tri)/(lp_aula_1_tri),
         PROP_FALTA_TRI1MT=(mat_falta_1_tri)/(mat_aula_1_tri),
         PROP_FALTA_TRI1GEO=(geo_falta_1_tri)/(geo_aula_1_tri),
         PROP_FALTA_TRI1HIS=(hist_falta_1_tri)/(hist_aula_1_tri),
         PROP_FALTA_TRI1SOC=(soc_falta_1_tri)/(soc_aula_1_tri),
         PROP_FALTA_TRI1FIL=(fil_falta_1_tri)/(fil_aula_1_tri),
         PROP_FALTA_TRI1BIO=(bio_falta_1_tri)/(bio_aula_1_tri),
         PROP_FALTA_TRI1QUI=(qui_falta_1_tri)/(qui_aula_1_tri),
         PROP_FALTA_TRI1FIS=(fis_falta_1_tri)/(fis_aula_1_tri),
         PROP_FALTA_TRI1ART=(art_falta_1_tri)/(art_aula_1_tri),
         PROP_FALTA_TRI1EDFIS=(ed_fis_falta_1_tri)/(ed_fis_aula_1_tri),
         PROP_FALTA_TRI1TOTAL=sum_(lp_falta_1_tri+mat_falta_1_tri+geo_falta_1_tri+hist_falta_1_tri+soc_falta_1_tri+fil_falta_1_tri+
                                     bio_falta_1_tri+qui_falta_1_tri+fis_falta_1_tri+art_falta_1_tri+ed_fis_falta_1_tri)/sum_(lp_aula_1_tri+mat_aula_1_tri+
                                                                                                                                geo_aula_1_tri+hist_aula_1_tri+
                                                                                                                                soc_aula_1_tri+fil_aula_1_tri+
                                                                                                                                bio_aula_1_tri+qui_aula_1_tri+
                                                                                                                                fis_aula_1_tri+art_aula_1_tri+
                                                                                                                                ed_fis_aula_1_tri)) %>% 
  ungroup()

describe(dup_EM_seges_2020$PROP_FALTA_MT)
describe(dup_EM_seges_2020$PROP_FALTA_TRI1MT)
describe(dup_EM_seges_2020$PROP_FALTA_TOTAL)
describe(dup_EM_seges_2020$PROP_FALTA_TRI1TOTAL)


dup_EF_seges_2020 <- dup_EF_seges_2020 %>%
  group_by(RA) %>% 
  mutate(PROP_FALTA_PT=sum_(lp_falta_1_tri+lp_falta_2_tri+lp_falta_3_tri)/sum_(lp_aula_1_tri+lp_aula_2_tri+lp_aula_3_tri),
         PROP_FALTA_MT=sum_(mat_falta_1_tri+mat_falta_2_tri+mat_falta_3_tri)/sum_(mat_aula_1_tri+mat_aula_2_tri+mat_aula_3_tri),
         PROP_FALTA_GEO=sum_(geo_falta_1_tri+geo_falta_2_tri+geo_falta_3_tri)/sum_(geo_aula_1_tri+geo_aula_2_tri+geo_aula_3_tri),
         PROP_FALTA_HIS=sum_(hist_falta_1_tri+hist_falta_2_tri+hist_falta_3_tri)/sum_(hist_aula_1_tri+hist_aula_2_tri+hist_aula_3_tri),
         PROP_FALTA_CIEN=sum_(cien_falta_1_tri+cien_falta_2_tri+cien_falta_3_tri)/sum_(cien_aula_1_tri+cien_aula_2_tri+cien_aula_3_tri),
         PROP_FALTA_ART=sum_(art_falta_1_tri+art_falta_2_tri+art_falta_3_tri)/sum_(art_aula_1_tri+art_aula_2_tri+art_aula_3_tri),
         PROP_FALTA_EDFIS=sum_(ed_fis_falta_1_tri+ed_fis_falta_2_tri+ed_fis_falta_3_tri)/sum_(ed_fis_aula_1_tri+ed_fis_aula_2_tri+ed_fis_aula_3_tri),
         PROP_FALTA_TOTAL=sum_(lp_falta_1_tri+lp_falta_2_tri+lp_falta_3_tri+mat_falta_1_tri+mat_falta_2_tri+mat_falta_3_tri+
                                 geo_falta_1_tri+geo_falta_2_tri+geo_falta_3_tri+hist_falta_1_tri+hist_falta_2_tri+hist_falta_3_tri+
                                 cien_falta_1_tri+cien_falta_2_tri+cien_falta_3_tri+art_falta_1_tri+art_falta_2_tri+art_falta_3_tri+
                                 ed_fis_falta_1_tri+ed_fis_falta_2_tri+ed_fis_falta_3_tri)/sum_(lp_aula_1_tri+lp_aula_2_tri+lp_aula_3_tri+
                                                                                                  mat_aula_1_tri+mat_aula_2_tri+mat_aula_3_tri+
                                                                                                  geo_aula_1_tri+geo_aula_2_tri+geo_aula_3_tri+
                                                                                                  hist_aula_1_tri+hist_aula_2_tri+hist_aula_3_tri+
                                                                                                  cien_aula_1_tri+cien_aula_2_tri+cien_aula_3_tri+
                                                                                                  art_aula_1_tri+art_aula_2_tri+art_aula_3_tri+
                                                                                                  ed_fis_aula_1_tri+ed_fis_aula_2_tri+ed_fis_aula_3_tri),
         PROP_FALTA_TRI1PT=(lp_falta_1_tri)/(lp_aula_1_tri),
         PROP_FALTA_TRI1MT=(mat_falta_1_tri)/(mat_aula_1_tri),
         PROP_FALTA_TRI1GEO=(geo_falta_1_tri)/(geo_aula_1_tri),
         PROP_FALTA_TRI1HIS=(hist_falta_1_tri)/(hist_aula_1_tri),
         PROP_FALTA_TRI1CIEN=(cien_falta_1_tri)/(cien_aula_1_tri),
         PROP_FALTA_TRI1ART=(art_falta_1_tri)/(art_aula_1_tri),
         PROP_FALTA_TRI1EDFIS=(ed_fis_falta_1_tri)/(ed_fis_aula_1_tri),
         PROP_FALTA_TRI1TOTAL=sum_(lp_falta_1_tri+mat_falta_1_tri+geo_falta_1_tri+hist_falta_1_tri+cien_falta_1_tri+art_falta_1_tri+
                                     ed_fis_falta_1_tri)/sum_(lp_aula_1_tri+mat_aula_1_tri+geo_aula_1_tri+hist_aula_1_tri+cien_aula_1_tri+
                                                                art_aula_1_tri+ed_fis_aula_1_tri)) %>% 
  ungroup()


dup_EM_seges_2020 <- dup_EM_seges_2020 %>%
  mutate(DUMMYNOTA_PT=ifelse((lp_1_tri+lp_2_tri+lp_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1PT=ifelse(lp_1_tri<=15, 1, 0),
         DUMMYNOTA_MT=ifelse((mat_1_tri+mat_2_tri+mat_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1MT=ifelse(mat_1_tri<=15, 1, 0),
         DUMMYNOTA_GEO=ifelse((geo_1_tri+geo_2_tri+geo_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1GEO=ifelse(geo_1_tri<=15, 1, 0),
         DUMMYNOTA_HIS=ifelse((hist_1_tri+hist_2_tri+hist_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1HIS=ifelse(hist_1_tri<=15, 1, 0),
         DUMMYNOTA_SOC=ifelse((soc_1_tri+soc_2_tri+soc_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1SOC=ifelse(soc_1_tri<=15, 1, 0),
         DUMMYNOTA_FIL=ifelse((fil_1_tri+fil_2_tri+fil_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1FIL=ifelse(fil_1_tri<=15, 1, 0),
         DUMMYNOTA_BIO=ifelse((bio_1_tri+bio_2_tri+bio_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1BIO=ifelse(bio_1_tri<=15, 1, 0),
         DUMMYNOTA_QUI=ifelse((qui_1_tri+qui_2_tri+qui_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1QUI=ifelse(qui_1_tri<=15, 1, 0),
         DUMMYNOTA_FIS=ifelse((fis_1_tri+fis_2_tri+fis_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1FIS=ifelse(fis_1_tri<=15, 1, 0),
         DUMMYNOTA_ART=ifelse((art_1_tri+art_2_tri+art_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1ART=ifelse(art_1_tri<=15, 1, 0),
         DUMMYNOTA_EDFIS=ifelse((ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1EDFIS=ifelse(ed_fis_1_tri<=15, 1, 0),
         DUMMYNOTA_EM_TOTAL=ifelse((lp_1_tri+lp_2_tri+lp_3_tri+ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri+mat_1_tri+mat_2_tri+mat_3_tri+
                                      geo_1_tri+geo_2_tri+geo_3_tri+hist_1_tri+hist_2_tri+hist_3_tri+soc_1_tri+soc_2_tri+soc_3_tri+
                                      fil_1_tri+fil_2_tri+fil_3_tri+bio_1_tri+bio_2_tri+bio_3_tri+qui_1_tri+qui_2_tri+qui_3_tri+
                                      fis_1_tri+fis_2_tri+fis_3_tri+art_1_tri+art_2_tri+art_3_tri)<=550, 1, 0),
         DUMMYNOTA_EM_TRI1TOTAL=ifelse((lp_1_tri+mat_1_tri+geo_1_tri+hist_1_tri+soc_1_tri+fil_1_tri+bio_1_tri+qui_1_tri+fis_1_tri+
                                          art_1_tri+ed_fis_1_tri)<=165, 1, 0),
         DUMMYFALTA_PT=ifelse((lp_falta_1_tri+lp_falta_2_tri+lp_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1PT=ifelse(lp_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_MT=ifelse((mat_falta_1_tri+mat_falta_2_tri+mat_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1MT=ifelse(mat_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_GEO=ifelse((geo_falta_1_tri+geo_falta_2_tri+geo_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1GEO=ifelse(geo_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_HIS=ifelse((hist_falta_1_tri+hist_falta_2_tri+hist_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1HIS=ifelse(hist_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_SOC=ifelse((soc_falta_1_tri+soc_falta_2_tri+soc_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1SOC=ifelse(soc_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_FIL=ifelse((fil_falta_1_tri+fil_falta_2_tri+fil_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1FIL=ifelse(fil_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_BIO=ifelse((bio_falta_1_tri+bio_falta_2_tri+bio_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1BIO=ifelse(bio_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_QUI=ifelse((qui_falta_1_tri+qui_falta_2_tri+qui_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1QUI=ifelse(qui_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_FIS=ifelse((fis_falta_1_tri+fis_falta_2_tri+fis_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1FIS=ifelse(fis_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_ART=ifelse((art_falta_1_tri+art_falta_2_tri+art_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1ART=ifelse(art_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_EDFIS=ifelse((ed_fis_falta_1_tri+ed_fis_falta_2_tri+ed_fis_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1EDFIS=ifelse(ed_fis_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_TOTAL=ifelse(PROP_FALTA_TOTAL>0.25, 1, 0),
         DUMMYFALTA_TRI1TOTAL=ifelse(PROP_FALTA_TRI1TOTAL>0.25, 1, 0),
         DUMMY1NOTA_EM_FINAL=ifelse((DUMMYNOTA_PT+DUMMYNOTA_MT+DUMMYNOTA_GEO+DUMMYNOTA_HIS+DUMMYNOTA_SOC+DUMMYNOTA_FIL+DUMMYNOTA_BIO+
                                       DUMMYNOTA_QUI+DUMMYNOTA_FIS+DUMMYNOTA_ART+DUMMYNOTA_EDFIS)>3, 1, 0),
         DUMMY1NOTA_EM_TRI1FINAL=ifelse((DUMMYNOTA_TRI1PT+DUMMYNOTA_TRI1MT+DUMMYNOTA_TRI1GEO+DUMMYNOTA_TRI1HIS+DUMMYNOTA_TRI1SOC+
                                           DUMMYNOTA_TRI1FIL+DUMMYNOTA_TRI1BIO+DUMMYNOTA_TRI1QUI+DUMMYNOTA_TRI1FIS+DUMMYNOTA_TRI1ART+
                                           DUMMYNOTA_TRI1EDFIS)>3, 1, 0))

dup_EF_seges_2020 <- dup_EF_seges_2020 %>%
  mutate(DUMMYNOTA_PT=ifelse((lp_1_tri+lp_2_tri+lp_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1PT=ifelse(lp_1_tri<=15, 1, 0),
         DUMMYNOTA_MT=ifelse((mat_1_tri+mat_2_tri+mat_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1MT=ifelse(mat_1_tri<=15, 1, 0),
         DUMMYNOTA_GEO=ifelse((geo_1_tri+geo_2_tri+geo_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1GEO=ifelse(geo_1_tri<=15, 1, 0),
         DUMMYNOTA_HIS=ifelse((hist_1_tri+hist_2_tri+hist_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1HIS=ifelse(hist_1_tri<=15, 1, 0),
         DUMMYNOTA_CIEN=ifelse((cien_1_tri+cien_2_tri+cien_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1CIEN=ifelse(cien_1_tri<=15, 1, 0),
         DUMMYNOTA_ART=ifelse((art_1_tri+art_2_tri+art_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1ART=ifelse(art_1_tri<=15, 1, 0),
         DUMMYNOTA_EDFIS=ifelse((ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri)<=50, 1, 0),
         DUMMYNOTA_TRI1EDFIS=ifelse(ed_fis_1_tri<=15, 1, 0),
         DUMMYNOTA_EF_TOTAL=ifelse((lp_1_tri+lp_2_tri+lp_3_tri+ed_fis_1_tri+ed_fis_2_tri+ed_fis_3_tri+mat_1_tri+mat_2_tri+mat_3_tri+
                                      geo_1_tri+geo_2_tri+geo_3_tri+hist_1_tri+hist_2_tri+hist_3_tri+cien_1_tri+cien_2_tri+cien_3_tri+
                                      art_1_tri+art_2_tri+art_3_tri)<=350, 1, 0),
         DUMMYNOTA_EF_TRI1TOTAL=ifelse((lp_1_tri+mat_1_tri+geo_1_tri+hist_1_tri+cien_1_tri+art_1_tri+ed_fis_1_tri)<=105, 1, 0),
         DUMMYFALTA_PT=ifelse((lp_falta_1_tri+lp_falta_2_tri+lp_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1PT=ifelse(lp_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_MT=ifelse((mat_falta_1_tri+mat_falta_2_tri+mat_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1MT=ifelse(mat_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_GEO=ifelse((geo_falta_1_tri+geo_falta_2_tri+geo_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1GEO=ifelse(geo_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_HIS=ifelse((hist_falta_1_tri+hist_falta_2_tri+hist_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1HIS=ifelse(hist_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_CIEN=ifelse((cien_falta_1_tri+cien_falta_2_tri+cien_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1CIEN=ifelse(cien_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_ART=ifelse((art_falta_1_tri+art_falta_2_tri+art_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1ART=ifelse(art_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_EDFIS=ifelse((ed_fis_falta_1_tri+ed_fis_falta_2_tri+ed_fis_falta_3_tri)>0.25, 1, 0),
         DUMMYFALTA_TRI1EDFIS=ifelse(ed_fis_falta_1_tri>0.25, 1, 0),
         DUMMYFALTA_TOTAL=ifelse(PROP_FALTA_TOTAL>0.25, 1, 0),
         DUMMYFALTA_TRI1TOTAL=ifelse(PROP_FALTA_TRI1TOTAL>0.25, 1, 0),
         DUMMY1NOTA_EF_FINAL=ifelse((DUMMYNOTA_PT+DUMMYNOTA_MT+DUMMYNOTA_GEO+DUMMYNOTA_HIS+DUMMYNOTA_CIEN+DUMMYNOTA_ART+
                                       DUMMYNOTA_EDFIS)>3, 1, 0),
         DUMMY1NOTA_EF_TRI1FINAL=ifelse((DUMMYNOTA_TRI1PT+DUMMYNOTA_TRI1MT+DUMMYNOTA_TRI1GEO+DUMMYNOTA_TRI1HIS+DUMMYNOTA_TRI1CIEN+
                                           DUMMYNOTA_TRI1ART+DUMMYNOTA_TRI1EDFIS)>3, 1, 0))



dup_EM_seges_2020$IDADE <- as.numeric(dup_EM_seges_2020$IDADE*365.25)
dup_EF_seges_2020$IDADE <- as.numeric(dup_EF_seges_2020$IDADE*365.25)

dup_EM_seges_2020 <- subset(dup_EM_seges_2020, ID_ETAPA_MATRICULA==16|ID_ETAPA_MATRICULA==17|ID_ETAPA_MATRICULA==18|
                              ID_ETAPA_MATRICULA==351|ID_ETAPA_MATRICULA==352|ID_ETAPA_MATRICULA==353|ID_ETAPA_MATRICULA==354)

dup_EF_seges_2020 <- subset(dup_EF_seges_2020, ID_ETAPA_MATRICULA==150|ID_ETAPA_MATRICULA==151|ID_ETAPA_MATRICULA==152|
                              ID_ETAPA_MATRICULA==153|ID_ETAPA_MATRICULA==154|ID_ETAPA_MATRICULA==155|ID_ETAPA_MATRICULA==156|
                              ID_ETAPA_MATRICULA==157|ID_ETAPA_MATRICULA==158|ID_ETAPA_MATRICULA==159|ID_ETAPA_MATRICULA==160|
                              ID_ETAPA_MATRICULA==138|ID_ETAPA_MATRICULA==139|ID_ETAPA_MATRICULA==140|ID_ETAPA_MATRICULA==141|
                              ID_ETAPA_MATRICULA==142|ID_ETAPA_MATRICULA==143|ID_ETAPA_MATRICULA==144|
                              ID_ETAPA_MATRICULA==145|ID_ETAPA_MATRICULA==146)

describe(dup_EM_seges_2020$IDADE)
describe(dup_EM_seges_2020$NOTA_ESCOLA_TRI1TOTAL)
describe(dup_EM_seges_2020$NOTA_ESCOLA_TOTAL)
describe(dup_EF_seges_2020$NOTA_ESCOLA_TRI1TOTAL)
describe(dup_EF_seges_2020$NOTA_ESCOLA_TOTAL)


saveRDS(dup_EM_seges_2020, 'seges_EM_2016')
saveRDS(dup_EF_seges_2020, 'seges_EF_2016')
rm(list = ls())



