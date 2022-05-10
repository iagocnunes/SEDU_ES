library(Hmisc)
library(tidyverse)
library(openxlsx)
library(lubridate)
library(data.table)


options(scipen=999)
memory.limit(size = 999999999999)
memory.limit()
rm(list = ls())

########## ESCOLAS_20`x` ############
censo_2020_1<-read.xlsx('./censo_2020/ESCOLAS_ES_2020_2.xlsx', sheet = 1, colNames = T, skipEmptyRows = T, skipEmptyCols = T)
colnames(censo_2020_1)

ESCOLAS_2020 <- censo_2020_1[, c("CO_ENTIDADE","NO_ENTIDADE","CO_ORGAO_REGIONAL","TP_SITUACAO_FUNCIONAMENTO","TP_FALTANTE",
                                 "DT_ANO_LETIVO_INICIO","DT_ANO_LETIVO_TERMINO","IN_VINCULO_SECRETARIA_EDUCACAO",
                                 "IN_VINCULO_SEGURANCA_PUBLICA","IN_VINCULO_SECRETARIA_SAUDE","IN_VINCULO_OUTRO_ORGAO",
                                 "IN_LOCAL_FUNC_PREDIO_ESCOLAR","TP_OCUPACAO_PREDIO_ESCOLAR","IN_LOCAL_FUNC_SOCIOEDUCATIVO",
                                 "IN_LOCAL_FUNC_UNID_PRISIONAL","IN_LOCAL_FUNC_PRISIONAL_SOCIO","IN_LOCAL_FUNC_GALPAO",
                                 "TP_OCUPACAO_GALPAO","IN_LOCAL_FUNC_SALAS_OUTRA_ESC","IN_LOCAL_FUNC_OUTROS",
                                 "IN_PREDIO_COMPARTILHADO","IN_AGUA_POTAVEL","IN_AGUA_REDE_PUBLICA","IN_AGUA_POCO_ARTESIANO",
                                 "IN_AGUA_CACIMBA","IN_AGUA_FONTE_RIO","IN_AGUA_INEXISTENTE","IN_ENERGIA_REDE_PUBLICA",
                                 "IN_ENERGIA_GERADOR_FOSSIL","IN_ENERGIA_RENOVAVEL","IN_ENERGIA_INEXISTENTE","IN_ESGOTO_REDE_PUBLICA",
                                 "IN_ESGOTO_FOSSA_SEPTICA","IN_ESGOTO_FOSSA_COMUM","IN_ESGOTO_FOSSA","IN_ESGOTO_INEXISTENTE",
                                 "IN_LIXO_SERVICO_COLETA","IN_LIXO_QUEIMA","IN_LIXO_ENTERRA","IN_LIXO_DESTINO_FINAL_PUBLICO",
                                 "IN_LIXO_DESCARTA_OUTRA_AREA","IN_TRATAMENTO_LIXO_SEPARACAO","IN_TRATAMENTO_LIXO_REUTILIZA",
                                 "IN_TRATAMENTO_LIXO_RECICLAGEM","IN_TRATAMENTO_LIXO_INEXISTENTE","IN_ALMOXARIFADO","IN_AREA_VERDE",
                                 "IN_AUDITORIO","IN_BANHEIRO","IN_BANHEIRO_EI","IN_BANHEIRO_PNE","IN_BANHEIRO_FUNCIONARIOS",
                                 "IN_BANHEIRO_CHUVEIRO","IN_BIBLIOTECA","IN_BIBLIOTECA_SALA_LEITURA","IN_COZINHA","IN_DESPENSA",
                                 "IN_DORMITORIO_ALUNO","IN_DORMITORIO_PROFESSOR","IN_LABORATORIO_CIENCIAS","IN_LABORATORIO_INFORMATICA",
                                 "IN_PATIO_COBERTO","IN_PATIO_DESCOBERTO","IN_PARQUE_INFANTIL","IN_PISCINA","IN_QUADRA_ESPORTES",
                                 "IN_QUADRA_ESPORTES_COBERTA","IN_QUADRA_ESPORTES_DESCOBERTA","IN_REFEITORIO","IN_SALA_ATELIE_ARTES",
                                 "IN_SALA_MUSICA_CORAL","IN_SALA_ESTUDIO_DANCA","IN_SALA_MULTIUSO","IN_SALA_DIRETORIA","IN_SALA_LEITURA",
                                 "IN_SALA_PROFESSOR","IN_SALA_REPOUSO_ALUNO","IN_SECRETARIA","IN_SALA_ATENDIMENTO_ESPECIAL","IN_TERREIRAO",
                                 "IN_VIVEIRO","IN_DEPENDENCIAS_OUTRAS","IN_ACESSIBILIDADE_CORRIMAO","IN_ACESSIBILIDADE_ELEVADOR",
                                 "IN_ACESSIBILIDADE_PISOS_TATEIS","IN_ACESSIBILIDADE_VAO_LIVRE","IN_ACESSIBILIDADE_RAMPAS",
                                 "IN_ACESSIBILIDADE_SINAL_SONORO","IN_ACESSIBILIDADE_SINAL_TATIL","IN_ACESSIBILIDADE_SINAL_VISUAL",
                                 "IN_ACESSIBILIDADE_INEXISTENTE","QT_SALAS_UTILIZADAS_DENTRO","QT_SALAS_UTILIZADAS_FORA",
                                 "QT_SALAS_UTILIZADAS","QT_SALAS_UTILIZA_CLIMATIZADAS","QT_SALAS_UTILIZADAS_ACESSIVEIS",
                                 "IN_EQUIP_PARABOLICA","IN_COMPUTADOR","IN_EQUIP_COPIADORA","IN_EQUIP_IMPRESSORA","IN_EQUIP_IMPRESSORA_MULT",
                                 "IN_EQUIP_SCANNER","IN_EQUIP_NENHUM","IN_EQUIP_DVD","QT_EQUIP_DVD","IN_EQUIP_SOM","QT_EQUIP_SOM",
                                 "IN_EQUIP_TV","QT_EQUIP_TV","IN_EQUIP_LOUSA_DIGITAL","QT_EQUIP_LOUSA_DIGITAL","IN_EQUIP_MULTIMIDIA",
                                 "QT_EQUIP_MULTIMIDIA","IN_DESKTOP_ALUNO","QT_DESKTOP_ALUNO","IN_COMP_PORTATIL_ALUNO",
                                 "QT_COMP_PORTATIL_ALUNO","IN_TABLET_ALUNO","QT_TABLET_ALUNO","IN_INTERNET","IN_INTERNET_ALUNOS",
                                 "IN_INTERNET_ADMINISTRATIVO","IN_INTERNET_APRENDIZAGEM","IN_INTERNET_COMUNIDADE","IN_ACESSO_INTERNET_COMPUTADOR",
                                 "IN_ACES_INTERNET_DISP_PESSOAIS","TP_REDE_LOCAL","IN_BANDA_LARGA","QT_PROF_ADMINISTRATIVOS",
                                 "QT_PROF_SERVICOS_GERAIS","QT_PROF_BIBLIOTECARIO","QT_PROF_SAUDE","QT_PROF_COORDENADOR",
                                 "QT_PROF_FONAUDIOLOGO","QT_PROF_NUTRICIONISTA","QT_PROF_PSICOLOGO","QT_PROF_ALIMENTACAO",
                                 "QT_PROF_PEDAGOGIA","QT_PROF_SECRETARIO","QT_PROF_SEGURANCA","QT_PROF_MONITORES","QT_PROF_GESTAO",
                                 "QT_PROF_ASSIST_SOCIAL","IN_ALIMENTACAO","IN_SERIE_ANO","IN_PERIODOS_SEMESTRAIS",
                                 "IN_FUNDAMENTAL_CICLOS","IN_GRUPOS_NAO_SERIADOS","IN_MODULOS","IN_FORMACAO_ALTERNANCIA",
                                 "IN_MATERIAL_PED_MULTIMIDIA","IN_MATERIAL_PED_INFANTIL","IN_MATERIAL_PED_CIENTIFICO",
                                 "IN_MATERIAL_PED_DIFUSAO","IN_MATERIAL_PED_MUSICAL","IN_MATERIAL_PED_JOGOS",
                                 "IN_MATERIAL_PED_ARTISTICAS","IN_MATERIAL_PED_DESPORTIVA","IN_MATERIAL_PED_INDIGENA",
                                 "IN_MATERIAL_PED_ETNICO","IN_MATERIAL_PED_CAMPO","IN_MATERIAL_PED_NENHUM","TP_INDIGENA_LINGUA",
                                 "CO_LINGUA_INDIGENA_1","CO_LINGUA_INDIGENA_2","CO_LINGUA_INDIGENA_3","IN_EXAME_SELECAO",
                                 "IN_RESERVA_PPI","IN_RESERVA_RENDA","IN_RESERVA_PUBLICA","IN_RESERVA_PCD","IN_RESERVA_OUTROS",
                                 "IN_RESERVA_NENHUMA","IN_REDES_SOCIAIS","IN_ESPACO_ATIVIDADE","IN_ESPACO_EQUIPAMENTO",
                                 "IN_ORGAO_ASS_PAIS","IN_ORGAO_ASS_PAIS_MESTRES","IN_ORGAO_CONSELHO_ESCOLAR",
                                 "IN_ORGAO_GREMIO_ESTUDANTIL","IN_ORGAO_OUTROS","IN_ORGAO_NENHUM","TP_PROPOSTA_PEDAGOGICA","TP_AEE",
                                 "TP_ATIVIDADE_COMPLEMENTAR","IN_MEDIACAO_PRESENCIAL","IN_MEDIACAO_SEMIPRESENCIAL",
                                 "IN_MEDIACAO_EAD","IN_COMUM_CRECHE","IN_COMUM_PRE","IN_COMUM_FUND_AI","IN_COMUM_FUND_AF",
                                 "IN_COMUM_MEDIO_MEDIO","IN_COMUM_MEDIO_INTEGRADO","IN_COMUM_MEDIO_NORMAL","IN_ESP_EXCLUSIVA_CRECHE",
                                 "IN_ESP_EXCLUSIVA_PRE","IN_ESP_EXCLUSIVA_FUND_AI","IN_ESP_EXCLUSIVA_FUND_AF",
                                 "IN_ESP_EXCLUSIVA_MEDIO_MEDIO","IN_ESP_EXCLUSIVA_MEDIO_INTEGR","IN_ESP_EXCLUSIVA_MEDIO_NORMAL",
                                 "IN_COMUM_EJA_FUND","IN_COMUM_EJA_MEDIO","IN_COMUM_EJA_PROF","IN_ESP_EXCLUSIVA_EJA_FUND",
                                 "IN_ESP_EXCLUSIVA_EJA_MEDIO","IN_ESP_EXCLUSIVA_EJA_PROF","IN_COMUM_PROF","IN_ESP_EXCLUSIVA_PROF")]

MATRICULA_2020 <-readRDS('MATRICULA_2020')
SITUACAO_2020 <-readRDS('SITUACAO_2020')
seges_EM_2020 <- readRDS('seges_EM_2020')
seges_EF_2020 <- readRDS('seges_EF_2020')
colnames(MATRICULA_2020)
colnames(seges_EM_2020)
colnames(seges_EF_2020)

colnames(SITUACAO_2020) <- paste("SITUACAO", colnames(SITUACAO_2020), sep = "_")
colnames(MATRICULA_2020) <- paste("MATRICULA", colnames(MATRICULA_2020), sep = "_")
colnames(seges_EM_2020) <- paste("SEGES", colnames(seges_EM_2020), sep = "_")
colnames(seges_EF_2020) <- paste("SEGES", colnames(seges_EF_2020), sep = "_")
colnames(ESCOLAS_2020) <- paste("ESCOLAS", colnames(ESCOLAS_2020), sep = "_")

table(SITUACAO_2020$SITUACAO_Nome_ETAPA)

SITUACAO_EF_2020 <- subset(SITUACAO_2020, SITUACAO_ETAPA_ENSINO==4|SITUACAO_ETAPA_ENSINO==5|SITUACAO_ETAPA_ENSINO==6|
                             SITUACAO_ETAPA_ENSINO==7|SITUACAO_ETAPA_ENSINO==8|SITUACAO_ETAPA_ENSINO==9|SITUACAO_ETAPA_ENSINO==10|
                             SITUACAO_ETAPA_ENSINO==11|SITUACAO_ETAPA_ENSINO==14|SITUACAO_ETAPA_ENSINO==15|SITUACAO_ETAPA_ENSINO==16|
                             SITUACAO_ETAPA_ENSINO==17|SITUACAO_ETAPA_ENSINO==18|SITUACAO_ETAPA_ENSINO==19|SITUACAO_ETAPA_ENSINO==20|
                             SITUACAO_ETAPA_ENSINO==21|SITUACAO_ETAPA_ENSINO==41)
SITUACAO_EM_2020 <- subset(SITUACAO_2020, SITUACAO_ETAPA_ENSINO==25|SITUACAO_ETAPA_ENSINO==26|SITUACAO_ETAPA_ENSINO==27|
                             SITUACAO_ETAPA_ENSINO==28|SITUACAO_ETAPA_ENSINO==29|SITUACAO_ETAPA_ENSINO==30|SITUACAO_ETAPA_ENSINO==31|
                             SITUACAO_ETAPA_ENSINO==32|SITUACAO_ETAPA_ENSINO==33|SITUACAO_ETAPA_ENSINO==34|SITUACAO_ETAPA_ENSINO==35|
                             SITUACAO_ETAPA_ENSINO==36|SITUACAO_ETAPA_ENSINO==37|SITUACAO_ETAPA_ENSINO==38)

MATRICULA_EF_2020 <- subset(MATRICULA_2020, MATRICULA_TP_ETAPA_ENSINO==4|MATRICULA_TP_ETAPA_ENSINO==5|MATRICULA_TP_ETAPA_ENSINO==6|
                              MATRICULA_TP_ETAPA_ENSINO==7|MATRICULA_TP_ETAPA_ENSINO==8|MATRICULA_TP_ETAPA_ENSINO==9|
                              MATRICULA_TP_ETAPA_ENSINO==10|MATRICULA_TP_ETAPA_ENSINO==11|MATRICULA_TP_ETAPA_ENSINO==14|
                              MATRICULA_TP_ETAPA_ENSINO==15|MATRICULA_TP_ETAPA_ENSINO==16|MATRICULA_TP_ETAPA_ENSINO==17|
                              MATRICULA_TP_ETAPA_ENSINO==18|MATRICULA_TP_ETAPA_ENSINO==19|MATRICULA_TP_ETAPA_ENSINO==20|
                              MATRICULA_TP_ETAPA_ENSINO==21|MATRICULA_TP_ETAPA_ENSINO==41)
MATRICULA_EM_2020 <- subset(MATRICULA_2020, MATRICULA_TP_ETAPA_ENSINO==25|MATRICULA_TP_ETAPA_ENSINO==26|MATRICULA_TP_ETAPA_ENSINO==27|
                              MATRICULA_TP_ETAPA_ENSINO==28|MATRICULA_TP_ETAPA_ENSINO==29|MATRICULA_TP_ETAPA_ENSINO==30|
                              MATRICULA_TP_ETAPA_ENSINO==31|MATRICULA_TP_ETAPA_ENSINO==32|MATRICULA_TP_ETAPA_ENSINO==33|
                              MATRICULA_TP_ETAPA_ENSINO==34|MATRICULA_TP_ETAPA_ENSINO==35|MATRICULA_TP_ETAPA_ENSINO==36|
                              MATRICULA_TP_ETAPA_ENSINO==37|MATRICULA_TP_ETAPA_ENSINO==38)
rm(MATRICULA_2020, SITUACAO_2020, censo_2020_1)

CENSO_EM_FULL_2020 <- SITUACAO_EM_2020 %>% 
  left_join(MATRICULA_EM_2020, by = c("SITUACAO_CO_ALUNO" = "MATRICULA_CO_PESSOA_FISICA", "SITUACAO_CO_ENTIDADE" = "MATRICULA_CO_ENTIDADE"))
CENSO_EM_FULL_2020 <- CENSO_EM_FULL_2020 %>% 
  left_join(seges_EM_2020, by = c("SITUACAO_CO_ALUNO" = "SEGES_CD_INEP_ALUNO", "SITUACAO_CO_ENTIDADE" = "SEGES_CENSO_ESCOLA_ENTURMACAO"))
CENSO_EM_FULL_2020 <- CENSO_EM_FULL_2020 %>% 
  left_join(ESCOLAS_2020, by = c("SITUACAO_CO_ENTIDADE" = "ESCOLAS_CO_ENTIDADE"))


CENSO_EF_FULL_2020 <- SITUACAO_EF_2020 %>% 
  left_join(MATRICULA_EF_2020, by = c("SITUACAO_CO_ALUNO" = "MATRICULA_CO_PESSOA_FISICA", "SITUACAO_CO_ENTIDADE" = "MATRICULA_CO_ENTIDADE"))
CENSO_EF_FULL_2020 <- CENSO_EF_FULL_2020 %>% 
  left_join(seges_EF_2020, by = c("SITUACAO_CO_ALUNO" = "SEGES_CD_INEP_ALUNO", "SITUACAO_CO_ENTIDADE" = "SEGES_CENSO_ESCOLA_ENTURMACAO"))
CENSO_EF_FULL_2020 <- CENSO_EF_FULL_2020 %>% 
  left_join(ESCOLAS_2020, by = c("SITUACAO_CO_ENTIDADE" = "ESCOLAS_CO_ENTIDADE"))


CENSO_EM_FULL_2020 <- CENSO_EM_FULL_2020 %>%
  mutate(switch_escola=ifelse(SITUACAO_switch_escola==1 | MATRICULA_switch_escola==1 | SEGES_switch_escola==1, 1, 0),
         switch_munic=ifelse(SITUACAO_switch_munic==1 | MATRICULA_switch_munic==1 | SEGES_switch_munic==1, 1, 0),
         switch_turma=ifelse(SEGES_switch_turma==1 | MATRICULA_switch_turma==1, 1, 0),
         IN_TRANSPORTE_ROD=ifelse(MATRICULA_IN_TRANSP_BICICLETA==1, 1,
                                  ifelse(MATRICULA_IN_TRANSP_MICRO_ONIBUS==1, 2,
                                         ifelse(MATRICULA_IN_TRANSP_ONIBUS==1, 3,
                                                ifelse(MATRICULA_IN_TRANSP_TR_ANIMAL==1, 4,
                                                       ifelse(MATRICULA_IN_TRANSP_VANS_KOMBI==1, 5,
                                                              ifelse(MATRICULA_IN_TRANSP_OUTRO_VEICULO==1, 6, 0)))))),
         IN_TRANSPORTE_AQUA=ifelse(MATRICULA_IN_TRANSP_EMBAR_ATE5==1, 1,
                                   ifelse(MATRICULA_IN_TRANSP_EMBAR_5A15==1, 2,
                                          ifelse(MATRICULA_IN_TRANSP_EMBAR_15A35==1, 3,
                                                 ifelse(MATRICULA_IN_TRANSP_EMBAR_35==1, 4, 0)))),
         ESCOLA_IN_VINCULO=ifelse(ESCOLAS_IN_VINCULO_SECRETARIA_EDUCACAO==1, 1,
                                  ifelse(ESCOLAS_IN_VINCULO_SEGURANCA_PUBLICA==1, 2,
                                         ifelse(ESCOLAS_IN_VINCULO_SECRETARIA_SAUDE==1, 3,
                                                ifelse(ESCOLAS_IN_VINCULO_OUTRO_ORGAO==1, 4, 0)))),
         ESCOLA_IN_LOCAL_FUNC=ifelse(ESCOLAS_TP_OCUPACAO_PREDIO_ESCOLAR==1, 11,
                                     ifelse(ESCOLAS_TP_OCUPACAO_PREDIO_ESCOLAR==2, 12,
                                            ifelse(ESCOLAS_TP_OCUPACAO_PREDIO_ESCOLAR==3, 13,
                                                   ifelse(ESCOLAS_IN_LOCAL_FUNC_SOCIOEDUCATIVO==1, 20,
                                                          ifelse(ESCOLAS_IN_LOCAL_FUNC_UNID_PRISIONAL==1, 30,
                                                                 ifelse(ESCOLAS_IN_LOCAL_FUNC_PRISIONAL_SOCIO==1, 40,
                                                                        ifelse(ESCOLAS_TP_OCUPACAO_GALPAO==1, 51,
                                                                               ifelse(ESCOLAS_TP_OCUPACAO_GALPAO==2, 52,
                                                                                      ifelse(ESCOLAS_TP_OCUPACAO_GALPAO==3, 53,
                                                                                             ifelse(ESCOLAS_TP_OCUPACAO_GALPAO==9, 59,
                                                                                                    ifelse(ESCOLAS_IN_LOCAL_FUNC_SALAS_OUTRA_ESC==1, 60,
                                                                                                           ifelse(ESCOLAS_IN_LOCAL_FUNC_OUTROS==1, 70, 0)))))))))))),
         ESCOLA_IN_ABAS_AGUA=ifelse(ESCOLAS_IN_AGUA_REDE_PUBLICA==1, 1,
                                    ifelse(ESCOLAS_IN_AGUA_POCO_ARTESIANO==1, 2,
                                           ifelse(ESCOLAS_IN_AGUA_CACIMBA==1, 3,
                                                  ifelse(ESCOLAS_IN_AGUA_FONTE_RIO==1, 4,
                                                         ifelse(ESCOLAS_IN_AGUA_INEXISTENTE==1, 5, 0))))),
         ESCOLA_IN_ENERGIA=ifelse(ESCOLAS_IN_ENERGIA_REDE_PUBLICA==1, 1,
                                  ifelse(ESCOLAS_IN_ENERGIA_GERADOR_FOSSIL==1, 2,
                                         ifelse(ESCOLAS_IN_ENERGIA_RENOVAVEL==1, 3,
                                                ifelse(ESCOLAS_IN_ENERGIA_INEXISTENTE==1, 4, 0)))),
         ESCOLA_IN_ESGOTO=ifelse(ESCOLAS_IN_ESGOTO_REDE_PUBLICA==1, 1,
                                 ifelse(ESCOLAS_IN_ESGOTO_FOSSA_SEPTICA==1, 2,
                                        ifelse(ESCOLAS_IN_ESGOTO_FOSSA_COMUM==1, 3,
                                               ifelse(ESCOLAS_IN_ESGOTO_FOSSA==1, 4,
                                                      ifelse(ESCOLAS_IN_ESGOTO_INEXISTENTE==1, 5, 0))))),
         ESCOLA_IN_LIXO=ifelse(ESCOLAS_IN_LIXO_SERVICO_COLETA==1, 1,
                               ifelse(ESCOLAS_IN_LIXO_QUEIMA==1, 2,
                                      ifelse(ESCOLAS_IN_LIXO_ENTERRA==1, 3,
                                             ifelse(ESCOLAS_IN_LIXO_DESTINO_FINAL_PUBLICO==1, 4,
                                                    ifelse(ESCOLAS_IN_LIXO_DESCARTA_OUTRA_AREA==1, 5, 0))))),
         ESCOLA_IN_TRATAMENTO_LIXO=ifelse(ESCOLAS_IN_TRATAMENTO_LIXO_SEPARACAO==1, 1,
                                          ifelse(ESCOLAS_IN_TRATAMENTO_LIXO_REUTILIZA==1, 2,
                                                 ifelse(ESCOLAS_IN_TRATAMENTO_LIXO_RECICLAGEM==1, 3,
                                                        ifelse(ESCOLAS_IN_TRATAMENTO_LIXO_INEXISTENTE==1, 4, 0)))))

CENSO_EF_FULL_2020 <- CENSO_EF_FULL_2020 %>%
  mutate(switch_escola=ifelse(SITUACAO_switch_escola==1 | MATRICULA_switch_escola==1 | SEGES_switch_escola==1, 1, 0),
         switch_munic=ifelse(SITUACAO_switch_munic==1 | MATRICULA_switch_munic==1 | SEGES_switch_munic==1, 1, 0),
         switch_turma=ifelse(SEGES_switch_turma==1 | MATRICULA_switch_turma==1, 1, 0),
         IN_TRANSPORTE_ROD=ifelse(MATRICULA_IN_TRANSP_BICICLETA==1, 1,
                                  ifelse(MATRICULA_IN_TRANSP_MICRO_ONIBUS==1, 2,
                                         ifelse(MATRICULA_IN_TRANSP_ONIBUS==1, 3,
                                                ifelse(MATRICULA_IN_TRANSP_TR_ANIMAL==1, 4,
                                                       ifelse(MATRICULA_IN_TRANSP_VANS_KOMBI==1, 5,
                                                              ifelse(MATRICULA_IN_TRANSP_OUTRO_VEICULO==1, 6, 0)))))),
         IN_TRANSPORTE_AQUA=ifelse(MATRICULA_IN_TRANSP_EMBAR_ATE5==1, 1,
                                   ifelse(MATRICULA_IN_TRANSP_EMBAR_5A15==1, 2,
                                          ifelse(MATRICULA_IN_TRANSP_EMBAR_15A35==1, 3,
                                                 ifelse(MATRICULA_IN_TRANSP_EMBAR_35==1, 4, 0)))),
         ESCOLA_IN_VINCULO=ifelse(ESCOLAS_IN_VINCULO_SECRETARIA_EDUCACAO==1, 1,
                                  ifelse(ESCOLAS_IN_VINCULO_SEGURANCA_PUBLICA==1, 2,
                                         ifelse(ESCOLAS_IN_VINCULO_SECRETARIA_SAUDE==1, 3,
                                                ifelse(ESCOLAS_IN_VINCULO_OUTRO_ORGAO==1, 4, 0)))),
         ESCOLA_IN_LOCAL_FUNC=ifelse(ESCOLAS_TP_OCUPACAO_PREDIO_ESCOLAR==1, 11,
                                     ifelse(ESCOLAS_TP_OCUPACAO_PREDIO_ESCOLAR==2, 12,
                                            ifelse(ESCOLAS_TP_OCUPACAO_PREDIO_ESCOLAR==3, 13,
                                                   ifelse(ESCOLAS_IN_LOCAL_FUNC_SOCIOEDUCATIVO==1, 20,
                                                          ifelse(ESCOLAS_IN_LOCAL_FUNC_UNID_PRISIONAL==1, 30,
                                                                 ifelse(ESCOLAS_IN_LOCAL_FUNC_PRISIONAL_SOCIO==1, 40,
                                                                        ifelse(ESCOLAS_TP_OCUPACAO_GALPAO==1, 51,
                                                                               ifelse(ESCOLAS_TP_OCUPACAO_GALPAO==2, 52,
                                                                                      ifelse(ESCOLAS_TP_OCUPACAO_GALPAO==3, 53,
                                                                                             ifelse(ESCOLAS_TP_OCUPACAO_GALPAO==9, 59,
                                                                                                    ifelse(ESCOLAS_IN_LOCAL_FUNC_SALAS_OUTRA_ESC==1, 60,
                                                                                                           ifelse(ESCOLAS_IN_LOCAL_FUNC_OUTROS==1, 70, 0)))))))))))),
         ESCOLA_IN_ABAS_AGUA=ifelse(ESCOLAS_IN_AGUA_REDE_PUBLICA==1, 1,
                                    ifelse(ESCOLAS_IN_AGUA_POCO_ARTESIANO==1, 2,
                                           ifelse(ESCOLAS_IN_AGUA_CACIMBA==1, 3,
                                                  ifelse(ESCOLAS_IN_AGUA_FONTE_RIO==1, 4,
                                                         ifelse(ESCOLAS_IN_AGUA_INEXISTENTE==1, 5, 0))))),
         ESCOLA_IN_ENERGIA=ifelse(ESCOLAS_IN_ENERGIA_REDE_PUBLICA==1, 1,
                                  ifelse(ESCOLAS_IN_ENERGIA_GERADOR_FOSSIL==1, 2,
                                         ifelse(ESCOLAS_IN_ENERGIA_RENOVAVEL==1, 3,
                                                ifelse(ESCOLAS_IN_ENERGIA_INEXISTENTE==1, 4, 0)))),
         ESCOLA_IN_ESGOTO=ifelse(ESCOLAS_IN_ESGOTO_REDE_PUBLICA==1, 1,
                                 ifelse(ESCOLAS_IN_ESGOTO_FOSSA_SEPTICA==1, 2,
                                        ifelse(ESCOLAS_IN_ESGOTO_FOSSA_COMUM==1, 3,
                                               ifelse(ESCOLAS_IN_ESGOTO_FOSSA==1, 4,
                                                      ifelse(ESCOLAS_IN_ESGOTO_INEXISTENTE==1, 5, 0))))),
         ESCOLA_IN_LIXO=ifelse(ESCOLAS_IN_LIXO_SERVICO_COLETA==1, 1,
                               ifelse(ESCOLAS_IN_LIXO_QUEIMA==1, 2,
                                      ifelse(ESCOLAS_IN_LIXO_ENTERRA==1, 3,
                                             ifelse(ESCOLAS_IN_LIXO_DESTINO_FINAL_PUBLICO==1, 4,
                                                    ifelse(ESCOLAS_IN_LIXO_DESCARTA_OUTRA_AREA==1, 5, 0))))),
         ESCOLA_IN_TRATAMENTO_LIXO=ifelse(ESCOLAS_IN_TRATAMENTO_LIXO_SEPARACAO==1, 1,
                                          ifelse(ESCOLAS_IN_TRATAMENTO_LIXO_REUTILIZA==1, 2,
                                                 ifelse(ESCOLAS_IN_TRATAMENTO_LIXO_RECICLAGEM==1, 3,
                                                        ifelse(ESCOLAS_IN_TRATAMENTO_LIXO_INEXISTENTE==1, 4, 0)))))


CENSO_EM_FULL_2020$DIS_1EM <- ifelse(CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==30|CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==25|
                                       CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==35,
                                     ifelse(CENSO_EM_FULL_2020$SEGES_IDADE>=6209.25, 1, 0), 0)
CENSO_EM_FULL_2020$DIS_2EM <- ifelse(CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==31|CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==26|
                                       CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==36,
                                     ifelse(CENSO_EM_FULL_2020$SEGES_IDADE>=6574.5, 1, 0), 0)
CENSO_EM_FULL_2020$DIS_3EM <- ifelse(CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==32|CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==27|
                                       CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==37,
                                     ifelse(CENSO_EM_FULL_2020$SEGES_IDADE>=6939.75, 1, 0), 0)
CENSO_EM_FULL_2020$DIS_4EM <- ifelse(CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==33|CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==28|
                                       CENSO_EM_FULL_2020$SITUACAO_ETAPA_ENSINO==38,
                                     ifelse(CENSO_EM_FULL_2020$SEGES_IDADE>=7305, 1, 0), 0)
CENSO_EM_FULL_2020$DIS <- ifelse(grepl(1, CENSO_EM_FULL_2020$DIS_1EM) | grepl(1, CENSO_EM_FULL_2020$DIS_2EM) |
                                   grepl(1, CENSO_EM_FULL_2020$DIS_3EM) | grepl(1, CENSO_EM_FULL_2020$DIS_4EM), 1, 0)

CENSO_EF_FULL_2020$DIS_1EF <- ifelse(CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==4|CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==14,
                                     ifelse(CENSO_EF_FULL_2020$SEGES_IDADE>=2922, 1, 0), 0)
CENSO_EF_FULL_2020$DIS_2EF <- ifelse(CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==5|CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==15,
                                     ifelse(CENSO_EF_FULL_2020$SEGES_IDADE>=3287.25, 1, 0), 0)
CENSO_EF_FULL_2020$DIS_3EF <- ifelse(CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==6|CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==16,
                                     ifelse(CENSO_EF_FULL_2020$SEGES_IDADE>=3652.5, 1, 0), 0)
CENSO_EF_FULL_2020$DIS_4EF <- ifelse(CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==7|CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==17,
                                     ifelse(CENSO_EF_FULL_2020$SEGES_IDADE>=4017.75, 1, 0), 0)
CENSO_EF_FULL_2020$DIS_5EF <- ifelse(CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==8|CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==18,
                                     ifelse(CENSO_EF_FULL_2020$SEGES_IDADE>=4383, 1, 0), 0)
CENSO_EF_FULL_2020$DIS_6EF <- ifelse(CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==9|CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==19,
                                     ifelse(CENSO_EF_FULL_2020$SEGES_IDADE>=4748.25, 1, 0), 0)
CENSO_EF_FULL_2020$DIS_7EF <- ifelse(CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==10|CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==20,
                                     ifelse(CENSO_EF_FULL_2020$SEGES_IDADE>=5113.5, 1, 0), 0)
CENSO_EF_FULL_2020$DIS_8EF <- ifelse(CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==11|CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==21,
                                     ifelse(CENSO_EF_FULL_2020$SEGES_IDADE>=5478.75, 1, 0), 0)
CENSO_EF_FULL_2020$DIS_9EF <- ifelse(CENSO_EF_FULL_2020$SITUACAO_ETAPA_ENSINO==41,
                                     ifelse(CENSO_EF_FULL_2020$SEGES_IDADE>=5844, 1, 0), 0)
CENSO_EF_FULL_2020$DIS <- ifelse(grepl(1, CENSO_EF_FULL_2020$DIS_1EF) | grepl(1, CENSO_EF_FULL_2020$DIS_2EF) |
                                   grepl(1, CENSO_EF_FULL_2020$DIS_3EF) | grepl(1, CENSO_EF_FULL_2020$DIS_4EF) |
                                   grepl(1, CENSO_EF_FULL_2020$DIS_5EF) | grepl(1, CENSO_EF_FULL_2020$DIS_6EF) |
                                   grepl(1, CENSO_EF_FULL_2020$DIS_7EF) | grepl(1, CENSO_EF_FULL_2020$DIS_8EF) |
                                   grepl(1, CENSO_EF_FULL_2020$DIS_9EF), 1, 0)

CENSO_EM_FULL_2020$ESTUDANTE <- 1
CENSO_EF_FULL_2020$ESTUDANTE <- 1

CENSO2_EM_FULL_2020 <- CENSO_EM_FULL_2020 %>%
  group_by(MATRICULA_ID_TURMA) %>% 
  mutate(ESTUD_TOTAL= sum(ESTUDANTE),
         DIS_TOTAL = sum(DIS),
         DIS_TURMA = (DIS_TOTAL/ESTUD_TOTAL)*100) %>% 
  ungroup()
CENSO3_EM_FULL_2020 <- CENSO_EM_FULL_2020 %>%
  group_by(SITUACAO_CO_ENTIDADE, SEGES_ID_TURNO) %>% 
  mutate(ESTUD_TURNO= sum(ESTUDANTE),
         DIS_TOTAL = sum(DIS),
         DIS_TURNO = (DIS_TOTAL/ESTUD_TURNO)*100,
         TURMAS_TURNO = n_distinct(MATRICULA_ID_TURMA),
         ESTUD_P_TURM_TURN = (ESTUD_TURNO/TURMAS_TURNO)) %>% 
  ungroup()
CENSO4_EM_FULL_2020 <- CENSO_EM_FULL_2020 %>%
  group_by(SITUACAO_CO_ENTIDADE) %>% 
  mutate(ESTUD_ESCOLA= sum(ESTUDANTE),
         DIS_TOTAL = sum(DIS),
         DIS_ESCOLA = (DIS_TOTAL/ESTUD_ESCOLA)*100,
         TURMAS_ESCOLA = n_distinct(MATRICULA_ID_TURMA),
         ESTUD_P_TURM_ESCOLA = (ESTUD_ESCOLA/TURMAS_ESCOLA)) %>% 
  ungroup()


CENSO2_EF_FULL_2020 <- CENSO_EF_FULL_2020 %>%
  group_by(MATRICULA_ID_TURMA) %>% 
  mutate(ESTUD_TOTAL= sum(ESTUDANTE),
         DIS_TOTAL = sum(DIS),
         DIS_TURMA = (DIS_TOTAL/ESTUD_TOTAL)*100) %>% 
  ungroup()

CENSO3_EF_FULL_2020 <- CENSO_EF_FULL_2020 %>%
  group_by(SITUACAO_CO_ENTIDADE, SEGES_ID_TURNO) %>% 
  mutate(ESTUD_TURNO= sum(ESTUDANTE),
         DIS_TOTAL = sum(DIS),
         DIS_TURNO = (DIS_TOTAL/ESTUD_TURNO)*100,
         TURMAS_TURNO = n_distinct(MATRICULA_ID_TURMA),
         ESTUD_P_TURM_TURN = (ESTUD_TURNO/TURMAS_TURNO)) %>% 
  ungroup()

CENSO4_EF_FULL_2020 <- CENSO_EF_FULL_2020 %>%
  group_by(SITUACAO_CO_ENTIDADE) %>% 
  mutate(ESTUD_ESCOLA= sum(ESTUDANTE),
         DIS_TOTAL = sum(DIS),
         DIS_ESCOLA = (DIS_TOTAL/ESTUD_ESCOLA)*100,
         TURMAS_ESCOLA = n_distinct(MATRICULA_ID_TURMA),
         ESTUD_P_TURM_ESCOLA = (ESTUD_ESCOLA/TURMAS_ESCOLA)) %>% 
  ungroup()

CENSO2_EM_FULL_2020 <- CENSO2_EM_FULL_2020 %>%
  select(SITUACAO_CO_ALUNO, ESTUD_TOTAL, DIS_TURMA)
CENSO3_EM_FULL_2020 <- CENSO3_EM_FULL_2020 %>%
  select(SITUACAO_CO_ALUNO, ESTUD_P_TURM_TURN, DIS_TURNO)
CENSO4_EM_FULL_2020 <- CENSO4_EM_FULL_2020 %>%
  select(SITUACAO_CO_ALUNO, ESTUD_P_TURM_ESCOLA, DIS_ESCOLA)

CENSO2_EF_FULL_2020 <- CENSO2_EF_FULL_2020 %>%
  select(SITUACAO_CO_ALUNO, ESTUD_TOTAL, DIS_TURMA)
CENSO3_EF_FULL_2020 <- CENSO3_EF_FULL_2020 %>%
  select(SITUACAO_CO_ALUNO, ESTUD_P_TURM_TURN, DIS_TURNO)
CENSO4_EF_FULL_2020 <- CENSO4_EF_FULL_2020 %>%
  select(SITUACAO_CO_ALUNO, ESTUD_P_TURM_ESCOLA, DIS_ESCOLA)

CENSO_EM_FULL_2020 <- CENSO_EM_FULL_2020 %>% 
  left_join(CENSO2_EM_FULL_2020, by = c("SITUACAO_CO_ALUNO" = "SITUACAO_CO_ALUNO"))
CENSO_EM_FULL_2020 <- CENSO_EM_FULL_2020 %>% 
  left_join(CENSO3_EM_FULL_2020, by = c("SITUACAO_CO_ALUNO" = "SITUACAO_CO_ALUNO"))
CENSO_EM_FULL_2020 <- CENSO_EM_FULL_2020 %>% 
  left_join(CENSO4_EM_FULL_2020, by = c("SITUACAO_CO_ALUNO" = "SITUACAO_CO_ALUNO"))

CENSO_EF_FULL_2020 <- CENSO_EF_FULL_2020 %>% 
  left_join(CENSO2_EF_FULL_2020, by = c("SITUACAO_CO_ALUNO" = "SITUACAO_CO_ALUNO"))
CENSO_EF_FULL_2020 <- CENSO_EF_FULL_2020 %>% 
  left_join(CENSO3_EF_FULL_2020, by = c("SITUACAO_CO_ALUNO" = "SITUACAO_CO_ALUNO"))
CENSO_EF_FULL_2020 <- CENSO_EF_FULL_2020 %>% 
  left_join(CENSO4_EF_FULL_2020, by = c("SITUACAO_CO_ALUNO" = "SITUACAO_CO_ALUNO"))


CENSO_EM_FULL_2020 <- arrange(CENSO_EM_FULL_2020, SITUACAO_CO_ALUNO)
CENSO_EF_FULL_2020 <- arrange(CENSO_EF_FULL_2020, SITUACAO_CO_ALUNO)

CENSO_EM_FULL_2020 <- CENSO_EM_FULL_2020[, c("SITUACAO_ANO_CENSO","SITUACAO_DEPENDENCIA","SITUACAO_CO_MUNICIPIO","SITUACAO_MUNICIPIO",
                                             "SITUACAO_CO_ENTIDADE","SITUACAO_ESCOLA","SITUACAO_CO_ALUNO","SEGES_RA","SEGES_NU_CPF",
                                             "SEGES_IDADE","MATRICULA_NU_IDADE_REFERENCIA","MATRICULA_NU_IDADE","MATRICULA_TP_SEXO",
                                             "MATRICULA_TP_COR_RACA","MATRICULA_TP_NACIONALIDADE","MATRICULA_CO_PAIS_ORIGEM",
                                             "MATRICULA_CO_UF_NASC","MATRICULA_CO_MUNICIPIO_NASC","MATRICULA_CO_PAIS_RESIDENCIA",
                                             "MATRICULA_CO_UF_END","MATRICULA_CO_MUNICIPIO_END","MATRICULA_TP_ZONA_RESIDENCIAL",
                                             "MATRICULA_TP_LOCAL_RESID_DIFERENCIADA","SITUACAO_ETAPA_ENSINO","SITUACAO_Nome_ETAPA",
                                             "MATRICULA_ID_TURMA","MATRICULA_TP_ETAPA_ENSINO","SEGES_ID_TURNO","SEGES_ID_NIVEL_MATRICULA",
                                             "SEGES_ID_ETAPA_MATRICULA","SEGES_dt_matric","SEGES_dt_encerr","SITUACAO_SITUACAO",
                                             "SITUACAO_CO_SITUACAO","switch_escola","switch_munic","SEGES_switch_turno",
                                             "switch_turma","SEGES_qt_escolas","SEGES_qt_switch_munic","SEGES_qt_turno",
                                             "SEGES_qt_turma","SEGES_qt_switch_escola","SEGES_qt_munic","SEGES_qt_switch_turno",
                                             "SEGES_qt_switch_turma","MATRICULA_switch_munic_end","MATRICULA_switch_loc_aula",
                                             "MATRICULA_switch_eja","MATRICULA_switch_duracao","MATRICULA_switch_dur_AEEmsm",
                                             "MATRICULA_switch_unif","MATRICULA_switch_cod_reg","MATRICULA_switch_cod_dist",
                                             "MATRICULA_switch_res_zon","MATRICULA_switch_transp",
                                             "MATRICULA_switch_profiss","MATRICULA_switch_dur_atvmsm","MATRICULA_switch_dur_AEEotr",
                                             "MATRICULA_switch_tipo_atend","MATRICULA_switch_cod_meso","MATRICULA_switch_urb_rur",
                                             "MATRICULA_switch_ed_indg","MATRICULA_switch_uf_end","MATRICULA_switch_res_dif",
                                             "MATRICULA_switch_regular","MATRICULA_switch_mediacao","MATRICULA_switch_dur_atvotr",
                                             "MATRICULA_switch_dias_atv","MATRICULA_switch_loc_turm","MATRICULA_switch_cod_micr",
                                             "MATRICULA_switch_loc_dif","SEGES_EMnotas_semNA","SEGES_EMnotas1tri_semNA",
                                             "SEGES_EMnotas2tri_semNA","SEGES_EMnotas3tri_semNA","SEGES_EMfaltas_semNA",
                                             "SEGES_EMfaltas1tri_semNA","SEGES_EMfaltas2tri_semNA","SEGES_EMfaltas3tri_semNA",
                                             "SEGES_EMaulas_semNA","SEGES_EMaulas1tri_semNA","SEGES_EMaulas2tri_semNA",
                                             "SEGES_EMaulas3tri_semNA","MATRICULA_IN_NECESSIDADE_ESPECIAL","MATRICULA_IN_BAIXA_VISAO",
                                             "MATRICULA_IN_CEGUEIRA","MATRICULA_IN_DEF_AUDITIVA","MATRICULA_IN_DEF_FISICA",
                                             "MATRICULA_IN_DEF_INTELECTUAL","MATRICULA_IN_SURDEZ","MATRICULA_IN_SURDOCEGUEIRA",
                                             "MATRICULA_IN_DEF_MULTIPLA","MATRICULA_IN_AUTISMO","MATRICULA_IN_SUPERDOTACAO",
                                             "MATRICULA_IN_RECURSO_LEDOR","MATRICULA_IN_RECURSO_TRANSCRICAO","MATRICULA_IN_RECURSO_INTERPRETE",
                                             "MATRICULA_IN_RECURSO_LIBRAS","MATRICULA_IN_RECURSO_LABIAL","MATRICULA_IN_RECURSO_AMPLIADA_18",
                                             "MATRICULA_IN_RECURSO_AMPLIADA_24","MATRICULA_IN_RECURSO_CD_AUDIO","MATRICULA_IN_RECURSO_PROVA_PORTUGUES",
                                             "MATRICULA_IN_RECURSO_VIDEO_LIBRAS","MATRICULA_IN_RECURSO_BRAILLE","MATRICULA_IN_RECURSO_NENHUM",
                                             "MATRICULA_IN_AEE_LIBRAS","MATRICULA_IN_AEE_LINGUA_PORTUGUESA","MATRICULA_IN_AEE_INFORMATICA_ACESSIVEL",
                                             "MATRICULA_IN_AEE_BRAILLE","MATRICULA_IN_AEE_CAA","MATRICULA_IN_AEE_SOROBAN",
                                             "MATRICULA_IN_AEE_VIDA_AUTONOMA","MATRICULA_IN_AEE_OPTICOS_NAO_OPTICOS",
                                             "MATRICULA_IN_AEE_ENRIQ_CURRICULAR","MATRICULA_IN_AEE_DESEN_COGNITIVO",
                                             "MATRICULA_IN_AEE_MOBILIDADE","MATRICULA_TP_OUTRO_LOCAL_AULA","MATRICULA_IN_TRANSPORTE_PUBLICO",
                                             "MATRICULA_TP_RESPONSAVEL_TRANSPORTE","IN_TRANSPORTE_ROD","IN_TRANSPORTE_AQUA",
                                             "MATRICULA_IN_ESPECIAL_EXCLUSIVA","MATRICULA_IN_REGULAR","MATRICULA_IN_EJA",
                                             "MATRICULA_IN_PROFISSIONALIZANTE","MATRICULA_CO_CURSO_EDUC_PROFISSIONAL",
                                             "MATRICULA_TP_MEDIACAO_DIDATICO_PEDAGO","MATRICULA_NU_DURACAO_TURMA","MATRICULA_NU_DUR_ATIV_COMP_MESMA_REDE",
                                             "MATRICULA_NU_DUR_ATIV_COMP_OUTRAS_REDES","MATRICULA_NU_DUR_AEE_MESMA_REDE",
                                             "MATRICULA_NU_DUR_AEE_OUTRAS_REDES","MATRICULA_NU_DIAS_ATIVIDADE","MATRICULA_TP_UNIFICADA",
                                             "MATRICULA_TP_TIPO_ATENDIMENTO_TURMA","MATRICULA_TP_TIPO_LOCAL_TURMA","MATRICULA_CO_REGIAO",
                                             "MATRICULA_CO_MESORREGIAO","MATRICULA_CO_MICRORREGIAO","MATRICULA_CO_DISTRITO",
                                             "MATRICULA_TP_LOCALIZACAO","MATRICULA_TP_LOCALIZACAO_DIFERENCIADA","MATRICULA_IN_EDUCACAO_INDIGENA",
                                             "SEGES_hist_1_tri","SEGES_hist_2_tri","SEGES_hist_3_tri","SEGES_geo_1_tri","SEGES_geo_2_tri",
                                             "SEGES_geo_3_tri","SEGES_soc_1_tri","SEGES_soc_2_tri","SEGES_soc_3_tri","SEGES_fil_1_tri","SEGES_fil_2_tri",
                                             "SEGES_fil_3_tri","SEGES_lp_1_tri","SEGES_lp_2_tri","SEGES_lp_3_tri","SEGES_mat_1_tri","SEGES_mat_2_tri",
                                             "SEGES_mat_3_tri","SEGES_bio_1_tri","SEGES_bio_2_tri","SEGES_bio_3_tri","SEGES_fis_1_tri",
                                             "SEGES_fis_2_tri","SEGES_fis_3_tri","SEGES_qui_1_tri","SEGES_qui_2_tri","SEGES_qui_3_tri",
                                             "SEGES_art_1_tri","SEGES_art_2_tri","SEGES_art_3_tri","SEGES_ed_fis_1_tri","SEGES_ed_fis_2_tri",
                                             "SEGES_ed_fis_3_tri","SEGES_lp_rec_1_tri","SEGES_lp_rec_2_tri","SEGES_mat_rec_1_tri",
                                             "SEGES_mat_rec_2_tri","SEGES_hist_rec_1_tri","SEGES_hist_rec_2_tri","SEGES_geo_rec_1_tri",
                                             "SEGES_geo_rec_2_tri","SEGES_soc_rec_1_tri","SEGES_soc_rec_2_tri","SEGES_fil_rec_1_tri",
                                             "SEGES_fil_rec_2_tri","SEGES_bio_rec_1_tri","SEGES_bio_rec_2_tri","SEGES_qui_rec_1_tri",
                                             "SEGES_qui_rec_2_tri","SEGES_fis_rec_1_tri","SEGES_fis_rec_2_tri","SEGES_ed_fis_rec_1_tri",
                                             "SEGES_ed_fis_rec_2_tri","SEGES_art_rec_1_tri","SEGES_art_rec_2_tri","SEGES_hist_falta_1_tri",
                                             "SEGES_hist_falta_2_tri","SEGES_hist_falta_3_tri","SEGES_geo_falta_1_tri","SEGES_geo_falta_2_tri",
                                             "SEGES_geo_falta_3_tri","SEGES_soc_falta_1_tri","SEGES_soc_falta_2_tri","SEGES_soc_falta_3_tri",
                                             "SEGES_fil_falta_1_tri","SEGES_fil_falta_2_tri","SEGES_fil_falta_3_tri","SEGES_lp_falta_1_tri",
                                             "SEGES_lp_falta_2_tri","SEGES_lp_falta_3_tri","SEGES_mat_falta_1_tri","SEGES_mat_falta_2_tri",
                                             "SEGES_mat_falta_3_tri","SEGES_bio_falta_1_tri","SEGES_bio_falta_2_tri","SEGES_bio_falta_3_tri",
                                             "SEGES_qui_falta_1_tri","SEGES_qui_falta_2_tri","SEGES_qui_falta_3_tri","SEGES_fis_falta_1_tri",
                                             "SEGES_fis_falta_2_tri","SEGES_fis_falta_3_tri","SEGES_art_falta_1_tri","SEGES_art_falta_2_tri",
                                             "SEGES_art_falta_3_tri","SEGES_ed_fis_falta_1_tri","SEGES_ed_fis_falta_2_tri",
                                             "SEGES_ed_fis_falta_3_tri","SEGES_hist_aula_1_tri","SEGES_hist_aula_2_tri","SEGES_hist_aula_3_tri",
                                             "SEGES_geo_aula_1_tri","SEGES_geo_aula_2_tri","SEGES_geo_aula_3_tri","SEGES_soc_aula_1_tri",
                                             "SEGES_soc_aula_2_tri","SEGES_soc_aula_3_tri","SEGES_fil_aula_1_tri","SEGES_fil_aula_2_tri",
                                             "SEGES_fil_aula_3_tri","SEGES_lp_aula_1_tri","SEGES_lp_aula_2_tri","SEGES_lp_aula_3_tri",
                                             "SEGES_mat_aula_1_tri","SEGES_mat_aula_2_tri","SEGES_mat_aula_3_tri","SEGES_bio_aula_1_tri",
                                             "SEGES_bio_aula_2_tri","SEGES_bio_aula_3_tri","SEGES_qui_aula_1_tri","SEGES_qui_aula_2_tri",
                                             "SEGES_qui_aula_3_tri","SEGES_fis_aula_1_tri","SEGES_fis_aula_2_tri","SEGES_fis_aula_3_tri",
                                             "SEGES_art_aula_1_tri","SEGES_art_aula_2_tri","SEGES_art_aula_3_tri","SEGES_ed_fis_aula_1_tri",
                                             "SEGES_ed_fis_aula_2_tri","SEGES_ed_fis_aula_3_tri","SEGES_NOTA_TURMA_PT","SEGES_NOTA_TURMA_MT",
                                             "SEGES_NOTA_TURMA_HIS","SEGES_NOTA_TURMA_GEO","SEGES_NOTA_TURMA_FIL","SEGES_NOTA_TURMA_SOC",
                                             "SEGES_NOTA_TURMA_BIO","SEGES_NOTA_TURMA_QUI","SEGES_NOTA_TURMA_FIS","SEGES_NOTA_TURMA_ART",
                                             "SEGES_NOTA_TURMA_EDFIS","SEGES_NOTA_TURMA_EM_TOTAL","SEGES_NOTA_TURMA_TRI1PT","SEGES_NOTA_TURMA_TRI1MT",
                                             "SEGES_NOTA_TURMA_TRI1HIS","SEGES_NOTA_TURMA_TRI1GEO","SEGES_NOTA_TURMA_TRI1FIL","SEGES_NOTA_TURMA_TRI1SOC",
                                             "SEGES_NOTA_TURMA_TRI1BIO","SEGES_NOTA_TURMA_TRI1QUI","SEGES_NOTA_TURMA_TRI1FIS","SEGES_NOTA_TURMA_TRI1ART",
                                             "SEGES_NOTA_TURMA_TRI1EDFIS","SEGES_NOTA_TURMA_EM_TRI1TOTAL","SEGES_NOTA_ESCOLA_PT","SEGES_NOTA_ESCOLA_MT",
                                             "SEGES_NOTA_ESCOLA_HIS","SEGES_NOTA_ESCOLA_GEO","SEGES_NOTA_ESCOLA_FIL","SEGES_NOTA_ESCOLA_SOC",
                                             "SEGES_NOTA_ESCOLA_BIO","SEGES_NOTA_ESCOLA_QUI","SEGES_NOTA_ESCOLA_FIS","SEGES_NOTA_ESCOLA_ART",
                                             "SEGES_NOTA_ESCOLA_EDFIS","SEGES_NOTA_ESCOLA_TOTAL","SEGES_NOTA_ESCOLA_TRI1PT","SEGES_NOTA_ESCOLA_TRI1MT",
                                             "SEGES_NOTA_ESCOLA_TRI1HIS","SEGES_NOTA_ESCOLA_TRI1GEO","SEGES_NOTA_ESCOLA_TRI1FIL",
                                             "SEGES_NOTA_ESCOLA_TRI1SOC","SEGES_NOTA_ESCOLA_TRI1BIO","SEGES_NOTA_ESCOLA_TRI1QUI",
                                             "SEGES_NOTA_ESCOLA_TRI1FIS","SEGES_NOTA_ESCOLA_TRI1ART","SEGES_NOTA_ESCOLA_TRI1EDFIS",
                                             "SEGES_NOTA_ESCOLA_TRI1TOTAL","SEGES_PROP_FALTA_PT","SEGES_PROP_FALTA_MT","SEGES_PROP_FALTA_HIS",
                                             "SEGES_PROP_FALTA_GEO","SEGES_PROP_FALTA_FIL","SEGES_PROP_FALTA_SOC","SEGES_PROP_FALTA_BIO",
                                             "SEGES_PROP_FALTA_FIS","SEGES_PROP_FALTA_QUI","SEGES_PROP_FALTA_ART","SEGES_PROP_FALTA_EDFIS",
                                             "SEGES_PROP_FALTA_TOTAL","SEGES_PROP_FALTA_TRI1PT","SEGES_PROP_FALTA_TRI1MT",
                                             "SEGES_PROP_FALTA_TRI1HIS","SEGES_PROP_FALTA_TRI1GEO","SEGES_PROP_FALTA_TRI1FIL",
                                             "SEGES_PROP_FALTA_TRI1SOC","SEGES_PROP_FALTA_TRI1BIO","SEGES_PROP_FALTA_TRI1QUI",
                                             "SEGES_PROP_FALTA_TRI1FIS","SEGES_PROP_FALTA_TRI1ART","SEGES_PROP_FALTA_TRI1EDFIS",
                                             "SEGES_PROP_FALTA_TRI1TOTAL","SEGES_DUMMYNOTA_PT","SEGES_DUMMYNOTA_TRI1PT","SEGES_DUMMYNOTA_MT",
                                             "SEGES_DUMMYNOTA_TRI1MT","SEGES_DUMMYNOTA_HIS","SEGES_DUMMYNOTA_TRI1HIS","SEGES_DUMMYNOTA_GEO",
                                             "SEGES_DUMMYNOTA_TRI1GEO","SEGES_DUMMYNOTA_FIL","SEGES_DUMMYNOTA_TRI1FIL","SEGES_DUMMYNOTA_SOC",
                                             "SEGES_DUMMYNOTA_TRI1SOC","SEGES_DUMMYNOTA_BIO","SEGES_DUMMYNOTA_TRI1BIO","SEGES_DUMMYNOTA_QUI",
                                             "SEGES_DUMMYNOTA_TRI1QUI","SEGES_DUMMYNOTA_FIS","SEGES_DUMMYNOTA_TRI1FIS","SEGES_DUMMYNOTA_ART",
                                             "SEGES_DUMMYNOTA_TRI1ART","SEGES_DUMMYNOTA_EDFIS","SEGES_DUMMYNOTA_TRI1EDFIS",
                                             "SEGES_DUMMYNOTA_EM_TOTAL","SEGES_DUMMYNOTA_EM_TRI1TOTAL","SEGES_DUMMYFALTA_PT",
                                             "SEGES_DUMMYFALTA_TRI1PT","SEGES_DUMMYFALTA_MT","SEGES_DUMMYFALTA_TRI1MT","SEGES_DUMMYFALTA_HIS",
                                             "SEGES_DUMMYFALTA_TRI1HIS","SEGES_DUMMYFALTA_GEO","SEGES_DUMMYFALTA_TRI1GEO",
                                             "SEGES_DUMMYFALTA_FIL","SEGES_DUMMYFALTA_TRI1FIL","SEGES_DUMMYFALTA_SOC","SEGES_DUMMYFALTA_TRI1SOC",
                                             "SEGES_DUMMYFALTA_BIO","SEGES_DUMMYFALTA_TRI1BIO","SEGES_DUMMYFALTA_QUI","SEGES_DUMMYFALTA_TRI1QUI",
                                             "SEGES_DUMMYFALTA_FIS","SEGES_DUMMYFALTA_TRI1FIS","SEGES_DUMMYFALTA_ART","SEGES_DUMMYFALTA_TRI1ART",
                                             "SEGES_DUMMYFALTA_EDFIS","SEGES_DUMMYFALTA_TRI1EDFIS","SEGES_DUMMYFALTA_TOTAL","SEGES_DUMMYFALTA_TRI1TOTAL",
                                             "SEGES_DUMMY1NOTA_EM_FINAL","SEGES_DUMMY1NOTA_EM_TRI1FINAL","ESCOLAS_CO_ORGAO_REGIONAL",
                                             "ESCOLAS_NO_ENTIDADE","ESCOLAS_TP_SITUACAO_FUNCIONAMENTO","ESCOLAS_DT_ANO_LETIVO_INICIO",
                                             "ESCOLAS_DT_ANO_LETIVO_TERMINO","ESCOLA_IN_VINCULO","ESCOLA_IN_LOCAL_FUNC","ESCOLAS_IN_PREDIO_COMPARTILHADO",
                                             "ESCOLAS_IN_AGUA_POTAVEL","ESCOLA_IN_ABAS_AGUA","ESCOLA_IN_ENERGIA","ESCOLA_IN_ESGOTO",
                                             "ESCOLA_IN_LIXO","ESCOLA_IN_TRATAMENTO_LIXO","ESCOLAS_IN_ALMOXARIFADO","ESCOLAS_IN_AREA_VERDE",
                                             "ESCOLAS_IN_AUDITORIO","ESCOLAS_IN_BANHEIRO","ESCOLAS_IN_BANHEIRO_EI","ESCOLAS_IN_BANHEIRO_PNE",
                                             "ESCOLAS_IN_BANHEIRO_FUNCIONARIOS","ESCOLAS_IN_BANHEIRO_CHUVEIRO","ESCOLAS_IN_BIBLIOTECA",
                                             "ESCOLAS_IN_BIBLIOTECA_SALA_LEITURA","ESCOLAS_IN_COZINHA","ESCOLAS_IN_DESPENSA",
                                             "ESCOLAS_IN_DORMITORIO_ALUNO","ESCOLAS_IN_DORMITORIO_PROFESSOR","ESCOLAS_IN_LABORATORIO_CIENCIAS",
                                             "ESCOLAS_IN_LABORATORIO_INFORMATICA","ESCOLAS_IN_PATIO_COBERTO","ESCOLAS_IN_PATIO_DESCOBERTO",
                                             "ESCOLAS_IN_PARQUE_INFANTIL","ESCOLAS_IN_PISCINA","ESCOLAS_IN_QUADRA_ESPORTES",
                                             "ESCOLAS_IN_QUADRA_ESPORTES_COBERTA","ESCOLAS_IN_QUADRA_ESPORTES_DESCOBERTA","ESCOLAS_IN_REFEITORIO",
                                             "ESCOLAS_IN_SALA_ATELIE_ARTES","ESCOLAS_IN_SALA_MUSICA_CORAL","ESCOLAS_IN_SALA_ESTUDIO_DANCA",
                                             "ESCOLAS_IN_SALA_MULTIUSO","ESCOLAS_IN_SALA_DIRETORIA","ESCOLAS_IN_SALA_LEITURA",
                                             "ESCOLAS_IN_SALA_PROFESSOR","ESCOLAS_IN_SALA_REPOUSO_ALUNO","ESCOLAS_IN_SECRETARIA",
                                             "ESCOLAS_IN_SALA_ATENDIMENTO_ESPECIAL","ESCOLAS_IN_TERREIRAO","ESCOLAS_IN_VIVEIRO",
                                             "ESCOLAS_IN_DEPENDENCIAS_OUTRAS","ESCOLAS_IN_ACESSIBILIDADE_CORRIMAO","ESCOLAS_IN_ACESSIBILIDADE_ELEVADOR",
                                             "ESCOLAS_IN_ACESSIBILIDADE_PISOS_TATEIS","ESCOLAS_IN_ACESSIBILIDADE_VAO_LIVRE",
                                             "ESCOLAS_IN_ACESSIBILIDADE_RAMPAS","ESCOLAS_IN_ACESSIBILIDADE_SINAL_SONORO",
                                             "ESCOLAS_IN_ACESSIBILIDADE_SINAL_TATIL","ESCOLAS_IN_ACESSIBILIDADE_SINAL_VISUAL",
                                             "ESCOLAS_IN_ACESSIBILIDADE_INEXISTENTE","ESCOLAS_QT_SALAS_UTILIZADAS_DENTRO","ESCOLAS_QT_SALAS_UTILIZADAS_FORA",
                                             "ESCOLAS_QT_SALAS_UTILIZADAS","ESCOLAS_QT_SALAS_UTILIZA_CLIMATIZADAS","ESCOLAS_QT_SALAS_UTILIZADAS_ACESSIVEIS",
                                             "ESCOLAS_IN_EQUIP_PARABOLICA","ESCOLAS_IN_COMPUTADOR","ESCOLAS_IN_EQUIP_COPIADORA",
                                             "ESCOLAS_IN_EQUIP_IMPRESSORA","ESCOLAS_IN_EQUIP_IMPRESSORA_MULT","ESCOLAS_IN_EQUIP_SCANNER",
                                             "ESCOLAS_IN_EQUIP_NENHUM","ESCOLAS_IN_EQUIP_DVD","ESCOLAS_QT_EQUIP_DVD","ESCOLAS_IN_EQUIP_SOM",
                                             "ESCOLAS_QT_EQUIP_SOM","ESCOLAS_IN_EQUIP_TV","ESCOLAS_QT_EQUIP_TV","ESCOLAS_IN_EQUIP_LOUSA_DIGITAL",
                                             "ESCOLAS_QT_EQUIP_LOUSA_DIGITAL","ESCOLAS_IN_EQUIP_MULTIMIDIA","ESCOLAS_QT_EQUIP_MULTIMIDIA",
                                             "ESCOLAS_IN_DESKTOP_ALUNO","ESCOLAS_QT_DESKTOP_ALUNO","ESCOLAS_IN_COMP_PORTATIL_ALUNO",
                                             "ESCOLAS_QT_COMP_PORTATIL_ALUNO","ESCOLAS_IN_TABLET_ALUNO","ESCOLAS_QT_TABLET_ALUNO",
                                             "ESCOLAS_IN_INTERNET","ESCOLAS_IN_INTERNET_ALUNOS","ESCOLAS_IN_INTERNET_ADMINISTRATIVO",
                                             "ESCOLAS_IN_INTERNET_APRENDIZAGEM","ESCOLAS_IN_INTERNET_COMUNIDADE","ESCOLAS_IN_ACESSO_INTERNET_COMPUTADOR",
                                             "ESCOLAS_IN_ACES_INTERNET_DISP_PESSOAIS","ESCOLAS_TP_REDE_LOCAL","ESCOLAS_IN_BANDA_LARGA",
                                             "ESCOLAS_QT_PROF_ADMINISTRATIVOS","ESCOLAS_QT_PROF_SERVICOS_GERAIS","ESCOLAS_QT_PROF_BIBLIOTECARIO",
                                             "ESCOLAS_QT_PROF_SAUDE","ESCOLAS_QT_PROF_COORDENADOR","ESCOLAS_QT_PROF_FONAUDIOLOGO",
                                             "ESCOLAS_QT_PROF_NUTRICIONISTA","ESCOLAS_QT_PROF_PSICOLOGO","ESCOLAS_QT_PROF_ALIMENTACAO",
                                             "ESCOLAS_QT_PROF_PEDAGOGIA","ESCOLAS_QT_PROF_SECRETARIO","ESCOLAS_QT_PROF_SEGURANCA",
                                             "ESCOLAS_QT_PROF_MONITORES","ESCOLAS_QT_PROF_GESTAO","ESCOLAS_QT_PROF_ASSIST_SOCIAL",
                                             "ESCOLAS_IN_ALIMENTACAO","ESCOLAS_IN_SERIE_ANO","ESCOLAS_IN_PERIODOS_SEMESTRAIS","ESCOLAS_IN_FUNDAMENTAL_CICLOS",
                                             "ESCOLAS_IN_GRUPOS_NAO_SERIADOS","ESCOLAS_IN_MODULOS","ESCOLAS_IN_FORMACAO_ALTERNANCIA",
                                             "ESCOLAS_IN_MATERIAL_PED_MULTIMIDIA","ESCOLAS_IN_MATERIAL_PED_INFANTIL","ESCOLAS_IN_MATERIAL_PED_CIENTIFICO",
                                             "ESCOLAS_IN_MATERIAL_PED_DIFUSAO","ESCOLAS_IN_MATERIAL_PED_MUSICAL","ESCOLAS_IN_MATERIAL_PED_JOGOS",
                                             "ESCOLAS_IN_MATERIAL_PED_ARTISTICAS","ESCOLAS_IN_MATERIAL_PED_DESPORTIVA","ESCOLAS_IN_MATERIAL_PED_INDIGENA",
                                             "ESCOLAS_IN_MATERIAL_PED_ETNICO","ESCOLAS_IN_MATERIAL_PED_CAMPO","ESCOLAS_IN_MATERIAL_PED_NENHUM",
                                             "ESCOLAS_TP_INDIGENA_LINGUA","ESCOLAS_CO_LINGUA_INDIGENA_1","ESCOLAS_CO_LINGUA_INDIGENA_2",
                                             "ESCOLAS_CO_LINGUA_INDIGENA_3","ESCOLAS_IN_EXAME_SELECAO","ESCOLAS_IN_RESERVA_PPI","ESCOLAS_IN_RESERVA_RENDA",
                                             "ESCOLAS_IN_RESERVA_PUBLICA","ESCOLAS_IN_RESERVA_PCD","ESCOLAS_IN_RESERVA_OUTROS","ESCOLAS_IN_RESERVA_NENHUMA",
                                             "ESCOLAS_IN_REDES_SOCIAIS","ESCOLAS_IN_ESPACO_ATIVIDADE","ESCOLAS_IN_ESPACO_EQUIPAMENTO",
                                             "ESCOLAS_IN_ORGAO_ASS_PAIS","ESCOLAS_IN_ORGAO_ASS_PAIS_MESTRES","ESCOLAS_IN_ORGAO_CONSELHO_ESCOLAR",
                                             "ESCOLAS_IN_ORGAO_GREMIO_ESTUDANTIL","ESCOLAS_IN_ORGAO_OUTROS","ESCOLAS_IN_ORGAO_NENHUM",
                                             "ESCOLAS_TP_PROPOSTA_PEDAGOGICA","ESCOLAS_TP_AEE","ESCOLAS_TP_ATIVIDADE_COMPLEMENTAR",
                                             "ESCOLAS_IN_MEDIACAO_PRESENCIAL","ESCOLAS_IN_MEDIACAO_SEMIPRESENCIAL","ESCOLAS_IN_MEDIACAO_EAD",
                                             "ESCOLAS_IN_COMUM_CRECHE","ESCOLAS_IN_COMUM_PRE","ESCOLAS_IN_COMUM_FUND_AI","ESCOLAS_IN_COMUM_FUND_AF",
                                             "ESCOLAS_IN_COMUM_MEDIO_MEDIO","ESCOLAS_IN_COMUM_MEDIO_INTEGRADO","ESCOLAS_IN_COMUM_MEDIO_NORMAL",
                                             "ESCOLAS_IN_ESP_EXCLUSIVA_CRECHE","ESCOLAS_IN_ESP_EXCLUSIVA_PRE","ESCOLAS_IN_ESP_EXCLUSIVA_FUND_AI",
                                             "ESCOLAS_IN_ESP_EXCLUSIVA_FUND_AF","ESCOLAS_IN_ESP_EXCLUSIVA_MEDIO_MEDIO","ESCOLAS_IN_ESP_EXCLUSIVA_MEDIO_INTEGR",
                                             "ESCOLAS_IN_ESP_EXCLUSIVA_MEDIO_NORMAL","ESCOLAS_IN_COMUM_EJA_FUND","ESCOLAS_IN_COMUM_EJA_MEDIO",
                                             "ESCOLAS_IN_COMUM_EJA_PROF","ESCOLAS_IN_ESP_EXCLUSIVA_EJA_FUND","ESCOLAS_IN_ESP_EXCLUSIVA_EJA_MEDIO",
                                             "ESCOLAS_IN_ESP_EXCLUSIVA_EJA_PROF","ESCOLAS_IN_COMUM_PROF","ESCOLAS_IN_ESP_EXCLUSIVA_PROF",
                                             "DIS","DIS_TURMA","DIS_TURNO","DIS_ESCOLA","ESTUD_TOTAL","ESTUD_P_TURM_TURN","ESTUD_P_TURM_ESCOLA")]


CENSO_EF_FULL_2020 <- CENSO_EF_FULL_2020[, c("SITUACAO_ANO_CENSO","SITUACAO_DEPENDENCIA","SITUACAO_CO_MUNICIPIO","SITUACAO_MUNICIPIO",
                                             "SITUACAO_CO_ENTIDADE","SITUACAO_ESCOLA","SITUACAO_CO_ALUNO","SEGES_RA","SEGES_NU_CPF",
                                             "SEGES_IDADE","MATRICULA_NU_IDADE_REFERENCIA","MATRICULA_NU_IDADE","MATRICULA_TP_SEXO",
                                             "MATRICULA_TP_COR_RACA","MATRICULA_TP_NACIONALIDADE","MATRICULA_CO_PAIS_ORIGEM",
                                             "MATRICULA_CO_UF_NASC","MATRICULA_CO_MUNICIPIO_NASC","MATRICULA_CO_PAIS_RESIDENCIA",
                                             "MATRICULA_CO_UF_END","MATRICULA_CO_MUNICIPIO_END","MATRICULA_TP_ZONA_RESIDENCIAL",
                                             "MATRICULA_TP_LOCAL_RESID_DIFERENCIADA","SITUACAO_ETAPA_ENSINO","SITUACAO_Nome_ETAPA",
                                             "MATRICULA_ID_TURMA","MATRICULA_TP_ETAPA_ENSINO","SEGES_ID_TURNO","SEGES_ID_NIVEL_MATRICULA",
                                             "SEGES_ID_ETAPA_MATRICULA","SEGES_dt_matric","SEGES_dt_encerr","SITUACAO_SITUACAO",
                                             "SITUACAO_CO_SITUACAO","switch_escola","switch_munic","SEGES_switch_turno",
                                             "switch_turma","SEGES_qt_escolas","SEGES_qt_switch_munic","SEGES_qt_turno",
                                             "SEGES_qt_turma","SEGES_qt_switch_escola","SEGES_qt_munic","SEGES_qt_switch_turno",
                                             "SEGES_qt_switch_turma","MATRICULA_switch_munic_end","MATRICULA_switch_loc_aula",
                                             "MATRICULA_switch_eja","MATRICULA_switch_duracao","MATRICULA_switch_dur_AEEmsm",
                                             "MATRICULA_switch_unif","MATRICULA_switch_cod_reg","MATRICULA_switch_cod_dist",
                                             "MATRICULA_switch_res_zon","MATRICULA_switch_transp",
                                             "MATRICULA_switch_profiss","MATRICULA_switch_dur_atvmsm","MATRICULA_switch_dur_AEEotr",
                                             "MATRICULA_switch_tipo_atend","MATRICULA_switch_cod_meso","MATRICULA_switch_urb_rur",
                                             "MATRICULA_switch_ed_indg","MATRICULA_switch_uf_end","MATRICULA_switch_res_dif",
                                             "MATRICULA_switch_regular","MATRICULA_switch_mediacao","MATRICULA_switch_dur_atvotr",
                                             "MATRICULA_switch_dias_atv","MATRICULA_switch_loc_turm","MATRICULA_switch_cod_micr",
                                             "MATRICULA_switch_loc_dif","SEGES_EFnotas_semNA","SEGES_EFnotas1tri_semNA",
                                             "SEGES_EFnotas2tri_semNA","SEGES_EFnotas3tri_semNA","SEGES_EFfaltas_semNA",
                                             "SEGES_EFfaltas1tri_semNA","SEGES_EFfaltas2tri_semNA","SEGES_EFfaltas3tri_semNA",
                                             "SEGES_EFaulas_semNA","SEGES_EFaulas1tri_semNA","SEGES_EFaulas2tri_semNA",
                                             "SEGES_EFaulas3tri_semNA","MATRICULA_IN_NECESSIDADE_ESPECIAL","MATRICULA_IN_BAIXA_VISAO",
                                             "MATRICULA_IN_CEGUEIRA","MATRICULA_IN_DEF_AUDITIVA","MATRICULA_IN_DEF_FISICA",
                                             "MATRICULA_IN_DEF_INTELECTUAL","MATRICULA_IN_SURDEZ","MATRICULA_IN_SURDOCEGUEIRA",
                                             "MATRICULA_IN_DEF_MULTIPLA","MATRICULA_IN_AUTISMO","MATRICULA_IN_SUPERDOTACAO",
                                             "MATRICULA_IN_RECURSO_LEDOR","MATRICULA_IN_RECURSO_TRANSCRICAO","MATRICULA_IN_RECURSO_INTERPRETE",
                                             "MATRICULA_IN_RECURSO_LIBRAS","MATRICULA_IN_RECURSO_LABIAL","MATRICULA_IN_RECURSO_AMPLIADA_18",
                                             "MATRICULA_IN_RECURSO_AMPLIADA_24","MATRICULA_IN_RECURSO_CD_AUDIO","MATRICULA_IN_RECURSO_PROVA_PORTUGUES",
                                             "MATRICULA_IN_RECURSO_VIDEO_LIBRAS","MATRICULA_IN_RECURSO_BRAILLE","MATRICULA_IN_RECURSO_NENHUM",
                                             "MATRICULA_IN_AEE_LIBRAS","MATRICULA_IN_AEE_LINGUA_PORTUGUESA","MATRICULA_IN_AEE_INFORMATICA_ACESSIVEL",
                                             "MATRICULA_IN_AEE_BRAILLE","MATRICULA_IN_AEE_CAA","MATRICULA_IN_AEE_SOROBAN",
                                             "MATRICULA_IN_AEE_VIDA_AUTONOMA","MATRICULA_IN_AEE_OPTICOS_NAO_OPTICOS",
                                             "MATRICULA_IN_AEE_ENRIQ_CURRICULAR","MATRICULA_IN_AEE_DESEN_COGNITIVO",
                                             "MATRICULA_IN_AEE_MOBILIDADE","MATRICULA_TP_OUTRO_LOCAL_AULA","MATRICULA_IN_TRANSPORTE_PUBLICO",
                                             "MATRICULA_TP_RESPONSAVEL_TRANSPORTE","IN_TRANSPORTE_ROD","IN_TRANSPORTE_AQUA",
                                             "MATRICULA_IN_ESPECIAL_EXCLUSIVA","MATRICULA_IN_REGULAR","MATRICULA_IN_EJA",
                                             "MATRICULA_IN_PROFISSIONALIZANTE","MATRICULA_CO_CURSO_EDUC_PROFISSIONAL",
                                             "MATRICULA_TP_MEDIACAO_DIDATICO_PEDAGO","MATRICULA_NU_DURACAO_TURMA","MATRICULA_NU_DUR_ATIV_COMP_MESMA_REDE",
                                             "MATRICULA_NU_DUR_ATIV_COMP_OUTRAS_REDES","MATRICULA_NU_DUR_AEE_MESMA_REDE",
                                             "MATRICULA_NU_DUR_AEE_OUTRAS_REDES","MATRICULA_NU_DIAS_ATIVIDADE","MATRICULA_TP_UNIFICADA",
                                             "MATRICULA_TP_TIPO_ATENDIMENTO_TURMA","MATRICULA_TP_TIPO_LOCAL_TURMA","MATRICULA_CO_REGIAO",
                                             "MATRICULA_CO_MESORREGIAO","MATRICULA_CO_MICRORREGIAO","MATRICULA_CO_DISTRITO",
                                             "MATRICULA_TP_LOCALIZACAO","MATRICULA_TP_LOCALIZACAO_DIFERENCIADA","MATRICULA_IN_EDUCACAO_INDIGENA",
                                             "SEGES_hist_1_tri","SEGES_hist_2_tri","SEGES_hist_3_tri","SEGES_geo_1_tri","SEGES_geo_2_tri",
                                             "SEGES_geo_3_tri","SEGES_lp_1_tri","SEGES_lp_2_tri","SEGES_lp_3_tri","SEGES_mat_1_tri","SEGES_mat_2_tri",
                                             "SEGES_mat_3_tri","SEGES_cien_1_tri","SEGES_cien_2_tri","SEGES_cien_3_tri","SEGES_art_1_tri",
                                             "SEGES_art_2_tri","SEGES_art_3_tri","SEGES_ed_fis_1_tri","SEGES_ed_fis_2_tri",
                                             "SEGES_ed_fis_3_tri","SEGES_lp_rec_1_tri","SEGES_lp_rec_2_tri","SEGES_mat_rec_1_tri",
                                             "SEGES_mat_rec_2_tri","SEGES_hist_rec_1_tri","SEGES_hist_rec_2_tri","SEGES_geo_rec_1_tri",
                                             "SEGES_geo_rec_2_tri","SEGES_cien_rec_1_tri","SEGES_cien_rec_2_tri","SEGES_ed_fis_rec_1_tri",
                                             "SEGES_ed_fis_rec_2_tri","SEGES_art_rec_1_tri","SEGES_art_rec_2_tri","SEGES_hist_falta_1_tri",
                                             "SEGES_hist_falta_2_tri","SEGES_hist_falta_3_tri","SEGES_geo_falta_1_tri","SEGES_geo_falta_2_tri",
                                             "SEGES_geo_falta_3_tri","SEGES_lp_falta_1_tri","SEGES_lp_falta_2_tri","SEGES_lp_falta_3_tri",
                                             "SEGES_mat_falta_1_tri","SEGES_mat_falta_2_tri","SEGES_mat_falta_3_tri","SEGES_cien_falta_1_tri",
                                             "SEGES_cien_falta_2_tri","SEGES_cien_falta_3_tri","SEGES_art_falta_1_tri","SEGES_art_falta_2_tri",
                                             "SEGES_art_falta_3_tri","SEGES_ed_fis_falta_1_tri","SEGES_ed_fis_falta_2_tri",
                                             "SEGES_ed_fis_falta_3_tri","SEGES_hist_aula_1_tri","SEGES_hist_aula_2_tri","SEGES_hist_aula_3_tri",
                                             "SEGES_geo_aula_1_tri","SEGES_geo_aula_2_tri","SEGES_geo_aula_3_tri","SEGES_lp_aula_1_tri",
                                             "SEGES_lp_aula_2_tri","SEGES_lp_aula_3_tri","SEGES_mat_aula_1_tri","SEGES_mat_aula_2_tri",
                                             "SEGES_mat_aula_3_tri","SEGES_cien_aula_1_tri","SEGES_cien_aula_2_tri","SEGES_cien_aula_3_tri",
                                             "SEGES_art_aula_1_tri","SEGES_art_aula_2_tri","SEGES_art_aula_3_tri","SEGES_ed_fis_aula_1_tri",
                                             "SEGES_ed_fis_aula_2_tri","SEGES_ed_fis_aula_3_tri","SEGES_NOTA_TURMA_PT","SEGES_NOTA_TURMA_MT",
                                             "SEGES_NOTA_TURMA_HIS","SEGES_NOTA_TURMA_GEO","SEGES_NOTA_TURMA_CIEN","SEGES_NOTA_TURMA_ART",
                                             "SEGES_NOTA_TURMA_EDFIS","SEGES_NOTA_TURMA_EF_TOTAL","SEGES_NOTA_TURMA_TRI1PT","SEGES_NOTA_TURMA_TRI1MT",
                                             "SEGES_NOTA_TURMA_TRI1HIS","SEGES_NOTA_TURMA_TRI1GEO","SEGES_NOTA_TURMA_TRI1CIEN","SEGES_NOTA_TURMA_TRI1ART",
                                             "SEGES_NOTA_TURMA_TRI1EDFIS","SEGES_NOTA_TURMA_EF_TRI1TOTAL","SEGES_NOTA_ESCOLA_PT","SEGES_NOTA_ESCOLA_MT",
                                             "SEGES_NOTA_ESCOLA_HIS","SEGES_NOTA_ESCOLA_GEO","SEGES_NOTA_ESCOLA_CIEN","SEGES_NOTA_ESCOLA_ART",
                                             "SEGES_NOTA_ESCOLA_EDFIS","SEGES_NOTA_ESCOLA_TOTAL","SEGES_NOTA_ESCOLA_TRI1PT","SEGES_NOTA_ESCOLA_TRI1MT",
                                             "SEGES_NOTA_ESCOLA_TRI1HIS","SEGES_NOTA_ESCOLA_TRI1GEO","SEGES_NOTA_ESCOLA_TRI1CIEN",
                                             "SEGES_NOTA_ESCOLA_TRI1ART","SEGES_NOTA_ESCOLA_TRI1EDFIS",
                                             "SEGES_NOTA_ESCOLA_TRI1TOTAL","SEGES_PROP_FALTA_PT","SEGES_PROP_FALTA_MT","SEGES_PROP_FALTA_HIS",
                                             "SEGES_PROP_FALTA_GEO","SEGES_PROP_FALTA_CIEN","SEGES_PROP_FALTA_ART","SEGES_PROP_FALTA_EDFIS",
                                             "SEGES_PROP_FALTA_TOTAL","SEGES_PROP_FALTA_TRI1PT","SEGES_PROP_FALTA_TRI1MT",
                                             "SEGES_PROP_FALTA_TRI1HIS","SEGES_PROP_FALTA_TRI1GEO","SEGES_PROP_FALTA_TRI1CIEN",
                                             "SEGES_PROP_FALTA_TRI1ART","SEGES_PROP_FALTA_TRI1EDFIS",
                                             "SEGES_PROP_FALTA_TRI1TOTAL","SEGES_DUMMYNOTA_PT","SEGES_DUMMYNOTA_TRI1PT","SEGES_DUMMYNOTA_MT",
                                             "SEGES_DUMMYNOTA_TRI1MT","SEGES_DUMMYNOTA_HIS","SEGES_DUMMYNOTA_TRI1HIS","SEGES_DUMMYNOTA_GEO",
                                             "SEGES_DUMMYNOTA_TRI1GEO","SEGES_DUMMYNOTA_CIEN","SEGES_DUMMYNOTA_TRI1CIEN","SEGES_DUMMYNOTA_ART",
                                             "SEGES_DUMMYNOTA_TRI1ART","SEGES_DUMMYNOTA_EDFIS","SEGES_DUMMYNOTA_TRI1EDFIS",
                                             "SEGES_DUMMYNOTA_EF_TOTAL","SEGES_DUMMYNOTA_EF_TRI1TOTAL","SEGES_DUMMYFALTA_PT",
                                             "SEGES_DUMMYFALTA_TRI1PT","SEGES_DUMMYFALTA_MT","SEGES_DUMMYFALTA_TRI1MT","SEGES_DUMMYFALTA_HIS",
                                             "SEGES_DUMMYFALTA_TRI1HIS","SEGES_DUMMYFALTA_GEO","SEGES_DUMMYFALTA_TRI1GEO",
                                             "SEGES_DUMMYFALTA_CIEN","SEGES_DUMMYFALTA_TRI1CIEN","SEGES_DUMMYFALTA_ART","SEGES_DUMMYFALTA_TRI1ART",
                                             "SEGES_DUMMYFALTA_EDFIS","SEGES_DUMMYFALTA_TRI1EDFIS","SEGES_DUMMYFALTA_TOTAL","SEGES_DUMMYFALTA_TRI1TOTAL",
                                             "SEGES_DUMMY1NOTA_EF_FINAL","SEGES_DUMMY1NOTA_EF_TRI1FINAL","ESCOLAS_CO_ORGAO_REGIONAL",
                                             "ESCOLAS_NO_ENTIDADE","ESCOLAS_TP_SITUACAO_FUNCIONAMENTO","ESCOLAS_DT_ANO_LETIVO_INICIO",
                                             "ESCOLAS_DT_ANO_LETIVO_TERMINO","ESCOLA_IN_VINCULO","ESCOLA_IN_LOCAL_FUNC","ESCOLAS_IN_PREDIO_COMPARTILHADO",
                                             "ESCOLAS_IN_AGUA_POTAVEL","ESCOLA_IN_ABAS_AGUA","ESCOLA_IN_ENERGIA","ESCOLA_IN_ESGOTO",
                                             "ESCOLA_IN_LIXO","ESCOLA_IN_TRATAMENTO_LIXO","ESCOLAS_IN_ALMOXARIFADO","ESCOLAS_IN_AREA_VERDE",
                                             "ESCOLAS_IN_AUDITORIO","ESCOLAS_IN_BANHEIRO","ESCOLAS_IN_BANHEIRO_EI","ESCOLAS_IN_BANHEIRO_PNE",
                                             "ESCOLAS_IN_BANHEIRO_FUNCIONARIOS","ESCOLAS_IN_BANHEIRO_CHUVEIRO","ESCOLAS_IN_BIBLIOTECA",
                                             "ESCOLAS_IN_BIBLIOTECA_SALA_LEITURA","ESCOLAS_IN_COZINHA","ESCOLAS_IN_DESPENSA",
                                             "ESCOLAS_IN_DORMITORIO_ALUNO","ESCOLAS_IN_DORMITORIO_PROFESSOR","ESCOLAS_IN_LABORATORIO_CIENCIAS",
                                             "ESCOLAS_IN_LABORATORIO_INFORMATICA","ESCOLAS_IN_PATIO_COBERTO","ESCOLAS_IN_PATIO_DESCOBERTO",
                                             "ESCOLAS_IN_PARQUE_INFANTIL","ESCOLAS_IN_PISCINA","ESCOLAS_IN_QUADRA_ESPORTES",
                                             "ESCOLAS_IN_QUADRA_ESPORTES_COBERTA","ESCOLAS_IN_QUADRA_ESPORTES_DESCOBERTA","ESCOLAS_IN_REFEITORIO",
                                             "ESCOLAS_IN_SALA_ATELIE_ARTES","ESCOLAS_IN_SALA_MUSICA_CORAL","ESCOLAS_IN_SALA_ESTUDIO_DANCA",
                                             "ESCOLAS_IN_SALA_MULTIUSO","ESCOLAS_IN_SALA_DIRETORIA","ESCOLAS_IN_SALA_LEITURA",
                                             "ESCOLAS_IN_SALA_PROFESSOR","ESCOLAS_IN_SALA_REPOUSO_ALUNO","ESCOLAS_IN_SECRETARIA",
                                             "ESCOLAS_IN_SALA_ATENDIMENTO_ESPECIAL","ESCOLAS_IN_TERREIRAO","ESCOLAS_IN_VIVEIRO",
                                             "ESCOLAS_IN_DEPENDENCIAS_OUTRAS","ESCOLAS_IN_ACESSIBILIDADE_CORRIMAO","ESCOLAS_IN_ACESSIBILIDADE_ELEVADOR",
                                             "ESCOLAS_IN_ACESSIBILIDADE_PISOS_TATEIS","ESCOLAS_IN_ACESSIBILIDADE_VAO_LIVRE",
                                             "ESCOLAS_IN_ACESSIBILIDADE_RAMPAS","ESCOLAS_IN_ACESSIBILIDADE_SINAL_SONORO",
                                             "ESCOLAS_IN_ACESSIBILIDADE_SINAL_TATIL","ESCOLAS_IN_ACESSIBILIDADE_SINAL_VISUAL",
                                             "ESCOLAS_IN_ACESSIBILIDADE_INEXISTENTE","ESCOLAS_QT_SALAS_UTILIZADAS_DENTRO","ESCOLAS_QT_SALAS_UTILIZADAS_FORA",
                                             "ESCOLAS_QT_SALAS_UTILIZADAS","ESCOLAS_QT_SALAS_UTILIZA_CLIMATIZADAS","ESCOLAS_QT_SALAS_UTILIZADAS_ACESSIVEIS",
                                             "ESCOLAS_IN_EQUIP_PARABOLICA","ESCOLAS_IN_COMPUTADOR","ESCOLAS_IN_EQUIP_COPIADORA",
                                             "ESCOLAS_IN_EQUIP_IMPRESSORA","ESCOLAS_IN_EQUIP_IMPRESSORA_MULT","ESCOLAS_IN_EQUIP_SCANNER",
                                             "ESCOLAS_IN_EQUIP_NENHUM","ESCOLAS_IN_EQUIP_DVD","ESCOLAS_QT_EQUIP_DVD","ESCOLAS_IN_EQUIP_SOM",
                                             "ESCOLAS_QT_EQUIP_SOM","ESCOLAS_IN_EQUIP_TV","ESCOLAS_QT_EQUIP_TV","ESCOLAS_IN_EQUIP_LOUSA_DIGITAL",
                                             "ESCOLAS_QT_EQUIP_LOUSA_DIGITAL","ESCOLAS_IN_EQUIP_MULTIMIDIA","ESCOLAS_QT_EQUIP_MULTIMIDIA",
                                             "ESCOLAS_IN_DESKTOP_ALUNO","ESCOLAS_QT_DESKTOP_ALUNO","ESCOLAS_IN_COMP_PORTATIL_ALUNO",
                                             "ESCOLAS_QT_COMP_PORTATIL_ALUNO","ESCOLAS_IN_TABLET_ALUNO","ESCOLAS_QT_TABLET_ALUNO",
                                             "ESCOLAS_IN_INTERNET","ESCOLAS_IN_INTERNET_ALUNOS","ESCOLAS_IN_INTERNET_ADMINISTRATIVO",
                                             "ESCOLAS_IN_INTERNET_APRENDIZAGEM","ESCOLAS_IN_INTERNET_COMUNIDADE","ESCOLAS_IN_ACESSO_INTERNET_COMPUTADOR",
                                             "ESCOLAS_IN_ACES_INTERNET_DISP_PESSOAIS","ESCOLAS_TP_REDE_LOCAL","ESCOLAS_IN_BANDA_LARGA",
                                             "ESCOLAS_QT_PROF_ADMINISTRATIVOS","ESCOLAS_QT_PROF_SERVICOS_GERAIS","ESCOLAS_QT_PROF_BIBLIOTECARIO",
                                             "ESCOLAS_QT_PROF_SAUDE","ESCOLAS_QT_PROF_COORDENADOR","ESCOLAS_QT_PROF_FONAUDIOLOGO",
                                             "ESCOLAS_QT_PROF_NUTRICIONISTA","ESCOLAS_QT_PROF_PSICOLOGO","ESCOLAS_QT_PROF_ALIMENTACAO",
                                             "ESCOLAS_QT_PROF_PEDAGOGIA","ESCOLAS_QT_PROF_SECRETARIO","ESCOLAS_QT_PROF_SEGURANCA",
                                             "ESCOLAS_QT_PROF_MONITORES","ESCOLAS_QT_PROF_GESTAO","ESCOLAS_QT_PROF_ASSIST_SOCIAL",
                                             "ESCOLAS_IN_ALIMENTACAO","ESCOLAS_IN_SERIE_ANO","ESCOLAS_IN_PERIODOS_SEMESTRAIS","ESCOLAS_IN_FUNDAMENTAL_CICLOS",
                                             "ESCOLAS_IN_GRUPOS_NAO_SERIADOS","ESCOLAS_IN_MODULOS","ESCOLAS_IN_FORMACAO_ALTERNANCIA",
                                             "ESCOLAS_IN_MATERIAL_PED_MULTIMIDIA","ESCOLAS_IN_MATERIAL_PED_INFANTIL","ESCOLAS_IN_MATERIAL_PED_CIENTIFICO",
                                             "ESCOLAS_IN_MATERIAL_PED_DIFUSAO","ESCOLAS_IN_MATERIAL_PED_MUSICAL","ESCOLAS_IN_MATERIAL_PED_JOGOS",
                                             "ESCOLAS_IN_MATERIAL_PED_ARTISTICAS","ESCOLAS_IN_MATERIAL_PED_DESPORTIVA","ESCOLAS_IN_MATERIAL_PED_INDIGENA",
                                             "ESCOLAS_IN_MATERIAL_PED_ETNICO","ESCOLAS_IN_MATERIAL_PED_CAMPO","ESCOLAS_IN_MATERIAL_PED_NENHUM",
                                             "ESCOLAS_TP_INDIGENA_LINGUA","ESCOLAS_CO_LINGUA_INDIGENA_1","ESCOLAS_CO_LINGUA_INDIGENA_2",
                                             "ESCOLAS_CO_LINGUA_INDIGENA_3","ESCOLAS_IN_EXAME_SELECAO","ESCOLAS_IN_RESERVA_PPI","ESCOLAS_IN_RESERVA_RENDA",
                                             "ESCOLAS_IN_RESERVA_PUBLICA","ESCOLAS_IN_RESERVA_PCD","ESCOLAS_IN_RESERVA_OUTROS","ESCOLAS_IN_RESERVA_NENHUMA",
                                             "ESCOLAS_IN_REDES_SOCIAIS","ESCOLAS_IN_ESPACO_ATIVIDADE","ESCOLAS_IN_ESPACO_EQUIPAMENTO",
                                             "ESCOLAS_IN_ORGAO_ASS_PAIS","ESCOLAS_IN_ORGAO_ASS_PAIS_MESTRES","ESCOLAS_IN_ORGAO_CONSELHO_ESCOLAR",
                                             "ESCOLAS_IN_ORGAO_GREMIO_ESTUDANTIL","ESCOLAS_IN_ORGAO_OUTROS","ESCOLAS_IN_ORGAO_NENHUM",
                                             "ESCOLAS_TP_PROPOSTA_PEDAGOGICA","ESCOLAS_TP_AEE","ESCOLAS_TP_ATIVIDADE_COMPLEMENTAR",
                                             "ESCOLAS_IN_MEDIACAO_PRESENCIAL","ESCOLAS_IN_MEDIACAO_SEMIPRESENCIAL","ESCOLAS_IN_MEDIACAO_EAD",
                                             "ESCOLAS_IN_COMUM_CRECHE","ESCOLAS_IN_COMUM_PRE","ESCOLAS_IN_COMUM_FUND_AI","ESCOLAS_IN_COMUM_FUND_AF",
                                             "ESCOLAS_IN_COMUM_MEDIO_MEDIO","ESCOLAS_IN_COMUM_MEDIO_INTEGRADO","ESCOLAS_IN_COMUM_MEDIO_NORMAL",
                                             "ESCOLAS_IN_ESP_EXCLUSIVA_CRECHE","ESCOLAS_IN_ESP_EXCLUSIVA_PRE","ESCOLAS_IN_ESP_EXCLUSIVA_FUND_AI",
                                             "ESCOLAS_IN_ESP_EXCLUSIVA_FUND_AF","ESCOLAS_IN_ESP_EXCLUSIVA_MEDIO_MEDIO","ESCOLAS_IN_ESP_EXCLUSIVA_MEDIO_INTEGR",
                                             "ESCOLAS_IN_ESP_EXCLUSIVA_MEDIO_NORMAL","ESCOLAS_IN_COMUM_EJA_FUND","ESCOLAS_IN_COMUM_EJA_MEDIO",
                                             "ESCOLAS_IN_COMUM_EJA_PROF","ESCOLAS_IN_ESP_EXCLUSIVA_EJA_FUND","ESCOLAS_IN_ESP_EXCLUSIVA_EJA_MEDIO",
                                             "ESCOLAS_IN_ESP_EXCLUSIVA_EJA_PROF","ESCOLAS_IN_COMUM_PROF","ESCOLAS_IN_ESP_EXCLUSIVA_PROF",
                                             "DIS","DIS_TURMA","DIS_TURNO","DIS_ESCOLA","ESTUD_TOTAL","ESTUD_P_TURM_TURN","ESTUD_P_TURM_ESCOLA")]

saveRDS(CENSO_EM_FULL_2020, 'sedu_EM_2020')
saveRDS(CENSO_EF_FULL_2020, 'sedu_EF_2020')
rm(list = ls())