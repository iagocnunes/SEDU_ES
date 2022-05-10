library(Hmisc)
library(tidyverse)
library(openxlsx)
library(data.table)
library(hablar)

options(scipen=999)
memory.limit(size = 999999999999)
memory.limit()
rm(list = ls())

sedu_EM_2016 <-readRDS('sedu_EM_2016')
sedu_EM_2016 <-readRDS('sedu_EF_2016')
sedu_EM_2016 <-readRDS('sedu_EM_2017')
sedu_EM_2016 <-readRDS('sedu_EF_2017')
sedu_EM_2016 <-readRDS('sedu_EM_2018')
sedu_EM_2016 <-readRDS('sedu_EF_2018')
sedu_EM_2016 <-readRDS('sedu_EM_2019')
sedu_EM_2016 <-readRDS('sedu_EF_2019')
sedu_EM_2016 <-readRDS('sedu_EM_2020')
sedu_EM_2016 <-readRDS('sedu_EF_2020')

type_EM_2016 <- as.data.frame(map(sedu_EM_2016, ~typeof(.)))
count_EM_2016 <- as.data.frame(map(sedu_EM_2016, ~sum(length(s(.)))))
NAs_EM_2016 <- as.data.frame(map(sedu_EM_2016, ~sum(is.na(.))))
distin_EM_2016 <- as.data.frame(map(sedu_EM_2016, ~n_distinct(., na.rm=T)))

stats_EM_2016 <- rbind(type_EM_2016, count_EM_2016, NAs_EM_2016, distin_EM_2016)
stats_EM_2016 <- setDT(as.data.frame(t(stats_EM_2016)), keep.rownames = TRUE)
stats_EM_2016 <- stats_EM_2016 %>% rename(Tipo='V1', Variáveis='rn',
                                          Quantidade_de_valores='V2',
                                          Quantidade_de_NAs='V3',
                                          Valores_distintos='V4')
write.xlsx(stats_EM_2016,'stats_EF_2020.xlsx')




















