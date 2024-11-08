rm(list = ls())

library(data.table)
library(magrittr)
library(lubridate)
library(janitor)
library(moments)
library(arrow)
library(dplyr)
library(glue)
library(purrr)
library(zoo)
library(stringr)
library(openxlsx)
library(tidyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(latex2exp)
library(corrr)
library(psych)
library(readxl)
library(gt)
library(tsibble)

source("utils.R")

dados_MD <- excel_sheets("2024-04-0115-23_MedidordeNiveld'Agua.xlsx")  %>%
  keep(~ str_detect(.x, "REMDMN")) %>%
  map(~ read_excel("2024-04-0115-23_MedidordeNiveld'Agua.xlsx", sheet = .))

names_dados_MD <- excel_sheets("2024-04-0115-23_MedidordeNiveld'Agua.xlsx")  %>%
  keep(~ str_detect(.x, "REMDMN"))

dados_MD_estruturados <- dados_MD %>%
  map(estrutura_dados)


estatisticas_MD <- dados_MD_estruturados %>%
  map(calcula_estatisticas)

names(estatisticas_MD) <- names_dados_MD


tabela_estatisticas_MD <- estatisticas_MD %>%
  map(gera_tabela) %>%
  do.call(rbind, .) %>%
  .[, Marco := names_dados_MD] %>%
  relocate(Marco)

# Correlações ------------------------------------------------------------------

# Carrega dados

precipitacao <- readRDS("resplendor_2024-precipitacao.rds")

bombeamento <- readRDS("resplendor_2024-bombeamento.rds")

reservatorios <- readRDS("resplendor_2024-reservatorios.rds")

lista_reservatorios <- list(precipitacao, bombeamento, reservatorios)

## calcula níveis médios mensais dos piezometros -------------------------------

medias_mensais <-
  dados_MD_estruturados %>%
  map(calc_media_mensal)

lista_join_bases <- map(medias_mensais, ~ list(
  inner_join(.x, lista_reservatorios[[1]]),
  inner_join(.x, lista_reservatorios[[2]]),
  left_join(.x, lista_reservatorios[[3]][, 1:3])
))

## Calcula correlações ---------------------------------------------------------

lista_correl <- list()

for (i in 1:length(medias_mensais)) {
  if (sum(!is.na(lista_join_bases[[i]][[1]]$Média)) <= 4) {
    lista_correl[[i]] <- list(NA, NA, NA, NA)
  } else {
    lista_correl[[i]] <- list(
      colpair_map(lista_join_bases[[i]][[1]][,-1], calc_p_value, 0.05)[1,3],
      colpair_map(lista_join_bases[[i]][[2]][,-1], calc_p_value, 0.05)[1,3],
      colpair_map(lista_join_bases[[i]][[3]][,-1], calc_p_value, 0.05)[1,3],
      colpair_map(lista_join_bases[[i]][[3]][,-1], calc_p_value, 0.05)[1,4]
    )
  }
}

tabela_correlacoes <- lista_correl %>%
  unlist() %>%
  matrix(nrow = length(medias_mensais), ncol = 4, byrow = TRUE) %>%
  as.data.frame() %>%
  setDT() %>%
  set_names("Precipitação", "Bombeamento", "Montante", "Jusante") %>%
  .[, Marco := names_dados_MD] %>%
  relocate(Marco)

# Salva tabelas ----------------------------------------------------------------


write.xlsx(tabela_estatisticas_MD, "descritivas_md.xlsx")
write.xlsx(tabela_correlacoes, "correlacoes_md.xlsx")
