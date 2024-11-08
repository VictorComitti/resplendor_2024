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
  map(~ .x %>%
        estrutura_dados() %>%
        mutate(semana = week(data)))

base_niveis_md <- dados_MD_estruturados %>%
  map(gera_base_niveis)

merge_bases <- function(dados_estruturados){

  dados <- dados_estruturados

  semana <- week(seq(ymd(min(dados$data, na.rm = TRUE)),
                     ymd(max(dados$data, na.rm = TRUE)), "weeks")) %>%
    as.data.frame() %>%
    mutate(ano = year(seq(ymd(min(dados$data, na.rm = TRUE)),
                          ymd(max(dados$data, na.rm = TRUE)),
                          "weeks"))) %>%
    mutate(mes = month(seq(ymd(min(dados$data, na.rm = TRUE)),
                           ymd(max(dados$data, na.rm = TRUE)),
                           "weeks"))) %>%
    mutate(Estacao = as.factor(ifelse(mes %in% c(10,11,12,1,2,3),
                                      "Estação Chuvosa", "Estação Seca"))) %>%
    set_names("semana", "ano", "mes", "Estacao") %>%
    mutate(semana_ano = paste0(semana, ano))

  base_niveis <- gera_base_niveis(dados_estruturados)

  merged_df <-left_join(semana,
                        base_niveis,
                        by="semana_ano") %>%
    mutate(data = seq(ymd(min(dados$data, na.rm = TRUE)),
                      ymd(max(dados$data, na.rm = TRUE)), "weeks"))

  return(merged_df)

}

merge_bases(dados_MD_estruturados[[1]])

merged_base <- dados_MD_estruturados %>%
  map(merge_bases)

gera_figura <- function(names, dados_estruturados){

  dados <- dados_estruturados

  merged_df <- merge_bases(dados_estruturados)

  chuvosa <- merged_df %>%
    filter(Estacao == "Estação Chuvosa")

  seca <- merged_df %>%
    filter(Estacao=="Estação Seca")

  Estacao <- merged_df$Estacao

  period <- case_when(Estacao != lead(Estacao) & Estacao == "Estação Seca" ~ "endEstação Seca",
                      Estacao != lead(Estacao) & Estacao == "Estação Chuvosa" ~ "endEstação Chuvosa",
                      Estacao != lag(Estacao) & Estacao == "Estação Seca" ~ "beginEstação Seca",
                      Estacao != lag(Estacao) & Estacao == "Estação Chuvosa" ~ "beginEstação Chuvosa")

  period[1]              <- paste0("begin", as.character(Estacao[1]))
  period[length(period)] <- "endEstação Chuvosa"


  rect <- cbind(merged_df, period)

  rect <- rect %>%
    select(c("data", "Estacao", "period"))

  rect_chuvosa <- na.omit(rect[rect$Estacao == "Estação Chuvosa", ])
  rect_chuvosa <- data.frame(start = rect_chuvosa[rect_chuvosa$period == "beginEstação Chuvosa", 1],
                             end  = rect_chuvosa[rect_chuvosa$period == "endEstação Chuvosa", 1])%>%
    setnames(c("inicio", "fim"))

  ref <- dados$cota_da_boca_m[1]

  title <- names

  plot <-  ggplot(alpha = 0.3) +
    geom_rect(data = rect_chuvosa,aes(xmin =inicio, xmax = fim,
                                      ymin = min(merged_df$nivel_d_agua_m,90, na.rm = TRUE),
                                      ymax = ref + 0.5, fill="Estação Chuvosa"))+
    scale_fill_manual(values = c('cyan'))+
    geom_line(data = merged_df,
              aes(x = data, y = nivel_d_agua_m, col = "Cota do Lençol Freático"), size=.8)+
    geom_hline(aes(yintercept = 90, col = "Cota 90"), size = 1)+
    geom_hline(aes(yintercept = ref, col="Cota do Terreno"), size = 1.4)+
    ggtitle(title)+
    theme(legend.title = element_blank())+
    geom_smooth(data = merged_df, aes(x = data, y = nivel_d_agua_m,  col="Tendência"),
                method ="loess", size = 0.65)+
    ylab("Cota (m)")+
    scale_colour_manual("",
                        breaks = c("Cota do Lençol Freático", "Cota 90",
                                   "Cota do Terreno", "Tendência"),
                        values = c("blue3", "red", "darkgoldenrod", "black"))

  return(plot)
}

figuras_MD <- map2(.x = names_dados_MD,
                .y = dados_MD_estruturados,
                .f = gera_figura)

map2(seq_along(figuras_MD), names_dados_MD, function(i, names) {
  ggsave(
    path = "C:/Users/victo/Desktop/Comitti/resplendor2024/figurasMD",
    filename = paste0(names, ".png"),
    plot = figuras_MD[[i]],
    width = 2000,    # Largura em polegadas
    height = 1000,   # Altura em polegadas
    dpi = 300,             # Resolução em pixels por polegada
    units = "px"
  )
})

