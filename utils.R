clean_data_bombeamento <- function(dados_bombeamento){
  dados_bombeamento %>%
    row_to_names(2) %>%
    select("Data", `Sensor de fluxo(m³/h)`) %>%
    mutate(`Sensor de fluxo(m³/h)` = as.numeric(`Sensor de fluxo(m³/h)`)) %>%
    mutate(Data = dmy(Data))
}

clean_data_precipitacao <- function(dados_precipitacao){
  dados_precipitacao %>%
    row_to_names(2) %>%
    select("Data", `Precipitação (mm)`) %>%
    mutate(`Precipitação (mm)` = as.numeric(`Precipitação (mm)`)) %>%
    mutate(Data = dmy(Data))
}

clean_data_reservatorios <- function(dados_reservatorios){
  dados_reservatorios %>%
    row_to_names(2) %>%
    select("Data", `Leitura Medida(m)`) %>%
    mutate(`Leitura Medida(m)` = as.numeric(`Leitura Medida(m)`)) %>%
    mutate(Data = dmy(Data))
}

calc_p_value <- function(vec_a, vec_b, sig_level){
  test_res <- cor.test(vec_a, vec_b)
  sig <- if_else(test_res$p.value < sig_level, "*", "")
  paste0(round(cor.test(vec_a, vec_b)$estimate, 2), sig)
}

estrutura_dados <- function(aba){
  return(aba %>%
           janitor::row_to_names(2) %>%
           janitor::clean_names() %>%
           select("data", "cota_da_boca_m", "leitura_m",
                  "profundidade_do_furo_m", "nivel_d_agua_m") %>%
           mutate(data = dmy(data),
                  across(-data, as.numeric),
                  Mes = months(data),
                  Ano = year(data),
                  Estacao = as.factor(ifelse(Mes %in% c("outubro", "novembro",
                                                        "dezembro", "janeiro",
                                                        "fevereiro", "março"),
                                             "chuvosa", "seca")),
                  mes_referencia:=as.yearmon(data)
           ))
}

calcula_estatisticas <- function (dados_estruturados){
  dados_estruturados %>%
    group_by(Estacao) %>%
    summarise(Média = round(mean(nivel_d_agua_m, na.rm = TRUE), 2),
              Máximo = round(max(nivel_d_agua_m, na.rm = TRUE), 2),
              Mínimo = round(min(nivel_d_agua_m, na.rm = TRUE), 2),
              Mediana = round(median(nivel_d_agua_m, na.rm = TRUE), 2),
              DP = round(sd(nivel_d_agua_m, na.rm = TRUE), 2))%>%
    setDT()
}

gera_tabela <- function(estatisticas){
  estatisticas %>%
    select(-c("Estacao")) %>%
    t() %>%
    as.list() %>%
    set_names(paste0("V", seq_along(.))) %>%
    flatten_df() %>%
    set_names("Média", "Máximo", "Mínimo", "Mediana", "Desvio Padrão", "Média",
              "Máximo", "Mínimo", "Mediana", "Desvio Padrão") %>%
    setDT()
}

calc_media_mensal <- function(base_estruturada) {
  base_estruturada %>%
    group_by(mes_referencia) %>%
    summarise(Média = mean(nivel_d_agua_m, na.rm=TRUE))%>%
    ungroup() %>%
    setDT()
}

gera_base_niveis <- function(x){
  dados <- x %>%
    mutate(semana = case_when(
      !is.na(lead(semana)) & lead(semana) == semana & semana - lag(semana, default = first(semana)) == 2 ~ semana - 1,
      !is.na(lag(semana)) & lag(semana) == semana & lead(semana, default = last(semana)) - semana == 2 ~ semana + 1,
      TRUE ~ semana
    )) %>%
    mutate(semana_ano = paste0(semana, year(data))) %>%
    select(c("semana_ano", "Estacao", "nivel_d_agua_m")) %>%
    setDT() %>%
    .[, .(nivel_d_agua_m = mean(nivel_d_agua_m)), by = semana_ano]
  return(dados)
}
