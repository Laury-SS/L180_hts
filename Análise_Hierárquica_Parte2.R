# Filtrando o ano a partir de 2020 -----
disque180_2 <- disque180[disque180$ano >= 2020,]

# Preprando uma tabela de frequencia para mes_ano e UF (com regiao para ordenar posteriormente) -----

# Todas as datas desde o início da serie
datas <- seq(min(disque180_2$Data_de_cadastro), max(disque180_2$Data_de_cadastro), by="day")
## todos os meses e anos
datas_mes_ano <- unique(format(datas, "%Y-%m"))

qtd_datas <- length(datas_mes_ano)

ufs <- unique(disque180_2$UF)

complete_grid <- expand.grid(mes_ano = datas_mes_ano, UF = ufs)

frequency <- disque180_2 %>%
  group_by(mes_ano, UF) %>%
  dplyr::summarise(frequency = n(), .groups = 'drop')

complete_frequency <- complete_grid %>%
  left_join(frequency, by = c("mes_ano", "UF")) %>%
  replace_na(list(frequency = 0)) %>%
  arrange(UF, mes_ano)

#adicionando regiao para ordenar
complete_frequency["regiao"] <- change_values_to_vector_name(
  complete_frequency$UF,
  regioes)

code_regiao <- c("centro-oeste"="co", "nordeste"="ne", "norte"="no", "sudeste"="sd", "sul"="su")
complete_frequency$regiao <- code_regiao[complete_frequency$regiao]

# nomes das series do ultimo nivel da arvore. 4 caracteres: 2 para regiao e 2 para a UF
complete_frequency["ultimo_nivel"] <- paste(complete_frequency$regiao, complete_frequency$UF, sep = "")

# organizar por regiao
complete_frequency <- complete_frequency %>%
  arrange(regiao)

# Previsao com fabletools ----
complete_frequency[,c("mes_ano", "UF", "frequency", "regiao")] %>%
  mutate(data_indice = as.Date(paste0(mes_ano, rep("-01", length(mes_ano))))) %>%
  mutate(m_y = tsibble::yearmonth(data_indice)) %>%
  as_tsibble(index = m_y, key=c(UF, regiao)) %>%
  dplyr::select(UF, regiao, frequency) -> denuncias_hts

# Agrega os dados por região e UF (Unidade Federativa), somando as frequências
denuncias_hts <- denuncias_hts |>
  aggregate_key(regiao / UF, frequency = sum(frequency))


# Modelo Middle Out com previsao por arima teste para 3 meses ----
midout_arima_modelo <- analise_modelo_hts(denuncias_hts, 3, "midout", "arima")

##plot nivel 0 ----
X11()
png(filename = "PrevNivel0_2021.png",width = 400, height = 200,units = "mm",res =200) 
plot_a_partir_de(midout_arima_modelo$plt_n0, 2020)
aumentar_espessura_linha(plot_a_partir_de(midout_arima_modelo$plt_n0, 2020), 1)
dev.off()

##plot nivel 1 ----
X11()
png(filename = "PrevNivel1_2021.png",width = 400, height = 200,units = "mm",res =200)
plot_a_partir_de(midout_arima_modelo$plt_n1, 2020)
aumentar_espessura_linha(plot_a_partir_de(midout_arima_modelo$plt_n1, 2020), 1)
dev.off()
 
##plot nivel 2 ----

# Gerar previsões por região a partir de 2020
metodo_hts_name <- "middle_out"

plt_norte <- plot_previsao_por_regiao_a_partir_de(midout_arima_modelo$previsao, denuncias_hts, "no", metodo_hts_name, 2020)
plt_nordeste <- plot_previsao_por_regiao_a_partir_de(midout_arima_modelo$previsao, denuncias_hts, "ne", metodo_hts_name, 2020)
plt_centro_oeste <- plot_previsao_por_regiao_a_partir_de(midout_arima_modelo$previsao, denuncias_hts, "co", metodo_hts_name, 2020)
plt_sudeste <- plot_previsao_por_regiao_a_partir_de(midout_arima_modelo$previsao, denuncias_hts, "sd", metodo_hts_name, 2020)
plt_sul <- plot_previsao_por_regiao_a_partir_de(midout_arima_modelo$previsao, denuncias_hts, "su", metodo_hts_name, 2020)

# Aumentar a espessura das linhas
X11(); aumentar_espessura_linha(plt_norte, 1)
X11(); aumentar_espessura_linha(plt_nordeste, 1)
X11(); aumentar_espessura_linha(plt_centro_oeste, 1)
X11(); aumentar_espessura_linha(plt_sudeste, 1)
X11(); aumentar_espessura_linha(plt_sul, 1)

#acuracia
print(midout_arima_modelo$acuracia, 
      n = 66)

# Tabela de Desempenho das Abordagens -----

# modelo bottom-up com previsao por arima teste para 3 meses
bu_arima_modelo <- analise_modelo_hts(denuncias_hts, 3, "bu", "arima")
# modelo top-down com previsao por arima teste para 3 meses
td_arima_modelo <- analise_modelo_hts(denuncias_hts, 3, "td", "arima")
# modelo Minimum trace com previsao por arima teste para 3 meses
MinT_arima_modelo <- analise_modelo_hts(denuncias_hts, 3, "MinT", "arima")

## Combina todos os modelos em um único dataframe
combine_modelos <- bind_rows(bu_arima_modelo$acuracia,td_arima_modelo$acuracia, 
                             midout_arima_modelo$acuracia, MinT_arima_modelo$acuracia)

## Primeiras linhas do dataframe combinado
head(combine_modelos)

## Resumo - médias das métricas MASE e RMSSE
resumo <- combine_modelos %>%
  mutate(nivel = case_when(
    is_aggregated(regiao) & is_aggregated(UF) ~ "0 - Brasil (Total)",
    !is_aggregated(regiao) & is_aggregated(UF) ~ "1 - Regional",
    TRUE ~ "2 - Estados"
  ))%>%
  filter(.model != "ARIMA") %>%
  group_by(.model, nivel) %>%
  dplyr::summarise(
    media_mase = mean(MASE, na.rm = TRUE),
    media_rmsse = mean(RMSSE, na.rm = TRUE)
  ) %>%
  arrange(nivel,media_mase, media_rmsse)

## Exibindo o resultado
print(resumo, n=12)


