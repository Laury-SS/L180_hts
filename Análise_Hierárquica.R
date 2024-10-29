#install.packages("readxl")
library(readxl)
#install.packages("finalfit")
library(finalfit)
library(ggplot2)
#install.packages("DescTools")
library(DescTools)
#install.packages("gtsummary")
library(gtsummary)
#install.packages("dplyr")
library(dplyr)
library(hrbrthemes)
library(viridis)
library(patchwork)
library(lubridate)
library(geobr)
library(ggnewscale)
#install.packages("gridExtra")
library(gridExtra)
library(grid)
#install.packages("stringr")
library(stringr)
#install.packages("hts")
#install.packages("tidyr")
#carregar o pacote hts
library(hts)
library(tidyr)


# Banco de dados ----
disque180 <- read.csv(file.choose())


# Ajustando Variaveis ----

remove_accents_and_lowercase_column2 <- function(column) {
  # Convert the column to lowercase and remove accents
  result <- gsub("[^[:ascii:]]", "", tolower(column), perl = TRUE)
  
  return(result)
}

remove_accents_and_lowercase_column <- function(column) {
  # Convert the column to lowercase and remove accents
  result <- iconv(column, to = "ASCII//TRANSLIT")
  result <- tolower(result)
  
  return(result)
}

# Coverter p/ data
convert_to_date <- function(date_text) {
  date <- as.Date(date_text, format = "%Y-%m-%d")
  if (any(is.na(date))) {
    date[is.na(date)] <- as.Date(date_text[is.na(date)], format = "%d/%m/%Y")
  }
  return(date)
}

disque180$Data_de_cadastro <- convert_to_date(disque180$Data_de_cadastro)

colunas_sel <- c("Denunciante","Gênero_da_vítima","Raça_Cor_da_vítima",
                 "Faixa_etária_da_vítima","Grau_de_instrução_da_vítima",
                 "Gênero_do_suspeito","Raça_Cor_do_suspeito",
                 "Faixa_etária_do_suspeito","Grau_de_instrução_do_suspeito",
                 "UF","Município")

disque180[, colunas_sel] <- lapply(disque180[, colunas_sel], 
                                   remove_accents_and_lowercase_column)



denunciantes_sel <- list("amigo(a)" = c("amigo (a)", "amigo(a)"),
                         "companheiro(a)" = c("companheiro", "companheiro (a)"),
                         "cunhado(a)" = c("cunhado (a)", "cunhado(a)"),
                         "ex-companheiro(a)" = c("ex companheiro", "ex-companheiro (a)",  "companheira (lesbica)"),
                         "ex-namorado(a)" = c("ex-namorada", "ex-namorada",  "ex namorada (lesbica)", "ex namorado"),
                         "irmao(a)" = c("irma(o)", "irmao (a)"),
                         "namorado(a)" = c("namorada (lesbica)","namorado" ,"namorado(a)"),
                         "vizinho(a)" = c("vizinho (a)", "vizinho(a)"),
                         "propria vitima" = c("vitima" , "propria vitima", "a propria vitima"))

change_values_to_vector_name <- function(column, vectors_list) {
  for (vector_name in names(vectors_list)) {
    matching_indices <- column %in% vectors_list[[vector_name]]
    
    column[matching_indices] <- vector_name
  }
  
  return(column)
}

disque180$Denunciante <- change_values_to_vector_name(disque180$Denunciante,
                                                      denunciantes_sel)



grau_de_instrucao_sel <- list("analfabeto(a)"= c("analfabeta(o)", "analfabeto" , "analfabeto(a)", "analfabeto/sem instrucao"),
                              "ensino fundamental"= c("ensino fundamental",  "ensino fundamental completo"),
                              "ensino medio"= c("ensino medio", "ensino medio completo"),
                              "ensino superior"= c("ensino superior", "ensino superior completo", "superior completo"),
                              "pos-graduacao"= c("pos-graduacao", "pos graduacao"),
                              "ensino superior incompleto"= c("superior incompleto","ensino superior incompleto"))

disque180$Grau_de_instrução_da_vítima <- change_values_to_vector_name(
  disque180$Grau_de_instrução_da_vítima,
  grau_de_instrucao_sel)



raca_cor_sus_sel <- list("indigena" = c("indigena", "indigena "))
disque180$Raça_Cor_do_suspeito <- change_values_to_vector_name(
  disque180$Raça_Cor_do_suspeito,
  raca_cor_sus_sel)



grau_de_instrucao_sus_sel <- list("analfabeto(a)" = c("analfabeta(o)", "analfabeto", "analfabeto(a)"),
                                  "ensino fundamental" = c("ensino fundamental", "ensino fundamental completo"),
                                  "ensino medio" = c("ensino medio", "ensino medio completo"),
                                  "ensino superior" = c("ensino superior", "ensino superior completo", "superior completo"),
                                  "pos-graduacao" = c("pos-graduacao", "pos graduacao"))

disque180$Grau_de_instrução_do_suspeito <- change_values_to_vector_name(
  disque180$Grau_de_instrução_do_suspeito,
  grau_de_instrucao_sus_sel)


Grupo_vulnerável_sel <- list("OUTRAS VIOLÊNCIAS CONTRA A MULHER" = c("01. OUTRAS VIOLÊNCIAS CONTRA A MULHER", "Tráfico de Mulheres",
                                                                     "ESPORTE SEM ASSÉDIO", "Violência contra Diversidade Religiosa",
                                                                     "Trabalho Escravo","VIOLÊNCIA OBSTÉTRICA", "TRÁFICO DE PESSOAS",
                                                                     "Violência Virtual"),
                             "VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER" = c("2-Violência Doméstica e Familiar Contra a Mulher", 
                                                                                  "02. VIOLÊNCIA DOMÉSTICA E FAMILIAR CONTRA A MULHER", 
                                                                                  "Violência Doméstica e Familiar"),
                             "CARCERE PRIVADO" = "Cárcere Privado", 
                             "VIOLENCIA FISICA" = "Violência Física", 
                             "VIOLENCIA SEXUAL" = "Violência Sexual",
                             "VIOLENCIA MORAL" = "Violência Moral", 
                             "VIOLÊNCIA OBSTÉTRICA" = "Violência Obstétrica", 
                             "VIOLÊNCIA CONTRA A MULHER" = "1-Violência Contra a Mulher",
                             "HOMICÍDIO" = "Homicídio")

#Grupo_vulnerável_sel <- tolower(disque180$Grupo_vulnerável_sel)


disque180$Grupo_vulnerável <- change_values_to_vector_name(
  disque180$Grupo_vulnerável,
  Grupo_vulnerável_sel)

disque180$UF <- gsub(" ", "", disque180$UF)


faixa_etaria_sel <- list("0 a 14 anos" = c("05 a 09 anos", "00 a 1 ano", "02 a 04 anos" ," menos de 01 ano", " recem-nascido (ate 28 dias)", " ate 9 anos", " 0 a 3 anos", " 01 ano", " 02 anos", " 03 anos", " 4 a 7 anos", " 04 anos", " 05 anos", "06 anos", " 07 anos", "8 a 11 anos", " 08 anos", " 09 anos", " entre 10 e 14 anos", " 10 a 11 anos", " 10 anos", " 11 anos", " 12 a 14 anos", " 12 anos", " 13 anos", "14 anos"),
                         "15 a 24 anos" = c(" entre 15 e 19 anos", " 15 a 17 anos", " 15 anos", " 16 anos", " 17 anos", " 18 a 24 anos", " 18 a 19 anos", "entre 20 e 24 anos", " 20 a 24 anos"),
                         "25 +" = c("80 anos ou mais", " 25 a 30 anos", "25 a 29 anos", " entre 25 e 29 anos", " 30 a 34 anos", " entre 30 e 34 anos", " 31 a 35 anos", " 35 a 39 anos", " entre 35 e 39 anos", "36 a 40 anos", " entre 40 e 44 anos", " 40 a 44 anos", " 41 a 45 anos", " 45 a 49 anos", " entre 45 e 49 anos", "46 a 50 anos", "50 a 54 anos", "entre 50 e 54 anos", " 51 a 55 anos", " entre 55 e 59 anos", " 55 a 59 anos", " 56 a 60 anos", " entre 60 e 64 anos", " 60 a 64 anos", " 61 a 65 anos", "65 a 69 anos", " entre 65 e 69 anos", "66 a 70 anos", "70 a 74 anos", " maior de 70 anos", " 71 a 75 anos", " 75 a 79 anos", " 76 a 80 anos", " 80 a 84 anos", " 81 a 85 anos", " 85 a 90 anos", "85 a 89 anos", " 90+", " 91 anos ou mais"),
                         "NA" = c("nao informado", " n/d", " null"))

faixa_etaria_sel<- lapply(faixa_etaria_sel, function(v) c(v, lapply(v, trimws)))

disque180$Faixa_etária_da_vítima <- change_values_to_vector_name(
  disque180$Faixa_etária_da_vítima,
  faixa_etaria_sel)

disque180$Faixa_etária_do_suspeito <- change_values_to_vector_name(
  disque180$Faixa_etária_do_suspeito,
  faixa_etaria_sel)


disque180 <- disque180[!is.na(disque180$UF),]


disque180 <- disque180[disque180$Gênero_da_vítima == "feminino",]


disque180 <- disque180 %>%
  mutate(ano = year(Data_de_cadastro),
         mes = month(Data_de_cadastro, label = TRUE))

#disque180 <- disque180[disque180$ano >= 2020,]

disque180 <- disque180 %>%
  mutate(mes_ano = format(Data_de_cadastro, "%Y-%m"))

disque180 <- disque180 %>%
  mutate(ano = year(Data_de_cadastro),
         mes = month(Data_de_cadastro, label = TRUE))

disque180["regiao"] <- trimws(disque180$UF)

regioes <- list(
  "norte" = c("ac", "ap", "am", "pa", "ro", "rr", "to"),
  "nordeste" = c("al", "ba", "ce", "ma", "pb", "pe", "pi", "rn", "se"),
  "centro-oeste" = c("df", "go", "mt", "ms"),
  "sudeste" = c("es", "mg", "rj", "sp"),
  "sul" = c("pr", "rs", "sc"),
  "exterior" = c("no","pt","it","py","us","ar","fr","ep","ve","nl","gb",
                 "ita","che","vgb","eua","esp","out","bol","por","ni",
                 "fra","guf","arg","ury","nor","pry"),
  "na" = c("denunciantenaosoubeinformar", 
           "atendimentointerrompido", "n/d", NA, "null")
)

disque180$regiao <- change_values_to_vector_name(
  disque180$regiao,
  regioes)

disque180 <- disque180[disque180$regiao != "exterior",]
disque180 <- disque180[disque180$regiao != "na",]


# Preprando uma tabela de frequencia para mes_ano e UF (com regiao para ordenar posteriormente) ----

# Todas as datas desde o início da serie
datas <- seq(min(disque180$Data_de_cadastro), max(disque180$Data_de_cadastro), by="day")
## todos os meses e anos
datas_mes_ano <- unique(format(datas, "%Y-%m"))

qtd_datas <- length(datas_mes_ano)

ufs <- unique(disque180$UF)

complete_grid <- expand.grid(mes_ano = datas_mes_ano, UF = ufs)

frequency <- disque180 %>%
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
library("tsibble")
library("fabletools")
library("fable")
library("feasts")

complete_frequency[,c("mes_ano", "UF", "frequency", "regiao")] %>%
  mutate(data_indice = as.Date(paste0(mes_ano, rep("-01", length(mes_ano))))) %>%
  mutate(m_y = tsibble::yearmonth(data_indice)) %>%
  as_tsibble(index = m_y, key=c(UF, regiao)) %>%
  dplyr::select(UF, regiao, frequency) -> denuncias_hts

# Agrega os dados por região e UF (Unidade Federativa), somando as frequências
denuncias_hts <- denuncias_hts |>
  aggregate_key(regiao / UF, frequency = sum(frequency))


## funcao para gerar todas as analises
analise_modelo_hts <- function(dados_tsibble, passos_prev, metodo, metodo_prev ) {
  
# mes_ano limite para criacao do modelo, ou seja, series temporais removendo os ultimos n passos (= passos_prev)
indice_limite <- unique(dados_tsibble$m_y)[length(unique(dados_tsibble$m_y)) - passos_prev]
  
base_model <- switch(metodo_prev, arima = {ARIMA})
metodo_hts <- switch(metodo, bu = {bottom_up},
                       td = {top_down},
                     midout = {middle_out},
                     MinT = {min_trace})
  
base_model_name <- switch(metodo_prev, arima = {"ARIMA"})
metodo_hts_name <- switch(metodo, bu = {"bottom_up"},
                            td = {"top_down"},
                          midout = {"middle_out"},
                          MinT = {"min_trace"})
  
  
# Ajusta um modelo ARIMA aos dados 
fit <- dados_tsibble |>
 filter(m_y <= indice_limite) |>
 model(base = base_model(frequency)) |>
 reconcile(
  metodo_ = metodo_hts(base))
  
names(fit)[names(fit) == "base"] <- base_model_name
names(fit)[names(fit) == "metodo_"] <- metodo_hts_name
  
  
# Faz previsões para os próximos períodos
fc <- fit |> forecast(h = passos_prev)

# Plota a previsão no nivel 0
plt_n0 <- fc |>
 filter(.model %in% c(metodo_hts_name), is_aggregated(regiao)) |>
 autoplot(dados_tsibble, alpha = 0.7, level = 90, size = 1) +
 labs(y = "Frequência de denúncias", x = "")+
  #title = paste0("Previsão ", metodo_hts_name, "/", base_model_name, " Nível 0 (Brasil)"))
  theme(strip.text.x = element_text(colour = "black", face = "bold",size=14),
        axis.title.x = element_text(colour = "black", face = "bold",size=14),
        axis.text.x = element_text(colour = "black", face = "bold",size=11),
        axis.title.y = element_text(colour = "black", face = "bold",size=14),
        axis.text.y = element_text(colour = "black", face = "bold",size=11),
        legend.position = "none")
    
#  Plota as previsões no nivel 1
  plt_n1 <- fc |>
    filter(
      .model %in% c(metodo_hts_name),
      !is_aggregated(regiao), is_aggregated(UF)
    ) |>
    autoplot(
      dados_tsibble, #|> filter(m_y => indice_limite)
      alpha = 0.7, level = 90, size = 1
    ) +
    labs(y = "Frequência de denúncias", x= "") +
    #title = paste0("Previsão ", metodo_hts_name, "/", base_model_name, " Nível 1 (Regiões)"),
    facet_wrap(vars(regiao), scales = "free_y", ncol = 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme(strip.text.x = element_text(colour = "black", face = "bold",size=14),
          axis.title.x = element_text(colour = "black", face = "bold",size=14),
          axis.text.x = element_text(colour = "black", face = "bold",size=11),
          axis.title.y = element_text(colour = "black", face = "bold",size=14),
          axis.text.y = element_text(colour = "black", face = "bold",size=11),
          legend.position = "none")
  
  
# Plota as previsões no nivel 2
plt_n2 <- fc |>
  filter(
    .model %in% c(metodo_hts_name),
    !is_aggregated(UF)
    ) |>
    autoplot(
      dados_tsibble, #|> filter(m_y => indice_limite)
      alpha = 0.7, level = 90,size = 1
    ) +
    labs(y = "Frequência de denúncias", x="") +
  #title = paste0("Previsão ", metodo_hts_name, "/", base_model_name, " Nível 2 (Estados)"),
    facet_wrap(vars(UF), scales = "free_y", ncol = 4) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(strip.text.x = element_text(colour = "black", face = "bold",size=14),
        axis.title.x = element_text(colour = "black", face = "bold",size=14),
        axis.text.x = element_text(colour = "black", face = "bold",size=11),
        axis.title.y = element_text(colour = "black", face = "bold",size=14),
        axis.text.y = element_text(colour = "black", face = "bold",size=11),
        legend.position = "none")

# Calcula a acurácia das previsões agregadas por região
acuracia <- fc |>
  accuracy(data = dados_tsibble)
  
  print(acuracia, 
        n = 2 * (sum(unlist(lapply(dados_tsibble[,key_vars(dados_tsibble)], function(x){length(unique(x))-1})))+1))
  
  return(list("plt_n0" = plt_n0, "plt_n1" = plt_n1,
              "plt_n2" = plt_n2, "acuracia" = acuracia, "previsao" = fc, "ajuste_modelo" = fit))
}

# plotar a partir de um certo ano
plot_a_partir_de <- function(plot, ano){
  return (plot %+% (plot$data %>% filter(year(m_y) >= ano)))
}

aumentar_espessura_linha <- function(plot, tamanho){
  plot_build <- ggplot_build(plot)
  plot_build$data[[3]]$linewidth <- tamanho
  gtable <- ggplot_gtable(plot_build)
  grid::grid.draw(gtable)
}

# modelo bottom-up com previsao por arima teste para 3 meses
bu_arima_modelo <- analise_modelo_hts(denuncias_hts, 3, "bu", "arima")
#plot nivel 0
bu_arima_modelo$plt_n0
#plot nivel 1
bu_arima_modelo$plt_n1
#plot nivel 2
bu_arima_modelo$plt_n2

#plot nivel 0
plot_a_partir_de(bu_arima_modelo$plt_n0, 2020)
aumentar_espessura_linha(plot_a_partir_de(bu_arima_modelo$plt_n0, 2020), 1)

#plot nivel 1
plot_a_partir_de(bu_arima_modelo$plt_n1, 2020)
aumentar_espessura_linha(plot_a_partir_de(bu_arima_modelo$plt_n1, 2020), 1)

#plot nivel 2
plot_a_partir_de(bu_arima_modelo$plt_n2, 2020)
aumentar_espessura_linha(plot_a_partir_de(bu_arima_modelo$plt_n2, 2020), 1)

#acuracia
print(bu_arima_modelo$acuracia, 
      n = 66)

# modelo top-down com previsao por arima teste para 3 meses
td_arima_modelo <- analise_modelo_hts(denuncias_hts, 3, "td", "arima")
#plot nivel 0
td_arima_modelo$plt_n0
#plot nivel 1
td_arima_modelo$plt_n1
#plot nivel 2
td_arima_modelo$plt_n2

#plot nivel 0
plot_a_partir_de(td_arima_modelo$plt_n0, 2020)
aumentar_espessura_linha(plot_a_partir_de(td_arima_modelo$plt_n0, 2020), 1)

#plot nivel 1
plot_a_partir_de(td_arima_modelo$plt_n1, 2020)
aumentar_espessura_linha(plot_a_partir_de(td_arima_modelo$plt_n1, 2020), 1)

#plot nivel 2
plot_a_partir_de(td_arima_modelo$plt_n2, 2020)
aumentar_espessura_linha(plot_a_partir_de(td_arima_modelo$plt_n2, 2020), 1)

#acuracia
print(td_arima_modelo$acuracia, 
      n = 66)

# modelo middle out com previsao por arima teste para 3 meses
midout_arima_modelo <- analise_modelo_hts(denuncias_hts, 3, "midout", "arima")

#plot nivel 0
X11()
png(filename = "PrevNivel0_2021.png",width = 400, height = 200,units = "mm",res =200) 
plot_a_partir_de(midout_arima_modelo$plt_n0, 2020)
aumentar_espessura_linha(plot_a_partir_de(midout_arima_modelo$plt_n0, 2020), 1)
dev.off()

#plot nivel 1
X11()
png(filename = "PrevNivel1_2021.png",width = 400, height = 200,units = "mm",res =200)
plot_a_partir_de(midout_arima_modelo$plt_n1, 2020)
aumentar_espessura_linha(plot_a_partir_de(midout_arima_modelo$plt_n1, 2020), 1)
dev.off()

#plot nivel 2
X11()
png(filename = "PrevNivel2_2021.png",width = 400, height = 200,units = "mm",res =200)
plot_a_partir_de(midout_arima_modelo$plt_n2, 2020)
aumentar_espessura_linha(plot_a_partir_de(midout_arima_modelo$plt_n2, 2020), 1)
dev.off()

#acuracia
print(midout_arima_modelo$acuracia, 
      n = 66)

# modelo Minimum trace com previsao por arima teste para 3 meses
MinT_arima_modelo <- analise_modelo_hts(denuncias_hts, 3, "MinT", "arima")
#plot nivel 0
MinT_arima_modelo$plt_n0
#plot nivel 1
MinT_arima_modelo$plt_n1
#plot nivel 2
MinT_arima_modelo$plt_n2

##plot nivel 0
plot_a_partir_de(MinT_arima_modelo$plt_n0, 2020)
aumentar_espessura_linha(plot_a_partir_de(MinT_arima_modelo$plt_n0, 2020), 1)

#plot nivel 1
plot_a_partir_de(MinT_arima_modelo$plt_n1, 2020)
aumentar_espessura_linha(plot_a_partir_de(MinT_arima_modelo$plt_n1, 2020), 1)

#plot nivel 2
plot_a_partir_de(MinT_arima_modelo$plt_n2, 2020)
aumentar_espessura_linha(plot_a_partir_de(MinT_arima_modelo$plt_n2, 2020), 1)

#acuracia
print(MinT_arima_modelo$acuracia, 
      n = 66)

-------------------------------------------------------------------------------------------------------------------
# Tabela de Desempenho das Abordagens
  
# Combina todos os modelos em um único dataframe
combine_modelos <- bind_rows(bu_arima_modelo$acuracia,td_arima_modelo$acuracia, 
                             midout_arima_modelo$acuracia, MinT_arima_modelo$acuracia)

# Primeiras linhas do dataframe combinado
head(combine_modelos)


# Resumo - médias das métricas MASE e RMSSE
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

# Exibindo o resultado
print(resumo, n=12)


-----------------------------------------------------------------------

# Melhorando a visualizacao das previsões do Nivel 2 - Middle Out
  
# Função para filtrar e gerar o gráfico por região a partir de um ano
plot_previsao_por_regiao_a_partir_de <- function(fc, dados_tsibble, regiao_filtro, metodo_hts_name, ano_inicio) {
  fc_filtrado <- fc %>%
    filter(.model %in% c(metodo_hts_name),
            regiao == regiao_filtro,
            !is_aggregated(UF),
            year(m_y) >= ano_inicio)
    
    dados_filtrados <- dados_tsibble %>% filter(regiao == regiao_filtro, year(m_y) >= ano_inicio)
    
    fc_filtrado %>%
      autoplot(dados_filtrados, alpha = 0.7, level = 90, size = 1) +
      labs(y = "Frequência de denúncias", x = "") +
      facet_wrap(~ str_to_upper(UF), scales = "free_y", ncol = 3) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(strip.text.x = element_text(colour = "black", face = "bold", size = 14),
            axis.title.x = element_text(colour = "black", face = "bold", size = 14),
            axis.text.x = element_text(colour = "black", face = "bold", size = 11),
            axis.title.y = element_text(colour = "black", face = "bold", size = 14),
            axis.text.y = element_text(colour = "black", face = "bold", size = 11),
            legend.position = "none")
  }

# Função para aumentar a espessura da linha
aumentar_espessura_linha <- function(plot, tamanho) {
  plot_build <- ggplot_build(plot)
  plot_build$data[[3]]$linewidth <- tamanho
  gtable <- ggplot_gtable(plot_build)
  grid::grid.draw(gtable)
}

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




