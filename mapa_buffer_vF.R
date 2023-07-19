library(flexdashboard)
library(plotly)
library(ggplot2)
library(sf)
library(readr)
library(janitor)
library(rgdal)
library(readxl)
library(writexl)
library(tidyverse)
library(geobr)
library(dplyr)
library(tidyr)


rm(list = ls())

# carregando dados
# Importando o mapa da linhas retornadas
caminho_arquivo <- "mapa.shp"
dados_shapefile <- suppressMessages(st_read(dsn = caminho_arquivo, quiet = TRUE))

# Baixando mapa de bairros do Bairros do Rio
bairros_rj = read_neighborhood() %>% filter(name_muni == 'Rio De Janeiro')


# Dados do segundo mapa
base = read_csv2('linha_reactive.csv') %>% clean_names()

resultado = inner_join(dados_shapefile, base, by = c("servico" = "linha_servico"))



# baixando dados de linhas reativadas
#linhas_reativadas = read_csv2('linha_reactive.csv') %>% clean_names()

# Juntando as basess
#resultado <- inner_join(linhas_reativadas, dados_shapefile, by = c("linha_servico" = "servico"))

# Criando coluna com um unico valor para substituir depois
dados_shapefile$retorno_linha <- "Linha Fixa"

# linhas
valores_retornado <- c("10", "12", "14", "104", "157", "201", "229", "254", "277", "301", "311", "349", "388", "435", "448", "518", "519", "603", "605", "626", "651", "652", "669", "678", "702", "709", "741", "743", "753", "757", "778", "785", "808", "809", "817", "822", "825", "830", "831", "833", "842", "845", "849", "851", "865", "870", "871", "880", "881", "885", "892", "893", "899", "901", "915", "921", "922", "925", "928", "951", "987", "990", "SP852", "SV669", "SV692", "SV831", "SV899", "SV922", "SVA665", "SVB665", "SVB685", "SVB901")

# Tratando para reconhecer linhas
dados_shapefile$retorno_linha[dados_shapefile$servico %in% valores_retornado] <- "Linha Retornada"

# Baixando dados censitarios

rj = read_census_tract(code_tract = 33) %>% filter(name_muni == 'Rio De Janeiro')

# Verificando o tipo de ligação
crs_rj <- st_crs(rj)
crs_dados_shapefile <- st_crs(dados_shapefile)


# Transformando
if (crs_rj != crs_dados_shapefile) {
  rj <- st_transform(rj, crs_dados_shapefile)
}

# Construção do Buffer ------------------------------------- MAPA


# criando intersecção 
interseccao <- st_intersects(rj$geom, dados_shapefile$geometry, sparse = FALSE)

# Cria uma coluna de sinalizador nos dados do setor
rj$sinalizador <- apply(interseccao, 1, any)

# Criando coluna com base no sinalizado
rj$proximo <- ifelse(rj$sinalizado == TRUE, "Linha a 500 Metros", "Linha a mais de 500 Metros")

# Definir a centralidade
rj$centroide <- st_centroid(rj$geom)

# Criar buffer de 500  e 250 metros -----------------------------------------------


rj_crs_metros <- st_transform(rj, crs = 31983)  # Transformando para um CRS brasileiro em metros (SIRGAS 2000 / UTM zone 23S)
rj_crs_metros$buffer <- st_buffer(rj_crs_metros$centroide, dist = 500)
rj_crs_metros$buffer_250 <- st_buffer(rj_crs_metros$centroide, dist = 250)

# Intersecção do buffer --------------------------
# criando intersecção 
interseccao_buffer <- st_intersects(rj_crs_metros$buffer, dados_shapefile$geometry, sparse = FALSE)

# Cria uma coluna de sinalizador nos dados do setor
rj$sinalizador_buffer <- apply(interseccao_buffer, 1, any)

# Sinalizador
rj$analise_sinalizador <- ifelse(rj$sinalizador_buffe == TRUE, "Linha a menos 500 Metros", "Linha a mais de 500 Metros")

# Linha proximas 
linhas_por_buffer <- lapply(1:length(rj_crs_metros$buffer), function(i) {
  intersecting_lines <- st_intersects(rj_crs_metros$buffer[[i]], dados_shapefile)
  dados_shapefile$servico[unlist(intersecting_lines)]
})

# Adicione essa informação ao dataframe rj_crs_metros
rj_crs_metros$linhas_servico <- linhas_por_buffer

# lengh para ver para observar o total de linhas
rj_crs_metros$num_linhas <- sapply(linhas_por_buffer, length)

# PASSANDO OS DADOS PARA CSV

readr::write_csv(rj_crs_metros, "rj_crs_metros.csv")


# Mapa quantidade de linhas proximas a 500 do setor sensitário

# Saber valor maximo e minimo ----- teste
min(rj_crs_metros$num_linhas)

# Numero maximo
max(rj_crs_metros$num_linhas)

# Intervalos
breaks = seq(0, 299, by = 299/5)

breaks = round(breaks)


# Criar categorias
rj_crs_metros$categoria_num_linhas = cut(rj_crs_metros$num_linhas, 
                                          breaks = breaks, 
                                          include.lowest = TRUE)

# Ajustar os rótulos para refletir os limites das categorias
levels(rj_crs_metros$categoria_num_linhas) = paste(head(breaks, -1),'a', tail(breaks, -1), 'linhas')


# Plotando prototipo de mapa

linhas_por_buffer = lapply(1:length(rj_crs_metros$buffer), function(i) {
  intersecting_lines = st_intersects(rj_crs_metros$buffer[[i]], dados_shapefile)
  dados_shapefile$servico[unlist(intersecting_lines)]
})

# Adicione essa informação ao dataframe rj_crs_metros
rj_crs_metros$linhas_servico = linhas_por_buffer

# Para obter a contagem de linhas por buffer, simplesmente aplique a função length a cada elemento de linhas_servico
rj_crs_metros$num_linhas = sapply(linhas_por_buffer, length)

# Agora, para cada linha em rj_crs_metros, você tem uma lista de linhas de ônibus que passam por seu buffer,
# bem como o número total dessas linhas


# Sinalizador
rj$analise_sinalizador <- ifelse(rj$sinalizador_buffe == TRUE, "Linha a menos 500 Metros", "Linha a mais de 500 Metros")



rj_crs_metros$linhas_servico <- linhas_por_buffer

# lengh para ver para observar o total de linhas
rj_crs_metros$num_linhas <- sapply(linhas_por_buffer, length)

# PASSANDO OS DADOS PARA CSV

# codigo correto --------------------

# Mapa quantidade de linhas proximas a 500 do setor sensitário

# Saber valor maximo e minimo
min(rj_crs_metros$num_linhas)

# Numero maximo
max(rj_crs_metros$num_linhas)

# Intervalos
breaks = c(0, 2, 4, 6, 8, 10, Inf)
labels = c("0 a 2", "2 a 4", "4 a 6", "6 a 8", "8 a 10", "Acima de 10")


rj_crs_metros$categoria_num_linhas = cut(rj_crs_metros$num_linhas, 
                                         breaks = breaks, 
                                         labels = labels, 
                                         include.lowest = TRUE)

# Ajustar os rótulos para refletir os limites das categorias
levels(rj_crs_metros$categoria_num_linhas) = paste(head(breaks, -1),'a', tail(breaks, -1), 'linhas')



rj_crs_metros <- rj_crs_metros %>%
  mutate(linhas_servico = gsub(pattern = "\\(c\\)|\\(|\\)", replacement = "", x = linhas_servico))



# Mapa de escala continua --------------------------------


g <- ggplot(data = rj_crs_metros) +
  geom_sf(aes(geometry = geom, fill = num_linhas, text = paste("Número de Linhas: ", num_linhas, "<br>", 
                                                               "Linhas de Serviço: ", linhas_servico))) +
  scale_fill_gradient(low = "white", high = "darkgreen") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Linhas de Ônibus Municipais a 500 Metros dos Setores Censitários no Rio de Janeiro",
       fill = "Quantidade de Linhas",
       caption = "Fonte: Prefeitura do Rio de Janeiro") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               title.theme = element_text(size = 12, face = "bold"),
                               label.theme = element_text(size = 10))) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.caption = element_text(size = 10),
        legend.title.align = 0.5,
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

g_interativo <- ggplotly(g, tooltip = "text")

# Ajustar layout do plot interativo
g_interativo <- g_interativo %>%
  layout(title = list(text = "<b>Linhas de Ônibus Municipais a 500 Metros dos Setores Censitários no Rio de Janeiro</b>",
                      x = 0.5),
         margin = list(l = 50, r = 50, t = 80, b = 50))
g_interativo

# Identificando setores censitários proximos

install.packages('purrr')

library(sf)
library(purrr)

rj_crs_metros$buffer_250_teste <- st_buffer(rj_crs_metros$centroide, dist = 250)

# Cria uma matriz de interseções
matriz_interseccao <- st_intersects(rj_crs_metros$buffer_250_teste, rj_crs_metros$geom)

# Transforma a geometria 'buffer_250_teste' para o mesmo CRS que 'geom'
rj_crs_metros$buffer_250_teste <- st_transform(rj_crs_metros$buffer_250_teste, st_crs(rj_crs_metros$geom))

# Agora que as duas geometrias têm o mesmo CRS, você pode usar st_intersects()
matriz_interseccao <- st_intersects(rj_crs_metros$buffer_250_teste, rj_crs_metros$geom)



# Conta o número de interseções para cada setor (subtraindo 1 para remover a própria interseção)
rj_crs_metros$num_setores_proximos <- sapply(matriz_interseccao, function(x) length(x))

# CRIANDO A CATEGORIZAÇÃO DE SETORES PROXIMOS
rj_crs_metros$cat_setores_proximos <- ifelse(rj_crs_metros$num_setores_proximos >= 10, "10 ou mais", "Menos que 10")






# MApa de proximidade -------------------------------------------------@@@@@@@@@@@@@@@@@@@@@@@@@@#################
# COdigo final ------------------------------------------------

paleta <- c("0 a 2" = "#FF0000",  # Vermelho mais forte
            "2 a 4" = "#FF5500",  # Vermelho-alaranjado
            "4 a 6" = "#FFAA00",  # Laranja
            "6 a 8" = "#FFFF00",  # Amarelo
            "8 a 10" = "#FFFF99", # Branco
            "Acima de 10" = "#FFFFFF") # Branco


rj_crs_metros$tooltip_text <- paste("Número de Linhas: ", rj_crs_metros$num_linhas, "<br>", 
                                    "Linhas de Serviço: ", rj_crs_metros$linhas_servico, "<br>",
                                    "Setores Proximos: ", rj_crs_metros$num_setores_proximos, "<br>",
                                    'Bairro:', rj_crs_metros$name_neighborhood)


proximidade <- ggplot(data = rj_crs_metros) +
  geom_sf(aes(geometry = geom, fill = categoria_num_linhas, col =  cat_setores_proximos, text = paste("Número de Linhas: ", num_linhas, "<br>", 
                                                                         "Linhas de Serviço: ", linhas_servico,
                                                                         'Setores Proximos: ', num_setores_proximos)), lwd = 0.1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Linhas de Ônibus Municipais a 500 Metros dos Setores Censitários no Rio de Janeiro",
       fill = "Quantidade de Linhas",
       caption = "Fonte: Prefeitura do Rio de Janeiro") +
  scale_fill_manual(values = paleta, guide = guide_legend(title.position = "top",
                                                          title.hjust = 0.5,
                                                          title.theme = element_text(size = 12, face = "bold"),
                                                          label.theme = element_text(size = 10))) +
  scale_color_manual(values = c('pink', 'black'))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.caption = element_text(size = 10),
        legend.title.align = 0.5,
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))




proximidade <- ggplotly(proximidade, tooltip = 'text')

# Ajustar layout do plot interativo
proximidade <- proximidade %>%
  layout(title = list(text = "<b>Linhas de Ônibus Municipais a 500 Metros dos Setores Censitários no Rio de Janeiro</b>",
                      x = 0.5),
         margin = list(l = 50, r = 50, t = 80, b = 50))
proximidade




# Mapa de Fator Critico ----------------------------------------------------------------
rj_crs_metros$critico <- ifelse(rj_crs_metros$num_linhas < 3 & rj_crs_metros$num_setores_proximos > 10, "critico", "não critico")



paleta <- c("critico" = "#FFAA00",
            "não critico" = "#FFFFF1") 



critico <- ggplot(data = rj_crs_metros) +
  geom_sf(aes(geometry = geom, fill = critico, text = paste("Número de Linhas: ", num_linhas, "<br>", 
                                                            "Linhas de Serviço: ", linhas_servico, "<br>",
                                                            "Setores Proximos: ", num_setores_proximos, "<br>",
                                                            "Bairro: ", name_neighborhood)), lwd = 0.1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Setores Classificados como críticos na Distribuição de Ônibus do Rio de Janeiro",
       fill = "Quantidade de Linhas",
       caption = "Fonte: Prefeitura do Rio de Janeiro") +
  scale_fill_manual(values = paleta, guide = guide_legend(title.position = "top",
                                                          title.hjust = 0.5,
                                                          title.theme = element_text(size = 12, face = "bold"),
                                                          label.theme = element_text(size = 10))) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.caption = element_text(size = 10),
        legend.title.align = 0.5,
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "cm"))

critico <- ggplotly(critico, tooltip = "text")


# Ajustar layout do plot interativo
critico <- critico %>%
  layout(title = list(text = "<b>Setores Classificados como críticos na Distribuição de Ônibus do Rio de Janeiro</b>",
                      x = 0.5),
         margin = list(l = 50, r = 50, t = 80, b = 50))
critico

















