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



# CONSTURINDO A A JUNÇÃO DOS DADOS -------------------------

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


# criando intersecção 
interseccao <- st_intersects(rj$geom, dados_shapefile$geometry, sparse = FALSE)

# Cria uma coluna de sinalizador nos dados do setor
rj$sinalizador <- apply(interseccao, 1, any)

# Criando coluna com base no sinalizado
rj$proximo <- ifelse(rj$sinalizado == TRUE, "Linha a 500 Metros", "Linha a mais de 500 Metros")

# Definir a centralidade
rj$centroide <- st_centroid(rj$geom)

# Criar buffer de 500 metros
# Por favor, note que a unidade do buffer depende do sistema de coordenadas de seus dados.
# Se seus dados estão em graus (como em WGS84), você deve primeiro transformá-los para um CRS de metros.
rj_crs_metros <- st_transform(rj, crs = 31983)  # Transformando para um CRS brasileiro em metros (SIRGAS 2000 / UTM zone 23S)
rj_crs_metros$buffer <- st_buffer(rj_crs_metros$centroide, dist = 500)

# Intersecção do buffer
# criando intersecção 
interseccao_buffer <- st_intersects(rj_crs_metros$buffer, dados_shapefile$geometry, sparse = FALSE)

# Cria uma coluna de sinalizador nos dados do setor
rj$sinalizador_buffer <- apply(interseccao_buffer, 1, any)

# Identificando a quantidade de linhas -----------------------
# Para cada buffer, identifique as linhas de ônibus que o intersectam
linhas_por_buffer <- lapply(1:length(rj_crs_metros$buffer), function(i) {
  intersecting_lines <- st_intersects(rj_crs_metros$buffer[[i]], dados_shapefile)
  dados_shapefile$servico[unlist(intersecting_lines)]
})

# Adicione essa informação ao dataframe rj_crs_metros
rj_crs_metros$linhas_servico <- linhas_por_buffer

# Para obter a contagem de linhas por buffer, simplesmente aplique a função length a cada elemento de linhas_servico
rj_crs_metros$num_linhas <- sapply(linhas_por_buffer, length)

# Agora, para cada linha em rj_crs_metros, você tem uma lista de linhas de ônibus que passam por seu buffer,
# bem como o número total dessas linhas


# Sinalizador
rj$analise_sinalizador <- ifelse(rj$sinalizador_buffe == TRUE, "Linha a menos 500 Metros", "Linha a mais de 500 Metros")



rj_crs_metros$linhas_servico <- linhas_por_buffer

# lengh para ver para observar o total de linhas
rj_crs_metros$num_linhas <- sapply(linhas_por_buffer, length)

# PASSANDO OS DADOS PARA CSV



# Mapa quantidade de linhas proximas a 500 do setor sensitário

# Saber valor maximo e minimo
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



rj_crs_metros <- rj_crs_metros %>%
  mutate(linhas_servico = gsub(pattern = "\\(c\\)|\\(|\\)", replacement = "", x = linhas_servico))


# pLOTANDO O GRAFICO

p <- ggplot(data = rj_crs_metros) +
  geom_sf(aes(geometry = geom, fill = categoria_num_linhas)) +
  geom_sf(data = dados_shapefile, aes(geometry = geometry, fill = retorno_linha))+
  scale_fill_brewer(palette = "RdYlGn") +
  labs(title = 'Linhas a 500 metros do Setor Censitário na Cidade do Rio de Janeiro', 
       subtitle = 'Fonte - IBGE / Data.rio')
  theme_minimal() +
  theme(legend.position = "bottom")

p_interativo = ggplotly(p)

p_interativo

# Mapa de escala continua --------------------------------
# Criação do gráfico com ggplot2
g = ggplot(data = rj_crs_metros) +
  geom_sf(aes(geometry = geom, fill = num_linhas, text = paste("Número de Linhas: ", num_linhas, "<br>", 
                                                               "Linhas de Serviço: ", linhas_servico))) +
  scale_fill_gradient(low = "white", high = "darkgreen") + 
  theme_minimal() +
  theme(legend.position = "bottom") 

# Adicionando interatividade com plotly
g_interativo = ggplotly(g, tooltip = "text")


g_interativo


















cores <- c('#8d2036', 'green')

cores2 <- c('#8d4996', 'yellow')


# Somente o mapa -- Buffer
buffer_500  = ggplot() +
  geom_sf(data = rj, aes(geometry = geom, fill = analise_sinalizador), color = 'black', alpha = 0.5, size = 0.5) +
  geom_sf(data = dados_shapefile, aes(geometry = geometry, color = retorno_linha)) +
  labs(title = "Mapa de Linhas de Ônibus na Cidade do Rio de Janeiro - Proximidades de Setores Censitários",
       color = "Retorno de Linha", subtitle = 'Fonte: Data.rio') +
  scale_color_manual(values = c("green", '#8d2036')) +
  theme_void() +
  theme(
    plot.title = element_text(family = "Arial", size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(family = "Arial", size = 14, face = "italic", hjust = 0.5),
    axis.title = element_text(family = "Arial", size = 12, face = "bold"), 
    axis.text = element_text(family = "Arial", size = 10), 
    legend.title = element_blank(),  
    legend.text = element_text(family = "Arial", size = 10)+
      scale_fill_manual(values = cores, name = "Marcadores", labels = c("Ponto Central a mais 500 Metros", "Ponto Central a menos 500 Metros")) +
      scale_color_manual(values = cores2, name = "Retorno de Linha", labels = c("Linha Fixa", "Linha Retomada")))


verde <- "#008000"       
vermelho_claro <- "#FFCCCC"   
# Atualizar as cores na estética do ggplot
buffer_500 <- buffer_500 +
  scale_fill_manual(values = c(vermelho_claro, verde))

buffer_500_t <- ggplotly(buffer_500)%>% 
  style(line = list(width = 0.4))

# Plot finat

buffer_500_t <- buffer_500_t %>%
  layout(
    legend = list(
      title = "",
      itemsizing = "constant",
      itemwidth = 60,
      items = list(
        list(label = "Ponto Central a mais 500 Metros", style = list(fill = verde)),
        list(label = "Ponto Central a menos 500 Metros", style = list(fill = vermelho_claro)),
        list(label = "Linha Fixa", style = list(color = "black")),
        list(label = "Linha Retomada", style = list(color = "black", linetype = "dashed"))
      )
    )
  )

buffer_500_t




