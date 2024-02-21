setwd("C:/Users/Wilmar Frechauth/Desktop/TRY R/CARROS")

if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(plotly)) install.packages("plotly")





dados <- read.csv("dados_shiny_2022081822.csv")

dados_limpos <- select(dados, -c(5,25))

#Variaveis a serem analizadas

  #MODELO
  #ANO
  #ACESSÓRIOS: 

#[25] "AIR.BAG"               "ALARME"               
#[27] "AR.CONDICIONADO"       "BLINDADO"             
#[29] "CÂMERA.DE.RÉ"          "DIREÇÃO .HIDRÁULICA"   
#[31] "SENSOR.DE.RÉ"          "SOM"                  
#[33] "TRAVA.ELÉTRICA"        "VIDRO.ELÉTRICO"  




theme_set(theme_bw())

#Grafico 1 - Linhas
##Médias dos valores ao longo do tempo

modelo <- "corolla xei 16v"
uf <- c("mt", "se")

dados_filtrados <- dados_limpos %>% filter(MODELO == modelo &
                                             UF %in% uf)

mediana_data <- dados_filtrados %>% group_by(DATA_COLETA_METADADOS, UF) %>% 
  summarise(mediaValor = median(VALOR))

ggplotly(

ggplot(mediana_data)+
  geom_line(aes(x = DATA_COLETA_METADADOS, y = mediaValor,
                group = UF, color = UF), 
            size = 1) +
  ggtitle("Média dos valores ao longo do tempo")

)


#Gráfico 2 - BoxPlot
#Variação de valores por UF: Min, Max, Mediana/Média, Outliers,

ggplotly(

dados_filtrados %>% ggplot() +
  geom_boxplot(aes(x = UF, y = VALOR, fill = UF)) +
  ggtitle("Variação dos valores por UF")+
theme(legend.position = 'none')

)

#Gráfico 3 - Pontos
#Variação: KM, VALOR, UF

ggplotly(
dados_filtrados %>% ggplot()+
  geom_point(aes(x = QUILOMETRAGEM, y = VALOR, color = UF)) +
  ggtitle("Distribuição de VALOR e QUILOMETRAGEM por UF")
)


#Gráfico 4 - BoxPlot
## VARIAÇÃO PREÇOS POR UF E TIPO ANUNCIO(GRÁFICO DE BOXPLOT) com e sem outlier ####

ggplotly(
  
dados_filtrados %>% 
  ggplot(aes(x = TIPO_ANUNCIO, y = VALOR,fill = UF))+
  geom_boxplot( ) +
  theme(legend.position="none")+
  ggtitle("Variação do Preço por Tipo de Anúncio")

) %>% layout(boxmode = 'group')



#Gráfico 5 - Pizza
## frequencia por cambio(GRÁFICO DE PIZZA) ####

freq_cambio <- dados_filtrados %>% 
  group_by(CÂMBIO) %>%
  summarise(qtd = n()) %>%
  mutate(prop = qtd / sum(qtd) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplotly(

freq_cambio %>%          
  ggplot(aes(x="", y=prop, fill=CÂMBIO)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = paste(CÂMBIO, '\n',round(prop,2),'%') ), 
            color = "white", size=6)+ 
  ggtitle("Quantidade por Câmbio")
)


###Refazendo o grafico de Pizza com PLOTLY
plot_ly(freq_cambio, labels = ~CÂMBIO, values = ~prop  , type = 'pie',
        textinfo = 'label+percent',showlegend = FALSE) %>%
  layout(title = 'Quantidade por Câmbio')


#Gráfico 6 - Pizza
## frequencia por DIREÇÃO(GRÁFICO DE PIZZA) ####
freq_direcao <- dados_filtrados %>% 
  group_by(DIREÇÃO) %>%
  summarise(qtd = n()) %>%
  mutate(prop = qtd / sum(qtd) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

freq_direcao %>%
  ggplot(aes(x="", y=prop, fill=DIREÇÃO)) +
  geom_bar(stat="identity",width=10, size = 1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() + 
  geom_text(aes(y = ypos, label = paste0(round(prop,1),'%') ), 
            color = "white", size=5)+
  ggtitle("Quantidade por Direção")

###Refazendo o grafico de Pizza com PLOTLY

plot_ly(freq_direcao, labels = ~DIREÇÃO, values = ~prop  , type = 'pie',
        textinfo = 'label+percent',showlegend = FALSE) %>%
  layout(title = 'Quantidade por Direção')



#Gráfico 7 - Barras
## frequencia por cor(GRÁFICO DE barras) ####

ggplotly(
dados_filtrados %>%
  group_by(COR) %>%
  summarise(QTD = n() ) %>%
  ggplot() +
  geom_bar(aes(x = reorder(COR,QTD ),y = QTD, fill = QTD), stat = 'identity')+
  ggtitle("Quantidade por Cor") + xlab('COR')+
  theme(legend.position = 'none'),
tooltip = c('x', 'y')
)















