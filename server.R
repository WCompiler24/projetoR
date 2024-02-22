#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  #Graf1 - Linhas

    output$Graph1_Linhas <- renderPlotly({
      
      mediana_data <- dados_filtrados %>% group_by(DATA_COLETA_METADADOS, UF) %>% 
        summarise(mediaValor = median(VALOR))
      
      
      ggplotly(
        
        ggplot(mediana_data)+
          geom_line(aes(x = DATA_COLETA_METADADOS, y = mediaValor,
                        group = UF, color = UF), 
                    size = 1) +
          ggtitle("Média dos valores ao longo do tempo")
        
      )
      
      
    })
    
    
    #Graf2 - Boxplot
    
    output$Graph2_BoxPlot <- renderPlotly({
      
      ggplotly(
        
        dados_filtrados %>% ggplot() +
          geom_boxplot(aes(x = UF, y = VALOR, fill = UF)) +
          ggtitle("Variação dos valores por UF")+
          theme(legend.position = 'none')
      )
      
    })
    
    
    #Graf3 - Pontos
    
    output$Graph3_Pontos <- renderPlotly({
      ggplotly(
        dados_filtrados %>% ggplot()+
          geom_point(aes(x = QUILOMETRAGEM, y = VALOR, color = UF)) +
          ggtitle("Distribuição de VALOR e QUILOMETRAGEM por UF")
      )
      
    })
    
    #Graf4 - BoxPlot
    
    output$Graph4_BoxPlot <- renderPlotly({
      
      ggplotly(
        
        dados_filtrados %>% 
          ggplot(aes(x = TIPO_ANUNCIO, y = VALOR,fill = UF))+
          geom_boxplot( ) +
          theme(legend.position="none")+
          ggtitle("Variação do Preço por Tipo de Anúncio")
        
      ) %>% layout(boxmode = 'group')
      
    })
    
    #Graf5 - Pizza
    

    output$Graph5_Pizza <- renderPlotly({
      
      ## frequencia por cambio(GRÁFICO DE PIZZA) ####
      freq_cambio <- dados_filtrados %>% 
        group_by(CÂMBIO) %>%
        summarise(qtd = n()) %>%
        mutate(prop = qtd / sum(qtd) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop )
      
      ###Refazendo o grafico de Pizza com PLOTLY
      plot_ly(freq_cambio, labels = ~CÂMBIO, values = ~prop  , type = 'pie',
              textinfo = 'label+percent',showlegend = FALSE) %>%
        layout(title = 'Quantidade por Câmbio')

    })
    
    
    #Graf6 - Pizza
    
    output$Graph6_Pizza <- renderPlotly({
      
      ## frequencia por DIREÇÃO(GRÁFICO DE PIZZA) ####
      freq_direcao <- dados_filtrados %>% 
        group_by(DIREÇÃO) %>%
        summarise(qtd = n()) %>%
        mutate(prop = qtd / sum(qtd) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop )
      
      ###Refazendo o grafico de Pizza com PLOTLY
      
      plot_ly(freq_direcao, labels = ~DIREÇÃO, values = ~prop  , type = 'pie',
              textinfo = 'label+percent',showlegend = FALSE) %>%
        layout(title = 'Quantidade por Direção')
      
    })
    
    
    #Graf7 - Barras
    
    output$Graph7_Barras <- renderPlotly({
      
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
      
    })
    
    
    
    
    
    

}
