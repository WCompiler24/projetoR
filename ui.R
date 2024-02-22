#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Dashboard de Vendas de Carros"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            print("Componentes laterais aqui")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("Graph1_Linhas"),
            plotlyOutput("Graph2_BoxPlot"),
            plotlyOutput("Graph3_Pontos"),
            plotlyOutput("Graph4_BoxPlot"),
            plotlyOutput("Graph5_Pizza"),
            plotlyOutput("Graph6_Pizza"),
            plotlyOutput("Graph7_Barras")
        )
    )
)
