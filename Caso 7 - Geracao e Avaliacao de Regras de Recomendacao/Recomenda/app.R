
library(shiny)
library(arules)
library(arulesViz)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Geração de Regras de Recomendação"),
    
    fluidRow(
        column(3,
               fileInput("arquivo", "Escolha o arquivo", multiple = F, accept = c(".csv")),
               actionButton("Processar", "Processar")
               ),
        column(3,
               numericInput("nisuporte", "Suporte Mínimo", 0.04, min = 0.0001, max = 1)
               ),
        column(3,
               numericInput("niconfianca", "Confiança Mínima", 0.08, min = 0.0001, max = 1)
               ),
        column(3, 
               numericInput("niminimo", "Tamanho Mínimo", 2, min = 1, max = 40)
               )
    ),
    fluidRow(
        column( 3, plotOutput("Graf1") ),
        column( 3, plotOutput("Graf2") ),
        column( 3, plotOutput("Graf3") ),
        column( 3, plotOutput("Graf4") )
    ),
    fluidRow(
        column(12, 
               h1(textOutput("txtregras")),
               tableOutput("tregras")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$Processar, { 
        
        file1 = input$arquivo
        
        transacoes = read.transactions(file1$datapath, format = "basket", sep = ",")
        
        regras = apriori( transacoes, parameter = list(supp = input$nisuporte, conf = input$niconfianca, minlen = input$niminimo))
        
        output$Graf1 = renderPlot( {plot(regras, method = "graph", control = list(type = "items"))} )
        output$Graf2 = renderPlot( {plot(regras, method = "matrix", control = list(type = "items"))} )
        output$Graf3 = renderPlot( {plot(regras, method = "matrix3D", measure = "lift") })
        output$Graf4 = renderPlot( {plot(regras, method = "grouped")} )
      
        output$txtregras = renderText( {"Regras"} )
        output$tregras = renderTable( {inspect(regras)} )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
