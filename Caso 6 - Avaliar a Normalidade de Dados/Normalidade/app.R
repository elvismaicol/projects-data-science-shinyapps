
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("sandstone"),
    # shinythemes::themeSelector(),

    # Application title
    titlePanel("Teste de Normalidade"),
    
    fluidRow(
        column(6, 
               helpText("Será analisada a primeira coluna do arquivo"),
               fileInput("arquivo", "Escolha o arquivo", multiple = F, accept = c("text/csv"))
               ),
        column(6, 
               actionButton("Processar", "Processar")
               )
    ),
    fluidRow(
        column(4, 
               plotOutput("Grafhist")
               ),
        column(4, 
               plotOutput("Grafqqplot")
               ),
        column(4, 
               h1(textOutput("test"))
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$Processar, {
        
        file1 = input$arquivo
        data = read.csv(file1$datapath,header = T)
        
        # Histograma
        output$Grafhist = renderPlot( { hist(data[, 1], main = "Histograma") })
        
        # Gráfico de Normalidade
        output$Grafqqplot = renderPlot( { qqnorm(data[, 1])
            # linha de normalidade
            qqline(data, col = "red")
            })
        
        # Teste de Shapiro wilk / [2]-> para pega apenas o resultado p-value
        tst = shapiro.test(data[, 1])[2]
        # paste0 -> serve para gerar o valor como texto
        tst = paste0("Valor de P: ", tst)
        output$test = renderText( { tst } )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
