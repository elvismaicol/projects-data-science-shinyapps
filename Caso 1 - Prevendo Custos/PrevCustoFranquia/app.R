library(shiny)
library(dplyr)
library(shinythemes)

# Carregar a base de dados
dados = read.csv("slr12.csv", sep = ";")

# Criando o modelo estat?stico para fazer as previs?es
modelo = lm(CusInic ~ FrqAnual, data = dados)

# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("slate"),
    
    #shinythemes::themeSelector(),
    
    titlePanel("Previsão de Custo Inicial para Montar uma Franquia"),
    
    fluidRow(
        column(4, 
               h2("Dados"),
               #tabela com os dados
               tableOutput("Dados")
        ),
        column(8, 
               #gráfico com os dados
               plotOutput("Graf")
        )
    ),
    fluidRow(
        column(6, 
               h3("Valor Anual da Franquia:"), 
               numericInput("NovoValor", "Insira Novo Valor", 1500, min = 1, max = 9999999),
               actionButton("Processar", "Processar")
        ),
        column(6, 
               h1(textOutput("Resultado"))
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Graf = renderPlot( {
        plot(CusInic ~ FrqAnual, data = dados, col = "blue", pch = 16, xlab = "Custo Inicial", ylab = "Valor Franquia Anual")
        # Mellhor ajuste
        abline(modelo)
    } )
    
    # Imprimindo dados
    output$Dados <- renderTable( { head(head(rename(dados, "Franquia Anual" = FrqAnual, "Custo Inicial" = CusInic), 10), 10) } )
    
    # Evento do botao para executar o calculo
    observeEvent(input$Processar, {
        valr = input$NovoValor
        prev = predict(modelo, data.frame(FrqAnual = eval(parse(text = valr))))
        prev = paste0("Previsão de Custos Inicial R$: ", round(prev, 2))
        output$Resultado = renderText( {prev} )
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
