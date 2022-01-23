
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage( 
    #shinythemes::themeSelector(),
    theme = shinytheme("spacelab"),

    # Application title
    titlePanel("Probabilidade de Falha em Equipamentos"),
    
    fluidRow(
        column(6, 
               radioButtons("Opcao", label = "Selecione o Cálculo", choices = list("Prop. Exata" = 1, "Menos que" = 2, "Mais que" = 3), selected = 1)),
        column(6, 
               numericInput("Ocorrencia", "Ocorrência Atual:", value = 2, min = 1, max = 99),
               actionButton("Processar", "Processar")
               )
    ),
    fluidRow(
        column(12, 
               plotOutput("Graf"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$Processar, {
        
        # variavel para armazenar a média dada pelo fabricante
        lamb = input$Ocorrencia
        
        # lendo o índice selecionado no radio button
        tipo = input$Opcao
        
        # variavies de referencia para construir o gráfico e calcular a probabilidade
        inic = lamb - 2
        fim = lamb + 2
        
        if (tipo == 1) {
            # calculo da probabilidade exata dpois()
            x = dpois(inic:fim, lambda = lamb)
            tit = "Probabilidade de Ocorrência"
        }
        
        if (tipo == 2) {
            # calculo da probabilidade Menor que ppois()
            x = ppois(inic:fim, lambda = lamb)
            tit = "Probabilidade de Ocorrência Menor que"
        }
        
        if (tipo == 3) {
            # calculo da probabilidade Maior que ppois()
            # lower.tail = FALSE -> para calcular o outro lado da calda da probabilidade
            x = ppois(inic:fim, lambda = lamb, lower.tail = FALSE)
            tit = "Probabilidade de Ocorrência Maior que"
        }
        
        # para arredondar os valores e converter para texto
        z = as.character(round(x, 4))
        # gerando um vetor de textos com as frequencias para os nomes das barras gráfico
        y = as.character(inic:fim)
        
        lab = paste(y, "Prob:", z)
        
        output$Graf = renderPlot({
            
            # x -> calculo da probabilidade
            # names.arg -> nome das barras
            # main = tit -> mostrará de acordo com o tipo
            barplot(x, names.arg = lab, cor = gray.colors(5), main = tit)
            # box() -> cria uma moldura
            box()
        })
                                 
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
