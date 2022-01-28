
library(shiny)
library(RColorBrewer)

#dados = read.csv("dados.csv", sep = ";")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Despesas de Empenho da Rubrica Diárias no País dos Municípios Gaúchos"),
    
    fluidRow(
        column(6, 
               fileInput("arquivo", "Escolha o arquivo: ", multiple = FALSE, accept = c(".csv")),
               #helpText("Observação: ")
        ),
        column(6,
               actionButton("Processar", "Processar")
        )
    ),
    
    fluidRow(
        column(2, 
               h3(textOutput("TEmpenho")),
               tableOutput("SEmpenho")
        ),
        column(4, 
               plotOutput("HistEmpenho")
        ),
        column(2, 
               h3(textOutput("TPIB")),
               tableOutput("SPIB")
        ),
        column(4, 
               plotOutput("HistPib")
        )
    ),
    
    fluidRow(
        column(3, 
               plotOutput("BoxEmpenho")
        ),
        column(3, 
               plotOutput("BoxPib")
        ),
        column(6, 
               plotOutput("Disp")
        )
    ),
    fluidRow(
        column(4, 
               plotOutput("MaioresEmpenhos")
        ),
        column(4, 
               plotOutput("MaioresPibs")
        ),
        column(4, 
               plotOutput("MaiorProporcao")
        )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$Processar, {
        
        # lendo o arquivo
        file1 = input$arquivo
        dados = read.csv(file1$datapath, header = T, sep = ";")
        
        
        output$TEmpenho = renderText( {"Dados de Empenho"} )
        output$SEmpenho = renderTable( {as.array(summary(dados$VALOREMPENHO))} )
        output$TPIB = renderText( {"Dados dos PIB"})
        output$SPIB = renderTable( {as.array((summary(dados$PIB)))} )
        
        # brewer.pal -> paleta de cores / n -> nº de cores / name-> nome da paleta
        output$HistEmpenho = renderPlot( {
            hist(dados$VALOREMPENHO, main = "Valores de Empenho", col = brewer.pal(n = 3, name = "Paired"), xlab = "Empenho")
        })
        
        output$HistPib = renderPlot( {
            hist(dados$PIB, main = "Valores do PIB", col = brewer.pal(n = 3, name = "Pastel1"), xlab = "PIB")
        })
        
        output$BoxEmpenho = renderPlot( {
            boxplot(dados$VALOREMPENHO, main = "Valores de Empenho", col = brewer.pal(n = 3, name = "Paired"), outline = F, horizontal = T)
        })
        
        output$BoxPib = renderPlot( {
            boxplot(dados$PIB, main = "Valores do PIB", col = brewer.pal(n = 3, name = "Pastel1"), outline = F, horizontal = T)
        })
        
        output$Disp = renderPlot( {
            plot(dados$VALOREMPENHO, dados$PIB, main = "Empenho vs PIB", xlab = "Empenho", ylab = "PIB", pch = 19, col =  "Blue")
        })
        
        # " order(- " -> para trazer em ordem decrescente(-)
        # head() -> para filtrar uma quantidade de linhas
        Mempenho = head(dados[order(-dados$VALOREMPENHO),], 10)
        
        # last -> para não exibir as legendas dos elementos
        output$MaioresEmpenhos = renderPlot({
            barplot(Mempenho$VALOREMPENHO, col = brewer.pal(n = 10, name = "RdBu"), las = 2, main = "Maiores Empenhos", xlab = "Empenhos")
            # legend (posição, local origem da legenda)
            # lty -> proporção / # cex -> tamanho / # ncol -> nº de colunas / # lwd ->tipo de caracter q será usado na legenda
            # box() -> gera uma moldura
            legend("topright", legend = Mempenho$MUNICIPIO, col = brewer.pal(n = 10, name = "RdBu"), lty = 1:2, cex = 0.6, ncol = 2, lwd = 4)
            box()
        })
        
        # " order(- " -> para trazer em ordem decrescente(-)
        # head() -> para filtrar uma quantidade de linhas
        Mpibs = head(dados[order(-dados$PIB),], 10)
        
        output$MaioresPibs = renderPlot({
            pie(Mpibs$PIB, col = brewer.pal(n = 10, name = "Spectral"), labels = Mpibs$MUNICIPIO, main = "Maiores PIBs", xlab = "PIBs")
        })
        
        dados$PROPORCAO = dados$VALOREMPENHO / dados$PIB
        Mprop = head(dados[order(-dados$PROPORCAO),] , 10)
        
        output$MaiorProporcao = renderPlot({
            barplot(Mprop$PROPORCAO, col = brewer.pal(n = 10, name = "Set3"), las = 2, main = "Maiores gastos em Proporção ao PIB", xlab = "Proporção")
            legend("topright", legend = Mprop$MUNICIPIO, col = brewer.pal(n = 10, name = "Set3"), lty = 1:2, cex = 0.6, ncol = 2, lwd = 4)
            box()    
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
