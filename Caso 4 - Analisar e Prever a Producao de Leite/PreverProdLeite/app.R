library(shiny)
#Pacote para fazer a previsão com Arima
library(forecast)
#Pacote para gerar elementos gráficos
library(ggplot2)
#library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage( #shinythemes::themeSelector(),
    

    # Application title
    titlePanel("Sistema de Análise e Previsão da Pordução de Leite"),
    
    fluidRow(
        # abre uma janela para selecionar o arquivo.
        # multiple = F -> permitirá selecionar apenas 1 arquivo por vez.
        # accept = c() -> vetor para configurar quais tipos de aquivos serão aceitos.
        column(4, 
               fileInput("arquivo", "Escolha o arquivo: ", multiple = FALSE, accept = c(".csv")),
               helpText("Observação: o arquivo deve conter apenas uma coluna, sem nome no cabeçalho e ser do tipo CSV. A frequência deve ser mensal")
               ),
        column(4, 
               #format -> formato da data, mensal
               #start -> data que por padrão vai aparecer no início da execução da aplicação
               #end -> data que por padrão vai aparecer no início da execução da aplicação
               # separador -> texto q ficará entre as datas start e end
               dateRangeInput('datas', label = "Período da Série", format = "mm/yyyy", language = "pt", start = "2000/01/01", end = "2013/12/31", startview = "year", separator = " até "),
               helpText("Observação: para definir mês e ano, selecione um dia qualquer")
               ),
        column(4,
               numericInput("PeriodoPrevisao", "Informe quantos meses quer prever:", 12, min = 1, max = 48),
               actionButton("Processar", "Processar")
               )
    ),
    fluidRow(
        column(6, 
               plotOutput("GrafSerie")
               ),
        column(6,
               plotOutput("GrafHist")
               )
    ),
    fluidRow(
        column(6, 
               plotOutput("GrafBox")
               ),
        column(6, 
               plotOutput("GrafDec")
               )
    ),
    hr(),
    fluidRow(
        column(6, 
               plotOutput("GrafPrev")
               ),
        column(2,
               h1(textOutput("llower")),
               tableOutput("lower")
               ),
        column(2, 
               h1(textOutput("lmean")), 
               tableOutput("mean")
               ),
        column(2,
               h1(textOutput("lupper")), 
               tableOutput("upper")
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$Processar, {
        
        # lendo o arquivo
        file1 = input$arquivo
        data = read.csv(file1$datapath, header = F)
        
        # separando o ano e mês 
        anoinic = as.integer(substr(input$datas[1], 1, 4))
        mesinic = as.integer(substr(input$datas[1], 6, 7))
        anofim = as.integer(substr(input$datas[2], 1, 4))
        mesfim = as.integer(substr(input$datas[2], 6, 7))
        
        # gerando uma série temporal // frequency = 12 - para mensal
        data = ts(data, start = c(anoinic, mesinic), end = c(anofim, mesfim), frequency = 12)
        
        # Gráfico da Série original
        output$GrafSerie = renderPlot( {autoplot(data, main = "Série original", xlab = "Anos", ylab = "Produção")} )
        
        # Histograma
        output$GrafHist = renderPlot( {hist(data, main = "Histograma", xlab = "Produção") } )
        
        #Boxplot
        output$GrafBox = renderPlot( {boxplot(data, main = "Boxplot")})
        
        #Decomposição
        # para gerar o gráfico de decomposição é preciso antes decompor a serie temporal (decompose(data))
        dec = decompose(data)
        output$GrafDec = renderPlot( {autoplot(dec, main = "Decomposição", xlab = "Anos")} )
        
        # criando um modelo para fazer a previsão
        # auto.arima() -> função cria um modelo arima buscando a melhor configuração/parametros
        modelo = auto.arima(data)
        
        valr = input$PeriodoPrevisao
        
        # realizando a previsão
        previsao = forecast(modelo, h = valr)
        
        #efetuando a impressão do resultado
        output$lower = renderTable( {previsao$lower} )
        output$mean = renderTable( {previsao$mean} )
        output$upper = renderTable( {previsao$upper} )
        
        output$llower = renderText( {"Baixo"} )
        output$lupper = renderText( {"Alto"} )
        output$lmean = renderText( {"Médio"} )
        
        output$GrafPrev = renderPlot( {autoplot(previsao, main = "Previsão de Produção", xlab = "Anos", ylab = "Produção")} )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
