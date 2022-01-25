
library(shiny)
library(forecast)
library(ggplot2)
library(xts)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("slate"),
    #shinythemes::themeSelector(),
    
    
    

    # Application title
    titlePanel("Benchmark de Séries Temporais"),
    
    fluidRow(
        column(4, 
               fileInput("arquivo", "Escolha o arquivo: ", multiple = FALSE, accept = c(".csv")),
               helpText("Observação: o arquivo deve conter apenas uma coluna, sem nome no cabeçalho e ser do tipo CSV. A frequência deve ser mensal")
               ),
        column(4,
               #format -> formato da data, mensal
               #start -> data que por padrão vai aparecer no início da execução da aplicação
               #end -> data que por padrão vai aparecer no início da execução da aplicação
               # separador -> texto q ficará entre as datas start e end
               dateRangeInput('datas', label = "Período da Série", format = "mm/yyyy", language = "pt", start = "2008/01/01", end = "2021/12/31", startview = "year", separator = " até "),
               helpText("Observação: para definir mês e ano, selecione um dia qualquer")
               ),
        column(4,
               actionButton("Processar", "Processar")
               )
    ),
    fluidRow(
        column(12, plotOutput("GrafPrev"))
    ),
    fluidRow(
        column(6, 
               h2(textOutput("TMnaive")),
               tableOutput("Mnaive"),
               h2(textOutput("TMmeanf")),
               tableOutput("Mmeanf"),
               h2(textOutput("TMrwf")),
               tableOutput("Mrwf"),
               h2(textOutput("TMholt")),
               tableOutput("Mholt"),
               h2(textOutput("TMhw")),
               tableOutput("Mhw"),
               ),
        column(6, 
               h2(textOutput("TMhw2")),
               tableOutput("Mhw2"),
               h2(textOutput("TMhw3")),
               tableOutput("Mhw3"),
               h2(textOutput("TMtslm")),
               tableOutput("Mtslm"),
               h2(textOutput("TMarima")),
               tableOutput("Marima"),
               h2(textOutput("TMnnetar")),
               tableOutput("Mnnetar"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # evento do botao para processar
    observeEvent(input$Processar, {
        
        # lendo o arquivo
        file1 = input$arquivo
        data = read.csv(file1$datapath, header = F)
        
        # separando o ano e o mês 
        anoinic = as.integer(substr(input$datas[1], 1, 4))
        mesinic = as.integer(substr(input$datas[1], 6, 7))
        anofim = as.integer(substr(input$datas[2], 1, 4))
        mesfim = as.integer(substr(input$datas[2], 6, 7))
        
        # gerando uma série temporal // frequency = 12 - para mensal
        data = ts(data, start = c(anoinic, mesinic), end = c(anofim, mesfim), frequency = 12)
        
        valr = 24
        
        treino = window(data, start = c(anoinic, mesinic), end = c(anofim - 2, mesfim))
        teste = window(data, start = c(anofim - 2, mesinic), end = c(anofim, mesfim))
    
        # naive -> copia último valor
        Mnaive = naive(treino, h = valr)
        
        # meanf -> média
        Mmeanf = meanf(treino, h = valr)
        
        # drift -> acompanha a tendência da serie
        Mrwf = rwf(treino, h = valr, drift = T)
        
        # holt -> considera pesos para os intervalos
        Mholt = holt(treino, h = valr)
        
        # holt winter aditivo -> captura sazonal aditivo
        Mhw = hw(treino, seasonal = "additive", h = valr)
        
        # holt winter multiplicativo -> captura sazonal multiplicativo
        Mhw2 = hw(treino, seasonal = "multiplicative", h = valr)
        
        # holt winter amortecido -> multiplicativo amortecido
        # damped = T -> configura que será amorteciso
        # phi = 0.9 -> grau de amortecimento
        Mhw3 = hw(treino, seasonal = "multiplicative", h = valr, damped = T, phi = 0.9)
        
        # arima -> ordem da parte autoregressiva, grau de diferenciação e ordem da média móvel
        Marima = auto.arima(treino)
        Marima = forecast(Marima, h = valr)
        
        # linear
        Mtslm = tslm(treino ~ trend, data = treino)
        Mtslm = forecast(Mtslm, h = valr)
        
        #rede neural -> RNA para ST
        Mnnetar = nnetar(treino)
        Mnnetar = forecast(Mnnetar, h = valr)
        
        output$Mnaive = renderTable( { accuracy(teste, Mnaive$mean) })
        output$TMnaive = renderText( {"Naive"} )
        
        output$Mmeanf = renderTable( { accuracy(teste, Mmeanf$mean) })
        output$TMmeanf = renderText( {"Meanf"} )
        
        output$Mrwf = renderTable( { accuracy(teste, Mrwf$mean) })
        output$TMrwf = renderText( {"Drift"} )
        
        output$Mholt = renderTable( { accuracy(teste, Mholt$mean) })
        output$TMholt = renderText( {"Holt"} )
        
        output$Mhw = renderTable( { accuracy(teste, Mhw$mean) })
        output$TMhw = renderText( {"Holt Winter"} )
        
        output$Mhw2 = renderTable( { accuracy(teste, Mhw2$mean) })
        output$TMhw2 = renderText( {"Holt Winter Multiplicativo"} )
        
        output$Mhw3 = renderTable( { accuracy(teste, Mhw3$mean) })
        output$TMhw3 = renderText( {"Holt Winter Multiplicativo com Drift"} )
        
        output$Marima = renderTable( { accuracy(teste, Marima$mean) })
        output$TMarima = renderText( {"Arima"} )
        
        output$Mtslm = renderTable( { accuracy(teste, Mtslm$mean) })
        output$TMtslm = renderText( {"Regressão Linear"} )
        
        output$Mnnetar = renderTable( { accuracy(teste, Mnnetar$mean) })
        output$TMnnetar = renderText( {"Rede Neural Artificial"} )
        
        output$GrafPrev = renderPlot( {
            
            par(bg = "gray")
            plot(data, main = "Forecast Benchmark")
            
            lines( Mnaive$mean, type = "l", pch = 22, lty = 6, col = "red", lw2 = 2 )
            lines( Mmeanf$mean, type = "l", pch = 22, lty = 5, col = "blue", lw2 = 2 )
            lines( Mrwf$mean, type = "l", pch = 22, lty = 4, col = "green", lw2 = 2 )
            lines( Mholt$mean, type = "l", pch = 22, lty = 3, col = "chocolate1", lw2 = 2 )
            lines( Mhw$mean, type = "l", pch = 22, lty = 2, col = "slateblue4", lw2 = 2 )
            lines( Mhw2$mean, type = "l", pch = 22, lty = 1, col = "purple", lw2 = 2 )
            lines( Mhw3$mean, type = "l", pch = 22, lty = 6, col = "orangered3", lw2 = 2 )
            lines( Marima$mean, type = "l", pch = 22, lty = 5, col = "skyblue4", lw2 = 2 )
            lines( Mtslm$mean, type = "l", pch = 22, lty = 4, col = "cyan", lw2 = 2 )
            lines( Mnnetar$mean, type = "l", pch = 22, lty = 3, col = "salmon", lw2 = 2 )
            
            # gerendo legenda no gráfico
            legend("bottomleft", legend = c("Naive", "Mean", "Dri", "Hol", "HwM", "HwM", "HwMD", "Ari", "Lm", "RNA")
                   , col = c("red", "blue", "green", "chocolate1", "slateblue4", "purple", "orangered3", "skyblue4", "cyan", "salmon")
                   , lty = 1:2, cex = 0.8, ncol = 2, lwd = 4)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
