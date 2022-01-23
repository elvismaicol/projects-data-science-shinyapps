library(shiny)
library(e1071)

carros = read.csv("car.data", sep = ",")

modelo = naiveBayes(class ~ ., carros)

buying = unique(carros$buying)
maint = unique(carros$maint)
doors = unique(carros$doors)
persons = unique(carros$persons)
lug_boot = unique(carros$lug_boot)
safety = unique(carros$safety)

# Define UI for application that draws a histogram
ui <- fluidPage( 
  
    # Application title
    titlePanel("Previsão de Veículos"),
    fluidRow(
        column(4, selectInput("buying","Preço", choices = buying)),
        column(4, selectInput("maint", "Manutenção", choices = maint)),
        column(4, selectInput("doors", "Portas", choices = doors))
    ),
    fluidRow(
        column(4, selectInput("persons", "Capacidade de Passageiros", choices = persons)),
        column(4, selectInput("lug_boot", "Porta Malas", choices = lug_boot)),
        column(4, selectInput("safety", "Segurança", choices = safety))
    ),
    fluidRow(
        column(12, actionButton("Processar", "Processar"), h1(textOutput("Resultado")))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$Processar, {
        novocarro = data.frame("buying" = input$buying, "maint" = input$maint, "doors" = input$doors, "persons" = input$persons, "lug_boot" = input$lug_boot, "safety" = input$safety)
        predicao = predict(modelo, novocarro)
        output$Resultado = renderText({ as.character(predicao) })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
