library(shiny)

dados = read.csv("slr12.csv", sep = ";")
modelo = lm(CusInic ~ FrqAnual, data=dados) 
    
# UI
ui <- fluidPage(

    titlePanel("Previsão de custo incial para montar uma franquia"),

    fluidRow(
        column(4,
               h2("Dados"),
               tableOutput("Dados")
        ),
        column(8,
               plotOutput("Grafico")
        )
    ),
    fluidRow(
        column(6,
            h3("Valor anual da franquia"),
            numericInput("novoValor", "Insira um novo valor", 1500, min = 1, max = 9999999),
            actionButton("processar","Processar")
        ),
        column(6,
            h1(textOutput("Resultado"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Grafico = renderPlot({
        plot(CusInic ~ FrqAnual, data=dados)
        abline(modelo) # Criar linha de melhor ajuste 
    })
    
    output$Dados = renderTable({ head(dados,10) })
    
    observeEvent(input$processar, {
        valor = input$novoValor
        previsao = predict(modelo, data.frame(FrqAnual = eval(parse(text = valor))))
        previsao = paste0("Previsão de custo incial R$ ", round(previsao,2)) # paste0 concatena string
        output$Resultado = renderText({ previsao })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
