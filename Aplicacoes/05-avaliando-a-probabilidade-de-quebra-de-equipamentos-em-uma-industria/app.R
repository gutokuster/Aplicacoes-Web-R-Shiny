library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Probabilidade de Falhas em Equipamentos"),
    fluidRow(
        column(6, 
               radioButtons("opcao", "Selecione o tipo de cálculo", choices=list("Probabilidade Exata"=1, "Menor que"=2, "Maior que"=3), selected =1)
               ),
        column(6, 
               numericInput("ocorrencia", "Ocorrência Atual:", value=2, min=1, max=99),
               actionButton("processar", "processar")
               )
    ),
    fluidRow(
        column(12, 
               plotOutput("graficoProbabilidades")
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$processar,{
        lamb = input$ocorrencia
        tipoCalc = input$opcao
        inicio = lamb - 2
        fim = lamb + 2
        if (tipoCalc == 1) {
            x = dpois(inicio:fim, lambda = lamb)
            titulo = "Probabilidade de Ocorrência"
        }
        if (tipoCalc == 2) {
            x = ppois(inicio:fim, lambda = lamb)
            titulo = "Probabilidade de Ocorrência Menor que"
        }
        if (tipoCalc == 3) {
            x = ppois(inicio:fim, lambda = lamb, lower.tail = F)
            titulo = "Probabilidade de Ocorrência Maior que"
        }
        
        z = as.character(round(x,4))
        y = as.character(inicio:fim)
        
        lab = paste(y, "Probabilidade:", z)
        output$graficoProbabilidades = renderPlot({
            barplot(x,names.arg = lab, col = gray.colors(5), main=titulo)
        })
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
