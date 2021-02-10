library(shiny)
library(arules)
library(arulesViz)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Geração de Regras de Recomendação"),
    fluidRow(
        column(3,
               fileInput("arquivo","Selecione um arquivo", multiple = F, accept = c(".csv")),
               actionButton("processar", "Processar")
        ),
        column(3,
               numericInput("nisuporte", "Suporte Mínimo", 0.04, min=0.0001, max = 1)
        ),
        column(3,
               numericInput("niconfianca", "Confiança Mínima", 0.08, min=0.0001, max = 1)
        ),
        column(3,
               numericInput("inminimo", "Tamanho Mínimo", 2, min=1, max = 40)
        )
    ),
    fluidRow(
        column(3, plotOutput("grafico1")),
        column(3, plotOutput("grafico2")),
        column(3, plotOutput("grafico3")),
        column(3, plotOutput("grafico4"))
    ),
    fluidRow(
        column(12,
               h1(textOutput("textoRegra")),
               tableOutput("tregras")
               
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$processar, {
        file1 <- input$arquivo
        
        transacoes = read.transactions(file1$datapath, format = "basket", sep = ',')
        
        regras <- apriori(transacoes, parameter = list(supp = input$nisuporte, conf = input$niconfianca, minlen = input$inminimo))
        
        output$grafico1 = renderPlot({ plot(regras, method = "graph", control = list(type="itens"))})
        output$grafico2 = renderPlot({ plot(regras, method = "matrix", control = list(type="itens"))})
        output$grafico3 = renderPlot({ plot(regras, method = "matrix3d", measure ="lift")})
        output$grafico4 = renderPlot({ plot(regras, method = "grouped")})
        
        output$textoRegras = renderText({ "Regras" })
        output$tregras <- renderTable({ inspect(regras) })
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
