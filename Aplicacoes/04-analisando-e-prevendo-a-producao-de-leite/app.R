library(shiny)
library(forecast)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sistema de Análise e Previsão de Séries Temporais"),
    fluidRow(
        column(4, fileInput("arquivo", "Selecione o arquivo com os dados:", multiple = F, accept = c(".csv")),
               helpText("O arquivo não pode ter cabeçalho e deve ter apenas uma coluna, a frequência deve ser mensal!")
               ),
        column(4, 
               dateRangeInput("datas", label="Período da Série", format="mm/yyyy", language="pt", start="01/01/2000", end="12/31/2013", startview="year", separator = " até "),
               helpText("Para definir mês e ano, selecione um dia qualquer.")
               ),
        column(4, 
               numericInput("periodoPrevisao", "Período para a Previsão (1 a 48 meses):", 12, min=1, max=48),
               actionButton("processar", "Processar")
               )
    ),
    fluidRow(
        column(6,
               plotOutput("graficoSerie")
               ),
        column(6, 
               plotOutput("graficoHistorico")
               )
    ),
    fluidRow(
        column(6, 
               plotOutput("graficoBox")
               ),
        column(6,
               plotOutput("graficoDecomposicao")
               )
    ),
    hr(),
    fluidRow(
        column(6, 
               plotOutput("graficoPrevisao")
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
    
    observeEvent(input$processar,{
        
        file1 = input$arquivo
        data = read.csv(file1$datapath, header = F)
        
        anoini = as.integer(substr(input$datas[1], 1, 4)) # Retorna o indice 1 de datas, posição 1 até 4 (ano)
        mesini = as.integer(substr(input$datas[1], 6, 7)) 
        anofim = as.integer(substr(input$datas[2], 1, 4)) 
        mesfim = as.integer(substr(input$datas[2], 6, 7)) 

        data = ts(data,start = c(anoini, mesini), end = c(anofim, mesfim), frequency = 12)
        output$graficoSerie = renderPlot({ autoplot(data, main="Série Original")})
        output$graficoHistorico = renderPlot({ hist(data, main = "Histograma")})
        output$graficoBox = renderPlot({ boxplot(data, main = "Box Plot")})
        dec = decompose(data)
        output$graficoDec = renderPlot({ autoplot(dec, main="Decomposição")})
            
        modelo = auto.arima(data)
        periodo = input$periodoPrevisao
        
        previsao = forecast(modelo, h=periodo)
        
        output$lower = renderTable({ previsao$lower})
        output$mean = renderTable({ previsao$mean})
        output$upper = renderTable({ previsao$upper})
        
        output$llower = renderText({"Lower"})
        output$lmean = renderText({"Mean"})
        output$lupper = renderText({"Upper"})
        
        output$graficoPrevisao = renderPlot({autoplot(previsao, main="Previsão da Série")})
        })

}

# Run the application 
shinyApp(ui = ui, server = server)
