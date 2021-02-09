library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Teste de Normalidade"),
    helpText("Será analisada a primeira coluna do arquivo."),
    fluidRow(
        column(6, 
               fileInput("arquivo", "Selecione o arquivo:", multiple = FALSE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
               ),
        column(6, 
               actionButton("processar", "Processar")
               )
    ),
    fluidRow(
        column(4, 
               plotOutput("graficoHistograma")
               ),
        column(4, 
               plotOutput("graficoNormalidade")
               ),
        column(4, 
               h1(textOutput("test"))
               )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$processar, {
        file1 <- input$arquivo
        data = read.csv(file1$datapath, header = T)
        
        output$graficoHistograma = renderPlot({ hist(data[,1], main="Histograma")})
        output$graficoNormalidade = renderPlot({ qqnorm(data[,1])
            qqline(data)
            })
        tst = shapiro.test(data[,1])[2]
        tst = paste0("Valor de P: ", tst)
        output$test = renderText({ tst })
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
