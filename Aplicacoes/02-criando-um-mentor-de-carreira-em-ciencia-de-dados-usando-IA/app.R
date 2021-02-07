library(shiny)
library(sets)

sets_options("universe", seq(1,100,1)) # Define limite mínimo e máximo de cada variável. De 1 a 100, passo 1) 

# Variáveis linguísticas
variaveis <- set(
    gostaExatas = fuzzy_partition(varnames = c(gemin = 5, gemen = 15, gemed = 50, gemaior = 75, gemax = 90), sd = 10),
    relacaoInterpassoal = fuzzy_partition(varnames = c(rimin = 30, rimen = 55, rimed = 55, rimaior = 75, rimax = 85), sd = 10),
    escreveCodigo = fuzzy_partition(varnames = c(ecmin = 10, ecmen = 25, ecmed = 50, ecmaior = 75, ecmax = 95), sd = 10),
    perfilLider = fuzzy_partition(varnames = c(plmin = 30, plmen = 50, plmed = 70, plmaior = 90, plmax = 95), sd = 10),
    gostaEstudar = fuzzy_partition(varnames = c(gsmin = 20, gsmen = 40, gsmed = 60, gsmaior = 80, gsmax = 90), sd = 10),
    habilidadeComunicacao= fuzzy_partition(varnames = c(hcmin = 40, hcmen = 50, hcmed = 60, hcmaior = 70, hcmax = 75), sd = 10),
    classificacao = fuzzy_partition(varnames = c(baixa=10, media=50, alta=75, altissima=95), sd = 10)
)

# Definição das Regras
regras <-
    set(
        fuzzy_rule( gostaExatas %is%	gemax	&& relacaoInterpassoal %is%	rimin	&& escreveCodigo %is%	ecmax	&& perfilLider %is%	plmin	&& gostaEstudar %is%	gsmax	&& habilidadeComunicacao %is%	hcmin	, classificacao %is%	altissima	),
        fuzzy_rule( gostaExatas %is%	gemax	&& relacaoInterpassoal %is%	rimen	&& escreveCodigo %is%	ecmaior	&& perfilLider %is%	plmin	&& gostaEstudar %is%	gsmax	&& habilidadeComunicacao %is%	hcmen	, classificacao %is%	altissima	),
        fuzzy_rule( gostaExatas %is%	gemaior	&& relacaoInterpassoal %is%	rimen	&& escreveCodigo %is%	ecmaior	&& perfilLider %is%	plmin	&& gostaEstudar %is%	gsmax	&& habilidadeComunicacao %is%	hcmed	, classificacao %is%	altissima	),
        fuzzy_rule( gostaExatas %is%	gemaior	&& relacaoInterpassoal %is%	rimen	&& escreveCodigo %is%	ecmaior	&& perfilLider %is%	plmen	&& gostaEstudar %is%	gsmaior	&& habilidadeComunicacao %is%	hcmed	, classificacao %is%	alta	),
        fuzzy_rule( gostaExatas %is%	gemaior	&& relacaoInterpassoal %is%	rimed	&& escreveCodigo %is%	ecmed	&& perfilLider %is%	plmed	&& gostaEstudar %is%	gsmaior	&& habilidadeComunicacao %is%	hcmaior	, classificacao %is%	alta	),
        fuzzy_rule( gostaExatas %is%	gemaior	&& relacaoInterpassoal %is%	rimed	&& escreveCodigo %is%	ecmed	&& perfilLider %is%	plmaior	&& gostaEstudar %is%	gsmaior	&& habilidadeComunicacao %is%	hcmaior	, classificacao %is%	alta	),
        fuzzy_rule( gostaExatas %is%	gemed	&& relacaoInterpassoal %is%	rimaior	&& escreveCodigo %is%	ecmen	&& perfilLider %is%	plmaior	&& gostaEstudar %is%	gsmed	&& habilidadeComunicacao %is%	hcmaior	, classificacao %is%	media	),
        fuzzy_rule( gostaExatas %is%	gemed	&& relacaoInterpassoal %is%	rimaior	&& escreveCodigo %is%	ecmen	&& perfilLider %is%	plmaior	&& gostaEstudar %is%	gsmed	&& habilidadeComunicacao %is%	hcmax	, classificacao %is%	media	),
        fuzzy_rule( gostaExatas %is%	gemen	&& relacaoInterpassoal %is%	rimax	&& escreveCodigo %is%	ecmin	&& perfilLider %is%	plmax	&& gostaEstudar %is%	gsmed	&& habilidadeComunicacao %is%	hcmax	, classificacao %is%	media	),
        fuzzy_rule( gostaExatas %is%	gemen	&& relacaoInterpassoal %is%	rimax	&& escreveCodigo %is%	ecmin	&& perfilLider %is%	plmax	&& gostaEstudar %is%	gsmen	&& habilidadeComunicacao %is%	hcmax	, classificacao %is%	baixa	),
        fuzzy_rule( gostaExatas %is%	gemin	&& relacaoInterpassoal %is%	rimax	&& escreveCodigo %is%	ecmin	&& perfilLider %is%	plmax	&& gostaEstudar %is%	gsmen	&& habilidadeComunicacao %is%	hcmax	, classificacao %is%	baixa	),
        fuzzy_rule( gostaExatas %is%	gemin	&& relacaoInterpassoal %is%	rimax	&& escreveCodigo %is%	ecmin	&& perfilLider %is%	plmax	&& gostaEstudar %is%	gsmin	&& habilidadeComunicacao %is%	hcmax	, classificacao %is%	baixa	)
    )

sistema <- fuzzy_system(variaveis, regras)

ui <- fluidPage(

    # Application title
    titlePanel("Aderência de Perfil para Carreira de Cientista de Dados"),
    helpText("Selecione as respostas movendo os sliders."),
    
    fluidRow(
        column(4,sliderInput("sexatas", "Gosto por exatas", min=5, max=90, step=10, value=40) ),
        column(4,sliderInput("sinter", "Relacionamento Interpessoal", min=30, max=85, step=5, value=50) ),
        column(4,sliderInput("scodigo", "Gosto por escrever código", min=10, max=95, step=5, value=40) )
    ),
    fluidRow(
        column(4,sliderInput("slideranca", "Perfil de Liderança", min=30, max=95, step=5, value=50) ),
        column(4,sliderInput("sestudar", "Gosta de Estudar", min=20, max=90, step=10, value=40) ),
        column(4,sliderInput("scomunica", "Habilidade de Comunicação", min=40, max=75, step=5, value=50) )
    ),
    fluidRow(
        column(6, h1("Sistema de Inferência:"), plotOutput("graficoSistema") ),
        column(6, actionButton("processar","Processar"), 
               helpText("A linha vermelha mostra a sua aderência a profissão de cientista de dados."), 
               plotOutput("graficoResultado"))
    ),
)

server <- function(input, output) {

    output$graficoSistema <- renderPlot({ plot(sistema) })
    observeEvent(input$processar, {
        inferencia <- fuzzy_inference(sistema, list(gostaExatas = input$sexatas, 
                                                   relacaoInterpassoal = input$sinter, escreveCodigo = input$scodigo, 
                                                   perfilLider = input$slideranca, gostaEstudar = input$sestudar, 
                                                   habilidadeComunicacao = input$scomunica))
        output$graficoResultado <- renderPlot({ 
            plot(sistema$variables$classificacao)
            lines(inferencia, col="red", lwd=4)
        })
    })
}

shinyApp(ui = ui, server = server)