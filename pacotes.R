# Erro durante a instalação do pacote 'forecast', resolvido com...
# sudo apt-get install libcurl4-openssl-dev libxml2-dev

# Pacotes + dependências necessários para a execução das aplicações
install.packages(c("shiny","sets","e1071","forecast","ggplot2","arules","arulesViz",
"xts", "RColorBrewer","GA","shinythemes","RJSONIO", "PKI", "rstudioapi", "packrat", "rsconnect"), dependencies = T)

# Instalação do RSconnect para publicação em https://www.shinyapps.io/
install.package('rsconnect')
