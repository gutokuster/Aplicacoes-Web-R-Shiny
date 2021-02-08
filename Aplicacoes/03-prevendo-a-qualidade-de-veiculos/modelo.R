library(e1071) # Pacote com a função Naive Bayes

carros = read.csv(file.choose(), sep = ',',)

# Gerando a amostra
# 2                 => Numeros deverão ser atribuidos com '1' ou '2',
# 1728              => Total de registros do arquivo,
# replace = T       => Utiliza reposição nos números gerados,
# prob = c(0.7,0.3) => Probabilidade de ocorrência 70% para o número '1' (dados de treino) e 30% para o número '2' (dados de teste)
amostra = sample(2, 1728, replace = T, prob = c(0.7,0.3))

carrosTreino = carros[amostra == 1, ]
carrosTeste = carros[amostra == 2, ]

modelo = naiveBayes(class ~ ., carrosTreino)

predicao = predict(modelo, carrosTeste)

confusao = table(carrosTeste$class, predicao)

# A taxa de acerto é definida a partir da matriz confusao e deve-se levar em conta 
# os números da diagonal, neste caso selecionamos os índices 1, 6, 11 e 16, 
# e dividimos pelo total de índices
#
# acc good unacc vgood
# acc    [98]    0     20     0
# good   17     [4]     0     0
# unacc  24      0  [369]     0
# vgood  13      1     0    [11]
# 
taxaacerto = ((confusao[1] + confusao[6] + confusao[11] + confusao[16]) / sum(confusao))