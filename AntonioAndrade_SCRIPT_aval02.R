
# Análise de vinhos
# 
getwd()


library(tidyverse)
library(rpart)
library(rpart.plot)
library(factoextra)


# Para resolver os exercícios da Avaliação 2, utilize o conjunto de dados disponibilizado.
# Ele armazena a classificação da qualidade de vinhos e foi obtido 
# em https://www.kaggle.com/datasets/nareshbhat/wine-quality-binary-classification.
# De maneira breve, suas colunas são:
# 
# 1 - fixed acidity
# 2 - volatile acidity
# 3 - citric acid
# 4 - residual sugar
# 5 - chlorides
# 6 - free sulfur dioxide
# 7 - total sulfur dioxide
# 8 - density
# 9 - pH
# 10 - sulphates
# 11 - alcohol
# 12 - quality ('good' ou 'bad', sendo essa coluna a que determina a qualidade do vinho com base nos outros atributos obtidos por sensores)
# 
# Note que as colunas 1 até a 11, são atributos numéricos e contínuos, enquanto a coluna 12 é uma coluna categória com um domínio de 2 valores.


# # # # QUESTÃO 1 --------------------------------------------------------------
# (valendo 3 pontos) Considerando o conjunto de dados da prova, 
# identifique 2 atributos numéricos que estejam linearmente correlacionados. 
# Justifique sua resposta usando pelo menos 2 meios diferentes para mostrar a correlação.

vinhos <- read_csv("wine.csv") #leitura do conjunto de dados

# Conforme especificado, há 11 atributos numéricos e contínuos. 
# Dessa forma, uma maneira de observar a correlação entre os atributos é através da observação 
# gráfica das combinações das possiveis correlações, com destaque para aquelas que apresentem uma 
# relação linear entre os atributos.
# 
# Para isso, seria interessante usar a função "pairs()", lembrando que o último atributo é um atributo categórico
# que classfica o vinho como "bom" ou "ruim". 

colnames(vinhos)
pairs(~`fixed acidity` + `volatile acidity` + `citric acid` + `residual sugar` + `chlorides` + `free sulfur dioxide` +
              `total sulfur dioxide` + `density` + `pH` + `sulphates` + `alcohol`, data = vinhos)

library(corrplot)

vinhos_var_num <- select(vinhos, `fixed acidity`, `volatile acidity`, `citric acid`, `residual sugar`, `chlorides`, `free sulfur dioxide`,
                                 `total sulfur dioxide`, `density`, `pH`, `sulphates`, `alcohol`)

vinhos_coef_correlacao <- cor(vinhos_var_num)
corrplot(vinhos_coef_correlacao, method = "number")

# Resposta:
# 
# Pela visualização dos gráficos obtidos pela função "pairs()" observa-se uma tendência à correlação linear
# entre o atributo "fixed-acidity" e o "pH". Por sinal, talvez fosse esperado por que é sabido desta relação química 
# entre o pH e a acidez (menor pH -> > acidez)
#  
# Além disso, pela matrix de correlação obtida pela função "corrplot", é obtido o valor de 0.68 para o 
# coeficiente de correlação de pearson, sendo um dos maiores valores, senão o maior. 


library(PerformanceAnalytics)
chart.Correlation(vinhos_coef_correlacao, histogram = TRUE, pch = 19)

# # # # Questão 2 --------------------------------------------------------------

# (valendo 3 pontos) Forneça uma visualização gráfica de sua escolha (e.g., gráfico de barra ou linha) que mostre visualmente 3 atributos do conjunto de dados da prova.
# Dessa forma, sua resposta deve responder as seguintes perguntas:
#         
# Qual é o objetivo da visualização?
# Resp.: Observar a correlação entre as variáveis, ou seja, o quanto elas apresentam um tendência de 
# estarem concentradas e torno de uma linha e apresentar uma relação entre ambas, ou uma variavel (resposta)
# ser função de outra (preditora). Essa correlação pode ser positiva [aumenta uma variável e a var resposta também aumenta (r>0)], 
# ou aumenta uma variável e a outra variavel resposta diminui (r<0)).
        
# Quais são os atributos escolhidos para isso?
# Resp.: Os atributos escolhidos são o "pH", "fixed-acidity" e a "quality" 


# Qual é o código necessário para realizar a visualização desejada?

class(vinhos$`quality`)
as.factor(vinhos$`quality`)

ggplot(data = vinhos) + 
        geom_line(aes(x = `fixed acidity`, y = `pH`, #lembre-se, x e y são obrigatórios!
                      group = `quality`, #no nosso caso, queremos que cada linha represente c
                      colour = `quality`))         

# Que tipo de conclusão pode-se tirar a partir da visualização?
# Resp.: Há uma correlação linear entre estas variáveis, e o pH do vinho ou a sua acidez 
# pode ser um fator que vá influenciar direta ou indiretamente a sua qualidade talvez. 

# # # # Questão 3 --------------------------------------------------------------

# (valendo 4 pontos) Elabore um programa que efetua uma classificação binária usando árvore de decisão.
# Sua árvore de decisão deve retornar se um vinho é "good" ou "bad" considerando os valores dos outros atributos do conjunto de dados
# (i.e., colunas 1 até a 11). Utilizando o método de validação holdout com 80% dos dados para treino e 20% dos dados para teste,
# responda de maneira sucinta as seguintes perguntas:
#         
# Qual é o código necessário para avaliar o desempenho da árvore de decisão usando o holdout?

# Normalizando os dados:

vinhos_norm <- mutate(vinhos,
                     fixed_acidity_norm = (`fixed acidity` - min(`fixed acidity`, na.rm = TRUE)) / (max(`fixed acidity`, na.rm = TRUE) - min(`fixed acidity`, na.rm = TRUE)),
                     volatile_acidity_norm = (`volatile acidity` - min(`volatile acidity`, na.rm = TRUE)) / (max(`volatile acidity`, na.rm = TRUE) - min(`volatile acidity`, na.rm = TRUE)),
                     citric_acid_norm = (`citric acid` - min(`citric acid`, na.rm = TRUE)) / (max(`citric acid`, na.rm = TRUE) - min(`citric acid`, na.rm = TRUE)),
                     residual_sugar_norm = (`residual sugar` - min(`residual sugar`, na.rm = TRUE)) / (max(`residual sugar`, na.rm = TRUE) - min(`residual sugar`, na.rm = TRUE)),
                     chlorides_norm = (`chlorides` - min(`chlorides`, na.rm = TRUE)) / (max(`chlorides`, na.rm = TRUE) - min(`chlorides`, na.rm = TRUE)),
                     free_sulfur_dioxide_norm = (`free sulfur dioxide` - min(`free sulfur dioxide`, na.rm = TRUE)) / (max(`free sulfur dioxide`, na.rm = TRUE) - min(`free sulfur dioxide`, na.rm = TRUE)),
                     total_sulfur_dioxide_norm = (`total sulfur dioxide` - min(`total sulfur dioxide`, na.rm = TRUE)) / (max(`total sulfur dioxide`, na.rm = TRUE) - min(`total sulfur dioxide`, na.rm = TRUE)),
                     density_norm = (`density` - min(`density`, na.rm = TRUE)) / (max(`density`, na.rm = TRUE) - min(`density`, na.rm = TRUE)),
                     pH_norm = (`pH` - min(`pH`, na.rm = TRUE)) / (max(`pH`, na.rm = TRUE) - min(`pH`, na.rm = TRUE)),
                     sulphates_norm = (`sulphates` - min(`sulphates`, na.rm = TRUE)) / (max(`sulphates`, na.rm = TRUE) - min(`sulphates`, na.rm = TRUE)),
                     alcohol_norm = (`alcohol` - min(`alcohol`, na.rm = TRUE)) / (max(`alcohol`, na.rm = TRUE) - min(`alcohol`, na.rm = TRUE))
) %>%
        select(fixed_acidity_norm,volatile_acidity_norm,
               citric_acid_norm,residual_sugar_norm,
               residual_sugar_norm, chlorides_norm,
               free_sulfur_dioxide_norm, total_sulfur_dioxide_norm,
               density_norm, pH_norm,sulphates_norm, alcohol_norm, `quality`)


# fç para dividir o conjunto de dados em treino e teste

prepare_hold_out <-
        function(tbl, training_perc) {
                # misturando as observações
                tbl_mixed <- tbl[sample(1:nrow(tbl)), ]
                nrow <- nrow(tbl_mixed)
                # de acordo com um valor de porcentagem, separamos o conjunto de dados em treino e teste
                nrow_train <- ceiling(training_perc * nrow)
                data_trn <- tbl_mixed[1:nrow_train, ]
                data_tst <- tbl_mixed[(1+nrow_train):(nrow), ]
                # retorna como uma lista nomeada
                list(training = data_trn, test = data_tst)
        }

set.seed(753)

vinhos_norm_split <- prepare_hold_out(vinhos_norm, 0.8)
vinhos_norm_split

# treino
vinhos_norm_split$training

# teste
vinhos_norm_split$test

# construção da árvore de decisão usando o conjunto de treino
tree_vinhos <- rpart(`quality` ~ fixed_acidity_norm +
                     volatile_acidity_norm +
                     citric_acid_norm + 
                     residual_sugar_norm +
                     chlorides_norm +
                     free_sulfur_dioxide_norm +
                     total_sulfur_dioxide_norm +
                     density_norm +
                     pH_norm +
                     sulphates_norm +
                     alcohol_norm,
                     data = vinhos_norm_split$training)


# Qual é a ilustração gráfica da árvore obtida?
# visualização grafica da árvore de decisão
rpart.plot(tree_vinhos)

       
# Qual foi o atributo escolhido para ser o nó raiz dessa árvore?
# Foi o atributo "alcohol" ou a quantidade de álcool do vinho
        
# Qual foi a matriz de confusão obtida? Ademais, identifique:
# predição das classes de cada observação no conjunto de teste
classes_preditas <- predict(tree_vinhos, vinhos_norm_split$test, type = "class")
classes_preditas

# qt// de elementos de classe em cd vetor:
table(vinhos_norm_split$test$`quality`)

table(classes_preditas)

confusion_matrix <- table(vinhos_norm_split$test$`quality`, classes_preditas)
confusion_matrix

# O número de classificações corretas para "good"
# Resp = 102

# O número de classificações corretas para "bad"
# Resp = 122

# O número de falsos positivos
# Resp = 34

# O número de falsos negativos
# Resp = 61