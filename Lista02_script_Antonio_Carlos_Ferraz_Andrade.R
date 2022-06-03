
# Lista de Exercicios 2 ---------------------------------------------------

getwd()


library(tidyverse)

# Questao 1 ---------------------------------------------------------------


# 1) As proximas questões são referentes ao conjunto de dados fornecido.
# Ele é oriundo do https://www.kaggle.com/datasets/andrewmvd/board-games (bgg_dataset.csv) 
# e contem dados técnicos de jogos de carta e tabuleiro. De maneira breve, ele é composto pelas seguintes colunas:
# (...)

# Para cada questão, forneça o codigo R necessario para construir e exibir os gráficos requeridos (usando o tidyverse).

# Estava com problema em fazer o subset de colunas. Aparecia a msg "unexpect symbol". 
# Provavelmente devido ao fato do nome das variáveis apresentar espaço entre duas palavras. 
# Para resolver, colocar o nome da variavel entre crases, exemplo: `nome variavel` ou renomeá-la no arquivo fonte,
# em algum editor de texot. Optei pela 2ª opção. "nome_variavel" 
# https://community.rstudio.com/t/unexpected-symbol/87240

 
df_games <- read_csv2("bgg_dataset.csv") # o separador de colunas é o ponto e virgula
colnames(df_games) # nome das variáveis
dim(df_games) # dimensão do df_games


# a) Mostre, em um gráfico de barras, a avalição média dos jogos que contenham o gênero "Abstract Games",
# agrupados por Gênero (Gênero X Avaliação).
# Lembre-se que um jogo pode conter mais de um genero.

df_abstract_games <-
  filter(df_games, str_detect(Domains, "Abstract Games")) %>%
  group_by(Domains) %>%
  summarise(media = mean(Rating_Average, na.rm = TRUE))

df_abstract_games

ggplot(df_abstract_games, aes(x=Domains, y=media)) + 
  geom_bar(stat = "identity", width = 0.6)+
  ggtitle('M?dia de votos para o g?nero "Abstract Games"')+
  labs(x = "", y = "Avalia??o M?dia")+
  coord_flip()


# b) Mostre,em um gráfico de linhas, a quantidade de jogos exclusivamente de estratégia publicados por ano, a partir de 1980.
jogos_estrategia <- filter(df_games, Domains == "Strategy Games", Year_Published >= 1980) %>%
  group_by(Year_Published) %>%
  count(Year_Published, na.rm = TRUE)

jogos_estrategia

ggplot(data = jogos_estrategia) + geom_line(mapping = aes(x = Year_Published, y = n))


# c) Mostre, em um gráfico de pontos, uma relação entre a idade mínima recomendada e o nível médio de complexidade dos jogos (Idade mínima X Nível de Complexidade).
# 2)

conjunto_grafico_c <- select(df_games, Complexity_Average, Min_Age) %>%
  group_by(Min_Age) %>%
  summarise(Nivel_complexidade = mean(Complexity_Average))

grafico_c <- ggplot(data = conjunto_grafico_c) +
  geom_point(mapping = aes(x = Min_Age, y = Nivel_complexidade)) +
  labs(x = 'Idade Mínima', y = 'Nível de Complexidade')

grafico_c

# Questao 2 ---------------------------------------------------------------

# As proximas questoes sao referentes ao conjunto de dados fornecido.
# Ele é oriundo do https://www.kaggle.com/datasets/mirichoi0218/insurance (insurance.csv)
# e contem dados sobre clientes de uma seguradora e seus respectivos planos de seguro.
# De maneira breve, ele é composto pelas seguintes colunas:

# Age - Idade do beneficiário "primário"
# Sex - Sexo do contratante
# BMI - índice de massa corporal (IMC) >> (kg/m^2) ideal 18.5 a 24.9
# Children - Numero de dependentes
# Smoker - Indicador se o cliente é fumante
# Region - Região de moradia nos USA
# Charges - Valor do seguro

# Para cada quest?o, forneça o codigo R necessario para construir e exibir o modelo de regresso linear e as aálises necessarias (usando o tidyverse).

# Considere a relaçao entre a idade dos clientes e os valores de seguro para as seguintes faixas de valores:
# Valores menores que 10.000,00;
# Valores entre 15.000,00 e 30.000,00;
# Valores entre 35.000,00 e 45.000,00.

# Leitura do conjunto de dados:
seguro <- read_csv("insurance.csv")

# existem algumas variaveis que sao do tipo "caracter". Trasformar em "factor"
seguro$sex <- as.factor(seguro$sex)
seguro$smoker <- as.factor(seguro$smoker)
seguro$region <- as.factor(seguro$region)
# resultado
head(seguro)

class(seguro$sex)
class(seguro$smoker)
class(seguro$region)

# Distribuiçao por sexo dos individuos da amostra:
table(seguro$sex) 
# female = 662   male = 676 (Percebe-se que h? uma distribuiçao similar por sexos)

table(seguro$smoker) # com rela??o ? atitude de fumar, a distribuiçã é a seguinte:
# 1064 nao fumam
# 274 fumam  (Ha MAIS Não FUMANTES QUE FUMANTES). A seguir é demonstração da distribuição dos fumantes por sexo

table(seguro$region) # também ha uma distribuição semelhante por regiões:
# northeast northwest southeast southwest 
# 324       325       364       325 


# Estatastica descritiva e visao geral sobre o conjunto de dados (valores minimos, maximos, media, mediana, 1º e 3º quartis)
summary(seguro)

# visualização grafica da distribuição das variaveis "age"; "sex"; "bmi"; "children"; "smoker"; "region"; "charges":

# age (visualizaçao da distribuiçao da variavel numerica para observar a distribuição das idades. Planos de saude com uma populaçao mais idoso subentende-de que o custo do plano sera maior?!)
ggplot(seguro, aes(x = sex, y = age)) +
        geom_boxplot() + theme_light() # nao ha presença de idosos com idades muito avançadas
# Nao há outliers, ou indivduos com idades muito avançadas. A distribuição da idade para homens e mulheres tambem ? semelhante. 


# sex
contagem_sex <- seguro %>% count(sex)
contagem_sex

ggplot(contagem_sex, aes(x=sex, y=n)) +
  geom_bar(stat="identity", fill = "steelblue", width = .3)

# Distribuiçao de fumantes por sexo:
sex_smokers <- seguro %>% count(sex, smoker)
sex_smokers

# percentagem de fumantes entre as mulheres:
mulheres_fumantes <- (115/547) * 100 
print(paste0('A porcentagem de fumantes entre as mulhres ? ', format(round(mulheres_fumantes, 2)),' %'))

homens_fumantes <- (159/517) * 100
print(paste0('A porcentagem de fumantes entre os homens ? ', format(round(homens_fumantes, 2)),' %'))

# A PORCENTAGEM DE HOMENS QUE FUMAM É MAIOR QUE A PORCENTAGEM DE MULHERES QUE FUMAM.  


ggplot(sex_smokers, aes(x=sex, y=n, fill=smoker))+
  geom_bar(stat="identity", width = .4)
# ENTRE OS HOMENS H? UMA PREVAÊNCIA MAIOR DE FUMANTES
# NESTA AMOSTRA, A QUANTIDADE HOMEMS FUMANTES É EM TORNO DE 30% > QUE AS MULHERES FUMANTES 


# bmi
ggplot(seguro, aes(x = sex, y = bmi)) +
  geom_boxplot() + theme_light()
## A DISTRIBUI??O DO BOXPLOT DO "bmi" (INDICE DE MASSA CORPORAL) REVELA A PRESENÇA DE "OUTLIERS", OU IDIVIDUOS 
## COM OBESIDADE SEVERA. 4 indivIduos considerados outliers entre os homens, e 4 entre as mulheres.
limite_minimo_bmi = median(seguro$bmi, na.rm = TRUE) - 3 * mad(seguro$bmi, na.rm = TRUE)
limite_maximo_bmi = median(seguro$bmi, na.rm = TRUE) + 3 * mad(seguro$bmi, na.rm = TRUE)

limite_minimo_bmi
limite_maximo_bmi

# numero de filhos:
summary(seguro$children)
# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 0.000   0.000   1.000   1.095   2.000   5.000 

# a quantidade de filhos varia de 0 a 5 filhos, com a media em torno de 1 filho


# a distribuição por regiões também apresenta-se "equilibrada"

# distribuição dos valores por sexo:
summary(seguro$charges)
# Min.    1st Qu. Median    Mean    3rd Qu.    Max. 
# 1122    4740    9382      13270   16640      63770 
# NA MEDIA, O VALOR DO SEGURO GIRA EM TORNO DE 13000. A MEDIA E A MESMA PARA OS SEXOS. EM TORNO DE 25% DAS MULHERES PAGAM
# UM VALOR ACIMA DE 15000 (3º QUARTIL) E 25% DOS HOMENS PAGAM ACIMA DE 20000 (3º QUARTIL).

ggplot(seguro, aes(x = sex, y = charges)) +
        geom_boxplot() + theme_light()

limite_minimo_charges = median(seguro$charges, na.rm = TRUE) - 3 * mad(seguro$charges, na.rm = TRUE)
limite_maximo_charges = median(seguro$charges, na.rm = TRUE) + 3 * mad(seguro$charges, na.rm = TRUE)

limite_minimo_charges
limite_maximo_charges 


# a) Determine e justifique, por meio da analise do coeficiente de Pearson e pela anaalise grafica, para qual faixa de valores a utilizaçãoo da regressãoo linear é mais eficiente.
# Considere a relação entre a idade dos clientes e os valores de seguro para as seguintes faixas de valores:

# Valores menores que 10.000,00;
df_menor_valor <- filter(seguro, charges <= 10000.00, between(bmi, 11.8082, 48.9918))
summary(df_menor_valor) # valor m?nimo = 1122 e maximo = 9991

ggplot(df_menor_valor, aes(x=age, y=charges, na.rm = TRUE)) +
  geom_point() + geom_smooth()

model_menor_valor <- lm(charges ~ age, df_menor_valor)
print(model_menor_valor)
summary(model_menor_valor)
cor(df_menor_valor$charges, df_menor_valor$age) # Coeficiente de correlção de Pearson = 0.9568336. Este valor do 
# Coeficiente de correlação de pearson indica um correlaço "quase perfeita"


# Valores entre 15.000,00 e 30.000,00;
df_valor_medio <- filter(seguro, between(charges, 15000, 30000), between(bmi, 11.8082, 48.9918))
summary(df_valor_medio) # valor minimo = 15007  e maximo = 29523

ggplot(df_valor_medio, aes(x=age, y=charges, na.rm = TRUE)) +
  geom_point() + geom_smooth()

model_valor_medio <- lm(charges ~ age,df_valor_medio)
print(model_valor_medio)
summary(model_valor_medio)
cor(df_valor_medio$charges, df_valor_medio$age) # Coeficiente de correlação de Pearson = 0.4904874


# Valores entre 35.000,00 e 45.000,00.
df_maior_valor <- filter(seguro, between(charges, 35000, 45000), between(bmi, 11.8082, 48.9918))
summary(df_maior_valor) # valor minimo =  e maximo = 

ggplot(df_maior_valor, aes(x=age, y=charges, na.rm = TRUE)) +
  geom_point() + geom_smooth()

model_maior_valor <- lm(charges ~ age,df_maior_valor)
print(model_maior_valor)
summary(model_maior_valor)
cor(df_maior_valor$charges, df_maior_valor$age) # Coeficiente de correlaço de Pearson = 0.5355416

# RESPOSTA, item a:
# Neste caso especifico, considerando este conjunto de dados e estas restrições de faixas de valores, a regressao linear é mais
# eficiente para a faixa de valores abaixo de 10000.
# Nesta situação, o valor do coeficiente de correlação de pearson é próximo do valor 1 (0.9568336). Isto indicando uma correlação
# "quase perfeita". Alem disso, a inspeção visual do grafico demonstra que os dados estão proximos a uma distribuição 
# linear, alem de baixa variação das observações pela "smooth curve".


# b) Construa o modelo de regressão linear para a faixa de valores determinada no item a, utilizando o algoritmo de holdout validation para definir os conjuntos de treino e teste (utilize 70% como porcentagem de divisão).

faixa_1 <- filter(seguro, charges < 10000 & age != "N/A" & charges != "N/A")  %>% 
  select(age, charges)

prepare_hold_out <-
  function(tbl, training_perc) {
  tbl_mixed <- tbl[sample(1:nrow(tbl)), ]
  nrow <- nrow(tbl_mixed)
  nrow_train <- ceiling(training_perc * nrow)
  data_trn <- tbl_mixed[1:nrow_train, ]
  data_tst <- tbl_mixed[(1+nrow_train):(nrow), ]
  list(training = data_trn, test = data_tst)
}

set.seed(12345)

seguro_mixed <- prepare_hold_out(faixa_1, 0.7)

# treino
seguro_mixed$training

# test
seguro_mixed$test

# modelo de regress?o linear usando o conjunto de treino
linear_model_seguro <- lm(charges ~ age, seguro_mixed$training)

# predição dos valores da variavel de interesse usando o modelo
# usando o conjunto de treino
valores_preditos_seguro <- predict(linear_model_seguro, seguro_mixed$test)
# valores_preditos_seguro

# c) O modelo construído e uma boa representaçao do conjunto de dados? Jusifique por meio das medidas de acuracia (Ex.: p-value) e da predição dos valores do conjunto de teste.

accuracy_measures <- function(predicted, observed) {
  e <- observed - predicted
  mae <- mean(abs(e), na.rm = TRUE)
  mse <- mean(e^2, na.rm =TRUE)
  rmse <- sqrt(mse)
  rss <- sum(e^2)
  tss <- sum((observed - mean(observed)) ^ 2)
  r2 <- 1 - rss/tss
  pe <- e/observed * 100
  mape <- mean(abs(pe), na.rm = TRUE)
  list(MAE = mae, RMSE = rmse, MAPE = mape, R2 = r2)
}

accuracy_measures(valores_preditos_seguro, seguro_mixed$test$charges)

summary(linear_model_seguro)
# Resp.: Pelo valor do R-squared proximo a 1 (0.909), percebe-se que os valores preditos possuem valores 
# proximos aos valores observados, isto é, o modelo foi capaz de predizer os valores os quais são "condizentes" com os 
# valores observados 

# Questao 3 ---------------------------------------------------------------

# Considerando o mesmo conjunto de dados utilizado na quest?o anterior (https://www.kaggle.com/datasets/mirichoi0218/insurance) - insurance.csv.
# Para cada questão, forneça o codigo R necessario para construir e exibir a árvore de decisão e as analises necessarias (usando o tidyverse, rpart e rpart.plot).
library(tidyverse)
seguro <- read_csv("insurance.csv")

seguro$sex <- as.factor(seguro$sex)
seguro$smoker <- as.factor(seguro$smoker)
seguro$region <- as.factor(seguro$region)

library(rpart)
library(rpart.plot)
library(factoextra)

# a) Construa uma arvore de decisao para determinar se um cliente é fumante com base nos outros atributos do conjunto.

# Obs.: Pela analise exploratoria previa, podemos identificar a presença de "OUTLIERS" nas features "charges" e "bmi". 
# Dessa forma, é interessante excluir estes outliers e então normalizar os valores numericos com o intuito de minimizar 
# problemas de escala com a distancia vetorial usada na árvore de decisão do modelo. O objetivoé minimizar o "peso" de determinadas
# features sobre o problema de classificação

# para a feature "charges" o limite de corte do outlier deve ser 31704.46
limite_maximo_charges = median(seguro$charges, na.rm = TRUE) + 3 * mad(seguro$charges, na.rm = TRUE)
limite_maximo_charges 

# para a feature "bmi" o limite de corte superior deve ser de 48.9918
limite_maximo_bmi = median(seguro$bmi, na.rm = TRUE) + 3 * mad(seguro$bmi, na.rm = TRUE)
limite_maximo_bmi

seguro_free_outliers <- filter(seguro, charges <= 31704.46 & bmi <= 48.9918) 

seguro_free_outliers_norm <- mutate(seguro_free_outliers,
                                    charges_norm = (charges - min(charges, na.rm = TRUE)) / (max(charges, na.rm = TRUE) - min(charges, na.rm = TRUE)),
                                    bmi_norm = (bmi - min(bmi, na.rm = TRUE)) / (max(bmi, na.rm = TRUE) - min(bmi, na.rm = TRUE)))

seguro_free_outliers_norm

seguro_normalizado <- select(seguro_free_outliers_norm, age, sex,children,bmi_norm, smoker, region, charges_norm)
seguro_normalizado

# fç para dividir em treino e teste

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

seguro_normalizado_split <- prepare_hold_out(seguro_normalizado, 0.8)
seguro_normalizado_split

# treino
seguro_normalizado_split$training

# teste
seguro_normalizado_split$test

# construçãoo da árvore de decião usando o conjunto de treino
tree_seguro <- rpart(smoker ~ age + sex + children + bmi_norm + region + charges_norm, data = seguro_normalizado_split$training)

# visualização grafica da arvore de decisão
rpart.plot(tree_seguro)


# b) Faça a predição das classes, construa a tabela de confusão e determine o número de falsos positivos e falsos negativos.
# predição das classes de cada observação no conjunto de teste
classes_preditas <- predict(tree_seguro, seguro_normalizado_split$test, type = "class")
classes_preditas

# qt// de elementos de classe em cd vetor:
table(seguro_normalizado_split$test$smoker)


table(classes_preditas)

confusion_matrix <- table(seguro_normalizado_split$test$smoker, classes_preditas)
confusion_matrix

confusion_matrix
#        classes_preditas
#        no    yes
# no     204   6
# yes    5     21

# Resposta:
# Falso positivo ou "Type I error" = 6
# Falso negativo ou "Type II erro" = 5



# Questao 4 ---------------------------------------------------------------
# referencia para o problema de carregar o pacote sf
# https://github.com/r-spatial/sf/issues/1534 reinstalei o package "units" e resolveu o problema. 
# 
# Forneça uma visualização gráfica em forma de um mapa que exiba as cidades do estado de São Paulo,
# utilizando o shapefile fornecido (arquivo .zip das cidades do Brasil - BR_Municipios_2020.zip).

# Obs.: Não consegui instalar o pacote 'SF' no meu notebook. Dessa forma, usei o "rstudio cloud". 
# Foi uma experiência interessante porque de certa forma possibilitou a familiarizaçãoo com esse ambiente do 
# rstudio cloud. Consegui instalar o pacote 'sf' e fazer os graficos dos municipios do estado de SP. 

# instalando o pacote sf
install.packages("sf")

# carregando o pactote sf
library(sf)

getwd()
#"/cloud/project"

st_drivers() # function shows all the drivers that are readable, and which may be written

library(tidyverse)
# ====
# prj <- st_read("BR_Municipios_2020.prj")

# cpg <- st_read("BR_Municipios_2020.cpg")

# shx <- st_read("BR_Municipios_2020.shx")
# ====

dbf <- st_read("BR_Municipios_2020.dbf") # Este arquivo cont?m dados como a "SIGLA_UF", 
# isto é, a sigla da unidade da federação. Dessa forma, apliquei a fç filter para selecionar
# os dados referentes ao estado de São Paulo. 
# Uma vez selecionado, apliquei a fç plot neste objeto com os dados oriundos da filtragem anterior

shp <- st_read("BR_Municipios_2020.shp")


mapa_municipios_SP <- filter(shp, SIGLA_UF == 'SP')

mapa_municipios_MG <- filter(shp, SIGLA_UF == 'MG')
mapa_municipios_RO <- filter(shp, SIGLA_UF == 'RO')
ro <- mapa_municipios_RO$geometry

sp <- mapa_municipios_SP$geometry
mg <- mapa_municipios_MG$geometry
ro <- mapa_muni$geometry

plot(sp)
plot(mg)
plot(ro)
union_sp_mg <- st_union(sp,mg)
intersection_sp_mg <- st_union(sp,mg)
plot(union_sp_mg)


plot(mapa_municipios_SP)
#======================================
estados_br <- st_read('BR_UF_2020.shp')






