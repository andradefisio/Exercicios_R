
# Aberto: Friday, 18 Mar 2022, 00:00
# Vencimento: Monday, 21 Mar 2022, 23:59

# Organize os codigos em R que responda cada questao de maneira clara.


library(tidyverse)


# Questao 01 --------------------------------------------------------------

# Construa uma função em R que receba quatro valores inteiros denominados a, b, c e d para realizar o seguinte comportamento.
# Assuma a seguinte expressao artimetica:

#      [ ] * [ ] - [ ] * [ ]

# Em sua função, os espaços em branco da expressao acima (indicados como [ ])
# devem ser preenchidos usando os valores a, b, c e d,
# visando retornar o maior valor possivel calculado pela expressao.
# Por exemplo, se a=1, b=2, c=3, d=4 uma das possibilidades de utilização
# dos valores para preencher os espa?os em branco ?: 1 * 4 - 2 * 3.
# A pergunta que fica, entretanto, é se o resultado da expressao é de fato
# o maior valor possivel que pode ser obtido.

# dado o vetor v <- c(a,b,c,d) a posição dos elementos neste vetor é dada por:
# a = v[1]
# b = v[2]
# c = v[3]
# d = v[4]

# Para que a expressao []*[] - []*[] assuma o maior valor, o que esta a esquerda do
# sinal de menos nesta expressao deve ser maior que do que o que esta a direita, ou as combinações
# [a]*[b]; [a]*[c] e [a]*[d] deve ser maiores que as possibilidades de suas "combinações complementares" ? direita do sinal de "-"

# obs.: lembrando que a ordem do produto n?o altera o resultado.
# Assim, tem-se a "fc_calcula_MaiorValor" (dadas as condições pré-estabelecidas):
#
# OBSERVAÇÃO: QUANDO VI O EMAIL DA NÃO NECESSIDADE DAS VALIDAÇÕES JÁ TINHA FEITO.

fc_calcula_MaiorValor <- function(v){  #corpo da funÇão
        if (is.null(v)){
                stop("Não ha uma sequencia com 4 numeros ou o vetor informado tem tamanho 0")
        }

        if (!is.numeric(v)){
                stop("Voce deve informar uma sequencia apenas com numeros e nao letras ou qq string")
        }

        if (length(v) != 4){
                stop("Nao ha 4 valores informados")
        }

        if (any(is.na(v))){
                stop("Existe NAs em v")
        }

        if ((v[1]*v[2]) > (v[3]*v[4])){
                print(paste0('A combinação dos numeros na mesma sequencia informada p/ preencher a condiçao inicial fornecida []*[]-[]*[], tem como > valor o n.º ', (v[1]*v[2]) - (v[3]*v[4])))

        } else if ((v[1]*v[3]) > (v[2]*v[4])){
                print(paste0('Com a sequencia informada, para obter o maior valor possivel do numero, o 1º nº deve multiplicar o 3º n? (à esquerdo do sinal de menos na condição inical fornecida []*[]-[]*[]), e o 2º multiplica o 4º nº. Assim, o numero é ', (v[1]*v[3]) - (v[2]*v[4])))

        } else {
                print(paste0('Com a sequencia informada, para obter o maior valor possivel do numero, o 1.º n.º deve multiplicar o 4.º nº (à esquerdo do sinal de menos na condição inicial fornecida []*[]-[]*[]), e o 2.º multiplica o 3.º  n.º. Assim, o numero é ', (v[1]*v[4]) - (v[2]*v[3])))

        }
}


# teste da fç:

v <- c(1,2,1,0)
fc_calcula_MaiorValor(v)

v2 <- c(1,1,0,2)
fc_calcula_MaiorValor(v2)

v3 <- c(-1,-2,-3,1) #teste com valores negativos
fc_calcula_MaiorValor(v3)

v4 <- c(4,5,7,8)
fc_calcula_MaiorValor(v4)

v5 <- c() #teste com vetor vazio
fc_calcula_MaiorValor(v5)

v6 <- c("antonio", 1,2,3) #teste com string
fc_calcula_MaiorValor(v6)

v7 <- c(1,2,3) #teste com vetor de comprimento menor que 4
fc_calcula_MaiorValor(v7)

v8 <- c(1,1,2,NA) #teste com NAs
fc_calcula_MaiorValor(v8)


# Questao 02 --------------------------------------------------------------

# As proximas questoes sao referentes ao conjunto de dados fornecido em anexo.
# Ele é oriundo do seguinte link https://www.kaggle.com/gregorut/videogamesales.
# O conjunto de dados refere-se às vendas de jogos de videogames. De maneira breve,
# ele e composto pelas seguintes colunas:

# Rank - Ranking de vendas gerais
# Name - Nome do jogo
# Platform - Plataforma do lançamento do jogo (e.g.. PC, PS4, etc.)
# Year - Ano de lançamento do jogo
# Genre - Genero do jogo
# Publisher - Editora / Produtora do jogo
# NA_Sales - Vendas na America do Norte (em milh?es)
# EU_Sales - Vendas na Europa (em milh?es)
# JP_Sales - Vendas no Japao (em milhoes)
# Other_Sales - Vendas no resto do mundo (em milhoes)
# Global_Sales - Total de vendas mundiais
#
# Para cada questao, forneça o codigo R necessario para recuperar
# o que esta sendo requerido (usando o pacote tidyverse).

df_vg <- read_csv("vgsales.csv")
df_vg
head(df_vg, n=2)

# item a ------------------------------------------------------------------
# Recupere todos os jogos produzidos pela Ubisoft que foram vendidos nos anos entre 2010 a 2019.

df_vg$Year <- as.numeric(as.character(df_vg$Year))  # Convert one variable to numeric
sapply(df_vg, class)

jogos_Ubisoft <- filter(df_vg, str_detect(Publisher, "Ubisoft"), between(Year, 2010, 2019))
jogos_Ubisoft


# item b ------------------------------------------------------------------
## Recupere todos os jogos do gênero de Puzzle, ordenando o resultado em relação ao total de vendas na América do Norte.

genero_Puzzle <- filter(df_vg, str_detect(Genre, "Puzzle")) %>%
        arrange(NA_Sales)

genero_Puzzle


# item c ------------------------------------------------------------------
### Recupere todos os jogos da franquia Street Fighter (existem varias variações desse jogo) lançados a partir de 1998; ordene os jogos de maneira que os mais recentes sejam apresentados primeiro.

street_fighter <- filter(df_vg, str_detect(Name, "Street Fighter")) %>%
        arrange(desc(as.numeric(Year), na.rm = TRUE))

street_fighter


# item d ------------------------------------------------------------------
#### Recupere a quantidade media de vendas no Japao para os jogos da plataforma Xbox 360 (X360) lançados entre 2008 e 2011, inclusive, por genero e ano de lançamento do jogo.

circulacao_xbox360 <- filter(df_vg, str_detect(Platform, "X360"))
circulacao_xbox360_JP <- select(circulacao_xbox360, Platform, Year, Genre, JP_Sales)

circulacao_xbox360_JP

circulacao_xbox360_JP_genero_ano <- group_by(circulacao_xbox360_JP, Genre,Year)
circulacao_xbox360_JP_genero_ano

media_circulacao_xbox360_JP <- summarise(circulacao_xbox360_JP_genero_ano,
                                         media = mean(JP_Sales), na.rm = TRUE)

media_circulacao_xbox360_JP
media_circulacao_xbox360_JP_2008_2011 <- filter(media_circulacao_xbox360_JP, between(Year, 2008, 2011))

media_circulacao_xbox360_JP_2008_2011


# item e ------------------------------------------------------------------
## Mostre em um gr?fico de sua escolha (linha ou coluna),
## a quantidade de jogos lançados em cada plataforma por ano.

games_distincts <- count(df_vg, Platform, Year) %>%
        group_by(Platform)

#group_by(Platform, Year)
games_distincts

jogos_por_plataforma <- games_distincts %>% group_by(Platform)
jogos_por_plataforma


ggplot(data = jogos_por_plataforma) + geom_line(mapping = aes(x = Year, y = n, group = Platform, color = Platform)) +
        labs(x ="Ano", y = "Quantidade de Lan?amentos"
        ) +
        ggtitle("Quanti// de jogos lan?ados em cd plataforma/ano"
        )
