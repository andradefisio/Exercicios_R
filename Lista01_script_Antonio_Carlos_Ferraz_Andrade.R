
# Lista01 -----------------------------------------------------------------

library("tidyverse")
options(encoding="utf-8")
options(scipen = 9999)


# QUESTÃO 01 --------------------------------------------------------------

# Dado o encunciado, tem-se que:
# o ponto A pode ser visto como A(x1[1], y1[1]) e o ponto B como B(x2[1], y2[1]))
# pontoA <- c(x1,y1)
# pontoB <- c(x2,y2)
# pela fórmula (Pitágoras) a distância "d" ? dada como sendo uma funçãoo de x e y

# nome da função: fc_distancia_euclid

fc_distancia_euclid <- function(pontoA, pontoB){
        d <- sqrt(sum((pontoA - pontoB)^2))
        print(paste0("O valor da dist?ncia euclidiana ? ", d))
}

#test
pontoA <- c(75,5)
pontoB <- c(9,20)

# usando os pontos definidos na célula de código anterior, tem-se:
fc_distancia_euclid(pontoA,pontoB)

#"O valor da distância euclidiana é 67.6830850360709"



# Questãoo 02 --------------------------------------------------------------

# Pelo enunciado da questão, temos que determinar inicialmente o valor monetário.
# valor_monetario <- atribuir um valor inteiro.    Se o valor monetàrio for != inteiro >> warning message
# valor_monetario <- ...


fc_numero_notas <- function(saque){
        is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
        
        if(!is.wholenumber(saque)){
                stop('!!! >>>> O valor do saque n?o ? inteiro <<<< !!!')
        }
        
        cedula <- c(100,50,10,5,2,1)
        contador = 1
        num_notas <- rep(0,6)
        
        for (ced in cedula){
                var_auxiliar <- saque %/% ced
                if (var_auxiliar > 0){
                        num_notas[contador] <- var_auxiliar 
                        saque <- saque - var_auxiliar*ced
                } 
                contador = contador + 1
        }
        list(' N.? notas de 100' = num_notas[1],' N.? notas de 50' = num_notas[2],' N.º notas de 10' = num_notas[3],' N.? notas de 5' = num_notas[4],' N.? notas de 2' = num_notas[5],' N.? moedas de 1' = num_notas[6])
}

# test:
fc_numero_notas(99)




# Questão 05 --------------------------------------------------------------

# fazendo a leitura do conjunto de dados "imdb_top_1000.csv"
filmes_programas <- read_csv("imdb_top_1000.csv")

filmes_programas

df <- filmes_programas

names(df)
dim(df)


## a)
Filmes_Progrmas_drama <- filter(df, str_detect(Genre,"Drama"))
Filmes_Progrmas_drama

Filmes_Progrmas_drama[ ,6] #verificando a coluna 6 (presença de "Drama na totalidade das rows")


## b)
df2 <- select(df, "Released_Year", "Star1","Star2","Star3","Star4") # fazendo um subset e usando a f? "select()" para "recortar" colunas espec?fica e de interesse no df

# selecionando os filmes de "Morgan Freeman"
filmes_Morgan_Freeman <- filter(df2, str_detect(Star1, "Morgan Freeman") | str_detect(Star2, "Morgan Freeman") | str_detect(Star3,"Morgan Freeman") | str_detect(Star4, "Morgan Freeman"))
filmes_Morgan_Freeman

arrange(filmes_Morgan_Freeman, as.numeric(Released_Year)) # com esta fç arranjamos a ordem de lançamento dos filmes. 


## c)
filmes_diretor_Christhopher_Nolan <- filter(df, str_detect(Director,"Christopher Nolan"))
filmes_diretor_Christhopher_Nolan[ ,7:10] # verificando o IMBD_Rating e o Director

# para encontrar a média do IMBD_Rating, usamos a fç "summarise()"

media_IMBD_ChristhopherNolan <- summarise(filmes_diretor_Christhopher_Nolan, media = mean(IMDB_Rating, na.rm = TRUE))
media_IMBD_ChristhopherNolan
print(paste0('A m?dia do IMBD_Rating do diretor Christhopher Nolan ? ', media_IMBD_ChristhoherNolan))


## d)
filmes_BradPitt <- filter(df, str_detect(Star1,"Brad Pitt") | str_detect(Star2,"Brad Pitt") | str_detect(Star3,"Brad Pitt") | str_detect(Star4,"Brad Pitt"))
head(arrange(filmes_BradPitt, desc(IMDB_Rating)), n = 5)


## e)
filmes_acao <- filter(df, str_detect(Genre,'Action'))
num_medio_votos <- summarise(filmes_acao, mean(No_of_Votes), na.rm = TRUE )
num_medio_votos

# A tibble: 1 x 2
#`mean(No_of_Votes)` na.rm
#       <dbl> <lgl>
#1     404172. TRUE 


## f) 
comedy_drama <- filter(df, Genre == "Comedy" | Genre == "Drama" | Genre == "Comedy, Drama")


por_genero_ano <- comedy_drama %>% 
        group_by(Genre, Released_Year) 

por_genero_ano_sum <- por_genero_ano %>% 
        summarise(n=n())# %>% 
        #mutate(por_genero_ano, n = n())
por_genero_ano_sum

por_genero_ano_sum$Released_Year <- as.numeric(por_genero_ano_sum$Released_Year)

                           #======================================#

grafico_comedy_drama_linha <- ggplot(data = por_genero_ano_sum) +
        geom_line(mapping = aes(x = Released_Year, y = n, group = Genre, colour = Genre)) +
        scale_y_continuous(n.breaks = 8
        ) +
        labs(x ="Ano", y = "Quantidade de Lan?amento"
        ) + 
        theme(axis.title = element_text(size= 10), plot.title = element_text(size=12, face="bold"
        )) +
        ggtitle("Quantidade de filmes de Drama e Com?dia que foram lan?ados por ano"
        ) 

grafico_comedy_drama_linha


## g) 
dados_star_wars <- df %>%
        filter(grepl("Star Wars", Series_Title))


grafico_star_wars <- ggplot(data = dados_star_wars) + geom_bar(stat = "identity", position = position_dodge(), mapping = aes(x = as.factor(Released_Year), y = No_of_Votes, fill = Series_Title)) +
        scale_y_continuous(n.breaks = 5) +
        labs(x = "Ano", y = "N? votos recebidos") +
        theme(axis.title = element_text(size= 10), plot.title = element_text(size= 12, face = "bold")) +
        ggtitle("N? de votos que cada filme da franquia Star Wars recebeu")

grafico_star_wars











