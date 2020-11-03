
source("http://www.openintro.org/stat/data/cdc.R")
head(cdc)

# Resumen
summary(cdc)


library(tidyverse)

# Visualizacion de distribuciones datos categoricos:

## genhlth

ggplot(data = cdc) +
  geom_bar(mapping = aes(x = genhlth))


cdc %>% count(genhlth)


## gender
ggplot(data = cdc) +
  geom_bar(mapping = aes(x = gender))




cdc %>% count(gender)


# Visualizacion de distribuciones datos continuos:

## Estatura

ggplot(data = cdc) +
  geom_histogram(mapping = aes(x = height), binwidth = 0.5)


cdc %>% 
  count(cut_width(height , 10))

# Solo hay una persona que tiene una estatura entre 85 y 95 pulgadas.  ¿Cuanto es eso en centimetros?


# 85 pulgadas a cm
85 * 2.54

# 95 pulgadas a cm
95 * 2.54


## El mayor grupo de personas miden entre 65 y 75 pulgadas. Es decir: 

65 * 2.54
75 * 2.54



# ¿Cual es la distribucion del peso?

ggplot(data = cdc) +
  geom_histogram(mapping = aes(x = weight), binwidth = 3)

Hay algunas personas bastante subidas de peso. Igual, hay muchas personas subidas de peso.

## Intervalos de pesos

cdc %>% 
  count(cut_width(weight , 40))

## La mayoria de personas estan pesando entre 100 libras y 220 libras.



# Edad

ggplot(data = cdc) +
  geom_histogram(mapping = aes(x = age), binwidth = 0.5)

## La gran mayoria de personas encuestadas son menores de 60 años.


# Covarianza

## ¿Afecta el peso la calidad de salud?

ggplot(data = cdc, mapping = aes(x = weight)) + 
  geom_freqpoly(mapping = aes(colour = genhlth), binwidth = 10)

## En efecto, a medida que aumenta el peso la calidad calidad de salud disminuye.


## Relación entre peso y estatura. Detectemos valores atipicos en esta relación.

ggplot(data = cdc, mapping = aes(x = weight, y = height)) + 
  geom_point()

## Hay un par de personas que no son tan altas pero tienen el casi 500 libras. Si esto es verdad, es gravisimo, hay ver que si no es un error inputado.


# Relacion peso con nivel de salud

ggplot(data = cdc, mapping = aes(x = genhlth, y = weight)) +
  geom_boxplot()

## Las personas con menor calidad de salud son las de mayor peso. hay un gran grupo que están super alejados del promedio.


# Peso x genero

ggplot(data = cdc, mapping = aes(x = gender, y = weight)) +
  geom_boxplot()


# ¿Son mas saludables las mujeres o los hombres?

ggplot(data = cdc) +
  geom_count(mapping = aes(x = gender, y = genhlth))

## Llama la atención la esfera de la categoria [poor] en ambos generos, parece que hay mas mujeres que caen en este grupo. El resto son bastante similares.

# contemos:


cdc %>% 
  count(gender, genhlth)

## En efecto, hay 394 mujeres y 283 hombres en la categoria poor.


cdc %>% 
  count(gender, genhlth) %>%  
  ggplot(mapping = aes(x = gender, y = genhlth)) +
    geom_tile(mapping = aes(fill = n))

## La gran mayoria de personas están saludables. El color claro es mas saludable.



# ¿Que tan dispersos estan los pesos de los pesos deseados?

ggplot(data = cdc) +
  geom_point(mapping = aes(x = weight, y = wtdesire))

## Hay cuatro observaciones que estan super alejados. 


ggplot(data = cdc, mapping = aes(x = weight, y = wtdesire)) + 
  geom_boxplot(mapping = aes(group = cut_width(weight, 0.5)))



barplot(table(cdc$smoke100))



hist(cdc$age)


# Estimador para la EDAD promedio

edades <- cdc$age
N <- length(edades)
n <- 2000

muestra_edades <- sample(N, n, replace = FALSE)

estimador_edad_promedio <- sum(edades[muestra_edades])/n
estimador_edad_promedio


## Intervalo de confianza para el ESTIMADOR DE LA EDAD 

alfa <- 0.05
z <- qnorm(1 - alfa/2)
p <- estimador_edad_promedio
inf <- p - z*sqrt(abs(p*(1-p)/n))
sup <- p + z*sqrt(abs(p*(1-p)/n))

paste(inf, sup)



# Estimador para el PESO promedio

pesos <- cdc$weight 
N <- length(pesos)
n <- 2000

muestra_pesos <- sample(N, n, replace = FALSE)

estimador_peso_promedio <- sum(pesos[muestra_pesos])/n
estimador_peso_promedio


## Intervalo de confianza para el ESTIMADOR DE PESOS

alfa <- 0.05
z <- qnorm(1 - alfa/2)
p <- estimador_peso_promedio
inf <- p - z*sqrt(abs(p*(1-p)/n))
sup <- p + z*sqrt(abs(p*(1-p)/n))

paste(inf, sup)



# Estimador para la ESTATURA promedio

estaturas <- cdc$height 
N <- length(estaturas)
n <- 2000

muestra_estaturas <- sample(N, n, replace = FALSE)

estimador_estatura_promedio <- sum(estaturas[muestra_estaturas])/n
estimador_estatura_promedio


## Intervalo de confianza para el ESTIMADOR DE ESTATURA

alfa <- 0.05
z <- qnorm(1 - alfa/2)
p <- estimador_estatura_promedio
inf <- p - z*sqrt(abs(p*(1-p)/n))
sup <- p + z*sqrt(abs(p*(1-p)/n))

paste(inf, sup)


# proporción de MUJERES

mujeres <- which(cdc$gender == "f")
n <- length(cdc$gender)
p <- length(mujeres) / n
p


# Intervalo de confianza para la proporción de MUJERES

paste( p - 1.96 * sqrt((p*(1-p)/n)) , p + 1.96 * sqrt((p*(1-p)/n)) )


# proporción de Smoke100

fumadores <- which(cdc$smoke100 == 1)
n <- length(cdc$smoke100)
p <- length(fumadores) / n
p


# Intervalo de confianza para la proporción de Smoke100

paste( p - 1.96 * sqrt((p*(1-p)/n)) , p + 1.96 * sqrt((p*(1-p)/n)) )




# 4

cars93 <- read_tsv("cars93.txt")
head(cars93)

# 4.a Calcule un MAS de tamanio 30 y encuentre las millas promedio por galon (MPGCITY). D ́e una cota del error al 95% de confianza.

set.seed(2020)
millas_promedio_por_galon <- cars93$MPGCITY
N <- length(millas_promedio_por_galon)
n <- 30

muestra_aleatoria_mpg <- sample(N, n, replace = FALSE)

estimador_millas_promedio_galon <- sum( millas_promedio_por_galon[ muestra_aleatoria_mpg ] ) / n
estimador_millas_promedio_galon


# Cota Millas por galon

alfa <- 0.05
z <- qnorm(1 - alfa/2)
p <- estimador_millas_promedio_galon
inf <- p - z*sqrt(abs(p*(1-p)/n))
sup <- p + z*sqrt(abs(p*(1-p)/n))

paste(inf, sup)


# Estime la proporcion de carros con al menos un airbag (AIRBAGS) y de una cota del error al 95% de confianza.

autos_con_airbag <- which(cars93$AIRBAGS >= 1) # Al menos 1 airbag
n <- length(cars93$AIRBAGS)
p <- length(autos_con_airbag) / n
p



paste( p - 1.96 * sqrt((p*(1-p)/n)) , p + 1.96 * sqrt((p*(1-p)/n)) )


# Estratificación

library(dplyr)

strat_sample <- cars93 %>%
                  group_by(cars93$TYPE)%>%
                  sample_n(size=30, replace = T)

table(strat_sample$TYPE)


# MPG promedio desde los datos estratificados

set.seed(2020)
millas_promedio_por_galon_estratificado <- strat_sample$MPGCITY
N <- length(millas_promedio_por_galon_estratificado)
n <- 30

muestra_aleatoria_mpg_estratificado <- sample(N, n, replace = FALSE)

estimador_millas_promedio_galon_estratificado <- sum( millas_promedio_por_galon_estratificado[ muestra_aleatoria_mpg_estratificado ] ) / n
estimador_millas_promedio_galon_estratificado


# Proporción

autos_con_airbag_estratificado <- which(strat_sample$AIRBAGS >= 1) # Al menos 1 airbag
n <- length(strat_sample$AIRBAGS)
p <- length(autos_con_airbag_estratificado) / n
p



paste( p - 1.96 * sqrt((p*(1-p)/n)) , p + 1.96 * sqrt((p*(1-p)/n)) )


# Comparacion MAS vs Estratificado
### MPG promedio con MAS:           20.9
### MPG promedio con Estratificado: 21.23333


### Proporción AIRBAGS con MAS:           0.6413043
### Proporción AIRBAGS con Estratificado: 0.65

### Los resultados son muy parecidos y bastantes cercanos. Las proporciones son más cercanas y distan por 0.01 a diferencia de los promedios que distan por 0.3 unidades.




