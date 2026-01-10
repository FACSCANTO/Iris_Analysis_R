# 1. Obtenir el data.frame "iris" desde R base (así podemos generar una copia del 
# documento en la carpeta:

write.csv(iris, "iris.csv", row.names = FALSE) # Genera el document en format csv en la carpeta del projecte.

# 2.Exploración y limpieza de datos

## 2.1.Inspección de estructura:

library(readr)
Datos_iris <- read_csv("iris.csv")
View(Datos_iris)
str(Datos_iris) # 150 obs, 5 var (4 numéricas, 1 factor)


## 2.2. Evaluación de calidad de los datos:

is.na.data.frame(Datos_iris) # Mira si hi han NA's en tot el data frame
sum(is.na.data.frame(Datos_iris)) > 0 # Mirar si hi ha algun NA en data frame


# 3.Análisis Estadístico Descriptivo

## Resumen general: Obtén la media, mediana, cuartiles, mínimo y máximo de las 4 variables numéricas.

estadistica_columnas <- summary(Datos_iris[,1:4])

### Sino saps quines columnes son numeriques també es pot fer així:

estadistica_columnas_2 <- summary(Filter(is.numeric, Datos_iris))


## Análisis por grupos (El paso clave): Dado que el objetivo suele ser distinguir especies, no basta con el resumen global.

### Agrupa los datos por la variable Species.

unique(Datos_iris$Species)

estadistica_columnas_setosa <- summary(Datos_iris[Datos_iris$Species == "setosa",1:4])

estadistica_columnas_versicolor <- summary(Datos_iris[Datos_iris$Species == "versicolor",1:4])

estadistica_columnas_virginica <- summary(Datos_iris[Datos_iris$Species == "virginica",1:4])


### Calcula la media y la desviación estándar para cada variable (Sépalo y Pétalo) dentro de cada especie.

# Method 1: with nested bucles:

especies <- unique(Datos_iris$Species)

esp <- c()
col <- c()

medias <- c()
std <- c()


for(i in 1:4){
  for(j in especies){
    
  m <- mean(Datos_iris[Datos_iris$Species == j,i])
  sd <- sd(Datos_iris[Datos_iris$Species == j,i])
  
  esp <- c(esp, j)
  col <- c(col, colnames(Datos_iris[i]))
  
  medias <- c(medias,m)
  std <- c(std, sd)
  
  }
}

res_final <- data.frame(esp, col, medias, std)

res_final

write.csv(res_final, "res_final.csv", row.names = FALSE)
# Create the "csv" document in the working directory

# row.names = FALSE, s'utilitza perque no afegeixi columnes amb números
# al principi.


# Method 2: using tidyverse.

library(tidyverse)

media_long_sepalo <- Datos_iris %>% 
  group_by(Species) %>% 
  summarise(media_long_sepalo = mean(Sepal.Length))

help(across)

Medias_por_especie <- Datos_iris %>% 
  group_by(Species) %>% 
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

print(Medias_por_especie)


sd_por_especie <- Datos_iris %>% 
  group_by(Species) %>% 
  summarise(across(where(is.numeric), \(x) sd(x, na.rm = TRUE)))


print(sd_por_especie)



# 4.Análisis Gráfico Univariante

## Analiza cada variable por separado para ver su distribución.

## Histogramas: Crea un histograma para cada una de las 4 variables numéricas. 

library(ggplot2)

# Histogramas superpuestos

long_sep_sobrepuesto <- ggplot(Datos_iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(
    alpha = 0.5,        # transparencia para ver superposición
    position = "identity",
    bins = 20
  ) +
  labs(
    title = "Histograma de la longitud del sépalo por especie",
    x = "Longitud del sépalo",
    y = "Frecuencia"
  )

print(long_sep_sobrepuesto)


# Histogramas separados (facet_wrap o facet_grid)

long_sep_separado <- ggplot(Datos_iris, aes(x = Sepal.Length)) +
  geom_histogram(
    bins = 20,
    fill = "pink",
    color = "black"
  ) +
  facet_wrap(~ Species) +
  labs(
    title = "Histograma de la longitud del sépalo por especie",
    x = "Longitud del sépalo",
    y = "Frecuencia"
  )

print(long_sep_separado)

<<<<<<< HEAD
## Poner los datos en forma larga para poder hacer histograma de todas las 
## variables y todas las especies con pivot longer

=======

## Poner los datos en forma larga para poder hacer histograma de todas las 
## variables y todas las especies con pivot longer 

pivot_longer
>>>>>>> f40a16c (Copias del documento)







## Diagramas de Caja (Boxplots): Esta es la mejor herramienta para comparar.

# Eje X = Especie, Eje Y = Variable (ej. Longitud del Sépalo).

# Busca outliers (puntos fuera de los bigotes).

# Observa si las cajas (el 50% central de los datos) se solapan o están bien separadas entre especies.

  










