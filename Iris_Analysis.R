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

## For 1 variable

media_long_sepalo <- Datos_iris %>% 
  group_by(Species) %>% 
  summarise(media_long_sepalo = mean(Sepal.Length))

print(media_long_sepalo)

## For all numeric variables:

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


# Histogramas separados (facet_wrap o facet_grid):

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


# Hacer una función con el código anterior para poder aplicarlo a las 
# otras variables.

library(tidyverse)
library(rlang)

## Aquí vamos a utilizar funciones del paquete tidyverse dentro de una
## función personalizada, y para eso necesitamos usar el paquete rlang.

hist_todo <- function(data, variable){
  
  ggplot(data, aes(x = {{ variable }})) +
    geom_histogram(
      bins = 20,
      fill = "pink",
      color = "black"
    ) +
    facet_wrap(~ Species) +
    labs(
      title = as_label(enquo(variable)), 
      x = "Longitud en centímetros",
      y = "Frecuencia"
    )
  
}


## Usamos la función "hist_todo" para crear los histogramas de cada variable

colnames(Datos_iris)

longitud_sepalo <- hist_todo(Datos_iris, Sepal.Length)

ggplot2::ggsave(
  filename = paste("longitud_sepalo",".pdf", sep = ""),
  plot = longitud_sepalo,
  device = "pdf"
)

anchura_sepalo <- hist_todo(Datos_iris, Sepal.Width)

ggplot2::ggsave(
  filename = paste("anchura_sepalo",".pdf", sep = ""),
  plot = anchura_sepalo,
  device = "pdf"
)

longitud_petalo <- hist_todo(Datos_iris, Petal.Length)

ggplot2::ggsave(
  filename = paste("longitud_petalo",".pdf", sep = ""),
  plot = longitud_petalo,
  device = "pdf"
)

anchura_petalo <- hist_todo(Datos_iris, Petal.Width)

ggplot2::ggsave(
  filename = paste("anchura_petalo",".pdf", sep = ""),
  plot = anchura_petalo,
  device = "pdf"
)


## Poner los datos en forma larga para poder hacer histograma de todas las 
## variables y todas las especies con pivot longer

Datos_iris_largo <- Datos_iris %>%
  pivot_longer(cols = 1:4, names_to = "measure", values_to = "values")

View(Datos_iris_largo)

todo_long_sep_separado <- ggplot(Datos_iris_largo, aes(x = values)) +
  geom_histogram(
    bins = 20,
    fill = "pink",
    color = "black"
  ) +
  facet_grid(Species ~ measure) +
  labs(
    title = "Histograma de la longitud del sépalo por especie",
    x = "Longitud del sépalo",
    y = "Frecuencia"
  )


ggplot2::ggsave(
  filename = paste("todo_long_sep_separado",".pdf", sep = ""),
  plot = todo_long_sep_separado,
  device = "pdf"
)


## Diagramas de Caja (Boxplots): Esta es la mejor herramienta para comparar.

# Eje X = Especie, Eje Y = Variable (ej. Longitud del Sépalo).

library(tidyverse)

box_plot_todo <- ggplot(Datos_iris_largo, aes(x = Species, y = values)) +
  geom_boxplot(
    fill = "pink",
    color = "black"
  ) +
  facet_wrap(~ measure) +
  labs(
    title = "Boxplot comparando especies",
    x = "Especie",
    y = "centímetros"
  )


ggplot2::ggsave(
  filename = paste("box_plot_todo",".pdf", sep = ""),
  plot = box_plot_todo,
  device = "pdf"
)


# 5. Análisis Gráfico Multivariante

## Scatterplots (Gráficos de dispersión):
## - Cruza dos variables (ej. Petal.Length vs Petal.Width).
## - Importante: Usa el color para diferenciar las especies.

install.packages("ggpubr")

library(tidyverse)
library(ggpubr)

colnames(Datos_iris)

## Cruzar todas las variables con todas las variables manualmente

### Sepal.Length vs Sepal.Width:

Sepal.Length_Sepal.Width <- ggplot(Datos_iris, aes(
  x = Sepal.Length, y = Sepal.Width, colour = factor(Species), 
  shape = factor(Species))) +
  geom_point(
    size = 1.8
  ) +
  labs(
    title = "Scatterplot",
    x = "Sepal.Length",
    y = "Sepal.Width"
  ) + geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(aes(label = ..eq.label..), label.x.npc = "center", 
                        label.y.npc = 1) + stat_cor(aes(label = ..rr.label..), label.x.npc = 0.7, label.y.npc = 0.25)
  # For the last two functions used in this script the libarary ggpubr is required 

print(Sepal.Length_Sepal.Width)

ggplot2::ggsave(
  filename = paste("Sepal.Length_Sepal.Width",".pdf", sep = ""),
  plot = Sepal.Length_Sepal.Width,
  device = "pdf"
)




### Realizar todas las combinaciones de variables automáticamente:

library(tidyverse)
library(ggpubr)

var_numericas <- names(Datos_iris)[1:4]

combinaciones <- combn(var_numericas, 2)


for (i in 1:ncol(combinaciones)) {
  
  var_x <- combinaciones[1, i]
  var_y <- combinaciones[2, i]
  
  # Crear el gráfico
  p <- ggplot(Datos_iris, aes(x = .data[[var_x]], y = .data[[var_y]], 
                              colour = factor(Species), 
                              shape = factor(Species))) +
    geom_point(size = 1.8) +
    labs(
      title = paste("Scatterplot:", var_x, "vs", var_y),
      x = var_x,
      y = var_y
    ) + 
    geom_smooth(method = lm, se = FALSE) +
    # Ecuación de la regresión
    stat_regline_equation(aes(label = ..eq.label..), 
                          label.x.npc = "center", 
                          label.y.npc = 1) + 
    # Coeficiente de correlación R^2
    stat_cor(aes(label = ..rr.label..), 
             label.x.npc = 0.7, 
             label.y.npc = 0.25) +
    theme_minimal()
  
  # Imprimir en consola (opcional)
  print(p)
  
  # 5. Guardar automáticamente en PDF
  nombre_archivo <- paste0(var_x, "_vs_", var_y, ".pdf")
  
  ggsave(
    filename = nombre_archivo,
    plot = p,
    device = "pdf",
    width = 7, 
    height = 5
  )
}
