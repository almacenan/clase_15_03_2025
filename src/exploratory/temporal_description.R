felicidad_temporal <- felicidad_temporal %>% 
  tibble()

serie_indice <- felicidad_temporal %>% 
  group_by(year) %>% 
  summarise(indice=mean(Life.Ladder))

"Life.Ladder"

plot(serie_indice,
     serie_indice,
     type = "b",
     pch = 20,
     col ="#4c4747"
    )

serie_temporal <- ts(serie_indice$indice, start = c(2005, 2),frequency = 2)
descop = stl(serie_temporal, s.window = 2)
plot(descop)
  

#seriserie_indiceserie_temporal <- ts()
serie <- ts(AirPassengers, start =1927, end =1960)
ejemplo = stl(serie, s.window = 6)
plot(ejemplo)




t.test(felicidad_2021$Ladder.score, mu= 5.3 )
plot(density(felicidad_2021$Ladder.score))


ggplot(felicidad_2021, aes(x= Ladder.score, y= Logged.GDP.per.capita, color=))

### IC y Pairplot

# Calcular los intervalos de confianza por país (o región)
intervalos <- felicidad_temporal %>%
  group_by(Country.name) %>%
  filter(n() > 1) %>%  # Filtrar países con al menos 2 observaciones
  summarize(
    media = mean(Life.Ladder, na.rm = TRUE),
    error = qt(0.975, df = n() - 1) * sd(Life.Ladder, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(lower = media - error, upper = media + error)

# Verificar el resultado
print(intervalos)


# Agregar una columna de índice al data frame de intervalos
intervalos <- intervalos %>%
  mutate(index = row_number())

# Graficar los intervalos de confianza
ggplot(intervalos, aes(x = index, y = Country.name)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "blue") +
  geom_point(aes(x = media), color = "red") +
  labs(title = "Intervalos de Confianza por País", x = "Índice", y = "País") +
  theme_minimal()




# Agrupar por año y calcular el índice de felicidad promedio
serie_indice <- felicidad_temporal %>% 
  group_by(year) %>% 
  summarise(indice = mean(Life.Ladder, na.rm = TRUE))

# Verificar la tabla resultante
print(serie_indice)

# Crear la serie temporal con frecuencia anual
serie_temporal <- ts(serie_indice$indice, start = min(serie_indice$year), frequency = 1)

# Verificar la serie temporal
print(serie_temporal)
frequency(serie_temporal)
length(serie_temporal)

# Crear la serie temporal con frecuencia anual
serie_temporal <- ts(serie_indice$indice, start = min(serie_indice$year), frequency = 1)

# Descomposición estacional usando stl
#descop <- stl(serie_temporal, s.window = "periodic")
  
serie_temporal <- ts(serie_indice$indice, start = c(2005, 1), frequency = 12)  # Mensual

# Graficar la descomposición
plot(descop)

#
#
#

# Instalar el paquete si no lo tienes
if (!require(GGally)) install.packages("GGally")

# Generar el pairplot
library(GGally)


felicidad_sin_pais <- felicidad_temporal %>% select(-Country.name)

# Crear el pairplot sin el color por región
#ggpairs(felicidad_sin_pais, 
#        title = "Pairplot de las variables disponibles")
#colnames(felicidad_temporal)
#ggpairs(felicidad_temporal, 
#        title = "Pairplot de las variables disponibles",
#        #aes(color = factor(region)), 
#        aes(color = factor(Country.name)),
#        cardinality_threshold = 200) 

##
###
###
# Cargar el paquete GGally
if (!require(GGally)) install.packages("GGally")
library(GGally)
library(dplyr)

# Filtrar columnas numéricas y eliminar columnas con más del 50% de NA
felicidad_limpia <- felicidad_temporal %>%
  select(where(is.numeric)) %>%
  select(where(~ sum(is.na(.)) < 0.5 * length(.)))  # Ajuste para el cálculo correcto

# Eliminar filas con NA restantes
felicidad_limpia <- na.omit(felicidad_limpia)

# Crear el pairplot con los datos limpios
ggpairs(felicidad_limpia, 
        title = "Pairplot de las variables disponibles (datos limpios)")



