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



# Calcular los intervalos de confianza por país (o región)
intervalos <- felicidad_temporal %>%
  group_by(Country.name) %>%
  summarize(
    media = mean(Life.Ladder, na.rm = TRUE),
    error = qt(0.975, df = n() - 1) * sd(Life.Ladder, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(lower = media - error, upper = media + error)


# Graficar los intervalos de confianza
ggplot(intervalos, aes(x = row_number(), y = Country.name)) +
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

# Crear el pairplot con todas las variables disponibles
#ggpairs(felicidad_temporal, 
#        title = "Pairplot de las variables disponibles",
#        aes(color = factor(region)))  # Puedes cambiar "region" según tu dataset

# Crear el pairplot con todas las variables disponibles (sin la columna de países)
#ggpairs(felicidad_sin_pais, 
#       title = "Pairplot de las variables disponibles",
#        aes(color = factor(region)))

felicidad_sin_pais <- felicidad_temporal %>% select(-Country.name)

# Crear el pairplot sin el color por región
ggpairs(felicidad_sin_pais, 
        title = "Pairplot de las variables disponibles")

colnames(felicidad_temporal)


ggpairs(felicidad_temporal, 
        title = "Pairplot de las variables disponibles",
        aes(color = factor(Region)), 
        cardinality_threshold = 200) 

