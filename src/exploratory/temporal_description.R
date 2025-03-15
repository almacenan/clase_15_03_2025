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

