felicidad_2021

ic <- t.test(felicidad_2021$Ladder.score, mu = 5.3)
plot(density(felicidad_2021$Ladder.score))


ggplot(felicidad_2021, aes(x = Ladder.score, y = Logged.GDP.per.capita
                           , color = Regional.indicator))+
  geom_point()+
    scale_color_brewer(palette = "Set3")

ic$conf.int

ic$p.value

fecilicidad_to_pairs <- felicidad_2021 %>% 
  select(Logged.GDP.per.capita,
         Social.support,
         Healthy.life.expectancy,
         Freedom.to.make.life.choices,
         Generosity,
         Perceptions.of.corruption,
         Ladder.score)
ggpairs(fecilicidad_to_pairs)
    
    
  
  #select(-Country.name, 
  #       -Regional.indicator,
  #       -Ladder.score.in.Dystopia,
  #       -Standard.error.of.ladder.score,
  #       -u
  #       )


