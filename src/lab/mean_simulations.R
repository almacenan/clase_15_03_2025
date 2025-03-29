#' @CasoUnivariado: una variable y su promedio

source("src/helpers/test.R")
       #dnorm(0)
#dnorm(0.5)
#dnorm(n=100)
x <- rnorm(n=100)
plot(density(x))


y <- rexp(100)
plot(density(y))

#Variables aleatorioas

y <- rexp(n=100, rate = 0.5)
mean(y)


#la semilla set.seed(1523) punto de partida de generacion de numeros aleatorios.
set.seed(1523)

medias_exp <- c() #Forma de generar arreglos vacios en R.


for(i in 1:1000){
  
  y <- rexp(n=100, rate = 0.5)
  medias_exp <- c(medias_exp, mean(y))
  
}

plot(density(medias_exp))
plot(density(y))

#' @casoMultivariado : Gneracion de variables de correlacionadas

mu <- c(0, 0)# convatenar con el c(x,y)
#sigma <- matrix(c( 1 , 0.7 , 0.8 , 1 ), byrow= T, ncol=2)
#sigma <- matrix(c( 1 , 1 , 1 , 1 ), byrow= T, ncol=2)
sigma <- matrix(c( 1 , -0.7 , 0.8 , 1 ), byrow= T, ncol=2)

datos_multivariados <- mvrnorm(n=100, mu=mu, Sigma = sigma)

plot(datos_multivariados)


## redificion

mu <- c(0, 0)# convatenar con el c(x,y)

sigma <- matrix(c( 1 , -0.7 , 0.8 , 1 ), byrow= T, ncol=2)

datos_multivariados <- mvrnorm(n=100, mu=mu, Sigma = sigma)
plot(datos_multivariados, pch = 20)


datos_multivariados <- datos_multivariados %>%  data.frame()
names(datos_multivariados) <- c("x","y")
modelo <- lm(data = datos_multivariados, y ~ x)
summary(modelo)
plot(modelo$residuals,pch=20)

plot(density(modelo$residuals))

#plot(datos_multivariados)

resettest(modelo)



resumen <- summary(modelo)
resumen$coefficients
#FUNCION EJECUTANDO
test_batery(modelo)















