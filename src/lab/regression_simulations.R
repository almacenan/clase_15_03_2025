
#Modelos y sus propiedades ------------------------

beta_0 = 10

beta_1 = 5

beta_2 = 8

beta_3 = 10

x1 <- rnorm(n= 100, mean= 150, sd =70)# Los metros cuadrados

#x2 valorizacoin del secto  

x2 <- runif(n=100,0,10)

x3 <- runif(n=100,0,10)

error <- rnorm(n=100,0,10)
prec_sim <- beta_0 + beta_1*x1 + beta_2*x2 + beta_3*x3 + error
  
cor(prec_sim, x2)

plot(prec_sim,x1)
plot(prec_sim,x2)
plot(prec_sim,x3)
modelo <- lm(prec_sim ~x1+x2+x3)
summary(modelo)
test_batery(modelo)

#Modelo bien especificado
error <- rnorm(n=100,0,10)
prec_sim <- beta_0 + beta_1*x1 + beta_2*x2 + beta_3*x3 + error
modelo <- lm(prec_sim ~x1+x2+x3)
summary(modelo)
test_batery(modelo)

#Modelo con varianza grande

 error1 <- rnorm(n=100, 25,70)
 prec_sim1 <- beta_0 + beta_1*x1 + beta_2*x2 + beta_3*x3 + error1
 modelo1 <- lm(prec_sim1~ x1+x2+x3)
 summary(modelo1)
 test_batery(modelo1)
 
 #En este caso el intercepto del modelo captura esa variablilidad pero no la hace significativa
 error2 <- 10*rchis(n=100,2)
 prec_sim2 <- beta_0 +beta_1*x1 +beta_2*x2 +beta_3*x3 + error2
 modelo2 <-  lm(prec_sim2 -x1+x2+x3)
 summary(modelo2)
 test_batery(modelo2)
 
 
 # En este caso el intercepto del modelo captura esa variabilidad pero no la hace significativa
 
 error3 <- rnorm(100, 0)*seq(from = 1, to = 500, length.out = 100)
 
 plot(density(error3)) # Ponemos un error con una distribucion no normal
 prec_sim3 <- beta_0 + beta_1*x1 + beta_2*x2 + beta_3*x3 + error3
 
 modelo3 <- lm(prec_sim3 ~ x1 + x2 + x3)
 summary(modelo3)
 test_batery(modelo3)
 
 
 # de tarea  VAN HACER MAS PRUEBAS DE ESTO REVENTANDO OTRAS COSAS, INTENTAR PARAMETROS PARA DEAÑAR