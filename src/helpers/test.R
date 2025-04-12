#' @description: Este archivo contiene las funciones más importantes para validar los supuestos de regresión lineal
#' 

test_batery <- function(modelo){
  
  summary_modelo <- summary(modelo)
  
  print("Testeando Buena especificación")
  espec_test <- resettest(modelo)
  
  if(espec_test$p.value < 0.05){
    print("Revisar la especificación del modelo")
  }else{
    print("El modelo paso el test de especificación")
  }
  print("Testeando normalidad de errores")
  espec_test <- shapiro.test(modelo$residuals)
  
  if(espec_test$p.value > 0.05){
    print("Revisar la normalidad del modelo")
  }else{
    print("El modelo paso el test de normalidad")
  }
  
  
  
  print("Testeando consistencia de errores")
  p_value_F <- summary_modelo$fstatistic[1]
  p_value__F_test <- pf(p_value_F,
                        summary_modelo$fstatistic[2],
                        summary_modelo$fstatistic[3],
                        lower.tail = FALSE)
  
  if(espec_test$p.value < 0.05){
    print("Revisar la consistencia del modelo - prueba F")
  }else{
    print("El modelo paso el test F")
  } 
  
  
  print("Testeando ajuste de modelo")
  r_cuadrado <- summary_modelo$r.squared
  r_cuadrado_adj <- summary_modelo$adj.r.squared
  
  print(paste("R cuadrado ajustado: ", round(r_cuadrado_adj, 3)))
  
  if(r_cuadrado < 0.6){
    print("el valor del R2 es mediocre")
  }
  if(r_cuadrado < 0.8 & r_cuadrado >= 0.6){
    print("el valor de R2 es razonablemente bueno")
  }else{
    print("el modelo tiene buen ajuste con el R2")
  }
  
  print("testeando media de errores cero")
  residuos <- modelo$residuals
  pruebat_pv <- t.test(residuos)$p.value 
  if(pruebat_pv < 0.05){
    print("media de los errores no son cero")
  }else{
    print("la media de los errores es cero")
    print(paste("media: ", mean(residuos) %>% round(2)))
  }
  
  print("testeando heterocedasticidad")
  test_het <- bptest(modelo)
  if(test_het$p.value > 0.5){
    print("el modelo tiene un comportamiento homocedastico")
  }else{
    print("el modelo parece tener un comportamiento heterocedastico")
  }
  
}
