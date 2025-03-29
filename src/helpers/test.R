# ' @descripcion: Este archivo contiene las funciones mas importantes para 
#validar los supuestos de regresion lineal

test_batery <- function(modelo){
  
  print("Testeando bUena especificacion")
  espec_test <- resettest(modelo)
  if(espec_test$p.value<0.05){
    print("Revisar la especificacion del modelo")
  }else{
    print("El modelo paso el test de especificacion.")
    
  }
  
}