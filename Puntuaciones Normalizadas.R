tnorm <- function(variable,media,desviacion){
  frecuencias <- table(variable)
  frecuencias1 <- data.frame(frecuencias)
  acumulado <- cumsum(frecuencias1[2])/sum(frecuencias1[2])
  acumulado1 <- data.frame(acumulado)
  zn <- qnorm(acumulado1$Freq)
  escalanueva <- media+(zn*desviacion)
  ptransformado <- round(escalanueva,0)
  ptransformado <- data.frame(ptransformado)
  frecuencias1 <- data.frame(frecuencias1,zn,ptransformado)
  zn1 <- qnorm(tail(acumulado1$Freq,1)-0.01)
  print(frecuencias1)
  print(paste("Los puntajes transformados que le corresponde al último puntaje son: zn:",zn1,"y PT:",round(media+(zn1*desviacion),0)))
}
  
