#Librerias-----
library(dplyr)
library(ggplot2)
library(ggpubr) #Instalar este paquete
library(writexl)
library(readxl)

#Correr en primer lugar el código que está entre la linea 8 y 30.------
#Experimento A
  Flores = data.frame(iris %>% select(Sepal.Length, Sepal.Width, Species) %>% 
                        filter(Species == "setosa" | Species == "versicolor") %>%
                        group_by(Sepal.Length, Sepal.Width))
  write.csv(Flores, "Experimento_A.csv", row.names = F)
  Fl = read.csv("Experimento_A.csv")
  
  #Se comprueba si hay diferencia de tamaño (largo) de sépalos entre especies
  a = Fl[Fl$Species == "setosa",]$Sepal.Length
  b = Fl[Fl$Species == "versicolor",]$Sepal.Length

#Experimento B
  Flores2 = data.frame(iris %>% select(Petal.Length, Petal.Width, Species) %>% 
                         filter(Species == "setosa" | Species == "virginica") %>%
                         group_by(Petal.Length, Petal.Width))
  write.csv(Flores2, "Experimento_B.csv", row.names = F)
  F2 = read.csv("Experimento_B.csv")
  
  #Se comprueba si hay diferencia de tamaño (largo) de petalos entre especies
  e = F2[F2$Species == "setosa",]$Petal.Length
  f = F2[F2$Species == "virginica",]$Petal.Length
#Correr primero hasta acá.

#Correr la función----------
main = function(){
  print("1. Cargar experimentos", quote = FALSE)
  print("2. Comprobar si existe diferencia signficativa entre los datos de x experimento", quote = FALSE)
  print("3. Correlación de Pearson y Spearman", quote = FALSE)
  print("4. Diagrama de dispersión + linea de regresión lineal", quote = FALSE)
  
  Entrada = as.integer(readline("Seleccione una opción: "))
  
if (Entrada == 1){
  print("1. Experimento_A", quote = F)
  print("2. Experimento_B", quote = F)
  TExp = as.integer(readline("Seleccione un experimento: "))
  
  switch (TExp,
          "1" = Fl,
          "2" = F2)
} else{
  if (Entrada == 2){
    print("Diferencia entre medias", quote = F)
    print("1. Experimento_A", quote = F)
    print("2. Experimento_B", quote = F)
    TExp = as.integer(readline("Seleccione un experimento: "))
    
    if (TExp == 1){
      leerpvalor <- function(pvalor_A){
        cat(sprintf("\nEl valor p es %.2e por lo tanto ", pvalor_A))
        if(pvalor_A>0.05){
          cat(sprintf("no se rechaza la hipótesis nula pues las medias son iguales\n"))
        } else {
          cat(sprintf("se rechaza la hipótesis nula pues las medias no son iguales\n"))  
        }
      }
      
      ttest_A = t.test(a,b)
      pvalor_A = ttest_A$p.value
      
      leerpvalor(pvalor_A)
      
      ggplot(Fl, aes(x = Species, y = Sepal.Length, colour = Species)) +
        geom_boxplot() +
        geom_jitter()
      
    } else if (TExp == 2){
      leerpvalor <- function(pvalor_B){
        cat(sprintf("\nEl valor p es %.2e por lo tanto ",pvalor_B))
        if(pvalor_B>0.05){
          cat(sprintf("no se rechaza la hipótesis nula 'las medias son iguales'\n"))
        } else {
          cat(sprintf("se rechaza la hipótesis nula 'las medias son iguales'\n"))  
        }
      }
      
      ttest_B = t.test(e,f)
      pvalor_B = ttest_B$p.value
      
      leerpvalor(pvalor_B)
      
      ggplot(F2, aes(x = Species, y = Petal.Length, colour = Species)) +
        geom_boxplot() +
        geom_jitter()
    }
  } else if (Entrada == 3){
    print("Correlación", quote = F)
    print("1. Experimento_A", quote = F)
    print("2. Experimento_B", quote = F)
    TExp = as.integer(readline("Seleccione un experimento: "))
    
    if (TExp == 1){
      print("Coeficiente de correlacion de Pearson del experimento A")
      pearson = cor(x = Fl$Sepal.Length, y = Fl$Sepal.Width, method = c("pearson"))
      print(pearson)
      
      print("Coeficiente de correlacion de Spearman del experimento A")
      spearman = cor(x = Fl$Sepal.Length, y = Fl$Sepal.Width, method = c("spearman"))
      print(spearman)
      
    } else if (TExp == 2){
      print("Coeficiente de correlacion de Pearson del experimento A")
      pearson2 = cor(x = F2$Petal.Length, y = F2$Petal.Width, method = c("pearson"))
      print(pearson2)
      
      print("Coeficiente de correlacion de Spearman del experimento B")
      spearman2 = cor(x = F2$Petal.Length, y = F2$Petal.Width, method = c("spearman"))
      print(spearman2)
    }
  } else if(Entrada == 4){
    print("Diagrama de dispersión + linea de regresión lineal", quote = F)
    print("1. Experimento_A", quote = F)
    print("2. Experimento_B", quote = F)
    TExp = as.integer(readline("Seleccione un experimento: "))
    
    if (TExp == 1){
      print("Gráfica del experimento A")
      ggplot(Fl, aes(x = Sepal.Width, y = Sepal.Length, colour = Species)) +
        geom_point() + geom_smooth(method = lm)
      
    } else if(TExp == 2){
      print("Gráfica del experimento B")
      ggplot(F2, aes(x = Petal.Width, y = Petal.Length, colour = Species)) +
        geom_point() + geom_smooth(method = lm)
    }
  }
}
}

#Se debe llamar la función por cada item----
main()
  
