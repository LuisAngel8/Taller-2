library(gapminder)
library(writexl)
library(readxl)
library(ggplot2)
library(dplyr)

#El programa debe ser ejecutado primero para que la función sea almacenada.
main <- function(){
  cat("1. Guardar gapminder \n")
  cat("2. Leer archivo gapminder \n")
  cat("3. Desplegar diagrama de distribución (lifeExp vs pop) \n")
  cat("4. Desplegar diagrama de distribución (gdpPercap vs pop) \n")
  cat("5. Desplegar diagrama de cajas de gdpPercap por continente desde 1990 a 2007 \n\n")
  cat("Presione cualquier otra número para SALIR")
  val <- as.integer(readline("Ingrese un valor: "))
  
  if(val == 1) {
    
    for (i in c("lifeExp", "pop", "gdpPercap")) {
      ran <- sample(1:length(gapminder$lifeExp), size = length(gapminder$lifeExp) * 0.1, replace = FALSE)
      gapminder[ran,i] <- NA
    }
    #Save file gapminder with extension .xlsx.
    write_xlsx(gapminder, "gapminder.xlsx")
    print("Se ha guardado el archivo")
    
  } else {
    if(val == 2) {
      
      read_excel("gapminder.xlsx")
      print("Se ha leído el archivo")
      
    } else {
      if(val == 3) {
        #Distribution lifeExp vs pop
        ggplot(gapminder, aes(x = lifeExp, y = log(pop), col = continent, 
                              xlab ="Expectativa de vida", ylab = "log(Población)"))+geom_jitter(na.rm = TRUE, alpha = 0.3)
      } else {
        if (val == 4) {
          #Distribution gdpPercap vs pop
          ggplot(gapminder, aes(x = log(gdpPercap), y = log(pop), col = continent, 
                                ylab = "Población"))+geom_jitter(na.rm = TRUE, alpha = 0.3)
        } else {
          if (val == 5) {
          #boxplot gdpPercap x continent between 1990 to 2007.
          vec4 <- c(1990:2007)
          gap <- gapminder[gapminder$year %in% vec4,]
          ggplot(gap, aes(x = continent, y = log(gdpPercap), col = continent))+geom_boxplot(na.rm = TRUE, alpha = 0.3)
          
          } else {
            print("Salió del menú de opciones")
          }
        }
      }
    }
  }
}

#Se debe utilizar la función main() cada vez que se vaya eligir una opción
main()


