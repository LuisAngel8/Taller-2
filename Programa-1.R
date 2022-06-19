library(gapminder)
library(writexl)
library(readxl)
library(ggplot2)
library(dplyr)


main <- function(){
  cat("1. Guardar gapminder \n", quote = FALSE)
  cat("2. Leer archivo gapminder \n", quote = FALSE)
  cat("3. Desplegar diagrama de distribución (lifeExp vs pop) \n", quote = FALSE)
  cat("4. Desplegar diagrama de distribución (gdpPercap vs pop) \n", quote = FALSE)
  cat("5. Desplegar diagrama de cajas de gdpPercap por continente desde 1990 a 2007 \n", quote = FALSE)
  cat("Presione cualquier otra número para SALIR")
  val <- as.integer(readline("Ingrese un valor: "))
  
  if(val == 1) {
    
    for (i in c("lifeExp", "pop", "gdpPercap")) {
      ran <- sample(1:length(gapminder$lifeExp), size = length(gapminder$lifeExp) * 0.1, replace = FALSE)
      gapminder[ran,i] <- NA
    }
    #Save file gapminder with extension .xlsx.
    library(writexl)
    write_xlsx(gapminder, "gapminder.xlsx")
    print("Se ha guardado el archivo")
    
  } else {
    if(val == 2) {
      
      library(readxl)
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

gapminder %>% 
  filter(year >= 1990) %>% 
  ggplot(aes(x = continent, y = log(gdpPercap), col = continent))+
  geom_boxplot(alpha = 0.3)
  
# gapminder
# 
# vec <- sort(sample(1:length(gapminder$lifeExp), size = length(gapminder$lifeExp) * 0.10))
# gapminder$lifeExp <- replace(gapminder$lifeExp, list = vec, values = NA)
# vec
# 
# vec2 <- sort(sample(1:length(gapminder$pop), size = length(gapminder$pop) * 0.10))
# gapminder$pop <- replace(gapminder$pop, list = vec2, values = NA)
# vec2
# 
# vec3 <- sort(sample(1:length(gapminder$gdpPercap), size = length(gapminder$gdpPercap) * 0.10))
# gapminder$gdpPercap <- replace(gapminder$gdpPercap, list = vec3, values = NA)
# vec3

for (i in c("lifeExp", "pop", "gdpPercap")) {
  ran <- sample(1:length(gapminder$lifeExp), size = length(gapminder$lifeExp) * 0.1, replace = FALSE)
  gapminder[ran,i] <- NA
}

sum(is.na(gapminder$lifeExp))
# gapminder$lifeExp[sample(nrow(gapminder),length(gapminder$lifeExp)*0.10)] <- NA
# gapminder$lifeExp

# val_rep <- sample(gapminder$lifeExp, size = length(gapminder$lifeExp) * 0.10)
# col_rep <- c("lifeExp", "gdpPercap", "pop")
# 
# gapminder[col_rep] <- sapply(gapminder[col_rep],
#                              function(x) replace(x, x %in% val_rep, NA))


write_xlsx(gapminder, "gapminder.xlsx")

library("readxl")
read_excel("C:\\Users\\Luis Angel\\Documents\\Unal\\ProgramacionR\\gapminder.xlsx")

ggplot(gapminder, aes(x = lifeExp, y = log(pop), col = continent, xlab ="Expectativa de vida", ylab = "log(Población)"))+geom_jitter(na.rm = TRUE, alpha = 0.3)
ggplot(gapminder, aes(x = log(gdpPercap), y = log(pop), col = continent, ylab = "Población"))+geom_jitter(na.rm = TRUE, alpha = 0.3)

vec4 <- c(1990:2007)
gap <- gapminder[gapminder$year %in% vec4,]
ggplot(gap, aes(x = continent, y = log(gdpPercap), col = continent))+geom_boxplot(na.rm = TRUE, alpha = 0.3)


