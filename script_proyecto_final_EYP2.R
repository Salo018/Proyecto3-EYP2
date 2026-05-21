# Instalar librerías solo si no están instaladas
install.packages("ggplot2")    # Visualización
install.packages("naniar")     # Datos faltantes
install.packages("corrplot")   # Matriz de correlaciones
install.packages("dplyr")      # Manejo de datos
install.packages("ggcorrplot") # Visualización
install.packages("readxl")     # Lectura de Excel

# Cargar librerías
library(ggplot2)
library(naniar)
library(corrplot)
library(ggcorrplot)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(readxl)

# Carga de datos desde GitHub

# URL de los archivos
url_viviendas <- "https://raw.githubusercontent.com/Salo018/Proyecto3-EYP2/main/data/Viviendas.csv"
url_punto1 <- "https://raw.githubusercontent.com/Salo018/Proyecto3-EYP2/main/data/Punto_1.xlsx"
url_consumo <- "https://raw.githubusercontent.com/Salo018/Proyecto3-EYP2/main/data/Consumo.xlsx"

# Leer archivos csv
viviendas <- read.csv(
  url_viviendas,
  sep = ";"
)

# Funcion para leer los archivos xlsx desde github
leer_excel <- function(url, sheet = 1) {
  temp <- tempfile(fileext = ".xlsx")
  download.file(
    url,
    destfile = temp,
    mode = "wb"
  )
  read_excel(temp, sheet = sheet)
}

# Leer los archivos excel
punto1 <- leer_excel(url_punto1)
consumo <- leer_excel(url_consumo)

# Convertir a Dataframe 
df.viviendas <- as.data.frame(viviendas)
df.punto1 <- as.data.frame(punto1)
df.consumo <- as.data.frame(consumo)

# Verificación
head(df.viviendas)
head(df.punto1)
head(df.consumo)


# Ver nulos
nulos <- sum(is.na(df.punto1))
nulos

# Porcentaje de nulos
por_nulos <- (nulos/nrow(df.punto1))*100
por_nulos


datos_1_limpio <- na.omit(df.punto1)

ggplot(data = datos_1_limpio, aes(x = Semana, y = yt))+
  geom_line() + geom_point() + labs(
    x = "Semanas",
    y = "Datos"
  )

# Comprobar media y varianza constante 

# Dividir en 3 
parte_1 <- datos_1_limpio$yt[1:7]
parte_2 <- datos_1_limpio$yt[8:14]
parte_3 <- datos_1_limpio$yt[15:20]


# Medias
cat("Media 1:", mean(parte_1))
cat("Media 2:", mean(parte_2))
cat("Media 3:", mean(parte_3))

# Varianzas 
cat("VarianzaV 1:", var(parte_1))
cat("Varianza 2:", var(parte_2))
cat("Varianza 3:", var(parte_3))

# Medias y varianzas totales 
cat("Media total:", mean(datos_1_limpio$yt))
cat("Varianza total:", var(datos_1_limpio$yt))


acf(datos_1_limpio$yt)
pacf(datos_1_limpio$yt)


# Probar Modelos
#Suavizacion exponencial simple
suavizacion_simple_p_1 <- function(datos, alpha) {
  
  y <- datos$yt
  n <- length(y)
  y_hat <- numeric(n)
  y_hat[1] <- y[1]
  
  for (t in 1:(n-1)) {
    y_hat[t+1] <- y_hat[t] + alpha * (y[t] - y_hat[t])
  }
  
  resultado <- data.frame(
    semana    = 1:n,
    real      = y,
    pronostico = y_hat,
    error     = y - y_hat
  )
  
  return(resultado)
}

# Pruebas de alpha para encontrar el mejor
p_0.1 <- suavizacion_simple_p_1(datos_1_limpio, alpha = 0.1)
p_0.3 <- suavizacion_simple_p_1(datos_1_limpio, alpha = 0.3)
p_0.5 <- suavizacion_simple_p_1(datos_1_limpio, alpha = 0.5)
p_0.8 <- suavizacion_simple_p_1(datos_1_limpio, alpha = 0.8)

# Metricas de errores 
metricas_error_p_1 <- function(resultado) {
  
  error  <- resultado$error
  real   <- resultado$real
  n      <- nrow(resultado)
  
  ECM  <- mean(error^2)
  RECM <- sqrt(ECM)
  MAPE <- mean(abs(error / real)) * 100
  
  return(data.frame(
    ECM  = ECM,
    RECM = RECM,
    MAPE = MAPE
  ))
}

metricas_error_p_1(p_0.1)
metricas_error_p_1(p_0.3)
metricas_error_p_1(p_0.5)
metricas_error_p_1(p_0.8)


# Gráfica
plot(datos_1_limpio, type = "o", col = "blue", ylim = range(c(datos_1_limpio, y_hat)), 
     xlab = "Semanas", ylab = "Datos")
lines(y_hat, type = "o", col = "red")
legend("topleft", legend = c("Real", "Pronóstico"),
       col = c("blue", "red"), lty = 1)
