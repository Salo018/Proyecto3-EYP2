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
library(zoo)
library(tidyr)

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
datos2 <- as.data.frame(viviendas)
df.punto1 <- as.data.frame(punto1)
df.consumo <- as.data.frame(consumo)

# Verificación
head(datos2)
head(df.punto1)
head(df.consumo)

# ---------------- #
# Punto 1:         #
# ---------------- #

# EDA PARA PUNTO 1

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

# ----------------- #
# Punto2: Viviendas
# ----------------- #

#EDA PARA PUNTO 2

# Ver las primeras y ultimas filas
head(datos2)
tail(datos2)

str(datos2)
summary(datos2$Viviendas)

#Ver la cantidad de datos
dim(datos2)

#mirar la cantidad de nulos
sum(is.na(datos2)) 

#mirar filas con nulos
datos2[!complete.cases(datos2), ]

#mirar fechas duplicadas 
anyDuplicated(datos2$Fecha)

#Verificar los tipos de datos de fecha

class(datos2$Fecha)   
head(datos2$Fecha)    

# Convertir Fecha a tipo Date
datos2$Fecha <- as.Date(datos2$Fecha, format = "%d/%m/%Y")
class(datos2$Fecha) 

#Historigrama viviendas
ggplot(datos2, aes(x = Viviendas)) +
  geom_histogram(fill = "blue4", color = "white", bins = 30, na.rm = TRUE) +
  labs(title = "Distribución de nuevas viviendas",
       x = "Viviendas", y = "Frecuencia") +
  theme_minimal()

#---------------------------------------------------------------------------
#Outliers
#RANGO INTERCUARTILICO 

Q1  <- quantile(datos2$Viviendas, 0.25, na.rm = TRUE)
Q3  <- quantile(datos2$Viviendas, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

limite_inf <- Q1 - 1.5 * IQR
limite_sup <- Q3 + 1.5 * IQR

outliers <- datos2[!is.na(datos2$Viviendas) &
                     (datos2$Viviendas < limite_inf |
                        datos2$Viviendas > limite_sup), ]

cat("Número de outliers detectados:", nrow(outliers), "\n")
print(outliers)

# Graficar outliers en la serie de tiempo
datos2$es_outlier <- !is.na(datos2$Viviendas) &
  (datos2$Viviendas < limite_inf |
     datos2$Viviendas > limite_sup)

ggplot(datos2, aes(x = Fecha, y = Viviendas)) +
  geom_line(color = "blue4", linewidth = 0.7, na.rm = TRUE) +
  geom_point(data = datos2[datos2$es_outlier == TRUE, ],
             aes(x = Fecha, y = Viviendas),
             color = "red", shape = 8, size = 3) +
  geom_point(data = datos2[is.na(datos2$Viviendas), ],
             aes(x = Fecha, y = 0),
             color = "deeppink3", shape = 4, size = 3) +
  labs(title    = "Serie con outliers marcados",
       x = "Fecha", y = "Viviendas") +
  theme_minimal()

#------------------------------------------------------------------------------------
#VISUALIZACIONES DE LA SERIE COMPLETA

ggplot(datos2, aes(x = Fecha, y = Viviendas)) +
  # Línea principal
  geom_line(color = "blue4", linewidth = 0.8, na.rm = FALSE) +
  
  geom_point(
    data = datos2[is.na(datos2$Viviendas), ],
    aes(x = Fecha, y = 0),
    color = "deeppink3", shape = 4, size = 3
  ) +
  labs(
    title = "Serie mensual de nuevas viviendas",
    x = "Fecha",
    y = "Nuevas viviendas"
  ) +
  theme_minimal()


#Visualizacion mensual de la serie

df3 <- datos2

df3$Año <- as.numeric(format(df3$Fecha, "%Y"))
df3$Mes <- factor(format(df3$Fecha, "%b"),
                  levels = c("ene.", "feb.", "mar.", "abr.", "may.", 
                             "jun.", "jul.", "ago.", "sept.", "oct.", 
                             "nov.", "dic."))

ggplot(df3, aes(x = Mes, y = Viviendas, group = Año, color = factor(Año))) +
  geom_line(alpha = 0.6) +
  labs(
    title = "Patrón estacional por mes",
    x = "Mes",
    y = "Viviendas",
    color = "Año"
  ) +
  theme_minimal() +
  scale_color_viridis_d(option = "plasma")

#BOXPLOT

ggplot(df3, aes(x = Mes, y = Viviendas, fill = Mes)) +
  geom_boxplot() +
  labs(
    title = "Distribución estacional por mes",
    x = "Mes",
    y = "Viviendas"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")



#EXPLORACIÓN DE LA SERIE
#ESTACIONALIDAD Y TENDENCIA 

#Media por mes

df3$Mes <- format(df3$Fecha, "%m")  

media_por_mes <- aggregate(Viviendas ~ Mes, data = df3, 
                           FUN = mean, na.rm = TRUE)
media_por_mes$Mes <- c("Ene","Feb","Mar","Abr","May","Jun",
                       "Jul","Ago","Sep","Oct","Nov","Dic")
print(media_por_mes)

cat("\nRango de medias mensuales:", 
    round(max(media_por_mes$Viviendas) - min(media_por_mes$Viviendas), 2), "\n")

#Media por decada 

df3$Año <- as.numeric(format(df3$Fecha, "%Y"))
df3$Decada <- paste0(floor(df3$Año / 10) * 10, "s")

media_por_decada <- aggregate(Viviendas ~ Decada, data = df3,
                              FUN = mean, na.rm = TRUE)
print(media_por_decada)


#Varianza por decada 

var_decada <- aggregate(Viviendas ~ Decada, data = df3,
                        FUN = sd, na.rm = TRUE)
names(var_decada)[2] <- "SD"
print(var_decada)

#METRICAS DE IMPUTACION DE NULOS

indices_reales <- which(!is.na(datos2$Viviendas))

#Generar los valores nulos artificiales
set.seed(2240094)
indices_falsos <- sample(indices_reales, 13)
valores_reales <- datos2$Viviendas[indices_falsos]

datos_test <- datos2$Viviendas
datos_test[indices_falsos] <- NA

indices_falsos


# Imputar con los distintos metodos 
media   <- datos_test
media[is.na(media)] <- mean(datos_test, na.rm = TRUE)

mediana <- datos_test
mediana[is.na(mediana)] <- median(datos_test, na.rm = TRUE)

calcular_moda <- function(x) {
  x <- x[!is.na(x)]
  as.numeric(names(sort(table(round(x)), decreasing = TRUE)[1]))
}
moda <- datos_test
moda[is.na(moda)] <- calcular_moda(datos_test)

interpolacion_lineal <- na.approx(datos_test, na.rm = FALSE)

# Métricas
calcular_metricas <- function(reales, imputados, nombre) {
  pred <- imputados[indices_falsos]
  mae  <- mean(abs(reales - pred))
  rmse <- sqrt(mean((reales - pred)^2))
  mape <- mean(abs((reales - pred) / reales)) * 100
  data.frame(Metodo = nombre, MAE = round(mae,2), RMSE = round(rmse,2), MAPE = round(mape,2))
}

resultados <- rbind(
  calcular_metricas(valores_reales, media,   "Media"),
  calcular_metricas(valores_reales, mediana, "Mediana"),
  calcular_metricas(valores_reales, moda,    "Moda"),
  calcular_metricas(valores_reales, interpolacion_lineal,  "Lineal")
)

print(resultados[order(resultados$RMSE), ])


# Tabla de comparación
comparacion <- data.frame(
  Indice  = 1:13,
  Real    = valores_reales,
  Lineal  = interpolacion_lineal[indices_falsos],
  Media   = media[indices_falsos],
  Mediana = mediana[indices_falsos],
  Moda    = moda[indices_falsos]
)
print(comparacion)





#Elegimos el mejor metodo: interpolacion lineal 

datos2$Viviendas <- na.approx(datos2$Viviendas, na.rm = FALSE)
# Verificar que no quedaron NAs
sum(is.na(datos2$Viviendas))

#VISUALIZACIONES DE LA SERIE COMPLETA

ggplot(datos2, aes(x = Fecha, y = Viviendas)) +
  # Línea principal
  geom_line(color = "blue4", linewidth = 0.8, na.rm = FALSE) +
  
  geom_point(
    data = datos2[is.na(datos2$Viviendas), ],
    aes(x = Fecha, y = 0),
    color = "deeppink3", shape = 4, size = 3
  ) +
  labs(
    title = "Serie mensual de nuevas viviendas sin nulos",
    x = "Fecha",
    y = "Nuevas viviendas"
  ) +
  theme_minimal()


