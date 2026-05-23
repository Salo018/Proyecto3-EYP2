
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

# CARGA DE DATOS DESDE GITHUB

# URL de los archivos
url_viviendas <- "https://raw.githubusercontent.com/Salo018/Proyecto3-EYP2/main/data/Viviendas.csv"
url_punto1 <- "https://raw.githubusercontent.com/Salo018/Proyecto3-EYP2/main/data/Punto_1.xlsx"
url_consumo <- "https://raw.githubusercontent.com/Salo018/Proyecto3-EYP2/main/data/Consumo.xlsx"

# Leer archivos csv
viviendas <- read.csv(
  url_viviendas,
  sep = ";"
)

# FUnción para leer los archivos xlsx 
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

# SOLUCIÓN CASOS DE ESTUDIO

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

# ---------------- #
# Punto 3: Consumo #
# ---------------- #

# EDA PARA PUNTO 3

# Revisar valores unicos en cada columna
unique(df.consumo$Año)
unique(df.consumo$Trimestre)
unique(df.consumo$Consumo)

# Revisar nulos
sum(is.na(df.consumo))

# Porcentaje de nulos 
mean(is.na(df.consumo)) * 100

# GRAFICOS DE PRUEBA INICIALES

# Grafico inicial
ggplot(data = df.consumo, aes(x = Año, y = Consumo, color = Trimestre, group = Trimestre)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Año",
    y = "Consumo personal (%)",
    title = "Consumo personal trimestral (1960-1990)"
  ) +
  theme_minimal()

# Crear la variable "Periodo" para unir el año con los trimestres
df.consumo_copy <- df.consumo %>%
  mutate(Trimestre_num = gsub("Trimestre_", "", Trimestre),
         Periodo = paste(Año, Trimestre_num, sep = "-"))

# Grafica con todos los años 
ggplot(data = df.consumo_copy, aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo del consumo personal (1960-2016)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Graficar solo los años de 1960 a 1990 (esto es solo para probar)
ggplot(data = df.consumo_copy %>% filter(Año >= 1960 & Año <= 1990),
       aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo del consumo personal (1960-1990)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Crear un df sin nulos 
df.sin_nulos <- df.consumo %>% filter(!is.na(Consumo))

# Resumen estadistico
summary(df.sin_nulos$Consumo)

# Boxplot
boxplot(df.sin_nulos$Consumo, main = "Boxplot Consumo")

# Desviaciones estandar 
df.sin_nulos <- df.sin_nulos %>%
  mutate(z_score = (Consumo - mean(Consumo, na.rm = TRUE)) / sd(Consumo, na.rm = TRUE))

outliers <- df.sin_nulos %>% filter(abs(z_score) > 3)
print(outliers)

# Crear la variable periodo para df sin nulos
df.sin_nulos_copy <- df.sin_nulos %>%
  mutate(Trimestre_num = gsub("Trimestre_", "", Trimestre),
         Periodo = paste(Año, Trimestre_num, sep = "-"))

# Visualización en la serie
ggplot(df.sin_nulos_copy, aes(x = Periodo, y = Consumo)) +
  geom_line(color = "blue") +
  geom_point(aes(color = abs(z_score) > 3)) +
  scale_color_manual(values = c("black", "red")) +
  labs(title = "Serie con posibles outliers")

# PROCEDIMIENTO PARA EVALUAR METODOS DE IMPUTACIÓN

# Crear nulos artificiales 5% mas o menos 

# Numero total de registros
n_total <- nrow(df.sin_nulos_copy)
print(n_total)

# 5% del total de registros para saber cuanto nulos se necesitan
n_nulos <- round(0.05 * n_total)
n_nulos
print(n_nulos)

# Posiciones de outliers
pos_outliers <- which(abs(df.sin_nulos$z_score) > 3)

# Cuántos faltan para llegar al 5%
faltan <- n_nulos - length(pos_outliers)

# Seleccionar posiciones aleatorias adicionales
set.seed(123)  # para reproducibilidad
pos_extra <- sample(setdiff(1:n_total, pos_outliers), faltan)

# Unir todas las posiciones
pos_nulos <- c(pos_outliers, pos_extra)

# Crear nulos artificiales
df.nulos_artificiales <- df.sin_nulos
df.nulos_artificiales$Consumo[pos_nulos] <- NA

# IMPUTACIÓN Y CALCULO DEL ECM

# Función para calcular el ECM 
calc_ecm <- function(original, imputado, posiciones) {
  mean((original[posiciones] - imputado[posiciones])^2, na.rm = TRUE)
}

# Imputación con distintos metodos

# Imputación con media
media_val <- mean(df.sin_nulos$Consumo, na.rm = TRUE)
df.media <- df.nulos_artificiales
df.media$Consumo[is.na(df.media$Consumo)] <- media_val
# Comparación contra valores reales en df.sin_nulos
ecm_media <- calc_ecm(df.sin_nulos$Consumo, df.media$Consumo, pos_nulos)

# Imputación con mediana
mediana_val <- median(df.sin_nulos$Consumo, na.rm = TRUE)
df.mediana <- df.nulos_artificiales
df.mediana$Consumo[is.na(df.mediana$Consumo)] <- mediana_val
# Comparación contra valores reales en df.sin_nulos
ecm_mediana <- calc_ecm(df.sin_nulos$Consumo, df.mediana$Consumo, pos_nulos)

# Imputación con moda
# Función para calcular la moda
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda_val <- get_mode(df.sin_nulos$Consumo)
df.moda <- df.nulos_artificiales
df.moda$Consumo[is.na(df.moda$Consumo)] <- moda_val
# Comparación contra valores reales en df.sin_nulos
ecm_moda <- calc_ecm(df.sin_nulos$Consumo, df.moda$Consumo, pos_nulos)

# Imputación con Interpolación lineal
df.interp <- df.nulos_artificiales
df.interp$Consumo <- na.approx(df.interp$Consumo, na.rm = FALSE)
# Comparación contra valores reales en df.sin_nulos
ecm_interp <- calc_ecm(df.sin_nulos$Consumo, df.interp$Consumo, pos_nulos)

# COMPARAR RESULTADOS 

# Comparar resultados en un data.frame
resultados <- data.frame(
  Metodo = c("Media", "Mediana", "Moda", "Interpolación"),
  ECM = c(ecm_media, ecm_mediana, ecm_moda, ecm_interp)
)

# Mostrar resultados ordenados de menor a mayor ECM
resultados <- resultados[order(resultados$ECM), ]
print(resultados)

# Método ganador
mejor_metodo <- resultados$Metodo[1]
mejor_ecm <- resultados$ECM[1]

cat("El mejor método es:", mejor_metodo, "con ECM =", mejor_ecm, "\n")

# Valores originales en las posiciones de nulos
valores_reales <- df.sin_nulos$Consumo[pos_nulos]

# Valores imputados por moda
valores_moda <- df.moda$Consumo[pos_nulos]

# Valores imputados por interpolación
valores_interp <- df.interp$Consumo[pos_nulos]

# DataFrame comparativo
comparacion <- data.frame(
  Posicion = pos_nulos,
  Real = valores_reales,
  Moda = valores_moda,
  Interpolacion = valores_interp
)

print(comparacion)

# Gráfico comparativo
ggplot(comparacion, aes(x = Posicion)) +
  geom_point(aes(y = Real), color = "black", size = 3) +
  geom_point(aes(y = Moda), color = "red", size = 2) +
  geom_point(aes(y = Interpolacion), color = "blue", size = 2) +
  labs(title = "Comparación imputación: Moda vs Interpolación",
       y = "Consumo", x = "Posición (índice)") +
  theme_minimal()

# Aplicar la imputación por interpolación lineal al df con sus nulos originales
# Dataset con la variable Periodo
df.consumo_interp <- df.consumo_copy   
# Imputar el nulo real con interpolación lineal
df.consumo_interp$Consumo <- na.approx(df.consumo_interp$Consumo, na.rm = FALSE)

# Grafica con el df imputado 
ggplot(data = df.consumo_interp, aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo del consumo personal (1960-2016)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# PENDIENTE -------------------------
acf(df.consumo_interp$Consumo)
pacf(df.consumo_interp$Consumo)

# Comprobacion si la serie es estacionaria

# Dividir en 4 partes 
p3_parte_1 <- df.consumo_interp$Consumo[1:47]
p3_parte_2 <- df.consumo_interp$Consumo[48:93]
p3_parte_3 <- df.consumo_interp$Consumo[94:140]
p3_parte_4 <- df.consumo_interp$Consumo[141:187]

# Medias
cat("Media 1:", mean(p3_parte_1))
cat("Media 2:", mean(p3_parte_2))
cat("Media 3:", mean(p3_parte_3))
cat("Media 4:", mean(p3_parte_4))

# Varianzas 
cat("Varianza 1:", var(p3_parte_1))
cat("Varianza 2:", var(p3_parte_2))
cat("Varianza 3:", var(p3_parte_3))
cat("Varianza 4:", var(p3_parte_4))

# Medias y varianzas totales 
cat("Media total:", mean(df.consumo_interp$Consumo))
cat("Varianza total:", var(df.consumo_interp$Consumo))

