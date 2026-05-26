# Instalar librerías solo si no están instaladas
#install.packages("ggplot2")    # Visualización
#install.packages("naniar")     # Datos faltantes
#install.packages("corrplot")   # Matriz de correlaciones
#install.packages("dplyr")      # Manejo de datos
#install.packages("ggcorrplot") # Visualización
#install.packages("readxl")     # Lectura de Excel

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
library(tseries)

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
acf(datos_1_limpio$yt, main = "ACF - Serie semanal")
pacf(datos_1_limpio$yt, main = "PACF - Serie semanal")

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
p_0 <- suavizacion_simple_p_1(datos_1_limpio, alpha = 0)

# Metricas de errores 
metricas_error_p_1 <- function(resultado) {
  
  error  <- resultado$error
  real <- resultado$real
  n <- nrow(resultado)
  
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
metricas_error_p_1(p_0)


# Probar modelo de promedios moviles 

prom_mov_p_1 <- function(datos, k) {
  
  y <- datos$yt
  n <- length(y)      
  y_pro <- rep(NA, n)
  
  for (t in (k+1):n) {
    y_pro[t] <- sum(y[(t-k):(t-1)]) / k  
  }
  
  resultado <- data.frame(
    semana     = 1:n,
    real       = y,
    pronostico = y_pro,
    error      = y - y_pro
  )
  
  return(resultado)
}

prom_m_4 <- prom_mov_p_1(datos_1_limpio, 4)
prom_m_8 <- prom_mov_p_1(datos_1_limpio, 8)
prom_m_12 <- prom_mov_p_1(datos_1_limpio, 12) # Mejores metricas pero no es correcto
prom_m_10 <- prom_mov_p_1(datos_1_limpio, 10) # Mejor hasta ahora 


#Funcion para calcular error de medias moviles porque hay nulos
metricas_error_mov <- function(resultado) {
  
  resultado_limpio <- resultado[!is.na(resultado$error), ]
  
  error <- resultado_limpio$error
  real  <- resultado_limpio$real
  
  ECM  <- mean(error^2)
  RECM <- sqrt(ECM)
  MAPE <- mean(abs(error / real)) * 100
  
  return(data.frame(
    ECM  = ECM,
    RECM = RECM,
    MAPE = MAPE
  ))
}

# Metricas de error de medias moviles 
metricas_error_mov(prom_m_4)
metricas_error_mov(prom_m_8)
metricas_error_mov(prom_m_12)
metricas_error_mov(prom_m_10)


# Comprobar modelo MA(1)

residuos_MA_p_1 <- function(theta, y, q){
  
  n <- length(y)
  mu <- theta[1]
  coef_ma <- theta[2:(q+1)]
  e <- numeric(n)
  
  for(t in (q+1):n){
    
    ma_part <- 0
    
    for(j in 1:q){
      ma_part <- ma_part + coef_ma[j] * e[t-j]
    }
    
    e[t] <- y[t] - mu - ma_part
  }
  
  return(e)
}

#Optimizar funcuión objetivo
SSE_MA <- function(theta, y, q){
  
  e <- residuos_MA_p_1(theta, y, q)
  
  return(sum(e^2))
}

#Estimación en modelo MA(q)

estimar_MA <- function(y, q){
  
  theta_ini <- rep(0.1, q + 1)
  
  ajuste <- optim(
    par = theta_ini,
    fn = SSE_MA,
    y = y,
    q = q
  )
  
  return(list(
    coef = ajuste$par,
    value = ajuste$value
  ))
}


#Implementación de un MA(1)
modelo_ma1_p_1 <- estimar_MA(datos_1_limpio$yt, q = 1)
modelo_ma1_p_1$coef
modelo_ma2_p_1 <- estimar_MA(datos_1_limpio$yt, q = 2)
modelo_ma2_p_1$coef

# Pronosticos
pronosticos_MA_p_1 <- function(y, modelo, q) {
  
  n <- length(y)
  mu <- modelo$coef[1]
  coef_ma <- modelo$coef[2:(q+1)]
  
  e <- residuos_MA_p_1(modelo$coef, y, q)
  
  y_hat <- rep(NA, n)
  
  for(t in (q+1):n) {
    ma_part <- 0
    for(j in 1:q) {
      ma_part <- ma_part + coef_ma[j] * e[t-j]
    }
    y_hat[t] <- mu + ma_part
  }
  
  return(data.frame(
    semana     = 1:n,
    real       = y,
    pronostico = y_hat,
    error      = y - y_hat
  ))
}

# Probar modelo
modelo_MA_p1 <- pronosticos_MA_p_1(datos_1_limpio$yt, modelo_ma1_p_1, q = 1)
modelo_MA2_p1 <- pronosticos_MA_p_1(datos_1_limpio$yt, modelo_ma2_p_1, q = 2)

# Metricas de MA(1)
metricas_error_mov(modelo_MA_p1)
metricas_error_mov(modelo_MA2_p1)

# El mejor modelo es MA(1)
# Prediccion de 4 periodos
pronostico_MA1_p_1 <- function(y, modelo, h = 4) {
  
  mu     <- modelo$coef[1]
  theta1 <- modelo$coef[2]
  
  # Recalcular residuos para obtener el ultimo e(t)
  e <- residuos_MA_p_1(modelo$coef, y, q = 1)
  ultimo_e <- e[length(e)]
  
  n <- length(y)
  y_hat_futuro <- numeric(h)
  
  for(h_i in 1:h) {
    if(h_i == 1) {
      # Primer paso: usa el ultimo residuo conocido
      y_hat_futuro[h_i] <- mu + theta1 * ultimo_e
    } else {
      # Pasos siguientes: residuos futuros = 0
      y_hat_futuro[h_i] <- mu
    }
  }
  
  resultado <- data.frame(
    semana    = (n+1):(n+h),
    pronostico = y_hat_futuro
  )
  
  return(resultado)
}

# Ver pronosticos
pronostico_4 <- pronostico_MA1_p_1(datos_1_limpio$yt, modelo_ma1_p_1, h = 4)
pronostico_4


# Modelo ARMA(p,q) 

residuos_ARMA_p_1 <- function(param, y, p, q){
  
  n      <- length(y)
  cte    <- param[1]
  phi    <- if(p > 0) param[2:(p+1)]          else numeric(0)  # guard p=0
  theta  <- if(q > 0) param[(p+2):(p+q+1)]    else numeric(0)  # guard q=0
  e      <- numeric(n)
  inicio <- max(p, q) + 1
  
  for(t in inicio:n){
    ar_part <- 0
    ma_part <- 0
    
    for(i in seq_len(p)) ar_part <- ar_part + phi[i]  * y[t-i]  # seq_len(0) no itera
    for(j in seq_len(q)) ma_part <- ma_part + theta[j] * e[t-j]  # seq_len(0) no itera
    
    e[t] <- y[t] - cte - ar_part - ma_part
  }
  return(e)
}

SSE_ARMA_p_1 <- function(param, y, p, q){
  e <- residuos_ARMA_p_1(param, y, p, q)
  return(sum(e^2))
}

estimar_ARMA_p_1 <- function(y, p, q){
  n_param <- 1 + p + q
  ini     <- rep(0.1, n_param)
  
  ajuste <- optim(
    par = ini,
    fn  = SSE_ARMA_p_1,
    y   = y,
    p   = p,
    q   = q
  )
  return(list(coef = ajuste$par, value = ajuste$value))
}

# Valores ajustados
fitted_ARMA_p_1 <- function(param, y, p, q){
  
  n      <- length(y)
  cte    <- param[1]
  phi    <- if(p > 0) param[2:(p+1)]          else numeric(0)
  theta  <- if(q > 0) param[(p+2):(p+q+1)]    else numeric(0)
  e      <- numeric(n)
  yhat   <- rep(NA, n)
  inicio <- max(p, q) + 1
  
  for(t in inicio:n){
    ar_part <- 0
    ma_part <- 0
    
    for(i in seq_len(p)) ar_part <- ar_part + phi[i]  * y[t-i]
    for(j in seq_len(q)) ma_part <- ma_part + theta[j] * e[t-j]
    
    yhat[t] <- cte + ar_part + ma_part
    e[t]    <- y[t] - yhat[t]
  }
  
  return(data.frame(
    t        = 1:n,
    real     = y,
    ajustado = yhat,
    error    = y - yhat
  ))
}

#  Implementacion
modelo_arma.0.1_p_1 <- estimar_ARMA_p_1(datos_1_limpio$yt, p = 0, q = 1)
modelo_arma.1.0_p_1 <- estimar_ARMA_p_1(datos_1_limpio$yt, p = 1, q = 0)
modelo_arma.1.1_p_1 <- estimar_ARMA_p_1(datos_1_limpio$yt, p = 1, q = 1)

resultado_arma.0.1_p_1 <- fitted_ARMA_p_1(modelo_arma.0.1_p_1$coef, datos_1_limpio$yt, p = 0, q = 1)
resultado_arma.1.0_p_1 <- fitted_ARMA_p_1(modelo_arma.1.0_p_1$coef, datos_1_limpio$yt, p = 1, q = 0)
resultado_arma.1.1_p_1 <- fitted_ARMA_p_1(modelo_arma.1.1_p_1$coef, datos_1_limpio$yt, p = 1, q = 1)

metricas_arma.0.1_p_1  <- metricas_error_mov(resultado_arma.0.1_p_1)
metricas_arma.1.0_p_1  <- metricas_error_mov(resultado_arma.1.0_p_1)
metricas_arma.1.1_p_1  <- metricas_error_mov(resultado_arma.1.1_p_1)

metricas_arma.0.1_p_1
metricas_arma.1.0_p_1
metricas_arma.1.1_p_1

# Grafico de valores reales y pronosticos con MA(1)
plot(
  modelo_MA_p1$semana, modelo_MA_p1$real,
  type = "b", pch = 1, col = "steelblue",
  xlab = "Semana", ylab = "Datos",
  main = "MA(1): Valores reales (azul) vs Pronósticos (rojo)",
  ylim = range(modelo_MA_p1$real, modelo_MA_p1$pronostico, na.rm = TRUE)
)

lines(modelo_MA_p1$semana, modelo_MA_p1$pronostico,
      type = "b", pch = 1, col = "red", lty = 2)


# -------------------- #
#  Punto 2: Viviendas  #
# -------------------- #
#EDA: analisis exploratorio de la serie

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

cat("Número de Outliers en la serie:", nrow(outliers), "\n")
print(outliers)

# Grafico de la serie con outliers 
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


#VISUALIZACION DE LA SERIE COMPLETA: Las X son los valores nulos

ggplot(datos2, aes(x = Fecha, y = Viviendas)) +
  
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

#Calcular la varianza total de la serie antes de imputar
varianza_inicial <- var(datos2$Viviendas, na.rm = TRUE)

varianza_inicial


#Media por mes

df3$Mes <- format(df3$Fecha, "%m")  

media_por_mes <- aggregate(Viviendas ~ Mes, data = df3, 
                           FUN = mean, na.rm = TRUE)
media_por_mes$Mes <- c("Ene","Feb","Mar","Abr","May","Jun",
                       "Jul","Ago","Sep","Oct","Nov","Dic")
print(media_por_mes)

cat("\nRango de medias mensuales:", 
    round(max(media_por_mes$Viviendas) - min(media_por_mes$Viviendas), 2), "\n")



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

#Moda
calcular_moda <- function(x) {
  x <- x[!is.na(x)]
  as.numeric(names(sort(table(round(x)), decreasing = TRUE)[1]))
}
moda <- datos_test
moda[is.na(moda)] <- calcular_moda(datos_test)

#Interpolacion lineal

interpolacion_lineal <- na.approx(datos_test, na.rm = FALSE)

# Media estacional 
mes_test <- format(datos2$Fecha, "%m")
media_mensual <- tapply(datos_test, mes_test, mean, na.rm = TRUE) 

imp_estacional <- datos_test
for (i in which(is.na(imp_estacional))) {
  imp_estacional[i] <- media_mensual[mes_test[i]]
}


#Medias Moviles 

K <- 12

imp_mm12 <- datos_test
for (i in which(is.na(imp_mm12))) {
  idx <- max(1, i - K%/%2) : min(length(imp_mm12), i + K%/%2)
  idx <- idx[idx != i]  # excluir el NA mismo
  imp_mm12[i] <- mean(imp_mm12[idx], na.rm = TRUE)
}

K <- 7

imp_mm7 <- datos_test
for (i in which(is.na(imp_mm7))) {
  idx <- max(1, i - K%/%2) : min(length(imp_mm7), i + K%/%2)
  idx <- idx[idx != i]  
  imp_mm7[i] <- mean(imp_mm7[idx], na.rm = TRUE)
}

K <- 3

imp_mm <- datos_test
for (i in which(is.na(imp_mm))) {
  idx <- max(1, i - K%/%2) : min(length(imp_mm), i + K%/%2)
  idx <- idx[idx != i]  # excluir el NA mismo
  imp_mm[i] <- mean(imp_mm[idx], na.rm = TRUE)
}

#LOCF
imp_locf <- na.locf(datos_test, na.rm = FALSE)



# Métricas de errores
calcular_metricas <- function(reales, imputados, nombre) {
  pred <- imputados[indices_falsos]
  
  mae  <- mean(abs(reales - pred))
  rmse <- sqrt(mean((reales - pred)^2))
  ecm  <- mean((reales - pred)^2)   # Error Cuadrático Medio
  mape <- mean(abs((reales - pred) / reales)) * 100
  
  data.frame(
    Metodo = nombre,
    MAE = round(mae, 2),
    RMSE = round(rmse, 2),
    ECM = round(ecm, 2),
    MAPE = round(mape, 2)
  )
}

resultados <- rbind(
  calcular_metricas(valores_reales, moda,                 "Moda"),
  calcular_metricas(valores_reales, interpolacion_lineal, "Lineal"),
  calcular_metricas(valores_reales, imp_estacional,       "Media estacional"),
  calcular_metricas(valores_reales, imp_mm,               "Medias moviles"),
  calcular_metricas(valores_reales, imp_mm7,               "Medias moviles7"),
  calcular_metricas(valores_reales, imp_mm12,               "Medias moviles12"),
  calcular_metricas(valores_reales, imp_locf, "LOCF")
)

print(resultados[order(resultados$RMSE), ])


# Tabla de comparación

comparacion <- data.frame(
  Indice            = 1:13,
  Real              = valores_reales,
  Lineal            = interpolacion_lineal[indices_falsos],
  Moda              = moda[indices_falsos],
  Media_estacional  = imp_estacional[indices_falsos],
  Medias_moviles  = imp_mm[indices_falsos],
  Medias_moviles7  = imp_mm7[indices_falsos],
  Medias_moviles12  = imp_mm12[indices_falsos],
  LOCF  = imp_locf[indices_falsos]
)
print(comparacion)


#Elegimos el metodo mas apropiado: Medias moviles K=3
K <- 3

for (i in which(is.na(datos2$Viviendas))) {
  idx <- max(1, i - K%/%2) : min(length(datos2$Viviendas), i + K%/%2)
  idx <- idx[idx != i]
  datos2$Viviendas[i] <- mean(datos2$Viviendas[idx], na.rm = TRUE)
}

#Verificar
sum(is.na(datos2$Viviendas))


#Calcular la varianza total de la serie despues de imputar
#El metodo de medias moviles de K=3 disminuyo la varianza en un 0.30% y fue el 
# mejor resultado obtenido entre la mayor parte de los metodos de imputación.
varianza_final <- var(datos2$Viviendas, na.rm = TRUE)

varianza_final

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
    title = "Serie mensual de nuevas viviendas imputada",
    x = "Fecha",
    y = "Nuevas viviendas"
  ) +
  theme_minimal()


#Descomposición de la serie 

#Pasar a formato de serie de tiempo
datos2.ts <- ts(datos2$Viviendas,
                start = c(1959,1),  
                frequency = 12)      
datos2.ts

gnp.decomp = decompose(datos2.ts, type = "mult")
plot(gnp.decomp)

# Graficas de ACF Y PACF para la serie original
acf(datos2$Viviendas)

pacf(datos2$Viviendas)


#Probar primer modelo: SUAVIZACION DE HOLT-WINTER
#p=12 porque la estacionalidad es anual y se repite cada 12 meses

holt_winter_p_2 <- function(y, alpha = 0.3, beta = 0.2, gamma = 0.1, p = 12) {
  n <- length(y)
  
  F <- numeric(n)      
  T <- numeric(n)     
  S <- numeric(n)     
  y_hat <- numeric(n) 
  
  # Nivel inicial
  F[p] <- mean(y[1:p])
  
  # Tendencia inicial
  T[p] <- (mean(y[(p+1):(2*p)]) - mean(y[1:p])) / p
  
  # Índice estacional inicial
  for (i in 1:p) {
    S[i] <- y[i] / F[p]
  }
  
  # Iteración
  for (t in (p+1):n) {
    #Nivel
    F[t] <- alpha * (y[t] / S[t-p]) + (1 - alpha) * (F[t-1] + T[t-1])
    #Tendencia
    T[t] <- gamma * (F[t] - F[t-1]) + (1 - gamma) * T[t-1]
    #Estacionalidad
    S[t] <- beta  * (y[t] / F[t])   + (1 - beta)  * S[t-p]
    #Pronostico
    y_hat[t] <- (F[t-1] + T[t-1]) * S[t-p]
  }
  
  data.frame(
    tiempo     = 1:n,
    real       = y,
    nivel      = F,
    tendencia  = T,
    estacional = S,
    pronostico = y_hat,
    error      = y - y_hat
  )
}

# Aplicar con parámetros iniciales 
resultados_p_2 <- holt_winter_p_2(datos2$Viviendas,
                                  alpha = 0.3,
                                  beta  = 0.2,
                                  gamma = 0.1,
                                  p     = 12)
head(resultados_p_2, 15)


#2. Encontrar parametros optimos

# Función que calcula RMSE
rmse_hw <- function(params) {
  alpha <- params[1]
  beta  <- params[2]
  gamma <- params[3]
  
  # Restricción: parámetros entre 0 y 1
  if (any(params <= 0) || any(params >= 1)) return(Inf)
  
  res <- holt_winter_p_2(datos2$Viviendas, alpha, beta, gamma, p = 12)
  
  # Solo calcular error donde hay pronóstico (desde p+1)
  errores <- res$error[13:nrow(res)]
  sqrt(mean(errores^2, na.rm = TRUE))
}

# Optimización
optimos <- optim(
  par    = c(0.3, 0.2, 0.1),   
  fn     = rmse_hw,
  method = "L-BFGS-B",
  lower  = c(0.01, 0.01, 0.01),
  upper  = c(0.99, 0.99, 0.99)
)

cat("Alpha óptimo:", round(optimos$par[1], 4), "\n")
cat("Beta óptimo: ", round(optimos$par[2], 4), "\n")
cat("Gamma óptimo:", round(optimos$par[3], 4), "\n")
cat("RMSE mínimo: ", round(optimos$value, 4), "\n")

# Aplicar con parámetros óptimos
alpha_opt <- optimos$par[1]
beta_opt  <- optimos$par[2]
gamma_opt <- optimos$par[3]

resultados_hw_opt_p_2 <- holt_winter_p_2(datos2$Viviendas,
                                         alpha = alpha_opt,
                                         beta  = beta_opt,
                                         gamma = gamma_opt,
                                         p     = 12)
resultados_hw_opt_p_2

#Metricas 
errores_opt_p_2 <- resultados_hw_opt_p_2$error[13:nrow(resultados_hw_opt_p_2)]
reales_opt_p_2  <- resultados_hw_opt_p_2$real[13:nrow(resultados_hw_opt_p_2)]

mae_hw   <- mean(abs(errores_opt_p_2), na.rm = TRUE)
recm_hw  <- sqrt(mean(errores_opt_p_2^2, na.rm = TRUE)) 
ecm_hw   <- mean(errores_opt_p_2^2, na.rm = TRUE)       
mape_hw  <- mean(abs(errores_opt_p_2 / reales_opt_p_2), na.rm = TRUE) * 100

# Métricas para valores óptimos
cat("MAE  : ", round(mae_hw, 2), "\n")
cat("RECM : ", round(recm_hw, 2), "\n")   # aquí cambias el nombre
cat("ECM  : ", round(ecm_hw, 2), "\n")
cat("MAPE : ", round(mape_hw, 2), "%\n")

#Grafico con pronosticos
ggplot(resultados_hw_opt_p_2[13:nrow(resultados_hw_opt_p_2), ],
       aes(x = tiempo)) +
  geom_line(aes(y = real,       color = "Real"),       linewidth = 0.8) +
  geom_line(aes(y = pronostico, color = "Pronóstico"), linewidth = 0.8, 
            linetype = "dashed") +
  scale_color_manual(values = c("Real" = "blue4", "Pronóstico" = "#B23AEE")) +
  labs(title = "Holt-Winter pronosticos",
       x = "Tiempo", y = "Viviendas", color = "") +
  theme_minimal()


#Se implemento de forma manual, pero se realizo una doble diferenciación para tendencia y
#estacionalidad, luego esta serie diferenciada se paso a un modelo ARMA con la que se probo 
#los respectivos p y q.
#Por lo que teoricamente estamos haciendo un ARIMA de forma muy manual 

#DIFERENCIACIÓN 

# Serie original
y <- datos2$Viviendas
n <- length(y)


# Primera diferenciacion: tendencia 
dl_y_3 <- numeric(n)
for (t in 2:n) {
  dl_y_3[t] <- y[t] - y[t-1]
}


# Diferenciación estacional: m=12 ya que la estacionalidad es anual y se repite 
#cada 12 meses
dl_y_2 <- numeric(n)
for (t in 14:n) {
  dl_y_2[t] <- dl_y_3[t] - dl_y_3[t-12]
}

dl_y<- dl_y_2[14:n]

# Verificar con prueba de Dickey-Fuller
adf.test(dl_y)


# Graficar serie original vs diferenciada
par(mfrow = c(1,2)) 
plot.ts(y, main = "Serie original (Viviendas)", col = "#5D478B")
plot.ts(dl_y, main = "Serie diferenciada (1 vez)", col = "indianred3")


# Graficas de ACF Y PACF para la serie diferenciada 

acf(dl_y)
pacf(dl_y)


## Ajustar un ARMA sobre la serie ya diferenciada para tendencia y estacionalidad 
# y comparar metricas de modelos ARMA(1,1), ARMA(1,2) y ARMA(2,1)
# Funciones ARMA
# Quitar valores con cero 
dl_y_limpio <- dl_y[-1]

residuos_ARMA <- function(param, y, p, q){
  n <- length(y)
  cte <- param[1]
  phi <- if(p > 0) param[2:(p+1)] else numeric(0)
  theta <- if(q > 0) param[(p+2):(p+q+1)] else numeric(0)
  e <- numeric(n)
  inicio <- max(p, q) + 1
  for(t in inicio:n){
    ar_part <- 0
    ma_part <- 0
    if(p > 0){ for(i in 1:p){ ar_part <- ar_part + phi[i] * y[t-i] } }
    if(q > 0){ for(j in 1:q){ ma_part <- ma_part + theta[j] * e[t-j] } }
    e[t] <- y[t] - cte - ar_part - ma_part
  }
  return(e)
}

SSE_ARMA <- function(param, y, p, q){
  e <- residuos_ARMA(param, y, p, q)
  sum(e^2)
}

estimar_ARMA <- function(y, p, q){
  n_param <- 1 + p + q
  ini <- rep(0.1, n_param)
  ajuste <- optim(par = ini, fn = SSE_ARMA, y = y, p = p, q = q)
  return(list(coef = ajuste$par, value = ajuste$value))
}

ajustar_ARMA <- function(param, y, p, q){
  n <- length(y)
  cte <- param[1]
  phi <- if(p > 0) param[2:(p+1)] else numeric(0)
  theta <- if(q > 0) param[(p+2):(p+q+1)] else numeric(0)
  e <- numeric(n)
  y_hat <- numeric(n)
  inicio <- max(p, q) + 1
  for(t in inicio:n){
    ar_part <- 0
    ma_part <- 0
    if(p > 0){ for(i in 1:p){ ar_part <- ar_part + phi[i] * y[t-i] } }
    if(q > 0){ for(j in 1:q){ ma_part <- ma_part + theta[j] * e[t-j] } }
    y_hat[t] <- cte + ar_part + ma_part
    e[t] <- y[t] - y_hat[t]
  }
  return(list(y_hat = y_hat, e = e))
}

graficar_ARMA <- function(modelo, y, p, q, titulo){
  ajuste <- ajustar_ARMA(modelo$coef, y, p, q)
  plot(y, type = "l", col = "red",
       main = titulo, ylab = "dl_y", xlab = "Tiempo")
  lines(ajuste$y_hat, col = "blue")
}

graficar_residuos <- function(modelo, y, p, q, titulo){
  ajuste <- ajustar_ARMA(modelo$coef, y, p, q)
  par(mfrow = c(1,3))
  plot(ajuste$e, type = "l", main = paste("Residuos", titulo))
  hist(ajuste$e, main = paste("Histograma", titulo))
  acf(ajuste$e,  main = paste("ACF Residuos", titulo))
  par(mfrow = c(1,1))
}

calcular_metricas_arma <- function(modelo, y, p, q){
  ajuste <- ajustar_ARMA(modelo$coef, y, p, q)
  e     <- ajuste$e
  y_hat <- ajuste$y_hat
  
  # Excluir posiciones iniciales donde y_hat = 0
  inicio <- max(p, q) + 1
  e_valido     <- e[inicio:length(e)]
  y_valido     <- y[inicio:length(y)]
  y_hat_valido <- y_hat[inicio:length(y_hat)]
  
  ecm   <- mean(e_valido^2, na.rm = TRUE)
  recm  <- sqrt(ecm)
  mae   <- mean(abs(y_valido - y_hat_valido), na.rm = TRUE)
  mape  <- mean(abs((y_valido - y_hat_valido) / y_valido), na.rm = TRUE) * 100
  
  return(list(ECM = ecm, RECM = recm, MAE = mae, MAPE = mape))
}


#Estimar modelos 
arma11 <- estimar_ARMA(dl_y_limpio, p = 1, q = 1)
arma12 <- estimar_ARMA(dl_y_limpio, p = 1, q = 2)
arma21 <- estimar_ARMA(dl_y_limpio, p = 2, q = 1)

#Graficos de cada modelo
graficar_ARMA(arma11, dl_y_limpio, p = 1, q = 1, titulo = "ARMA(1,1)")
graficar_ARMA(arma12, dl_y_limpio, p = 1, q = 2, titulo = "ARMA(1,2)")
graficar_ARMA(arma21, dl_y_limpio, p = 2, q = 1, titulo = "ARMA(2,1)")

graficar_residuos(arma11, dl_y_limpio, p = 1, q = 1, titulo = "ARMA(1,1)")
graficar_residuos(arma12, dl_y_limpio, p = 1, q = 2, titulo = "ARMA(1,2)")
graficar_residuos(arma21, dl_y_limpio, p = 2, q = 1, titulo = "ARMA(2,1)")

#Metricas
metricas11 <- calcular_metricas_arma(arma11, dl_y_limpio, p = 1, q = 1)
metricas12 <- calcular_metricas_arma(arma12, dl_y_limpio, p = 1, q = 2)
metricas21 <- calcular_metricas_arma(arma21, dl_y_limpio, p = 2, q = 1)

cat("ARMA(1,1):\n"); print(metricas11)
cat("\nARMA(1,2):\n"); print(metricas12)
cat("\nARMA(2,1):\n"); print(metricas21)

# Tabla de metricas 
comparacion_modelos <- data.frame(
  Modelo = c("Holt-Winter", "ARMA(1,1)", "ARMA(1,2)", "ARMA(2,1)"),
  MAE    = c(mae_hw, metricas11$MAE, metricas12$MAE, metricas21$MAE),
  RECM   = c(recm_hw, metricas11$RECM, metricas12$RECM, metricas21$RECM),
  ECM    = c(ecm_hw, metricas11$ECM, metricas12$ECM, metricas21$ECM)
)

print(comparacion_modelos)


#Predicciones de ARMA 
ajuste11 <- ajustar_ARMA(arma11$coef, dl_y_limpio, p = 1, q = 1)
ajuste12 <- ajustar_ARMA(arma12$coef, dl_y_limpio, p = 1, q = 2)
ajuste21 <- ajustar_ARMA(arma21$coef, dl_y_limpio, p = 2, q = 1)

predicciones_tabla <- data.frame(
  Real   = dl_y_limpio,
  ARMA11 = ajuste11$y_hat,
  ARMA12 = ajuste12$y_hat,
  ARMA21 = ajuste21$y_hat
)
head(predicciones_tabla, 20)


# Extraer residuos de cada modelo
residuos11 <- ajuste11$e
residuos12 <- ajuste12$e
residuos21 <- ajuste21$e

# Quitar los ceros iniciales
inicio11 <- max(1,1) + 1  # = 2
inicio12 <- max(1,2) + 1  # = 3
inicio21 <- max(2,1) + 1  # = 3

res11_limpio <- residuos11[inicio11:length(residuos11)]
res12_limpio <- residuos12[inicio12:length(residuos12)]
res21_limpio <- residuos21[inicio21:length(residuos21)]

#Reintegración de los modelos: quitar las diferenciaciones para poder comparar
#Metricas 

# Reintegrar modelo ARMA a escala real
reintegrar_viviendas <- function(ajuste_arma, y, n) {
  y_hat_diff <- ajuste_arma$y_hat
  m <- length(y_hat_diff)
  y_hat_real <- numeric(n)
  
  for(i in 1:m) {
    t <- i + 14 
    if(t <= n) {
      y_hat_real[t] <- y[t-1] + y[t-12] - y[t-13] + y_hat_diff[i]
    }
  }
  return(y_hat_real)
}

# Reconstruir las predicciones reales en escala de viviendas para los 3 modelos
y_hat_real_11 <- reintegrar_viviendas(ajuste11, y, n)
y_hat_real_12 <- reintegrar_viviendas(ajuste12, y, n)
y_hat_real_21 <- reintegrar_viviendas(ajuste21, y, n)

# Definir el periodo donde todos tienen predicciones válidas
periodo_evaluacion <- 15:n

# Calcular los verdaderos errores en escala de viviendas
err_11_real <- y[periodo_evaluacion] - y_hat_real_11[periodo_evaluacion]
err_12_real <- y[periodo_evaluacion] - y_hat_real_12[periodo_evaluacion]
err_21_real <- y[periodo_evaluacion] - y_hat_real_21[periodo_evaluacion]

#  Calcular métricas reales para ARMA(1,1)
mae_11  <- mean(abs(err_11_real), na.rm = TRUE)
ecm_11  <- mean(err_11_real^2, na.rm = TRUE)
recm_11 <- sqrt(ecm_11)
mape_11 <- mean(abs(err_11_real / y[periodo_evaluacion]), na.rm = TRUE) * 100

# Calcular métricas reales para ARMA(1,2)
mae_12  <- mean(abs(err_12_real), na.rm = TRUE)
ecm_12  <- mean(err_12_real^2, na.rm = TRUE)
recm_12 <- sqrt(ecm_12)
mape_12 <- mean(abs(err_12_real / y[periodo_evaluacion]), na.rm = TRUE) * 100

# Calcular métricas reales para ARMA(2,1)
mae_21  <- mean(abs(err_21_real), na.rm = TRUE)
ecm_21  <- mean(err_21_real^2, na.rm = TRUE)
recm_21 <- sqrt(ecm_21)
mape_21 <- mean(abs(err_21_real / y[periodo_evaluacion]), na.rm = TRUE) * 100

# TABLA COMPARATIVA DE METRICAS
comparacion_completa_real <- data.frame(
  Modelo = c("Holt-Winter", "ARIMA(1,1,1) Real", "ARIMA(1,1,2) Real", "ARIMA(2,1,1) Real"),
  MAE    = c(8.484688, mae_11, mae_12, mae_21),
  RECM   = c(11.01188, recm_11, recm_12, recm_21),
  ECM    = c(121.2616, ecm_11, ecm_12, ecm_21),
  MAPE   = c(7.534807, mape_11, mape_12, mape_21)
)

print(comparacion_completa_real)


# AIC DE TODOS LOS MODELOS

calcular_aic <- function(residuos, k) {
  n   <- length(residuos)
  sse <- sum(residuos^2)
  n * log(sse / n) + 2 * k
}


res11_limpio <- ajuste11$e[(max(1,1)+1) : length(ajuste11$e)]
res12_limpio <- ajuste12$e[(max(1,2)+1) : length(ajuste12$e)]
res21_limpio <- ajuste21$e[(max(2,1)+1) : length(ajuste21$e)]

head(res12_limpio)
sum(is.na(res12_limpio))

# AIC por modelo
aic_hw    <- calcular_aic(errores_opt_p_2, k = 3)  
aic_arma11 <- calcular_aic(res11_limpio,   k = 3)  
aic_arma12 <- calcular_aic(res12_limpio,   k = 4)  
aic_arma21 <- calcular_aic(res21_limpio,   k = 4)  

# Tabla comparativa
aic_values <- data.frame(
  Modelo = c("Holt-Winter", "ARMA(1,1)", "ARMA(1,2)", "ARMA(2,1)"),
  AIC    = c(aic_hw, aic_arma11, aic_arma12, aic_arma21)
)

aic_values[order(aic_values$AIC), ]


#Evaluación de supuestos de Modelos ARMA que utilizan la serie diferenciada 
#Realizamos las 2 diferenciaciones (de tendencia y estacional)antes de aplicar ARMA
#por lo que se podria entender teoricamente que se realiza un modelo ARIMA pero de forma manual

modelo_a_evaluar <- arma12    
ajuste_actual    <- ajuste12
p_actual <- 1                 
q_actual <- 2               
titulo_modelo <- paste0("ARMA(", p_actual, q_actual, ")")


# Se eliminan los ceros iniciales de los residuos 
nn <- max(p_actual, q_actual) + 1
residuos_evaluar <- ajuste_actual$e[nn:length(ajuste_actual$e)]

#COMPROBACION DE SUPUESTOS PARA MODELO ARMA 12
# Supuesto1: Media cero
mean_residuos <- mean(residuos_evaluar, na.rm = TRUE)
mean_residuos


#Supuesto2: Varianza constante en los residuos
plot(residuos_evaluar, type = "l", col = "darkblue",
     main = paste("Residuos a lo largo del tiempo -", titulo_modelo),
     ylab = "Residuos", xlab = "Tiempo")
abline(h = 0, col = "red", lty = 2)


#Supuesto3: Independencia

acf(residuos_evaluar)
pacf(residuos_evaluar)


# SUPUESTO 4: Distribucion normal 
media_res <- mean(residuos_evaluar, na.rm = TRUE)
media_res
sd_res    <- sd(residuos_evaluar, na.rm = TRUE)
sd_res

#proporción de los residuos que se encuentran fuera del rango de mas o menos 2 desviaciones estándar
fuera <- sum(residuos_evaluar < (media_res - 2*sd_res) | residuos_evaluar > (media_res + 2*sd_res), na.rm = TRUE)
prop_fuera <- fuera / length(residuos_evaluar)
#Ideal cercano a 0.05
prop_fuera

#Histograma para verificar normalidad
hist(residuos_evaluar, breaks = 20, col = "lightgray", prob = TRUE,
     main = paste("Histograma de Residuos -", titulo_modelo), xlab = "Residuos")
curve(dnorm(x, mean = media_res, sd = sd_res), add = TRUE, col = "#5D478B", lwd = 2)


fuera_outliers <- which(residuos_evaluar < (media_res - 3*sd_res) | residuos_evaluar > (media_res + 3*sd_res))
fuera_outliers


# Supuesto6:Modelo parcimonioso
coeficientes <- modelo_a_evaluar$coef
if(is.null(names(coeficientes)) || all(names(coeficientes) == "")){
  
  nombres <- "Constante"
  if(p_actual > 0) nombres <- c(nombres, paste0("Phi_", 1:p_actual))
  if(q_actual > 0) nombres <- c(nombres, paste0("Theta_", 1:q_actual))
  names(coeficientes) <- nombres
}

parsimonia <- data.frame(
  Parametro  = names(coeficientes),
  Estimacion = coeficientes,
  # Evalúa si el parámetro aporta poco o nada
  CasiCero   = abs(coeficientes) < 0.05 
)
parsimonia


#Supuestos para Holt-winter 

#1. Residuos con media Cero
#Media 
mean(errores_opt_p_2, na.rm = TRUE)
#Prueba de hipotesis: H0:Residuos con media cero (valorP>0.05)
#H1: la media pobracional es distinta 
t.test(errores_opt_p_2, mu = 0)

#2 Histograma de los errores: Comprobar normalidad de los errores
hist(errores_opt_p_2, main = "Histograma residuos", 
     xlab = "Error", col = "steelblue")    


#3. Residuos sin autocorrelación
acf(errores_opt_p_2)
Box.test(errores_opt_p_2, lag = 20, type = "Ljung-Box")


#4.Varianza de los residuos 
plot(errores_opt_p_2, type = "l", col = "blue",
     main = "Residuos a lo largo del tiempo",
     ylab = "Error", xlab = "Tiempo")
abline(h = 0, col = "red", lty = 2)
n <- length(errores_opt_p_2)
var1 <- var(errores_opt_p_2[1:(n/2)], na.rm = TRUE)
var2 <- var(errores_opt_p_2[(n/2+1):n], na.rm = TRUE)
c(var1, var2)


#Se llego a la conclusion de que el mejor modelo es Holt-winter por las metricas 
#Y aunque no cumple todos los supuestos, los supuestos de autocorrelacion son mejores 
#en este modelo.

# PREDECIR LOS 6 MESES despues de 2021-05-01

# Traer los datos y la serie ya creados en el modelo de prueba de hw
alpha_opt <- optimos$par[1]
beta_opt  <- optimos$par[2]
gamma_opt <- optimos$par[3]

resultados_hw_opt_p_2 <- holt_winter_p_2(datos2$Viviendas,
                                         alpha = alpha_opt,
                                         beta  = beta_opt,
                                         gamma = gamma_opt,
                                         p     = 12)

# Extraer valores del modelo de prueba optimizado 
n        <- nrow(resultados_hw_opt_p_2)
F_ultimo <- resultados_hw_opt_p_2$nivel[n]        
T_ultimo <- resultados_hw_opt_p_2$tendencia[n]    
S_ciclo  <- resultados_hw_opt_p_2$estacional[(n - 11):n]  

# Fechas reales de pronóstico
fechas_pron <- seq(as.Date("2021-06-01"), by = "month", length.out = 6)

# Calcular pronóstico 
y_pron <- numeric(6)
for (m in 1:6) {
  idx_est   <- ((m - 1) %% 12) + 1
  y_pron[m] <- (F_ultimo + m * T_ultimo) * S_ciclo[idx_est]
}

# Tabla de pronostico 
pron_6m <- data.frame(
  Fecha      = format(fechas_pron, "%Y-%m"),
  Pronostico = round(y_pron, 2)
)

pron_6m

#Grafico serie de tiempo con valores predichos 
col_real  <- "#2c3e50"
col_ajust <- "magenta3"
col_pron  <- "navy"

fechas_hist <- seq(as.Date("1959-01-01"), by = "month", length.out = n)

plot(fechas_hist, resultados_hw_opt_p_2$real,
     type = "l", col = col_real, lwd = 1,
     xlim = c(as.Date("1959-01-01"), as.Date("2022-06-01")),
     ylim = c(40, max(resultados_hw_opt_p_2$real, y_pron) * 1.08),
     main = "Holt-Winter multiplicativo pronostico",
     xlab = "Fecha", ylab = "Nuevas viviendas")

lines(fechas_hist[13:n], resultados_hw_opt_p_2$pronostico[13:n],
      col = col_ajust, lwd = 1, lty = 1)

abline(v = as.Date("2021-05-01"), col = "gray60", lty = 3, lwd = 1.5)

lines(fechas_pron, y_pron, col = col_pron, lwd = 2, lty = 1)

legend("topleft",
       legend = c("Serie Historica", "Ajustado Holt-winter", "Pronóstico Jun–Nov 2021"),
       col    = c(col_real, col_ajust, col_pron),
       lwd    = c(1, 1, 2),
       lty    = c(1, 1, 1),
       bty    = "n", cex = 0.85)

# Gráfico solo de los valores predichos
plot(pron_6m$Pronostico, type = "o", col = "orangered",
     main = "Pronóstico HW - 6 meses",
     xlab = "6 meses futuros", ylab = "Diferencia de las viviendas")

