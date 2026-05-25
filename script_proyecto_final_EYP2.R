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
modelo_arma.2.1_p_1 <- estimar_ARMA_p_1(datos_1_limpio$yt, p = 2, q = 1)
modelo_arma.1.2_p_1 <- estimar_ARMA_p_1(datos_1_limpio$yt, p = 1, q = 2)
modelo_arma.2.2_p_1 <- estimar_ARMA_p_1(datos_1_limpio$yt, p = 2, q = 2)

resultado_arma.0.1_p_1 <- fitted_ARMA_p_1(modelo_arma.0.1_p_1$coef, datos_1_limpio$yt, p = 0, q = 1)
resultado_arma.1.0_p_1 <- fitted_ARMA_p_1(modelo_arma.1.0_p_1$coef, datos_1_limpio$yt, p = 1, q = 0)
resultado_arma.1.1_p_1 <- fitted_ARMA_p_1(modelo_arma.1.1_p_1$coef, datos_1_limpio$yt, p = 1, q = 1)
resultado_arma.2.1_p_1 <- fitted_ARMA_p_1(modelo_arma.2.1_p_1$coef, datos_1_limpio$yt, p = 2, q = 1)
resultado_arma.1.2_p_1 <- fitted_ARMA_p_1(modelo_arma.1.2_p_1$coef, datos_1_limpio$yt, p = 1, q = 2)
resultado_arma.2.2_p_1 <- fitted_ARMA_p_1(modelo_arma.2.2_p_1$coef, datos_1_limpio$yt, p = 2, q = 2)

metricas_arma.0.1_p_1  <- metricas_error_mov(resultado_arma.0.1_p_1)
metricas_arma.1.0_p_1  <- metricas_error_mov(resultado_arma.1.0_p_1)
metricas_arma.1.1_p_1  <- metricas_error_mov(resultado_arma.1.1_p_1)
metricas_arma.2.1_p_1  <- metricas_error_mov(resultado_arma.2.1_p_1)
metricas_arma.1.2_p_1  <- metricas_error_mov(resultado_arma.1.2_p_1)
metricas_arma.2.2_p_1  <- metricas_error_mov(resultado_arma.2.2_p_1)

metricas_arma.0.1_p_1
metricas_arma.1.0_p_1
metricas_arma.1.1_p_1
metricas_arma.2.1_p_1
metricas_arma.1.2_p_1
metricas_arma.2.2_p_1

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
