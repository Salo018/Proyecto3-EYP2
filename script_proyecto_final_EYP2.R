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

###### Subir mi punto desde aquí ######

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
  geom_line(color = "navy") +
  geom_point(color = "navy") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo del consumo personal (1960-2016)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Graficar solo los años de 1960 a 1990 (esto es solo para probar)
ggplot(data = df.consumo_copy %>% filter(Año >= 1960 & Año <= 1990),
       aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "navy") +
  geom_point(color = "navy") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo del consumo personal (1960-1990)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# PRUEBAS DE MÉTODOS DE IMPUTACIÓN

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
  geom_line(color = "navy") +
  geom_point(aes(color = abs(z_score) > 3)) +
  scale_color_manual(values = c("black", "red")) +
  labs(title = "Serie con posibles outliers")

# PROCEDIMIENTO PARA EVALUAR METODOS DE IMPUTACIÓN

# Crear nulos artificiales 5% 

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
set.seed(2242055)  # semilla para reproducibilidad
pos_extra <- sample(setdiff(1:n_total, pos_outliers), faltan)

# Unir todas las posiciones
pos_nulos <- c(pos_outliers, pos_extra)

# Crear nulos artificiales
df.nulos_artificiales <- df.sin_nulos
df.nulos_artificiales$Consumo[pos_nulos] <- NA

# IMPUTACIÓN 

## FUNCIONES DE EVALUACIÓN

# Función para ECM
calc_ecm <- function(original, imputado, posiciones) {
  mean((original[posiciones] - imputado[posiciones])^2, na.rm = TRUE)
}
# Función para MAE 
calc_mae <- function(real, imputado, pos) {
  mean(abs(real[pos] - imputado[pos]), na.rm = TRUE)
}
# Función para MAPE
calc_mape <- function(real, imputado, pos) {
  valid <- real[pos] != 0 & !is.na(imputado[pos])
  mean(abs((real[valid] - imputado[valid]) / real[valid]), na.rm = TRUE) * 100
}
# Función para RMSE
calc_rmse <- function(real, imputado, pos) {
  sqrt(mean((real[pos] - imputado[pos])^2, na.rm = TRUE))
}

# Imputación con distintos métodos

# Imputación con MEDIA
media_val <- mean(df.sin_nulos$Consumo, na.rm = TRUE)
df.media <- df.nulos_artificiales
df.media$Consumo[is.na(df.media$Consumo)] <- media_val
# Comparación contra valores reales en df.sin_nulos
ecm_media <- calc_ecm(df.sin_nulos$Consumo, df.media$Consumo, pos_nulos)
mae_media <- calc_mae(df.sin_nulos$Consumo, df.media$Consumo, pos_nulos)
mape_media <- calc_mape(df.sin_nulos$Consumo, df.media$Consumo, pos_nulos)
rmse_media <- calc_rmse(df.sin_nulos$Consumo, df.media$Consumo, pos_nulos)

# Imputación con MEDIANA
mediana_val <- median(df.sin_nulos$Consumo, na.rm = TRUE)
df.mediana <- df.nulos_artificiales
df.mediana$Consumo[is.na(df.mediana$Consumo)] <- mediana_val
# Comparación contra valores reales en df.sin_nulos
ecm_mediana <- calc_ecm(df.sin_nulos$Consumo, df.mediana$Consumo, pos_nulos)
mae_mediana <- calc_mae(df.sin_nulos$Consumo, df.mediana$Consumo, pos_nulos)
mape_mediana <- calc_mape(df.sin_nulos$Consumo, df.mediana$Consumo, pos_nulos)
rmse_mediana <- calc_rmse(df.sin_nulos$Consumo, df.mediana$Consumo, pos_nulos)

# Imputación con MODA
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
mae_moda <- calc_mae(df.sin_nulos$Consumo, df.moda$Consumo, pos_nulos)
mape_moda <- calc_mape(df.sin_nulos$Consumo, df.moda$Consumo, pos_nulos)
rmse_moda <- calc_rmse(df.sin_nulos$Consumo, df.moda$Consumo, pos_nulos)

# Imputación con INTERPOLACIÓN LINEAL
df.interp <- df.nulos_artificiales
df.interp$Consumo <- na.approx(df.interp$Consumo, na.rm = FALSE)
# Comparación contra valores reales en df.sin_nulos
ecm_interp <- calc_ecm(df.sin_nulos$Consumo, df.interp$Consumo, pos_nulos)
mae_interp <- calc_mae(df.sin_nulos$Consumo, df.interp$Consumo, pos_nulos)
mape_interp <- calc_mape(df.sin_nulos$Consumo, df.interp$Consumo, pos_nulos)
rmse_interp <- calc_rmse(df.sin_nulos$Consumo, df.interp$Consumo, pos_nulos)

# Imputación con MEDIAS MOVILES
# Medias Móviles con ventana = 3
k <- 3
df.mm <- df.nulos_artificiales
for (i in which(is.na(df.mm$Consumo))) {
  # Definimos la ventana alrededor del na
  idx <- max(1, i - k %/% 2) : min(nrow(df.mm), i + k %/% 2)
  idx <- idx[idx != i] # excluimos el propio na
  df.mm$Consumo[i] <- mean(df.mm$Consumo[idx], na.rm = TRUE)
}
# Comparación contra valores reales en df.sin_nulos
ecm_mm <- calc_ecm(df.sin_nulos$Consumo, df.mm$Consumo, pos_nulos)
mae_mm <- calc_mae(df.sin_nulos$Consumo, df.mm$Consumo, pos_nulos)
mape_mm <- calc_mape(df.sin_nulos$Consumo, df.mm$Consumo, pos_nulos)
rmse_mm <- calc_rmse(df.sin_nulos$Consumo, df.mm$Consumo, pos_nulos)

# Imputación con LOCF
df.locf <- df.nulos_artificiales
df.locf$Consumo <- na.locf(df.locf$Consumo, option = "locf")
# Comparación contra valores reales en df.sin_nulos
ecm_locf <- calc_ecm(df.sin_nulos$Consumo, df.locf$Consumo, pos_nulos)
mae_locf <- calc_mae(df.sin_nulos$Consumo, df.locf$Consumo, pos_nulos)
mape_locf <- calc_mape(df.sin_nulos$Consumo, df.locf$Consumo, pos_nulos)
rmse_locf <- calc_rmse(df.sin_nulos$Consumo, df.locf$Consumo, pos_nulos)

# COMPARAR RESULTADOS 

# Comparar resultados en un df
resultados <- data.frame(
  Metodo = c("Media", "Mediana", "Moda", "Interpolación", "Medias Móviles", "LOCF"),
  ECM = c(ecm_media, ecm_mediana, ecm_moda, ecm_interp, ecm_mm, ecm_locf),
  MAE = c(mae_media, mae_mediana, mae_moda, mae_interp, mae_mm, mae_locf),
  MAPE = c(mape_media, mape_mediana, mape_moda, mape_interp, mape_mm, mape_locf),
  RMSE = c(rmse_media, rmse_mediana, rmse_moda, rmse_interp, rmse_mm, rmse_locf)
)

print(resultados)

# Top 3 métodos con mejores resultados
mejores_ecm <- resultados[order(resultados$ECM), ][1:3, c("Metodo","ECM")]
print(mejores_ecm)

mejores_mae <- resultados[order(resultados$MAE), ][1:3, c("Metodo","MAE")]
print(mejores_mae)

mejores_mape <- resultados[order(resultados$MAPE), ][1:3, c("Metodo","MAPE")]
print(mejores_mape)

mejores_rmse <- resultados[order(resultados$RMSE), ][1:3, c("Metodo","RMSE")]
print(mejores_rmse)

# Comparación con los métodos con mejores resultados

# Valores originales en las posiciones de nulos
valores_reales <- df.sin_nulos$Consumo[pos_nulos]

# Valores imputados por cada método
valores_moda <- df.moda$Consumo[pos_nulos]
valores_locf <- df.locf$Consumo[pos_nulos]
valores_mm <- df.mm$Consumo[pos_nulos]

# Df comparativo
comparacion <- data.frame(
  Posicion = pos_nulos,
  Real = valores_reales,
  Moda = valores_moda,
  LOCF = valores_locf,
  MM = valores_mm
)

print(comparacion)

# Gráfico comparativo Moda vs LOCF vs MM
ggplot(comparacion, aes(x = Posicion)) +
  geom_point(aes(y = Real), color = "navy", size = 2.5) +
  geom_point(aes(y = Moda), color = "darkorange", size = 2) +
  geom_point(aes(y = LOCF), color = "limegreen", size = 2) +
  geom_point(aes(y = MM), color = "deeppink", size = 2) +
  labs(title = "Comparación imputación: Moda vs LOCF vs MM",
       y = "Consumo", x = "Posición (índice)") +
  theme_minimal()

# Mostrar las varianzas de cada método y del original
data.frame(
  Metodo = c("Original", "Moda", "Medias Móviles", "LOCF"),
  Varianza = c(
    var(df.sin_nulos$Consumo, na.rm = TRUE),
    var(df.moda$Consumo, na.rm = TRUE),
    var(df.mm$Consumo, na.rm = TRUE),
    var(df.locf$Consumo, na.rm = TRUE)
  )
)

# VISUALIZAR LOS 3 MÉTODOS APLICADOS PARA ESCOGER EL MEJOR

# Moda
df.moda_copy <- df.moda %>%
  mutate(Trimestre_num = gsub("Trimestre_", "", Trimestre),
         Periodo = paste(Año, Trimestre_num, sep = "-"))
# Grafica  
ggplot(data = df.moda_copy, aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "chocolate") +
  geom_point(color = "chocolate") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo imputada con moda"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Locf
df.locf_copy <- df.locf %>%
  mutate(Trimestre_num = gsub("Trimestre_", "", Trimestre),
         Periodo = paste(Año, Trimestre_num, sep = "-"))
# Grafica  
ggplot(data = df.locf_copy, aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "darkgreen") +
  geom_point(color = "darkgreen") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo imputada con locf"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Medias móviles
df.mm_copy <- df.mm %>%
  mutate(Trimestre_num = gsub("Trimestre_", "", Trimestre),
         Periodo = paste(Año, Trimestre_num, sep = "-"))
# Grafica 
ggplot(data = df.mm_copy, aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "maroon") +
  geom_point(color = "maroon") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo imputada con mm"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Comparar con el df de prueba

# Grafica con todos los años 
ggplot(data = df.sin_nulos_copy, aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "cornflowerblue") +
  geom_point(color = "cornflowerblue") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo prueba"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# IMPUTACIÓN AL DF ORIGINAL CON MM
k <- 3
df.consumo_imp <- df.consumo_copy

for (i in which(is.na(df.consumo_imp$Consumo))) {
  # Definimos la ventana alrededor del na
  idx <- max(1, i - k %/% 2) : min(nrow(df.consumo_imp), i + k %/% 2)
  idx <- idx[idx != i]
  df.consumo_imp$Consumo[i] <- mean(df.consumo_imp$Consumo[idx], na.rm = TRUE)
}

data.frame(
  Metodo = c("Original", "Medias Móviles"),
  Varianza = c(
    var(df.consumo_copy$Consumo, na.rm = TRUE),
    var(df.consumo_imp$Consumo, na.rm = TRUE)
  )
)

# Revisar visualmente
# Original
ggplot(data = df.consumo_copy, aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "navy") +
  geom_point(color = "navy") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo original"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Imputado
ggplot(data = df.consumo_imp, aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "purple4") +
  geom_point(color = "purple4") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo original imputada"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Comprobación de componentes de la serie de tiempo 
# Descomposición
consumo_ts <- ts(df.consumo_imp$Consumo,
                 start = c(1970, 1),   # año inicial y trimestre inicial
                 frequency = 4)        # frecuencia trimestral

# Descomposición aditiva
descomp_add <- decompose(consumo_ts, type = "additive")
plot(descomp_add) 

# Comprobacion si la serie es estacionaria con acf y pacf
acf(df.consumo_imp$Consumo)
pacf(df.consumo_imp$Consumo)

# Comprobacion si la serie es estacionaria dividiendola en partes
# Dividir en 4 partes 
p3_parte_1 <- df.consumo_imp$Consumo[1:47]
p3_parte_2 <- df.consumo_imp$Consumo[48:93]
p3_parte_3 <- df.consumo_imp$Consumo[94:140]
p3_parte_4 <- df.consumo_imp$Consumo[141:187]

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
cat("Media total:", mean(df.consumo_imp$Consumo))
cat("Varianza total:", var(df.consumo_imp$Consumo))

# SERIE NO ESTACIONARIA

plot.ts(consumo_ts)

# MODELOS 

#Probar primer modelo: Suavización exponencial Holt-winter
#p=4 porque la estacionalidad es trimestral
holt_winter_p_3 <- function(y, alpha = 0.3, beta = 0.2, gamma = 0.1, p = 4) {
  n <- length(y)
  
  F <- numeric(n)      
  T <- numeric(n)     
  S <- numeric(n)     
  y_hat <- numeric(n) 
  
  F[p] <- mean(y[1:p])
  
  T[p] <- (mean(y[(p+1):(2*p)]) - mean(y[1:p])) / p
  
  for (i in 1:p) {
    S[i] <- mean(y[seq(i, p*2, by=p)]) / F[p]
  }
  
  # Iteración Holt-Winter
  for (t in (p+1):n) {
    # Nivel
    F[t] <- alpha * (y[t] / S[t-p]) + (1 - alpha) * (F[t-1] + T[t-1])
    # Tendencia
    T[t] <- gamma * (F[t] - F[t-1]) + (1 - gamma) * T[t-1]
    # Estacionalidad
    S[t] <- beta  * (y[t] / F[t])   + (1 - beta)  * S[t-p]
    # Pronóstico
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

# Serie de consumo trimestral
y <- df.consumo_imp$Consumo

# 1. Optimización de parámetros con p=4
rmse_hw <- function(params) {
  alpha <- params[1]; beta <- params[2]; gamma <- params[3]
  if (any(params <= 0) || any(params >= 1)) return(Inf)
  
  res <- holt_winter_p_3(y, alpha, beta, gamma, p = 4)
  errores <- res$error[(4+1):nrow(res)]  # desde p+1
  sqrt(mean(errores^2, na.rm = TRUE))
}

optimos <- optim(
  par    = c(0.3, 0.2, 0.1),
  fn     = rmse_hw,
  method = "L-BFGS-B",
  lower  = c(0.01, 0.01, 0.01),
  upper  = c(0.99, 0.99, 0.99)
)

alpha_opt <- optimos$par[1]
beta_opt  <- optimos$par[2]
gamma_opt <- optimos$par[3]

cat("Alpha óptimo:", round(alpha_opt, 4), "\n")
cat("Beta óptimo: ", round(beta_opt, 4), "\n")
cat("Gamma óptimo:", round(gamma_opt, 4), "\n")
cat("RMSE mínimo: ", round(optimos$value, 4), "\n")

# 2. Aplicar con parámetros óptimos
resultados_hw_opt <- holt_winter_p_3(y,
                                     alpha = alpha_opt,
                                     beta  = beta_opt,
                                     gamma = gamma_opt,
                                     p     = 4)

# 3. Métricas de error (desde p+1 = 5)
errores_opt_p_3 <- resultados_hw_opt$error[5:nrow(resultados_hw_opt)]
reales_opt  <- resultados_hw_opt$real[5:nrow(resultados_hw_opt)]

mae_hw <- mean(abs(errores_opt_p_3), na.rm = TRUE)
rmse_hw <- sqrt(mean(errores_opt_p_3^2, na.rm = TRUE))
ecm_hw <- mean(errores_opt_p_3^2, na.rm = TRUE)   # ECM = MSE

cat("MAE : ", round(mae_hw, 2), "\n")
cat("RMSE:", round(rmse_hw, 2), "\n")
cat("ECM :", round(ecm_hw, 2), "\n")

# 4. Gráfico comparativo
ggplot(resultados_hw_opt[5:nrow(resultados_hw_opt), ],
       aes(x = tiempo)) +
  geom_line(aes(y = real,       color = "Real"), linewidth = 0.8) +
  geom_line(aes(y = pronostico, color = "Pronóstico"), linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("Real" = "navy", "Pronóstico" = "maroon2")) +
  labs(title = "Holt-Winters (trimestral, p=4)",
       x = "Tiempo", y = "Consumo", color = "") +
  theme_minimal()

# Aplicar diferenciación para probar modelos ARMA Y AR
y <- df.consumo_imp$Consumo
n <- length(y)

# Diferenciación de primer orden
dl_y <- numeric(n)
for (t in 2:n) {
  dl_y[t] <- y[t] - y[t-1]
}

# Segunda diferenciación (aplicada sobre la primera)
dl_y2 <- numeric(n)
for (t in 3:n) {
  dl_y2[t] <- dl_y[t] - dl_y[t-1]
}

# Graficar original vs diferenciada
plot.ts(y, main = "Serie original (Consumo)", col = "navy")
plot.ts(dl_y, main = "Diferenciada 1 vez", col = "red")
plot.ts(dl_y2, main = "Diferenciada 2 veces", col = "olivedrab")

acf(dl_y)
pacf(dl_y)

acf(dl_y2)
pacf(dl_y2)

acf(consumo_ts)
pacf(consumo_ts)

## Ajustar un ARMA sobre la primera diferencia y comparar métricas 
# Funciones ARMA RMA(1,1), ARMA(1,2) y ARMA(2,1)
y <- dl_y[-1]
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
    
    if(p > 0){
      for(i in 1:p){
        ar_part <- ar_part + phi[i] * y[t-i]
      }
    }
    if(q > 0){
      for(j in 1:q){
        ma_part <- ma_part + theta[j] * e[t-j]
      }
    }
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
    if(p > 0){
      for(i in 1:p){
        ar_part <- ar_part + phi[i] * y[t-i]
      }
    }
    if(q > 0){
      for(j in 1:q){
        ma_part <- ma_part + theta[j] * e[t-j]
      }
    }
    y_hat[t] <- cte + ar_part + ma_part
    e[t] <- y[t] - y_hat[t]
  }
  return(list(y_hat = y_hat, e = e))
}

graficar_ARMA <- function(modelo, y, p, q, titulo){
  ajuste <- ajustar_ARMA(modelo$coef, y, p, q)
  
  plot(y, type = "l", col = "red",
       main = titulo,
       ylab = "Serie simulada", xlab = "Tiempo")
  lines(ajuste$y_hat, col = "navy")

}

graficar_residuos <- function(modelo, y, p, q, titulo){
  ajuste <- ajustar_ARMA(modelo$coef, y, p, q)
  
  # residuos
  par(mfrow = c(1,3))
  plot(ajuste$e, type = "l", main = paste("Residuos", titulo))
  hist(ajuste$e, main = paste("Histograma", titulo))
  acf(ajuste$e, main = paste("ACF Residuos", titulo))
  par(mfrow = c(1,1))
}

# Estimar modelos
arma11 <- estimar_ARMA(y, p = 1, q = 1)
arma12 <- estimar_ARMA(y, p = 1, q = 2)
arma21 <- estimar_ARMA(y, p = 2, q = 1)

# Graficar resultados
graficar_ARMA(arma11, y, p = 1, q = 1, titulo = "ARMA(1,1)")
graficar_ARMA(arma12, y, p = 1, q = 2, titulo = "ARMA(1,2)")
graficar_ARMA(arma21, y, p = 2, q = 1, titulo = "ARMA(2,1)")

# Graficar residuos
graficar_residuos(arma11, y, p = 1, q = 1, titulo = "ARMA(1,1)")
graficar_residuos(arma12, y, p = 1, q = 2, titulo = "ARMA(1,2)")
graficar_residuos(arma21, y, p = 2, q = 1, titulo = "ARMA(2,1)")

# Función para calcular métricas
calcular_metricas <- function(modelo, y, p, q){
  ajuste <- ajustar_ARMA(modelo$coef, y, p, q)
  
  # Residuos y predicciones
  e <- ajuste$e
  y_hat <- ajuste$y_hat
  
  # ECM (MSE)
  ecm <- mean(e^2, na.rm = TRUE)
  
  # RECM (antes RMSE)
  recm <- sqrt(ecm)
  
  # MAE
  mae <- mean(abs(y - y_hat), na.rm = TRUE)

  return(list(ECM = ecm, RECM = recm, MAE = mae))
}

# Calcular métricas para cada modelo
metricas11 <- calcular_metricas(arma11, y, p = 1, q = 1)
metricas12 <- calcular_metricas(arma12, y, p = 1, q = 2)
metricas21 <- calcular_metricas(arma21, y, p = 2, q = 1)

# Tabla comparativa
metricas_tabla <- data.frame(
  Modelo = c("ARMA(1,1)", "ARMA(1,2)", "ARMA(2,1)"),
  ECM = c(metricas11$ECM, metricas12$ECM, metricas21$ECM),
  RECM = c(metricas11$RECM, metricas12$RECM, metricas21$RECM),
  MAE = c(metricas11$MAE, metricas12$MAE, metricas21$MAE)
)

print(metricas_tabla)
cat("MAE : ", round(mae_hw, 2), "\n")
cat("RMSE:", round(rmse_hw, 2), "\n")
cat("ECM :", round(ecm_hw, 2), "\n")

# data.frames con predicciones de cada modelo
ajuste11 <- ajustar_ARMA(arma11$coef, y, p = 1, q = 1)
ajuste12 <- ajustar_ARMA(arma12$coef, y, p = 1, q = 2)
ajuste21 <- ajustar_ARMA(arma21$coef, y, p = 2, q = 1)

# Organizar en una sola tabla
predicciones_tabla <- data.frame(
  Real   = y,
  ARMA11 = ajuste11$y_hat,
  ARMA12 = ajuste12$y_hat,
  ARMA21 = ajuste21$y_hat
)

# Mostrar primeras filas
head(predicciones_tabla, 10)

# Probar modelo 5: AR (2)
ar2 <- estimar_ARMA(y, p = 2, q = 0)

# Ajustar el modelo para obtener predicciones
ajuste_ar2 <- ajustar_ARMA(ar2$coef, y, p = 2, q = 0)

# Extraer residuos y predicciones
e_ar2 <- ajuste_ar2$e
y_hat_ar2 <- ajuste_ar2$y_hat

# Calcular métricas
ECM  <- mean(e_ar2^2, na.rm = TRUE)
RECM <- sqrt(ECM)
MAE  <- mean(abs(e_ar2), na.rm = TRUE)

# Tabla de resultados
metricas_ar2 <- data.frame(
  Modelo = "AR(2)",
  ECM = ECM,
  RECM = RECM,
  MAE = MAE
)

print(metricas_ar2)

# Estimar AR(2) sobre la serie diferenciada
ar2 <- estimar_ARMA(dl_y, p = 2, q = 0)

# Graficar serie vs predicciones
graficar_ARMA(ar2, dl_y, p = 2, q = 0, titulo = "AR(2)")

# Calcular AIC para escoger el mejor modelo
# Función AIC 
calcular_aic <- function(residuos, k) {
  n   <- length(residuos)
  sse <- sum(residuos^2)
  n * log(sse / n) + 2 * k
}
# Residuos limpios de cada modelo 
res11_limpio <- ajuste11$e[(max(1,1)+1) : length(ajuste11$e)]
res12_limpio <- ajuste12$e[(max(1,2)+1) : length(ajuste12$e)]
res21_limpio <- ajuste21$e[(max(2,1)+1) : length(ajuste21$e)]
res_ar2      <- ajuste_ar2$e[(max(2,0)+1) : length(ajuste_ar2$e)]

# Holt-Winter
# k= 3 porque se estiman alpha, beta, gamma
aic_hw     <- calcular_aic(errores_opt_p_3, k = 3)
# ARMA(1,1): cte, ar1, ma1 → k = 3
aic_arma11 <- calcular_aic(res11_limpio, k = 3)
# ARMA(1,2): cte, ar1, ma1, ma2 → k = 4
aic_arma12 <- calcular_aic(res12_limpio, k = 4)
# ARMA(2,1): cte, ar1, ar2, ma1 → k = 4
aic_arma21 <- calcular_aic(res21_limpio, k = 4)
# AR(2): cte, ar1, ar2 → k = 3
aic_ar2    <- calcular_aic(res_ar2, k = 3)

# Tabla comparativa
aic_values <- data.frame(
  Modelo = c("Holt-Winter", "ARMA(1,1)", "ARMA(1,2)", "ARMA(2,1)", "AR(2)"),
  AIC    = c(aic_hw, aic_arma11, aic_arma12, aic_arma21, aic_ar2)
)
# Mostrar ordenados de menor a mayor AIC
print(aic_values[order(aic_values$AIC), ])
# Mejor modelo
best_model <- aic_values[which.min(aic_values$AIC), ]
cat("\nEl mejor modelo según AIC es:", best_model$Modelo,
    "con AIC =", round(best_model$AIC, 2), "\n")

# SUPUESTOS DE ARMA (1,2) modelo escogido
ajuste12 <- ajustar_ARMA(arma12$coef, y, p = 1, q = 2)
e <- ajuste12$e
nn <- max(1,2) + 1

# Supuesto 1: media cero
mean_residuos <- mean(e[nn:length(e)], na.rm = TRUE)
mean_residuos

# Supuesto 2: varianza constante
plot(e[nn:length(e)], type = "l", main = "Residuos ARMA(1,2)",
     ylab = "Residuos", xlab = "Tiempo")

# Supuesto 3: independencia
# (i) FAC y FACP 
acf(e[nn:length(e)])
pacf(e[nn:length(e)])
# (ii) Estadistico de Q de box  
Box.test(e[nn:length(e)], lag = 20, type = "Ljung-Box")

# Supuesto 4: distribución normal
media <- mean(e[nn:length(e)])
sd_res <- sd(e[nn:length(e)])
# (i) Proporción fuera de +-2sd desviaciones estándar
fuera <- sum(e[nn:length(e)] < (media - 2*sd_res) | e[nn:length(e)] > (media + 2*sd_res))
prop_fuera <- fuera / length(e[nn:length(e)])
cat("Proporción fuera de ±2sd:", round(prop_fuera,4), "\n")
# (ii) Histograma y prueba formal
hist(e[nn:length(e)], main = "Histograma residuos ARMA(1,2)")

# Supuesto 5: no existen observaciones aberrantes
# (i) Identificar residuos fuera de +-3sd
fuera_out <- which(e[nn:length(e)] < (media - 3*sd_res) | e[nn:length(e)] > (media + 3*sd_res))
cat("Índices de residuos fuera de +- 3:", fuera_out, "\n")

# Supuesto 6: el modelo considerado es parsiomonioso
coeficientes <- arma12$coef
# Asignar nombres si no existen
if(is.null(names(coeficientes)) || all(names(coeficientes) == "")){
  names(coeficientes) <- paste0("Coef", seq_along(coeficientes))
}
# Criterio: coeficiente cercano a 0 
parsimonia <- data.frame(
  Parametro  = names(coeficientes),
  Estimacion = coeficientes,
  CasiCero   = abs(coeficientes) < 0.05
)
print(parsimonia)

# Supuesto 7: el modelo es admisible 
phi <- arma12$coef[1]      # AR(1)
theta <- arma12$coef[2:3]  # MA(2)
# Polinomio AR: 1 - phi*L
ar_poly <- c(1, -phi)
ar_roots <- polyroot(ar_poly)
# Polinomio MA: 1 + theta1*L + theta2*L^2
ma_poly <- c(1, theta)
ma_roots <- polyroot(ma_poly)
print(ar_roots)
print(ma_roots)

# PRONÓSTICO DE 6 TRIMESTRES CON ARMA(1,2)
# Ajustar el modelo 
ajuste12 <- ajustar_ARMA(arma12$coef, y, p = 1, q = 2)
# Función de pronóstico hacia adelante
pronosticar_ARMA <- function(param, y, p, q, h){
  n <- length(y)
  cte <- param[1]
  phi <- if(p > 0) param[2:(p+1)] else numeric(0)
  theta <- if(q > 0) param[(p+2):(p+q+1)] else numeric(0)
  # Residuos y predicciones históricas
  ajuste <- ajustar_ARMA(param, y, p, q)
  e <- ajuste$e
  y_hat <- ajuste$y_hat
  # Pronósticos futuros
  futuros <- numeric(h)
  for(t in 1:h){
    ar_part <- 0
    ma_part <- 0
    # AR usa últimos valores observados 
    if(p > 0){
      for(i in 1:p){
        if(t - i <= 0){
          ar_part <- ar_part + phi[i] * y[n + (t - i)]
        } else {
          ar_part <- ar_part + phi[i] * futuros[t - i]
        }
      }
    }
    # MA usa últimos residuos, se asumen 0
    if(q > 0){
      for(j in 1:q){
        ma_part <- ma_part + theta[j] * 0
      }
    }
    
    futuros[t] <- cte + ar_part + ma_part
  }
  return(futuros)
}

# Pronóstico de 6 trimestres
pronostico_6 <- pronosticar_ARMA(arma12$coef, y, p = 1, q = 2, h = 6)
# Mostrar resultados
print(pronostico_6)
# Gráfico 
plot(y, type = "l", col = "black",
     main = "Pronóstico ARMA(1,2) - 6 trimestres",
     xlab = "Trimestres", ylab = "Consumo")
# Ajuste histórico
lines(ajuste12$y_hat, col = "blue", lty = 2)
# Pronóstico 
lines((length(y)+1):(length(y)+6), pronostico_6,
      col = "orangered", lty = 2, type = "o")

# Gráfico solo de los valores predichos
plot(pronostico_6, type = "o", col = "orangered",
     main = "Pronóstico ARMA(1,2) - 6 trimestres",
     xlab = "Trimestres futuros", ylab = "Consumo")
