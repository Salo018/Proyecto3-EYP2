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
set.seed(2242055)  # para reproducibilidad
pos_extra <- sample(setdiff(1:n_total, pos_outliers), faltan)

# Unir todas las posiciones
pos_nulos <- c(pos_outliers, pos_extra)

# Crear nulos artificiales
df.nulos_artificiales <- df.sin_nulos
df.nulos_artificiales$Consumo[pos_nulos] <- NA

# IMPUTACIÓN #

# FUNCIONES DE EVALUACIÓN

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

# Imputación con distintos metodos

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
  # Definimos la ventana alrededor del NA
  idx <- max(1, i - k %/% 2) : min(nrow(df.mm), i + k %/% 2)
  idx <- idx[idx != i] # Excluimos el propio NA
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

# Comparar resultados en un data.frame
resultados <- data.frame(
  Metodo = c("Media", "Mediana", "Moda", "Interpolación", "Medias Móviles", "LOCF"),
  ECM = c(ecm_media, ecm_mediana, ecm_moda, ecm_interp, ecm_mm, ecm_locf),
  MAE = c(mae_media, mae_mediana, mae_moda, mae_interp, mae_mm, mae_locf),
  MAPE = c(mape_media, mape_mediana, mape_moda, mape_interp, mape_mm, mape_locf),
  RMSE = c(rmse_media, rmse_mediana, rmse_moda, rmse_interp, rmse_mm, rmse_locf)
)

print(resultados)

# Mostrar resultados ordenados de menor a mayor ECM
resultados <- resultados[order(resultados$ECM), ]
print(resultados)

# Método ganador
mejor_metodo <- resultados$Metodo[1]
mejor_ecm <- resultados$ECM[1]

cat("El mejor método es:", mejor_metodo, "con ECM =", mejor_ecm, "\n")

# Mejor por MAE
mejor_metodo_mae <- resultados$Metodo[which.min(resultados$MAE)]
mejor_mae <- min(resultados$MAE)

cat("El mejor método según MAE es:", mejor_metodo_mae, "con MAE =", mejor_mae, "\n")

# Mejor por MAPE
mejor_metodo_mape <- resultados$Metodo[which.min(resultados$MAPE)]
mejor_mape <- min(resultados$MAPE)

cat("El mejor método según MAPE es:", mejor_metodo_mape, "con MAPE =", mejor_mape, "\n")

# Mejor por RMSE
mejor_metodo_rmse <- resultados$Metodo[which.min(resultados$RMSE)]
mejor_rmse <- min(resultados$RMSE)

cat("El mejor método según RMSE es:", mejor_metodo_rmse, "con RMSE =", mejor_rmse, "\n")

# Valores originales en las posiciones de nulos
valores_reales <- df.sin_nulos$Consumo[pos_nulos]

# Valores imputados por cada método
valores_moda <- df.moda$Consumo[pos_nulos]
valores_interp <- df.interp$Consumo[pos_nulos]
valores_mm <- df.mm$Consumo[pos_nulos]

# DataFrame comparativo
comparacion <- data.frame(
  Posicion = pos_nulos,
  Real = valores_reales,
  Moda = valores_moda,
  Interpolacion = valores_interp,
  MM = valores_mm
)

print(comparacion)

# Gráfico comparativo Moda vs Interpolación vs LOCF
ggplot(comparacion, aes(x = Posicion)) +
  geom_point(aes(y = Real), color = "black", size = 3) +
  geom_point(aes(y = Moda), color = "red", size = 2) +
  geom_point(aes(y = Interpolacion), color = "blue", size = 2) +
  geom_point(aes(y = MM), color = "green", size = 2) +
  labs(title = "Comparación imputación: Moda vs Interpolación vs MM",
       y = "Consumo", x = "Posición (índice)") +
  theme_minimal()

# Mostrar las varianzas de cada método y del original
data.frame(
  Metodo = c("Original", "Media", "Mediana", "Moda", "Interpolación", "Medias Móviles", "LOCF"),
  Varianza = c(
    var(df.sin_nulos$Consumo, na.rm = TRUE),
    var(df.media$Consumo, na.rm = TRUE),
    var(df.mediana$Consumo, na.rm = TRUE),
    var(df.moda$Consumo, na.rm = TRUE),
    var(df.interp$Consumo, na.rm = TRUE),
    var(df.mm$Consumo, na.rm = TRUE),
    var(df.locf$Consumo, na.rm = TRUE)
  )
)


# Crear la variable "Periodo" para unir el año con los trimestres
df.locf_copy <- df.locf %>%
  mutate(Trimestre_num = gsub("Trimestre_", "", Trimestre),
         Periodo = paste(Año, Trimestre_num, sep = "-"))

# Grafica con todos los años 
ggplot(data = df.locf_copy, aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo del consumo personal (1960-2016)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##compa

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


### medias moviles
# Crear la variable "Periodo" para unir el año con los trimestres
df.mm_copy <- df.mm %>%
  mutate(Trimestre_num = gsub("Trimestre_", "", Trimestre),
         Periodo = paste(Año, Trimestre_num, sep = "-"))

# Grafica con todos los años 
ggplot(data = df.mm_copy, aes(x = Periodo, y = Consumo, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    x = "Periodo (Año-Trimestre)",
    y = "Consumo personal (%)",
    title = "Serie de tiempo del consumo personal (1960-2016)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# IMPUTACIÓN AL DF ORIGINAL

# Imputación con MEDIAS MÓVILES sobre df.consumo_copy
k <- 3
df.consumo_imp <- df.consumo_copy   # Usamos tu df original con nulos

for (i in which(is.na(df.consumo_imp$Consumo))) {
  # Definimos la ventana alrededor del NA
  idx <- max(1, i - k %/% 2) : min(nrow(df.consumo_imp), i + k %/% 2)
  idx <- idx[idx != i] # Excluimos el propio NA
  df.consumo_imp$Consumo[i] <- mean(df.consumo_imp$Consumo[idx], na.rm = TRUE)
}

data.frame(
  Metodo = c("Original", "Medias Móviles"),
  Varianza = c(
    var(df.consumo_copy$Consumo, na.rm = TRUE),
    var(df.consumo_imp$Consumo, na.rm = TRUE)
  )
)



