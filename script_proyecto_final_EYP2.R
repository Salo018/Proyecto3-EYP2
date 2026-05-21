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











