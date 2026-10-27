
# Instalar el paquete una sola vez
install.packages("AgroR")

# Cargar la librería
library(AgroR)

# Cargar los datos
data("bean")

# Examinar la estructura
str(bean)

# Mostrar todas las observaciones
print(bean)

# Verificar número de observaciones
nrow(bean)

# Verificar repeticiones por tratamiento
table(bean$trat)

View(bean)

#analisis descriptivo

# Instalar dplyr si es necesario
install.packages("dplyr")

# Cargar librería
library(dplyr)

# Resumen descriptivo
descriptivos <- bean %>%
  group_by(trat) %>%
  summarise(
    n = sum(!is.na(prod)),
    Media = mean(prod, na.rm = TRUE),
    Mediana = median(prod, na.rm = TRUE),
    Desviacion_estandar = sd(prod, na.rm = TRUE),
    Varianza = var(prod, na.rm = TRUE),
    Minimo = min(prod, na.rm = TRUE),
    Maximo = max(prod, na.rm = TRUE),
    CV_porcentaje = 100 * sd(prod, na.rm = TRUE) /
      mean(prod, na.rm = TRUE),
    .groups = "drop"
  )

# Mostrar resultados
print(descriptivos)

#diagrama de cajas
boxplot(
  prod ~ trat,
  data = bean,
  col = "lightgreen",
  border = "darkgreen",
  main = "Producción de fríjol por tratamiento",
  xlab = "Tratamiento (cepa de Azospirillum)",
  ylab = "Producción de grano (g/planta)",
  las = 2
)

# Calcular medias y desviaciones estándar
medias <- bean %>%
  group_by(trat) %>%
  summarise(
    media = mean(prod, na.rm = TRUE),
    sd = sd(prod, na.rm = TRUE),
    .groups = "drop"
  )

# Gráfico
pos <- barplot(
  medias$media,
  names.arg = medias$trat,
  col = "lightgreen",
  border = "darkgreen",
  main = "Producción media de fríjol por cepa",
  xlab = "Tratamiento",
  ylab = "Producción de grano (g/planta)",
  las = 2
)

# Añadir barras de ±1 desviación estándar
arrows(
  x0 = pos,
  y0 = pmax(0, medias$media - medias$sd),
  x1 = pos,
  y1 = medias$media + medias$sd,
  angle = 90,
  code = 3,
  length = 0.05
)
# GUARDAR TABLAS DESCRIPTIVA
write.csv(
  descriptivos,
  "analisis_descriptivo_bean.csv",
  row.names = FALSE
)