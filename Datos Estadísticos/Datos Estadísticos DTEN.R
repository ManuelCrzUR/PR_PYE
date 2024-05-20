install.packages("dplyr")  #Grammar of Data Manipulation
library("dplyr")
install.packages("DescTools")  #Tool for descriptive statistics
library(DescTools)

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.table("twins.txt", header = TRUE, sep = "\t")
View(twins)

# Selección de la columna "DTEN"
db8_twins <- select(twins, DTEN)

# Exclusión de datos que se encuentran incompletos
db8_twins <- db8_twins %>%
  filter(DTEN != ".")
View(db8_twins)

# Calcular la media para DTEN
db8_twins[] <- lapply(db8_twins, as.numeric)
mu_dten <- mean(db8_twins$DTEN)
round(mu_dten, digits = 2)

# Calcular la mediana para DTEN
db8_twins[] <- lapply(db8_twins, as.numeric)
mediana_dten <- median(db8_twins$DTEN)
round(mediana_dten, digits = 2)

# Calcular la moda para DTEN
db8_twins[] <- lapply(db8_twins, as.numeric)
moda_dten <- Mode(db8_twins$DTEN)
moda_dten

# Calcular el rango para DTEN
round(range(db8_twins$DTEN), digits = 2)

# Calcular la varianza para DTEN
round(var(db8_twins$DTEN), digits = 2)

# Calcular la desviación estándar para DTEN
round(sd(db8_twins$DTEN), digits = 2)

# Calcular el coeficiente de variación para DTEN
CV <- sd(db8_twins$DTEN)/mean(db8_twins$DTEN)
round(CV, digits = 2)

# Calcular Q1 para DTEN
Q1_1 <- quantile(db8_twins$DTEN, 0.25)
Q1_1

# Calcular Q2 para DTEN
Q2_1 <- quantile(db8_twins$DTEN, 0.50)
Q2_1

# Calcular Q3 para DTEN
Q3_1 <- quantile(db8_twins$DTEN, 0.75)
Q3_1

# Calcular el mínimo para DTEN
round(min(db8_twins$DTEN), digits = 2)

# Calcular el máximo para DTEN
round(max(db8_twins$DTEN), digits = 2)