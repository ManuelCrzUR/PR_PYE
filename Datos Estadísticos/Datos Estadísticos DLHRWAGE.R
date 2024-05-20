install.packages("dplyr")  #Grammar of Data Manipulation
library("dplyr")
install.packages("DescTools")  #Tool for descriptive statistics
library(DescTools)

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.table("twins.txt", header = TRUE, sep = "\t")
View(twins)

# Selección de la columna "DLHRWAGE"
db2_twins <- select(twins, DLHRWAGE)

# Exclusión de datos que se encuentran incompletos
db2_twins <- db2_twins %>%
  filter(DLHRWAGE != ".")
View(db2_twins)

# Calcular la media para DLHRWAGE
db2_twins[] <- lapply(db2_twins, as.numeric)
mu_twin1 <- mean(db2_twins$DLHRWAGE)
round(mu_twins1, digits = 2)

# Calcular la mediana para DLHRWAGE
db2_twins[] <- lapply(db2_twins, as.numeric)
mediana_twin1 <- median(db2_twins$DLHRWAGE)
round(mediana_twins1, digits = 2)

# Calcular la moda para DLHRWAGE
db2_twins[] <- lapply(db2_twins, as.numeric)
moda_twin1 <- Mode(db2_twins$DLHRWAGE)
moda_twin1

# Calcular el rango para DLHRWAGE
round(range(db2_twins$DLHRWAGE), digits = 2)

# Calcular la varianza para DLHRWAGE
round(var(db2_twins$DLHRWAGE), digits = 2)

# Calcular la desviación estándar para DLHRWAGE
round(sd(db2_twins$DLHRWAGE), digits = 2)

# Calcular el coeficiente de variación para DLHRWAGE
CV <- sd(db2_twins$DLHRWAGE)/mean(db2_twins$DLHRWAGE)
round(CV, digits = 2)

# Calcular Q1 para DLHRWAGE
Q1_1 <- quantile(db2_twins$DLHRWAGE, 0.25)
Q1_1

# Calcular Q2 para DLHRWAGE
Q2_1 <- quantile(db2_twins$DLHRWAGE, 0.50)
Q2_1

# Calcular Q3 para DLHRWAGE
Q3_1 <- quantile(db2_twins$DLHRWAGE, 0.75)
Q3_1

# Calcular el mínimo para DLHRWAGE
round(min(db2_twins$DLHRWAGE), digits = 2)

# Calcular el máximo para DLHRWAGE
round(max(db2_twins$DLHRWAGE), digits = 2)