install.packages("dplyr")  #Grammar of Data Manipulation
library("dplyr")
install.packages("DescTools")  #Tool for descriptive statistics
library(DescTools)

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.table("twins.txt", header = TRUE, sep = "\t")
View(twins)

# Selección de la columna "DMARRIED"
db9_twins <- select(twins, DMARRIED)

# Exclusión de datos que se encuentran incompletos
db9_twins <- db9_twins %>%
  filter(DMARRIED != ".")
View(db9_twins)

# Calcular la media para DMARRIED
db9_twins[] <- lapply(db9_twins, as.numeric)
mu_dmarried <- mean(db9_twins$DMARRIED)
round(mu_dmarried, digits = 2)

# Calcular la mediana para DMARRIED
db9_twins[] <- lapply(db9_twins, as.numeric)
mediana_dmarried <- median(db9_twins$DMARRIED)
round(mediana_dmarried, digits = 2)

# Calcular la moda para DMARRIED
db9_twins[] <- lapply(db9_twins, as.numeric)
moda_dmarried <- Mode(db9_twins$DMARRIED)
moda_dmarried

# Calcular el rango para DMARRIED
round(range(db9_twins$DMARRIED), digits = 2)

# Calcular la varianza para DMARRIED
round(var(db9_twins$DMARRIED), digits = 2)

# Calcular la desviación estándar para DMARRIED
round(sd(db9_twins$DMARRIED), digits = 2)

# Calcular el coeficiente de variación para DMARRIED
CV <- sd(db9_twins$DMARRIED)/mean(db9_twins$DMARRIED)
round(CV, digits = 2)

# Calcular Q1 para DMARRIED
Q1_1 <- quantile(db9_twins$DMARRIED, 0.25)
Q1_1

# Calcular Q2 para DMARRIED
Q2_1 <- quantile(db9_twins$DMARRIED, 0.50)
Q2_1

# Calcular Q3 para DMARRIED
Q3_1 <- quantile(db9_twins$DMARRIED, 0.75)
Q3_1

# Calcular el mínimo para DMARRIED
round(min(db9_twins$DMARRIED), digits = 2)

# Calcular el máximo para DMARRIED
round(max(db9_twins$DMARRIED), digits = 2)