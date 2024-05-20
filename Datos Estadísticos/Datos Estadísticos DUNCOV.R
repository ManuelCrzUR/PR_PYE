install.packages("dplyr")  #Grammar of Data Manipulation
library("dplyr")
install.packages("DescTools")  #Tool for descriptive statistics
library(DescTools)

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.table("twins.txt", header = TRUE, sep = "\t")
View(twins)

# Selección de la columna "DUNCOV"
db10_twins <- select(twins, DUNCOV)

# Exclusión de datos que se encuentran incompletos
db10_twins <- db10_twins %>%
  filter(DUNCOV != ".")
View(db10_twins)

# Calcular la media para DUNCOV
db10_twins[] <- lapply(db10_twins, as.numeric)
mu_duncov <- mean(db10_twins$DUNCOV)
round(mu_duncov, digits = 2)

# Calcular la mediana para DUNCOV
db10_twins[] <- lapply(db10_twins, as.numeric)
mediana_duncov <- median(db10_twins$DUNCOV)
round(mediana_duncov, digits = 2)

# Calcular la moda para DUNCOV
db10_twins[] <- lapply(db10_twins, as.numeric)
moda_duncov <- Mode(db10_twins$DUNCOV)
moda_duncov

# Calcular el rango para DUNCOV
round(range(db10_twins$DUNCOV), digits = 2)

# Calcular la varianza para DUNCOV
round(var(db10_twins$DUNCOV), digits = 2)

# Calcular la desviación estándar para DUNCOV
round(sd(db10_twins$DUNCOV), digits = 2)

# Calcular el coeficiente de variación para DUNCOV
CV <- sd(db10_twins$DUNCOV)/mean(db10_twins$DUNCOV)
round(CV, digits = 2)

# Calcular Q1 para DUNCOV
Q1_1 <- quantile(db10_twins$DUNCOV, 0.25)
Q1_1

# Calcular Q2 para DUNCOV
Q2_1 <- quantile(db10_twins$DUNCOV, 0.50)
Q2_1

# Calcular Q3 para DUNCOV
Q3_1 <- quantile(db10_twins$DUNCOV, 0.75)
Q3_1

# Calcular el mínimo para DUNCOV
round(min(db10_twins$DUNCOV), digits = 2)

# Calcular el máximo para DUNCOV
round(max(db10_twins$DUNCOV), digits = 2)