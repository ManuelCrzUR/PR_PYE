install.packages("dplyr")
library("dplyr")
install.packages("DescTools")  #Tool for descriptive statistics
library(DescTools)

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.csv(file.path(dir, "twins.txt"), header = TRUE)
View(twins)

# Selección de columnas "Twins"
db4_twins <- select(twins, AGE, AGESQ)

# Exclusión de datos que se encuentran incompletos
db4_twins <- db4_twins %>%
  filter(AGE != "." & AGESQ != ".")
View(db4_twins)

# Selección de la columna "AGE"
age <- select(twins, AGE)

# Exclusión de los datos que son diferentes a "AGE"
age <- age %>%
  filter(AGE != ".")
View(age)

# Selección de la columna "AGESQ"
agesq <- select(twins, AGESQ)

# Exclusión de los datos que son diferentes a "AGESQ"
agesq <- agesq %>%
  filter(AGESQ != ".")
View(agesq)

# Calcular la media para AGE
db4_twins[] <- lapply(db4_twins, as.numeric)
mu_age <- mean(db4_twins$AGE)
round(mu_age, digits = 2)

# Calcular la media para AGESQ
db4_twins[] <- lapply(db4_twins, as.numeric)
mu_agesq <- mean(db4_twins$AGESQ)
round(mu_agesq, digits = 2)

# Calcular la mediana para AGE
db4_twins[] <- lapply(db4_twins, as.numeric)
mediana_age <- median(db4_twins$AGE)
round(mediana_age, digits = 2)

# Calcular la mediana para AGESQ
db4_twins[] <- lapply(db4_twins, as.numeric)
mediana_agesq <- median(db4_twins$AGESQ)
round(mediana_agesq, digits = 2)

# Calcular la moda para AGE
db4_twins[] <- lapply(db4_twins, as.numeric)
moda_age <- Mode(db4_twins$AGE)
moda_age

# Calcular la moda para AGESQ
db4_twins[] <- lapply(db4_twins, as.numeric)
moda_agesq <- Mode(db4_twins$AGESQ)
round(moda_agesq, digits = 2)

# Calcular el rango para AGE
round(range(db4_twins$AGE), digits = 2)

# Calcular el rango para AGESQ
round(range(db4_twins$AGESQ), digits = 2)

# Calcular la varianza para AGE
round(var(db4_twins$AGE), digits = 2)

# Calcular la varianza para AGESQ
round(var(db4_twins$AGESQ), digits = 2)

# Calcular la desviación estándar para AGE
round(sd(db4_twins$AGE), digits = 2)

# Calcular la desviación estándar para AGESQ
round(sd(db4_twins$AGESQ), digits = 2)

# Calcular el coeficiente de variación AGE
CV <- sd(db4_twins$AGE)/mean(db4_twins$AGE)
round(CV, digits = 2)

# Calcular el coeficiente de variación para AGESQ
CV <- sd(db4_twins$AGESQ)/mean(db4_twins$AGESQ)
round(CV, digits = 2)

# Calcular Q1 para AGE
Q1_1 <- quantile(db4_twins$AGE, 0.25)
round(Q1_1, digits = 2)

# Calcular Q1 para AGESQ
Q1_2 <- quantile(db4_twins$AGESQ, 0.25)
round(Q1_2, digits = 2)

# Calcular Q2 para AGE
Q2_1 <- quantile(db4_twins$AGE, 0.50)
round(Q2_1, digits = 2)

# Calcular Q2 para AGESQ
Q2_2 <- quantile(db4_twins$AGESQ, 0.50)
round(Q2_2, digits = 2)

# Calcular Q3 para AGE
Q3_1 <- quantile(db4_twins$AGE, 0.75)
round(Q3_1, digits = 2)

# Calcular Q3 para AGESQ
Q3_2 <- quantile(db4_twins$AGESQ, 0.75)
round(Q3_2, digits = 2)

# Calcular el mínimo para AGE
round(min(db4_twins$AGE), digits = 2)

# Calcular el mínimo para AGESQ
round(min(db4_twins$AGESQ), digits = 2)

# Calcular el máximo para AGE
round(max(db4_twins$AGE), digits = 2)

# Calcular el máximo para AGESQ
max(db4_twins$AGESQ)