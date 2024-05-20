install.packages("dplyr")  #Grammar of Data Manipulation
library("dplyr")
install.packages("DescTools")  #Tool for descriptive statistics
library(DescTools)

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.csv(file.path(dir, "twins.txt"), header = TRUE)
View(twins)

# Selección de columnas "Twins"
db6_twins <- select(twins, WHITEL, MALEL)

# Exclusión de datos que se encuentran incompletos
db6_twins <- db6_twins %>%
  filter(WHITEL != "." & MALEL != ".")
View(db6_twins)

# Selección de la columna "WHITEL"
whitel <- select(twins, WHITEL)

# Exclusión de los datos que son diferentes a "WHITEL"
whitel <- whitel %>%
  filter(WHITEL != ".")
View(whitel)

# Selección de la columna "MALEL"
malel <- select(twins, MALEL)

# Exclusión de los datos que son diferentes a "MALEHL
malel <- malel %>%
  filter(MALEL != ".")
View(malel)

# Calcular la media para WHITEL
db6_twins[] <- lapply(db6_twins, as.numeric)
mu_whitel <- mean(db6_twins$WHITEL)
round(mu_whitel, digits = 2)

# Calcular la media para MALEL
db6_twins[] <- lapply(db6_twins, as.numeric)
mu_malel <- mean(db6_twins$MALEL)
round(mu_malel, digits = 2)

# Calcular la mediana para WHITEL
db6_twins[] <- lapply(db6_twins, as.numeric)
mediana_whitel <- median(db6_twins$WHITEL)
round(mediana_whitel, digits = 2)

# Calcular la mediana para MALEL
db6_twins[] <- lapply(db6_twins, as.numeric)
mediana_malel <- median(db6_twins$MALEL)
round(mediana_malel, digits = 2)

# Calcular la moda para WHITEL
db6_twins[] <- lapply(db6_twins, as.numeric)
moda_whitel <- Mode(db6_twins$WHITEL)
moda_whitel

# Calcular la moda para MALEL
db6_twins[] <- lapply(db6_twins, as.numeric)
moda_malel <- Mode(db6_twins$MALEL)
round(moda_malel, digits = 2)

# Calcular el rango para WHITEL
round(range(db6_twins$WHITEL), digits = 2)

# Calcular el rango para MALEL
round(range(db6_twins$MALEL), digits = 2)

# Calcular la varianza para WHITEL
round(var(db6_twins$WHITEL), digits = 2)

# Calcular la varianza para MALEL
round(var(db6_twins$MALEL), digits = 2)

# Calcular la desviación estándar para WHITEL
round(sd(db6_twins$WHITEL), digits = 2)

# Calcular la desviación estándar para MALEL
round(sd(db6_twins$MALEL), digits = 2)

# Calcular el coeficiente de variación WHITEL
CV <- sd(db6_twins$WHITEL)/mean(db6_twins$WHITEL)
round(CV, digits = 2)

# Calcular el coeficiente de variación para MALEL
CV <- sd(db6_twins$MALEL)/mean(db5_twins$MALEL)
round(CV, digits = 2)

# Calcular Q1 para WHITEL
Q1_1 <- quantile(db6_twins$WHITEL, 0.25)
Q1_1

# Calcular Q1 para MALEL
Q1_2 <- quantile(db6_twins$MALEL, 0.25)
Q1_2

# Calcular Q2 para WHITEL
Q2_1 <- quantile(db6_twins$WHITEL, 0.50)
Q2_1

# Calcular Q2 para MALEL
Q2_2 <- quantile(db6_twins$MALEL, 0.50)
Q2_2

# Calcular Q3 para WHITEL
Q3_1 <- quantile(db6_twins$WHITEL, 0.75)
Q3_1

# Calcular Q3 para MALEL
Q3_2 <- quantile(db6_twins$MALEL, 0.75)
Q3_2

# Calcular el mínimo para WHITEL
round(min(db6_twins$WHITEL), digits = 2)

# Calcular el mínimo para MALEL
round(min(db6_twins$MALEL), digits = 2)

# Calcular el máximo para WHITEL
round(max(db6_twins$WHITEL), digits = 2)

# Calcular el máximo para MALEL
max(db6_twins$MALEL)