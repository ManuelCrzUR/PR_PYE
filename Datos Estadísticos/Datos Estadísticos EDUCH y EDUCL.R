install.packages("dplyr")  #Grammar of Data Manipulation
library("dplyr")
install.packages("DescTools")  #Tool for descriptive statistics
library(DescTools)

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.csv(file.path(dir, "twins.txt"), header = TRUE)
View(twins)

# Selección de columnas "Twins"
db7_twins <- select(twins, EDUCH, EDUCL)

# Exclusión de datos que se encuentran incompletos
db7_twins <- db7_twins %>%
  filter(EDUCH != "." & EDUCL != ".")
View(db7_twins)

# Selección de la columna "EDUCH"
educh <- select(twins, EDUCH)

# Exclusión de los datos que son diferentes a "EDUCH"
educh <- educh %>%
  filter(EDUCH != ".")
View(educh)

# Selección de la columna "EDUCL"
educl <- select(twins, EDUCL)

# Exclusión de los datos que son diferentes a "EDUCL"
educl <- educl %>%
  filter(EDUCL != ".")
View(educl)

# Calcular la media para EDUCH
db7_twins[] <- lapply(db7_twins, as.numeric)
mu_educh <- mean(db7_twins$EDUCH)
round(mu_educh, digits = 2)

# Calcular la media para EDUCL
db7_twins[] <- lapply(db7_twins, as.numeric)
mu_educl <- mean(db7_twins$EDUCL)
round(mu_educl, digits = 2)

# Calcular la mediana para EDUCH
db7_twins[] <- lapply(db7_twins, as.numeric)
mediana_educh <- median(db7_twins$EDUCH)
round(mediana_educh, digits = 2)

# Calcular la mediana para EDUCL
db7_twins[] <- lapply(db7_twins, as.numeric)
mediana_edcul <- median(db7_twins$EDUCL)
round(mediana_edcul, digits = 2)

# Calcular la moda para EDUCH
db7_twins[] <- lapply(db7_twins, as.numeric)
moda_educh <- Mode(db7_twins$EDUCH)
moda_educh

# Calcular la moda para EDUCL
db7_twins[] <- lapply(db7_twins, as.numeric)
moda_educl <- Mode(db7_twins$EDUCL)
round(moda_educl, digits = 2)

# Calcular el rango para EDUCH
round(range(db7_twins$EDUCH), digits = 2)

# Calcular el rango para EDUCL
round(range(db7_twins$EDUCL), digits = 2)

# Calcular la varianza para EDUCH
round(var(db7_twins$EDUCH), digits = 2)

# Calcular la varianza para EDUCL
round(var(db7_twins$EDUCL), digits = 2)

# Calcular la desviación estándar para EDUCH
round(sd(db7_twins$EDUCH), digits = 2)

# Calcular la desviación estándar para EDUCL
round(sd(db7_twins$EDUCL), digits = 2)

# Calcular el coeficiente de variación EDUCH
CV <- sd(db7_twins$EDUCH)/mean(db7_twins$EDUCH)
round(CV, digits = 2)

# Calcular el coeficiente de variación para EDUCL
CV <- sd(db7_twins$EDUCL)/mean(db7_twins$EDUCL)
round(CV, digits = 2)

# Calcular Q1 para EDUCH
Q1_1 <- quantile(db7_twins$EDUCH, 0.25)
Q1_1

# Calcular Q1 para EDUCL
Q1_2 <- quantile(db7_twins$EDUCL, 0.25)
Q1_2

# Calcular Q2 para EDUCH
Q2_1 <- quantile(db7_twins$EDUCH, 0.50)
Q2_1

# Calcular Q2 para EDUCL
Q2_2 <- quantile(db7_twins$EDUCL, 0.50)
Q2_2

# Calcular Q3 para EDUCH
Q3_1 <- quantile(db7_twins$EDUCH, 0.75)
Q3_1

# Calcular Q3 para EDUCL
Q3_2 <- quantile(db7_twins$EDUCL, 0.75)
Q3_2

# Calcular el mínimo para EDUCH
round(min(db7_twins$EDUCH), digits = 2)

# Calcular el mínimo para EDUCL
round(min(db7_twins$EDUCL), digits = 2)

# Calcular el máximo para EDUCH
round(max(db7_twins$EDUCH), digits = 2)

# Calcular el máximo para EDUCL
max(db7_twins$EDUCH)