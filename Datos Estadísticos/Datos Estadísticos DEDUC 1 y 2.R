install.packages("dplyr")  #Grammar of Data Manipulation
library("dplyr")
install.packages("DescTools")  #Tool for descriptive statistics
library(DescTools)

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.table("twins.txt", header = TRUE, sep = "\t")
View(twins)

# Selección de la columna "DEDUC1"
db3_twins <- select(twins, DEDUC1, DEDUC2)

# Exclusión de datos que se encuentran incompletos
db3_twins <- db3_twins %>%
  filter(DEDUC1 != "." & DEDUC2 != ".")
View(db3_twins)

# Calcular la media para DEDUC1
db3_twins[] <- lapply(db3_twins, as.numeric)
mu <- mean(db3_twins$DEDUC1)
round(mu, digits = 2)

# Calcular la media para DEDUC2
db3_twins[] <- lapply(db3_twins, as.numeric)
mu2 <- mean(db3_twins$DEDUC2)
round(mu2, digits = 2)

# Calcular la mediana para DEDUC1
db3_twins[] <- lapply(db3_twins, as.numeric)
mediana <- median(db3_twins$DEDUC1)
round(mediana, digits = 2)

# Calcular la mediana para DEDUC2
db3_twins[] <- lapply(db3_twins, as.numeric)
mediana2 <- median(db3_twins$DEDUC2)
round(mediana2, digits = 2)

# Calcular la moda para DEDUC1
db3_twins[] <- lapply(db3_twins, as.numeric)
moda <- Mode(db3_twins$DEDUC1)
moda

# Calcular la moda para DEDUC2
db3_twins[] <- lapply(db3_twins, as.numeric)
moda2 <- Mode(db3_twins$DEDUC2)
moda2

# Calcular el rango para DEDUC1
round(range(db3_twins$DEDUC1), digits = 2)

# Calcular el rango para DEDUC2
round(range(db3_twins$DEDUC2), digits = 2)

# Calcular la varianza para DEDUC1
round(var(db3_twins$DEDUC1), digits = 2)

# Calcular la varianza para DEDUC2
round(var(db3_twins$DEDUC2), digits = 2)

# Calcular la desviación estándar para DEDUC1
round(sd(db3_twins$DEDUC1), digits = 2)

# Calcular la desviación estándar para DEDUC2
round(sd(db3_twins$DEDUC2), digits = 2)

# Calcular el coeficiente de variación para DEDUC1
CV <- sd(db3_twins$DEDUC1)/mean(db3_twins$DEDUC1)
round(CV, digits = 2)

# Calcular el coeficiente de variación para DEDUC2
CV2 <- sd(db3_twins$DEDUC2)/mean(db3_twins$DEDUC2)
round(CV2, digits = 2)

# Calcular Q1 para DEDUC1
Q1_1 <- quantile(db3_twins$DEDUC1, 0.25)
Q1_1

# Calcular Q1 para DEDUC2
Q1_2 <- quantile(db3_twins$DEDUC2, 0.25)
Q1_2

# Calcular Q2 para DEDUC1
Q2_1 <- quantile(db3_twins$DEDUC1, 0.50)
Q2_1

# Calcular Q2 para DEDUC2
Q2_2 <- quantile(db3_twins$DEDUC2, 0.50)
Q2_2

# Calcular Q3 para DEDUC1
Q3_1 <- quantile(db3_twins$DEDUC1, 0.75)
Q3_1

# Calcular Q3 para DEDUC2
Q3_2 <- quantile(db3_twins$DEDUC2, 0.75)
Q3_2

# Calcular el mínimo para DEDUC1
round(min(db3_twins$DEDUC1), digits = 2)

# Calcular el mínimo para DEDUC2
round(min(db3_twins$DEDUC2), digits = 2)

# Calcular el máximo para DEDUC1
round(max(db3_twins$DEDUC1), digits = 2)

# Calcular el máximo para DEDUC2
round(max(db3_twins$DEDUC2), digits = 2)