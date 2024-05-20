install.packages("dplyr")
library("dplyr")
install.packages("DescTools")  #Tool for descriptive statistics
library(DescTools)

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.csv(file.path(dir, "twins.txt"), header = TRUE)
View(twins)

# Selección de columnas "Twins"
db5_twins <- select(twins, WHITEH, MALEH)

# Exclusión de datos que se encuentran incompletos
db5_twins <- db5_twins %>%
  filter(WHITEH != "." & MALEH != ".")
View(db5_twins)

# Selección de la columna "WHITEH"
whiteh <- select(twins, WHITEH)

# Exclusión de los datos que son diferentes a "WHITEH"
whiteh <- whiteh %>%
  filter(WHITEH != ".")
View(whiteh)

# Selección de la columna "MALEH"
maleh <- select(twins, MALEH)

# Exclusión de los datos que son diferentes a "MALEH"
maleh <- maleh %>%
  filter(MALEH != ".")
View(maleh)

# Calcular la media para WHITEH
db5_twins[] <- lapply(db5_twins, as.numeric)
mu_whiteh <- mean(db5_twins$WHITEH)
round(mu_whiteh, digits = 2)

# Calcular la media para MALEH
db5_twins[] <- lapply(db5_twins, as.numeric)
mu_maleh <- mean(db5_twins$MALEH)
round(mu_maleh, digits = 2)

# Calcular la mediana para WHITEH
db5_twins[] <- lapply(db5_twins, as.numeric)
mediana_white <- median(db5_twins$WHITEH)
round(mediana_white, digits = 2)

# Calcular la mediana para MALEH
db5_twins[] <- lapply(db5_twins, as.numeric)
mediana_maleh <- median(db5_twins$MALEH)
round(mediana_maleh, digits = 2)

# Calcular la moda para WHITEH
db5_twins[] <- lapply(db5_twins, as.numeric)
moda_white <- Mode(db5_twins$WHITEH)
moda_white

# Calcular la moda para MALEH
db5_twins[] <- lapply(db5_twins, as.numeric)
moda_maleh <- Mode(db5_twins$MALEH)
round(moda_maleh, digits = 2)

# Calcular el rango para WHITEH
round(range(db5_twins$WHITEH), digits = 2)

# Calcular el rango para MALEH
round(range(db5_twins$MALEH), digits = 2)

# Calcular la varianza para WHITEH
round(var(db5_twins$WHITEH), digits = 2)

# Calcular la varianza para MALEH
round(var(db5_twins$MALEH), digits = 2)

# Calcular la desviación estándar para WHITEH
round(sd(db5_twins$WHITEH), digits = 2)

# Calcular la desviación estándar para MALEH
round(sd(db5_twins$MALEH), digits = 2)

# Calcular el coeficiente de variación WHITEH
CV <- sd(db5_twins$WHITEH)/mean(db5_twins$WHITEH)
round(CV, digits = 2)

# Calcular el coeficiente de variación para MALEH
CV <- sd(db5_twins$MALEH)/mean(db5_twins$MALEH)
round(CV, digits = 2)

# Calcular Q1 para WHITEH
Q1_1 <- quantile(db5_twins$WHITEH, 0.25)
Q1_1

# Calcular Q1 para MALEH
Q1_2 <- quantile(db5_twins$MALEH, 0.25)
Q1_2

# Calcular Q2 para WHITEH
Q2_1 <- quantile(db5_twins$WHITEH, 0.50)
Q2_1

# Calcular Q2 para MALEH
Q2_2 <- quantile(db5_twins$MALEH, 0.50)
Q2_2

# Calcular Q3 para WHITEH
Q3_1 <- quantile(db5_twins$WHITEH, 0.75)
Q3_1

# Calcular Q3 para MALEH
Q3_2 <- quantile(db5_twins$MALEH, 0.75)
Q3_2

# Calcular el mínimo para WHITEH
round(min(db5_twins$WHITEH), digits = 2)

# Calcular el mínimo para MALEH
round(min(db5_twins$MALEH), digits = 2)

# Calcular el máximo para WHITEH
round(max(db5_twins$WHITEH), digits = 2)

# Calcular el máximo para MALEH
max(db5_twins$MALEH)