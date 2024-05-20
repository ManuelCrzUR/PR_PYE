install.packages("dplyr")
library("dplyr")

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.csv(file.path(dir, "twins.txt"), header = TRUE)
View(twins)

# Selección de columnas "Twins"
db_twins <- select(twins, HRWAGEH, HRWAGEL)

# Exclusión de datos que se encuentran incompletos
db_twins <- db_twins %>%
  filter(HRWAGEH != "." & HRWAGEL != ".")
View(db_twins)

# Selección de la columna "HRWAGEH"
twins1 <- select(twins, HRWAGEH)

# Exclusión de los datos que son diferentes a "HRWAGEH"
twins1 <- twins1 %>%
  filter(HRWAGEH != ".")
View(twins1)

# Selección de la columna "HRWAGEL"
twins2 <- select(twins, HRWAGEL)

# Exclusión de los datos que son diferentes a "HRWAGEL"
twins2 <- twins2 %>%
  filter(HRWAGEL != ".")
View(twins2)

# Calcular la media para el gemelo 1 ("HRWAGEH")
db_twins[] <- lapply(db_twins, as.numeric)
mu_twins1 <- mean(db_twins$HRWAGEH)
round(mu_twins1, digits = 2)

# Calcular la media para el gemelo 2 ("HRWGEL")
db_twins[] <- lapply(db_twins, as.numeric)
mu_twins2 <- mean(db_twins$HRWAGEL)
round(mu_twins2, digits = 2)

# Calcular la mediana para el gemelo 1 ("HRWAGEH")
db_twins[] <- lapply(db_twins, as.numeric)
mediana_twins1 <- median(db_twins$HRWAGEH)
round(mediana_twins1, digits = 2)

# Calcular la mediana para el gemelo 2 ("HRWAGEL")
db_twins[] <- lapply(db_twins, as.numeric)
mediana_twins2 <- median(db_twins$HRWAGEL)
mediana_twins2

# Calcular la moda para el gemelo 1 ("HRWAGEH")
db_twins[] <- lapply(db_twins, as.numeric)
moda_twins1 <- Mode(db_twins$HRWAGEH)
moda_twins1

# Calcular la moda para el gemelo 2 ("HRWAGEL")
db_twins[] <- lapply(db_twins, as.numeric)
moda_twins2 <- Mode(db_twins$HRWAGEL)
moda_twins2

# Calcular el rango para el gemelo 1 ("HRWAGEH")
round(range(db_twins$HRWAGEH), digits = 2)

# Calcular el rango para el gemelo 2 ("HRWGEL")
round(range(db_twins$HRWAGEL), digits = 2)

# Calcular la varianza para el gemelo 1 ("HRWAGEH")
round(var(db_twins$HRWAGEH), digits = 2)

# Calcular la varianza para el gemelo 2 ("HRWAGEL")
round(var(db_twins$HRWAGEL), digits = 2)

# Calcular la desviación estándar para el gemelo 1 ("HRWAGEH")
round(sd(db_twins$HRWAGEH), digits = 2)

# Calcular la desviación estándar para el gemelo 2 ("HRWAGEL")
round(sd(db_twins$HRWAGEL), digits = 2)

# Calcular el coeficiente de variación para el gemelo 1 ("HRWAGEH")
CV <- sd(db_twins$HRWAGEH)/mean(db_twins$HRWAGEH)
round(CV, digits = 2)

# Calcular el coeficiente de variación para el gemelo 2 ("HRWAGEL")
CV <- sd(db_twins$HRWAGEL)/mean(db_twins$HRWAGEL)
round(CV, digits = 2)

# Calcular Q1 para el gemelo 1 ("HRWAGEH")
Q1_1 <- quantile(db_twins$HRWAGEH, 0.25)
Q1_1

# Calcular Q1 para el gemelo 2 ("HRWAGEL")
Q1_2 <- quantile(db_twins$HRWAGEL, 0.25)
Q1_2

# Calcular Q2 para el gemelo 1 ("HRWAGEH")
Q2_1 <- quantile(db_twins$HRWAGEH, 0.50)
Q2_1

# Calcular Q2 para el gemelo 2 ("HRWAGEL")
Q2_2 <- quantile(db_twins$HRWAGEL, 0.50)
Q2_2

# Calcular Q3 para el gemelo 1 ("HRWAGEH")
Q3_1 <- quantile(db_twins$HRWAGEH, 0.75)
Q3_1

# Calcular Q3 para el gemelo 2 ("HRWAGEL")
Q3_2 <- quantile(db_twins$HRWAGEL, 0.75)
round(Q3_2, digits = 2)

# Calcular el mínimo para el gemelo 1 ("HRWAGEH")
round(min(db_twins$HRWAGEH), digits = 2)

# Calcular el mínimo para el gemelo 2 ("HRWAGEL")
round(min(db_twins$HRWAGEL), digits = 2)

# Calcular el máximo para el gemelo 1 ("HRWAGEH")
round(max(db_twins$HRWAGEH), digits = 2)

# Calcular el máximo para el gemelo 2 ("HRWAGEL")
max(db_twins$HRWAGEL)