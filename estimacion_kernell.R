# Cargar paquete necesario si no está instalado
if (!require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)

# Cargar paquete ggplot2
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
library(ggplot2)

# Obtención de la ruta del directorio actual
dir <- getwd()

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.csv(file.path(dir, "twins.txt"), header = TRUE)

# Selección de columnas "Twins"
db_twins <- dplyr::select(twins, HRWAGEH, HRWAGEL)

#Exclusión de datos que se encuentran incompletos
db_twins <- db_twins %>%
  filter(HRWAGEH != "." & HRWAGEL != ".")

# Crear nuevas variables numéricas a partir de las columnas existentes en db_twins
db_twins_numeric <- db_twins %>%
  mutate(HRWAGEH_numeric = as.numeric(HRWAGEH),
         HRWAGEL_numeric = as.numeric(HRWAGEL))

# Verificar el resultado
str(db_twins_numeric)
