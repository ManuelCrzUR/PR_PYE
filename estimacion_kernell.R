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

# Exclusión de datos que se encuentran incompletos
db_twins <- db_twins %>%
  filter(HRWAGEH != "." & HRWAGEL != ".")

# Calcular la densidad de kernel para HRWAGEH
densidad_hrwageh <- density(db_twins$HRWAGEH)

# Calcular la densidad de kernel para HRWAGEL
densidad_hrwagel <- density(db_twins$HRWAGEL)

# Graficar ambas densidades en el mismo gráfico
plot(densidad_hrwageh, main = "Estimación de Kernel para HRWAGEH", xlab = "HRWAGEH", col = "blue")
lines(densidad_hrwagel, col = "red")
