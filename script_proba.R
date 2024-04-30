# Instalación y Descarga de libreria

install.packages("dplyr")
library(dplyr)

# Importación de base de datos "Twins"
twins <- read.csv(file.choose(twins.txt), header = TRUE)
View(twins)

# Selección de columnas "Twins"
db_twins <- select(twins, HRWAGEH, HRWAGEL)

# Exclusión de datos que se encuentran incompletosS
db_twins <- db_twins %>%
  filter(HRWAGEH != "." & HRWAGEL != ".")

View(db_twins)
