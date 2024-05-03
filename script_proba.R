
##### FILTRO PARA LA BASE DE DATOS

# Instalación y Descarga de libreria
if (!require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)

# Obtención de la ruta del directorio actual
dir <- getwd()

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.csv(file.path(dir, "twins.txt"), header = TRUE)
View(twins)

# Selección de columnas "Twins"
db_twins <- select(twins, HRWAGEH, HRWAGEL)

# Exclusión de datos que se encuentran incompletos
db_twins <- db_twins %>%
  filter(HRWAGEH != "." & HRWAGEL != ".")

View(db_twins)





library(shiny)

# Definir la interfaz de usuario
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      .titulo-centrado {
        text-align: center;
        font-family: 'Arial', sans-serif;
        font-weight: bold
      }
      .imagen {
        display: inline-block;
        width: 50px; /* Ajusta el tamaño de la imagen según tus necesidades */
        height: auto; /* Esto mantendrá la proporción de la imagen */
      }
      .integrantes {
        text-align: center;
        font-size: 16px;
      }
      ")
    )
  ),
  titlePanel(
    div(class = "titulo-centrado", 
        img(src = file.path(dir, "imagen_izquierda.png"), class = "imagen"),
        "Proyecto Final Probabilidad y Estadistica 1",
        img(src = file.path(dir, "imagen_derecha.png"), class = "imagen"),
        div(class = "integrantes", "Integrantes: Diego Buitrago, Manuel Cruz, Mariana Romero")
    )
  )
)


# Define server logic
server <- function(input, output) {
  output$mensaje <- renderText({
    "¡Hola desde mi primera aplicación Shiny!"
  })
}

# Run the application
shinyApp(ui = ui, server = server)