
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

####### ESTIMACIÓN KERNELL

# Cargar paquete necesario si no está instalado
if (!require("ggplot2")) {
  install.packages("ggplot2")
}

# Cargar los datos
db_twins <- select(twins, HRWAGEH, HRWAGEL)

# Crear gráfico de densidad para el gemelo 1
densidad_gemelo1 <- density(db_twins$HRWAGEH)
plot(densidad_gemelo1, main = "Distribución del salario por hora - Gemelo 1",
     xlab = "Salario por hora", ylab = "Densidad", col = "blue")

# Añadir línea de densidad para el gemelo 2
lines(density(db_twins$HRWAGEL), col = "red")

# Agregar leyenda
legend("topright", legend = c("Gemelo 1", "Gemelo 2"), col = c("blue", "red"), lty = 1)


####### UI PAGINA WEB

library(shiny)

# Definir la interfaz de usuario
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      body {
        background-color: #f0f0f0; /* Cambia el color de fondo aquí */
      }
      .titulo-centrado {
        text-align: center;
        font-family: 'Arial', sans-serif;
        font-weight: bold;
      }
      .imagen {
        display: inline-block;
        width: 50px; /* Ajusta el tamaño de la imagen según tus necesidades */
        height: auto; /* Esto mantendrá la proporción de la imagen */
      }
      .integrantes {
        text-align: center;
        font-size: 16px;
        margin-top: 30px;
      }
      .docente {
        text-alig: center;
        font-size: 16px;
        margin-bottom: 20px;
      }
      .introduccion {
        font-size: 22px;
        font-weight: bold;
      }
      .explicacion {
        text-align: justify;
        font-size: 14px;
        margin: 20px; /* Agrega margen para separar del contenido anterior */
      }
      .problema {
        font-size: 22px;
        font-weight: bold;
      }
      .sust_problema {
        texst-align: justify;
        font-size: 14px;
        margin: 20px;
      }
      .metodologia {
        font-size: 22px;
        font-weight: bold;
      }
      .desc_grafica {
        font-size: 16px;
        margin: 20px;
      }
      .desc_numerica {
        font-size: 16px;
        margin: 20px;
      }
      ")
    )
  ),
  titlePanel(
    div(class = "titulo-centrado", 
        img(src = file.path(dir, "imagen_izquierda.png"), class = "imagen"),
        "Proyecto Final Probabilidad y Estadistica 1",
        img(src = file.path(dir, "imagen_derecha.png"), class = "imagen"),
        div(class = "integrantes", "Integrantes: Diego Buitrago, Manuel Cruz, Mariana Romero"),
        div(class = 'docente', 'Docente: Nicolas López López')
    )
  ),
  # Itroducción 
  div(class = 'introduccion',
      'Introducción:'
  ),
  # Nuevo apartado para añadir texto explicativo
  div(class = "explicacion",
      "Este es el proyecto final para la asignatura de Probabilidad y Estadística 1, para el semestre 2024 - 1. En esta aplicación, presentamos los resultados de nuestro análisis estadístico sobre un conjunto de datos específico. Utilizamos la plataforma Shiny para crear una interfaz interactiva que permite a los usuarios explorar los datos y entender mejor nuestras conclusiones."
  ), 
  # Descripción del problema
  div(class = 'problema',
      'Descripción del problema: Estudio Ingreso de Gemelos'
  ), 
  div(class = 'sust_problema', 
      'Se cuenta con información de pares de gemelos monocigóticos mayores de 18 años, y se desea contrastar el fenomeno de años de educación, asociados con el ingreso por hora.'
  ), 

  div(class = 'metodologia', 
      'Metodologia:',
  ),
  div(class = 'desc_grafica', 
      'Para este caso de estudio, se buscara un forma gráfica de representar los datos que fueron recolectados y expuestos, mediante el grafico asociado con la "Estimación Kernel".'
  ),
  div(class = 'desc_numerica',
      'Para el caso númerico, se busco realizar una comparación entre cada gurpo de gemelos (gemelo 1 vs gemelo 2), en el cual se busca contrastar las siguientes medidas: (CENTRO: Media, Mediana, Moda), (DISPERSIÓN: Rango, Varianza, Desviación estandar, CV), (LOCALIZACIÓN: Q1, Q2, Q3, Minimo, Maximo)' 
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