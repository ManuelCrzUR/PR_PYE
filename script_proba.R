
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

####### ANALISIS NUMERICO 


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
mu_twins1

# Calcular la media para el gemelo 2 ("HRWGEL")
db_twins[] <- lapply(db_twins, as.numeric)
mu_twins2 <- mean(db_twins$HRWAGEL)
mu_twins2

# Calcular la mediana para el gemelo 1 ("HRWAGEH")
db_twins[] <- lapply(db_twins, as.numeric)
mediana_twins1 <- median(db_twins$HRWAGEH)
mediana_twins1

# Calcular la mediana para el gemelo 2 ("HRWAGEL")
db_twins[] <- lapply(db_twins, as.numeric)
mediana_twins2 <- median(db_twins$HRWAGEL)
mediana_twins2

# Calcular la moda para el gemelo 1 ("HRWAGEH")
install.packages("DescTools")
library("DescTools")
db_twins[] <- lapply(db_twins, as.numeric)
moda_twins1 <- Mode(db_twins$HRWAGEH)
moda_twins1

#Moda <- function(x) {
#moda <- Mode(x)
#return(moda[1])
#}

# Calcular la moda para el gemelo 2 ("HRWAGEL")
db_twins[] <- lapply(db_twins, as.numeric)
moda_twins2 <- Mode(db_twins$HRWAGEL)
moda_twins2

# Calcular el rango para el gemelo 1 ("HRWAGEH")
range(db_twins$HRWAGEH)

# Calcular el rango para el gemelo 2 ("HRWGEL")
range(db_twins$HRWAGEL)

# Calcular la varianza para el gemelo 1 ("HRWAGEH")
var(db_twins$HRWAGEH)

# Calcular la varianza para el gemelo 2 ("HRWAGEL")
var(db_twins$HRWAGEL)

# Calcular la desviación estándar para el gemelo 1 ("HRWAGEH")
sd(db_twins$HRWAGEH)

# Calcular la desviación estándar para el gemelo 2 ("HRWAGEL")
sd(db_twins$HRWAGEL)

# Calcular el coeficiente de variación para el gemelo 1 ("HRWAGEH")
CV <- sd(db_twins$HRWAGEH)/mean(db_twins$HRWAGEH)
CV

# Calcular el coeficiente de variación para el gemelo 2 ("HRWAGEL")
CV <- sd(db_twins$HRWAGEL)/mean(db_twins$HRWAGEL)
CV

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
Q3_2

# Calcular el mínimo para el gemelo 1 ("HRWAGEH")
min(db_twins$HRWAGEH)

# Calcular el mínimo para el gemelo 2 ("HRWAGEL")
min(db_twins$HRWAGEL)

# Calcular el máximo para el gemelo 1 ("HRWAGEH")
max(db_twins$HRWAGEH)

# Calcular el máximo para el gemelo 2 ("HRWAGEL")
max(db_twins$HRWAGEL)


####### UI PAGINA WEB

library(shiny)

# Guardar los resultados de los cálculos en variables
media_twins1 <- mu_twins1
media_twins2 <- mu_twins2
mediana_twins1 <- mediana_twins1
mediana_twins2 <- mediana_twins2
moda_twins1 <- moda_twins1
moda_twins2 <- moda_twins2
rango_twins1 <- range(db_twins$HRWAGEH)
rango_twins2 <- range(db_twins$HRWAGEL)
varianza_twins1 <- var(db_twins$HRWAGEH)
varianza_twins2 <- var(db_twins$HRWAGEL)
desviacion_twins1 <- sd(db_twins$HRWAGEH)
desviacion_twins2 <- sd(db_twins$HRWAGEL)
cv_twins1 <- CV <- sd(db_twins$HRWAGEH)/mean(db_twins$HRWAGEH)
cv_twins2 <- CV <- sd(db_twins$HRWAGEL)/mean(db_twins$HRWAGEL)
Q1_twins1 <- Q1_1
Q1_twins2 <- Q1_2
Q2_twins1 <- Q2_1
Q2_twins2 <- Q2_2
Q3_twins1 <- Q3_1
Q3_twins2 <- Q3_2
min_twins1 <- min(db_twins$HRWAGEH)
min_twins2 <- min(db_twins$HRWAGEL)
max_twins1 <- max(db_twins$HRWAGEH)
max_twins2 <- max(db_twins$HRWAGEL)

# Definir la interfaz de usuario
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      body {
        background-color: #fefcfb;
        color: #0a1128;
        padding-left: 5%;
        padding-right: 5%;
      }
      .titulo-centrado {
        text-align: center;
        font-family: 'Arial', sans-serif;
        font-weight: bold;
        border-radius: 12px;
        background-color: #034078;
        color: #fefcfb;
        padding: 10px;
        padding-top: 20px;
      }
      .imagen {
        display: inline-block;
        width: 50px; /* Ajusta el tamaño de la imagen según tus necesidades */
        height: auto; /* Esto mantendrá la proporción de la imagen */
      }
      .integrantes {
        text-align: center;
        font-size: 16px;
        font-weight: 500;
        margin-top: 20px;
      }
      .docente {
        text-alig: center;
        font-size: 16px;
        font-weight: 500;
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
        text-align: justify;
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
      .linea-horizontal {
        border: none;
        border-top: 5px solid #034078; /* Color de la línea */
        width: 100%; /* Ancho de la línea */
      }
      .seccion-medidas-tc {
        background-color: #cfdbd5; /* Color de fondo de la nueva sección */
        padding: 20px;
        margin-top: 20px; /* Espacio superior */
        border-radius: 10px; /* Bordes redondeados */
      }
      ")
    ),
    tags$script(
      HTML('
      $(document).on("shiny:connected", function(e) {
        $(".nav a").click(function() {
          $(".nav a").removeClass("active");
          $(this).addClass("active");
        });
      });
      ')
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
  
  navbarPage(
    "Barra de Navegación", # Título de la página
    tabPanel("Autores"),
    tabPanel("Contextualización", onClick = "Shiny.setInputValue('nav_clicked', 'introduccion')"),
    tabPanel("Medidas TC"),
    tabPanel("Estimación Kernell"),
    tabPanel("Análisis de Resultados"),
    tabPanel("Conclusiones")
  ),
  
  # Div encapsulador de la sección "Contextualización"
  div(class = "contextualizacion",
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
          'Para el caso númerico, se busco realizar una comparación entre cada gurpo de gemelos (gemelo 1 vs gemelo 2), en el cual se busca contrastar las siguientes medidas: (CENTRO: Media, Mediana, Moda), (DISPERSIÓN: Rango, Varianza, Desviación estandar, CV), (LOCALIZACIÓN: Q1, Q2, Q3, Minimo, Maximo)',
          hr(class = "linea-horizontal") # Línea horizontal
      )
  ),
  
  # Nueva sección para medidas de TC
  div(class = 'seccion-medidas-tc',
      h2(class = "titulo-centrado", "Medidas de TC"),
      # Resultados de los cálculos
      div(class = "resultados",
          paste0("Media Gemelo 1 (HRWAGEH): ", media_twins1),
          br(),
          paste0("Media Gemelo 2 (HRWAGEL): ", media_twins2),
          br(),
          paste0("Mediana Gemelo 1 (HRWAGEH): ", mediana_twins1),
          br(),
          paste0("Mediana Gemelo 2 (HRWAGEL): ", mediana_twins2),
          br(),
          paste0("Moda Gemelo 1 (HRWAGEH): ", moda_twins1),
          br(),
          paste0("Moda Gemelo 2 (HRWAGEL): ", moda_twins2),
          br(),
          paste0("Rango Gemelo 1 (HRWAGEH): ", rango_twins1[2] - rango_twins1[1]),
          br(),
          paste0("Rango Gemelo 2 (HRWAGEL): ", rango_twins2[2] - rango_twins2[1]),
          br(),
          paste0("Varianza Gemelo 1 (HRWAGEH): ", varianza_twins1),
          br(),
          paste0("Varianza Gemelo 2 (HRWAGEL): ", varianza_twins2),
          br(),
          paste0("Desviación Estándar Gemelo 1 (HRWAGEH): ", desviacion_twins1),
          br(),
          paste0("Desviación Estándar Gemelo 2 (HRWAGEL): ", desviacion_twins2),
          br(),
          paste0("Coeficiente de Variación Gemelo 1 (HRWAGEH): ", cv_twins1),
          br(),
          paste0("Coeficiente de Variación Gemelo 2 (HRWAGEL): ", cv_twins2),
          br(),
          paste0("Q1 Gemelo 1 (HRWAGEH): ", Q1_twins1),
          br(),
          paste0("Q1 Gemelo 2 (HRWAGEL): ", Q1_twins2),
          br(),
          paste0("Q2 Gemelo 1 (HRWAGEH): ", Q2_twins1),
          br(),
          paste0("Q2 Gemelo 2 (HRWAGEL): ", Q2_twins2),
          br(),
          paste0("Q3 Gemelo 1 (HRWAGEH): ", Q3_twins1),
          br(),
          paste0("Q3 Gemelo 2 (HRWAGEL): ", Q3_twins2),
          br(),
          paste0("Mínimo Gemelo 1 (HRWAGEH): ", min_twins1),
          br(),
          paste0("Mínimo Gemelo 2 (HRWAGEL): ", min_twins2),
          br(),
          paste0("Máximo Gemelo 1 (HRWAGEH): ", max_twins1),
          br(),
          paste0("Máximo Gemelo 2 (HRWAGEL): ", max_twins2)
      )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$mensaje <- renderText({
    "¡Hola desde mi primera aplicación Shiny!"
  })
  
  observeEvent(input$nav_clicked, {
    scrollToElement(".introduccion")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

