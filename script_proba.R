####### FILTRO PARA LA BASE DE DATOS
# Instalación y Descarga de libreria
if (!require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)

# Cargar paquete necesario si no está instalado
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
library(dplyr)

# Obtención de la ruta del directorio actual
dir <- getwd()

# Importación de base de datos "Twins" utilizando una ruta relativa
twins <- read.csv(file.path(dir, "twins.txt"), header = TRUE)
View(twins)

# Selección de columnas "Twins" 
# función select de dplyr
db <- dplyr::select(twins, HRWAGEH, HRWAGEL, EDUCH, EDUCL)


# Exclusión de datos que se encuentran incompletos
db <- db %>%
  filter(HRWAGEH != "." & HRWAGEL != ".")
View(db)

# selección de los años de estudio
db_twins <- dplyr::select(db, HRWAGEH, HRWAGEL)
View(db_twins)

db_hr <- dplyr::select(db, EDUCH, EDUCL)
View(db_hr)

################### ANALISIS NUMERICO PARA AÑOS DE EDUCACIÓN:

# Cargar las librerías necesarias
install.packages("DescTools")
library("DescTools")
library("dplyr")

# Seleccionar las columnas de interés
db_hr <- dplyr::select(db, EDUCH, EDUCL)
View(db_hr)

# Convertir todas las columnas a numérico
db_hr[] <- lapply(db_hr, as.numeric)

# Calcular la media para EDUCH
mu_hr1 <- mean(db_hr$EDUCH, na.rm = TRUE)
mu_hr1

# Calcular la media para EDUCL
mu_hr2 <- mean(db_hr$EDUCL, na.rm = TRUE)
mu_hr2

# Calcular la mediana para EDUCH
mediana_hr1 <- median(db_hr$EDUCH, na.rm = TRUE)
mediana_hr1

# Calcular la mediana para EDUCL
mediana_hr2 <- median(db_hr$EDUCL, na.rm = TRUE)
mediana_hr2

# Calcular la moda para EDUCH
moda_hr1 <- Mode(db_hr$EDUCH, na.rm = TRUE)
moda_hr1

# Calcular la moda para EDUCL
moda_hr2 <- Mode(db_hr$EDUCL, na.rm = TRUE)
moda_hr2

# Calcular el rango para EDUCH
rango_hr1 <- range(db_hr$EDUCH, na.rm = TRUE)
rango_hr1

# Calcular el rango para EDUCL
rango_hr2 <- range(db_hr$EDUCL, na.rm = TRUE)
rango_hr2

# Calcular la varianza para EDUCH
varianza_hr1 <- var(db_hr$EDUCH, na.rm = TRUE)
varianza_hr1

# Calcular la varianza para EDUCL
varianza_hr2 <- var(db_hr$EDUCL, na.rm = TRUE)
varianza_hr2

# Calcular la desviación estándar para EDUCH
desviacion_hr1 <- sd(db_hr$EDUCH, na.rm = TRUE)
desviacion_hr1

# Calcular la desviación estándar para EDUCL
desviacion_hr2 <- sd(db_hr$EDUCL, na.rm = TRUE)
desviacion_hr2

# Calcular el coeficiente de variación para EDUCH
cv_hr1 <- sd(db_hr$EDUCH, na.rm = TRUE) / mean(db_hr$EDUCH, na.rm = TRUE)
cv_hr1

# Calcular el coeficiente de variación para EDUCL
cv_hr2 <- sd(db_hr$EDUCL, na.rm = TRUE) / mean(db_hr$EDUCL, na.rm = TRUE)
cv_hr2

# Calcular Q1 para EDUCH
Q1_hr1 <- quantile(db_hr$EDUCH, 0.25, na.rm = TRUE)
Q1_hr1

# Calcular Q1 para EDUCL
Q1_hr2 <- quantile(db_hr$EDUCL, 0.25, na.rm = TRUE)
Q1_hr2

# Calcular Q2 para EDUCH
Q2_hr1 <- quantile(db_hr$EDUCH, 0.50, na.rm = TRUE)
Q2_hr1

# Calcular Q2 para EDUCL
Q2_hr2 <- quantile(db_hr$EDUCL, 0.50, na.rm = TRUE)
Q2_hr2

# Calcular Q3 para EDUCH
Q3_hr1 <- quantile(db_hr$EDUCH, 0.75, na.rm = TRUE)
Q3_hr1

# Calcular Q3 para EDUCL
Q3_hr2 <- quantile(db_hr$EDUCL, 0.75, na.rm = TRUE)
Q3_hr2

# Calcular el mínimo para EDUCH
min_hr1 <- min(db_hr$EDUCH, na.rm = TRUE)
min_hr1

# Calcular el mínimo para EDUCL
min_hr2 <- min(db_hr$EDUCL, na.rm = TRUE)
min_hr2

# Calcular el máximo para EDUCH
max_hr1 <- max(db_hr$EDUCH, na.rm = TRUE)
max_hr1

# Calcular el máximo para EDUCL
max_hr2 <- max(db_hr$EDUCL, na.rm = TRUE)
max_hr2

# Redondear los resultados
media_hr1 <- round(mu_hr1, digits = 2)
media_hr2 <- round(mu_hr2, digits = 2)
mediana_hr1 <- mediana_hr1
mediana_hr2 <- mediana_hr2
moda_hr1 <- moda_hr1
moda_hr2 <- moda_hr2
rango_hr1 <- round(rango_hr1, digits = 2)
rango_hr2 <- round(rango_hr2, digits = 2)
varianza_hr1 <- round(varianza_hr1, digits = 2)
varianza_hr2 <- round(varianza_hr2, digits = 2)
desviacion_hr1 <- round(desviacion_hr1, digits = 2)
desviacion_hr2 <- round(desviacion_hr2, digits = 2)
cv_hr1 <- round(cv_hr1, digits = 2)
cv_hr2 <- round(cv_hr2, digits = 2)
Q1_hr1 <- Q1_hr1
Q1_hr2 <- Q1_hr2
Q2_hr1 <- Q2_hr1
Q2_hr2 <- Q2_hr2
Q3_hr1 <- Q3_hr1
Q3_hr2 <- round(Q3_hr2, digits = 2)
min_hr1 <- round(min_hr1, digits = 2)
min_hr2 <- round(min_hr2, digits = 2)
max_hr1 <- round(max_hr1, digits = 2)
max_hr2 <- round(max_hr2, digits = 2)


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

#######UI PAGINA WEB
library(shiny)

# Guardar los resultados de los cálculos en variables
media_twins1 <- round(mu_twins1, digits = 2)
media_twins2 <- round(mu_twins2, digits = 2)
mediana_twins1 <- mediana_twins1
mediana_twins2 <- mediana_twins2
moda_twins1 <- moda_twins1
moda_twins2 <- moda_twins2
rango_twins1 <- round(range(db_twins$HRWAGEH), digits = 2)
rango_twins2 <- round(range(db_twins$HRWAGEL), digits = 2)
varianza_twins1 <- round(var(db_twins$HRWAGEH), digits = 2)
varianza_twins2 <- round(var(db_twins$HRWAGEL), digits = 2)
desviacion_twins1 <- round(sd(db_twins$HRWAGEH), digits = 2)
desviacion_twins2 <- round(sd(db_twins$HRWAGEL), digits = 2)
cv_twins1 <- CV <- round(sd(db_twins$HRWAGEH)/mean(db_twins$HRWAGEH), digits = 2)
cv_twins2 <- CV <- round(sd(db_twins$HRWAGEL)/mean(db_twins$HRWAGEL), digits = 2)
Q1_twins1 <- Q1_1
Q1_twins2 <- Q1_2
Q2_twins1 <- Q2_1
Q2_twins2 <- Q2_2
Q3_twins1 <- Q3_1
Q3_twins2 <- round(Q3_2, digits = 2)
min_twins1 <- round(min(db_twins$HRWAGEH), digits = 2)
min_twins2 <- round(min(db_twins$HRWAGEL), digits = 2)
max_twins1 <- round(max(db_twins$HRWAGEH), digits = 2)
max_twins2 <- round(max(db_twins$HRWAGEL), digits = 2)

######CARGA PARA LA ESTIMACIÓN KERNELL
# Crear nuevas variables numéricas a partir de las columnas existentes en db_twins
db_twins_numeric <- db_twins %>%
  mutate(HRWAGEH_numeric = as.numeric(HRWAGEH),
         HRWAGEL_numeric = as.numeric(HRWAGEL))

# Verificar el resultado
str(db_twins_numeric)

# Calcular la densidad de kernel para HRWAGEH_numeric
densidad_hrwageh_numeric <- density(db_twins_numeric$HRWAGEH_numeric)

# Graficar la densidad de kernel para HRWAGEH_numeric
plot(densidad_hrwageh_numeric, main = "Densidad de Kernel para HRWAGEH_numeric",
     xlab = "HRWAGEH_numeric", ylab = "Densidad")

# Calcular la densidad de kernel para HRWAGEL_numeric
densidad_hrwagel_numeric <- density(db_twins_numeric$HRWAGEL_numeric)

# Graficar la densidad de kernel para HRWAGEL_numeric
plot(densidad_hrwagel_numeric, main = "Densidad de Kernel para HRWAGEL_numeric",
     xlab = "HRWAGEL_numeric", ylab = "Densidad")

######POSIBLE DEPLOY DE LOS GRAFICO DE ESTIMACIÓN KERNELL
install.packages("DT")
library(DT)

# Definir la interfaz de usuario
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      body {
        background-color: #b8d0eb;
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
        padding-bottom: 30px;
        margin-gottom: 10px;
      }
      .imagen {
        display: inline-block;
        width: 50px; /* Ajusta el tamaño de la imagen según tus necesidades */
        height: auto; /* Esto mantendrá la proporción de la imagen */
      }
      .autores {
        font-size: 22px;
        font-weight: bold;
        margin-bottom: 20px;
        border-radius: 10px;
      }
      .integrantes {
        display: flex;
        flex-wrap: nowrap; /* Evita que las tarjetas se envuelvan a la siguiente línea */
        justify-content: center; /* Centra las tarjetas horizontalmente */
        gap: 20px;
        background-color: #034078;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 10px;
      }
      .tarjeta {
        border: 1px solid #ccc;
        border-radius: 8px;
        box-shadow: 2px 2px 5px rgba(0,0,0,0.1);
        background-color: #fff;
        padding: 20px;
        width: 250px; /* Ancho de cada tarjeta */
        display: flex;
        flex-direction: column;
        align-items: center;
        text-align: center;
        border-radius: 10px;
      }
      .tarjeta img {
        border-radius: 50%;
        width: 150px; /* Ancho de la imagen */
        height: 150px; /* Altura de la imagen */
        margin-bottom: 10px;
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
      .tratamiento_datos {
        font-size: 16px;
        margin: 20px;
      }
      .tabla-container {
        display: flex; /* Establece el contenedor como flexbox */
        justify-content: center; /* Centra los elementos del contenedor */
        gap: 20px; /* Espacio entre las tablas */
      }
      .tabla-info {
        border-collapse: collapse; /* Fusiona los bordes de las celdas */
      }
      .tabla-info th, .tabla-info td {
        border: 1px solid black; /* Añade bordes a las celdas */
        padding: 8px; /* Añade espacio interno a las celdas */
        text-align: center; /* Centra el texto dentro de las celdas */
      }
      .cont_datos {
        font-size: 16px;
        margin: 20px;
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
      .seccion-estimacion-kernell {
        text-align: center; /* Centra los elementos dentro de la sección */
        padding-top: 10px;
        padding-bottom: 10px;
      }
      .boton-container {
        margin-bottom: 20px; /* Espacio entre los botones y las gráficas */
        padding-bottom: 20px;
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
  tags$head(
    tags$script(HTML('
      $(document).ready(function(){
        $("#btn_densidad_hrwageh").click(function(){
          $("#densidad_hrwageh_numeric").show();
          $("#densidad_hrwagel_numeric").hide();
        });
        $("#btn_densidad_hrwagel").click(function(){
          $("#densidad_hrwagel_numeric").show();
          $("#densidad_hrwageh_numeric").hide();
        });
      });
    '))
  ),
  titlePanel(
    div(class = "titulo-centrado", 
        img(src = file.path(dir, "imagen_izquierda.png"), class = "imagen"),
        "Proyecto Final Probabilidad y Estadistica 1",
        img(src = file.path(dir, "imagen_derecha.png"), class = "imagen")
    )
  ),
  
  navbarPage(
    "Barra de Navegación", # Título de la página
    tabPanel("Autores", onClick = "shiny.setInputValue('nav_clicked', 'integrantes')"),
    tabPanel("Contextualización", onClick = "Shiny.setInputValue('nav_clicked', 'introduccion')"),
    tabPanel("Datos Estadísticos"),
    tabPanel("Estimación Kernell"),
    tabPanel("Análisis de Resultados"),
    tabPanel("Conclusiones")
  ),
  div(class = 'autores', 'Autores:'),
  div(class = 'integrantes', 
      div(class = 'tarjeta estudiante1',
          h3('Manuel Cruz G'),
          img(src = 'est1_img.jpg'), 
          p('Estudiante MACC'),
          p('Esgrimista profesional'),
          p('manuels.cruz@urosario.edu.co')
      ),
      div(class = 'tarjeta estudiante2',
          h3('Mariana Romero R'),
          img(src = 'est1_img.jpg'), 
          p('Estudiante MACC'),
          p('Estudiante Ing. Sis. Energeticos'),
          p('mariana.lu@urosario.edu.co')
      ),
      div(class = 'tarjeta estudiante3',
          h3('Nicolas López L'),
          img(src = 'est1_img.jpg'), 
          p('Docente'),
          p('Estadístico (BSc & MSc)'),
          p('nico@urosario.edu.co')
      ),
  ),
  
  # Div encapsulador de la sección "Contextualización"
  div(class = "contextualizacion",
      # Introducción 
      div(class = 'introduccion',
          'Introducción:'
      ),
      # Nue:vo apartado para añadir texto explicativo
      div(class = "explicacion",
          "En esta página se presenta el proyecto final para la asignatura 
          Probabilidad y Estadística 1, correspondiente al semestre 2024-1.
          En esta aplicación, se muestran los resultados del análisis 
          estadístico sobre un conjunto de datos específico. Utilizamos la 
          plataforma Shiny para crear una interfaz interactiva que permite a los 
          usuarios explorar los datos y entender mejor nuestras conclusiones."
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
      div(class = 'tratamiento_datos',
          'Para el tratamiento de datos se importo la base de datos que fue brindada
          para el desarrollo del analisis. En esta se encontraron ciertas observaciones que pueden ser relevante y tener importancia a lo largo del analisis de resultados:'
      ),
      div(class = 'tabla-container',
          div(class = 'tabla-info',
              HTML("
        <table class='tabla-info'>
          <tr>
            <th>Registros</th>
            <th>Variables</th>
          </tr>
          <tr>
            <td>183</td>
            <td>16</td>
          </tr>
        </table>
      ")
          ),
          div(class = 'tabla-info',
              HTML("
        <table class='tabla-info'>
          <tr>
            <th>Datos completos</th>
            <th>Datos incompletos</th>
          </tr>
          <tr>
            <td>149</td>
            <td>34</td>
          </tr>
        </table>
      ")
          )
      ),
      div(class = 'cont_datos', 
          'Como ultimo paso de la preparación de la base de datos, se eliminan los datos
          con información faltante, llegando a una dimensión final de 149 datos con 6
          variables'
      ),
      div(class = 'desc_grafica', 
          'Para contar con una claridad y mayor profundidad en el estudio realizado
          se describio el reporte de manera gráfica mediante el uso del grafico
          "Estimación Kernell". Para llegar a la mejor comprensión de esta herramienta
          grafica es necesario entender sus ventaja, desventajas y como leer este gráfico.
          Ciertos conceptos fueron adquirido a lo largo de la materia "Probabilidad y 
          Estadistica I" a lo largo del semestre 2024-1'
      ),
      div(class = 'tabla-container',
          style = 'display: flex; justify-content: center; gap: 20px;',  # Estilo para centrar el contenedor
          div(class = 'tabla-info',
              style = 'margin-left: auto; margin-right: auto;',  # Estilo para centrar la tabla
              HTML("
          <table class='tabla-info'>
            <tr>
              <th>Categoría</th>
              <th>Descripción</th>
            </tr>
            <tr>
              <td>Ventajas</td>
              <td>
                <ul>
                  <li>Proporciona una representación suave y continua de la distribución de datos.</li>
                  <li>Permite visualizar la forma y la estructura de la distribución de datos, especialmente en casos de datos multimodales o con formas no normales.</li>
                  <li>No requiere la especificación de una distribución paramétrica, lo que lo hace útil cuando no se conocen las características exactas de los datos.</li>
                  <li>Puede ser útil para identificar patrones y anomalías en los datos.</li>
                </ul>
              </td>
            </tr>
            <tr>
              <td>Desventajas</td>
              <td>
                <ul>
                  <li>La interpretación puede ser subjetiva y depende en gran medida de la elección del ancho de banda.</li>
                  <li>Puede ser sensible a la elección del núcleo utilizado para suavizar la distribución.</li>
                  <li>No proporciona medidas numéricas directas de la distribución, como la media o la varianza.</li>
                  <li>Puede ser computacionalmente costoso para conjuntos de datos grandes debido al cálculo de densidades en cada punto.</li>
                </ul>
              </td>
            </tr>
          </table>
        ")
          )
      )
      ,
      div(class = 'desc_numerica',
          'Como ultimo paso para el analisis de los datos, se busca dar un reporte numerico
          para los datos correspondientes al estudio. En este reporte se puede apreciar medidas
           numericas e centro, dispersión y localización para los gemelos L y H.',
           hr(class = "linea-horizontal") # Línea horizontal
      )
  ),
  
  # Nueva sección para datos estadísticos
  div(class = 'seccion-medidas-tc',
      h2(class = "titulo-centrado", "Datos Estadísticos para ingresos por hora de gemelos H y L"),
      # Resultados de los cálculos
      DTOutput("statisticsTable")
      
  ),
  div(class = 'seccion-medidas-tc-2', 
      h2(class = "titulo-centrado", "Datos Estadísticos para los años de educación de gemelos H y L")
  ),
  # Nueva sección para estimación kernell
  div(class = 'seccion-estimacion-kernell',
      h2(class = "titulo-centrado", "Estimación Kernell para ingresos por hora de gemelos H y L"),
      actionButton("btn_densidad_hrwageh", "Mostrar Gráfica de HRWAGEH"),
      actionButton("btn_densidad_hrwagel", "Mostrar Gráfica de HRWAGEL"),
      plotOutput("densidad_hrwageh_numeric"),
      plotOutput("densidad_hrwagel_numeric")
  ),

  # Nueva sección para análisis de resultados
  div(class = 'seccion-analisis-de-resultados',
      h2(class = "titulo-centrado", "Análsis de Resultados"),
  ),

  # Nueva sección para conclusiones
  div(class = 'seccion-conclusiones',
      h2(class = "titulo-centrado", "Conclusiones"),
  ),
)

# Define server logic
server <- function(input, output, session) {
  output$mensaje <- renderText({
    "¡Hola desde mi primera aplicación Shiny!"
  })
  observeEvent(input$nav_clicked, {
    scrollToElement(".introduccion")
  })
  output$statisticsTable <- renderDT({
    datatable(resultados, options = list(pageLenght = 12, autowidth = TRUE))
  })
  
  # Crear nuevas variables numéricas a partir de las columnas existentes en db_twins
  db_twins_numeric <- db_twins %>%
    mutate(HRWAGEH_numeric = as.numeric(HRWAGEH),
           HRWAGEL_numeric = as.numeric(HRWAGEL))
  
  # Calcular la densidad de kernel para HRWAGEH_numeric
  densidad_hrwageh_numeric <- density(db_twins_numeric$HRWAGEH_numeric)
  
  # Graficar la densidad de kernel para HRWAGEH_numeric
  output$densidad_hrwageh_numeric <- renderPlot({
    plot(densidad_hrwageh_numeric, main = "Densidad de Kernel para Gemelo H",
         xlab = "Ingreso por horas Gemelo H ($USD)", ylab = "Densidad")
  })
  
  # Calcular la densidad de kernel para HRWAGEL_numeric
  densidad_hrwagel_numeric <- density(db_twins_numeric$HRWAGEL_numeric)
  
  # Graficar la densidad de kernel para HRWAGEL_numeric
  output$densidad_hrwagel_numeric <- renderPlot({
    plot(densidad_hrwagel_numeric, main = "Densidad de Kernel para Gemelo L",
         xlab = "Ingreso por horas Gemelo L ($USD)", ylab = "Densidad")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
