# Preparación plataforma de visualización indicadores de género USACH 2018-2024
# Cargar paquetes
pacman::p_load(fontawesome, here, ggrepel, markdown, plotly, RColorBrewer, readxl, scales, shiny, shinythemes, tidyverse)

source("02-proc/graficos_plotly_app.R")

ui <- navbarPage(
  id = "tabs",
  title = div(
    style = "color: white; font-weight: bold;",
    "Plataforma interactiva de visualización: Sistema de Indicadores de Género Estratégicos (SIGEs)"
  ),
  theme = shinytheme("superhero"),
  
  header = tags$head(
    tags$style(HTML("
      body {
        background-color: #F4F4F4;
      }
      .navbar {
        background-color: #00A499 !important;
      }
      .navbar-default .navbar-nav > li > a {
        color: white !important;
        font-weight: bold;
      }
      .navbar-default .navbar-brand {
        color: white !important;
      }
      .tab-content {
        background-color: white;
        padding: 20px;
        border-radius: 10px;
      }
      .plotly html-widget {
        padding: 10px;
      }
      .welcome-panel {
        background-color: #EA7600;
        color: white;
        text-align: center;
        padding: 50px;
        border-radius: 10px;
      }
      .welcome-title {
        font-size: 40px;
        font-weight: bold;
      }
      .welcome-subtitle {
        font-size: 20px;
        margin-top: 20px;
      }
      .indicator {
        font-size: 18px;
        margin-top: 10px;
      }
      .button-group {
        margin-top: 40px;
      }
      .button {
  margin: 10px;
  padding: 15px 25px;
  background-color: #394049;
  color: white;
  border: none;
  border-radius: 5px;
  font-size: 16px;
  cursor: pointer;
  transition: background-color 0.3s ease, transform 0.1s ease;
}
.button:hover {
  background-color: #4F565F;
}
.button:active {
  transform: scale(0.97);
}
      .tab-description {
        font-size: 16px;
        margin-bottom: 20px;
        color: #333333;
      }
    "))
  ),
  
  # Portada ----
  tabPanel("Inicio",
           fluidPage(
             div(
               class = "welcome-panel",
               h1(class = "welcome-title", "Plataforma interactiva de visualización: Sistema de Indicadores de Género Estratégicos (SIGEs)"),
               p(class = "welcome-subtitle", "En la ruta de la reacreditación institucional con perspectiva de género"),
               tags$a(href = "https://github.com/diegoesturillo/Plataforma-Indicadores", target = "_blank",
                      tags$img(src = "logo_ogd.png", height = "200px", 
                               style = "margin-top: 50px; margin-bottom: 20px; cursor: pointer;")
               ),
               tags$div(
                 tags$img(src = "vicaviged_dged_n.png", height = "100px", 
                          style = "display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;")
               ),
               br(),
               br(),
               div(class = "button-group",
                   actionButton("go_presentacion", "Presentación", class = "button"),
                   actionButton("go_academicos", "Estamento académico y docente", class = "button"),
                   actionButton("go_jce", "Jornadas Completas Equivalentes (JCE)", class = "button"),
                   actionButton("go_jerarquia", "Jerarquía académica", class = "button"),
                   actionButton("go_jerarquia_jce", "Jerarquía académica JCE", class = "button"),
                   actionButton("go_publicaciones", "Publicaciones", class = "button"),
                   actionButton("go_directivos", "Puestos Directivos", class = "button"),
                   actionButton("go_genero", "Matrícula por género", class = "button"),
                   actionButton("go_area", "Matrícula por área del conocimiento", class = "button"),
                   actionButton("go_titulados", "Titulación por género y área del conocimiento", class = "button"),
                   actionButton("go_desglose_titulados", "Desglose titulación por área del conocimiento", class = "button"),
                   actionButton("go_cuidados_corresp", "Cuidados y corresponsabilidad", class = "button"),
                   actionButton("go_lgbt", "LGBTIQA+", class = "button"),
                   actionButton("go_violencia", "Diagnóstico violencia de género", class = "button"),
                   div(style = "text-align: center; margin-top: 30px; color: #FFF; font-size: 0.9em;",
                       fa("sync", fill = "#FFF"),  
                       span(paste("Última actualización:", format(Sys.Date(), "%d de %b de %Y")))
                   )
                  )
                 )
                )
               ),
  # Presentación----
  tabPanel(
    "Presentación",
    fluidPage(
      div(
        style = "background-color: #ffffff;
               padding: 30px;
               border-radius: 10px;
               box-shadow: 0 4px 8px rgba(0,0,0,0.1);
               color: #212529;",
        includeMarkdown("www/presentacion.md")
      )
    )
  ),
  # Estamento académico y docente----
  tabPanel("Estamento académico y docente",
           div(
             style = "background-color: #f0f0f0;
                  border-left: 6px solid #00A499;
                  border-right: 6px solid #00A499;
                  padding: 15px 20px;
                  margin-bottom: 20px;
                  border-radius: 8px;
                  font-size: 16px;
                  color: #333;
                  box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
             tags$p("En esta sección se muestra una caracterización del estamento académico y docente por género y grado académico para el periodo 2018-2024."),
             tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
             tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada.")
           ),
         fluidPage(
           fluidRow(
             column(6, plotlyOutput("p1")),
             column(6, plotlyOutput("p2")),
           fluidRow(
             column(6, plotlyOutput("p3")),
             column(6, plotlyOutput("p4")))
           )
          ),
         fluidPage(
           div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                      "Notas metodológicas:"),
               tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                      "(1) El estamento académico y docente está compuesto por todas aquellas personas que realicen docencia, 
                      independiente de su situación contractual (planta/contrata u honorarios) y jerarquización, por lo que contempla académicos y profesoras por hora de clase. Se excluyen ayudantes en situación becarial."),
               tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                      "(2) El recuento del estamento académico y docente en esta pestaña está hecho en base al número de docentes (cantidad de personas)"),
               tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                      "(3) A su vez, se considera  en el recuento tanto al personal en calidad jurídica de planta o contrata como honorarios.")
           )
          )
         ),
  # Estamento académico por JCE----
  tabPanel("Jornadas Completas Equivalentes (JCE)",
           div(
             style = "background-color: #f0f0f0;
                  border-left: 6px solid #00A499;
                  border-right: 6px solid #00A499;
                  padding: 15px 20px;
                  margin-bottom: 20px;
                  border-radius: 8px;
                  font-size: 16px;
                  color: #333;
                  box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
             tags$p("En esta sección se muestra una caracterización del estamento académico por género y grado académico para el periodo 2018-2024."),
             tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
             tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada.")
           ),
           fluidPage(
             fluidRow(
               column(6, plotlyOutput("p5")),
               column(6, plotlyOutput("p6")),
               fluidRow(
                 column(6, plotlyOutput("p7")),
                 column(6, plotlyOutput("p8")))
             )
           ),
           fluidPage(
             div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
                 tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                        "Notas metodológicas:"),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(1) De acuerdo con la Subsecretaría de Educación Superior (SES, 2024), las Jornadas Completas Equivalentes (JCE) 
                        son una unidad de comparación de la dedicación horaria del personal académico (y docente), que se obtiene al dividir el número total de horas por las que está contratada/o
                        una o un académica/o en una institución de educación superior (IES) por 44, donde esta última cifra corresponde o se asimila a una jornada de trabajo completa normal"),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(2) El recuento presentando en estos gráficos corresponde a las Jornadas Completas Equivalentes (JCE)."),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(3) A su vez, se considera  en el recuento tanto al personal en calidad jurídica de planta o contrata como honorarios.")
             )
           )
  ),
  # Jerarquía académica----
  tabPanel("Jerarquía académica",
           fluidPage(
             div(
               style = "background-color: #f0f0f0;
                      border-left: 6px solid #00A499;
                      border-right: 6px solid #00A499;
                      padding: 15px 20px;
                      margin-bottom: 20px;
                      border-radius: 8px;
                      font-size: 16px;
                      color: #333;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               tags$p("En esta sección se pueden visualizar las diferencias de jerarquía académica por género, considerando los distintos niveles de la carrera académica para el periodo 2018-2024."
             ),
             tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
             tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada.")
             ),
             fluidRow(
               column(6, plotlyOutput("p9")),
               column(6, plotlyOutput("p10"))
             ),
             fluidPage(
               div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
                   tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                          "Notas metodológicas:"),
                   tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                          "(1) El recuento del estamento académico y docente por jerarquía en esta pestaña considera el número de docentes ( cantidad de personas)."),
                   tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                          "(2) Considerar que las jerarquías titular, asociado/a, asistente e instructor/a aplican solamente para el cuerpo académico regular y no para el profesorado por hora."),
                   tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                          "(3) A su vez, se considera  en el recuento tanto al personal en calidad jurídica de planta o contrata como honorarios.")
               )
             )
           )
  ),
  # Jerarquía académica por JCE----
  tabPanel("Jerarquía académica JCE",
           fluidPage(
             div(
               style = "background-color: #f0f0f0;
                      border-left: 6px solid #00A499;
                      border-right: 6px solid #00A499;
                      padding: 15px 20px;
                      margin-bottom: 20px;
                      border-radius: 8px;
                      font-size: 16px;
                      color: #333;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               tags$p("En esta sección se pueden visualizar las diferencias de jerarquía académica por género, considerando los distintos niveles de la carrera académica para el periodo 2018-2024."
               ),
               tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
               tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada.")
             ),
             fluidRow(
               column(6, plotlyOutput("p11")),
               column(6, plotlyOutput("p12"))
             ),
             fluidPage(
               div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
                   tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                          "Notas metodológicas:"),
                   tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                          "(1) El recuento del estamento académico y docente por jerarquía en esta pestaña considera las Jornadas Completas Equivalentes (JCE)."),
                   tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                          "(2) A su vez, se considera  en el recuento tanto al personal en calidad jurídica de planta o contrata como honorarios.")
               )
             )
           )
  ),
  # Publicaciones en revistas indexadas----
  tabPanel("Publicaciones",
           fluidPage(
             div(
               style = "background-color: #f0f0f0;
                      border-left: 6px solid #00A499;
                      border-right: 6px solid #00A499;
                      padding: 15px 20px;
                      margin-bottom: 20px;
                      border-radius: 8px;
                      font-size: 16px;
                      color: #333;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               tags$p("Indicador de producción científica y académica en la USACH, diferenciada por género y área del conocimiento para el periodo 2018-2024."
             ),
             tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
             tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada.")
             ),
             fluidRow(
               column(12, align = "center",
                      div(style = "width: 50%;", plotlyOutput("p13"))),
               fluidRow(column(6, plotlyOutput("p14")),
                        column(6, plotlyOutput("p15"))),
               fluidRow(column(6, plotlyOutput("p16")),
                        column(6, plotlyOutput("p_scielo"))
               ),
               fluidRow(
                 column(12, align = "center",
                        div(style = "width: 50%;", plotlyOutput("p17")))
               )
               ),
             fluidPage(
               div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
                   tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                          "Notas metodológicas:"),
                   tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                          "(1) Para contabilizar las publicaciones, se agruparon y consolidaron los id's de publicación en función del año y género para las autorías, lo que entrega la cantidad de publicaciones únicas."),
                   tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                          "(2) Los gráficos presentados muestran  principalmente las diferencias por géneros basándose en autorías de un solo sexo. 
                          Se añadió también una tercera línea que muestra las publicaciones con autorías de género mixto (al menos un hombre o una mujer como autor/a de una publicación)."),
                   tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                          "(3) Los gráficos 14, 15, 16 y 17 (N° de publicaciones por base de datos/directorio de indexación) considera publicaciones que pueden estar indexadas en más de una misma base de datos/directorio.
                          Es por esto que no se consideran estrictamente publicaciones únicas."),
                   tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                          "(4) Para respetar la parsimonia visual, el gráfico 18 muestra las publicaciones por disciplina OECD. Al pasar el cursos sobre los puntos, se puede ver el desglose por género.")
                   )
                  )
                 )
           ),
  # Puestos directivos----
  tabPanel("Puestos directivos",
           fluidPage(
             div(
               style = "background-color: #f0f0f0;
                      border-left: 6px solid #00A499;
                      border-right: 6px solid #00A499;
                      padding: 15px 20px;
                      margin-bottom: 20px;
                      border-radius: 8px;
                      font-size: 16px;
                      color: #333;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               tags$p("Visualización de la representación femenina en cargos directivos dentro de la universidad para el periodo 2018-2024."),
             tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
             tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada.")
             ),
             fluidRow(
               column(6, plotlyOutput("p18")),
               column(6, plotlyOutput("p19")),
               
             ),
             br(),
             fluidRow(
               column(6, plotlyOutput("p20")),
               column(6, plotlyOutput("p21"))
               )
              ),
           fluidPage(
             div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
                 tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                        "Notas metodológicas:"),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(1) Los datos presentados consideran hasta agosto de 2024. Para los vicedecanatos se cuenta con información desde el año 2021."),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(2) Cabe mencionarse que han habido cambios en la estructura orgánica de la Universidad, por que lo ha ido variando la cantidad de puestos requeridos."),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(3) No se graficó el puesto de rectoría ni prorrectoria porque es representado por una (1) sola persona."),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(4) Los puestos de direcciones y jefaturas competen puestos tanto académicos como administrativos.")
                 )
                )
               ),
  # Matrícula de pregrado por género----
  tabPanel("Matrícula por género",
           fluidPage(
             div(
               style = "background-color: #f0f0f0;
                      border-left: 6px solid #00A499;
                      border-right: 6px solid #00A499;
                      padding: 15px 20px;
                      margin-bottom: 20px;
                      border-radius: 8px;
                      font-size: 16px;
                      color: #333;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               tags$p("En esta sección se muestra la distribución de la matrícula estudiantil total, de 1er año y la tasa de retención de 1er año por género para el periodo 2018-2024."
             ),
             tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
             tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada.")
             ),
             fluidRow(
               column(6, plotlyOutput("p22")),
               column(6, plotlyOutput("p23"))
             ),
             br(),
             fluidRow(
               column(12, 
                      tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                      h4(style = "text-align: center; color: #00A499; font-weight: bold;")
               )
             ),
             fluidRow(
               column(12, align = "center",
                      div(style = "width: 50%;", plotlyOutput("p24"))
                      )
                     )
                    ),
           fluidPage(
             div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
                 tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                        "Notas metodológicas:"),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(1) Los números presentados en los gráficos consideran solamente planes regulares."),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(2) Para el gráfico 24 se cuenta con información hasta el año 2023."),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(3) La tasa de retención de 1er año se entiende, según el MINEDUC (2023), como 
                        el porcentaje de estudiantes que estando matriculadas/os en la carrera respectiva el año de referencia 
                        como estudiantes de primer año, se mantiene en la misma institución y en la misma generación o cohorte de origen.
                        La ecuación sería: (Número de estudiantes de la cohorte 'n' matriculados como alumnos regulares en el año 'n+1/Número total de estudiantes de la cohorte 'n')* 100.")
                 )
                )
               ),
  # Matrícula de pregrado por área de conocimiento----
  tabPanel("Matrícula por área del conocimiento",
           fluidPage(
             div(
               style = "background-color: #f0f0f0;
                      border-left: 6px solid #00A499;
                      border-right: 6px solid #00A499;
                      padding: 15px 20px;
                      margin-bottom: 20px;
                      border-radius: 8px;
                      font-size: 16px;
                      color: #333;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               tags$p("En esta sección se puede comparar la matrícula estudiantil total, de 1er año y la tasa de retención de 1er año por género y área del conocimiento genérica para el periodo 2018-2024."
             ),
             tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
             tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada.")
             ),
             fluidRow(
               column(6, plotlyOutput("p25")),
               column(6, plotlyOutput("p26"))
             ),
             fluidRow(
               column(12, 
                      tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                      h4(style = "text-align: center; color: #00A499; font-weight: bold;")
               )
             ),
             br(),
             fluidRow(
               column(12, plotlyOutput("p27"))
               )
              ),
           fluidPage(
             div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
                 tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                        "Notas metodológicas:"),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(1) Los números presentados en los gráficos consideran solamente planes regulares."),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(2) Para facilitar la visualización, se muestran en los gráficos las áreas de conocimiento genéricas que utiliza el MINEDUC.
                        Sin embargo, dentro de la misma base de datos de matrícula se integran columnas aparte con la clasificación por áreas y sub-áreas CINE-F 1997 y 2013 de la UNESCO."),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(3) La tasa de retención de 1er año se entiende, según el MINEDUC (2023), como 
                        el porcentaje de estudiantes que estando matriculadas/os en la carrera respectiva el año de referencia 
                        como estudiantes de primer año, se mantiene en la misma institución y en la misma generación o cohorte de origen.
                        La ecuación sería: (Número de estudiantes de la cohorte 'n' matriculados como alumnos regulares en el año 'n+1/Número total de estudiantes de la cohorte 'n')* 100."),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(4) Para el gráfico 27 se cuenta con información hasta el año 2023. Para el caso de la tasa de retención para Derecho (área de conocimiento y carrera en sí misma), al ser un programa relativamente nuevo, se tienen datos desde el año 2019.")
                 )
                )
               ),
  # Titulación----
  tabPanel("Titulación por género y área del conocimiento",
           fluidPage(
             div(
               style = "background-color: #f0f0f0;
                      border-left: 6px solid #00A499;
                      border-right: 6px solid #00A499;
                      padding: 15px 20px;
                      margin-bottom: 20px;
                      border-radius: 8px;
                      font-size: 16px;
                      color: #333;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               tags$p("En esta sección se puede ver la distribución de la titulación por género y área del conocimiento genérica en el periodo 2018-2024."
             ),
             tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
             tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada.")
             ),
             fluidRow(
               column(6, plotlyOutput("p28")),
               column(6, plotlyOutput("p29"))
               )
              ),
           fluidPage(
             div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
                 tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                        "Notas metodológicas:"),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(1) Los números presentados en los gráficos consideran solamente planes regulares.
                        A su vez, refiere al término de planes comunes/bachilleratos o ciclos iniciales, licenciaturas no conducentes a título profesional,
                        profesionales con y sin licenciatura y técnicos de nivel superior."),
                 tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                        "(2) Se priorizó la visualización por áreas de conocimiento génericas que utiliza el MINEDUC.
                        Sin embargo, dentro de la misma base de datos de matrícula se integran columnas aparte con la clasificación por áreas y sub-áreas CINE-F 1997 y 2013 de la UNESCO.")
                 )
                )
               ),
  # Desglose titulación----
  tabPanel(
    "Desglose titulación por área del conocimiento",
    fluidPage(
      div(
        style = "background-color: #f0f0f0;
                      border-left: 6px solid #00A499;
                      border-right: 6px solid #00A499;
                      padding: 15px 20px;
                      margin-bottom: 20px;
                      border-radius: 8px;
                      font-size: 16px;
                      color: #333;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
        tags$p("En esta sección se muestra el desglose de la titulación por género y área del conocimiento genérica en el periodo 2018-2024."
      ),
      tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
      tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada.")
      ),
    fluidRow(
      column(6, plotlyOutput("p30")),
      column(6, plotlyOutput("p31"))
    ),
    fluidRow(
      column(6, plotlyOutput("p32")),
      column(6, plotlyOutput("p33"))
    ),
    fluidRow(
      column(6, plotlyOutput("p34")),
      column(6, plotlyOutput("p35"))
    ),
    fluidRow(
      column(6, plotlyOutput("p36")),
      column(6, plotlyOutput("p37"))
    ),
    fluidRow(
      fluidRow(
        column(12, align = "center",
               div(style = "width: 50%;", plotlyOutput("p38"))))
      )
     ),
    fluidPage(
      div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
          tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                 "Notas metodológicas:"),
          tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                 "(1) Los números presentados en los gráficos consideran planes regulares."),
          tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                 "(2) Se priorizó la visualización por áreas de conocimiento génericas que utiliza el MINEDUC.
                        Sin embargo, dentro de la misma base de datos de matrícula se integran columnas aparte con la clasificación por áreas y sub-áreas CINE-F 1997 y 2013 de la UNESCO.")
      )
     )
    ),
  # Cuidados y corresponsabilidad----
  tabPanel("Cuidados y corresponsabilidad",
           fluidPage(
             div(
               style = "background-color: #f0f0f0;
                      border-left: 6px solid #00A499;
                      border-right: 6px solid #00A499;
                      padding: 15px 20px;
                      margin-bottom: 20px;
                      border-radius: 8px;
                      font-size: 16px;
                      color: #333;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               tags$p("En esta sección se puede ver la distribución de cuidados de niñas, niños y adolescentes (NNA) y personas adultas con dependencia funcional en la comunidad universitaria."
             ),
           tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
           tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada."),
           tags$p("Para más detalles de los resultados correspondientes a estos estudios, revisar la página web de la",
           tags$a("Dirección de Género, Equidad y Diversidad", href = "https://direcciondegenero.usach.cl/documentos-0",
                  target = "_blank")
           ),
  ),
             fluidRow(
               column(6, plotlyOutput("p39")),
               column(6, plotlyOutput("p40")),
               fluidRow(
                 column(12, 
                        tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                        h4("Distribución de horas y días de cuidado", style = "text-align: center; color: #00A499; font-weight: bold;")
                 )
               ),
               
               fluidRow(
                 column(6, plotlyOutput("p41")),
                 column(6, plotlyOutput("p42")),
                 fluidRow(
                   column(12, 
                          tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                          h4("Estrategias individuales de conciliación, efectos subjetivos y apoyos desde la comunidad", style = "text-align: center; color: #00A499; font-weight: bold;")
               ),
               fluidRow(
                 column(6, plotlyOutput("p43")),
                 column(6, plotlyOutput("p44")),
                 
                 fluidRow(
                   column(12, align = "center",
                          div(style = "width: 50%;", plotlyOutput("p45"))
                          )
                         )
                        )
                       )
                      )
                     )
                    ),
  fluidPage(
    div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
        tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
               "Notas metodológicas:"),
        tags$p(style = "margin-top: 0; margin-bottom: 0;", 
               "(1) Los gráficos  están basados en los resultados de la Encuesta Cuidados y Corresponsabilidad en la Usach, realizada el año 2023."),
        tags$p(style = "margin-top: 0; margin-bottom: 0;", 
               "(2) La muestra obtenida fue de 1.077 personas de todos los estamentos. Sin embargo, para la mayoría de los gráficos se considera la población de género femenino y masculino, con un recuento de 1.040 personas."),
        tags$p(style = "margin-top: 0; margin-bottom: 0;", 
               "(3) Se realizaron pruebas estadísticas (Alpha de Cronbach) para medir confiabilidad de la batería de indicadores del instrumento y los datos constatan que los resultados son consistentes.")
        )
       )
      ),
# LGBTIQA+----
tabPanel("LGBTIQA+",
         fluidPage(
           div(
             style = "background-color: #f0f0f0;
                      border-left: 6px solid #00A499;
                      border-right: 6px solid #00A499;
                      padding: 15px 20px;
                      margin-bottom: 20px;
                      border-radius: 8px;
                      font-size: 16px;
                      color: #333;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
             tags$p("En esta sección se pueden ver distintos indicadores relativos a la experiencia de vida de la comunidad LGBTQIA+ en la universidad."
           ),tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
           tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada."),
           tags$p("Para más detalles de los resultados correspondientes a estos estudios, revisar la página web de la",
                  tags$a("Dirección de Género, Equidad y Diversidad", href = "https://direcciondegenero.usach.cl/documentos-0",
                         target = "_blank")
           ),
           ),
           fluidRow(
             column(6, plotlyOutput("p46")),
             column(6, plotlyOutput("p47")),
             column(6, plotlyOutput("p48")),
             column(6, plotlyOutput("p49")),
             fluidRow(
               column(12, 
                      tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                      h4("Tipos de violencias experimentadas por la población LGBTIQA+, por identidad de género", style = "text-align: center; color: #00A499; font-weight: bold;")
               )
             ),
           ),
           fluidRow(
             column(6, plotlyOutput("p50")),
             column(6, plotlyOutput("p51"))
           ),
           fluidRow(
             column(6, plotlyOutput("p52")),
             column(6, plotlyOutput("p53"))
           ),
           fluidRow(
             column(12, 
                    tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                    h4("Tipos de violencias experimentadas por la población LGBTIQA+, por orientación sexual", style = "text-align: center; color: #00A499; font-weight: bold;"),
           fluidRow(
             column(6, plotlyOutput("p54")),
             column(6, plotlyOutput("p55"))
           ),
           fluidRow(
             column(6, plotlyOutput("p56")),
             column(6, plotlyOutput("p57"))
           ),
           fluidRow(
             column(12, 
                    tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                    h4(style = "text-align: center; color: #00A499; font-weight: bold;"),
                    fluidRow(
                      column(12, align = "center",
                             div(style = "width: 50%;", plotlyOutput("p58")))
           
         ),
         fluidRow(
           column(12, 
                  tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                  h4("Estudio Diálogos diversos. Trayectorias de estudiantes trans y de género diverso.", style = "text-align: center; color: #00A499; font-weight: bold;"),
                  fluidRow(
                    column(6, plotlyOutput("p59")),
                    column(6, plotlyOutput("p60"))
                    ),
         fluidRow(
           column(12, align = "center",
                  div(style = "width: 50%;", plotlyOutput("p61"))
                  )
                 )
                )
               )
              )
             )
            ),
         fluidRow(
           column(12,
                  tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                  h4("Trayectoria de estudiantes trans y de género diverso: elementos de desigualdad", style = "text-align: center; color: #00A499; font-weight: bold;"),
                  fluidRow(
                    column(6, plotlyOutput("p62")),
                    column(6, plotlyOutput("p63"))
              )
             )
            ),
         fluidRow(
           column(12, 
                  tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                  h4("Trayectoria de estudiantes trans y de género diverso: proceso de admisión", style = "text-align: center; color: #00A499; font-weight: bold;"),
                  fluidRow(
                    column(6, plotlyOutput("p64")),
                    column(6, plotlyOutput("p65"))
                  )
           )
          ),
         fluidRow(
           column(12, 
                  tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                  h4("Trayectoria de estudiantes trans y de género diverso: reconocimiento de la diversidad", style = "text-align: center; color: #00A499; font-weight: bold;"),
                  fluidRow(
                    column(6, plotlyOutput("p66")),
                    column(6, plotlyOutput("p67"))
                  ),
         fluidRow(
         column(12, align = "center",
                div(style = "width: 50%;", plotlyOutput("p68"))
                )
               )
              )
             )
            )
           ),
         fluidPage(
           div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                      "Notas metodológicas:"),
               tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                      "(1) Los gráficos presentados provienen de dos estudios: desde el Gráfico 46 al 58 forman parte del estudio sobre calidad de vida de la población LGBTIQA+ en la USACH del año 2023. 
                      El resto de gráficos provienen del estudio 'Trayectorias de estudiantes trans y género diverso' del año 2022."),
               tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                      "(2) La muestra obtenida del primer estudio fueron 962 personas, la cual contempla todos los estamentos de la universidad. Para el estudio de trayectorias de estudiantes trans y de género diverso, se obtuvo una muestra de 296 estudiantes.
                      Para ambos estudios contestan personas que se identifican como parte de la comunidad LGBTIQA+"),
               tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                      "(3) Se realizaron pruebas estadísticas (Alpha de Cronbach) para medir confiabilidad de la batería de indicadores del instrumento y los datos constatan que los resultados son consistentes.")
               )
              )
             ),
# Diagnóstico violencia de género----
tabPanel("Diagnóstico violencia de género",
         fluidPage(
           div(
             style = "background-color: #f0f0f0;
                      border-left: 6px solid #00A499;
                      border-right: 6px solid #00A499;
                      padding: 15px 20px;
                      margin-bottom: 20px;
                      border-radius: 8px;
                      font-size: 16px;
                      color: #333;
                      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
             tags$p("En esta sección se pueden ver distintos indicadores relativos al diagnóstico sobre violencia de género en la USACH."
             ),tags$p("Para ver datos y porcentajes en cada año del gráfico seleccionado, pase el cursor sobre el punto de interés."),
             tags$p("Además, como los gráficos son interactivos, puede activar y desactivar variables en las leyendas haciendo doble click en la variable de interés para mirarla de forma aislada."),
             tags$p("Para más detalles de los resultados correspondientes a estos estudios, revisar la página web de la",
                    tags$a("Dirección de Género, Equidad y Diversidad", href = "https://direcciondegenero.usach.cl/documentos-0",
                           target = "_blank")
             ),
           ),
          fluidRow(
            column(6, plotlyOutput("p69")),
            column(6, plotlyOutput("p70"))
          ),
          fluidRow(
            column(6, plotlyOutput("p71")),
            column(6, plotlyOutput("p72"))
          ),
          fluidRow(
            column(12, align = "center",
                   div(style = "width: 50%;", plotlyOutput("p73"))
          )
         ),
         fluidRow(
           column(12,
                  tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                  h4("Lugares y espacios donde ocurre la violencia de género", style = "text-align: center; color: #00A499; font-weight: bold;"),
                  fluidRow(
                    column(12, align = "center",
                    div(style = "width: 50%;", plotlyOutput("p74"))
                    )
                   )
                  )
                 ),
         fluidRow(
           column(12,
                  tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                  h4("Percepciones: ¿Qué tipo de violencia ocurre en la USACH y cómo se da según género?", style = "text-align: center; color: #00A499; font-weight: bold;"),
                  fluidRow(
                    column(6, plotlyOutput("p75")),
                    column(6, plotlyOutput("p76"))
                  ),
                  fluidRow(
                    column(6, plotlyOutput("p77")),
                    column(6, plotlyOutput("p78"))
                  )
                 )
                ),
         fluidRow(
           column(12,
                  tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                  h4("Consecuencias y expectativas de quienes viven violencia de género", style = "text-align: center; color: #00A499; font-weight: bold;"),
                  fluidRow(
                    column(6, plotlyOutput("p79")),
                    column(6, plotlyOutput("p80"))
                           )
                          )
                         )
                        ),
         fluidPage(
           div(style = "background-color: #f0f0f0;
              border-left: 6px solid #EA7600;
              border-right: 6px solid #EA7600;
              padding: 15px 20px;
              margin-bottom: 20px;
              border-radius: 8px;
              font-size: 14px;
              color: #333;
              box-shadow: 2px 2px 8px rgba(0,0,0,0.1);",
               tags$p(style = "font-weight: bold; font-size: 16px; margin-top: 0; margin-bottom: 10px; color: #000000;",
                      "Notas metodológicas:"),
               tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                      "(1) Los gráficos presentados provienen del diagnóstico sobre violencia de género en la USACH del año 2022."),
               tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                      "(2) La muestra obtenida, que contempló todos los estamentos de la universidad, fue de 1810 personas."),
               tags$p(style = "margin-top: 0; margin-bottom: 0;", 
                      "(3) Se realizaron pruebas estadísticas (Alpha de Cronbach) para medir confiabilidad de la batería de indicadores del instrumento y los datos constatan que los resultados son consistentes.")
               )
              )
             )
            )

                     
# Server----

server <- function(input, output, session) {
  
  observeEvent(input$go_presentacion, {
  updateTabsetPanel(session, "tabs", selected = "Presentación")
})
  observeEvent(input$go_academicos, {
    updateTabsetPanel(session, "tabs", selected = "Estamento académico y docente")
  })
  observeEvent(input$go_jce, {
    updateTabsetPanel(session, "tabs", selected = "Jornadas Completas Equivalentes (JCE)")
  })
  observeEvent(input$go_jerarquia, {
    updateTabsetPanel(session, "tabs", selected = "Jerarquía académica")
  })
  observeEvent(input$go_jerarquia_jce, {
    updateTabsetPanel(session, "tabs", selected = "Jerarquía académica JCE")
  })
  observeEvent(input$go_publicaciones, {
    updateTabsetPanel(session, "tabs", selected = "Publicaciones")
  })
  observeEvent(input$go_directivos, {
    updateTabsetPanel(session, "tabs", selected = "Puestos directivos")
  })
  observeEvent(input$go_genero, {
    updateTabsetPanel(session, "tabs", selected = "Matrícula por género")
  })
  observeEvent(input$go_area, {
    updateTabsetPanel(session, "tabs", selected = "Matrícula por área del conocimiento")
  })
  observeEvent(input$go_titulados, {
    updateTabsetPanel(session, "tabs", selected = "Titulación por género y área del conocimiento")
  })
  observeEvent(input$go_desglose_titulados, {
    updateTabsetPanel(session, "tabs", selected = "Desglose titulación por área del conocimiento")
  })
  observeEvent(input$go_cuidados_corresp, {
    updateTabsetPanel(session, "tabs", selected = "Cuidados y corresponsabilidad")
    })
  observeEvent(input$go_lgbt, {
    updateTabsetPanel(session, "tabs", selected = "LGBTIQA+")
    
  })
  observeEvent(input$go_violencia, {
    updateTabsetPanel(session, "tabs", selected = "Diagnóstico violencia de género")
    
  })
  
  # Plots----
  # Pestaña Estamento académico----
  output$p1 <- renderPlotly({ p1 })
  output$p2 <- renderPlotly({ p2 })
  output$p3 <- renderPlotly({ p3 })
  output$p4 <- renderPlotly({ p4 })
  
  # Pestaña JCE----
  output$p5 <- renderPlotly({ p5 })
  output$p6 <- renderPlotly({ p6 })
  output$p7 <- renderPlotly({ p7 })
  output$p8 <- renderPlotly({ p8 })
  
  # Pestaña Jerarquía académica----
  output$p9 <- renderPlotly({ p9 })
  output$p10 <- renderPlotly({ p10 })
  
  # Pestaña Jerarquía académica JCE----
  output$p11 <- renderPlotly({ p11 })
  output$p12 <- renderPlotly({ p12 })
  
  # Pestaña Publicaciones----
  output$p13 <- renderPlotly({ p13 })
  output$p14 <- renderPlotly({ p14 })
  output$p15 <- renderPlotly({ p15 })
  output$p16 <- renderPlotly({ p16 })
  output$p17 <- renderPlotly({ p17 })
  output$p_scielo <- renderPlotly({ p_scielo })
  
  # Pestaña Puestos directivos----
  output$p18 <- renderPlotly({ p18 })
  output$p19 <- renderPlotly({ p19 })
  output$p20 <- renderPlotly({ p20 })
  output$p21 <- renderPlotly({ p21 })
  
  # Pestaña Matrícula por género-----
  output$p22 <- renderPlotly({ p22 })
  output$p23 <- renderPlotly({ p23 })
  output$p24 <- renderPlotly({ p24 })
  
  # Pestaña Matrícula por género y área del conocimiento----
  output$p25 <- renderPlotly({ p25 })
  output$p26 <- renderPlotly({ p26 })
  output$p27 <- renderPlotly({ p27 })
  
  # Pestaña Titulación por género y área del conocimiento----
  output$p28 <- renderPlotly({ p28 })
  output$p29 <- renderPlotly({ p29 })
  
  # Pestaña Desglose titulación----
  output$p30 <- renderPlotly({ p30 })
  output$p31 <- renderPlotly({ p31 })
  output$p32 <- renderPlotly({ p32 })
  output$p33 <- renderPlotly({ p33 })
  output$p34 <- renderPlotly({ p34 })
  output$p35 <- renderPlotly({ p35 })
  output$p36 <- renderPlotly({ p36 })
  output$p37 <- renderPlotly({ p37 })
  output$p38 <- renderPlotly({ p38 })
  
  # Pestaña Cuidados y Corresponsabilidad----
  output$p39 <- renderPlotly({ p39 })
  output$p40 <- renderPlotly({ p40 })
  output$p41 <- renderPlotly({ p41 })
  output$p42 <- renderPlotly({ p42 })
  output$p43 <- renderPlotly({ p43 })
  output$p44 <- renderPlotly({ p44 })
  output$p45 <- renderPlotly({ p45 })
  
  # Pestaña LGBTIQA+----
  output$p46 <- renderPlotly({ p46 })
  output$p47 <- renderPlotly({ p47 })
  output$p48 <- renderPlotly({ p48 })
  output$p49 <- renderPlotly({ p49 })
  output$p50 <- renderPlotly({ p50 })
  output$p51 <- renderPlotly({ p51 })
  output$p52 <- renderPlotly({ p52 })
  output$p53 <- renderPlotly({ p53 })
  output$p54 <- renderPlotly({ p54 })
  output$p55 <- renderPlotly({ p55 })
  output$p56 <- renderPlotly({ p56 })
  output$p57 <- renderPlotly({ p57 })
  output$p58 <- renderPlotly({ p58 })
  output$p59 <- renderPlotly({ p59 })
  output$p60 <- renderPlotly({ p60 })
  output$p61 <- renderPlotly({ p61 })
  output$p62 <- renderPlotly({ p62 })
  output$p63 <- renderPlotly({ p63 })
  output$p64 <- renderPlotly({ p64 })
  output$p65 <- renderPlotly({ p65 })
  output$p66 <- renderPlotly({ p66 })
  output$p67 <- renderPlotly({ p67 })
  output$p68 <- renderPlotly({ p68 })
  
  # Pestaña Violencia de género----
  output$p69 <- renderPlotly({ p69 })
  output$p70 <- renderPlotly({ p70 })
  output$p71 <- renderPlotly({ p71 })
  output$p72 <- renderPlotly({ p72 })
  output$p73 <- renderPlotly({ p73 })
  output$p74 <- renderPlotly({ p74 })
  output$p75 <- renderPlotly({ p75 })
  output$p76 <- renderPlotly({ p76 })
  output$p77 <- renderPlotly({ p77 })
  output$p78 <- renderPlotly({ p78 })
  output$p79 <- renderPlotly({ p79 })
  output$p80 <- renderPlotly({ p80 })
}

shinyApp(ui, server)
