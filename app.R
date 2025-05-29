# Preparación plataforma de visualización indicadores de género USACH 2018-2024
# Cargar paquetes
pacman::p_load(here, ggrepel, markdown, plotly, RColorBrewer, readxl, scales, shiny, shinythemes, tidyverse)

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
               p(class = "welcome-subtitle", "Visualización de indicadores clave de la participación de mujeres en la comunidad universitaria"),
               tags$a(href = "https://github.com/diegoesturillo/indicadores-usach", target = "_blank",
                      tags$img(src = "usach-p1.png", height = "200px", style = "margin-top: 50px;",
                               style = "margin-bottom: 20px; cursor: pointer;")
               ),
               br(),
               br(),
               div(class = "button-group",
                   actionButton("go_presentacion", "Presentación", class = "button"),
                   actionButton("go_consideraciones", "Consideraciones metodológicas", class = "button"),
                   actionButton("go_academicos", "Estamento académico", class = "button"),
                   actionButton("go_jerarquia", "Jerarquía académica", class = "button"),
                   actionButton("go_publicaciones", "Publicaciones", class = "button"),
                   actionButton("go_directivos", "Puestos Directivos", class = "button"),
                   actionButton("go_genero", "Matrícula por género", class = "button"),
                   actionButton("go_area", "Matrícula por área del conocimiento", class = "button"),
                   actionButton("go_titulados", "Titulación por género y área del conocimiento", class = "button"),
                   actionButton("go_desglose_titulados", "Desglose titulación por área del conocimiento", class = "button"),
                   actionButton("go_cuidados_corresp", "Cuidados y corresponsabilidad", class = "button")
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
  # Consideraciones metodológicas----
  tabPanel("Consideraciones metodológicas",
           fluidPage(
             div(style = "background-color: #ffffff; padding: 30px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); color: #212529;",
                 
                 h2("Consideraciones metodológicas", 
                    style = "color: #00A499; font-weight: bold;"),
                 
                 tags$hr(style = "border-top: 2px solid #00A499;"),
                 
                 p("Esta plataforma fue desarrollada para ofrecer una visualización clara y accesible de los indicadores de género en la comunidad universitaria USACH. 
                   A continuación, se presentan algunas directrices y criterios metodológicos que orientaron la construcción de los gráficos incluidos:", 
                   style = "font-size: 17px; line-height: 1.6;"),
                 
                 tags$ul(style = "font-size: 17px; padding-left: 20px; line-height: 1.6;",
                         tags$li("La información aquí presentada corresponde al período 2018–2024. Sin embargo, para algunos años no se dispone de información completa."),
                         tags$li("Los datos para construir los gráficos fueron recolectados y procesados desde las distintas versiones de la Encuesta Mujeres en la Academia (EMA). 
                                 A su vez, se contrastaron con datos oficiales del Servicio de Información de Educación Superior (SIES) del Ministerio de Educación (MINEDUC)."),
                         tags$li("No se graficaron datos sobre el puesto de rectoría y prorrectoría, ya que implican un (1) solo puesto."),
                         tags$li("La información del gráfico de publicaciones académicas para el año 2024 considera el reporte al mes de agosto, por lo que no representa el total final."),
                         tags$li("Los gráficos fueron elaborados utilizando herramientas interactivas, permitiendo mayor exploración por parte de la persona usuaria."),
                         tags$li("Toda la información ha sido anonimizada y se presenta con fines exclusivamente analíticos.")
                 ),
                 
                 br(),
                 div(style = "background-color: #EA7600; color: white; padding: 15px; border-radius: 5px; font-size: 16px;",
                     strong("Nota:"),
                     span(" Esta plataforma se actualizará periódicamente. 
                     Para mayor detalle sobre la metodología o los datos, contactar al Observatorio de Género y Diversidad, unidad anclada a la Dirección de Género, Equidad y Diversidad de la USACH 
                          (direccion.genero@usach.cl).")
                 )
             )
           )
  ),
  # Estamento académico----
  tabPanel("Estamento académico",
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
           "En esta sección se muestra una caracterización del estamento académico por género, grado académico y edad promedio, tanto en la USACH como en el conglomerado AMA para el periodo 2018-2024."
         ),
         fluidPage(
           fluidRow(
             column(6, plotlyOutput("p1")),
             column(6, plotlyOutput("p2")),
           fluidRow(
             column(6, plotlyOutput("p3")),
             column(6, plotlyOutput("p4")))
))),
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
               "En esta sección se pueden visualizar las diferencias de jerarquía académica por género, considerando los distintos niveles de la carrera académica para el periodo 2018-2024."
             ),
             fluidRow(
               column(6, plotlyOutput("p5")),
               column(6, plotlyOutput("p6"))
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
               "Indicador de producción científica y académica en la USACH, diferenciada por género y área del conocimiento para el periodo 2018-2024."
             ),
             fluidRow(
               column(12, plotlyOutput("p7")),
               fluidRow(column(6, plotlyOutput("p8")),
                        column(6, plotlyOutput("p9"))),
               fluidRow(column(6, plotlyOutput("p10")),
                        column(6, plotlyOutput("p11"))),
               fluidRow(
                 column(12, align = "center",
                         div(style = "width: 50%;", plotlyOutput("p12")))
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
               "Visualización de la representación femenina en cargos directivos dentro de la universidad para el periodo 2018-2024."
             ),
             fluidRow(
               column(6, plotlyOutput("p13")),
               column(6, plotlyOutput("p14")),
               
             ),
             br(),
             fluidRow(
               column(6, plotlyOutput("p15")),
               column(6, plotlyOutput("p16"))
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
               "En esta sección se muestra la distribución de la matrícula estudiantil total, de 1er año y la tasa de retención de 1er año por género para el periodo 2018-2024."
             ),
             fluidRow(
               column(6, plotlyOutput("p17")),
               column(6, plotlyOutput("p18"))
             ),
             br(),
             fluidRow(
               column(12, plotlyOutput("p19"))
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
               "En esta sección se puede comparar la matrícula estudiantil total, de 1er año y la tasa de retención de 1er año por género y área del conocimiento genérica para el periodo 2018-2024."
             ),
             fluidRow(
               column(6, plotlyOutput("p20")),
               column(6, plotlyOutput("p21"))
             ),
             br(),
             fluidRow(
               column(12, plotlyOutput("p22"))
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
               "En esta sección se puede ver la distribución de la titulación por género y área del conocimiento genérica en el periodo 2018-2023."
             ),
             fluidRow(
               column(6, plotlyOutput("p23")),
               column(6, plotlyOutput("p24"))
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
        "En esta sección se muestra el desglose de la titulación por género y área de conocimiento genérica en el periodo 2018-2023."
      ),
    fluidRow(
      column(6, plotlyOutput("p25")),
      column(6, plotlyOutput("p26"))
    ),
    fluidRow(
      column(6, plotlyOutput("p27")),
      column(6, plotlyOutput("p28"))
    ),
    fluidRow(
      column(6, plotlyOutput("p29")),
      column(6, plotlyOutput("p30"))
    ),
    fluidRow(
      column(6, plotlyOutput("p31")),
      column(6, plotlyOutput("p32"))
    ),
    fluidRow(
      column(12, plotlyOutput("p33"))
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
               "En esta sección se puede ver la distribución de cuidados de niñas, niños y adolescentes (NNA) y personas adultas con dependencia funcional en el estamento académico por género."
             ),
             fluidRow(
               column(12, plotlyOutput("p34")),
               # Línea separadora con título
               fluidRow(
                 column(12, 
                        tags$hr(style = "border-top: 2px solid #00A499; margin: 30px 0;"),
                        h4("Distribución de horas y días de cuidado", style = "text-align: center; color: #00A499; font-weight: bold;")
                 )
               ),
               
               # Segundo y tercer gráfico (abajo)
               fluidRow(
                 column(6, plotlyOutput("p35")),
                 column(6, plotlyOutput("p36"))
               )
             )
           ))
)

# Server----

server <- function(input, output, session) {
  
  observeEvent(input$go_presentacion, {
  updateTabsetPanel(session, "tabs", selected = "Presentación")
})
  observeEvent(input$go_consideraciones, {
    updateTabsetPanel(session, "tabs", selected = "Consideraciones metodológicas")
  })
  observeEvent(input$go_academicos, {
    updateTabsetPanel(session, "tabs", selected = "Estamento académico")
  })
  observeEvent(input$go_jerarquia, {
    updateTabsetPanel(session, "tabs", selected = "Jerarquía académica")
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
  
  # Plots----
  output$p1 <- renderPlotly({ p1 })
  output$p2 <- renderPlotly({ p2 })
  output$p3 <- renderPlotly({ p3 })
  output$p4 <- renderPlotly({ p4 })
  output$p5 <- renderPlotly({ p5 })
  output$p6 <- renderPlotly({ p6 })
  output$p7<- renderPlotly({ p7 })
  output$p8 <- renderPlotly({ p8 })
  output$p9 <- renderPlotly({ p9 })
  output$p10 <- renderPlotly({ p10 })
  output$p11 <- renderPlotly({ p11 })
  output$p12 <- renderPlotly({ p12 })
  output$p13 <- renderPlotly({ p13 })
  output$p14 <- renderPlotly({ p14 })
  output$p15 <- renderPlotly({ p15 })
  output$p16 <- renderPlotly({ p16 })
  output$p17 <- renderPlotly({ p17 })
  output$p18 <- renderPlotly({ p18 })
  output$p19 <- renderPlotly({ p19 })
  output$p20 <- renderPlotly({ p20 })
  output$p21 <- renderPlotly({ p21 })
  output$p22 <- renderPlotly({ p22 })
  output$p23 <- renderPlotly({ p23 })
  output$p24 <- renderPlotly({ p24 })
  output$p25 <- renderPlotly({ p25 })
  output$p26 <- renderPlotly({ p26 })
  output$p27 <- renderPlotly({ p27 })
  output$p28 <- renderPlotly({ p28 })
  output$p29 <- renderPlotly({ p29 })
  output$p30 <- renderPlotly({ p30 })
  output$p31 <- renderPlotly({ p31 })
  output$p32 <- renderPlotly({ p32 })
  output$p33 <- renderPlotly({ p33 })
  output$p34 <- renderPlotly({ p34 })
  output$p35 <- renderPlotly({ p35 })
  output$p36 <- renderPlotly({ p36 })
}

shinyApp(ui, server)
