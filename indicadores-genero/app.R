# Preparación plataforma de visualización indicadores de género USACH 2018-2024
# Cargar paquetes
pacman::p_load(here, ggrepel, plotly, purrr, readxl, rio, scales, shiny, shinythemes, tidyverse)

source("graficos_plotly_app.R")

ui <- navbarPage(
  id = "tabs",
  title = div(
    style = "color: white; font-weight: bold;",
    "Panel de Indicadores de Género USACH"
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
               h1(class = "welcome-title", "Bienvenid-s al Panel de Indicadores de Género de la USACH"),
               p(class = "welcome-subtitle", "Visualización de indicadores clave de la participación de mujeres en la comunidad universitaria"),
               tags$a(href = "https://github.com/diegoesturillo/indicadores-genero-usach", target = "_blank",
                      tags$img(src = "usach-p1.png", height = "200px", style = "margin-top: 50px;",
                               style = "margin-bottom: 20px; cursor: pointer;")
               ),
               br(),
               br(),
               div(class = "button-group",
                   actionButton("go_consideraciones", "Consideraciones metodológicas", class = "button"),
                   actionButton("go_academicos", "Académicos", class = "button"),
                   actionButton("go_jerarquia", "Jerarquía académica", class = "button"),
                   actionButton("go_publicaciones", "Publicaciones", class = "button"),
                   actionButton("go_directivos", "Puestos Directivos", class = "button"),
                   actionButton("go_genero", "Matrícula por género", class = "button"),
                   actionButton("go_area", "Matrícula por área de Conocimiento", class = "button"),
                   actionButton("go_titulados", "Titulados por género y área de conocimiento", class = "button")
               )
             )
           )
  ),
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
  # Pestañas----
  tabPanel("Académicos",
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
             "En esta sección se muestra la distribución del estamento académico por género y grado académico."
           ),
           fluidPage(
             fluidRow(
               column(6, plotlyOutput("plot_acad_genero")),
               column(6, plotlyOutput("plot_acad_grado"))
             )
           )
  ),
  
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
               "En esta sección se pueden visualizar las diferencias de jerarquía académica por género, considerando los distintos niveles de la carrera académica."
             ),
             fluidRow(
               column(6, plotlyOutput("plot_jerarquias")),
               column(6, plotlyOutput("plot_jerarquias_2"))
             )
           )
  ),
  
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
               "Indicador de producción científica y académica, diferenciada por género."
             ),
             fluidRow(
               column(6, plotlyOutput("plot_pub"))
             )
           )
  ),
  
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
               "Visualización de la representación femenina en cargos directivos dentro de la universidad."
             ),
             fluidRow(
               column(6, plotlyOutput("plot_jd_ca")),
               column(6, plotlyOutput("plot_d_vd"))
             ),
             br(),
             fluidRow(
               column(12, plotlyOutput("plot_dir_jef"))
             )
           )
  ),
  
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
               "En esta sección se muestra la distribución de la matrícula estudiantil total, de 1er año y la tasa de retención por género entre los años 2018 y 2024."
             ),
             fluidRow(
               column(6, plotlyOutput("plot_mat_total")),
               column(6, plotlyOutput("plot_mat_1"))
             ),
             br(),
             fluidRow(
               column(12, plotlyOutput("plot_tr"))
             )
           )
  ),
  
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
               "En esta sección se puede comparar la matrícula estudiantil total, de 1er año y la tasa de retención por género y área del conocimiento genérica."
             ),
             fluidRow(
               column(6, plotlyOutput("plot_mat_area")),
               column(6, plotlyOutput("plot_mat_area_1"))
             ),
             br(),
             fluidRow(
               column(12, plotlyOutput("plot_tr_area"))
             )
           )
  ),
  
  tabPanel("Titulados por género y área del conocimiento",
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
               "En esta sección se pueden ver la distribución de estudiantes titulados/as por género y área del conocimiento genérica."
             ),
             fluidRow(
               column(6, plotlyOutput("plot_titulados")),
               column(6, plotlyOutput("plot_titulados_area"))
             )
           )
  ))


server <- function(input, output, session) {
  observeEvent(input$go_consideraciones, {
    updateTabsetPanel(session, "tabs", selected = "Consideraciones metodológicas")
  })
  observeEvent(input$go_academicos, {
    updateTabsetPanel(session, "tabs", selected = "Académicos")
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
    updateTabsetPanel(session, "tabs", selected = "Titulados por género y área del conocimiento")
  })
  
  # Plots
  output$plot_acad_genero <- renderPlotly({ plot_acad_genero })
  output$plot_acad_grado <- renderPlotly({ plot_acad_grado })
  output$plot_jerarquias <- renderPlotly({ plot_jerarquias })
  output$plot_jerarquias_2 <- renderPlotly({ plot_jerarquias_2 })
  output$plot_pub <- renderPlotly({ plot_pub })
  output$plot_jd_ca <- renderPlotly({ plot_jd_ca })
  output$plot_d_vd <- renderPlotly({ plot_d_vd })
  output$plot_dir_jef <- renderPlotly({ plot_dir_jef })
  output$plot_mat_total <- renderPlotly({ plot_mat_total })
  output$plot_mat_1 <- renderPlotly({ plot_mat_1 })
  output$plot_tr <- renderPlotly({ plot_tr })
  output$plot_mat_area <- renderPlotly({ plot_mat_area })
  output$plot_mat_area_1 <- renderPlotly({ plot_mat_area_1 })
  output$plot_tr_area <- renderPlotly({ plot_tr_area })
  output$plot_titulados <- renderPlotly({ plot_titulados})
  output$plot_titulados_area <- renderPlotly({ plot_titulados_area})
}

shinyApp(ui, server)
