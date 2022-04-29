library(shiny)
library(tidyverse) 
# ==============================================================================
#  App Shiny - Trabajo Final - Grupo 6 - Parte 2
# ==============================================================================

# Data CSV
# -------------------------------------------------------------------------
dfReporte <- read_csv("data.csv")
dfGrafico <- dfReporte[,c('Departamento','Provincia','Distrito','Genero','CantidadAfiliados','TotalPoblacion','PorcentajeAP')]
dfdep= c(distinct(dfGrafico, Departamento))
dfbar= dfGrafico[,c('Departamento','CantidadAfiliados')]

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Dashboard de Afiliados al SIS y población INEI - Grupo 06"),
  
  
  tabsetPanel(
    
    tabPanel("Dashboard",
             
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(inputId = "Departamentos",
                                    label = "Seleccione un Departamento",
                                    choices = c(dfdep$Departamento),
                                    selected = c(dfdep$Departamento))
               ),
               mainPanel( 
                 dataTableOutput("tdinamica"),
                 plotOutput("grafico") 
               )
             )
    ),
    
    tabPanel("Estadistica",
             
             verbatimTextOutput("resumen1"),    # Muestra cuadro de texto ("resumen") 
             
    )
  )
)


# Lógica del servidor
server <- function(input, output) {
  
  output$tdinamica <- renderDataTable(dfGrafico<- filter(dfGrafico, Departamento  %in% input$Departamentos), 
                                      options = list(pageLength = 5))
  
  output$grafico <- renderPlot({
    # generate bins based on input$bins from ui.R
    plotdf <- filter(dfGrafico, Departamento  %in% input$Departamentos)
    
    x    <- plotdf$CantidadAfiliados 
    
    ggplot(dat = plotdf,
           aes(x=CantidadAfiliados, y=TotalPoblacion, color=Departamento)) + geom_point()
    
  })
  
  
  output$resumen1 <- renderPrint({
    # Texto que irá en el cuadro
    summary(dfGrafico)   
  })
  
   
  
}


shinyApp(ui, server)