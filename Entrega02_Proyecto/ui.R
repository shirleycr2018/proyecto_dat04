#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(dygraphs)
library(leaflet)

header   <- dashboardHeaderPlus(title = "DATS04")
sidebar  <- dashboardSidebar(
             sidebarMenu(
            
                 menuItem("Centro de Datos",  tabName = "tab_dem1", icon = icon("database")),
                 menuItem("Estadisticos Resumen", tabName = "tab_dem2", icon = icon("chart-bar")),
                 menuItem("Estadistica Descriptiva", tabName = "tab_dem3", icon = icon("chart-line")),
                 menuItem("Regresion Lineal", tabName = "tab_dem4", icon = icon("chart-area")),
                 menuItem("Series de Datos", tabName = "tab_dem5", icon = icon("calendar-alt")),
                 menuItem("Predicciones", tabName = "tab_dem6", icon = icon("project-diagram")),
                 menuItem("Mapas", tabName = "tab_dem7", icon = icon("map-marked-alt"))
             )
            )
body     <- dashboardBody(
  
                mainPanel( width = 12,
                 
                    box(
                           dateRangeInput('daterange01',
                                          label = 'Fechas: yyyy-mm-dd',
                                          start = "2013-02-19", end = Sys.Date()
                           ))
               
                ),
                
                tabItems(
                    tabItem("tab_dem1",
                            fluidRow( 
                                mainPanel( width = 12,
                                    tabsetPanel( id = 'dataset',
                                                 tabPanel(title = "Estructura",
                                                          verbatimTextOutput("estructura") ),
                                        tabPanel(title = "Resumen",
                                                 verbatimTextOutput("summary") ),
                                        tabPanel(title = "Datos",
                                                 
                                                 
                                                 DT::dataTableOutput("table"))
                                  )
                                )
                              )
                            ),
                    
                    tabItem( "tab_dem2",
                             fluidRow(
                               mainPanel(width = 12,
                                  checkboxGroupInput(
                                    "opciones", 
                                    "Seleccione :",
                                    c("Prom",  "Median", "Moda",  "Std", "Var","Min","Max"),
                                   selected = c("Prom", "Median", "Moda", "Std","Var","Min","Max"),
                                   inline = TRUE
                                 )
                               )
                             ),
                             
                             fluidRow(
                               mainPanel( width = 12,
                                
                               tabsetPanel( id = 'histogramas',
                                            tabPanel( title = "Candelas",
                                                      
                                                      plotlyOutput("plotCandles")
                                            ),            
                                 tabPanel( title = "Open-Close",
                                           
                                        fluidRow(
                                                         plotlyOutput("plotcentro1")
                                                        
                                                ),
                                        fluidRow(
                                           
                                                         plotlyOutput("plotcentro5")
                                                         )
                                 ),
                                 
                                 tabPanel( title = "Low-High",
                                           fluidRow(
                                            
                                                            plotlyOutput("plotcentro2")),
                                           fluidRow(
                                           
                                                            plotlyOutput("plotcentro7"))
                                                    
                                 ),
                                 tabPanel( title = "Weighted",
                                    fluidRow(
                                 
                                           plotlyOutput("plotcentro6"))
                                 )
                                 ,
                                 tabPanel(title = "Volumenes",
                                          fluidRow(
                                    
                                          plotlyOutput("plotcentro4")),
                                          fluidRow(
                                       
                                          plotlyOutput("plotcentro3")),
                                  )
                                 
                                )
                              )
                            )
                           
                          )
                    ,
                    tabItem("tab_dem3",
                            fluidRow(
                              mainPanel( width = 12,
                                
                                box(column=6,
                                selectInput(inputId="cname1",
                                            choices=c("Open","High","Low","Close",
                                                      "Volume_BTC","Volume_Currency",
                                                      "Weighted_Price"),
                                            label="Columna:", selected="Volume_BTC")),
                                box( column=6,
                                sliderInput("slider1", label = "Clases:", min = 0, 
                                            max = 60, value = 10))
                               )
                              ),
                            fluidRow(
                              mainPanel( width = 12,
                                
                                tabsetPanel( id = 'frecuencias',
                                             
                                            tabPanel(title = "Tabla Frecuencias",
                                                      div(style = 'overflow-x: scroll', DT::dataTableOutput("table_frec"))
                                                      ),  
                                         
                                  tabPanel(title = "Frecuencias Acumuladas",
                                           fluidRow(
                                                   plotlyOutput("plotfrec1")),
                                           
                                           ),
                                  
                                  tabPanel(title="Cuantiles",
                                           fluidRow(
                                             selectInput(inputId="cuantiles1",
                                                         choices=c("Cuartil" = "4",
                                                                   "Quintil" = "5",
                                                                   "Decil"   = "10",
                                                                   "Percentil" = "100"),
                                                         label="Cuantil:", 
                                                         selected="Cuartil")
                                           ),
                                           fluidRow(
                                             
                                             column( width = 12,
                                                     fluidRow(
                                                       verbatimTextOutput("percentilvalue")
                                                     ),
                                                     fluidRow(
                                                     uiOutput("percentilsel")
                                                     ),
                                                     fluidRow(
                                                      column(width = 12,
                                                             plotlyOutput("perchistogram"))
                                                       
                                                     )
                                                    )
                                                )
                                             )
                                           )
                                        )
                                      )
                                  ),
                    
                    tabItem("tab_dem4",
                            fluidRow(
                              
                              mainPanel( width = 12,
                                tabsetPanel( id = 'regresionlineal',
                                    
                          
                                    tabPanel(title = "Correlaciones",
                                          fluidRow(
                                          
                                                    plotOutput("plotcor")
                                                    ),
                                          fluidRow(
                                           
                                                   verbatimTextOutput("cortabtext")
                                                   
                                          )),
                                    
                                    tabPanel(title = "Modelo",
                                             mainPanel( width = 12,
                                             fluidRow(
                                               column(width = 6,
                                                      verbatimTextOutput("modelotext")),
                                               fluidRow( width = 6,
                                                
                                                        verbatimTextOutput("confittext")
                                                        
                                                 ,
                                                        verbatimTextOutput("residualtext")
                                                 
                                               )
                                               
                                               
                                             ),
                                             fluidRow(
                                               column(width=12,
                                               plotlyOutput("multiregresion")
                                               )
                                             )
                                    )
                                        )
                                    )
                                 )
                              )
                            ),
                    tabItem("tab_dem5",
                            fluidRow(
                              
                              mainPanel( width = 12,
                                tabsetPanel( id = 'serietiempo',      
                                  tabPanel(title = "Serie de Tiempo Mes",
                                          
                                                  dygraphOutput("dygraphlowhigh"),
                                           
                                                  dygraphOutput("dygraphopenclose")),
                                  tabPanel(title = "Candlesticks",
                                           dygraphOutput("dygraphcandle")),
                                   
                                  tabPanel(title = "Serie de Tiempo Dias",
                                           
                                           fluidRow(dygraphOutput("dygraphdias"))
                                           
                                      )
                                  
                                  )
                              )
                          )
                    ),
                    tabItem("tab_dem6",
                            
                            tabsetPanel( id = 'predicciones',
                                         
                                         tabPanel(title ="Mod HoltWinters",
                                               
                                                  mainPanel( width = 12,
                                                             fluidRow(
                                                               mainPanel( width = 12,
                                                                          column( width = 6,
                                                                                  numericInput("proyecthw", "Periodo:", 50, min = 1, max = 365)
                                                                          ),
                                                                          column( width = 6,
                                                                                  selectInput(inputId="cname3",
                                                                                              choices=c("Open","High","Low","Close",
                                                                                                        "Volume_BTC","Volume_Currency",
                                                                                                        "Weighted_Price"),
                                                                                              label="Columna:", selected="Open")
                                                                          ) 
                                                                      )
                                                                  ),
                                                             
                                                             fluidRow(dygraphOutput("dygraphpredic")),
                                                             fluidRow(plotOutput("dygraphpredicfore")),
                                                             fluidRow(verbatimTextOutput("hw_md"))
                                                          )
                                                    
                                                  ),
                                         
                                         
                                         tabPanel(title = "Mod Arima",
                                                  mainPanel( width = 12,
                                                  fluidRow(
                                                    mainPanel( width = 12,
                                                               fluidRow(
                                                                 column( width = 2,
                                                                         numericInput("perproyeccion", 
                                                                                      "Periodo:", 60, min = 1, max = 365)
                                                                 ),
                                                                 column( width = 4,
                                                                         selectInput(inputId="cname2",
                                                                                     choices=c("Open","High","Low","Close",
                                                                                               "Volume_BTC","Volume_Currency",
                                                                                               "Weighted_Price"),
                                                                                     label="Columna:", selected="Open")
                                                                 ) ))),
                                                  
                                                  fluidRow(
                                                    mainPanel(width = 12,
                                                              
                                                              fluidRow(column( width = 12,
                                                                           dygraphOutput("dygraphpredic2"))),
                                                              fluidRow(plotOutput("arimaforecast")),
                                                              fluidRow(column( width = 12,
                                                                        verbatimTextOutput("arima_md")))
                                                              
                                                              )
                                                        )
                                                  )
                                                  
                                         )        
                               )
                            
                            
                        ),
                    tabItem("tab_dem7", 
                            
                             tabPanel("Map",
                                      leafletOutput("bbmap",height=1000))
                            )
                     
                )
            )
dbpage   <- dashboardPagePlus( 
                skin = "green",
                header,
                sidebar,
                body 
            )
# Define UI for application that draws a histogram
shinyUI(
    
    dbpage
)
