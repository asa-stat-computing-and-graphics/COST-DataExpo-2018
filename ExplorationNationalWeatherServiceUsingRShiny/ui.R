library(shiny)
library(shinythemes)
options(shiny.maxRequestSize=30*1024^2) 
tagList(
  #shinythemes::themeSelector(),
  navbarPage('JSM 2018 Weather App',
    theme = shinytheme("readable"),
    #"shinythemes",
    
    #####################################
    #XL code: for the map  
    tabPanel("Map navigation",

             sidebarPanel(
               selectInput("measure", label = "Pick a statistic",
                           choices = list("High Temperature (F)", "Low Temperature (F)", "Precipitation (in)",
                                          "Mean Humidity","Mean Visibility (mi)", "Mean Wind Speed (mph)"),
                           selected = "High Temperature (F)"),
               uiOutput("dateSelection"),
               # selectInput("Airpt", label = "Pick an Airport",
               #             choices = as.list(code_list), selected = "KBHB"),
               uiOutput("airptSelection"),
               uiOutput("foreSelection"),
               h3( 'Forecast Table'),
               tableOutput('fore'),
               width = 4

             ),
             mainPanel(

               fluidPage(
                 leafletOutput("map"),

                 h3('Historical Plots'),
                 fluidRow(column(4,

                                 plotlyOutput('max_temp_plot', width = "300px", height = "200px"),
                                 plotlyOutput('humidplot', width = "300px", height = "200px")),

                          column(4,
                                 plotlyOutput('min_temp_plot', width = "300px", height = "200px"),
                                 plotlyOutput('visplot', width = "300px", height = "200px")),

                          column(4,
                                 plotlyOutput('preciplot', width = "300px", height = "200px"),
                                 plotlyOutput('windplot', width = "300px", height = "200px")

                          ),
                          fluidRow(h5('Note: Data observed prior to the selected date were
                                      plotted unless the selected date is before 2014-07-30
                                      for which data up to the next 30 days were plotted.'))

                          )#fluidrow
                          )#fluidpage
               )#main panel




               ),#tabpanel
    
    #END of XL's code for map
    ################################3

    tabPanel("Temperature",
             sidebarPanel(
               selectInput("num", label = h4("Pick a variable to explore"), 
                           choices = list("Maximum Temperature" = "Max_TemperatureF", 
                                          "Minimum Temperature" = "Min_TemperatureF"), 
                           selected = "Max_TemperatureF"),
               selectInput("airpt", label = h4("Pick an Airport"), 
                           choices = as.list(code_list), selected = "All Airports"),
               selectInput("airpt1", label = h4("Pick another Airport"), 
                           choices = as.list(code_list), selected = "KBHB"),
               width = 3),
             mainPanel(
               tabsetPanel(
               tabPanel("Region direct comparison",
                        
                        column(5, align="left", 
                               plotlyOutput("tempLine",width  = "400px",height = "200px"),
                               plotlyOutput("tempHist", width  = "400px",height = "200px")),
                        column(5, align="right",
                               plotlyOutput("tempLine1",width  = "400px",height = "200px"),
                               plotlyOutput("tempHist1",width  = "400px",height = "200px"), offset = 1),
                        column(7, align="left",
                               plotlyOutput("tempchm1",width  = "800px",height = "500px"),
                               plotlyOutput("tempchm2",width  = "800px",height = "500px"))),
               tabPanel("Actual vs. Predicted", 
                         column(7, align = "center", 
                                plotOutput("tempPredBubble",width  = "800px",height = "400px"), 
                                plotOutput("tempPredBubble1",width  = "800px",height = "400px")) 
#                        column(5, align="left", plotOutput("tempPredBubble",width  = "400px",height = "400px")#,
#                               #plotOutput("tempPredBubble",width  = "400px",height = "400px")
#),
#                        column(4, align="right",plotOutput("tempPredBubble1",width  = "400px",height = "400px")#,
#                               plotOutput("tempPredBubble1",width  = "400px",height = "400px")
#)
                          ),
        tabPanel("Model comparison", 
         column(7, align="left",
                plotlyOutput("max_error", width = "800px", height = "600px"))
                ))
             )
    ),
    

    ################################
    ## Precipitaiton panel plots
    ## GV 7/13/2018

    tabPanel("Precipitation",
               sidebarPanel(
               #selectInput("precipCol", label = h4("Pick a color"), 
               #            choices = list("Red" = "red", "Blue" = "blue", "Orange" = "orange","Grey" = "grey"), 
               #            selected = "red"),               
               selectInput("precipAirpt", label = h4("Pick an Airport"), 
                           choices = as.list(code_list), selected = "All Airports"),
               selectInput("precipAirpt1", label = h4("Pick another Airport"), 
                           choices = as.list(code_list), selected = "KBHB"),
             width = 3),
             mainPanel(
               tabsetPanel(
               tabPanel("Region direct comparison",
                        column(5, align="left", 
                               plotlyOutput("precipScatter",width  = "400px",height = "400px"),
                               plotlyOutput("precipHist",width  = "400px",height = "400px")),
                        column(4, align="right",
                               plotlyOutput("precipScatter1",width  = "400px",height = "400px"),
                              plotlyOutput("precipHist1",width  = "400px",height = "400px"), offset = 1)
             ),
               tabPanel("Actual vs. Predicted", 
                        column(5, align="left", 
                               plotOutput("precipBox",width  = "400px",height = "400px"),
                               plotOutput("precipObsScatter",width  = "400px",height = "400px")),
                        column(4, align="right",
                               plotOutput("precipBox1",width  = "400px",height = "400px"),
                              plotOutput("precipObsScatter1",width  = "400px",height = "400px"), offset = 1)
                        ),
             tabPanel("Actual vs. Update Predicted", 
                      column(5, align="left", 
                             plotOutput("precipBoxn",width  = "400px",height = "400px"),
                             plotOutput("precipObsScattern",width  = "400px",height = "400px")),
                      column(4, align="right",
                             plotOutput("precipBox1n",width  = "400px",height = "400px"),
                             plotOutput("precipObsScatter1n",width  = "400px",height = "400px"), offset = 1)
             )## END tabPanel
               )## END tabsetPanel
             )## END mainPanel
    ) ##END tabPanel


    ## END Precipitaiton panel plots
    ## GV 7/13/2018
    ################################


 
  )
)
