#########################################################################################################################
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::: Riparian Models x Shiny Project ::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# Version: 0.10
# Status: In Development
# Push Date: 11/14/23
# Push Author: Colton Shaw
#########################################################################################################################







#########################################################################################################################
# Style Guide: 
# 1) Comments such as those in this box structure exist purely for documentation, therefore, requiring a different style 
#    of writing and formatting.
# 2) Any other comments outside of these boxes will follow the same rules:
#      1. Comments with a space between itself and the # denote a piece of code that has been commented out
#      2. Comments without a space between itself and the # denote a comment on either the code below it or the line to
#         the left of it
#########################################################################################################################







#########################################################################################################################
# This section identifies and downloads libraries used throughout the program
#########################################################################################################################


#Load R package outside for the app to run it only once when the user enters the app
library(shiny)
library(stringr)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
#library(dplyr)
library(DT)
library(vroom)
# library(reactlog)
library(openxlsx)
library(rhandsontable)
library(HYDROCAL)
library(ecorest)


#########################################################################################################################
# This section controlls all HTML outputs. These are all static elements.
#########################################################################################################################


ui <-dashboardPage(   #open user-interface
  
  #prints the title for the webpage
  # dashboardHeader(title = "Riparian Models for Assessing Ecological Impacts and Benefits"),
  dashboardHeader(title = "WARM"),
  
  #########################################################################################################################
  # Handles elements of the sidebar
  # Acts as a general directory for HTML formatting
  #########################################################################################################################
  
  dashboardSidebar(  #sidebar layout
    sidebarMenu(  #each menu tab
      menuItem("Home Page", icon=icon("star"), startExpanded=TRUE, #menu tab
               menuSubItem("Start", tabName="str"), #menu subtab
               menuSubItem("About",tabName="abt") #menu subtab
      ),
      menuItem("Model Comparison", icon=icon("book"), startExpanded=TRUE, #menu tab
               menuSubItem("Criteria", tabName="Broad"), #menu subtab
               menuSubItem("Keyword", tabName="Keyword") #menu subtab
      ),
      menuItem("Calculators", icon=icon("calculator"), startExpanded=TRUE, #menu tab
               menuSubItem("Bosque Rio Grande", tabName="Bosque"), #menu subtab
               menuSubItem("Chatfield", tabName="Chatfield"), #menu subtab
               menuSubItem("Cottonwood Missouri", tabName="Cottonwood"), #menu subtab
               menuSubItem("Lower Willamette", tabName="LowerWill"), #menu subtab
               menuSubItem("Modified Mink", tabName="Mink"), #menu subtab
               menuSubItem("Resaca", tabName="Resaca"), #menu subtab
               menuSubItem("SMURF", tabName="SMURF"), #menu subtab
               menuSubItem("Skokomish", tabName="Skokomish"), #menu subtab
               menuSubItem("Upper Mississippi", tabName="UpperMiss") #menu subtab
      )
    ),
    textOutput("res") #communicates with server to render the menu
  ),
  
  #########################################################################################################################
  # Handles elements within each sidebar 
  # Defines the contents within each tab
  # The framework for the inputs and outputs are given
  #########################################################################################################################
  
  dashboardBody( #main body, describes whats inside each of the tabs above
    tabItems(
      
      #########################################################################################################################
      # Contains ReadMe for ecorest webapp
      ### Currrently under development...
      # Prerequisite: None
      # Box 1. Prints ecorest package website to screen
      #########################################################################################################################
      
      tabItem(tabName="str", # tab that links to documentation
              title="Start", #section to choose model from bluebook
              fluidPage(
                tags$iframe(src = './readme.html',
                            width = '100%', height = '800px',
                            frameborder = 0, scrolling = 'auto'
                )
              )
      ),
      
      #########################################################################################################################
      # Prints out documentation
      ### Currrently under development...
      # Prerequisite: None
      # Box 1. Prints ecorest package website to screen
      # Use this link to style https://stackoverflow.com/questions/24049159/change-the-color-and-font-of-text-in-shiny-app
      #########################################################################################################################
      
      tabItem(tabName="abt", # tab that links to documentation
              title = "About",
              # fluidPage(
              #   tags$iframe(src = './CRAN_ecorest.html',
              #               width = '100%', height = '800px',
              #               frameborder = 0, scrolling = 'auto'
              #   )
              # )
      ),
      
      # # #########################################################################################################################
      # # # Allows for downloading of troubleshooting
      # # ### Currrently under development...
      # # # Prerequisite: None
      # # # Box 1. Troubleshooting download
      # # #########################################################################################################################
      # 
      # tabItem(tabName="mnl", #tab that shows the user manual
      #         fluidPage(
      #           tags$iframe(src = "./User_Reference_Guide.html", 
      #                       width = '100%', height = '800px', 
      #                       frameborder = 0, scrolling = 'auto'
      #           )
      #         )
      # ),
      
      #########################################################################################################################
      # Handles elements of bb model and user model 
      # Box 1. BB model input
      # Box 2. User model
      #########################################################################################################################
      
      tabItem(tabName="Broad", #tab to choose model
              box(
                selectInput("region","Region",c("None","Varying","Southwest","Southeast","Northwest","Northeast","Midwest","Mid-Atlantic","South Central")),
                selectInput("model_type","Model Type",c("None","Analytical","Conceptual","Index","Spatial")),
                # checkboxGroupInput("instream_processes","Instream Processes",c("None","Physical Characteristics","Stream Condition","Stream Hydrologic Processes","Adjacent Land Use","Climate and Weather")),
                checkboxGroupInput("instream_processes","Instream Processes",c("None","Adjacent Land Use","Climate and Weather")),
                checkboxGroupInput("riparian_zone_processes","Riparian Zone Processes",c("None","Bank Characteristics","Habitat Connectivity","Stream Habitat","Canopy/Groud Cover","Native/Invasive Species","Vegetation Composition","Species Richness",
                                                         "Riparian Functions","Floodplain Functions","Climate and Weather"))
              ),
              box(
                title="Models Ranked By User Input",
                solidHeader = T,
                status = "primary",
                helpText("Ranked in ascending order"),
                rHandsontableOutput("ordered_models")
              )
      ),
      
      #########################################################################################################################
      # Handles elements for viewing the chosen model
      # Prerequisite: Model input
      # Box 1. Metadata Table
      # Box 2. HSIplotter
      #########################################################################################################################
      
      tabItem(tabName="Keyword", #tab to look at the chosen model
              box(
                title="Keyword Search and Choose Model",
                solidHeader = T,
                status = "primary",
                uiOutput("keyword_search")
              ),
              box(
                title="Models Ranked By User Input",
                solidHeader = T,
                status = "primary",
                helpText("Ranked in ascending order"),
                rHandsontableOutput("keyword_models")
              )
      ),
      
      
      #########################################################################################################################
      # Handles elements for viewing the chosen model
      # Prerequisite: Model input
      #########################################################################################################################
      
      tabItem(tabName="Bosque", #tab to look at the chosen model
              title="Bosque",
              solidHeader = T,
              status = "primary",
              rHandsontableOutput("Bosque")
      ),
      
      
      #########################################################################################################################
      # Handles elements for viewing the chosen model
      # Prerequisite: Model input
      #########################################################################################################################
      
      tabItem(tabName="Chatfield", #tab to look at the chosen model
              title="Chatfield",
              solidHeader = T,
              status = "primary",
              rHandsontableOutput("Chatfield")
      ),
      
      
      #########################################################################################################################
      # Handles elements for viewing the chosen model
      # Prerequisite: Model input
      #########################################################################################################################
      
      tabItem(tabName="Cottonwood", #tab to look at the chosen model
              title="Cottonwood",
              solidHeader = T,
              status = "primary",
              rHandsontableOutput("Cottonwood")
      ),
      
      
      #########################################################################################################################
      # Handles elements for viewing the chosen model
      # Prerequisite: Model input
      #########################################################################################################################
      
      tabItem(tabName="LowerWill", #tab to look at the chosen model
              title="Lower Willamette",
              solidHeader = T,
              status = "primary",
              rHandsontableOutput("LowerWill_input"),
              helpText("Western Pond Turtle"),
              helpText("Below are the attribute descriptions and HSI calculations for the Western Pond Turtle HEP model:"),
              helpText("V1 = Percent area with water depth preferred by adults (0-100%)"),
              helpText("V2 = Percent cover along water’s edge (0-100%)"),
              helpText("V3 = Water temperature during low flows (5-30C) "),
              helpText("V4 = Percent area with water depth less than 0.3 meters (0-100%)"),
              helpText("V5 = Availability of suitable nesting sites (None-Abundant (qualitative))"),
              rHandsontableOutput("LowerWill")
      ),
      
      
      #########################################################################################################################
      # Handles elements for viewing the chosen model
      # Prerequisite: Model input
      #########################################################################################################################
      
      tabItem(tabName="Mink", #tab to look at the chosen model
              # title="Skokomish",
              # solidHeader = T,
              # status = "primary",
              # box(
              title="Modified Mink",
              solidHeader = T,
              status = "primary",
              # checkboxGroupInput("grain_choice","Grain Roughness Calculators",c("Jarrett (1984)" = "Jarrett (1984)","Limerinos (1970)" = "Limerinos (1970)","Bathurst (1985)" = "Bathurst (1985)","Strickler (1923)" = "Strickler (1923)",
              #                                                                   "Wong and Parker (2002)" = "Wong and Parker (2002)","Maynord (1991)" = "Maynord (1991)")),
              # rHandsontableOutput("Cottonwood_input"),
              rHandsontableOutput("Mink_input"),
              rHandsontableOutput("Mink_output")
              # )
              # box(
              #   fileInput("Resaca_inputs", "Upload user inputs in ecorest format (.csv)", #user uploads csv file and is put into input$file1
              #             multiple = FALSE,
              #             accept =   ".csv"),
              #   rHandsontableOutput("Resaca_multi")
              # )
      ),
      
      
      #########################################################################################################################
      # Handles elements for viewing the chosen model
      # Prerequisite: Model input
      # Box 1. Metadata Table
      # Box 2. HSIplotter
      #########################################################################################################################
      
      tabItem(tabName="UpperMiss", #tab to look at the chosen model
              # box(
                title="Upper Mississippi",
                solidHeader = T,
                status = "primary",
                # checkboxGroupInput("grain_choice","Grain Roughness Calculators",c("Jarrett (1984)" = "Jarrett (1984)","Limerinos (1970)" = "Limerinos (1970)","Bathurst (1985)" = "Bathurst (1985)","Strickler (1923)" = "Strickler (1923)",
                #                                                                   "Wong and Parker (2002)" = "Wong and Parker (2002)","Maynord (1991)" = "Maynord (1991)")),
                rHandsontableOutput("UpperMiss_single")
              # )
              # box(
              #   fileInput("UpperMiss_structural_inputs", "Upload user inputs for the structural diversity module in ecorest format (.csv)", #user uploads csv file and is put into input$file1
              #             multiple = FALSE,
              #             accept =   ".csv"),
              #   fileInput("UpperMiss_HSI_inputs", "Upload user inputs for the UMRR forestry module in ecorest format (.csv)", #user uploads csv file and is put into input$file1
              #             multiple = FALSE,
              #             accept =   ".csv"),
              #   rHandsontableOutput("UpperMiss_multi")
              # )
              # br(),br(),
              # helpText("Brownlie (1981)"),
              # rHandsontableOutput("brownlie_input"),
              # br(),br(),
              # helpText("Engelund (1967)"),
              # rHandsontableOutput("engelund_input"),
              # br(),br(),
              # helpText("van Rijn (1984)"),
              # rHandsontableOutput("vanrijn_input")
              # uiOutput("grain")
              # wellPanel(
              #   rHandsontableOutput("autofill_inputs"),
              #   submitButton("If autofill is desired, please place values in boxes above and press this button.  
              #                 (Note: Autofill is not required and may be overidden manually.)")
              # )
              # radioButtons("direct_button", "If this calculation is desired to appear in the final output, please place values in boxes above and press this button.", 
              #              c("Not Include", "Include"))
              # ),
              # box(
              #   title="Results",
              #   solidHeader = T,
              #   status = "primary",
              #   # rHandsontableOutput("brownlie"),
              #   # rHandsontableOutput("engelund"),
              #   # rHandsontableOutput("vanrijn")
              # )
      ),
      
      #########################################################################################################################
      # Handles elements for viewing the chosen model
      # Prerequisite: Model input
      # Box 1. Metadata Table
      # Box 2. HSIplotter
      #########################################################################################################################
      
      tabItem(tabName="Resaca", #tab to look at the chosen model
              # title="Resaca",
              # solidHeader = T,
              # status = "primary",
              # box(
                title="Resaca",
                solidHeader = T,
                status = "primary",
                # checkboxGroupInput("grain_choice","Grain Roughness Calculators",c("Jarrett (1984)" = "Jarrett (1984)","Limerinos (1970)" = "Limerinos (1970)","Bathurst (1985)" = "Bathurst (1985)","Strickler (1923)" = "Strickler (1923)",
                #                                                                   "Wong and Parker (2002)" = "Wong and Parker (2002)","Maynord (1991)" = "Maynord (1991)")),
                rHandsontableOutput("Resaca_single_metric"),
                rHandsontableOutput("Resaca_single_species")
              # ),
              # box(
              #   fileInput("Resaca_inputs", "Upload user inputs in ecorest format (.csv)", #user uploads csv file and is put into input$file1
              #             multiple = FALSE,
              #             accept =   ".csv"),
              #   rHandsontableOutput("Resaca_multi")
              # )
      ),
      
      #########################################################################################################################
      # Handles elements for viewing the chosen model
      # Prerequisite: Model input
      #########################################################################################################################
      
      tabItem(tabName="Skokomish", #tab to look at the chosen model
                title="Skokomish",
                solidHeader = T,
                status = "primary",
              rHandsontableOutput("Skokomish_input"),  
              rHandsontableOutput("Skokomish")
      ),
      
      #########################################################################################################################
      # Handles elements for viewing the chosen model
      # Prerequisite: Model input
      # Box 1. Metadata Table
      # Box 2. HSIplotter
      #########################################################################################################################
      
      tabItem(tabName="SMURF", #tab to look at the chosen model
              title="SMURF",
              solidHeader = T,
              status = "primary",
              h1("Simple Model for Urban Riparian Function (SMURF)",align='center',style="background-color:DarkSeaGreen",color="black"), 
              style="background-color:LightGrey",
              #########################################################################################################################
              # Contains elements of the left-hand side of the page
              # HTMl elements for user inputs
              #########################################################################################################################
              fluidRow(
                column(12,
                       column(3,
                              align='center',
                              uiOutput(outputId="FileText1"),
                              fileInput("instream_inputs", "Upload user inputs for the instream module in ecorest format (.csv)", #user uploads csv file and is put into input$file1
                                        multiple = FALSE,
                                        accept =   ".csv"),
                              fileInput("fauna_inputs", "Upload user inputs for the  fauna module in ecorest format (.csv)", #user uploads csv file and is put into input$file1
                                        multiple = FALSE,
                                        accept =   ".csv"),
                              fileInput("corridor_inputs", "Upload user inputs for the corridor module in ecorest format (.csv)", #user uploads csv file and is put into input$file1
                                        multiple = FALSE,
                                        accept =   ".csv"),
                              fileInput("area_inputs", "Upload user inputs for area in ecorest format (.csv)", #user uploads csv file and is put into input$file1
                                        multiple = FALSE,
                                        accept =   ".csv")
                       ),
                       
                       column(3,
                              align='center',
                              uiOutput(outputId="InstreamText1"),  
                              numericInput("hyd.att",label="Hydrologic Attentuation (0-10):",NA,min=0,max=10),
                              numericInput("stripwidth.ft",label="Mean Buffer Width (0-1000):",NA,min=0,max=1000),
                              numericInput("flowpath.score",label="Flowpath Metric (0-20):",NA,min=0,max=20),
                              numericInput("shading.ratio",label="shading.ratio:",NA),
                              numericInput("cancov.score",label="Stream Canopy Cover (0-20):",NA,min=0,max=20),
                              numericInput("canstr.score",label="Multi-story Canopy Structure (0-20):",NA,min=0,max=20),
                              numericInput("carbret.score",label="Organic Matter Retention: (0-20)",NA,min=0,max=20),
                       ),
                       
                       column(3, 
                              align='center',
                              uiOutput(outputId="FaunaText1"),
                              numericInput("canstr.score.fauna",label="Multi-story Canopy Structure (0-20):",NA,min=0,max=20),
                              numericInput("deadfall.score",label="Deadfall Density (0-20):",NA,min=0,max=20),
                              numericInput("snag.score",label="Snag Density (0-20):",NA,min=0,max=20),
                              numericInput("batcan.score",label="batcan.score (0-20):",NA,min=0,max=20),
                              numericInput("embed.score",label="Embeddedness (0-20):",NA,min=0,max=20),
                              numericInput("detritus.score",label="Detrital Ground Cover (0-20):",NA,min=0,max=20),
                              numericInput("herb.score",label="Herbaceous Vegetation Cover (0-20):",NA,min=0,max=20),
                              numericInput("inv.veg.score",label="Invasive vegetation Dominance (0-20):",NA,min=0,max=20),
                       ),
                       
                       column(3, 
                              align='center',
                              uiOutput(outputId="CorridorText1"),
                              numericInput("buffer.dev.Score",label="Buffer Development (0-20):",NA,min=0,max=20),
                              numericInput("edge.density.perft",label="Edge Density (ft/ft^2):",NA),
                              numericInput("corridorwidth.ft",label="Corridor width (ft):",NA),
                              numericInput("corridormin.ft",label="Corridor Minimum Width (ft):",NA),
                              
                              uiOutput(outputId="AreaText1"),
                              numericInput("area.input",label="Site Area:",NA),
                       ),
                ),
              ),
              
                
                #########################################################################################################################
                # Contains elements of the right-hand side of the page
                # HTMl elements for user inputs
                #########################################################################################################################
                fluidRow(
                  column(6,
                         style="background-color:GoldenRod",
                         align='center',
                         uiOutput(outputId="OutputsText2"),
                         dataTableOutput("SMURF_multi")
                  ),
                  column(6,
                         style="background-color:GoldenRod",
                         align='center',
                         uiOutput(outputId="OutputsText1"),
                         dataTableOutput("SMURF_single")
                  )
                )
      )
    )
  )
)


#########################################################################################################################
# This section controlls functions linked to the inputs, outputs,and more
# The server side is handled here, were reactive elements are triggered
#########################################################################################################################

server <- function(input, output, session) { #this function constantly refreshes 
  output$res<-renderMenu({}) #calls the HTMl above and renders it
  
  values <- reactiveValues() #stores values reactively
  
  
  #########################################################################################################################
  # Function: observe
  # Output: checks for on the fly operations
  #########################################################################################################################

  observe({
    if(!is.null(input$Bosque)){
      Bosque<- hot_to_r(input$Bosque)
      if(!is.na(Bosque[1,1])){
        if(Bosque[1,1]==6){
          if(!is.na(Bosque[2,1]) && !is.na(Bosque[3,1]) && 
             !is.na(Bosque[4,1]) && !is.na(Bosque[5,1]) &&
             !is.na(Bosque[7,1]) && 
             !is.na(Bosque[16,1]) && !is.na(Bosque[17,1]) && 
             !is.na(Bosque[18,1]) && !is.na(Bosque[19,1]) && 
             !is.na(Bosque[20,1]) && !is.na(Bosque[21,1]) && 
             !is.na(Bosque[22,1]) && !is.na(Bosque[23,1]) && 
             !is.na(Bosque[24,1]) && !is.na(Bosque[25,1])){
            HYDRO <- ((Bosque[4,1]+Bosque[5,1])**0.5+Bosque[2,1]+Bosque[3,1])/3
            BIOTA <- ((Bosque[7,1]*Bosque[22,1])+(Bosque[16,1]*Bosque[19,1])+(Bosque[17,1]*Bosque[20,1])+(Bosque[18,1]*Bosque[21,1]))/4
            SPATIAL <- (Bosque[23,1]+Bosque[24,1]+Bosque[25,1])/3
            Bosque[26,1] <- (HYDRO+BIOTA+SPATIAL)/3
          }
        }else if(Bosque[1,1]==1 || Bosque[1,1]==2 || Bosque[1,1]==3 || Bosque[1,1]==4 || Bosque[1,1]==5){
          if(!is.na(Bosque[2,1]) && !is.na(Bosque[3,1]) && 
             !is.na(Bosque[4,1]) && !is.na(Bosque[5,1]) &&
             !is.na(Bosque[6,1]) && !is.na(Bosque[7,1]) && 
             !is.na(Bosque[8,1]) && !is.na(Bosque[9,1]) && 
             !is.na(Bosque[10,1]) && !is.na(Bosque[11,1]) && 
             !is.na(Bosque[12,1]) && !is.na(Bosque[13,1]) && 
             !is.na(Bosque[14,1]) && !is.na(Bosque[15,1]) && 
             !is.na(Bosque[23,1]) &&
             !is.na(Bosque[24,1]) && !is.na(Bosque[25,1])){
            HYDRO <- ((Bosque[4,1]+Bosque[5,1])**0.5+Bosque[2,1]+Bosque[3,1])/3
            BIOTA <- (3*(((((Bosque[6,1]*Bosque[9,1]*Bosque[10,1])**0.5+Bosque[7,1])/2)*Bosque[12,1])**0.5)+(Bosque[8,1]*Bosque[11,1])+Bosque[15,1]+(Bosque[13,1]*Bosque[14,1])**0.5)/6
            SPATIAL <- (Bosque[23,1]+Bosque[24,1]+Bosque[25,1])/3
            Bosque[26,1] <- (HYDRO+BIOTA+SPATIAL)/3
          }
        }
        else{
          Bosque[26,1] <- 0
        }
      }
    }
    else{
      Bosque <- data.frame(Values=rep(NA_real_,26))
    }
    values[["Bosque"]] <- Bosque
    
    
    if(!is.null(input$Cottonwood)){
      Cottonwood <- hot_to_r(input$Cottonwood)
      if(!is.na(Cottonwood[1,1])){
        if(Cottonwood[1,1]>25){
          if(!is.na(Cottonwood[2,1]) && !is.na(Cottonwood[3,1]) && 
             !is.na(Cottonwood[4,1]) && !is.na(Cottonwood[5,1]) && 
             !is.na(Cottonwood[6,1]) && !is.na(Cottonwood[9,1]) && 
             !is.na(Cottonwood[10,1]) && !is.na(Cottonwood[11,1]) && 
             !is.na(Cottonwood[12,1]) && !is.na(Cottonwood[13,1]) && 
             !is.na(Cottonwood[14,1])){
            HYDRO <- (Cottonwood[2,1]+Cottonwood[3,1])/2
            BIOTA <- (Cottonwood[4,1]+Cottonwood[5,1]+Cottonwood[6,1])/3
            SPATIAL <- ((((Cottonwood[10,1]+Cottonwood[11,1])/2)*Cottonwood[12,1])+Cottonwood[9,1]+Cottonwood[13,1]+Cottonwood[14,1])/4
            Cottonwood[15,1] <- ((3*HYDRO)+BIOTA+SPATIAL)/5
          }
        }else if(Cottonwood[1,1]>=2 && Cottonwood[1,1]<=25){
          if(!is.na(Cottonwood[2,1]) && !is.na(Cottonwood[4,1]) && 
             !is.na(Cottonwood[5,1]) && !is.na(Cottonwood[6,1]) && 
             !is.na(Cottonwood[7,1]) && !is.na(Cottonwood[8,1]) && 
             !is.na(Cottonwood[9,1]) && 
             !is.na(Cottonwood[10,1]) && !is.na(Cottonwood[11,1]) && 
             !is.na(Cottonwood[12,1]) && !is.na(Cottonwood[13,1]) && 
             !is.na(Cottonwood[14,1])){
            HYDRO <- Cottonwood[2,1]
            BIOTA <- ((Cottonwood[4,1]+Cottonwood[5,1]+Cottonwood[6,1])/3+(Cottonwood[7,1]+Cottonwood[8,1])/2)/2
            SPATIAL <- ((((Cottonwood[10,1]+Cottonwood[11,1])/2)*Cottonwood[12,1])+Cottonwood[9,1]+Cottonwood[13,1]+Cottonwood[14,1])/4
            Cottonwood[15,1] <- ((3*HYDRO)+BIOTA+SPATIAL)/5
          }
        }
        else{
          Cottonwood[15,1] <- 0
        }
      }
    }
    else{
      Cottonwood <- data.frame(Values=rep(NA_real_,15))
    }
    values[["Cottonwood"]] <- Cottonwood
    
    
    if(!is.null(input$Chatfield)){
      Chatfield <- hot_to_r(input$Chatfield)
      filled <- 0
      for(i in 1:32){
        if(!is.na(Chatfield[i,1]) && is.numeric(Chatfield[i,1])){
          filled <- filled + 1
        }
        if(i==32 && filled==32){
          V1 <- ((sum(Chatfield[1:4,1])/4)+min(Chatfield[1:4,1]))/2
          V2 <- ((sum(Chatfield[5:7,1])/3)+min(Chatfield[5:7,1]))/2
          V3 <- ((sum(Chatfield[8:10,1])/3)+min(Chatfield[8:10,1]))/2
          V4 <- ((sum(Chatfield[11:13,1])/3)+min(Chatfield[11:13,1]))/2
          V5 <- ((sum(Chatfield[14:16,1])/3)+min(Chatfield[14:16,1]))/2
          V6 <- ((sum(Chatfield[17:18,1])/2)+min(Chatfield[17:18,1]))/2
          V7 <- ((sum(Chatfield[19:22,1])/4)+min(Chatfield[19:22,1]))/2
          V8 <- ((sum(Chatfield[23:24,1])/2)+min(Chatfield[23:24,1]))/2
          V9 <- ((sum(Chatfield[25:27,1])/3)+min(Chatfield[25:27,1]))/2
          V10 <- ((sum(Chatfield[28:32,1])/5)+min(Chatfield[28:32,1]))/2

          Chatfield[33,1] <- ((V1*0.3)+(V2*0)+(V3*0)+(V4*0.3)+(V5*0.05)+(V6*0)+(V7*0.1)+(V8*0.1)+(V9*0.15)+(V10*0))/50-1
          Chatfield[34,1] <- ((V1*0.15)+(V2*0.15)+(V3*0)+(V4*0.1)+(V5*0.15)+(V6*0.1)+(V7*0.1)+(V8*0.15)+(V9*0.1)+(V10*0))/50-1
          Chatfield[35,1] <- ((V1*0.15)+(V2*0.05)+(V3*0.25)+(V4*0.1)+(V5*0.1)+(V6*0.05)+(V7*0)+(V8*0.05)+(V9*0.05)+(V10*0.2))/50-1
          Chatfield[36,1] <- ((V1*0.2)+(V2*0)+(V3*0.15)+(V4*0.1)+(V5*0.1)+(V6*0.05)+(V7*0)+(V8*0.1)+(V9*0.1)+(V10*0.2))/50-1
          Chatfield[37,1] <- sum(Chatfield[32:36,1])/4
        }
      }
    }
    else{
      Chatfield <- data.frame(SV=rep(NA_real_,37))
    }
    values[["Chatfield"]] <- Chatfield
    
    
    if(!is.null(input$Mink_input)){
      Mink_input <- hot_to_r(input$Mink_input)
      filled <- 0
      for(i in 1:8){
        if(!is.na(Mink_input[i,1]) && is.numeric(Mink_input[i,1])){
          filled <- filled + 1
        }
        if(i==8 && filled==8){
          if(Mink_input[3,1]>=0 && Mink_input[3,1]<25){
            Mink_input[3,2] <- 0
          }
          else if(Mink_input[3,1]>=25 && Mink_input[3,1]<75){
            Mink_input[3,2] <- -0.5+(0.02*Mink_input[3,1])
          }
          else if(Mink_input[3,1]>=75 && Mink_input[3,1]<=100){
            Mink_input[3,2] <- 1
          }
          if(Mink_input[4,1]>=0 && Mink_input[4,1]<75){
            Mink_input[4,2] <- 0.1+(0.012*Mink_input[4,1])
          }
          else if(Mink_input[4,1]>=75 && Mink_input[4,1]<=100){
            Mink_input[4,2] <- 1
          }
          if(Mink_input[5,1]>=0 && Mink_input[5,1]<75){
            Mink_input[5,2] <- 0.1+(0.012*Mink_input[5,1])
          }
          else if(Mink_input[5,1]>=75 && Mink_input[5,1]<=100){
            Mink_input[5,2] <- 1
          }
          if(Mink_input[6,1]>=0 && Mink_input[6,1]<50){
            Mink_input[6,2] <- 0+(0.02*Mink_input[6,1])
          }
          else if(Mink_input[6,1]>=50 && Mink_input[6,1]<75){
            Mink_input[6,2] <- 1
          }
          else if(Mink_input[6,1]>=75 && Mink_input[6,1]<=100){
            Mink_input[6,2] <- 1.6+(-0.008*Mink_input[6,1])
          }
          if(Mink_input[7,1]>=0 && Mink_input[7,1]<75){
            Mink_input[7,2] <- 0.1+(0.012*Mink_input[7,1])
          }
          else if(Mink_input[7,1]>=75 && Mink_input[7,1]<=100){
            Mink_input[7,2] <- 1
          }
          if(Mink_input[8,1]>=0 && Mink_input[8,1]<=100){
            Mink_input[8,2] <- 0+(0.01*Mink_input[8,1])
          }
          if(Mink_input[2,1]>=405){
            Mink_input[9,2] <- min(Mink_input[4:6,2])
          }
          else{
            Mink_input[9,2] <- FALSE
          }
          if(Mink_input[2,1]>=0 && Mink_input[2,1]<405){
            Mink_input[10,2] <- (min(1,sum(Mink_input[4:6,2]))+Mink_input[7,2])/2
          }
          else{
            Mink_input[10,2] <- FALSE
          }
          Mink_input[11,2] <- (Mink_input[7,2]*Mink_input[8,2])**0.5
          Mink_input[12,2] <- (4*Mink_input[6,2]+Mink_input[7,2])/5
          Mink_input[13,2] <- min(Mink_input[6:11,2])
        }
      }
    }
    else{
      Mink_input <- data.frame(Data=rep(NA_real_,13),HSI=rep(NA_real_,13))
    }
    values[["Mink_input"]] <- Mink_input
    
    
    if(!is.null(input$Resaca_single_species)){
      Resaca_single_species <- hot_to_r(input$Resaca_single_species)
    }
    else{
      load('resaca.rda')
      Resaca_single_species <- cbind(rep(NA_real_,64),resaca)
    }
    values[["Resaca_single_species"]] <- Resaca_single_species
    
    if(!is.null(input$Resaca_single_metric)){
      Resaca_single_metric <- hot_to_r(input$Resaca_single_metric)
      if(!is.na(Resaca_single_metric[1,1])){
        if(Resaca_single_metric[1,1]<15)
          Resaca_single_metric[2,1] <- 0.0667*Resaca_single_metric[1,1]
        else
          Resaca_single_metric[2,1] <- 1
      }
      else
        Resaca_single_metric[2,1] <- 0 
      if(!is.na(Resaca_single_metric[1,2])){
        if(Resaca_single_metric[1,2]>75)
          Resaca_single_metric[2,2] <- 1
        else
          Resaca_single_metric[2,2] <- Resaca_single_metric[1,2]/75
      }
      else
        Resaca_single_metric[2,2] <- 0 
      if(is.na(Resaca_single_metric[1,3])){
        #is a dropdown menu that starts as NULL
        Resaca_single_metric[2,3] <- 0
      }
      else{
        load('resaca_model.rda')
        for(i in 1:64){
          if(!is.na(Resaca_single_species[i,1])){
            j <- 2
            k <- 3
            if(Resaca_single_metric[1,3]=="Ebony/Snake-eyes Shrubland"){
              j <- j + 16
              k <- k + 9
            }
            else if(Resaca_single_metric[1,3]=="Subtropical Texas Palmetto Woodland"){
              j <- j + 9
              k <- k + 5
            }
            else if(Resaca_single_metric[1,3]=="Texas Ebony Resaca Forest"){
              j <- j + 1
              k <- k + 1
            }
            if(Resaca_single_species[i,1] > 0){
              Resaca_single_species[i,k+3] <- 1
            }
            if(is.na(Resaca_single_species[i,k])){
              next
            }
            if(Resaca_single_species[i,1] < resaca_model[i,j]){
              Resaca_single_species[i,k+2] <- Resaca_single_species[i,1] * resaca_model[i,j+1]
            }
            else if(Resaca_single_species[i,1] >= resaca_model[i,j+1] && Resaca_single_species[i,1] <= resaca_model[i,j+2]){
              Resaca_single_species[i,k+2] <- 1
            }
            else if(Resaca_single_species[i,1] > resaca_model[i,j+4] && Resaca_single_species[i,1] <= resaca_model[i,j+5]){
              Resaca_single_species[i,k+2] <- Resaca_single_species[i,1] * resaca_model[i,j+7] + resaca_model[i,j+8]
            }
            else{
              Resaca_single_species[i,k+2] <- 0
            }
          }
          else{
            next
          }
        }
        if(Resaca_single_metric[1,3]=="Ebony/Snake-eyes Shrubland"){
          if(sum(Resaca_single_species[,15],na.rm=TRUE)<20 && sum(Resaca_single_species[,15],na.rm=TRUE)>0){
            Resaca_single_metric[2,4] <- sum(Resaca_single_species[,14],na.rm=TRUE)/20
            Resaca_single_metric[2,5] <- sum(Resaca_single_species[,15],na.rm=TRUE)/20
          }
          else if(sum(Resaca_single_species[,15],na.rm=TRUE)>20){
            Resaca_single_metric[2,4] <- sum(Resaca_single_species[,14],na.rm=TRUE)/sum(Resaca_single_species[,15],na.rm=TRUE)
            Resaca_single_metric[2,5] <- 1
          }
          else{
            Resaca_single_metric[2,4] <- 0
            Resaca_single_metric[2,5] <- 0
          }
        }
        else if(Resaca_single_metric[1,3]=="Subtropical Texas Palmetto Woodland"){
          if(sum(Resaca_single_species[,11],na.rm=TRUE)<18 && sum(Resaca_single_species[,11],na.rm=TRUE)>0){
            Resaca_single_metric[2,4] <- sum(Resaca_single_species[,10],na.rm=TRUE)/18
            Resaca_single_metric[2,5] <- sum(Resaca_single_species[,11],na.rm=TRUE)/18
          }
          else if(sum(Resaca_single_species[,11],na.rm=TRUE)>20){
            Resaca_single_metric[2,4] <- sum(Resaca_single_species[,10],na.rm=TRUE)/sum(Resaca_single_species[,11],na.rm=TRUE)
            Resaca_single_metric[2,5] <- 1
          }
          else{
            Resaca_single_metric[2,4] <- 0
            Resaca_single_metric[2,5] <- 0
          }
        }
        else if(Resaca_single_metric[1,3]=="Texas Ebony Resaca Forest"){
          if(sum(Resaca_single_species[,7],na.rm=TRUE)<24 && sum(Resaca_single_species[,7],na.rm=TRUE)>0){
            Resaca_single_metric[2,4] <- sum(Resaca_single_species[,6],na.rm=TRUE)/24
            Resaca_single_metric[2,5] <- sum(Resaca_single_species[,7],na.rm=TRUE)/24
          }
          else if(sum(Resaca_single_species[,7],na.rm=TRUE)>20){
            Resaca_single_metric[2,4] <- sum(Resaca_single_species[,6],na.rm=TRUE)/sum(Resaca_single_species[,7],na.rm=TRUE)
            Resaca_single_metric[2,5] <- 1
          }
          else{
            Resaca_single_metric[2,4] <- 0
            Resaca_single_metric[2,5] <- 0
          }
        }
      }
      if(!is.na(Resaca_single_metric[1,6])){
        if(Resaca_single_metric[1,6]>85)
          Resaca_single_metric[2,6] <- 1
        else
          Resaca_single_metric[2,6] <- Resaca_single_metric[1,6]/85
      }
      else
        Resaca_single_metric[2,6] <- 0
      if(!is.na(Resaca_single_metric[1,7])){
        if(Resaca_single_metric[1,7]>60)
          Resaca_single_metric[2,7] <- 1
        else
          Resaca_single_metric[2,7] <- Resaca_single_metric[1,7]/60
      }
      else
        Resaca_single_metric[2,7] <- 0
      if(!is.na(Resaca_single_metric[1,8])){
        Resaca_single_metric[2,8] <- 1-(Resaca_single_metric[1,8]/100)
      }
      else
        Resaca_single_metric[2,8] <- 0
      if(!is.na(Resaca_single_metric[1,9])){
        if(Resaca_single_metric[1,9]=='Permanent/Connected')
          Resaca_single_metric[2,9] <- 1
        else if(Resaca_single_metric[1,9]=='Semipermanent/Disconnected')
          Resaca_single_metric[2,9] <- 0.5
        else
          Resaca_single_metric[2,9] <- 0
      }
      if(!is.na(Resaca_single_metric[1,10])){
        if(Resaca_single_metric[1,10]<5)
          Resaca_single_metric[2,10] <- Resaca_single_metric[1,10]/5
        else
          Resaca_single_metric[2,10] <- 1
      }
      
      # Calculate final answer for resaca
      Resaca_single_metric[2,11] <- ((Resaca_single_metric[2,1]+Resaca_single_metric[2,2])/2+
        (Resaca_single_metric[2,4]+Resaca_single_metric[2,5]+Resaca_single_metric[2,6]+Resaca_single_metric[2,7])/4+
        Resaca_single_metric[2,8]+(as.numeric(Resaca_single_metric[2,9])+Resaca_single_metric[2,10])/2)/4
      values[["Resaca_single_species"]] <- Resaca_single_species
    }
    else{
      Resaca_single_metric <- data.frame(slope=rep(NA_real_,2),bank=rep(NA_real_,2),
                                  habitat=rep(NA_character_,2),species=rep(NA_real_,2),
                                  richness=rep(NA_real_,2),ripcancov=rep(NA_real_,2),
                                  aqcancov=rep(NA_real_,2),invasives=rep(NA_real_,2),
                                  waterregime=rep(NA_character_,2),waterdepth=rep(NA_real_,2)
                                  ,final=rep(NA_real_,2))
    }
    values[["Resaca_single_metric"]] <- Resaca_single_metric
    
    
    if(!is.null(input$Skokomish_input)){
      Skokomish_input <- hot_to_r(input$Skokomish_input)
    }
    else{
      Skokomish_input <- data.frame(factorsnum=rep(NA_character_,1))
    }
    values[["Skokomish_input"]] <- Skokomish_input
    
    
    if(!is.null(input$Skokomish) && !is.null(input$Skokomish_input)){
      Skokomish <- hot_to_r(input$Skokomish)
      Skokomish_input <- hot_to_r(input$Skokomish_input)
      i=1
              if(!is.na(Skokomish_input[i,1]) && !is.na(Skokomish[1,i]) && !is.na(Skokomish[2,i]) 
                 && !is.na(Skokomish[3,i]) && !is.na(Skokomish[4,i]) && !is.na(Skokomish[5,i]) && !is.na(Skokomish[6,i])){
                if(Skokomish_input[i,1]=="Channel Capacity and In-Channel Habitat"){
                  Skokomish[7,i] <- (Skokomish[2,i]+Skokomish[5,i]+Skokomish[6,i])/3
                }
                else if(Skokomish_input[i,1]=="In-Channel Habitat"){
                  Skokomish[7,i] <- (Skokomish[2,i]+Skokomish[3,i])/2
                }
                else if(Skokomish_input[i,1]=="Floodplain Habitat"){
                  Skokomish[7,i] <- (Skokomish[4,i]+Skokomish[5,i])/2
                }
                Skokomish[8,i] <- Skokomish[1,i] * Skokomish[7,i]
              }
            }
    else{
      Skokomish <- data.frame(inputs=rep(NA_real_,8))
    }
    values[["Skokomish"]] <- Skokomish
    
    if(!is.null(input$LowerWill_input)){
      LowerWill_input <- hot_to_r(input$LowerWill_input)
    }
    else{
      LowerWill_input <- data.frame(VariableSelection=rep(NA_character_,1),Acres=rep(NA_real_,1))
    }
    values[["LowerWill_input"]] <- LowerWill_input
    
    if(!is.null(input$LowerWill)){
      LowerWill <- hot_to_r(input$LowerWill)
        LowerWill_input <- values[["LowerWill_input"]]
          if(!is.na(LowerWill_input[1,1])){
            if(!is.na(LowerWill[1,1])){
            if(LowerWill_input[1,1]=="Western Pond Turtle"){
              if(!is.na(LowerWill[1,1])){
                if(as.numeric(LowerWill[1,1])==0){
                  LowerWill[1,2] <- 0
                }
                else if(as.numeric(LowerWill[1,1])>0 && as.numeric(LowerWill[1,1])<20){
                  LowerWill[1,2] <- 0 + ((as.numeric(LowerWill[1,1])-0)/20)*(0.5-0)
                }
                else if(as.numeric(LowerWill[1,1])>=20 && as.numeric(LowerWill[1,1])<50){
                  LowerWill[1,2] <- 0.5 + ((as.numeric(LowerWill[1,1])-20)/30)*(1-0.5)
                }
                else if(as.numeric(LowerWill[1,1])>=50 && as.numeric(LowerWill[1,1])<=75){
                  LowerWill[1,2] <- 1
                }
                else if(as.numeric(LowerWill[1,1])>75 && as.numeric(LowerWill[1,1])<100){
                  LowerWill[1,2] <- 1 - ((as.numeric(LowerWill[1,1])-75)/25)*(1-0.2)
                }
                else if(as.numeric(LowerWill[1,1])>=100){
                  LowerWill[1,2] <- 0.2
                }
              }
              if(!is.na(LowerWill[2,1])){
                if(as.numeric(LowerWill[2,1])<=0){
                  LowerWill[2,2] <- 0
                }
                else if(as.numeric(LowerWill[2,1])>0 && as.numeric(LowerWill[2,1])<25){
                  LowerWill[2,2] <- 0 + ((as.numeric(LowerWill[2,1])-5)/5)*(0.2-0)
                }
                else if(as.numeric(LowerWill[2,1])>=25 && as.numeric(LowerWill[2,1])<50){
                  LowerWill[2,2] <- 0.2 + ((as.numeric(LowerWill[2,1])-25)/25)*(0.5-0.2)
                }
                else if(as.numeric(LowerWill[2,1])>=50 && as.numeric(LowerWill[2,1])<75){
                  LowerWill[2,2] <- 0.5 + ((as.numeric(LowerWill[2,1])-50)/25)*(1-0.5)
                }
                else if(as.numeric(LowerWill[2,1])>=75){
                  LowerWill[2,2] <-1
                }
              }
              if(!is.na(LowerWill[3,1])){
                if(as.numeric(LowerWill[3,1])<=5){
                  LowerWill[3,2] <- 0
                }
                else if(as.numeric(LowerWill[3,1])>=5 && as.numeric(LowerWill[3,1])<10){
                  LowerWill[3,2] <- 0 + ((as.numeric(LowerWill[3,1])-5)/5)*(0.2-0)
                }
                else if(as.numeric(LowerWill[3,1])>=10 && as.numeric(LowerWill[3,1])<15){
                  LowerWill[3,2] <- 0.2 + ((as.numeric(LowerWill[3,1])-10)/5)*(0.6-0.2)
                }
                else if(as.numeric(LowerWill[3,1])>=15 && as.numeric(LowerWill[3,1])<20){
                  LowerWill[3,2] <- 0.6 + ((as.numeric(LowerWill[3,1])-15)/5)*(1-0.6)
                }
                else if(as.numeric(LowerWill[3,1])>=20 && as.numeric(LowerWill[3,1])<25){
                  LowerWill[3,2] <- 1
                }
                else if(as.numeric(LowerWill[3,1])>=25 && as.numeric(LowerWill[3,1])<30){
                  LowerWill[3,2] <- 1 -((as.numeric(LowerWill[3,1])-25)/5)*(1-0.6)
                }
                else if(as.numeric(LowerWill[3,1])>=30){
                  LowerWill[3,2] <- 0.6
                }
              }
              if(!is.na(LowerWill[4,1])){
                if(as.numeric(LowerWill[4,1])>=0 && as.numeric(LowerWill[4,1])<25){
                  LowerWill[4,2] <- 0.1+((as.numeric(LowerWill[4,1])-0)/25)*(1-0.1)
                }
                else if(as.numeric(LowerWill[4,1])>=25 && as.numeric(LowerWill[4,1])<50){
                  LowerWill[4,2] <- 1
                }
                else if(as.numeric(LowerWill[4,1])>=50 && as.numeric(LowerWill[4,1])<75){
                  LowerWill[4,2] <- 1 -((as.numeric(LowerWill[4,1])-50)/25)*(1-0.3)
                }
                else if(as.numeric(LowerWill[4,1])>=75 && as.numeric(LowerWill[4,1])<100){
                  LowerWill[4,2] <- 0.3 -((as.numeric(LowerWill[4,1])-75)/25)*(0.3-0)
                }
                else if(as.numeric(LowerWill[4,1])>=100){
                  LowerWill[4,2] <- 0
                }
              }
              if(!is.na(LowerWill[5,1])){
                if(LowerWill[5,1]=="None"){
                  LowerWill[5,2] <- 0
                }
                else if(LowerWill[5,1]=="Very few"){
                  LowerWill[5,2] <- 0.2
                }
                else if(LowerWill[5,1]=="Sparse"){
                  LowerWill[5,2] <- 0.5
                }
                else if(LowerWill[5,1]=="Moderate"){
                  LowerWill[5,2] <- 0.8
                }
                else if(LowerWill[5,1]=="Abundant"){
                  LowerWill[5,2] <- 1
                }
              }
              if(!is.na(LowerWill[5,2]) && !is.na(LowerWill[4,2]) && !is.na(LowerWill[3,2]) && !is.na(LowerWill[2,2]) && !is.na(LowerWill[1,2])){
                LowerWill[1,3] <- (LowerWill[1,2] + LowerWill[2,2] + LowerWill[3,2] + LowerWill[4,2] + LowerWill[5,2])/5
                LowerWill[1,4] <- LowerWill[1,3] * LowerWill_input[1,2]
              }
            }
            else if(LowerWill_input[1,1]=="Beaver"){
              if(!is.na(LowerWill[1,1])){
                if(as.numeric(LowerWill[1,1])==0){
                  LowerWill[1,2] <- 0
                }
                else if(as.numeric(LowerWill[1,1])>0 && as.numeric(LowerWill[1,1])<25){
                  LowerWill[1,2] <- 0 + ((as.numeric(LowerWill[1,1])-0)/25)*(0.5-0)
                }
                else if(as.numeric(LowerWill[1,1])>=25 && as.numeric(LowerWill[1,1])<50){
                  LowerWill[1,2] <- 0.5 + ((as.numeric(LowerWill[1,1])-25)/25)*(1-0.5)
                }
                else if(as.numeric(LowerWill[1,1])>50 && as.numeric(LowerWill[1,1])<75){
                  LowerWill[1,2] <- 1 - ((as.numeric(LowerWill[1,1])-50)/25)*(1-0.8)
                }
                else if(as.numeric(LowerWill[1,1])>75 && as.numeric(LowerWill[1,1])<100){
                  LowerWill[1,2] <- 0.8 - ((as.numeric(LowerWill[1,1])-75)/25)*(0.8-0.6)
                }
                else if(as.numeric(LowerWill[1,1])>=100){
                  LowerWill[1,2] <- 0.6
                }
              }
              if(!is.na(LowerWill[2,1])){
                if(as.numeric(LowerWill[2,1])<=0){
                  LowerWill[2,2] <- 0.2
                }
                else if(as.numeric(LowerWill[2,1])>0 && as.numeric(LowerWill[2,1])<25){
                  LowerWill[2,2] <- 0.2 + ((as.numeric(LowerWill[2,1])-0)/25)*(0.4-0.2)
                }
                else if(as.numeric(LowerWill[2,1])>=25 && as.numeric(LowerWill[2,1])<50){
                  LowerWill[2,2] <- 0.4 + ((as.numeric(LowerWill[2,1])-25)/25)*(0.6-0.4)
                }
                else if(as.numeric(LowerWill[2,1])>=50 && as.numeric(LowerWill[2,1])<75){
                  LowerWill[2,2] <- 0.6 + ((as.numeric(LowerWill[2,1])-50)/25)*(0.8-0.6)
                }
                else if(as.numeric(LowerWill[2,1])>=75 && as.numeric(LowerWill[2,1])<100){
                  LowerWill[2,2] <- 0.8 + ((as.numeric(LowerWill[2,1])-75)/25)*(1-0.8)
                }
                else if(as.numeric(LowerWill[2,1])>=100){
                  LowerWill[2,2] <-1
                }
              }
              if(!is.na(LowerWill[3,1])){
                if(as.numeric(LowerWill[3,1])<=0){
                  LowerWill[3,2] <- 0
                }
                else if(as.numeric(LowerWill[3,1])>0 && as.numeric(LowerWill[3,1])<25){
                  LowerWill[3,2] <- 0 + ((as.numeric(LowerWill[3,1])-0)/25)*(0.6-0)
                }
                else if(as.numeric(LowerWill[3,1])>=25 && as.numeric(LowerWill[3,1])<50){
                  LowerWill[3,2] <- 0.6 + ((as.numeric(LowerWill[3,1])-25)/25)*(1-0.6)
                }
                else if(as.numeric(LowerWill[3,1])>=50 && as.numeric(LowerWill[3,1])<75){
                  LowerWill[3,2] <- 1 - ((as.numeric(LowerWill[3,1])-50)/25)*(1-0.9)
                }
                else if(as.numeric(LowerWill[3,1])>=75 && as.numeric(LowerWill[3,1])<100){
                  LowerWill[3,2] <- 0.9 - ((as.numeric(LowerWill[3,1])-75)/25)*(0.9-0.8)
                }
                else if(as.numeric(LowerWill[3,1])>=100){
                  LowerWill[3,2] <-0.8
                }
              }
              if(!is.na(LowerWill[4,1])){
                if(as.numeric(LowerWill[4,1])>=0 && as.numeric(LowerWill[4,1])<1){
                  LowerWill[4,2] <- 0 + ((as.numeric(LowerWill[4,1])-0)/1)*(0.3-0)
                }
                else if(as.numeric(LowerWill[4,1])>=1 && as.numeric(LowerWill[4,1])<2){
                  LowerWill[4,2] <- 0.3 + ((as.numeric(LowerWill[4,1])-1)/1)*(1-0.3)
                }
                else if(as.numeric(LowerWill[4,1])>=2){
                  LowerWill[4,2] <- 1
                }
              }
              if(!is.na(LowerWill[5,1])){
                if(LowerWill[5,1]=="A"){
                  LowerWill[5,2] <- 1
                }
                if(LowerWill[5,1]=="B"){
                  LowerWill[5,2] <- 0.6
                }
                if(LowerWill[5,1]=="C"){
                  LowerWill[5,2] <- 0.2
                }
              }
              if(!is.na(LowerWill[5,2]) && !is.na(LowerWill[4,2]) && !is.na(LowerWill[3,2]) && !is.na(LowerWill[2,2]) && !is.na(LowerWill[1,2])){
                LowerWill[1,3] <- (LowerWill[1,2] + LowerWill[2,2] + LowerWill[3,2] + LowerWill[4,2] + LowerWill[5,2])/5
                LowerWill[1,4] <- LowerWill[1,3] * LowerWill_input[1,2]
              }
            }
            else if(LowerWill_input[1,1]=="Wood Duck"){
              if(as.numeric(LowerWill[1,1])>=0 && as.numeric(LowerWill[1,1])<25){
                LowerWill[1,2] <- 0 + ((as.numeric(LowerWill[1,1])-0)/25)*(0.4-0)
              }
              if(as.numeric(LowerWill[1,1])>=25 && as.numeric(LowerWill[1,1])<40){
                LowerWill[1,2] <- 0.4 + ((as.numeric(LowerWill[1,1])-25)/15)*(0.8-0.4)
              }
              if(as.numeric(LowerWill[1,1])>=40 && as.numeric(LowerWill[1,1])<50){
                LowerWill[1,2] <- 0.8 + ((as.numeric(LowerWill[1,1])-40)/10)*(1-0.8)
              }
              if(as.numeric(LowerWill[1,1])>=50 && as.numeric(LowerWill[1,1])<75){
                LowerWill[1,2] <- 1
              }
              if(as.numeric(LowerWill[1,1])>=75 && as.numeric(LowerWill[1,1])<85){
                LowerWill[1,2] <- 1 - ((as.numeric(LowerWill[1,1])-75)/10)*(1-0.6)
              }
              if(as.numeric(LowerWill[1,1])>=85 && as.numeric(LowerWill[1,1])<100){
                LowerWill[1,2] <- 0.6 - (((as.numeric(LowerWill[1,1])-85)/15))*(0.6-0)
              }
              if(as.numeric(LowerWill[1,1])>=100){
                LowerWill[1,2] <- 0
              }
              LowerWill[1,3] <- LowerWill[1,2]
              LowerWill[1,4] <- LowerWill[1,3] * LowerWill_input[1,2]
            }
            else if(LowerWill_input[1,1]=="Yellow Warbler"){
              if(!is.na(LowerWill[1,1])){
                if(as.numeric(LowerWill[1,1])>=0 && as.numeric(LowerWill[1,1])<25){
                  LowerWill[1,2] <- 0 + ((as.numeric(LowerWill[1,1])-0)/25)*(0.4-0)
                }
                if(as.numeric(LowerWill[1,1])>=25 && as.numeric(LowerWill[1,1])<50){
                  LowerWill[1,2] <- 0.4 + ((as.numeric(LowerWill[1,1])-25)/25)*(0.75-0.4)
                }
                if(as.numeric(LowerWill[1,1])>=50 && as.numeric(LowerWill[1,1])<60){
                  LowerWill[1,2] <- 0.75 + ((as.numeric(LowerWill[1,1])-50)/10)*(1-0.75)
                }
                if(as.numeric(LowerWill[1,1])>=60 && as.numeric(LowerWill[1,1])<80){
                  LowerWill[1,2] <- 1
                }
                if(as.numeric(LowerWill[1,1])>=80 && as.numeric(LowerWill[1,1])<90){
                  LowerWill[1,2] <- 1 - ((as.numeric(LowerWill[1,1])-80)/10)*(1-0.8)
                }
                if(as.numeric(LowerWill[1,1])>=90 && as.numeric(LowerWill[1,1])<100){
                  LowerWill[1,2] <- 0.8 - (((as.numeric(LowerWill[1,1])-90)/10))*(0.8-0.6)
                }
                if(as.numeric(LowerWill[1,1])>=100){
                  LowerWill[1,2] <- 0.6
                }
              }
              if(!is.na(LowerWill[2,1])){
                if(as.numeric(LowerWill[2,1])<=0){
                  LowerWill[2,2] <- 0
                }
                else if(as.numeric(LowerWill[2,1])>0 && as.numeric(LowerWill[2,1])<20){
                  LowerWill[2,2] <- 0
                }
                else if(as.numeric(LowerWill[2,1])>=20 && as.numeric(LowerWill[2,1])<40){
                  LowerWill[2,2] <- 0.1
                }
                else if(as.numeric(LowerWill[2,1])>=40 && as.numeric(LowerWill[2,1])<60){
                  LowerWill[2,2] <- 0.2
                }
                else if(as.numeric(LowerWill[2,1])>=60 && as.numeric(LowerWill[2,1])<70){
                  LowerWill[2,2] <- 0.8
                }
                else if(as.numeric(LowerWill[2,1])>=70 && as.numeric(LowerWill[2,1])<80){
                  LowerWill[2,2] <- 1
                }
                else if(as.numeric(LowerWill[2,1])>=80 && as.numeric(LowerWill[2,1])<=100){
                  LowerWill[2,2] <- 0.1
                }
              }
              if(!is.na(LowerWill[3,1])){
                if(as.numeric(LowerWill[3,1])<=0){
                  LowerWill[3,2] <- 0
                }
                else if(as.numeric(LowerWill[3,1])==1){
                  LowerWill[3,2] <- 0.5
                }
                else if(as.numeric(LowerWill[3,1])>=2){
                  LowerWill[3,2] <- 1
                }
              }
              if(!is.na(LowerWill[4,1])){
                if(as.numeric(LowerWill[4,1])>=0 && as.numeric(LowerWill[4,1])<25){
                  LowerWill[4,2] <- 0.1 + ((as.numeric(LowerWill[4,1])-0)/25)*(0.3-0.1)
                }
                else if(as.numeric(LowerWill[4,1])>=25 && as.numeric(LowerWill[4,1])<50){
                  LowerWill[4,2] <- 0.3 + ((as.numeric(LowerWill[4,1])-25)/25)*(0.55-0.3)
                }
                else if(as.numeric(LowerWill[4,1])>=50 && as.numeric(LowerWill[4,1])<75){
                  LowerWill[4,2] <- 0.55 + ((as.numeric(LowerWill[4,1])-50)/25)*(0.8-0.55)
                }
                else if(as.numeric(LowerWill[4,1])>=75 && as.numeric(LowerWill[4,1])<100){
                  LowerWill[4,2] <- 0.8 + ((as.numeric(LowerWill[4,1])-75)/25)*(1-0.8)
                }
                else if(as.numeric(LowerWill[4,1])>=100){
                  LowerWill[4,2] <- 1
                }
              }
              if(!is.na(LowerWill[4,2]) && !is.na(LowerWill[3,2]) && !is.na(LowerWill[2,2]) && !is.na(LowerWill[1,2])){
                LowerWill[1,3] <- (LowerWill[1,2] + LowerWill[2,2] + LowerWill[3,2] + LowerWill[4,2])/4
                LowerWill[1,4] <- LowerWill[1,3] * LowerWill_input[1,2]
              }
            }
            else if(LowerWill_input[1,1]=="Native Amphibians"){
              if(!is.na(LowerWill[1,1])){
                if(as.numeric(LowerWill[1,1])==0){
                  LowerWill[1,2] <- 0
                }
                else if(as.numeric(LowerWill[1,1])>0 && as.numeric(LowerWill[1,1])<10){
                  LowerWill[1,2] <- 0 + ((as.numeric(LowerWill[1,1])-0)/10)*(0.6-0)
                }
                else if(as.numeric(LowerWill[1,1])>=10 && as.numeric(LowerWill[1,1])<25){
                  LowerWill[1,2] <- 0.6 + ((as.numeric(LowerWill[1,1])-10)/15)*(1-0.6)
                }
                else if(as.numeric(LowerWill[1,1])>=25 && as.numeric(LowerWill[1,1])<=40){
                  LowerWill[1,2] <- 1
                }
                else if(as.numeric(LowerWill[1,1])>40 && as.numeric(LowerWill[1,1])<50){
                  LowerWill[1,2] <- 1 - ((as.numeric(LowerWill[1,1])-40)/10)*(1-0.2)
                }
                else if(as.numeric(LowerWill[1,1])>=50){
                  LowerWill[1,2] <- 0.2
                }
              }
              if(!is.na(LowerWill[2,1])){
                if(as.numeric(LowerWill[2,1])<=0){
                  LowerWill[2,2] <- 0
                }
                else if(as.numeric(LowerWill[2,1])>0 && as.numeric(LowerWill[2,1])<25){
                  LowerWill[2,2] <- 0 + ((as.numeric(LowerWill[2,1])-0)/25)*(0.5-0)
                }
                else if(as.numeric(LowerWill[2,1])>=25 && as.numeric(LowerWill[2,1])<50){
                  LowerWill[2,2] <- 0.5 + ((as.numeric(LowerWill[2,1])-25)/25)*(1-0.5)
                }
                else if(as.numeric(LowerWill[2,1])>=50){
                  LowerWill[2,2] <- 1
                }
              }
              if(!is.na(LowerWill[3,1])){
                if(as.numeric(LowerWill[3,1])<=0){
                  LowerWill[3,2] <- 0
                }
                else if(as.numeric(LowerWill[3,1])>=0 && as.numeric(LowerWill[3,1])<25){
                  LowerWill[3,2] <- 0 + ((as.numeric(LowerWill[3,1])-0)/25)*(0.3-0)
                }
                else if(as.numeric(LowerWill[3,1])>=25 && as.numeric(LowerWill[3,1])<50){
                  LowerWill[3,2] <- 0.3 + ((as.numeric(LowerWill[3,1])-25)/25)*(0.6-0.3)
                }
                else if(as.numeric(LowerWill[3,1])>=50 && as.numeric(LowerWill[3,1])<75){
                  LowerWill[3,2] <- 0.6 + ((as.numeric(LowerWill[3,1])-50)/25)*(0.9-0.6)
                }
                else if(as.numeric(LowerWill[3,1])>=75 && as.numeric(LowerWill[3,1])<100){
                  LowerWill[3,2] <- 0.9 + ((as.numeric(LowerWill[3,1])-75)/25)*(1-0.9)
                }
                else if(as.numeric(LowerWill[3,1])>=100){
                  LowerWill[3,2] <- 1
                }
              }
              if(!is.na(LowerWill[4,1])){
                if(as.numeric(LowerWill[4,1])>=0 && as.numeric(LowerWill[4,1])<10){
                  LowerWill[4,2] <- 0 + ((as.numeric(LowerWill[4,1])-0)/10)*(0.1-0)
                }
                else if(as.numeric(LowerWill[4,1])>=10 && as.numeric(LowerWill[4,1])<30){
                  LowerWill[4,2] <- 0.1 + ((as.numeric(LowerWill[4,1])-10)/20)*(0.6-0.1)
                }
                else if(as.numeric(LowerWill[4,1])>=30 && as.numeric(LowerWill[4,1])<60){
                  LowerWill[4,2] <- 0.6 + ((as.numeric(LowerWill[4,1])-30)/30)*(1-0.6)
                }
                else if(as.numeric(LowerWill[4,1])>=60){
                  LowerWill[4,2] <- 1
                }
              }
              if(!is.na(LowerWill[5,1])){
                if(as.numeric(LowerWill[5,1])>=0 && as.numeric(LowerWill[5,1])<5){
                  LowerWill[5,2] <- 0.1 + ((as.numeric(LowerWill[5,1])-0)/5)*(0.5-0.1)
                }
                else if(as.numeric(LowerWill[5,1])>=5 && as.numeric(LowerWill[5,1])<10){
                  LowerWill[5,2] <- 0.5 + ((as.numeric(LowerWill[5,1])-5)/5)*(1-0.5)
                }
                else if(as.numeric(LowerWill[5,1])>=10 && as.numeric(LowerWill[5,1])<15){
                  LowerWill[5,2] <- 1 - ((as.numeric(LowerWill[5,1])-10)/5)*(1-0.3)
                }
                else if(as.numeric(LowerWill[5,1])>=15 && as.numeric(LowerWill[5,1])<20){
                  LowerWill[5,2] <- 0.3 - ((as.numeric(LowerWill[5,1])-15)/5)*(0.3-0)
                }
                else if(as.numeric(LowerWill[5,1])>=20){
                  LowerWill[5,2] <- 0
                }
              }
              if(!is.na(LowerWill[6,1])){
                if(LowerWill[6,1]=="Developed"){
                  LowerWill[6,2] <- 0
                }
                else if(LowerWill[6,1]=="Row Crops"){
                  LowerWill[6,2] <- 0.1
                }
                else if(LowerWill[6,1]=="Managed Pasture"){
                  LowerWill[6,2] <- 0.5
                }
                else if(LowerWill[6,1]=="Fallow Grass/herbs"){
                  LowerWill[6,2] <- 0.7
                }
                else if(LowerWill[6,1]=="Shrubs/trees"){
                  LowerWill[6,2] <- 1
                }
              }
              if(!is.na(LowerWill[6,2]) && !is.na(LowerWill[5,2]) && !is.na(LowerWill[4,2]) && !is.na(LowerWill[3,2]) && !is.na(LowerWill[2,2]) && !is.na(LowerWill[1,2])){
                LowerWill[1,3] <- (LowerWill[1,2] + LowerWill[2,2] + LowerWill[3,2] + LowerWill[4,2] + LowerWill[5,2] + LowerWill[6,2])/6
                LowerWill[1,4] <- LowerWill[1,3] * LowerWill_input[1,2]
              }
            }
            else if(LowerWill_input[1,1]=="Salmonids Tributaries - Prespawning Adults"){
              if(!is.na(LowerWill[1,1])){
                if(as.numeric(LowerWill[1,1])==0){
                  LowerWill[1,2] <- 0
                }
                else if(as.numeric(LowerWill[1,1])>0 && as.numeric(LowerWill[1,1])<5){
                  LowerWill[1,2] <- 0 + ((as.numeric(LowerWill[1,1])-0)/5)*(0.5-0)
                }
                else if(as.numeric(LowerWill[1,1])>=5 && as.numeric(LowerWill[1,1])<10){
                  LowerWill[1,2] <- 0.5 + ((as.numeric(LowerWill[1,1])-5)/5)*(1-0.5)
                }
                else if(as.numeric(LowerWill[1,1])>=10 && as.numeric(LowerWill[1,1])<15){
                  LowerWill[1,2] <- 1 - ((as.numeric(LowerWill[1,1])-10)/5)*(1-0.9)
                }
                else if(as.numeric(LowerWill[1,1])>=15 && as.numeric(LowerWill[1,1])<20){
                  LowerWill[1,2] <- 0.9 - ((as.numeric(LowerWill[1,1])-15)/5)*(0.9-0.5)
                }
                else if(as.numeric(LowerWill[1,1])>=20 && as.numeric(LowerWill[1,1])<25){
                  LowerWill[1,2] <- 0.5 - ((as.numeric(LowerWill[1,1])-20)/5)*(0.5-0)
                }
                else if(as.numeric(LowerWill[1,1])>=25){
                  LowerWill[1,2] <- 0.
                }
              }
              if(!is.na(LowerWill[2,1])){
                if(as.numeric(LowerWill[2,1])<=0){
                  LowerWill[2,2] <- 0.2
                }
                else if(as.numeric(LowerWill[2,1])>0 && as.numeric(LowerWill[2,1])<25){
                  LowerWill[2,2] <- 0.2 + ((as.numeric(LowerWill[2,1])-0)/25)*(0.6-0)
                }
                else if(as.numeric(LowerWill[2,1])>=25 && as.numeric(LowerWill[2,1])<50){
                  LowerWill[2,2] <- 0.6 + ((as.numeric(LowerWill[2,1])-25)/25)*(1-0.6)
                }
                else if(as.numeric(LowerWill[2,1])>=50 && as.numeric(LowerWill[2,1])<75){
                  LowerWill[2,2] <- 1 - ((as.numeric(LowerWill[2,1])-50)/25)*(1-0.9)
                }
                else if(as.numeric(LowerWill[2,1])>=75 && as.numeric(LowerWill[2,1])<100){
                  LowerWill[2,2] <- 0.9 - ((as.numeric(LowerWill[2,1])-75)/25)*(0.9-0.2)
                }
                else if(as.numeric(LowerWill[2,1])>=50){
                  LowerWill[2,2] <- 0.2
                }
              }
              if(!is.na(LowerWill[3,1])){
                if(as.numeric(LowerWill[3,1])<=0){
                  LowerWill[3,2] <- 0.1
                }
                else if(as.numeric(LowerWill[3,1])>0 && as.numeric(LowerWill[3,1])<10){
                  LowerWill[3,2] <- 0.1 + ((as.numeric(LowerWill[3,1])-0)/10)*(0.2-0.1)
                }
                else if(as.numeric(LowerWill[3,1])>=10 && as.numeric(LowerWill[3,1])<20){
                  LowerWill[3,2] <- 0.2 + ((as.numeric(LowerWill[3,1])-10)/10)*(0.4-0.2)
                }
                else if(as.numeric(LowerWill[3,1])>=20 && as.numeric(LowerWill[3,1])<30){
                  LowerWill[3,2] <- 0.4 + ((as.numeric(LowerWill[3,1])-20)/10)*(0.8-0.4)
                }
                else if(as.numeric(LowerWill[3,1])>=30 && as.numeric(LowerWill[3,1])<40){
                  LowerWill[3,2] <- 0.8 + ((as.numeric(LowerWill[3,1])-30)/10)*(1-0.8)
                }
                else if(as.numeric(LowerWill[3,1])>=40){
                  LowerWill[3,2] <- 1
                }
              }
              if(!is.na(LowerWill[4,1])){
                if(LowerWill[4,1]=="A"){
                  LowerWill[4,2] <- 1
                }
                else if(LowerWill[4,1]=="B"){
                  LowerWill[4,2] <- 0.6
                }
                else if(LowerWill[4,1]=="C"){
                  LowerWill[4,2] <- 0.3
                }
              }
              if(!is.na(LowerWill[6,2]) && !is.na(LowerWill[5,2]) && !is.na(LowerWill[4,2]) && !is.na(LowerWill[3,2]) && !is.na(LowerWill[2,2]) && !is.na(LowerWill[1,2])){
                LowerWill[1,3] <- (LowerWill[1,2] + LowerWill[2,2] + LowerWill[3,2] + LowerWill[4,2])/4
                LowerWill[1,4] <- LowerWill[1,3] * LowerWill_input[1,2]
              }
            }
              else if(LowerWill_input[1,1]=="Salmonids Tributaries - Juveniles"){
                if(!is.na(LowerWill[1,1])){
                  if(as.numeric(LowerWill[1,1])==0){
                    LowerWill[1,2] <- 0
                  }
                  else if(as.numeric(LowerWill[1,1])>0 && as.numeric(LowerWill[1,1])<5){
                    LowerWill[1,2] <- 0 + ((as.numeric(LowerWill[1,1])-0)/5)*(0.3-0)
                  }
                  else if(as.numeric(LowerWill[1,1])>=5 && as.numeric(LowerWill[1,1])<10){
                    LowerWill[1,2] <- 0.3 + ((as.numeric(LowerWill[1,1])-5)/5)*(0.9-0.3)
                  }
                  else if(as.numeric(LowerWill[1,1])>=10 && as.numeric(LowerWill[1,1])<15){
                    LowerWill[1,2] <- 0.9 - ((as.numeric(LowerWill[1,1])-10)/5)*(1-0.9)
                  }
                  else if(as.numeric(LowerWill[1,1])>=15 && as.numeric(LowerWill[1,1])<20){
                    LowerWill[1,2] <- 1 - ((as.numeric(LowerWill[1,1])-15)/5)*(1-0.9)
                  }
                  else if(as.numeric(LowerWill[1,1])>=20 && as.numeric(LowerWill[1,1])<25){
                    LowerWill[1,2] <- 0.9 - ((as.numeric(LowerWill[1,1])-20)/5)*(0.9-0)
                  }
                  else if(as.numeric(LowerWill[1,1])>=25){
                    LowerWill[1,2] <- 0.
                  }
                }
                if(!is.na(LowerWill[2,1])){
                  if(as.numeric(LowerWill[2,1])<=0){
                    LowerWill[2,2] <- 0.2
                  }
                  else if(as.numeric(LowerWill[2,1])>0 && as.numeric(LowerWill[2,1])<25){
                    LowerWill[2,2] <- 0.2 + ((as.numeric(LowerWill[2,1])-0)/25)*(0.6-0.2)
                  }
                  else if(as.numeric(LowerWill[2,1])>=25 && as.numeric(LowerWill[2,1])<50){
                    LowerWill[2,2] <- 0.6 + ((as.numeric(LowerWill[2,1])-25)/25)*(1-0.6)
                  }
                  else if(as.numeric(LowerWill[2,1])>=50 && as.numeric(LowerWill[2,1])<75){
                    LowerWill[2,2] <- 1 - ((as.numeric(LowerWill[2,1])-50)/25)*(1-0.9)
                  }
                  else if(as.numeric(LowerWill[2,1])>=75 && as.numeric(LowerWill[2,1])<100){
                    LowerWill[2,2] <- 0.9 - ((as.numeric(LowerWill[2,1])-75)/25)*(0.9-0.2)
                  }
                  else if(as.numeric(LowerWill[2,1])>=50){
                    LowerWill[2,2] <- 0.2
                  }
                }
                if(!is.na(LowerWill[3,1])){
                  if(as.numeric(LowerWill[3,1])<=0){
                    LowerWill[3,2] <- 0.1
                  }
                  else if(as.numeric(LowerWill[3,1])>=0 && as.numeric(LowerWill[3,1])<10){
                    LowerWill[3,2] <- 0.1 + ((as.numeric(LowerWill[3,1])-0)/10)*(0.2-0.1)
                  }
                  else if(as.numeric(LowerWill[3,1])>=10 && as.numeric(LowerWill[3,1])<20){
                    LowerWill[3,2] <- 0.2 + ((as.numeric(LowerWill[3,1])-10)/10)*(0.4-0.2)
                  }
                  else if(as.numeric(LowerWill[3,1])>=20 && as.numeric(LowerWill[3,1])<30){
                    LowerWill[3,2] <- 0.4 + ((as.numeric(LowerWill[3,1])-20)/10)*(0.8-0.4)
                  }
                  else if(as.numeric(LowerWill[3,1])>=30 && as.numeric(LowerWill[3,1])<40){
                    LowerWill[3,2] <- 0.8 + ((as.numeric(LowerWill[3,1])-30)/10)*(1-0.8)
                  }
                  else if(as.numeric(LowerWill[3,1])>=40){
                    LowerWill[3,2] <- 1
                  }
                }
                if(!is.na(LowerWill[4,1])){
                  if(LowerWill[4,1]=="A"){
                    LowerWill[4,2] <- 1
                  }
                  else if(LowerWill[4,1]=="B"){
                    LowerWill[4,2] <- 0.6
                  }
                  else if(LowerWill[4,1]=="C"){
                    LowerWill[4,2] <- 0.3
                  }
                }
                if(!is.na(LowerWill[4,2]) && !is.na(LowerWill[3,2]) && !is.na(LowerWill[2,2]) && !is.na(LowerWill[1,2])){
                  LowerWill[1,3] <- (LowerWill[1,2] + LowerWill[2,2] + LowerWill[3,2] + LowerWill[4,2])/4
                  LowerWill[1,4] <- LowerWill[1,3] * LowerWill_input[1,2]
                }
              }
              else if(LowerWill_input[1,1]=="Native Salmonids Mainstem"){
                if(!is.na(LowerWill[1,1])){
                  if(as.numeric(LowerWill[1,1])<=10){
                    LowerWill[1,2] <- 0
                  }
                  else if(as.numeric(LowerWill[1,1])>10 && as.numeric(LowerWill[1,1])<11){
                    LowerWill[1,2] <- 10 + ((as.numeric(LowerWill[1,1])-10)/1)*(0.3-0)
                  }
                  else if(as.numeric(LowerWill[1,1])>=11 && as.numeric(LowerWill[1,1])<=20){
                    LowerWill[1,2] <- 0.3
                  }
                  else if(as.numeric(LowerWill[1,1])>20 && as.numeric(LowerWill[1,1])<21){
                    LowerWill[1,2] <- 0.3 - ((as.numeric(LowerWill[1,1])-20)/1)*(1-0.3)
                  }
                  else if(as.numeric(LowerWill[1,1])>=21 && as.numeric(LowerWill[1,1])<=30){
                    LowerWill[1,2] <- 1
                  }
                  else if(as.numeric(LowerWill[1,1])>30 && as.numeric(LowerWill[1,1])<31){
                    LowerWill[1,2] <- 1 - ((as.numeric(LowerWill[1,1])-30)/1)*(1-0.6)
                  }
                  else if(as.numeric(LowerWill[1,1])>=31 && as.numeric(LowerWill[1,1])<=40){
                    LowerWill[1,2] <- 0.6
                  }
                  else if(as.numeric(LowerWill[1,1])>40 && as.numeric(LowerWill[1,1])<41){
                    LowerWill[1,2] <- 0.6 - ((as.numeric(LowerWill[1,1])-40)/1)*(0.6-0.2)
                  }
                  else if(as.numeric(LowerWill[1,1])>=41 && as.numeric(LowerWill[1,1])<=80){
                    LowerWill[1,2] <- 0.2
                  }
                  else if(as.numeric(LowerWill[1,1])>80 && as.numeric(LowerWill[1,1])<81){
                    LowerWill[1,2] <- 0.2 - ((as.numeric(LowerWill[1,1])-80)/1)*(0.2-0.1)
                  }
                  else if(as.numeric(LowerWill[1,1])>=81){
                    LowerWill[1,2] <- 0.1
                  }
                }
                if(!is.na(LowerWill[2,1])){
                  if(as.numeric(LowerWill[2,1])<=0.5){
                    LowerWill[2,2] <- 0.5
                  }
                  else if(as.numeric(LowerWill[2,1])>0.5 && as.numeric(LowerWill[2,1])<0.6){
                    LowerWill[2,2] <- 0.5 + ((as.numeric(LowerWill[2,1])-0.5)/0.1)*(1-0.5)
                  }
                  else if(as.numeric(LowerWill[2,1])>=0.6 && as.numeric(LowerWill[2,1])<=3){
                    LowerWill[2,2] <- 0.1
                  }
                  else if(as.numeric(LowerWill[2,1])>3 && as.numeric(LowerWill[2,1])<3.1){
                    LowerWill[2,2] <- 1 - ((as.numeric(LowerWill[2,1])-3)/0.1)*(1-0.6)
                  }
                  else if(as.numeric(LowerWill[2,1])>=3.1 && as.numeric(LowerWill[2,1])<=10){
                    LowerWill[2,2] <- 0.6
                  }
                  else if(as.numeric(LowerWill[2,1])>10){
                    LowerWill[2,2] <- 0
                  }
                }
                if(!is.na(LowerWill[3,1])){
                  if(LowerWill[3,1]=="Bedrock"){
                    LowerWill[3,2] <- 0.1
                  }
                  else if(LowerWill[3,1]=="Riprap"){
                    LowerWill[3,2] <- 0.1 + ((as.numeric(LowerWill[3,1])-0)/10)*(0.2-0.1)
                  }
                  else if(LowerWill[3,1]=="Sand"){
                    LowerWill[3,2] <- 0.2 + ((as.numeric(LowerWill[3,1])-10)/10)*(0.4-0.2)
                  }
                  else if(LowerWill[3,1]=="Fines"){
                    LowerWill[3,2] <- 0.4 + ((as.numeric(LowerWill[3,1])-20)/10)*(0.8-0.4)
                  }
                }
                if(!is.na(LowerWill[3,2]) && !is.na(LowerWill[2,2]) && !is.na(LowerWill[1,2])){
                  LowerWill[1,3] <- (LowerWill[1,2] + LowerWill[2,2] + LowerWill[3,2])/3
                  LowerWill[1,4] <- LowerWill[1,3] * LowerWill_input[1,2]
                }
              }
            }
      }
    }
    else{
      LowerWill_input <- values[["LowerWill_input"]]
      variables <- 6
            LowerWill <- data.frame(
                                    Quantity=rep(NA_character_,variables),
                                    SI=rep(NA_real_,variables),
                                    HSI=rep(NA_real_,variables),
                                    HU=rep(NA_real_,variables))
    }
    values[["LowerWill"]] <- LowerWill
    
    
    #Calculations and table outputs dependent on user inputs
    rows<-0
    if(is.null(values[["criteria_models"]])){ #Setup tables
      load('RiparianModels.rda') #load data from file with models
      criteria_models <- RiparianModels[1:14,1:20] #populate tables
      rows<-nrow(criteria_models)
    }
    else{ #if there are user inputs, order the models
      criteria_models <- values[["criteria_models"]]
      rows<-nrow(criteria_models)
      if(!is.null(input$region) || !is.null(input$model_type) || !is.null(input$instream_processes) || !is.null(input$riparian_zone_processes)){ #compare user inputs with models to assign value to each model, respectively
        model_value<-data.frame(matrix(ncol=2,nrow=(rows)))
        model_value[,1]<-0
        model_value[,2]<-criteria_models["Model"]
        colnames(model_value)<-c("value","model")
        for(i in 1:(rows)){ 
          if(!is.null(input$region) && input$region!='None'){
            count <- ncol(input$region)
              if(input$region == 'Varying'){
                if(input$region %in% criteria_models[[i,paste0("Region of Application")]] || grepl(pattern="/",criteria_models[[i,paste0("Region of Application")]])==TRUE){
                  model_value[i,1] = model_value[i,1] + 0.5
                }
                if(criteria_models[[i,paste0("Region of Application")]] %in% input$region){
                  model_value[i,1] = model_value[i,1] + 1
                }
                else if(criteria_models[[i,paste0("Region of Application")]]=='Varying'){
                  model_value[i,1] = model_value[i,1] + 1
                }
              }
            else{
                if(criteria_models[[i,paste0("Region of Application")]]=='Varying'){
                  model_value[i,1] = model_value[i,1] + 0.25
                }
                else if(grepl(pattern="/",criteria_models[[i,paste0("Region of Application")]])==TRUE){
                  terms <- sapply(strsplit(criteria_models[[i,paste0("Region of Application")]],"/"),"[")
                  count <- length(terms)
                  for(j in 1:count){
                    if(terms[j] %in% input$region){
                      model_value[i,1] = model_value[i,1] + 1
                    }
                  }
                }
              else{
                if(criteria_models[[i,paste0("Region of Application")]] %in% input$region){
                  model_value[i,1] = model_value[i,1] + 1
                }
                else if(criteria_models[[i,paste0("Region of Application")]]=='Varying'){
                  model_value[i,1] = model_value[i,1] + 0.25
                }
              }
            }
          }
          if(!is.null(input$model_type) && input$model_type!='None'){
            if(grepl(pattern="/",criteria_models[[i,paste0("Model Type")]])==TRUE){
              terms <- sapply(strsplit(criteria_models[[i,paste0("Model Type")]],"/"),"[")
              count <- length(terms)
              for(j in 1:count){
                if(terms[j] %in% input$model_type){
                  model_value[i,1] = model_value[i,1] + 1
                }
              }
            }
            else{
              if(input$model_type %in% criteria_models[[i,paste0("Model Type")]]){
                model_value[i,1] = model_value[i,1] + 1
              }
            }
          }
          instr<-input$instream_processes
          if(!is.null(instr) && !grepl('None',paste0(instr,sep="", collapse="|"))){
            for(j in 1:length(instr)){
              if(!is.na(criteria_models[i,paste0(instr[j])])){
                if(criteria_models[i,paste0(instr[j])]>1)
                  model_value[i,1] = model_value[i,1] + 1.5
                else
                  model_value[i,1] = model_value[i,1] + 1
              }
            }
          }
          rip<-input$riparian_zone_processes
          if(!is.null(rip) && !grepl('None',paste0(rip,sep="", collapse="|"))){
            for(j in 1:length(rip)){
              if(!is.na(criteria_models[i,paste0(rip[j])])){
                if(criteria_models[i,paste0(rip[j])]>1)
                  model_value[i,1] = model_value[i,1] + 1.5
                else
                  model_value[i,1] = model_value[i,1] + 1
              }
            }
          }
        }
      }
      
      # Order the models according to the assigned values
      if(input$region!='None' || input$model_type!='None' || !is.null(input$instream_processes) || !is.null(input$riparian_zone_processes)){
        temp_criteria <- criteria_models
        ordered <- model_value[order(model_value$value),] #order models by value
        for(j in 1:rows){
          k <- rows-j+1
          for(i in 1:rows){ #switch model order in dataframe
            if(ordered[k,2]==criteria_models[i,"Model"]){
              temp_criteria[j,] <- criteria_models[i,]
              i <- rows
            }
          }
        }
        ordered_models <- temp_criteria
        values[["ordered_models"]] <- ordered_models
      }
      
      # Order the models according to whether the search term appears or not
      if(input$keyword!="" && !is.null(input$keyword)){
        search_value<-data.frame(matrix(ncol=3,nrow=(rows)))
        search_value[,1]<-0
        search_value[,2]<-criteria_models["Model"]
        search_value[,3]<-0
        colnames(search_value)<-c("value","model","freq")
        for(i in 1:rows){
          for(j in 1:ncol(criteria_models)){
          if(grepl(input$keyword,criteria_models[i,j],ignore.case=TRUE)==TRUE){
              search_value[i,1] <- 1
              search_value[i,3] <- search_value[i,3] + 1
              j<-ncol(criteria_models)
            }
          }
        }
        
        if(sum(sapply(search_value[,1],'%in%',x=0))<rows){
          cut <- NULL
          search_hits<-which(sapply(search_value[,1],'%in%',x=1))
          ordered <- search_value[order(search_value$freq),] #order models by value
          if(sum(sapply(search_value[,1],'%in%',x=0))==rows-1)
            cut <- ordered[rows,]
          else
            cut <- ordered[(sum(sapply(search_value[,1],'%in%',x=0))+1):rows,]
          temp_criteria <- data.frame(matrix(nrow=nrow(cut),ncol=ncol(criteria_models)))
          colnames(temp_criteria) <- colnames(criteria_models)
          for(j in 1:nrow(cut)){
            k <- nrow(cut)-j+1
            for(i in 1:rows){ #switch model order in dataframe
              if(cut[j,2]==criteria_models[i,"Model"]){
                temp_criteria[j,] <- criteria_models[i,]
                i <- rows
              }
            }
          }
          values[["keyword_models"]] <- temp_criteria
        }
      }
    }
    values[["criteria_models"]] <- criteria_models

    if(!is.null(input$UpperMiss_single)){
      single_input <- hot_to_r(input$UpperMiss_single)
      canopy_index <- 0
      if(!is.na(single_input[1,1])){
        if(single_input[1,1]>=0 && single_input[1,1]<20)
          canopy_index <- 0 + (0.005*single_input[1,1])
        else if(single_input[1,1]>=20 && single_input[1,1]<40)
          canopy_index <- -0.2 + (0.015*single_input[1,1])
        else if(single_input[1,1]>=40 && single_input[1,1]<70)
          canopy_index <- -0.4 + (0.02*single_input[1,1])
        else if(single_input[1,1]>=70 && single_input[1,1]<80)
          canopy_index <- 1 + (0*single_input[1,1])
        else if(single_input[1,1]>=80 && single_input[1,1]<90)
          canopy_index <- 2.6 + (-0.02*single_input[1,1])
        else if(single_input[1,1]>=90)
          canopy_index <- 0.8 + (0.8*single_input[1,1])
      }
      forest_index <- 0
      if(!is.na(single_input[2,1])){
        if(single_input[2,1]>=0 && single_input[2,1]<20)
          forest_index <- 0 + (0.005*single_input[2,1])
        else if(single_input[2,1]>=20 && single_input[2,1]<40)
          forest_index <- -0.1 + (0.01*single_input[2,1])
        else if(single_input[2,1]>=40 && single_input[2,1]<60)
          forest_index <- -0.3 + (0.15*single_input[2,1])
        else if(single_input[2,1]>=60 && single_input[2,1]<75)
          forest_index <- -0.2 + (0.0133*single_input[2,1])
        else if(single_input[2,1]>=75 && single_input[2,1]<100)
          forest_index <- 0.2 + (0.008*single_input[2,1])
        else if(single_input[2,1]==100)
          forest_index <- 0 + (0.01*single_input[2,1])
      }
      invasive_index <- 0
      if(!is.na(single_input[3,1])){
        if(single_input[3,1]>=0 && single_input[3,1]<10)
          invasive_index <- 1 + (-0.01*single_input[3,1])
        else if(single_input[3,1]>=10 && single_input[3,1]<20)
          invasive_index <- 1.5 + (-0.06*single_input[3,1])
        else if(single_input[3,1]>=20 && single_input[3,1]<40)
          invasive_index <- 0.5 + (-0.01*single_input[3,1])
        else if(single_input[3,1]>=40 && single_input[3,1]<50)
          invasive_index <- 0.3 + (-0.005*single_input[3,1])
        else if(single_input[3,1]>=50 && single_input[3,1]<75)
          invasive_index <- 0.13 + (-0.0016*single_input[3,1])
        else if(single_input[3,1]>=75 && single_input[3,1]<100)
          invasive_index <- 0.04 + (-0.0004*single_input[3,1])
        else if(single_input[3,1]==100)
          invasive_index <- 0
      }
      regen_index <- 0
      if(!is.na(single_input[4,1])){
        if(single_input[4,1]>=0 && single_input[4,1]<20)
          regen_index <- 0 + (0.005*single_input[4,1])
        else if(single_input[4,1]>=20 && single_input[4,1]<40)
          regen_index <- -0.3 + (0.02*single_input[4,1])
        else if(single_input[4,1]>=40 && single_input[4,1]<80)
          regen_index <- 0.1 + (0.01*single_input[4,1])
        else if(single_input[4,1]>=80 && single_input[4,1]<100)
          regen_index <- 0.5 + (0.005*single_input[4,1])
        else if(single_input[4,1]==100)
          regen_index <- 0 + (0.01*single_input[4,1])
      }
      UpperMiss_single <- single_input  
      UpperMiss_single[11,2] <- (canopy_index + forest_index + invasive_index + regen_index + (UpperMiss_single[5,2]+UpperMiss_single[6,2]+UpperMiss_single[7,2]+UpperMiss_single[8,2]+UpperMiss_single[9,2])/5)/5
      if(!is.na(UpperMiss_single[10,1])){
        UpperMiss_single[12,2] <- UpperMiss_single[11,2] * UpperMiss_single[10,1]
      }
    }
    else{
      UpperMiss_single <- data.frame(index=rep(NA_integer_,12),structure=rep(NA_real_,12))
    }
    values[["UpperMiss_single"]] <- UpperMiss_single
  })
  
  v <- reactiveValues(data = NULL)
  
  
  #########################################################################################################################
  # Function: criteria_models -renderRHandsontable-
  # Output: prints models in order for user inputs
  #########################################################################################################################
  
  output$criteria_models <- renderRHandsontable({
    DF <- values[["criteria_models"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE, 
                    colHeaders = colnames(DF),
                    readOnly = TRUE
      )
    }
  })
    
  
  #########################################################################################################################
  # Function: criteria_models -renderRHandsontable-
  # Output: prints models in order for user inputs
  #########################################################################################################################
  
  output$ordered_models <- renderRHandsontable({
    DF <- values[["ordered_models"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE, 
                    colHeaders = colnames(DF),
                    readOnly = TRUE
      )
    }
  })
  
  
    #########################################################################################################################
    # Function: keyword_models -renderRHandsontable-
    # Output: prints models in order for user inputs
    #########################################################################################################################

    output$keyword_models <- renderRHandsontable({
      DF <- values[["keyword_models"]]
      if (!is.null(DF)){
        rhandsontable(DF, useTypes = TRUE,
                      colHeaders = colnames(DF),
                      readOnly = TRUE
        )
      }
  })

  
  #########################################################################################################################
  # Function: Bosque -renderRHandsontable-
  # Output: prints inputs and outputs 
  #########################################################################################################################
  
  output$Bosque <- renderRHandsontable({
    DF <- values[["Bosque"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE,
                    rowHeaders = c("CODE TYPE (1-6)","DEPTHGW","WETTEDAREA","FLOODEDFREQ","DURATION",
                                   "CANTREE","CANSHRUB","CANHERB","DISTBIGTR",
                                   "NATIVETREE","INDICATHB","SPPCOUNT","COVGRIND",
                                   "CTGRNDCOV","DEPTHOM",
                                   "CANGRASS","CANFORB","CANSEDGE","INDICATGR",
                                   "INDICATFB","NATIVESDG","SPPCOUNT",
                                   "PATCHSIZE","TYPDISTURB","DISPATCH","HSI"),
                    colHeaders = c("Variables"),
                    rowHeaderWidth = 250,
                    stretchH = "all"
      ) %>%
        hot_cell(row=26,col=1,readOnly=TRUE)
    }
  })
  
  
  #########################################################################################################################
  # Function: Cottonwood -renderRHandsontable-
  # Output: prints inputs and outputs 
  #########################################################################################################################
  
  output$Cottonwood <- renderRHandsontable({
    DF <- values[["Cottonwood"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE,
                    rowHeaders = c("Age (yrs)","LCPI","DEPTHGW","RICHNATIVE",
                                   "CVALUE","WIS","CANHERB",
                                   "CANSHRUB","ADJLANDUSE","PATCHSIZE",
                                   "DISPATCH","PROPCTW",
                                   "RECRUIT","INTERPERS","ERI"),
                    colHeaders = c("Variables"),
                    rowHeaderWidth = 250,
                    stretchH = "all"
      ) %>%
        hot_cell(row=15,col=1,readOnly=TRUE)
    }
  })
  
  
  #########################################################################################################################
  # Function: Chatfield -renderRHandsontable-
  # Output: prints inputs and outputs 
  #########################################################################################################################
  
  output$Chatfield <- renderRHandsontable({
    DF <- values[["Chatfield"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE,
                    rowHeaders = c("SV 1.1 Total Volume","SV 1.2 Peak Flows","SV 1.3 Base Flows", #33 rows
                                   "SV 1.4 Flow Variability","SV 2.1 Land Erosion","SV 2.2 Channel Erosion",
                                   "SV 2.3 Transport", "SV 3.1 Temperature Regime","SV 3.2 Organics / Nutrients",
                                   "SV 3.3 Inorganics / Toxins","SV 4.1 Saturation Frequency","SV 4.2 Floodplain Width",
                                   "SV 4.3 Saturation Duration","SV 5.1 Woody Vegetation Structure",
                                   "SV 5.2 Herbaceous Vegetation Structure", "SV 5.3 Species Diversity",
                                   "SV 6.1 Large Woody Debris","SV 6.2 Detritus",
                                   "SV 7.1 Evolutionary Stage","SV 7.2 Planform","SV 7.3 Dimension",
                                   "SV 7.4 Profile","SV 8.1 Dynamic Equilibrium","SV 8.2 Resilience",
                                   "SV 9.1 Hydraulic Structure","SV 9.2 Coarse Scale","SV 9.3 Fine Scale",
                                   "SV 10.1 Microbes","SV 10.2 Macrophytes","SV 10.3 Macroinvertebrates","SV 10.4 Fish and Amphibians","SV 10.5 Other Animals",
                                   "Hydraulic Functional Condition Score","Geomorphic Functional Condition Score",
                                   "Physiochemical Functional Condition Score","Biotic Functional Condition Score",
                                   "Overall Functional Condition Score"),
                    colHeaders = c("SV Score"),
                    rowHeaderWidth = 250,
                    stretchH = "all"
                    ) %>%
        hot_row(c(33,34,35,36,37), readOnly = TRUE)
    }
  })
  
  
  #########################################################################################################################
  # Function: Mink_input -renderRHandsontable-
  # Output: prints inputs and outputs 
  #########################################################################################################################
  
  output$Mink_input<- renderRHandsontable({
    DF <- values[["Mink_input"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE,
                    rowHeaders = c("Year","Area (Hectares)",
                                   "SIV1","SIV2","SIV3","SIV4","SIV5","SIV6",
                                   "SIFS1","SIFS2","SIRL","SIPE","HSI"),
                    colHeaders = c("Data","HSI"),
                    rowHeaderWidth = 250,
                    stretchH = "all"
                    # )
      ) %>%
        hot_cell(1,"HSI", readOnly = TRUE) %>%
        hot_cell(2,"HSI", readOnly = TRUE) %>%
        hot_cell(9,"Data", readOnly = TRUE) %>%
        hot_cell(10,"Data", readOnly = TRUE) %>%
        hot_cell(11,"Data", readOnly = TRUE) %>%
        hot_cell(12,"Data", readOnly = TRUE) %>%
        hot_cell(13,"Data", readOnly = TRUE) %>%
        hot_col(col=2, readOnly = TRUE, copyable = TRUE) %>%
        hot_col(col="Data", renderer = "
    function(instance, td, row, col, prop, value, cellProperties){
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      if(row==8 || row==9|| row==10 || row==11 || row==12){
        td.style.background = 'grey'
      }
      return td;
      }") %>%
        hot_col(col="HSI", renderer = "
    function(instance, td, row, col, prop, value, cellProperties){
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      if(row==0 || row==1){
        td.style.background = 'grey'
      }
      return td;
      }")
    }
  })
  
  
  #########################################################################################################################
  # Function: criteria_models -renderRHandsontable-
  # Output: prints models in order for user inputs
  #########################################################################################################################
  
  output$UpperMiss_single <- renderRHandsontable({
    DF <- values[["UpperMiss_single"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE, 
                    colHeaders = c("Data (0-100)","Score (0-1)"),
                    rowHeaders = c("Percent Canopy Cover",
                                   "Percent Desired Forest Type",
                                   "Percent Invasive Species",
                                   "Regeneration",
                                   "Horizontal Structural Diversity",
                                   "Vertical Structural Diversity",
                                   "Size Class Diversity",
                                   "Standing Dead Wood",
                                   "Tree Species Diversity",
                                   "Area (Units)",
                                   "HSI",
                                   "HU"
                    ),
                    rowHeaderWidth = 250,
                    stretchH = "all"
      ) %>%
        hot_row(row=11, readOnly = TRUE) %>%
        hot_row(row=12, readOnly = TRUE) %>%
        # hot_col(col="Data (0-100)", format="0%") %>%
        hot_col(col="Data (0-100)", renderer = "
    function(instance, td, row, col, prop, value, cellProperties){
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      if(row==4 || row==5 || row==6 || row==7 || row==8 || row==10 || row==11){
        td.style.background = 'grey'
      }
      return td;
    }") %>%
        hot_col(col="Score (0-1)", renderer = "
    function(instance, td, row, col, prop, value, cellProperties){
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      if(row==0 || row==1 || row==2 || row==3 || row==9){
        td.style.background = 'grey'
      }
      return td;
    }")
        }
  })
  
  #########################################################################################################################
  # Function: Resaca_single_metric -renderRHandsontable-
  # Output: prints inputs and outputs for single input
  #########################################################################################################################
  
  output$Resaca_single_metric <- renderRHandsontable({
    DF <- values[["Resaca_single_metric"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE, 
                    rowHeaders = c("Enter Metric Input","SI"),
                    colHeaders = c("Slope 1:X",
                                   "Percent Bank Canopy Cover",
                                   "Habitat",
                                   "Species Composition",
                                   "Richness",
                                   "Percent Riparian Canopy Cover",
                                   "Percent Aquatic Canopy Cover",
                                   "Percent Invasives",
                                   "Water Regime",
                                   "Water Depth (ft)",
                                   "Resaca Reference Condition Index"
                    ),
                    rowHeaderWidth = 150,
                    height = 200,
                    stretchH = "all"
      ) %>%
        hot_col(col="Habitat", allowInvalid = FALSE, type = "dropdown", source = c(NA_character_,"Ebony/Snake-eyes Shrubland","Subtropical Texas Palmetto Woodland","Texas Ebony Resaca Forest"), readOnly = FALSE) %>%
        hot_col(col="Water Regime", allowInvalid = FALSE, type = "dropdown", source = c(NA_character_,"Permanent/Connected","Semipermanent/Disconnected","Dry"), readOnly = FALSE) %>%
        hot_col(col="Habitat", renderer = "
    function(instance, td, row, col, prop, value, cellProperties){
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      if(row==1){
        td.style.background = 'black'
      }
      return td;
      }") %>%
        hot_col(col="Species Composition", renderer = "
    function(instance, td, row, col, prop, value, cellProperties){
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      if(row==0){
        td.style.background = 'black'
      }
      return td;
      }") %>%
        hot_col(col="Richness", renderer = "
    function(instance, td, row, col, prop, value, cellProperties){
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      if(row==0){
        td.style.background = 'black'
      }
      return td;
      }") %>%
        hot_col(col="Resaca Reference Condition Index", renderer = "
    function(instance, td, row, col, prop, value, cellProperties){
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      if(row==0){
        td.style.background = 'black'
      }
      if(row==1){
        td.style.background = 'green'
      }
      return td;
      }")
    }
  })
  
  #########################################################################################################################
  # Function: Resaca_single_species -renderRHandsontable-
  # Output: prints inputs and outputs for single input
  #########################################################################################################################
  
  output$Resaca_single_species <- renderRHandsontable({
    DF <- values[["Resaca_single_species"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE, 
                    colHeaders = c("% Composition",
                                   "Common Name",
                                   "Scientific Name",
                                   "TERF Min",
                                   "TERF Max",
                                   "TERF SSI",
                                   "TERF Rich",
                                   "STPW Min",
                                   "STPW Max",
                                   "STPW SSI",
                                   "STPW Rich",
                                   "TESES Min",
                                   "TESES Max",
                                   "TESES SSI",
                                   "TESES Rich"
                    ),
                    rowHeaders = TRUE,
                    # rowHeaderWidth = 150,
                    stretchH = "all"
      # )
      ) %>%
        hot_col(col="Common Name", readOnly = TRUE) %>%
        hot_col(col="Scientific Name", readOnly = TRUE) %>%
        # hot_col(col="TERF Min", readOnly = TRUE) %>%
        # hot_col(col="TERF Max", readOnly = TRUE) %>%
        # hot_col(col="TERF SSI", readOnly = TRUE) %>%
        # hot_col(col="TERF Rich", readOnly = TRUE) %>%
        # hot_col(col="STPW Min", readOnly = TRUE) %>%
        # hot_col(col="STPW Max", readOnly = TRUE) %>%
        # hot_col(col="STPW SSI", readOnly = TRUE) %>%
        # hot_col(col="STPW Rich", readOnly = TRUE) %>%
        # hot_col(col="TESES Min", readOnly = TRUE) %>%
        # hot_col(col="TESES Max", readOnly = TRUE) %>%
        # hot_col(col="TESES SSI", readOnly = TRUE) %>%
        # hot_col(col="TESES Rich", readOnly = TRUE) %>%
          hot_col(col="% Composition", renderer = "
      function(instance, td, row, col, prop, value, cellProperties){
        Handsontable.renderers.TextRenderer.apply(this, arguments);
          td.style.background = 'yellow'
        return td;
      }")
    }
  })
  
  
  #########################################################################################################################
  # Function: Skokomish_inputs -renderRHandsontable-
  # Output: prints inputs and outputs 
  #########################################################################################################################
  
  output$Skokomish_input <- renderRHandsontable({
    DF <- values[["Skokomish_input"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE, 
                    colHeaders = c("Assessment Area Limiting Factor(s)"),
                    rowHeaders = NULL,
                    height = 200,
                    stretchH = "all"
      ) %>%
      hot_col(col="Assessment Area Limiting Factor(s)", allowInvalid = FALSE, type = "dropdown",
              source = c(NA_character_,"In-Channel Habitat",
                         "Floodplain Habitat","Channel Capacity and In-Channel Habitat"), readOnly = FALSE)
    }
  })
  
  
  #########################################################################################################################
  # Function: Skokomish-renderRHandsontable-
  # Output: prints inputs and outputs 
  #########################################################################################################################
  
  output$Skokomish <- renderRHandsontable({
    DF <- values[["Skokomish"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE, 
                    rowHeaders = c("Affected Acres", "V1 - Woody",
                                   "V2 - Pools", "V3 - Connectivity",
                                   "V4 - Riparian Cover", "V5 - Capacity",
                                   "HQI", "HU"),
                    colHeaders = NULL,
                    rowHeaderWidth = 250,
                    stretchH = "all"
      ) %>%
        hot_row(row=7, readOnly = TRUE) %>%
        hot_row(row=8, readOnly = TRUE)
    }
  })
      
      #########################################################################################################################
      # Function: LowerWill_input -renderRHandsontable-
      # Output: prints inputs and outputs 
      #########################################################################################################################
      
      output$LowerWill_input <- renderRHandsontable({
        DF <- values[["LowerWill_input"]]
        if (!is.null(DF)){
          rhandsontable(DF, useTypes = TRUE,
                        colHeaders = c("Variable Selection",
                                       "Acreage"),
                        rowHeaders = NULL,
                        height = 200,
                        stretchH = "all"
          ) %>%
          hot_col(col="Variable Selection", allowInvalid = FALSE, type = "dropdown",
                  source = c(NA_character_,"Western Pond Turtle","Beaver",
                             "Wood Duck","Yellow Warbler","Native Amphibians",
                             "Salmonids Trbutary - Prespawning Adults",
                             "Salmonids Trbutary - Juvenile",
                             "Native Salmonids Mainstem"), readOnly = FALSE)
    }
  })
      
      #########################################################################################################################
      # Function: LowerWill -renderRHandsontable-
      # Output: prints inputs and outputs 
      #########################################################################################################################
      
      output$LowerWill <- renderRHandsontable({
        DF <- values[["LowerWill"]]
        if (!is.null(DF)){
          rhandsontable(DF, useTypes = TRUE,
                        rowHeaders = c("V1","V2","V3","V4","V5","V6"),
                        colHeaders = c("Quantity","SI",
                                       "HSI","HU"),
                        stretchH = "all"
          ) %>%
            hot_col(col="SI", readOnly = TRUE, copyable = TRUE) %>%
            hot_col(col="HSI", readOnly = TRUE, copyable = TRUE) %>%
            hot_col(col="HU", readOnly = TRUE, copyable = TRUE)
        }
      })
  
  #Import and return user input for UpperMiss modules
  UpperMiss_HSI_inputs <- reactive({
    req(input$UpperMiss_HSI_inputs) #default is NULL so this waits until 1st file is uploaded for the code to run
    ext <- tools::file_ext(input$UpperMiss_HSI_inputs$name)
    switch(ext,
           csv = vroom::vroom(input$UpperMiss_HSI_inputs$datapath, delim = ","), #datapath is the path to where the data has been uploaded
           validate("Invalid file; Please upload a .csv file") #add error message if the user puts another type of file
    )
  })
  
  UpperMiss_structural_inputs <- reactive({
    req(input$UpperMiss_structural_inputs) #default is NULL so this waits until 1st file is uploaded for the code to run
    ext <- tools::file_ext(input$UpperMiss_structural_inputs$name)
    switch(ext,
           csv = vroom::vroom(input$UpperMiss_structural_inputs$datapath, delim = ","), #datapath is the path to where the data has been uploaded
           validate("Invalid file; Please upload a .csv file") #add error message if the user puts another type of file
    )
  })
  
  output$UpperMiss_multi <- renderRHandsontable({
    DF <- values[["UpperMiss_multi"]]
    if (!is.null(DF)){
      rhandsontable(DF, useTypes = TRUE, 
                    colHeaders = c("Final Score"),
                    stretchH = "all",
                    readOnly = TRUE,
                    copyable = TRUE,
                    align = center
      ) 
    }
  })
  
  
  #edit custom UI/text on the page 
  #https://stackoverflow.com/questions/33392784/make-bold-text-in-html-output-r-shiny
  # for error handling
  output$FileText1 <- renderText({
    HTML(paste0("<u><b><h2 style=color:black;font-size:20px;>","Multiple Comparison Inputs","</b></u></h2>"))
  })
  
  output$InstreamText1 <- renderText({
    HTML(paste0("<u><b><h2 style=color:black;font-size:20px;>","Instream Inputs","</b></u></h2>"))
  })
  
  output$FaunaText1 <- renderText({
    HTML(paste0("<u><b><h2 style=color:black;font-size:20px;>","Fauna Inputs","</b></u></h2>"))
  })
  
  output$CorridorText1 <- renderText({
    HTML(paste0("<u><b><h2 style=color:black;font-size:20px;>","Corridor Inputs","</b></u></h2>"))
  })
  
  output$AreaText1 <- renderText({
    HTML(paste0("<u><b><h2 style=color:black;font-size:20px;>","Area Inputs","</b></u></h2>"))
  })
  
  output$OutputsText1 <- renderText({
    HTML(paste0("<u><b><h2 style=color:black;font-size:20px;>","Single Calculation Outputs","</b></u></h2>"))
  })
  
  output$OutputsText2 <- renderText({
    HTML(paste0("<u><b><h2 style=color:black;font-size:20px;>","Multiple Comparison Outputs","</b></u></h2>"))
  })
  
  #Import and return user input for instream module
  instream_inputs <- reactive({
    req(input$instream_inputs) #default is NULL so this waits until 1st file is uploaded for the code to run
    ext <- tools::file_ext(input$instream_inputs$name)
    switch(ext,
           csv = vroom::vroom(input$instream_inputs$datapath, delim = ","), #datapath is the path to where the data has been uploaded
           validate("Invalid file; Please upload a .csv file") #add error message if the user puts another type of file
    )
  })
  
  #Import and return user input curves for fauna module
  fauna_inputs <- reactive({
    req(input$fauna_inputs) #default is NULL so this waits until 1st file is uploaded for the code to run
    ext <- tools::file_ext(input$fauna_inputs$name)
    switch(ext,
           csv = vroom::vroom(input$fauna_inputs$datapath, delim = ","), #datapath is the path to where the data has been uploaded
           validate("Invalid file; Please upload a .csv file") #add error message if the user puts another type of file
    )
  })
  
  #Import and return user input curves for corridor module
  corridor_inputs <- reactive({
    req(input$corridor_inputs) #default is NULL so this waits until 1st file is uploaded for the code to run
    ext <- tools::file_ext(input$corridor_inputs$name)
    switch(ext,
           csv = vroom::vroom(input$corridor_inputs$datapath, delim = ","), #datapath is the path to where the data has been uploaded
           validate("Invalid file; Please upload a .csv file") #add error message if the user puts another type of file
    )
  })
  
  #Import and return suitability index curves for area
  area_inputs <- reactive({
    req(input$area_inputs) #default is NULL so this waits until 1st file is uploaded for the code to run
    ext <- tools::file_ext(input$area_inputs$name)
    switch(ext,
           csv = vroom::vroom(input$area_inputs$datapath, delim = ","), #datapath is the path to where the data has been uploaded
           validate("Invalid file; Please upload a .csv file") #add error message if the user puts another type of file
    )
  })
  
  SMURF_single <- reactive({
    load('SMURF_instream.rda')
    load('SMURF_fauna.rda')
    load('SMURF_corridor.rda')
    instream.names <- c("hyd.att", "stripwidth.ft", "flowpath.score", "shading.ratio", 
                        "cancov.score", "canstr.score", "carbret.score")
    
    fauna.names <- c("canstr.score", "deadfall.score", "snag.score", "batcan.score",
                     "embed.score", "detritus.score", "herb.score", "inv.veg.score")
    
    corridor.names <- c("buffer.dev.Score", "edge.density.perft", "corridorwidth.ft", "corridormin.ft")
    
    #Create empty matrices to store suitability outputs
    SI.instream <- c(); SI.fauna <- c(); SI.corridor <- c()
    
    #Fill site inputs with the user inputs
    #site.instream: hyd.att, stripwidth.ft, flowpath.score, shading.ratio, cancov.score, canstr.score, and carbret.score (7 total)
    site.instream <- c(input$hyd.att,input$stripwidth.ft,input$flowpath.score,input$shading.ratio,input$cancov.score,input$canstr.score,input$carbret.score)
    #site.fauna: canstr.score, deadfall.score, snag.score, batcan.score, embed.score, detritus.score, herb.score, and inv.veg.score (8 total)
    site.fauna <- c(input$canstr.score.fauna, input$deadfall.score, input$snag.score, input$batcan.score, input$embed.score, input$detritus.score, input$herb.score, input$inv.veg.score)
    #site.corridor: buffer.dev.Score, edge.density.perft, corridorwidth.ft, and corridormin.ft (4 total)
    site.corridor<- c(input$buffer.dev.Score, input$edge.density.perft, input$corridorwidth.ft, input$corridormin.ft)
    #site.area: area.input (1 total)
    site.area <- c(input$area.input)
    
    #Calculate suitability indices for each input variable and module using SIcalc( ) from the ecorest package
    SI.instream <- SIcalc(data.frame(lapply(SMURF_instream,as.numeric)), site.instream)
    SI.fauna <- SIcalc(data.frame(lapply(SMURF_fauna,as.numeric)), site.fauna)
    SI.corridor <- SIcalc(data.frame(lapply(SMURF_corridor,as.numeric)), site.corridor)
    
    #Create empty data frame to store outputs (Instream SI, Habitat SI, Corridor SI, HSI, Area, Habitat Units)
    SMURF.out <- as.data.frame(matrix(NA, nrow = 1, ncol = 6))
    colnames(SMURF.out) <- c("Instream.SI", "Fauna.SI", "Corridor.SI", "HSI", "Area", "HU")
    #If any input is NA, return NA
    if (sum(is.na(c(site.instream,site.fauna,site.corridor))) > 0){
      SMURF.out$Instream.SI <- NA
      SMURF.out$Fauna.SI <- NA
      SMURF.out$Corridor.SI <- NA
      SMURF.out$HSI <- NA
      SMURF.out$Area <- NA
      SMURF.out$HU <- NA
    }
    
    #Else compute all other outputs
    else{
      #Compute module-specific habitat suitability indices using HSIarimean( ) from the ecorest package - ARITHMETIC MEAN
      SMURF.out$Instream.SI <- round(HSIarimean(SI.instream), digits=3)
      SMURF.out$Fauna.SI <- round(HSIarimean(SI.fauna), digits=3)
      SMURF.out$Corridor.SI <- round(HSIarimean(SI.corridor), digits=3)
      
      #Compute overarching habitat suitability index and habitat units
      SMURF.out$HSI <- round((SMURF.out$Instream.SI * SMURF.out$Fauna.SI * SMURF.out$Corridor.SI) ^ (1/3), digits=3)
      SMURF.out$Area <- round(site.area, digits=3)
      SMURF.out$HU <- round(SMURF.out$HSI * SMURF.out$Area, digits=3)
      colnames(SMURF.out) <- c("Instream Suitability", "Fauna Suitability", "Corridor Suitability", "Overall Suitability", "Area", "Habitat Units")
    }
    #Send output from function
    return(SMURF.out)
  })
  
  output$SMURF_single<-renderDataTable({
    datatable(
      t(SMURF_single()),
      colnames=NULL,
      options=list(paging=FALSE,
                   searching=FALSE,
                   paging=FALSE)
    )
  })
  
  SMURF_multi<- reactive({
    load('SMURF_instream.rda')
    load('SMURF_fauna.rda')
    load('SMURF_corridor.rda')
    instream.names <- c("hyd.att", "stripwidth.ft", "flowpath.score", "shading.ratio", 
                        "cancov.score", "canstr.score", "carbret.score")
    
    fauna.names <- c("canstr.score", "deadfall.score", "snag.score", "batcan.score",
                     "embed.score", "detritus.score", "herb.score", "inv.veg.score")
    
    corridor.names <- c("buffer.dev.score", "edge.density.perft", "corridorwidth.ft", "corridormin.ft")
    
    #Fill site inputs with the user inputs
    site.instream <- instream_inputs()
    colnames(site.instream) <- instream.names
    #site.fauna: canstr.score, deadfall.score, snag.score, batcan.score, embed.score, detritus.score, herb.score, and inv.veg.score (8 total)
    site.fauna <- fauna_inputs()
    colnames(site.fauna) <- fauna.names
    #site.corridor: buffer.dev.Score, edge.density.perft, corridorwidth.ft, and corridormin.ft (4 total)
    site.corridor<- corridor_inputs()
    colnames(site.corridor) <- corridor.names
    #site.area: area.input (1 total)
    site.area <- area_inputs()
    
    #Create empty matrices to store suitability outputs
    SI.instream <- data.frame(matrix(ncol=7,nrow=nrow(site.instream)))
    SI.fauna <- data.frame(matrix(ncol=8,nrow=nrow(site.fauna)))
    SI.corridor <- data.frame(matrix(ncol=4,nrow=nrow(site.corridor)))

    # #Fill site inputs with the user inputs
    # #site.instream: hyd.att, stripwidth.ft, flowpath.score, shading.ratio, cancov.score, canstr.score, and carbret.score (7 total)
    # site.instream <- c(input$hyd.att,input$stripwidth.ft,input$flowpath.score,input$shading.ratio,input$cancov.score,input$canstr.score,input$carbret.score)
    # #site.fauna: canstr.score, deadfall.score, snag.score, batcan.score, embed.score, detritus.score, herb.score, and inv.veg.score (8 total)
    # site.fauna <- c(input$canstr.score, input$deadfall.score, input$snag.score, input$batcan.score, input$embed.score, input$detritus.score, input$herb.score, input$inv.veg.score)
    # #site.corridor: buffer.dev.Score, edge.density.perft, corridorwidth.ft, and corridormin.ft (4 total)
    # site.corridor<- c(input$buffer.dev.Score, input$edge.density.perft, input$corridorwidth.ft, input$corridormin.ft)
    # #site.area: area.input (1 total)
    # site.area <- c(input$area.input)

    #Calculate suitability indices for each input variable and module using SIcalc( ) from the ecorest package
    for(i in 1:nrow(site.area)){
      SI.instream[i,1:7] <- SIcalc(SMURF_instream, site.instream[i,1:7])
      SI.fauna[i,1:8] <- SIcalc(SMURF_fauna, site.fauna[i,1:8])
      SI.corridor[i,1:4] <- SIcalc(SMURF_corridor, site.corridor[i,1:4])
    }

    #Create empty data frame to store outputs (Instream SI, Habitat SI, Corridor SI, HSI, Area, Habitat Units)
    SMURF.out <- as.data.frame(matrix(NA, nrow = nrow(site.area), ncol = 6))
    colnames(SMURF.out) <- c("Instream.SI", "Fauna.SI", "Corridor.SI", "HSI", "Area", "HU")
    #If any input is NA, return NA
    for(i in 1:nrow(site.area)){
      if (sum(is.na(c(site.instream[i,1],site.fauna[i,1],site.corridor[i,1]))) > 0){
        SMURF.out[i,1] <- NA
        SMURF.out[i,2] <- NA
        SMURF.out[i,3] <- NA
        SMURF.out[i,4] <- NA
        SMURF.out[i,5] <- NA
        SMURF.out[i,6] <- NA
      }
      
      #Else compute all other outputs
      else{
        #Compute module-specific habitat suitability indices using HSIarimean( ) from the ecorest package - ARITHMETIC MEAN
        SMURF.out[i,1] <- round(HSIarimean(as.numeric(SI.instream[i,1:7])), digits=3)
        SMURF.out[i,2] <- round(HSIarimean(as.numeric(SI.fauna[i,1:8])), digits=3)
        SMURF.out[i,3] <- round(HSIarimean(as.numeric(SI.corridor[i,1:4])), digits=3)
        
        #Compute overarching habitat suitability index and habitat units
        SMURF.out[i,4] <- round((SMURF.out[i,1] * SMURF.out[i,2] * SMURF.out[i,3]) ^ (1/3), digits=3)
        SMURF.out[i,5]<- round(site.area[i,1], digits=3)
        SMURF.out[i,6] <- round(SMURF.out[i,4] * SMURF.out[i,5], digits=3)
      }
    }
    #Send output from function
    return(SMURF.out)
  })
  
  output$SMURF_multi<-renderDataTable({
    datatable(
      t(SMURF_multi()),
      colnames=NULL,
      options=list(paging=FALSE,
                   searching=FALSE,
                   paging=FALSE)
    )
  })
  
  model_content <- reactive({
    load('RiparianModels.rda')
    criteria_models <- RiparianModels[1:14,1:20]
    return(criteria_models)
  })

  output$keyword_search = renderUI({
    searchInput(
      inputId = "keyword",
      label = "Keyword Search",
      value = "",
      btnSearch = icon("magnifying-glass"),
      btnReset = icon("xmark")
    )
  })
  
  #########################################################################################################################
  # Function: redirect -website redirect output-
  # Output: prints website page of HYDROCAL
  #########################################################################################################################
  
  output$redirect<-renderUI({ #prints description of HYDROCAL from web
    tags$a(href=input$website,input$website)
  })
}

# shiny::reactlogShow()
shinyApp(ui=ui, server=server) 