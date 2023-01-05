##this document contains the code of the function safetyGraphicsInit with some
##adjustments for a simpler use of the app

#load packages
library(shiny)
require(safetyGraphics)
library(tidyverse)
library(shinyjs)
library(yaml)
library(shinydashboard)



#Define longer text elements that will later on appear on the starting page 
#of the app 

#info on yaml file
info_yaml <- "Here you can upload a .yaml file that contains a mapping connecting 
your data's columns to the charts of the safetyGraphics App <br/>
<b>If you have not used the safetyGraphics App before </b> <br/>
Leave this field blank and fill in the mapping manually after starting the app. 
You can save this mapping for your next session via Settings &#8594 Code &#8594 Download mapping.yaml <br/>
<b>If you have used the safetyGraphics App before </b> <br/>
Upload the .yaml file generated in a previous session"

#info on dm data
dmInfoText <- "This dataset should contain exactly one record per patient<br/> 
<b>ID Column</b> Unique subject identifier  <br/>
<b>Treatment Column</b> Name of treatments 
(Some plots require two treatment groups (e.g. placebo and verum), 
others can also display more than two groups (e.g. various doses of medication)) <br/>
<b>Sex Column</b> <br/>
<b>Race Column</b> <br/>
<b>Age Column</b> Numeric age
"

#indo on ae data
aeInfoText <-"This dataset should contain one record per patient and adverse event <br/>
<b>ID Column</b> Unique subject identifier  <br/>
<b>Sequence Column</b> Numeric adverse event sequence <br/>
<b>AE Start/ End Day Column</b> Numeric start and end day of adverse event <br/>
<b>Preferred Term Column</b> Preferred term of adverse event <br/>
<b>AE Body System</b> System organ class of adverse event <br/>
<b>AE Severity</b> Severity of adverse event (mild, moderate, severe) <br/>
<b>AE Seriousness</b> Seriousness of adverse event (Y, N) <br/>
"

#info on lab data
labInfoText <- "This dataset should contain one record per patient, lab measurement and visit day<br/>
<b>Measure Column</b> Name of laboratory measure or test. In order to perform DILI analysis, 
the following lab values are necessary:<br/>
Aminotransferase, alanine (ALT)<br/>
Aminotransferase, aspartate (AST)<br/>
Alkaline phosphatase (ALP)<br/>
Total Bilirubin<br/>
<b>ID Column</b> Unique subject identifier <br/>
<b>Value Column</b> Numeric result of a laboratory test<br/>
<b>Lower/ Upper Limit of Normal Column</b> Numeric upper/ lower limit of normal 
for the laboratory test results <br/>
<b>Study Day column</b> Numeric study day <br/>
<b>Visit Column</b> Visit name (e.g. 'Baseline' or 'Week 5-7') <br/>
<b>Visit Number Column</b> Numeric values indicating the order of the visit names in visit column <br/>
<b>Unit Column</b> Unit of lab measurements  <br/>
"
#waiting time before starting the app
delayTime = 5000 #increased from 1000 to 2000 due to error

#maximum file size for uploaded datasets
maxFileSize = 50 #set to 50 because labs dataset is often large

#list of charts to be displayed in the app (makeChartConfig selects standard charts)
charts_init <- makeChartConfig()
#make list of data domains needed for selected charts
all_domains <- charts_init %>% map(~.x$domain) %>% unlist() %>% 
  unique()

options(shiny.maxRequestSize = (maxFileSize * 1024^2))

css_path <- system.file("www", "index.css", package = "safetyGraphics")
app_css <- HTML(readLines(css_path))


### ui ###
ui <- fluidPage(useShinyjs(), tags$head(tags$style(app_css)), 
                
                div(id = "init", 
                    
                    titlePanel("safetyGraphics Initializer"), 
                    #Header with info on how to use the app
                    p(icon("info-circle"), 
                      HTML("First, select charts by dragging items between the lists below. 
                  Next, click the Test App button to run the safetyGraphics Shiny app with test data
                  or upload your own data and then click Run App.
                  If you have used the app before and saved the mapping you used, you can upload it aswell before starting the app. </br>
                  Reload the webpage to select new charts/data. </br>
                           The app works best when using Google Chrome"), 
                      class = "info"),
                    
                    
                    #Sidepanel for loading data
                    sidebarLayout(
                      position = "right", 
                      
                      #dataset upload
                      sidebarPanel(
                        h4("Data Loader"), 
                        
                        #Run with test data
                        p("Run the SafetyGraphics App with a test dataset by 
                          clicking the button below",
                          #Run App button for a test version
                          actionButton("testApp", 
                                       "Test App", 
                                       class = "btn-block"),
                          style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:5px"),
                        
                        p(HTML("<center> <b> -OR- </b></center>")),
                        
                        #Start app with own datasets
                        div("Upload your own data and mapping. Then click the Run App button",
                            
                            ##the run app button by safetyGraphics Package
                            shinyjs::disabled(actionButton("runApp", 
                                                           "Run App", 
                                                           class = "btn-block")),
                            
                            hr(), 
                            
                            #Upload data
                            HTML("<b> Load Data </b>"),
                            textOutput("dataSummary"), 
                            all_domains %>% map(~loadDataUI(.x, domain = .x)), 
                            
                            "Below, you can read about data requirements"   ,       
                            
                            ##action buttons for information on datasets
                            #ae data
                            p(id = "remarkAe",      
                              actionButton("btnAe", HTML("<b> &#9432 </b> Adverse Event  Data   &#8597;")),
                              hidden(p(id = "infoAe", icon("info-circle"), HTML(aeInfoText)))),
                            
                            #dm data
                            p(id = "remarkDm",     
                              actionButton("btnDm", HTML("<b> &#9432 </b>  Demographic Data &#8597;")),
                              hidden(p(id = "infoDm", icon("info-circle"), HTML(dmInfoText)))),
                            
                            #lab data
                            p(id = "remarkLab",      
                              actionButton("btnLab", HTML("<b> &#9432 </b>  Laboratory Data &#8597;")),
                              hidden(p(id = "infoLab", icon("info-circle"), HTML(labInfoText)))),
                            
                            hr(), 
                            
                            #Upload .yaml
                            HTML("<b> Load Mapping (optional)</b> </br>"),
                            
                            HTML("Upload a .yaml file containing a mapping for your data."),
                            
                            #add yaml file uploader
                            fileInput("loadYaml",
                                      label = NULL,
                                      multiple = FALSE,
                                      accept = c(".yaml")),
                            
                            #yalm file uploader
                            actionButton("btnYaml", HTML(" <b> &#9432 </b>  .yaml Mapping File &#8597;")),
                            hidden(p(id = "infoYaml", icon("info-circle"), HTML(info_yaml))),
                            #style # background of box
                            style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:5px"
                        )          
                      ), 
                      
                      #selection of charts
                      mainPanel(
                        loadChartsUI("load-charts", 
                                     charts = charts_init), ),
                    ), ), 
                shinyjs::hidden(div(id = "sg-app", 
                                    uiOutput("sg"))))


### server ###
server <- function(input, output, session) {
  
  charts <- callModule(loadCharts, "load-charts", 
                       charts = charts_init)
  
  domainDataR <- all_domains %>% map(~callModule(loadData, 
                                                 .x, domain = .x))
  
  names(domainDataR) <- all_domains
  
  domainData <- reactive({
    domainDataR %>% map(~.x())
  })
  
  current_domains <- reactive({
    charts() %>% map(~.x$domain) %>% unlist() %>% unique()
  })
  
  observe({
    for (domain in all_domains) {
      if (domain %in% current_domains()) {
        shinyjs::show(id = paste0(domain, "-wrap"))
      }
      else {
        shinyjs::hide(id = paste0(domain, "-wrap"))
      }
    }
  })
  
  
  ###Add Text for information on datasets
  #dm
  observeEvent(input$btnDm, {
    toggle("infoDm")
  })
  
  #ae
  observeEvent(input$btnAe, {
    toggle("infoAe")
  })
  
  #lab
  observeEvent(input$btnLab, {
    toggle("infoLab")
  })
  
  #yaml
  observeEvent(input$btnYaml, {
    toggle("infoYaml")
  })
  
  
  
  initStatus <- reactive({
    currentData <- domainData()
    chartCount <- length(charts())
    domainCount <- length(current_domains())
    loadCount <- sum(currentData %>% map_lgl(~!is.null(.x)))
    notAllLoaded <- sum(currentData %>% map_lgl(~!is.null(.x))) < 
      domainCount
    ready <- FALSE
    if (domainCount == 0) {
      status <- paste("No charts selected. Select one or more charts and then load domain data to initilize app.")
    }
    else if (notAllLoaded) {
      status <- paste(chartCount, " charts selected. ", 
                      loadCount, " of ", domainCount, " data domains loaded. Load remaining data domains to initialize app.")
    }
    else {
      status <- paste("Loaded ", loadCount, " data domains for ", 
                      chartCount, " charts. Click 'Run App' button to initialize app.")
      ready <- TRUE
    }
    return(list(status = status, ready = ready))
  })
  output$dataSummary <- renderText({
    initStatus()$status
  })
  observe({
    if (initStatus()$ready) {
      shinyjs::enable(id = "runApp")
    }
    else {
      shinyjs::disable(id = "runApp")
    }
  })
  
  #load list from .yaml file
  #if it is missing, set mapping to NULL to get an auto-generated mapping
  mappingList <- reactive({
    
    #req(input$load-yaml)
    if(is.null(input$loadYaml)) return(NULL)
    else read_yaml(input$loadYaml$datapath)
    
  })
  
  #run app with test
  observeEvent(input$testApp, {
    shinyjs::hide(id = "init")
    shinyjs::show(id = "sg-app")
    
    #app configuration - using example data from the safety Graphics package
    config <- app_startup(list(labs = safetyData::adam_adlbc, aes = safetyData::adam_adae, dm =
                                 safetyData::adam_adsl), meta = NULL, charts = charts(), 
                          filterDomain = "dm",  autoMapping = TRUE 
    )
    #domainData = domainData() %>% 
    #                        keep(~!is.null(.x)), meta = NULL, charts = charts(), 
    #                      filterDomain = "dm", mapping = mappingList(), autoMapping = TRUE 
    #)
    output$sg <- renderUI({
      safetyGraphicsUI("sg", config$meta, config$domainData, 
                       config$mapping, config$standards)
    })
    shinyjs::delay(delayTime, callModule(safetyGraphicsServer, 
                                         "sg", config$meta, config$mapping, config$domainData, 
                                         config$charts, config$filterDomain))
  })
  
  
  
  #run with actual data
  observeEvent(input$runApp, {
    shinyjs::hide(id = "init")
    shinyjs::show(id = "sg-app")
    
    #app configuration
    config <- app_startup(domainData = domainData() %>% 
                            keep(~!is.null(.x)), meta = NULL, charts = charts(), 
                          filterDomain = "dm", mapping = mappingList(), autoMapping = TRUE 
    )
    output$sg <- renderUI({
      safetyGraphicsUI("sg", config$meta, config$domainData, 
                       config$mapping, config$standards)
    })
    shinyjs::delay(delayTime, callModule(safetyGraphicsServer, 
                                         "sg", config$meta, config$mapping, config$domainData, 
                                         config$charts, config$filterDomain))
  })
}



app <- shinyApp(ui = ui, server = server)

#do not use runApp on Linux Server
#runApp(app, launch.browser = TRUE)

