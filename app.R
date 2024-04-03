library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinyBS)
library(DT)
library(ggplot2)
library(ggpubr)
library(plotly)
library(rhandsontable)

rm(list=ls())

source("axlHelper.R")

ui <- dashboardPage(
  title = "MerGers",
  dashboardHeader(title = tags$div(tags$img(src="logo.png"))),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      br(),br(),br(),br(),br(),br(),br(),
      menuItem("Data Upload", tabName = "data", icon = icon("upload")),
      menuItem("Tables", tabName = "tables", icon = icon("table")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data",
        fluidRow(
          box(fileInput(inputId = "plates", label = "Upload the Axl File",
                        multiple = FALSE, accept = c(".xlsx", ".csv")),
              column(2, checkboxInput("appendFile", "Add File", value = FALSE)),
              column(2, checkboxInput("csvUpload", "CSV Format", value = FALSE))),
          actionButton("configure", "", icon = icon("gear"))
        ),
        bsModal("configs", "Configurations", "configure", size = "large",
                tabsetPanel(
                  tabPanel(title = "Sample Type",
                           br(),
                           rHandsontableOutput("sampleTypeConfig"),
                           br(),
                           br(),
                           actionButton("sampleTypeRun", "Rerun Sample Type")),
                  tabPanel(title = "Normalization",
                           br(),
                           fluidRow(
                             column(3, uiOutput("normType")),
                             column(3, uiOutput("normTrt")),
                             column(3, checkboxInput("normPlate", "Plate-Level Normalization", TRUE)),
                             column(3, sliderInput("normCount", "Values to Use (0 for All)",
                                                   min = 0, max = 12, value = 8, step = 4))
                           ),
                           actionButton("normalizeRun", "Rerun Normalization")),
                  tabPanel(title = "Column Management",
                           br(),
                           uiOutput("trtSelectOutput")
                  ),
                  tabPanel(title = "Save/Load",
                           br(),
                           fluidRow(
                             column(6, textInput("saveName", "Save Name", "Save 1")),
                             column(6, br(), actionButton("saveFile", "Save"))
                           ),
                           uiOutput("loadOutput"),
                           fileInput(inputId = "importSave", label = "Upload the RDS Save",
                                     multiple = FALSE, accept = ".RDS")
                  )
                )),
        br(),
        br(),
        fluidRow(
          box(
            "Data Check: ", withSpinner(textOutput("dataCheck", inline = T),
                                        size=0.4, proxy.height = "20px"))
        ),
        fluidRow(
          box(id = "summaryBox",
              "Issues: ",
              withSpinner(
                dataTableOutput("errorSummary"),
                size=0.4, proxy.height = "20px"),
              width = 9
          )
        ),
        fluidRow(
          box(id = "buttonBox",
              actionButton("correctErrors", "Correct Errors"),
              actionButton("ignoreErrors", "Ignore Errors")
          ))
      ),
      tabItem(
        tabName = "tables",
        br(),
        fluidRow(
          column(4),
          column(4, downloadButton("downloadCSV", "Download in CSV format"))
          # ,column(4, downloadButton("downloadAxl", "Download in Axl format"))
        ),
        column(12, align="center",
               uiOutput("tablePlateChoiceUI")),
        br(),
        fluidRow(
          column(1, align = "center", actionButton("showSample","Toggle Sample DF")),
          column(2, checkboxInput("sampleType", "Aggregate by Sample Type", value = FALSE))),
        br(),
        fluidRow(
          column(11, align = "center", dataTableOutput("sampleDF"))
        ),
        br(),
        column(11, align = "center", "Plate Data"),
        br(),
        fluidRow(
          column(8),
          column(3, actionButton("addCol", "Add Column"))
        ),
        bsModal("addCols", "Add Column", "addCol", size = "large",
                textInput("colName", "New Column Name", "x1"),
                textInput("colVal", "Default Column Value", "0"),
                checkboxInput("eval", "Evaluate Input", FALSE),
                actionButton("addColTrigger", "Add Column")
        ),
        br(),
        fluidRow(
          column(11, offset = 2, withSpinner(rHandsontableOutput("plateDF")))
        )),
      tabItem(
        tabName = "analysis",
        tabsetPanel(
          tabPanel("Overview",
                   
                   br(),
                   column(12, align="center",
                          selectInput("plotChoice", "Select the Analysis",
                                      choices = c("Overview" = 1,
                                                  "Patient Samples" = 2,
                                                  "Patient Samples Normalized to the Negative Control" = 3,
                                                  "NC signal versus Sample Responses" = 4,
                                                  "Simple Trend of PC 500" = 5))),
                   br(),
                   
                   fluidRow(
                     column(11, align = "center", withSpinner(plotlyOutput("plotcomp", height = 800)))
                   )
          ),
          tabPanel("PC Signal",
                   br(),
                   fluidRow(
                     column(4, checkboxGroupInput("logPlot", "Toggle Log Scale",
                                                  c("Concentration", "Sample"),c("Concentration", "Sample"),
                                                  inline = TRUE)),
                     column(4, selectInput("logPlotCol", "Select Fill Column",
                                           c("None", "plate", "trt", "row", "col")))
                   ),
                   fluidRow(
                     column(11, align = "center", withSpinner(plotlyOutput("plotsignal", height = 800)))
                   )
          ),
          tabPanel("Heatmap",
                   br(),
                   fluidRow(
                     column(4, uiOutput("heatmapType")),
                     column(4, selectInput("heatmapChartY", "Select Y Axis",
                                           c("Response", "Concentration", "S/N")))
                   ),
                   br(),
                   br(),
                   fluidRow(
                     column(11, align = "center", withSpinner(plotlyOutput("heatmapChart", height = 600)))
                   )
          ),
          tabPanel("PC Dilution",
                   br(),
                   fluidRow(
                     column(4, sliderInput("dilCutoff", "Dilution Cutoff", 0.6, 1.8, 1.1, 0.1)),
                     column(4, uiOutput("dilPlateChoiceUI"))
                   ),
                   fluidRow(
                     column(11, align = "center", withSpinner(plotlyOutput("dilChart", height = 600)))
                   ),
                   fluidRow(
                     column(11, align = "center", withSpinner(tableOutput("dilTable")))
                   )
          ),
          tabPanel("Sample Tracker",
                   br(),
                   fluidRow(
                     column(4, uiOutput("samplePlateChoiceUI")),
                     column(4, uiOutput("sampleSampleChoiceUI"))
                   ),
                   fluidRow(
                     column(11, align = "center", withSpinner(plotlyOutput("sampleChart", height = 600)))
                   )
          ),
          tabPanel("Custom",
                   br(),
                   fluidRow(
                     column(4, selectInput("custChartType", "Select Chart Type",
                                           c("Scatterplot", "Boxplot", "Histogram"))),
                     column(4, selectInput("custChartX", "Select X Axis",
                                           c("Response", "Concentration", "S/N",
                                             "Sample", "Treatment", "Type", "Plate",
                                             "Well", "Row", "Col"))),
                     column(4, selectInput("custChartY", "Select Y Axis",
                                           c("Response", "Concentration", "S/N")))
                   ),
                   br(),
                   fluidRow(
                     column(4, checkboxGroupInput("custChartLog", "Toggle Log Scale",
                                                  c("X", "Y"), inline = TRUE)),
                     column(4, selectInput("custChartCol", "Select Colour Column",
                                           c("None", "Plate", "Treatment", "Row", "Col", "Type"))),
                     column(4, selectInput("custChartFacet", "Select Facet Column",
                                           c("None", "Plate", "Treatment", "Row", "Col", "Type"))),
                   ),
                   br(),
                   br(),
                   fluidRow(
                     column(11, align = "center", withSpinner(plotlyOutput("custChart", height = 600)))
                   )
          )
        )
      )
    )
  )
)

server <- function(input, output, session)
{
  # Reactive Variables
  
  importTrigger <- reactiveVal(FALSE)
  fileCount <- reactiveVal(0)
  finalData <- reactiveVal(data.frame())
  errorHold <- reactiveVal(data.frame())
  plates <- reactiveVal(c())
  varNames <- reactiveValues(names = c())
  savedFiles <- reactiveValues(names = c())
  
  typeDF <- reactiveVal(data.frame(
    Type = c("Serum", "Serum", "Positive Control", "Positive Control", "Negative Control"),
    Ident = c("HS", "S", "PC", "HPC", "NC")))
  
  treatDF <- reactiveVal(data.frame(
    Column = c("trt", "drug"),
    Default = c("BT/TAG", "0"),
    Alt = c("BT/TAG + DRUG", "1")
  ))
  
  trts <- reactiveVal(c())
  
  # Data Upload
  
  ## Plate Input
  
  plateFile <- reactive({
    platePath <- input$plates$datapath
    print("Upload Get")
    hide(selector = "a[data-value='analysis']", anim = FALSE)
    hide(selector = "a[data-value='tables']", anim = FALSE)
    hide("summaryBox")
    hide("buttonBox")
    
    importTrigger(FALSE)
    if(!input$csvUpload)
    {
      plateFile <- loadPlateFiles(platePath, force.read = TRUE)
    }
    
    if(input$csvUpload)
    {
      plateFile <- read.csv(platePath)
    }
    
    plateFile
  })
  
  checkData <- reactive({
    imported <- importTrigger()
    
    print("Verifying")
    print(imported)
    
    checkResponse <- "Verified!"
    
    hide(selector = "a[data-value='analysis']", anim = FALSE)
    hide(selector = "a[data-value='tables']", anim = FALSE)
    hide("summaryBox")
    hide("buttonBox")
    
    if(!imported)
    {
      print("Not Imported")
      if(is.null(input$plates$datapath))
        return("Please Upload Data")
      
      t1 <- tryCatch(plateFile(), error = function(e) e)
      if(any(class(t1) %in% "error"))
      {
        return(paste("Data Error:", t1$message, "Please check the data."))
      }
      if(!is.list(t1) | length(t1) == 0)
      {
        return("Data Error.")
      }
    }
    
    show(selector = "a[data-value='tables']", anim = TRUE)
    show(selector = "a[data-value='analysis']", anim = TRUE)
    show("summaryBox")
    show("buttonBox")
    
    treatCol <- varNames$treatCol
    
    if(length(treatCol)==0)
    {
      checkResponse <- "No Treatment Column Found."
    }
    
    checkResponse
  })
  
  output$dataCheck <- renderText({
    checkData()
  })
  
  observeEvent(input$plates, {
    axdf <- plateFile()
    
    print("Checking")
    
    treatCol <- intersect(names(axdf), treatDF()$Column)
    
    axdf_orig <- finalData()
    
    fC <- fileCount()
    trDF <- treatDF()
    
    if(fC==0 | !input$appendFile)
    {
      if(length(treatCol)>0)
      {
        varNames$names <- c(varNames$names, "treatCol")
        varNames$treatCol <- treatCol
        trts(unique(axdf[,treatCol]))
        default <- trDF$Default[trDF$Column %in% treatCol]
      }
      
      if(length(treatCol)==0)
      {
        treatCol <- "plate"
        default <- "All"
      }
    }
    if(input$appendFile & fC>0)
    {
      print("Treat Append")
      tC_orig <- varNames$treatCol
      print(tC_orig)
      if(length(treatCol)>0)
      {
        default <- trDF$Default[trDF$Column %in% treatCol]
        if(length(tC_orig)==0)
        {
          axdf_orig[,treatCol] <- default
          varNames$names <- c(varNames$names, "treatCol")
          varNames$treatCol <- treatCol
          trts(unique(axdf[,treatCol]))
        }
        
        if(length(tC_orig)>0 & tC_orig!=treatCol)
        {
          print("Merge treatCols")
          axdf[,tC_orig] <- axdf[,treatCol]
          axdf[,treatCol] <- NULL
          axdf[,tC_orig] <- sapply(axdf[,tC_orig], function(x) {
            xR <- which(trDF[trDF$Column %in% treatCol,] %in% x)
            trDF[trDF$Column %in% tC_orig, xR]
          })
          
          print("Merged")
          default <- trDF$Default[trDF$Column %in% tC_orig]
          treatCol <- tC_orig
        }
      }
      
      if(length(treatCol)==0)
      {
        if(length(tC_orig)==0)
        {
          treatCol <- "plate"
          default <- "All"
        }
        
        if(length(tC_orig)>0)
        {
          default <- trDF$Default[trDF$Column %in% tC_orig]
          axdf[,tC_orig] <- default
        }
      }
    }
    print("Treatcols Handled")
    
    axdf <- sampleFormat(axdf)
    axdf <- sampleClass(axdf, typeDF())
    
    print(treatCol)
    print(default)
    axdf <- normalizeResponses(axdf = axdf, treatCol = treatCol, trt = default)
    
    print("Treated")
    
    fileCount(1)
    if(fC==0 | !input$appendFile)
    {
      errs <- checkDataErrors(axdf)
    }
    
    if(input$appendFile & fC>0)
    {
      if(fC==1)
      {
        axdf_orig <- cbind(data.frame(fileC = rep(1, nrow(axdf_orig))), axdf_orig)
        axdf_orig$plate <- paste0("1.", axdf_orig$plate)
      }
      
      axdf <- cbind(data.frame(fileC = rep(fC+1, nrow(axdf))), axdf)
      axdf$plate <- paste0(fC+1, ".", axdf$plate)
      
      missingCols <- setdiff(names(axdf_orig), names(axdf))
      print("In orig but not new")
      print(missingCols)
      
      if(length(missingCols)>0)
        axdf[,missingCols] <- NA
      
      missingCols <- setdiff(names(axdf), names(axdf_orig))
      print("In new but not orig")
      print(missingCols)
      
      if(length(missingCols)>0)
        axdf_orig[,missingCols] <- NA
      
      axdf <- rbind(axdf_orig, axdf)
      fileCount(fC+1)
      
      print("Recheck Errors")
      errs <- checkDataErrors(axdf)
    }
    print("Finishing Up")
    errorHold(errs)
    
    finalData(axdf)
    print("Final")
    plates(unique(axdf$plate))
  })
  
  ## Errors
  
  output$errorSummary <- renderDataTable({
    errorHold()
  }, rownames = FALSE,
  options = list(dom = "ftp"))
  
  observeEvent(input$correctErrors, {
    errs <- errorHold()
    
    errChoices <- input$errorSummary_rows_selected
    
    handled <- errs[errChoices,]
    
    axdf <- finalData()
    
    # Treat Duplicate Plates
    handleDups <- handled[handled$Code %in% "DUP",]
    axdf <- axdf[!(axdf$plate %in% handleDups$Plate),]
    handled <- handled[!(handled$Plate %in% handleDups$Plate),]
    
    # Treat Concentration
    handleConc <- handled[substr(handled$Code, 1, 2) %in% "PC",]
    axdf <- treatConc(axdf, plates = handleConc$Plate)
    
    errs <- errs[-errChoices,]
    errs <- errs[!(errs$Plate %in% handleDups$Plate),]
    
    errorHold(errs)
    
    finalData(axdf)
    plates(unique(axdf$plate))
  })
  
  observeEvent(input$ignoreErrors, {
    errs <- errorHold()
    
    errChoices <- input$errorSummary_rows_selected
    
    errs <- errs[-errChoices,]
    
    errorHold(errs)
  })
  
  # Configuration Module
  
  ## Sample Type Mapping
  output$sampleTypeConfig <- renderRHandsontable({
    rhandsontable(typeDF())
  })
  
  observe({
    newdf <- as.data.frame(hot_to_r(input$sampleTypeConfig))
    
    if(nrow(newdf)>0)
      typeDF(newdf)
  })
  
  ## Normalization
  
  observeEvent(input$sampleTypeRun, {
    axdf <- finalData()
    
    print("Reclassing")
    axdf <- sampleFormat(axdf)
    axdf <- sampleClass(axdf, typeDF())
    print("Reclassed")
    
    finalData(axdf)
  })
  
  output$normType <- renderUI({
    def <- ifelse("Negative Control" %in% typeDF()$Type, "Negative Control", "All")
    
    selectInput("normTypeSel", "Type", c("All", typeDF()$Type), def, multiple = FALSE)
  })
  
  output$normTrt <- renderUI({
    default <- intersect(trts(), c("BT/TAG", "0"))
    
    def <- ifelse(default %in% trts(), default, "All")
    
    selectInput("normTrtSel", "Treatment", c("All", trts()), def, multiple = FALSE)
  })
  
  observeEvent(input$normalizeRun, {
    axdf <- finalData()
    
    axdf <- normalizeResponses(axdf, treatCol,
                               input$normTypeSel, input$normTrtSel, input$normPlate, input$normCount)
    
    finalData(axdf)
  })
  
  ## Column Management
  output$trtSelectOutput <- renderUI({
    if(length(varNames$treatCol)==0)
    {
      tagList(selectInput("trtSelect", label = "Select Treatment Column", names(finalData())),
              actionButton("updateTrtCol", "Update"))
    }
  })
  
  observeEvent(input$updateTrtCol, {
    varNames$treatCol <- input$trtSelect
  })
  
  ## Saves
  
  observeEvent(input$saveFile, {
    saveName <- input$saveName
    
    namesList <- savedFiles$names
    
    namesList <- unique(c(namesList, saveName))
    
    savedFiles$names <- namesList
    
    i <- 1
    newName <- paste("Save", i)
    while(newName %in% namesList)
    {
      i <- i+1
      newName <- paste("Save", i)
    }
    
    updateTextInput(inputId = "saveName", label = "Save Name", value = newName)
    
    
    saveData <- list(df = finalData(), err = errorHold(),
                     pl = plates(), fc = fileCount(), td = typeDF(), vn = varNames)
    
    savedFiles[[saveName]] <- saveData
  })
  
  output$loadOutput <- renderUI({
    namesList <- savedFiles$names
    
    if(length(namesList)==0)
    {
      return()
    }
    
    tagList(selectInput("loadName", "Saved Files", namesList, selected = namesList[1]),
            actionButton("loadSave", "Load Data"),
            downloadButton("exportSave", "Export Data"))
  })
  
  observeEvent(input$loadSave, {
    namesList <- savedFiles$names
    if(input$loadName %in% namesList)
    {
      saveData <- savedFiles[[input$loadName]]
      finalData(saveData$df)
      errorHold(saveData$err)
      plates(saveData$pl)
      fileCount(saveData$fc)
      typeDF(saveData$td)
      vn <- saveData$vn
      for(n in names(vn))
      {
        varNames[[n]] <- vn$n
      }
    }
  })
  
  output$exportSave <- downloadHandler(
    filename =  function() {paste0(input$loadName, ".RDS")},
    content = function(file) {
      namesList <- savedFiles$names
      
      if(input$loadName %in% namesList)
      {
        saveData <- savedFiles[[input$loadName]]
        saveData[["saveName"]] <- input$loadName
        saveRDS(saveData, file)
      }
    }
  )
  
  observeEvent(input$importSave, {
    savePath <- input$importSave$datapath
    saveData <- readRDS(savePath)
    
    print("Read RDS")
    print(savePath)
    
    finalData(saveData$df)
    errorHold(saveData$err)
    plates(saveData$pl)
    fileCount(saveData$fc)
    typeDF(saveData$td)
    vn <- saveData$vn
    for(n in vn$names)
    {
      varNames[[n]] <- vn[[n]]
    }
    
    namesList <- savedFiles$names
    
    saveName <- saveData$saveName
    
    namesList <- c(namesList, saveName)
    
    savedFiles[[saveName]] <- saveData
    
    savedFiles$names <- namesList
    
    i <- 1
    newName <- paste("Save", i)
    while(newName %in% namesList)
    {
      i <- i+1
      newName <- paste("Save", i)
    }
    
    importTrigger(TRUE)
    
    updateTextInput(inputId = "saveName", label = "Save Name", value = paste("Save", i+1))
  })
  
  # Tables
  
  ## Plate Choice
  
  output$tablePlateChoiceUI <- renderUI({
    selectInput("tablePlateChoice", "Select the Plate",
                choices = c(0, plates()))
    
  })
  
  observeEvent(input$tablePlateChoice, {
    if(input$tablePlateChoice!=0)
    {
      hide(selector = "a[data-value='data']", anim = FALSE)
      hide(selector = "a[data-value='analysis']", anim = FALSE)
    }
    if(input$tablePlateChoice==0)
    {
      show(selector = "a[data-value='data']", anim = TRUE)
      show(selector = "a[data-value='analysis']", anim = TRUE)
    }
  })
  
  # Sample Summary
  
  observeEvent(input$showSample, {
    toggle("sampleDF", anim = TRUE)
  })
  
  output$sampleDF <- renderDataTable({
    
    if(is.null(input$tablePlateChoice) || input$tablePlateChoice %in% 0)
    {
      return(data.frame())
    }
    
    axdf <- finalData()
    
    axdf <- axdf[axdf$plate %in% input$tablePlateChoice,]
    
    axdf <- Filter(function(x) !all(is.na(x)), axdf)
    
    if(!input$sampleType)
    {
      sampleDF <- data.frame(t(aggregate(y ~ sample, axdf, function(x) {
        
        c("Mean" = round(mean(x), 2), "SD" = round(sd(x), 2))
      })))
      
      names(sampleDF) <- sampleDF[1,]
      sampleDF <- sampleDF[-1,]
      rownames(sampleDF) <- gsub("y.", "", rownames(sampleDF), fixed = TRUE)
      sampleDF <- cbind(data.frame(Metric = rownames(sampleDF)), sampleDF)
    }
    
    if(input$sampleType)
    {
      sampleDF <- data.frame(aggregate(y ~ type, axdf, function(x) {
        xmean <- mean(x)
        xsd <- sd(x)
        xunder <- sum(x<(xmean-1.5*xsd))
        xover <- sum(x>(xmean+1.5*xsd))
        
        c("Count" = length(x), "Mean" = round(xmean, 2), "SD" = round(xsd, 2),
          "Outliers Below" = xunder, "Outliers Above" = xover)
      }))
      
      sampleDF <- data.frame(cbind(sampleDF$type, sampleDF$y), check.names = FALSE)
      names(sampleDF)[1] <- "Metric"
    }
    
    sampleDF
  }, options = list(dom = "t", scrollX = TRUE))
  
  ## Column Add
  
  observeEvent(input$addColTrigger, {
    axdf <- finalData()
    
    newVal <- input$colVal
    
    if(input$eval)
    {
      newVal <- tryCatch({with(axdf, eval(parse(text = newVal)))},
                         error = function(cond) {newVal},
                         warning = function(cond) {newVal})
    }
    
    newVal <- ifelse(!is.na(as.numeric(newVal)), as.numeric(newVal), newVal)
    
    axdf[,input$colName] <- newVal
    
    finalData(axdf)
  })
  
  ## Raw Data
  
  output$plateDF <- renderRHandsontable({
    axdf <- finalData()
    
    axdf <- axdf[axdf$plate %in% input$tablePlateChoice,]
    
    axdf <- Filter(function(x) !all(is.na(x)), axdf)
    
    rhandsontable(axdf)
  })
  
  observe({
    newdf <- as.data.frame(hot_to_r(input$plateDF))
    axdf <- finalData()
    
    if(nrow(newdf)>0)
    {
      if(length(unique(newdf$plate))==1 &
         unique(newdf$plate)==input$tablePlateChoice)
      {
        axdf[axdf$plate %in% input$tablePlateChoice, names(newdf)] <- newdf
        finalData(axdf)
      }
    }
    
  })
  
  ## Data Downloads
  
  output$downloadAxl <- downloadHandler(
    filename =  "Axl Plates - Axl Format.xlsx",
    content = function(file) {
      wb = createAxlFormat(finalData())
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadCSV <- downloadHandler(
    filename =  "Axl Plates - Table Format.csv",
    content = function(file) {
      write.csv(finalData(), file, row.names = FALSE)
    }
  )
  
  # Analysis
  
  ## Overview
  
  hsdata <- reactive({
    # hs <- subset(finalData(),substr(sample,1,1)=="H")
    print("Get HS")
    hs <- finalData()
    hs <- hs[hs$type %in% "Positive Control",]
    
    # hs$sample <- as.numeric(gsub("HS #", "", hs$sample))
    
    hs
  })
  
  ncdata <- reactive({
    # nc = subset(finalData(),substr(sample,1,1)=="N")
    nc <- finalData()
    nc <- nc[nc$type %in% "Negative Control",]
    
    tC <- varNames$treatCol
    
    if(length(tC)==0)
    {
      tC <- "plate"
    }
    
    nc = aggregate(nc$y,by=as.list(nc[,c("plate",tC)]),FUN=median)
    names(nc)[NCOL(nc)]<-"nc"
    
    # merge the median NCs and calculate a normalized signal
    hs = merge(hsdata(), nc)
    hs$normalized = hs$y/hs$nc
    
    hs
  })
  
  an1 <- reactive({
    hs <- hsdata()
    print("an1")
    print(head(hs))
    
    tC <- varNames$treatCol
    
    if(length(tC)==0)
    {
      tC <- "sample"
    }
    
    g1 <- ggplot(hs,aes(x=sample,y=y, group = as.character(sample), fill = get(tC),
                        text = paste("Sample HS #:", sample,
                                     "<br>Instrument Response:", y))) +
      geom_boxplot(show.legend = FALSE) +
      facet_wrap(~get(tC)) +
      labs(x="Sample HS #",y="Instrument Response", title = "Patient Samples")
    
    ggplotly(g1, tooltip = "text")
  })
  
  an2 <- reactive({
    hs <- ncdata()
    
    tC <- varNames$treatCol
    
    if(length(tC)==0)
    {
      tC <- "plate"
    }
    
    g1 <- ggplot(hs,aes(x=sample,y=normalized, group = as.character(sample), fill = get(tC),
                        text = paste("Sample HS #:", sample,
                                     "<br>Instrument Response:", y))) +
      geom_boxplot(show.legend = FALSE) +
      facet_wrap(~get(tC)) +
      labs(x="Sample HS #",y="Instrument Response",
           title = "Patient Samples Normalized to the Negative Control")
    
    ggplotly(g1, tooltip = "text")
  })
  
  an3 <- reactive({
    hs <- ncdata()
    
    tC <- varNames$treatCol
    
    if(length(tC)==0)
    {
      tC <- "plate"
    }
    
    g1 <- ggplot(hs,aes(x=nc,y=y, fill = get(tC),
                        text = paste("NC:", nc,
                                     "<br>Instrument Response:", y))) +
      geom_abline(intercept=0,slope=1,color="gray48", fill = "gray48", size=2, show.legend = FALSE) +
      geom_point(shape = 21, size = 2, col = "black", show.legend = FALSE) +
      geom_smooth(method = "lm",formula=y~x, color="black", show.legend = FALSE) +
      facet_wrap(~get(tC),scales="free_x") +
      labs(x="NC",y="Sample Responses",
           title = "NC signal versus Sample Responses")
    
    ggplotly(g1, tooltip = "text")
  })
  
  an4 <- reactive({
    hpc = subset(finalData(),type %in% c("PC 500"))
    tC <- varNames$treatCol
    
    if(length(tC)==0)
    {
      tC <- "plate"
    }
    
    if(nrow(hpc)==0)
    {
      g1 <- ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = "No PC 500 Measured") + 
        theme_void()
      return(ggplotly(g1))
    }
    
    g1 <- ggplot(hpc, aes(x = plate, y = y, fill = get(tC),
                          text = paste("Plate:", sample,
                                       "<br>Instrument Response:", y,
                                       "<br>Treatment:", get(tC)))) +
      geom_point(shape = 21, size = 3, col = "black") +
      scale_y_log10() +
      labs(x="Plate",y="Response",color="Treatment",
           title="Simple Trend of PC 500")
    
    ggplotly(g1, tooltip = "text")
  })
  
  output$plotcomp <- renderPlotly({
    # plotList <- list(an1(), an2(), an3(), an4())
    plotChoice <- as.numeric(input$plotChoice)
    
    if(plotChoice>1)
    {
      fName <- paste0("an", plotChoice-1)
      g1 <- get(fName)()
    }
    # g1 <- plotList[[plotChoice-1]]
    
    if(plotChoice %in% 1)
    {
      g1 <- subplot(an1(), an2(),
                    an3(), an4(),
                    nrows = 2)
      g1$x$layout$title <- NULL
      g1$x$layout$showlegend <- FALSE
      
    }
    
    g1
  })
  
  ## Signal Plot
  
  signalPlotCreator <- reactive({
    axdf <- finalData()
    
    if(!("conc" %in% names(axdf)))
    {
      g1 <- ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = "No Concentration Measured") + 
        theme_void()
      
      return(ggplotly(g1))
    }
    
    g1 <- ggplot(axdf[!is.na(axdf$conc),],
                 aes(x = conc, y = y,
                     text = paste("Concentration:", conc,
                                  "<br>Signal:", y))) +
      geom_point() +
      labs(x = "Concentration", y = "PC Signal", title = "PC Signal vs Concentration")
    
    if("Concentration" %in% input$logPlot)
      g1 <- g1 + scale_x_log10()
    
    if("Sample" %in% input$logPlot)
      g1 <- g1 + scale_y_log10()
    
    lpc <- input$logPlotCol
    
    if(lpc %in% "trt")
    {
      lpc <- varNames$treatCol
    }
    
    if(!is.null(lpc) & lpc!="None")
    {
      g1 <- g1 + aes(col = as.character(get(lpc))) + labs(col = lpc) + 
        aes(text = paste("Concentration:", conc,
                         "<br>Signal:", y, paste0("<br>", lpc, ":"), get(lpc)))
    }
    
    g1 <- ggplotly(g1, tooltip = "text")
    
    g1
  })
  
  output$plotsignal <- renderPlotly({
    signalPlotCreator()
  })
  
  ## Heat Map
  
  output$heatmapType <- renderUI({
    selectInput("heatmapTypeSel", "Type", c("All", typeDF()$Type), multiple = FALSE)
  })
  
  output$heatmapChart <- renderPlotly({
    axdf <- finalData()
    
    type <- input$heatmapTypeSel
    
    if(type %in% "All")
    {
      type <- unique(axdf$type)
    }
    
    # axdf <- axdf[axdf$type %in% type,]
    
    yC <- switch(input$heatmapChartY,
                 "Response" = "y",
                 "Concentration" = "conc",
                 "S/N" = "sn")
    
    if(yC %in% "conc" & !("conc" %in% names(axdf)))
    {
      axdf$conc <- NA
    }
    
    axdf[!(axdf$type %in% type), yC] <- NA
    
    axdf <- axdf[order(axdf$row, axdf$col),]
    
    axdf$row2 <- LETTERS[axdf$row]
    
    axdf$row2 <- factor(axdf$row2, rev(LETTERS))
    
    gList <- list()
    
    for(plate in unique(axdf$plate))
    {
      g1 <- ggplot(axdf[axdf$plate %in% plate,],
                   aes(x = col, y = row2, fill = get(yC),
                       text = paste("Row:", row, "<br>Col:", col,
                                    "<br>", input$heatmapChartY, ":", get(yC)))) +
        geom_tile(show.legend = FALSE) + scale_fill_distiller(palette = "RdPu") +
        theme_minimal()
      
      gList[[plate]] <- ggplotly(g1, tooltip = "text") %>%
        layout(annotations = list(x = 0.5 , y = 1.01, xanchor = "center", yanchor = "bottom",
                                  text = paste("Heatmap for Plate", plate), showarrow = F, 
                                  xref='paper', yref='paper'))
    }
    
    gF <- subplot(gList, nrows = 2, margin = c(0.05, 0.05, 0.07, 0.07))
    
    gF
  })
  
  ## PC Dilution Chart
  
  output$dilPlateChoiceUI <- renderUI({
    selectInput("dilPlateChoice", "Select the Plate",
                choices = c(plates()))
  })
  
  dilutionTable <- reactive({
    axdf <- finalData()
    
    treatCol <- varNames$treatCol
    
    plate <- input$dilPlateChoice
    
    axdf_p <- axdf[axdf$plate %in% plate & axdf$type %in% "Positive Control",]
    
    if(!("analyst" %in% names(axdf_p)))
    {
      axdf_p$analyst <- 1
    }
    
    axdf_p[,treatCol] <- as.character(axdf_p[,treatCol])
    axdf_p$analyst <- as.character(axdf_p$analyst)
    
    axdf_p <- axdf_p[order(axdf_p$conc, axdf_p$sn),]
  })
  
  dilutionCutpoint <- reactive({
    axdf_p <- dilutionTable()
    cutpoint <- input$dilCutoff
    
    axdf_p$lsn <- log(axdf_p$sn)
    
    axdf_p$lsncut <- as.numeric(axdf_p$lsn>=cutpoint)
    
    cutpoint_df <- data.frame()
    
    for(r in unique(axdf_p$run))
    {
      axdf_r <- axdf_p[axdf_p$run %in% r & axdf_p$drug %in% 0,]
      axdf_r$lsncutdiff <- c(0, diff(axdf_r$lsncut))
      
      if(sum(axdf_r$lsncutdiff!=0)>1)
      {
        cutpoint_df <- rbind(cutpoint_df,
                             data.frame(Run = r, X = NA))
        
        next()
      }
      
      r1 <- max(which(axdf_r$lsncut==0))
      r0 <- min(which(axdf_r$lsncut==1))
      
      x1 = log(axdf_r$conc[r1])
      x0 = log(axdf_r$conc[r0])
      
      y1 = axdf_r$lsn[r1]
      y0 = axdf_r$lsn[r0]
      
      interpol <- x0 + (cutpoint - y0)*(x1-x0)/(y1-y0)
      
      cutpoint_df <- rbind(cutpoint_df,
                           data.frame(Run = r, X = interpol))
    }
    
    cutpoint_df <- cutpoint_df[complete.cases(cutpoint_df),]
    
    cpm <- mean(cutpoint_df$X)
    cpsd <- sd(cutpoint_df$X)
    ci <- qt(0.99, nrow(cutpoint_df)-1)*cpsd*sqrt(1+1/nrow(cutpoint_df))
    
    cp_df <- data.frame(Runs = nrow(cutpoint_df), Mean = cpm, SD = cpsd,
                        CI_Upper = cpm+ci, CI_Lower = cpm-ci)
    
    cp_df
  })
  
  output$dilChart <- renderPlotly({
    axdf_p <- dilutionTable()
    cp_df <- dilutionCutpoint()
    
    treatCol <- varNames$treatCol
    
    g1 <- ggplot(axdf_p, aes(x = log(conc), y = log(sn),
                             col = as.character(run), group = run, shape = analyst,
                             text = paste("Concentration:", conc,
                                          "<br>SN:", round(sn, 2),
                                          "<br>Run:", run,
                                          "<br>Analyst:", analyst)), show.legend = FALSE) +
    geom_point() + geom_line() + geom_hline(yintercept = input$dilCutoff, linetype = "dashed") +
      geom_vline(xintercept = cp_df$Mean, linetype = "dashed", col = "blue") +
      facet_wrap(~get(treatCol)) + ggtitle("PC Dilution")
  
  ggplotly(g1, tooltip = "text")
  })
  
  output$dilTable <- renderTable({
    cp_df <- dilutionCutpoint()
    
    cp_df[,-1] <- exp(cp_df[,-1])
    
    cp_df
  })

## Sample Tracker

output$samplePlateChoiceUI <- renderUI({
  selectInput("samplePlateChoice", "Select the Plate",
              choices = c(plates()))
})

output$sampleSampleChoiceUI <- renderUI({
  axdf <- finalData()
  
  plateChoice <- input$samplePlateChoice
  
  samples <- sort(unique(axdf$sample[axdf$plate %in% plateChoice]))
  
  selectInput("sampleSampleChoice", "Select the Sample",
              choices = c("All", samples))
})

output$sampleChart <- renderPlotly({
  axdf <- finalData()
  
  treatCol <- varNames$treatCol
  
  plateChoice <- input$samplePlateChoice
  
  axdf_p <- axdf[axdf$plate %in% plateChoice,]
  
  if(input$sampleSampleChoice!="All")
  {
    axdf_p <- axdf_p[axdf_p$sample %in% input$sampleSampleChoice,]
  }
  
  if(!("analyst" %in% names(axdf_p)))
  {
    axdf_p$analyst <- 1
  }
  
  axdf_p[,treatCol] <- as.character(axdf_p[,treatCol])
  axdf_p$analyst <- as.character(axdf_p$analyst)
  
  axdf_p <- axdf_p[order(axdf_p$conc, axdf_p$sn),]
  
  g1 <- ggplot(axdf_p, aes(x = run, y = sn, col = sample, group = sample, shape = analyst,
                     text = paste("Concentration:", conc,
                                  "<br>SN:", round(sn, 2),
                                  "<br>Run:", run,
                                  "<br>Analyst:", analyst,
                                  "<br>Sample:", sample))) +
    geom_point() + geom_line() + facet_wrap(~get(treatCol)) + ggtitle("Samples across Runs")
  
  ggplotly(g1, tooltip = "text")
  
})

## Custom Chart

output$custChart <- renderPlotly({
  axdf <- finalData()
  
  type <- input$custChartType
  
  xC <- switch(input$custChartX,
               "Response" = "y",
               "Concentration" = "conc",
               "S/N" = "sn",
               "Sample" = "sample",
               "Treatment" = varNames$treatCol,
               "Type" = "type",
               "Plate" = "plate",
               "Well" = "well",
               "Row" = "row",
               "Col" = "col")
  
  if(length(xC)==0)
  {
    xC <- "y"
  }
  
  yC <- switch(input$custChartY,
               "Response" = "y",
               "Concentration" = "conc",
               "S/N" = "sn")
  
  if((xC %in% "conc" | yC %in% "conc") & !("conc" %in% names(axdf)))
  {
    print("No Conc")
    g1 <- ggplot() + 
      annotate("text", x = 4, y = 25, size=8, label = "No Concentration Measured") + 
      theme_void()
    
    return(ggplotly(g1))
  }
  
  g1 <- ggplot(axdf, aes(x = get(xC), y = get(yC),
                         text = paste(input$custChartX, ":", get(xC), "<br>", input$custChartY, ":", get(yC))))
  
  if(type %in% "Histogram")
  {
    g1 <- ggplot(axdf, aes(y = get(yC)))
  }
  
  g1 <- g1 + labs(x = input$custChartX, y = input$custChartY)
  
  plotType <- switch(type, "Scatterplot" = geom_point,
                     "Boxplot" = geom_boxplot,
                     "Histogram" = geom_histogram)
  
  g1 <- g1 + plotType()
  
  if(xC %in% c("y", "conc", "sn"))
  {
    if("X" %in% input$custChartLog)
      g1 <- g1 + scale_x_log10()
  }
  
  if("Y" %in% input$custChartLog)
    g1 <- g1 + scale_y_log10()
  
  colC <- switch(input$custChartCol,
                 "None" = "None",
                 "Plate" = "plate",
                 "Treatment" = varNames$treatCol,
                 "Row" = "row",
                 "Col" = "col",
                 "Type" = "type")
  
  if(length(colC)==0)
  {
    colC <- "None"
  }
  
  if(!is.null(colC) & colC!="None")
  {
    g1 <- g1 + aes(col = as.character(get(colC))) + labs(col = colC)
  }
  
  facetC <- switch(input$custChartFacet,
                   "None" = "None",
                   "Plate" = "plate",
                   "Treatment" = varNames$treatCol,
                   "Row" = "row",
                   "Col" = "col",
                   "Type" = "type")
  
  if(length(facetC)==0)
  {
    facetC <- "None"
  }
  
  if(!is.null(facetC) & facetC!="None")
  {
    g1 <- g1 + facet_grid(~get(facetC)) + labs(facet = facetC)
  }
  
  ggplotly(g1, tooltip = "text")
})
}

shinyApp(ui, server)