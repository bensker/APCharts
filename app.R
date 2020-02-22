library(qcc)
library(shiny)
library(DT)
library(tidyverse)
library(markdown)

ui <- navbarPage(
  "AP Charts",
  tabPanel
  ("Import",
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        # Input: Select a file ----
        fileInput("file1",
          "Choose CSV File",
          multiple = TRUE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     'text/tab-separated-values',
                     'text/plain',
                     '.tsv',
                     ".csv")
        
        ),
        #actionButton("go","Display"),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons(
          "sep",
          "Separator",
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          selected = ","
        ),
        
        # Input: Select quotes ----
        radioButtons(
          "quote",
          "Quote",
          choices = c(
            None = "",
            "Double Quote" = '"',
            "Single Quote" = "'"
          ),
          selected = '"'
        ),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Select number of rows to display ----
        radioButtons("disp", "Display",
                     choices = c(All = "all",
                                 Head = "header"),
                     selected = "all")
      ),
       # Main panel for displaying outputs ----
      mainPanel(# Output: Data file ----
                DT::dataTableOutput("rendered_file")
                )
      )
  ),
  tabPanel("Plot",
           sidebarLayout(
             sidebarPanel(
               radioButtons(
               "plotType", "Plot type",
               c("p-chart" = "p", 
                 "Individual x" = "xbar.one",
                 "np-chart" = "np",
                 "u-chart" = "u",
                 "c-chart" = "c")
                           ),
               
               # Horizontal line ----
               tags$hr(),
               
               uiOutput("var1"),
               uiOutput("var2"),
               uiOutput("var3"),
               
               # Horizontal line ----
               tags$hr(),
               
               textInput("title", "Title:", "Chart Title"),
               textInput("yaxis", "Y-axis title", "Y-axis Label"),
               textInput("xaxis", "X-axis title:", "X-axis Label")
               #submitButton("Submit")
                      ),
             mainPanel(
               # Output: Header + table of distribution ----
               h4("Chart"),
               plotOutput("plot1",
                 # Equivalent to: click = clickOpts(id = "plot_click")
                 click = "plot_click"
                 # dblclick = dblclickOpts(id = "plot_dblclick"),
                 # hover = hoverOpts(id = "plot_hover"),
                 # brush = brushOpts(id = "plot_brush")
                        ),
               
               h4("Summary"),
               verbatimTextOutput("summary")
               
                      )
                    )
           ),
  # tabPanel("Publish",
  #          sidebarLayout(
  #            sidebarPanel(
  #              radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
  #                           inline = TRUE),
  #              #downloadButton('downloadReport')
  #              ),
  #            mainPanel(
  #              plotOutput("plot1"
  #              )
  #            )
  #          )
  # ),
  navbarMenu("More",
            tabPanel(
               "Violations",
               fluidRow(
                 column(width = 12,
                        verbatimTextOutput("violations"))
               )
             )
         )
)


server <- function(input, output) {
  df <- reactive({
    req(input$file1)
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote,
             stringsAsFactors = TRUE)  
    
  })
  
  # Print data table ----  
  output$rendered_file <- DT::renderDataTable({
  #  input$go
    if(input$disp == "head") {
      head(df())
    }
    else {
      df()
    }
  })
  
  
    # Dynamically generate UI input when data is uploaded ----
  output$var1 <- renderUI({
    selectInput(inputId = "select_var1", 
                       label = "Select value", 
                       choices = names(df()),
                       selected = names(df())[[2]])
  })
  
  output$var2 <- renderUI({
    selectInput(inputId = "select_var2", 
                label = "Select Sample", 
                choices = names(df()),
                selected = names(df())[[2]])
  })
  
  output$var3 <- renderUI({
    selectInput(inputId = "select_var3", 
                label = "Select Label", 
                choices = names(df()),
                selected = names(df())[[1]])
    
  })

  
  selectedData <- reactive({
    req(input$select_var1)
    req(input$select_var2)
    req(input$select_var3)
    selectedData <- df() %>% select(input$select_var3,input$select_var1,input$select_var2)
  })
  
  qccObject <- reactive({
    req(input$plotType)
    qccObject<-qcc(data = selectedData()[,2], sizes = selectedData()[,3], labels = selectedData()[,1], type = input$plotType , rules = 1:6)
  })
    
  output$plot1 <- renderPlot({
      req(input$title)
      req(input$xaxis)
      req(input$yaxis)
      plot.qcc(qccObject(), xlab=input$xaxis, ylab=input$yaxis, title = input$title, add.stats=FALSE)
  })     
  
  
  output$summary <- renderPrint({
    print.qcc(qccObject())
  })
  
  output$violations <- renderPrint({
    viol<-data.frame(qccObject()$violations)
    rules <- data.frame(RuleNum=1:6, RuleTitle= c("1. One point plots outside 3-sigma control limits",
                                                  "2. Two of three consecutive points plot beyond a 2-sigma limit",
                                                  "3. Four of five consecutive points plot beyond a 1-sigma limit",
                                                  "4. Eight consecutive points plot on one side of the center line",
                                                  "5. 15 points in zone C",
                                                  "6. 6 or more points increasing or decreasing")
    )
    viol<-merge(viol, rules, by.x =1, by.y =1)
    table(viol$RuleTitle) 
  }) 
  
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$dblclick_info <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
  output$brush_info <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })
  
  # output$downloadReport <- downloadHandler(
  #   filename = function() {
  #     paste('my-report', sep = '.', switch(
  #       input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
  #     ))
  #   },
  #   
  #   content = function(file) {
  #     src <- normalizePath('report.Rmd')
  #     
  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     # owd <- setwd(tempdir())
  #     # on.exit(setwd(owd))
  #     file.copy(src, 'report.Rmd', overwrite = TRUE)
  #     
  #     out <- render('report.Rmd', switch(
  #       input$format,
  #       PDF = pdf_document(), HTML = html_document(), Word = word_document()
  #     ))
  #     file.rename(out, file)
    # })
}


shinyApp(ui, server)
