library(qcc)
library(shiny)
library(DT)
library(tidyverse)

ui <- navbarPage(
  "AP Charts",
  tabPanel
  ("Import",
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        # Input: Select a file ----
        fileInput(
          "file1",
          "Choose CSV File",
          multiple = TRUE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        
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
               c("P-Chart" = "p", 
                 "Individual X" = "x",
                 "NP-Chart" = "np",
                 "U-Chart" = "u",
                 "C-Chart" = "c")
                           ),
               
               # Horizontal line ----
               tags$hr(),
               
               uiOutput("var1"),
               uiOutput("var2"),
               uiOutput("var3")
                      ),
             mainPanel(
               plotOutput("plot1",
                 # Equivalent to: click = clickOpts(id = "plot_click")
                 click = "plot_click",
                 dblclick = dblclickOpts(id = "plot_dblclick"),
                 hover = hoverOpts(id = "plot_hover"),
                 brush = brushOpts(id = "plot_brush")
                        )
                      )
                    )
           ),
  navbarMenu("More",
             tabPanel(
               "About",
               fluidRow(
                 column(width = 3,
                        verbatimTextOutput("click_info")),
                 column(width = 3,
                        verbatimTextOutput("dblclick_info")),
                 column(width = 3,
                        verbatimTextOutput("hover_info")),
                 column(width = 3,
                        verbatimTextOutput("brush_info"))
                      )
                  )
             )
)


server <- function(input, output) {
  df <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)  
    
  })
  
  # Print data table ----  
  output$rendered_file <- DT::renderDataTable({
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
                       selected = names(df())[[1]])
  })
  
  output$var2 <- renderUI({
    selectInput(inputId = "select_var2", 
                label = "Select Sample", 
                choices = names(df()),
                selected = names(df())[[2]])
  })
  
  output$var3 <- renderUI({
    selectInput(inputId = "select_var3", 
                label = "Select Lable", 
                choices = names(df()),
                selected = names(df())[[1]])
    
  })

  # Select columns to use ----
  # df_sel <- reactive({
  #   req(input$select_var)
  #   df_sel <- df() %>% select(input$select_var)
  # })
  
  selectedData <- reactive({
    req(input$select_var1)
    req(input$select_var2)
    req(input$select_var3)
    selectedData <- df() %>% select(input$select_var1,input$select_var2,input$select_var3)
  })
    
  output$plot1 <- renderPlot({
    if (input$plotType == "p") {
      # data("orangejuice")
      # with(orangejuice, qcc(D[trial], sizes = size[trial], type = "p", rules = 1:4))
      qcc(data = selectedData()[,1], sizes = selectedData()[,2], lables = selectedData()[,3], type = "p", rules = 1:6)
    } else if (input$plotType == "x") {
      qcc(data = selectedData()[,1], lables = selectedData()[,3], type = "xbar.one", rules = 1:6)
    }
    else if (input$plotType == "np") {
      qcc(data = selectedData()[,1], sizes = selectedData()[,2], lables = selectedData()[,3], type = "np", rules = 1:6)
    }
    else if (input$plotType == "c") {
      qcc(data = selectedData()[,1], lables = selectedData()[,3], type = "c", rules = 1:6)
    }
    else if (input$plotType == "u") {
      qcc(data = selectedData()[,1], sizes = selectedData()[,2], lables = selectedData()[,3], type = "u", rules = 1:6)
    }
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
  output$table <- renderDataTable({
    DT::datatable(orangejuice)
  })
}


shinyApp(ui, server)
