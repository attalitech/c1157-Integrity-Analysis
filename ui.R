# Testing Baseline RCT Values for Fraud / Error
# August  2025 

###############################
# UI                          #
###############################

ui <- 
  dashboardPage(
    title = "RCT Integrity Analysis",
    dashboardHeader(
      title = 
        div(
          h3(
            "Evaluation of Baseline Data Integrity", 
            style="margin: 0;"
            ), 
          h4(
            "Carlisle Shafer 'Monte Carlo' approach", 
            style="margin: 0;"
            )
          ),
      titleWidth = "100%"
      ),
    dashboardSidebar(
      collapsed = FALSE,
      title = "Instructions",
      tags$style(".skin-blue .sidebar .shiny-download-link { color: #444; }"),
      tags$style(".sidebar { height: 10px; }"),
      p(),
      downloadButton("documentation", "Download Documentation"),
      p(),
      downloadButton("template", "Download Template"),
      p(),
      downloadButton("example", "Download Example"),
      br(),
      br(),
      br(),
      h6(
        "Developed from John Carlisle's analysis of fraudulent research studies ",
        "(references 2012, 2015, and 2017, see documentation) using the Monte Carlo approach",
        "developed by John Carlisle and Steve Shafer."
        ),
      br(),
      HTML(
        '<p>
        <h6>Please direct questions and feedback to Steve Shafer at
        <a href="mailto:steven.shafer@stanford.edu">steven.shafer@stanford.edu</a>
        .
        </h6>
        </p>'
        )
      ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$script(src = "app.js"),
      tags$head(tags$link(href = "app.css", rel = "stylesheet")),
      style = "max-height: 95vh; overflow-y: auto;" ,
      tags$head(
        tags$style(
          type="text/css", 
          "#inline label { 
          display: table-cell; 
          text-align: center; 
          vertical-align: middle; 
          } 
        #inline .form-group {
        display: table-row;
        }"
        )
      ),
      fluidRow(
        img(
          src='Table.png', align = "right", width = "100%"
        ),
        style = 'border-bottom: 1px solid; padding-left: 5%; padding-right: 5%; padding-bottom: 2%'
      ),  
      fluidRow(
        column(
          12,
          HTML("<br>Select data entry spreadsheet (csv, xls, or xlsx)<br>"),
          fileInput("upload", NULL, accept = c(".csv", ".xls", ".xlsx")),
          uiOutput("GoButton"),
          uiOutput("logContent"),
          uiOutput("downloadButton")
        )
      ),
      uiOutput("stopButton")
    )
  )
