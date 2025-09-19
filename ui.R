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
    width = 250,
    div(
      class = "sidebar-section",
      h4("Resources", class = "sidebar-title"),
      tags$a(
        "📖 Documentation",
        href= "resources/IntegrityAnalysis.docx",
        download = "Integrity Analysis.docx",
        class = "btn btn-default"
      ),
      tags$a(
        "📋 Data Template",
        href= "resources/Template.xlsx",
        download = "Template for Integrity Analysis.xlsx",
        class = "btn btn-default"
      ),
      tags$a(
        "🧪 Sample Data",
        href= "resources/Example.xlsx",
        download = "Example for Integrity Analysis.xlsx",
        class = "btn btn-default"
      )
    ),
    div(
      class = "sidebar-section",
      h4("How It Works", class = "sidebar-title"),
      p("Upload your RCT baseline data to detect potential integrity issues using Carlisle's Monte Carlo method.")
    ),
    div(
      class = "sidebar-section",
      h4("About This Tool", class = "sidebar-title"),
      p("Developed from John Carlisle's analysis of fraudulent research studies (references 2012, 2015, and 2017, see documentation) using the Monte Carlo approach developed by John Carlisle and Steve Shafer.")
    ),

    div(
      class = "sidebar-section contact-section",
      "Questions/feedback?",
      tags$a(
        "Contact Steve Shafer",
        href = "mailto:steven.shafer@stanford.edu?subject=Question about RCT Integrity Analysis app",
        target = "_blank"
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    shinyjs::useShinyjs(),
    fluidRow(
      column(
        12,
        fileInput("upload", "Select data entry spreadsheet (csv, xls, or xlsx)", accept = c(".csv", ".xls", ".xlsx")),
        uiOutput("GoButton"),
        uiOutput("logContent"),
        uiOutput("downloadButton")
      )
    ),
    uiOutput("stopButton")
  )
)
