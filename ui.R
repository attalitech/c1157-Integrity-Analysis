# Testing Baseline RCT Values for Fraud / Error
# August  2025 

###############################
# UI                          #
###############################

ui <- fluidPage(
  useShinyjs(),
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
    HTML(
      "<div style = 'font-size: 14px; font-weight: bold;'>
      &nbsp&nbspEvaluation of Baseline Data Integrity<br>
      </div>"
      ),
    HTML(
      "<div style = 'font-size: 12px;'>
      &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbspCarlisle Shafer 'Monte Carlo' approach,
      Anaesthesia 2015;70:848-58
      </div>"),
    style = 'border-bottom: 1px solid'
  ),
  fluidRow(
    HTML(
      "<div style = 'font-size: 12px;'>
      &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbspSpreadsheet Columns
      </div>")
  ),
  fixedRow(
    column(
      1
    ),
    column(
      1,
      HTML(
        "<div style = 'font-size: 11px; font-weight: bold;'>
        Name </div>
        <div style = 'font-size: 10px;'>
        TRIAL<br>
        ROW<br>
        N<br>
        MEAN<br>
        SD<br>
        ROUND</div>"
        )
    ),
    column(
      1,
      HTML(
        "<div style = 'font-size: 11px; font-weight: bold;'>
        Type </div>
        <div style='font-size: 10px;'> 
        alphanumeric<br>
        alphanumeric<br>
        integer<br>
        floating<br>
        floating<br>
        integer</div>"
      )
    ),
    column(
      1,
      HTML(
        "<div style = 'font-size: 11px; font-weight: bold;'>
        Mandatory</div>
        <div style='font-size: 10px;'> 
        No<br>
        Yes<br>
        Yes<br>
        Yes<br>
        Yes<br>
        No</div>"
      )
    ),
    column(
      5,
      HTML(
        "<div style = 'font-size: 11px; font-weight: bold;'>
        Description</div>
        <div style='font-size: 10px;'> 
        Unique trial identifier (not needed if only 1 trial)<br>
        Table row (e.g., 'weight', 'age')<br>
        Number of subjects for specific row entry<br>
        Mean<br>
        Standard deviation (NOT SEM)<br>
        Decimal rounding</div>"
      )
    )
  ),
  fixedRow(
    column(
      1
    ),
    column(
      11,
      HTML(
        "<div style='font-size: 5px;'>
        <br>
        </div>
        <div style='font-size: 11px;'> 
        Categorical variables are handled as additional columns, with 
        the name of the column corresponding to the category (e.g., 'M' and
        'F'). The number of columns should equal the number of categories 
        in the analysis.</div>
        "
      )
    ),
    style = 'border-bottom: 1px solid'
  ),  
  fluidRow(
    column(
      8,
      HTML("<br>Select Input File (csv, xls, or xlsx)<br>"),
      fileInput("upload", NULL, accept = c(".csv", ".xls", ".xlsx")),
      uiOutput("message"),
      uiOutput("downloadButton"),
      #downloadButton("download", "Download Results")
    ),
    column(
      4,
      HTML("<br><br>"),
      downloadButton("template", "Download Spreadsheet Template"),
      HTML("<br><br>"),
      downloadButton("example", "Download Example Spreadsheet")
    )
  ),
  br(),
  actionBttn("stop", HTML("&nbsp &nbsp EXIT &nbsp &nbsp"), style = "gradient", size = "xs", color = "warning"),
  br()
)
