# UI layout definition for readData tab

tabPanel(
	value = "readData",
    "Read Data",
    helpText('Select a file that contains your data.'),
    tags$hr(),
    uiOutput("inputFileSelector"),
    uiOutput("initialDataColumns"),
    DT::dataTableOutput("initialDataDisplay")
)
