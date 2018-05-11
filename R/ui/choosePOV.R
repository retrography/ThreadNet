tabPanel(value = "choosePOV",
    "Choose POV",
    helpText('Select columns from your data to define your point of view.'),
    tags$hr(),
	uiOutput("chunk_controls_X"), # JUST HERE FOR NOW
    tabsetPanel(
        id = "tabs",
        tabPanel(value = "defineThreads",
            "Define Threads",
            tags$h4("Threads are defined by contextual features that STAY THE SAME during a thread. At least ONE is required."),
            uiOutput("selectThreads"),
            plotlyOutput("ContextFlowers_Threads")
        ),
        tabPanel(value = "defineEvents",
            "Define Events",
            tags$h4("Events are marked by contextual features that CHANGE within the threads. At least ONE is required."),
            uiOutput("selectEvents"),
            plotlyOutput("ContextFlowers_Events")
        ),
        tabPanel(value = "showThreads",
            "Review Data",
            tags$h4("Data threaded from your chosen POV"),
            DT::dataTableOutput("DisplayThreadPOV")
        )
    )
)

