# Server: Read Data tab functions

# limit what files to accept on input
fileTypes <- c("text/csv","text/comma-separated-values,text/plain",".csv")

# file selector dialog
output$fileSelector <- renderUI({
	tags$div(
		align = "center",
		fileInput("inputFile","Please select a .csv file",accept=fileTypes)
	)
})

# user selects columns to include
# [-1] drop first column (tStamp)
output$columnSelector <- renderUI({
	checkboxGroupInput(
		"CFcolumnsID",
		"Select columns to include in analysis:",
		names(occ())[-1],
		selected = names(occ())[-1],
		inline = TRUE
	)
})

# user filters data for review
output$dataFilter <- DT::renderDataTable(
	selectOcc(),
	filter  = "top",
	options = list(autoWidth = TRUE)
)
