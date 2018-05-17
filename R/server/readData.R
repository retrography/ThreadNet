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
output$columnSelector <- renderUI({
	checkboxGroupInput(
		"CFcolumnsID",
		"Select columns to include in analysis:",
		# TODO: better way than "cfnames" but need to address occ() first
		cfnames(occ()),
		selected = cfnames(occ()),
		inline = TRUE
	)
})

# user filters data for review3
output$dataFilter <- DT::renderDataTable(
	selectOcc(), # TODO: this calls occ() -- review
	filter = "top",
	options = list(autoWidth = TRUE)
)
