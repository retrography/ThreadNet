# Server output functions for readData tab

# Custom UI element to select an input file
output$inputFileSelector <- renderUI({
	tags$div(
		align = "center",
		fileInput(
			"file1",
			"Please select a .csv file",
			accept = c(
				"text/csv",
				"text/comma-separated-values,text/plain",
				".csv"
			)
		) 
	)
})

# Custom UI element to select columns for display
output$initialDataColumns <- renderUI({
	checkboxGroupInput(
		"CFcolumnsID",
		"Select columns to include in analysis:",
		cfnames(occ()),
		selected = cfnames(occ()),
		inline = TRUE
	)
})

# Display the initial dataset
output$initialDataDisplay <- DT::renderDataTable(
	selectOcc(),
	filter = "top",
	options = list(autoWidth = TRUE)
)
