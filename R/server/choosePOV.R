# Server Output Functions for Choose POV Tab 

#### Define Threads sub-tab ####

output$selectThreads <- renderUI({
	checkboxGroupInput(
		"THREAD_CF_ID",
		"Select columns to define threads:",
		cfnames(selectOccFilter()),
		selected = get_THREAD_CF(),
		inline = TRUE
	)
})

output$ContextFlowers_Threads <- renderPlotly({
	CF_multi_pie(
		selectOccFilter(),
		get_THREAD_CF()
	)
})

#### Define Events sub-tab ####

output$selectEvents <- renderUI({
	checkboxGroupInput(
		"EVENT_CF_ID",
		"Select columns to mark events:",
		cfnames(selectOccFilter()),
		selected = get_EVENT_CF(),
		inline = TRUE
	)
})

output$ContextFlowers_Events <- renderPlotly({
	CF_multi_pie(
		selectOccFilter(),
		get_EVENT_CF()
	)
})

#### Review Data sub-tab ####

output$DisplayThreadPOV <- DT::renderDataTable({threadedOcc()})
