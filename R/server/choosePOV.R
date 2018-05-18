# Server Output Functions for Choose POV Tab 

#### Define Threads sub-tab ####

output$povThreadSelector <- renderUI({
	checkboxGroupInput(
        "THREAD_CF_ID",
        "Select columns to define threads:",
		get_CF(),
        selected = "",
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

output$povEventSelector <- renderUI({
	checkboxGroupInput(
        "EVENT_CF_ID",
        "Select columns to mark events:",
		get_CF(),
		selected = "",
        inline = TRUE
    )
})

output$ContextFlowers_Events <- renderPlotly({
	CF_multi_pie(
		selectOccFilter(),
		get_EVENT_CF()
	)
})

#### Preview Data sub-tab ####

# The POV tabs reconstruct the data into threads by sorting by tStamp and
# adding columns for threadNum and seqNum for the selected POV in ThreadOccByPOV
output$povDataThreads <- DT::renderDataTable({

	# don't call ThreadedOccByPOV unless inputs have been defined
	validate(need(get_THREAD_CF() != "", "You must select at least one Thread"))
	validate(need(get_EVENT_CF()  != "", "You must select at least one Event"))
	
	# we have inputs: call the fuction to thread occurences by selected POV
	ThreadOccByPOV(selectOccFilter())
})
