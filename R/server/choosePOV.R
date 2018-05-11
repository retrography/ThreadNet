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

# put "save dataset" button and text input here
# the thing to save will be "newEventMap"
# TODO: update ui/choosePOV.R accordingly; and update server.R with function to run the saveEventMap function from Event_Mappings.R
# mapping name and go button
# TODO: prevent button from working if nothing is selected / no eventMap var created yet
output$chunk_controls_X <- renderUI({
    tags$div(
        align = "left",
        textInput("EventMapNameX", label = h4("Enter label to save result")),
        actionButton("EventButtonX", "Create New Mapping"),
        span(textOutput("EventValidateX"), style="color:red")
    )
})


output$DisplayThreadPOV <- DT::renderDataTable({generateBaseThreadOcc()})
