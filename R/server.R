##########################################################################################################
# THREADNET:SHINY SERVER

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

server <- shinyServer(function(input, output, session) {

	# hide tabs on startup
	# make these conditional? (see note below)
	observe({
    	hide(selector = "#navbar li a[data-value=choosePOV]")
    	hide(selector = "#navbar li a[data-value=visualize]")
    	hide(selector = "#navbar li a[data-value=subsets]")
    	hide(selector = "#navbar li a[data-value=comparisons]")
    	hide(selector = "#navbar li a[data-value=movingWindow]")
    	hide(selector = "#navbar li a[data-value=parameterSettings]")
	})

	# create reactive value to force execution of function that gets map names for menus
	rv <- reactiveValues(newmap=0)

	# add reactive value to force update
	get_event_mapping_names <- reactive({
		rv$newmap
		get_event_mapping_name_list()
	})

	# Global variables references throughout app
	# TODO: review where these are called
	get_CF            <<- reactive({ return(input$CFcolumnsID)  })
	get_THREAD_CF     <<- reactive({ return(input$THREAD_CF_ID) })
	get_EVENT_CF      <<- reactive({ return(input$EVENT_CF_ID)  })
	get_timeScale     <<- reactive({ return(input$timeScaleID)  })
	get_COMPARISON_CF <<- reactive({ return(setdiff(get_CF(), union(get_THREAD_CF(),get_EVENT_CF() ))) })

	# Generate sliders to control the zoom level for zooming in-out
	get_Zoom <- function(inputID,zoomID){zoom <- ifelse(zoom_upper_limit(get_event_mapping_threads(inputID))==1,"ZM_1",paste0("ZM_",zoomID)) }

	# TODO: just call get_event_mapping_threads() on the reactive input object wherever these are called
	# Remove these when no longer needed
	freqNgramInputEvents  <- get_event_mapping_threads(freqNgramInputMapID())
	subsetEventsViz       <- get_event_mapping_threads(SelectSubsetMapInputID())
	threadedEventsComp_A  <- get_event_mapping_threads(CompareMapInputID_A())
	threadedEventsComp_B  <- get_event_mapping_threads(CompareMapInputID_B())
	threadedEventsDiaComp <- get_event_mapping_threads(DiaCompareMapInputID())
	threadedEventsMove    <- get_event_mapping_threads(MovingWindowMapInputID())
	threadedEventsViz_ALL <- get_event_mapping_threads(VisualizeEventMapInputID())

	# Input Variable Accessors
	ChunkInputMapID          <<- reactive({ return(input$ChunkInputMapID         ) })
	chunkZoomID              <<- reactive({ return(input$chunkZoomID             ) })
	CompareMapInputID_A      <<- reactive({ return(input$CompareMapInputID_A     ) })
	CompareMapInputID_B      <<- reactive({ return(input$CompareMapInputID_B     ) }) 
	CompareZoomID_A          <<- reactive({ return(input$CompareZoomID_A         ) })
	CompareZoomID_B          <<- reactive({ return(input$CompareZoomID_B         ) })
	DiaCompareMapInputIDm    <<- reactive({ return(input$DiaCompareMapInputID    ) })
	DiaCompareZoomID         <<- reactive({ return(input$DiaCompareZoomID        ) })
	freqNgramInputMapID      <<- reactive({ return(input$freqNgramInputMapID     ) })
	freqNgramZoomID          <<- reactive({ return(input$freqNgramZoomID         ) })
	MovingWindowMapInputID   <<- reactive({ return(input$MovingWindowMapInputID  ) })
	MovingWindowZoomID       <<- reactive({ return(input$MovingWindowZoomID      ) })
	RegExInputMapID          <<- reactive({ return(input$RegExInputMapID         ) })
	regexZoomID              <<- reactive({ return(input$regexZoomID             ) })
	VisualizeEventMapInputID <<- reactive({ return(input$VisualizeEventMapInputID) })
	VisualizeTabZoomID       <<- reactive({ return(input$VisualizeTabZoomID      ) })


	####################################
	# Functions for reading Input Data #
	####################################

	# dataframe for occurrences that are read in from inputFile
	occ <- eventReactive(input$inputFile,read_occurrences(input$inputFile))

	# selected columns from the raw data
	selectOcc <- reactive(occ()[c("tStamp", input$CFcolumnsID)])

	###############################################
	# Functions for Generating Base Threaded View #
	###############################################

	# select rows using the nice DT input
    # this depends on initialDataDisplay defined in server/readData.R
	# TODO: anything in later tabs that calls selectOccFilter should be updated to depend on eventMap, not inputFile
    selectOccFilter <- reactive(selectOcc()[input$initialDataDisplay_rows_all,])

	# The POV tabs reconstruct the data into threads by sorting by tStamp and
    # adding columns for threadNum and seqNum for the selected POV in ThreadOccByPOV
	# TODO: this is no longer reactive, can be moved to another file (eg server/choosePOV)
	generateBaseThreadOcc <- function() {

		# validate that Thread and Event have been selected
		validate(need(get_THREAD_CF() != "", "You must select at least one Thread"))
        validate(need(get_EVENT_CF()  != "", "You must select at least one Event"))

		# This is the processed newEventMap data to display
		newThreadData <- ThreadOccByPOV(selectOccFilter(), get_THREAD_CF(), get_EVENT_CF())

		# once everything is returned, show the other tabs
		# TODO: make these conditional panels based on GlobalEventList has at least one element?
		shinyjs::show(selector = "#navbar li a[data-value=visualize]")
  	 	shinyjs::show(selector = "#navbar li a[data-value=subsets]")
   		shinyjs::show(selector = "#navbar li a[data-value=comparisons]")
   		shinyjs::show(selector = "#navbar li a[data-value=movingWindow]")
   		shinyjs::show(selector = "#navbar li a[data-value=parameterSettings]")

		return(newThreadData)
	}

	#############################
	# TODO: functions to review #
	#############################

	# get the data that will be the input for this tab
	chunkInputEvents <- reactive({
		rv$newmap
		get_event_mapping_threads(input$ChunkInputMapID)
	})

	# get the input values and return data frame with regex & label
	regexInput <- reactive({
		data.frame(
			pattern <- unlist(lapply(1:input$numRegexInputRows,function(i){input[[paste0('regex', i)]]})),
			label   <- unlist(lapply(1:input$numRegexInputRows,function(i){input[[paste0('regexLabel', i)]]})),
			stringsAsFactors = FALSE
		)
	})


	fng_select <- reactive(
		support_level(
			thread_text_vector(freqNgramInputEvents(),'threadNum',get_Zoom(freqNgramInputMapID(),freqNgramZoomID()),' '),
			frequent_ngrams(
				freqNgramInputEvents() ,
				'threadNum',
				get_Zoom(freqNgramInputMapID(),freqNgramZoomID()),
				input$freqNgramRange[1],
				input$freqNgramRange[2],
				TRUE
			)
		)
	)

	# The bottom example shows a server-side table. Make sure you have included row names in the table (as the first column of the table).
	# In the case of server-side processing, the row names of the selected rows are available in input$x3_rows_selected as a character vector.

	selected_ngrams <- reactive({
		s <- as.integer(input$freqnGramTable_rows_selected)
		data.frame(
			pattern <- unlist(lapply(1:length(s),function(i){ str_replace_all(fng_select()[i,'ngrams'],' ',',') })),
			label   <- unlist(lapply(1:length(s),function(i){paste0("<",str_replace_all(fng_select()[i,'ngrams'],' ','_'),">")})),
			stringsAsFactors=FALSE
		)
	})

	CF_levels <- reactive( get_CF_levels(threadedEventsDiaComp(),input$selectComparisonID))

	# Get data for the Moving Window tab.
	threadedEventsMove_A <- reactive({get_moving_window(threadedEventsMove(),input$MovingWindowSizeID,input$WindowLocation_A_ID) })
	threadedEventsMove_B <- reactive({get_moving_window(threadedEventsMove(),input$MovingWindowSizeID,input$WindowLocation_B_ID ) })

	threadedEventsViz <- reactive({
		loc   <- input$VisualizeRangeID[1]
		width <- input$VisualizeRangeID[2] - input$VisualizeRangeID[1]
		get_moving_window(threadedEventsViz_ALL(),width,loc)
	})

	################################
	# Button click event functions #
	################################

	# Test button for "new map" on choosePOV
	observeEvent(input$EventButtonX,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		isolate(
			add_event_mapping(newEventMap)
		)
		output$EventValidateX <- renderText("OK")
	}, ignoreInit = TRUE)

	observeEvent( input$EventButton2,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		isolate(
			OccToEvents_By_Chunk(
				chunkInputEvents(),
				input$Chunks_method_Button, # which method?
				input$EventMapName2,
				input$fixed_chunk_size,
				input$chunk_time_gap_threshold,
				'mins',
				input$chunk_CFs,
				get_EVENT_CF(),
				get_COMPARISON_CF()
			)
		)
		#output$EventValidate2 = renderText(paste('New map named', input$EventMapName2 ,'has been created'))
	}, ignoreInit = TRUE )

	observeEvent(input$EventButton3, {
		rv$newmap <- rv$newmap+1 # trigger reactive value
		isolate(
			OccToEvents3(
				get_event_mapping_threads(RegExInputMapID())
				input$EventMapName3,
				get_EVENT_CF(),
				get_COMPARISON_CF(),
				'threadNum',
				get_Zoom(RegExInputMapID(),regexZoomID()),
				regexInput(),
				input$KeepIrregularEvents
			)
		)
	  #output$EventValidate3 = renderText(paste('New map named', input$EventMapName3 ,'has been created'))
	}, ignoreInit = TRUE )

	observeEvent(input$EventButton4,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		isolate(
			OccToEvents3(
				freqNgramInputEvents(),
				input$EventMapName4,
				get_EVENT_CF(),
				get_COMPARISON_CF(),
				'threadNum',
				get_Zoom(freqNgramInputMapID(),freqNgramZoomID()),
				selected_ngrams(),
				input$KeepIrregularEvents_2
			)
		)
		#output$EventValidate4 = renderText(paste('New map named', input$EventMapName4 ,'has been created'))
	}, ignoreInit = TRUE)

	cluster_result <- eventReactive(input$EventButton6,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		isolate(
			# TODO: review this -- need to explicitly call function to add event
			clusterEvents(
				get_event_mapping_threads(input$ClusterEventsInputID),
				input$EventMapName6,
				input$ClusterMethodID,
				get_EVENT_CF(),
				'cluster'
			)
		)
	}, ignoreInit = TRUE )

	observeEvent(input$SelectSubsetButton, {
		rv$newmap <- rv$newmap+1 # trigger reactive value
	    store_event_mapping(input$SelectSubsetMapName, subsetEventsViz()[input$SelectSubsetDataTable_rows_all,])
	    #output$SelectSubsetValidate <- renderText(paste('New map named', input$SelectSubsetMapName ,'has been created'))
	}, ignoreInit = TRUE)

	# Delete an Event Mapping
	# TODO: if delete all, then go back to choosePOV and hide other tabs
	observeEvent(input$DeleteMappingButton,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		delete_event_mapping(input$ManageEventMapInputID)
		output$delete_confirm <- renderText(paste(input$ManageEventMapInputID, " deleted."))
	}, ignoreInit = TRUE)

	# Export an Event Mapping as RData
	observeEvent(input$ExportMappingRData,{
		export_event_mapping_rdata(input$ManageEventMapInputID )
		output$action_confirm <- renderText(paste(input$ManageEventMapInputID, " exported as .RData file"))
	})

	# Export an Event Mapping as CSV
	observeEvent(input$ExportMappingCsv,{
		export_event_mapping_csv(input$ManageEventMapInputID)
		output$action_confirm <- renderText(paste(input$ManageEventMapInputID, " exported as .csv file"))
	})

	###################################################
	# Tab-specific server output function definitions #
	###################################################

	source(file.path("server", "readData.R"),          local = TRUE)$value
	source(file.path("server", "choosePOV.R"),         local = TRUE)$value
	source(file.path("server", "comparisons.R"),       local = TRUE)$value
	source(file.path("server", "visualize.R"),         local = TRUE)$value
	source(file.path("server", "subsets.R"),           local = TRUE)$value
	source(file.path("server", "movingWindow.R"),      local = TRUE)$value
	source(file.path("server", "parameterSettings.R"), local = TRUE)$value
})
