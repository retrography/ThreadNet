
#### Define Threads sub-tab ####

output$timeSelector <- renderUI({
    radioButtons(
      "timeScaleID",
      "Select time scale for graphs:",
      c( 'auto', 'mins', 'hours','days','weeks' ),
      selected = 'auto',
      inline = TRUE)
})

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

########
# The POV tabs reconstruct the data into threads by sorting by tStamp and
# adding columns for threadNum and seqNum for the selected POV in ThreadOccByPOV
output$povDataThreads <- DT::renderDataTable({

  # thread occurences by POV
  threadedOcc()
},
filter = "top",
options = list(autoWidth = TRUE))


#### Add new Dataset sub-tab ####
output$addPOV1 <- renderUI({
  tags$div(
    align="left",
    textInput(
      "POVMapName",
      label = h4(paste("Enter label for this POV mapping")),
      value = paste0(knitr::combine_words(get_THREAD_CF(),sep='+'),
                     '>>',
                     knitr::combine_words(get_EVENT_CF(),sep='+'))
    ))})

output$addPOV2 <- renderUI({
  tags$div(
    align="center",
    h3('  '),
    actionButton("addPOVButton", "Save this POV",
              style = "font-size: 150%;
                 font-weight: bold;
                 height: 60px;
                 width: 200px;
                 text-align:center;
                 border-radius: 6px;
                 border-width: 2px") ) })



