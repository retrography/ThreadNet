##########################################################################################################
# THREADNET:  Event Mappings

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

#####################################################
# GlobalEventMappings is a global variable

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# TODO: deprecate this function when no longer needed
check_map_name <- function(mapname){

    existingMap <- ifelse(mapname %in% get_event_mapping_name_list(),TRUE, FALSE)
    return(existingMap)
}

# TODO; review this
get_event_mapping_name_list <- function() {

  n <- unlist(lapply(1:length(GlobalEventMappings),function(i){
    unlist(GlobalEventMappings[[i]][["name"]]) }))

  return(n)
}

store_event_mapping <- function(EventMapName, EventMap){

	# TODO: do validation here

	# Validate the there is an actual EventMapName supplied (no blanks)
	# Validate that the EventMapName is not already in the list
	# Validate that the eventMap itself is not identical to anything already in the list (? -- confirm with Brian if this check is necessary)

	# TODO: do validation here

	# Validate the there is an actual EventMapName supplied (no blanks)
	# Validate that the EventMapName is not already in the list
	# Validate that the eventMap itself is not identical to anything already in the list (? -- confirm with Brian if this check is necessary)

  # Add the mapping to the global list of mappings. Sort by threadNum and seqNum
  em = list(name = paste(EventMapName), threads = e[order(e[['threadNum']],e[['seqNum']]),])

  	GlobalEventMappings <<- append(list(em), GlobalEventMappings )

	# TODO: return ok/fail
}

get_event_mapping_threads <- function(mapname){

  	# get the index for the mapname
	idx <- get_event_index(mapname)

  	result <- ifelse(idx==0, NULL, GlobalEventMappings[[idx]][["threads"]])

	return(result)
}

delete_event_mapping <- function(mapname){

  	# get the index for the mapname
	idx <- get_event_index(mapname)

  	GlobalEventMappings[[idx]] <<- NULL

}

export_event_mapping_rdata <- function(mapname){

  nicename <- paste0("EventMap_",mapname)

  assign(nicename, get_event_mapping_threads(mapname))

  save(list=nicename, file = paste0(nicename,".Rdata"))

}

export_event_mapping_csv <- function(mapname){

  output <- as.data.frame(get_event_mapping_threads(mapname))

  output[grep('V_',colnames(output))] <- NULL

  write.csv(output, file=file.choose(), quote = TRUE, row.names = FALSE)

}
