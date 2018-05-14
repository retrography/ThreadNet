# Functions related to GlobalEventMappings

#' Checks the name attempting to be create against the list of
#' map names in memory and forces the creation of a new name.
#'
#' @param mapname name of map attempting to be created

# TODO:
# Create, Delete, Export_Rdata, Export_Csv -- all need to validate whether entry exist before running

check_map_name <- function(mapname){

	# TODO: clean this up

    if (mapname %in% get_event_mapping_name_list()){
      existingMap = TRUE
    } else {
      existingMap = FALSE
    }
    return(existingMap)
  }

get_event_mapping_name_list <- function(){

  n <- unlist(lapply(1:length(GlobalEventMappings),function(i){
    unlist(GlobalEventMappings[[i]][["name"]]) }))

  return(n)
  }


add_event_mapping <- function(eventMap) {

	# this will be where we actually add this list

	# first validate that eventMap['name'] is not in event_map_names
	# using check_map_name

	# then add the mapping

	GlobalEventMappings <<- append(list(eventMap), GlobalEventMappings)

}


# TEMP -- REMOVE THIS WHEN DEPRECATED
# FIND EVERYTHING THAT CALLS THIS, SEE THAT IT GETS THE RETURN, AND USE THE ABOVE FUNCTION
store_event_mapping <- function(EventMapName, e){

  # Add the mapping to the global list of mappings. Sort by threadNum and seqNum
  em = list(name = paste(EventMapName), threads = e[order(e[['threadNum']],e[['seqNum']]),])

  # GlobalEventMappings <<- append(list(em), GlobalEventMappings )

  return(em)

}

get_event_mapping_threads <- function(mapname){

	# TODO: review this function

  idx <- which(mapname==get_event_mapping_name_list() )

  if (idx==0) {
    return(NULL)
  } else {
  	return(GlobalEventMappings[[idx]][["threads"]])
	}
}

delete_event_mapping <- function(mapname){

	# TODO: validate that mapname exists via check_map_name

  # get the index for the mapname
  idx <- which(mapname==get_event_mapping_name_list())

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
