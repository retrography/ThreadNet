##########################################################################################################
# THREADNET:  Core functions

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# Main function to convert threads to a network
# Converts a sequentially ordered streams of ;events (threads) and creates a unimodal, unidimensional network.
# Sequentially adjacent pairs of events become edges in the resulting network.
# returns a list containing two dataframes, one for the nodes (nodeDF) and one for the edges (edgeDF)
threads_to_network <- function(et,TN,CF,timesplit){

	# et dataframe containing threads
	# TN name of column in dataframe that contains a unique thread number for each thread
	# CF name of the column in dataframe that contains the events that will form the nodes of the network
	# timesplit time measure

  et$time <- et[[timesplit]]

  # First get the node names & remove the spaces
  node_label <- unique(et[[CF]])
  node_label <- str_replace_all(node_label," ","_")

  # set up the data frames we need to draw the network
  nodes = data.frame(
    id = 1:length(node_label),
    label = node_label,
    title=node_label)

  node_position_y = data.frame(table(et[[CF]]))
  colnames(node_position_y) <- c('label', 'y_pos')
  node_position_x = aggregate(et$time, list(et[[CF]]), mean)
  colnames(node_position_x) <- c('label', 'x_pos')

  nodes = merge(nodes, node_position_y, by=c("label"))
  nodes = merge(nodes, node_position_x, by=c("label"))

  # get the 2 grams for the edges
  ngdf = count_ngrams(et,TN, CF, 2)

  # need to split 2-grams into from and to
  from_to_str = str_split(str_trim(ngdf$ngrams), " ", n=2)

  # need to find a better way to do this...
  nEdges = length(from_to_str)
  from_labels=matrix(data="", nrow=nEdges,ncol=1)
  to_labels =matrix(data="", nrow=nEdges,ncol=1)
  from=integer(nEdges)
  to=integer(nEdges)
  for (i in 1:length(from_to_str)){

    # Get from and to by spliting the 2-gram
    from_labels[i] = str_split(from_to_str[[i]]," ")[1]
    to_labels[i] = str_split(from_to_str[[i]]," ")[2]

    # use match to lookup the nodeID from the label...
    from[i] = match(from_labels[i], nodes$label)
    to[i] = match(to_labels[i], nodes$label)
  }

  edges = data.frame(
    from,
    to,
    label = paste(ngdf$freq)
  )

  edges = merge(edges, nodes[,c('id', 'y_pos', 'x_pos')], by.x=c('from'), by.y=c('id'))
  edges = merge(edges, nodes[,c('id', 'y_pos', 'x_pos')], by.x=c('to'), by.y=c('id'))
  colnames(edges)<-c('from', 'to', 'label', 'from_y', 'from_x', 'to_y', 'to_x')
  return(list(nodeDF = nodes, edgeDF = edges))
}

# here is a version without all the position stuff, which should be separated out, if possible.
# Added in the "group" for the network graphics - default group is 'threadNum' because it will always be there
threads_to_network_original <- function(et,TN,CF,grp='threadNum'){

  # First get the node names & remove the spaces
  node_label = levels(factor(et[[CF]]))  # unique(et[[CF]])
  node_label=str_replace_all(node_label," ","_")
  nNodes = length(node_label)

  node_group=character()
  for (n in 1:nNodes){node_group = c(node_group, as.character(unlist( et[which(et[[CF]]==node_label[n]),grp][1]) ) )}

  # set up the data frames we need to draw the network
  nodes = data.frame(
    id = 1:length(node_label),
    label = node_label,
    Group = node_group,
    title=node_label)

  # get the 2 grams for the edges
  ngdf = count_ngrams(et,TN, CF, 2)

  # Adjust the frequency of the edges to 0-1 range
  ngdf$freq = round(ngdf$freq/max(ngdf$freq),3)

  # need to split 2-grams into from and to
  from_to_str = str_split(str_trim(ngdf$ngrams), " ", n=2)

  # need to find a better way to do this...
  nEdges = length(from_to_str)
  from_labels=matrix(data="", nrow=nEdges,ncol=1)
  to_labels =matrix(data="", nrow=nEdges,ncol=1)
  from=integer(nEdges)
  to=integer(nEdges)
  for (i in 1:length(from_to_str)){

    # Get from and to by spliting the 2-gram
    from_labels[i] = str_split(from_to_str[[i]]," ")[1]
    to_labels[i] = str_split(from_to_str[[i]]," ")[2]

    # use match to lookup the nodeID from the label...
    from[i] = match(from_labels[i], nodes$label)
    to[i] = match(to_labels[i], nodes$label)
  }

  edges = data.frame(from,to,
    label = ngdf$freq,
    Value =ngdf$freq) %>% filter(!from==to)

  return(list(nodeDF = nodes, edgeDF = edges))
}

# Make new threads from a user defined POV
# Take the raw occurrences from the input file and sort them by time stamp within
# a set of contextual factors that remain constant for each thread
# and return dataframe containing the same occurrences sorted from a different point of view
ThreadOccByPOV <- function(threadData){

	# threadData is the cleaned occurences list
	# THREAD_CF is a list of 1 or more context factors that define the threads (and stay constant during each thread)
	# EVENT_CF  is a list of 1 or more context factors that define events (and change during threads)

	THREAD_CF <- get_THREAD_CF()
	EVENT_CF  <- get_EVENT_CF()

  	withProgress(message = "Creating Events", value = 0,{
	
		# we define 5 stages for progress updates
    	n <- 5

		# Increment stage
    	incProgress(1/n)

		threadPOV <- paste0(THREAD_CF,collapse="_")
		eventPOV  <- paste0(EVENT_CF,collapse="_")

		# Only run if THREAD_CF contains more than one column reference
		if(length(THREAD_CF)>1){
			CF_Cols  <- threadData[,THREAD_CF]
			df_args  <- c(CF_Cols, sep="+")
			combined <- do.call(paste, df_args)
			threadData[[threadPOV]] <- combined
		}

		# Only run if EVENT_CF > 1 and != THREAD_CF
		if(length(EVENT_CF)>1 & EVENT_CF != THREAD_CF){
			CF_Cols  <- threadData[,EVENT_CF]
			df_args  <- c(CF_Cols, sep="+")
			combined <- do.call(paste,df_args)
			threadData[[eventPOV]] <- combined
		}

		# temp holder -- can move all "occ" below to threadData; or just call this "occ" from the beginning
		occ <- threadData

		# sort -- review if this is necessary
    	occ <- occ[order(occ[threadPOV],occ$tStamp),]

		###########

    	# add two columns to the data frame
		# TODO: review why add columns containing the row count?
		rowCount      <- integer(nrow(occ))
    	occ$threadNum <- rowCount
    	occ$seqNum    <- rowCount

    	# add new column called label - just copy the new combined event_CF column
		# TODO: review this
    	occ$label <- occ[[eventPOV]]

    	# occurrences have zero duration
    	occ$eventDuration <- 0

    	# add columns for the time gaps that appear from this POV
    	occ$timeGap <- diff_tStamp(occ$tStamp)

    	# create new column for relative time stamp.
		# Initialize to absolute tStamp and adjust below
    	occ$relativeTime <- lubridate::ymd_hms(occ$tStamp)

    	# then get the unique values in that POV
    	occ[threadPOV] <- as.factor(occ[,threadPOV])
    	pov_list       <- levels(occ[[threadPOV]])

		# Increment stage
        incProgress(2/n)

    	# now loop through the pov_list and assign values to the new columns
    	start_row <- 1
    	thrd      <- 1

    	for(p in pov_list) {

      		# get the length of the thread
      		tlen <- sum(occ[[threadPOV]]==p)

      		# guard against error
      		if(tlen>0) {

        		# compute the index of the end row
        		end_row <- start_row + tlen - 1

        		# they all get the same thread number and incrementing seqNum
        		occ[start_row:end_row, "threadNum"] <- as.matrix(rep(as.integer(thrd),tlen))
        		occ[start_row:end_row, "seqNum"]    <- as.matrix(c(1:tlen))

        		# find the earliest time value for this thread
        		start_time <- min(lubridate::ymd_hms(occ$tStamp[start_row:end_row]))

        		# increment the counters for the next thread
        		start_row <- end_row + 1
        		thrd      <- thrd + 1
    		} # tlen>0
		}

		# Increment stage
    	incProgress(3/n)

    	# split occ data frame by threadNum to find earliest time value for that thread
    	# then substract that from initiated relativeTime from above
    	occ_split <- lapply(split(occ, occ$threadNum), function(x) {x$relativeTime <- x$relativeTime - min(lubridate::ymd_hms(x$tStamp)); x})

    	# row bind data frame back together
    	occ <- data.frame(do.call(rbind, occ_split))

    	#  these are just equal to the row numbers -- one occurrence per event
    	occ["occurrences"] <- 1:nrow(occ)

    	# now go through and change each of the CF values to a vector (0,0,0,1,0,0,0,0)
    	for (cf in EVENT_CF){

    		# make a new column for each CF
    	  	VCF <- paste0("V_",cf)
    	  	occ[[VCF]] <- vector(mode = "integer",length=nrow(occ))

    	  	for (r in 1:nrow(occ)){
				occ[[r,VCF]] <- list(convert_CF_to_vector(occ,cf,r))
			}
    	}

		# Increment stage
    	incProgress(4/n)

		# return events with network cluster added for zooming...
		# NOTE: no eventmap stored here any more
		# TODO: Need to add button on "Review Data" tab to explicitly name and add this to the list
		# e contains eventMap and clust from clusterEvents
    	e <- clusterEvents(occ, 'Network Proximity')

		results <- e[[1]] # [1] to get the eventMap

		# Increment stage
    	incProgress(5/n)

  	}) # end progress bar

   incProgress(5/n)

  return( e )

}


##############################################################################################################
#' Maps occurrences into events
#'
#' Thus function provides a place to map occurrences into events, so is is not necessary to interpret individual
#' occurrences in isolation.  There are many ways to accomplish this mapping.
#' @family ThreadNet_Core
#'
#' @param  o  a dataframe of occurrences
#' @param m = method parameter = one of c('Variable chunks','Uniform chunks')
#' @param EventMapName = used to store this mapping for visualization and comparison
#' @param uniform_chunk_size = used to identify breakpoints -- from input slider
#' @param tThreshold = used to identify breakpoints -- from input slider
#' @param timescale hours, min or sec
#' @param chunk_CF - context factors used to delineate chunks
#' @param EVENT_CF - context factors used to define events
#' @param compare_CF = context factors used for comparison -- need to be copied over here when the thread is created.
#'
#' @result event data frame, with occurrences aggregated into events.
#'
#' @export


# new version containing more ways to create chunks -- uses concepts from original prototype, but better implementation
# chunk by handoff, time gap and handoff gap
OccToEvents_By_Chunk <- function(o, m, EventMapName, uniform_chunk_size, tThreshold, chunk_CF){

	event_CF <- get_EVENT_CF()
    compare_CF <- get_COMPARISON_CF()

	timescale <- 'mins'

  # o = table of occurrences
  # m = method parameter =  c( "Handoffs", "Time Gap","Fixed Size")
  # uniform_chunk_size = used to identify breakpoints -- from input slider
  # tThreshold = used to identify breakpoints -- from input slider
  # EventMapName = used to store this mapping in an environment


  # Only run if eventMapName is filled in
  if (EventMapName =="") {return(data.frame()) }


  #### First get the break points between the events
 # Ideally, these should operate WITHIN each thread, not on the whole set of occurrences...
  # Add RLE -- consecutive runs -- as a way to chunk -- let user pick the CFs
  # very similar to the changes algorithm...
# choices = c( "Changes", "Time Gap","Fixed Size"),
  if (m=="Changes"){
    o$handoffGap =  diff_handoffs(o[chunk_CF])
    breakpoints = which(o$handoffGap == 0)
  } else if (m=="Time Gap") {
    breakpoints = which(o$timeGap > tThreshold)
  } else if (m=="Fixed Size") {
    breakpoints = seq(1,nrow(o),uniform_chunk_size)
  }


  # Grab the breakpoints from the beginning of the threads as well
  threadbreaks = which(o$seqNum == 1)
  breakpoints = sort(union(threadbreaks,breakpoints))


  ### Use the break points to find the chunks -- just store the index back to the raw data
  nChunks = length(breakpoints)

  # make the dataframe for the results.  This is the main data structure for the visualizations and comparisons.
  e = make_event_df(event_CF, compare_CF, nChunks)

  #  need to create chunks WITHIN threads.  Need to respect thread boundaries
  # take union of the breakpoints, plus thread boundaries, plus 1st and last row

  # counters for assigning thread and sequence numbers
  thisThread=1  # just for counting in this loop
  lastThread=0
  seqNo=0  # resets for each new thread

  for (chunkNo in 1:nChunks){

    # Chunks start at the current breakpoint
    start_idx=breakpoints[chunkNo]

    # Chunks end at the next breakpoint, minus one
    # for the last chunk,the stop_idx is the last row
    if (chunkNo < nChunks){
      stop_idx = breakpoints[chunkNo+1] - 1
    } else if (chunkNo==nChunks){
      stop_idx = nrow(o)
    }

    # assign the occurrences
    e$occurrences[[chunkNo]] = list(start_idx:stop_idx)

    e$label[[chunkNo]] = paste0('<',
                                str_replace_all(concatenate(o$label[start_idx:stop_idx]), ' ','++'),
                                '>')

    # assign timestamp and duration
    e$tStamp[chunkNo] = o$tStamp[start_idx]
    e$eventDuration[chunkNo] = difftime(o$tStamp[stop_idx], o$tStamp[start_idx],units=timescale )

    # copy in the threadNum and assign sequence number
    e$threadNum[chunkNo] = o$threadNum[start_idx]
    thisThread = o$threadNum[start_idx]


    # fill in data for each of the context factors
    for (cf in compare_CF){
      e[chunkNo,cf] = as.character(o[start_idx,cf])
    }

    for (cf in event_CF){
      VCF = paste0("V_",cf)
      e[[chunkNo, VCF]] = list(aggregate_VCF_for_event(o,e$occurrences[chunkNo],cf ))
    }

    # Advance or reset the seq counters
    if (thisThread == lastThread){
      seqNo = seqNo +1
    } else if (thisThread != lastThread){
      lastThread = thisThread
      seqNo = 1
    }
    e$seqNum[chunkNo] = seqNo

  }

  # convert them to factors
  for (cf in compare_CF){
    e[cf] = as.factor(e[,cf])
  }

  # fill in the last column with the label (tried using row number...)
  e$ZM_1 = as.factor(e$label)

	# return events with network cluster added for zooming...
    # NOTE: no eventmap stored here any more
	# TODO: need to add step to explicitly add event map in caller function
  e <- clusterEvents(e, EventMapName, 'Contextual Similarity')[[1]] # [1] to get the event map

	# NOTE: "e" is just the threads that were created; need to explicitly give it a name and add it to the map list

  return(e)

}

# this one creates events based on frequent ngrams or regular expressions
OccToEvents3 <- function(o, EventMapName,TN, CF, rx, KeepIrregularEvents){

	EVENT_CF <- get_EVENT_CF()
    compare_CF <- get_COMPARISON_CF()

  # keep track of the length of each pattern
  for (i in 1:nrow(rx))
  {rx$patLength[i] = length(unlist(strsplit(rx$pattern[i], ',')))
  }

  # put this here for now
  timescale='mins'

  # get the text vector for this set of threaded occurrences, delimited by commas
  tv = thread_text_vector( o, TN, CF, ',' )

  # apply regex to tv and split on the commas
  tvrx =  replace_regex_list( tv, rx )

  tvrxs = lapply(1:length(tvrx), function(i) {unlist(strsplit(tvrx[[i]],','))})

  # count the total number of chunks
  nChunks = length(unlist(tvrxs))

  # make the dataframe for the results.  This is the main data structure for the visualizations and comparisons.
  e = make_event_df(EVENT_CF, compare_CF, nChunks)

  #loop through the threads and fill in the data for each event
  # when it's a row number in the input data array, just copy the row
  # when it's one of the regex labels, use the numbers in the pattern to compute V_ for the new event
  chunkNo=0
  original_row=0
  for (thread in 1:length(tvrxs)){

    # the events stay in sequence
    for (sn in 1:length(tvrxs[[thread]])){

      # increment the current row numbers
      chunkNo = chunkNo+1
      original_row=original_row+1

      # assign the thread and sequence number
      e$threadNum[chunkNo] = thread
      e$seqNum[chunkNo] = sn

      # Make sure the CFs are factors
      for (cf in compare_CF){
        e[[chunkNo, cf]] =  o[[original_row, cf]]
      }

      # this is a chunk that matched one of the patterns
      if (tvrxs[[thread]][sn] %in% rx$label ){

        # Use the ZM_1 column to store the new labels
        e$ZM_1[chunkNo] = tvrxs[[thread]][sn]
        e$label[chunkNo] =  tvrxs[[thread]][sn]

        # need to locate the unique for from the o dataframe and aggregate those V_cf values.
        rxLen = rx$patLength[which( rx$label==tvrxs[[thread]][sn])]
        e$occurrences[[chunkNo]] = list(seq(original_row,original_row+rxLen-1,1))

        original_row = original_row + rxLen-1
        # compute the V_ based on the occurrences
        for (cf in EVENT_CF){
          VCF = paste0("V_",cf)
          e[[chunkNo, VCF]] = list(aggregate_VCF_for_regex(o,e$occurrences[chunkNo],cf ))
        }

        # assign timestamp and duration -- use first - last occurrence times
        # e$eventDuration[chunkNo] = difftime(o$tStamp[stop_idx], o$tStamp[start_idx],units=timescale )
        e[[chunkNo,'tStamp']] = o[[original_row,'tStamp']]

      }
      else if (KeepIrregularEvents=='Keep') {
        # copy data from input structure

        # Use the ZM_1 column to store the new labels
        e$ZM_1[chunkNo] = tvrxs[[thread]][sn]
        e$label[chunkNo] =  tvrxs[[thread]][sn]
        e$occurrences[[chunkNo]] = original_row

        # copy the rest of the data
        for (cf in EVENT_CF){
          VCF = paste0("V_",cf)
          e[[chunkNo, VCF]] =  o[[original_row, VCF]]
        }

        e[[chunkNo,'tStamp']] = o[[original_row,'tStamp']]
        e[[chunkNo,'eventDuration']] = o[[original_row,'eventDuration']]

      }

    } # sn loop
  } # thread loop


  # take out the irregular events (empty rows) if so desired
  if (KeepIrregularEvents=='Drop'){
    # keep the subset where the event is not blank
    e=subset(e, !ZM_1=='')
    }

    # store the event map in the GlobalEventMappings and return the eventmap
    eventMap = store_event_mapping(EventMapName, e)
    return(eventMap[['threads']])

}

# new function for new tab
# e is the event list
# cluster_method is either "Sequential similarity" or "Contextual Similarity" or "Network Structure"
# TODO: handle cluster_method separately, then call "createEventMap"?

# Rename to "createEventMap"?
# TODO: don't pass in "EventMapName" here, and then add to list
# just return the map, and have the called give it a name and explicitly add it
clusterEvents <- function(e, cluster_method){

	event_CF <- get_EVENT_CF()

	# make sure to cluster on the correct column (one that exists...)
  	if (cluster_method=="Sequential similarity") {
		dd <- dist_matrix_seq(e)
	} else if (cluster_method=="Contextual Similarity") {
		dd <- dist_matrix_context(e,event_CF)
	} else if (cluster_method=="Network Proximity") {
    	# The focal column is used to trade the network.  It will probably only be present in the OneToOne mapping, but we should check more generally
    	# if it's not present, then use the highest granularity of zooming available.
    	focalCol <- paste0(event_CF,collapse="_")

    	if(!focalCol %in% colnames(e)) {
			zoom     <- as.integer(str_replace(colnames(e[max(grep("ZM_",colnames(e)))]),"ZM_",""))
			focalCol <- paste0('ZM_',zoom)
		}

    	dd <- dist_matrix_network(e,focalCol)
	}

  	### cluster the elements
  	clust <- hclust(dd, method="ward.D2")

	# delete old ZM columns
  	e[grep("ZM_",colnames(e))] <- NULL

  	# number of chunks is the number of rows in the distance matrix
  	nChunks <- attr(dd,'Size')

  	# make new data frame with column names for each cluster level
  	zm <- setNames(data.frame(matrix(ncol = nChunks, nrow = nChunks)), paste0("ZM_", 1:nChunks))

  	## Create a new column for each cluster solution
  	for (cluster_level in 1:nChunks){
    	clevelName     <- paste0("ZM_",cluster_level)
    	zm[clevelName] <- cutree(clust, k=cluster_level)
  	}

  	# append this onto the events to allow zooming
  	# need to handle differently for network clusters
 	# we are relying on "unique" returning values in the same order whenever it is called on the same data
  	if (cluster_method=="Network Proximity") {
		merge_col_name       <- paste0(event_CF,collapse="_")
    	zm[[merge_col_name]] <- unique(e[[merge_col_name]])
    	newmap               <-  merge(e, zm, by=merge_col_name)
	} else {
		newmap <- cbind(e, zm)
	}

	# generate event map
	# TODO: this should be returned
	eventMap <- newmap[order(newmap[['threadNum']],newmap[['seqNum']]),]

	# For now two different types of functions call this, so return both parts:
	# TODO: split this function so the callers that need eventMap can get it
	# and the caller that needs clust can also get it
	results <- list(eventMap,clust)

	return(results)

}

# new data structure for events
make_event_df <- function(event_CF,compare_CF,N){

  	# Make a data frame with columns for each CF, and put one vector into each column
  	e <- data.frame(
    	tStamp        = numeric(N),  # this is the event start time
    	eventDuration = numeric(N),
    	label         = character(N),
    	occurrences   = integer(N),
    	threadNum     = integer(N),
    	seqNum        = integer(N)
	)

 	# add columns for each of the context factors used to define events
  	# first make the dataframes for each
  	cf1v <- setNames(data.frame(matrix(ncol = length(event_CF),   nrow = N)), paste0("V_",event_CF))
  	cf2  <- setNames(data.frame(matrix(ncol = length(compare_CF), nrow = N)), compare_CF)

  	# Then combine them
  	e <- cbind(e, cf2,cf1v)

  	# and add one more column for the event code/description -- maybe use label instead of this?
  	e$ZM_1 <- character(N)

  	return(e)
}
