##########################################################################################################
# THREADNET Misc functions

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

#' numThreads counts how many threads in the data set
#'
#' Threads must have unique thred numbers for this function to work
#'
#' @family ThreadNet_Misc
#' @param o data frame with occurrences or events
#' @param TN column with thread number
#'
#' @return number of threads
#' @export
numThreads <- function(o,TN) {length(unique(o[[TN]]))}

# add a new column to the occurrences table based on a combination of context factors CF
# TODO: deprecate this function; review all functions that call this
combineContextFactors <- function(threadData,CF,newCol){

	# CF     = contextual factors to be combined
	# newCol = name of new combined contextual factor

  	# Use the old column if there is one
  	if (!(newCol %in% names(threadData))) {

    	# Need to get the CF parameters into the right format for tidyr::unite function
    	cfn    <- sapply(CF, as.character)
    	newCol <- as.character(newCol)

    	#  unite the columns, but keep the old ones
    	threadData <- unite_(threadData, newCol, cfn, sep="+", remove=FALSE)

  	}

  	# Coerce the new column into a factor
  	# Should already be a factor -- this function should only be called if the new col is not in names(threadData)
  	threadData[newCol] <- as.factor(threadData[,newCol])

  	return(threadData)
}

####################################
### FUNCTIONS TO CALCULATE DIFFS ###
####################################

# TODO: review who calls these
# TODO: review use of global variables here

diff_handoffs <- function(o){

  # initialize the previous row
  previous_row <<- o[1,]

  return(apply(o,1, row_diff_handoff))

}

row_diff_handoff <- function(this_row){

  # just add up the differences.
  d <- sum(this_row == previous_row)

  # store the previous row
  previous_row <<- this_row

  # return the number of differences
  return(d)
}


# TODO: the next two functions are only called in setPOV function -- move with those
diff_tStamp <- function(ts){

  # initialize the first row
  previous_row <<- ts[1]

  return(sapply(ts, row_diff_tStamp))

}

row_diff_tStamp <- function(this_row){

  # add up the differences.
  d <- max(0,difftime(this_row, previous_row, units="secs"))

  # store the previous row
  previous_row <<-this_row

  # return the time difference
  return(d)
}

###########################


#' threadSizeTable provides a distribution of the length of threads
#'
#' This function should work on either ocurrences or events.
#' it returns length and duration of each thread.It requires tStamp field to compute duration.
#'
#' @family ThreadNet_Misc
#'
#' @param o data frame with threads
#' @param TN column comtaining the threadNumber
#'
#' @return data frame with table of thread lengths
#' @export
threadSizeTable <- function(o,TN){

  # get the number of threads
  nThreads = nrow(unique(o[TN]))

  id=integer(nThreads)
  num=integer(nThreads)
  dur=numeric(nThreads)
  sizes = data.frame(id,
                     num,
                     dur)

  for (i in 1:nThreads){
    sizes$id = i
    sizes$num[i] = sum(o[TN]==i)
  }

  s = as.data.frame(table(sizes$num))

  return(s)
}

#' get_moving_window returns a set of threads for a moving window
#'
#' @family ThreadNet_Misc
#'
#' @param e data frame with threads (needs to have threadNum and seqNum)
#' @param s size of window
#' @param l location of window
#'
#' @return data from with just the threads in the window
#' @export
#'
get_moving_window <- function(e, s, l ){

    # get the list of threads
	w <- e[e[[seqNum]]==1,threadNum]

  	# get get the appropriate subset of threads for the window
  	w <- w[l:(l+s-1)]

  	# and now subset the rows for those threads
  	return(e[e$threadNum %in% w,])

}

# e is the data
# w = window size
# s = step (how far to move the window in each step)
# n is the ngram size
window_correlation  <- function(e,w,s=1,n=2){

  # make data frame
  vt=data.frame( ngrams=character(), freq=integer(), wid=integer() )

  # use the finest  granularity
  zcf = zoom_upper_limit(e)

  # now many threads?
  nThreads = numThreads(e,'threadNum')

  wcount=0
  # scan through the data
  for (wloc in seq( 1, nThreads, s)){
    wcount= wcount +1
    # print(paste('wloc =',wloc))

    # get text vector for the whole data set - just keep the first two colomns
    ngdf = count_ngrams(get_moving_window(e, w, wloc), 'threadNum', zcf, n)[1:2]
    # print(paste('nrow ngdf =',nrow(ngdf)))

    # add a the row number
    ngdf$wid = wcount

    # append the columns to the end
    vt=rbind(vt,ngdf)
  }

  # convert to factor so that we can compute distances using the factor levels
  vt$ngrams = factor(vt$ngrams)

  nWindows = length(unique(vt$wid))

  # get the set of unique ngrams for the whole data set
  vt_unique = data.frame(ngrams=unique(vt$ngrams))

  # put the results here
  windowFreqMatrix = matrix(0,nrow=nWindows, ncol=nrow(vt_unique))

  for (i in 1:nWindows){

    # get the merged list
    vtmerge = merge(x=vt_unique, y=vt[vt$wid==i,], by='ngrams', all.x = TRUE)

  # use the wid.y to get the whole vector, but replace the NA with zeros
   b=vtmerge[vtmerge$wid==i,'freq']
   b[is.na(b)] <- 0

   windowFreqMatrix[i,]=b
  }


 # old way: correlate one row with the next and stick it in a dataframe
  df =data.frame(window=1:(nWindows-1),
                 thread = seq( 1, nThreads-s, s),
                 correlation= unlist(lapply(1:(nWindows-1),
                                              function(i){abs( cor(windowFreqMatrix[i,],windowFreqMatrix[i+1,])) })))

  # add the last row explicitly
  df = rbind( df, data.frame(window=nWindows, thread=nThreads, correlation=0))

  # return( df )

  # get the ngram data and labels
  b_df=as.data.frame(windowFreqMatrix)
  colnames(b_df)=vt_unique$ngrams

  # stick the ngram frequencies on the end for good measure
 return(cbind(df,b_df))

}

# e is the data
# w = window size
# s = step (how far to move the window in each step)
# n is the ngram size
# similar as above, except one window on each side of a focal thread.
dual_window_correlation  <- function(e,w,s=1,n=2){

  # make data frame
  vt=data.frame( ngrams=character(), freq=integer(), id=integer() )

  # use the finest  granularity
  zcf = zoom_upper_limit(e)

  # now many threads?
  nThreads = numThreads(e,'threadNum')


  # scan through the threads - treat each thread as a window of one
  # can probably do with the split and apply much faster
  for (t in 1:nThreads){

    # print(paste('wloc =',wloc))

    # get text vector for the whole data set - just keep the first two colomns
    ngdf = count_ngrams(get_moving_window(e, 1, t), 'threadNum', zcf, n)[1:2]
    # print(paste('nrow ngdf =',nrow(ngdf)))

    # add a the row number
    ngdf$id = t

    # append the columns to the end
    vt=rbind(vt,ngdf)
  }

  # convert to factor
  vt$ngrams = factor(vt$ngrams)

  # compute number of windows.
  nWindows = floor(nThreads/w)

  # get the set of unique ngrams for the whole data set
  vt_unique = data.frame(ngrams=unique(vt$ngrams))

  # put the results here
  ngramFreqMatrix = matrix(0,nrow=nThreads, ncol=nrow(vt_unique))

  for (i in 1:nThreads){

    # get the merged list
    vtmerge = merge(x=vt_unique, y=vt[vt$id==i,], by='ngrams', all.x = TRUE)

    # use the wid.y to get the whole vector, but replace the NA with zeros
    b=vtmerge[vtmerge$id==i,'freq']
    b[is.na(b)] <- 0

    ngramFreqMatrix[i,]=b
  }

  # return(ngramFreqMatrix)

  # old way: correlate one row with the next and stick it in a dataframe
  df =data.frame( thread = seq(w,nThreads-(w+1),s),
                  correlation= unlist(lapply(seq(w,nThreads-(w+1),s),
                                            function(i){abs( cor(colSums( ngramFreqMatrix[(i-w):i, ] ),
                                                                colSums( ngramFreqMatrix[(i+1):(i+w+1), ] )))  })))

  # # add the last row explicitly
  # df = rbind( df, data.frame( thread=nThreads, correlation=0))

   return( df )

  # # get the ngram data and labels
  # b_df=as.data.frame(ngramFreqMatrix)
  # colnames(b_df)=vt_unique$ngrams
  #
  # # stick the ngram frequencies on the end for good measure
  # return(cbind(df,b_df))

}

# find the biggest column with ZM_, and then get the number that goes with that.
# It will not be the same as the column number.
# TODO: re-write this to be more efficient and deprecate function
zoom_upper_limit <- function(e){
  upper_limit = as.integer(str_replace(colnames(e[max(grep("ZM_",colnames(e)))]),"ZM_",""))
  return(upper_limit)
}

# this is used for the regex pages to show the threads.
# similar code is used in count_ngrams and to make networks, but with different delimiters
# and with a minimum sequence length (ngram size), but this can be filtered after this function3
thread_text_vector <- function(o, TN, CF, delimiter){

  # Initialize text vector
  tv = vector(mode="character")

  # Loop through the unique thread numbers
  j=0
  for (i in unique(o[[TN]])){
    txt =o[o[[TN]]==i,CF]

    j=j+1
    tv[j] = str_replace_all(concatenate(o[o[[TN]]==i,CF] ),' ',delimiter)
  }
  return(tv)

}

# use this to replace patterns for regex and ngrams
# tv is the text vector for the set of threads
# rx is the dataframe for regexpressions ($pattern, $label)
replace_regex_list <- function(tv, rx ){

  for (i in 1:length(tv)) {
    for (j in 1:nrow(rx) ) {
      tv[i] = str_replace_all(tv[i],rx$pattern[j],rx$label[j])
    }
  }
  return(tv)
}

#########################
# Dist Matrix Functions #
#########################

# NOTE:
# These functions are used exclusively in clusterEvents
# Once that function is cleaned up, a more reasonable
# home for these functions may present itself

# this function pulls computes their similarity of chunks based on sequence
dist_matrix_seq <- function(e){

  nChunks = nrow(e)
  evector=vector(mode="list", length = nChunks)
  for (i in 1:nChunks){
    evector[i]=unique(as.integer(unlist(e$occurrences[[i]])))
  }
  return( stringdistmatrix( evector, method="osa") )
}

# this function pulls computes their similarity of chunks based on context
# e = events, with V_columns
# CF = event CFs
# w = weights (0-1)
#
dist_matrix_context <- function( e, CF ){

  nChunks <- nrow(e)
  evector <- VCF_matrix( e, paste0( "V_",CF ))

  return( dist( evector, method="euclidean") )
}

# this function computes their similarity of chunks based on network
dist_matrix_network <- function(e,CF){

  # first get the nodes and edges
  n=threads_to_network_original(e,'threadNum',CF)

  # now get the shortest paths between all nodes in the graph
  d=distances(graph_from_data_frame(n$edgeDF),
              v=n$nodeDF[['label']],
              to=n$nodeDF[['label']])

  return( as.dist(d) )
}

#######################
# CF Vector Functions #
#######################

# this will convert the context factor into a list (like this: 0 0 0 0 0 0 0 0 1 0 0)
# o is the dataframe of occurrences
# CF is the context factor (column)
# r is the row (occurrence number in the One-to-One mapping)
# TODO: review where this is called -- not necessarily needed as function
convert_CF_to_vector <- function(o,CF,r){ as.integer((levels(o[[CF]]) ==o[[r,CF]])*1) }

# Aggregate the VCF (CF vector) for that CF
# There are two layers to this.
# 1) aggregate_VCF_for_event
#   Within an single event, aggregate the VCF for the occurrences that make up that event.
#   This function will only get used when creating from the fuctions that convert occurrences to events
# 2) aggregate_VCF_for_cluster
#   For a cluster level, aggregate the events at that cluster level (e.g., ZM_n)
#   This function will work on any event, even the one_to_one mapping.
#
# o is a dataframe of occurrences.  The values of V_ (the VCF) does not have to be filled in.  It gets re-computed here for each occurrence.
# occlist is the list of occurrences of that event (e$occurrences)
# cf is the name of the contextual factor to create the VCF

aggregate_VCF_for_event <- function(o, occList, cf){

  # get the column name for the VCF
  VCF = paste0("V_",cf)

  # start with the first one so the dimension of the vector is correct
  aggCF = convert_CF_to_vector(o, cf, unlist(occList)[1])

  # now add the rest, if there are any
  if (length(unlist(occList)) > 1){
    for (idx in seq(2,length(unlist(occList)),1)){
      aggCF = aggCF + convert_CF_to_vector(o, cf, unlist(occList)[idx])
    }}
  return(aggCF)
}

# this version  assumes that the VCF is already computed.
# Might come in handy, but it's not correct...
aggregate_VCF_for_regex <- function(o, occList, cf){

  # get the column name for the VCF
  VCF = paste0("V_",cf)

  # start with the first one so the dimension of the vector is correct
  aggCF = unlist(o[unlist(occList)[1],VCF])

  # now add the rest, if there are any
  if (length(unlist(occList)) > 1){
    for (idx in seq(2,length(unlist(occList)),1)){
      aggCF = aggCF+unlist(o[[unlist(occList)[idx],VCF]])
    }}
  return(aggCF)
}

# Same basic idea, but works on a set of events within a cluster, rather than a set of occurrences within an event
# so you get get a subset of rows, convert to a matrix and add them up
# e holds the events
# cf holds a single contextual factor, so you need to call this in a loop
# zoom_col and z are used to subset the data.  They could actually be anything.
aggregate_VCF_for_cluster <- function(e, cf, eclust, zoom_col){

  # get the column name for the VCF
  VCF = paste0("V_",cf)

  # get the matrix for each

  # get the subset of events for that cluster  -- just the VCF column
  # s =  e[ which(e[[zoom_col]]==eclust), VCF]   This version uses the
  s =  e[ which(as.integer(e[[zoom_col]])==eclust), VCF]

  if ( is.null(unlist(s) ))
    return(NULL)
  else
    return( colSums( matrix( unlist(s), nrow = length(s), byrow = TRUE) ))
}

# this one takes the whole list
VCF_matrix <- function(e, vcf ){

  m = one_vcf_matrix(e, vcf[1] )

  if (length(vcf)>1){
    for (idx in seq(2,length(vcf),1)){
      m = cbind( m, one_vcf_matrix(e, vcf[idx] ) )
    }}
  return(m)
}

# this one takes a single column as an argument
one_vcf_matrix <- function(e, vcf){ matrix( unlist( e[[vcf]] ), nrow = length( e[[vcf]] ), byrow = TRUE) }
