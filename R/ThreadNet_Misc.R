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

# this function is used to split up the threads into n ~equal buckets
make_subsets <- function(d,n){
  return(split(d, ceiling(seq_along(d)/(length(d)/n))))
}

######### Functions that return column names #######

# names of the columns for contextual factors
# grab all of the columns except the first, which has the time stamp
# tStamp in the first column
#' cfnames provides names of all the contextual factors (except the time stamp)
#' @family ThreadNet_Misc
#' @param o data frame with threads
#'
#' @return list of column names
#' @export
#'
# DEPRECATED -- REMOVE WHEN NO LONGER CALLED
cfnames <- function(o){
  colnames(o)[2:length(colnames(o))]}

## this is used to populate the UI for comparison of categories within a CF
#' get_CF_levels returns the levels of a contextual factor
#' @family ThreadNet_Misc
#' @param o data frame with threads
#' @param cf  a contextual factors (column)
#'
#' @return list of unique factor levels
#' @export
# TODO: deprecate this function
get_CF_levels <- function(o,cf){

  return(levels(o[,cf]))
}

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
