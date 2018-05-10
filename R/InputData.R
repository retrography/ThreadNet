# Functions related to validating raw data input

read_occurrences <- function(inputFile){

    # confirm a valid argument was passed to the function
    validate(need(!is.null(inputFile), "No file selected."))

    # read in the table of occurrences
    o <- read.csv(inputFile$datapath)

    # perform simple validation on first columns of input data
    # generate a test dataset if file is not valid

    col1 <- colnames(o)[1]

    if(col1 != "tStamp"){
        if(col1 != "sequence"){
            # invalid -- first is not "tStamp" or "sequence"
            o <- make_example_DF()
        } else {
            # first col is squence
            o <- add_relative_timestamps(o,"sequence", 1)
        }
    }

    # clean up the data -- remove blanks, etc.
    o <- cleanOcc(o,cfnames(o))

    # data is loaded; show choosePOV (todo -- run this when function has returned ok
    shinyjs::show(selector = "#navbar li a[data-value=choosePOV]")

    return(o)
}

#' Add relative timestamps
#'
#' This function uses the sequence numbers to add a column with time stamp to the data, so that it can be used throughout the rest
#' of the app, which expects to see a time stamp.  Start time for all threads is the same: "2017-01-01 00:00:00"  Happy New Year!
#'
#' @param o   data frame of occurrences
#' @param SN column containing the sequence numbers
#' @param tstep time step for events within each thread.  Default is one minute.
#'
#' @return  data frame of occurrences
#' @export
#'
#' @examples
add_relative_timestamps <- function(o, SN, tstep=1){

  startTime <- as.POSIXct("2017-01-01 00:00:00")

  # add the column at the beginning
  o <- cbind(startTime + 60*as.numeric(as.character(o[[SN]])), o)

  # set the column name
  colnames(o)[1] <- "tStamp"

  return(o)

}

##  Make an example data frame for display...
make_example_DF <- function(){
	correct_occ <- read.table(
		text="
			tStamp actor action object location
            '2017-4-7 17:52:04' jimmy tosses ball playground
            '2017-4-7 17:52:12' rover fetches ball playground
            '2017-5-18 9:05:52' jimmy tosses ball forest
            '2017-5-18 9:06:24' rover fetches stick forest
            '2017-5-18 9:10:48' jimmy searches ball forest
		", header=TRUE
	)
}


# this function will clean up the raw occurrence data
#' Clean up occurrences
#'
#' Clean up removes blanks from the data. Blanks cause problems because the n-gram algorithm interprets blanks as seperate tokens.
#'
#' @family ThreadNet_Misc
#' @param o  data frame with occurrences
#' @param cfnames names of contextual factors (columns) to clean up.
#'
#' @return data frame with blanks removed.
#' @export
#'
#' @examples
cleanOcc <- function(o, cfnames){

	withProgress(message = "Cleaning Data", value = 0,{

  		'denominator for loop'
  		n <- length(o)

  		## clean up the spaces here and make it back into a factor
  		for (cf in cfnames){
    		i = 1
    		o[,cf] = sapply(o[,cf],fixBlanks)
    		o[cf] = factor( o[,cf] )
    		incProgress(i/n)
     		i = i + 1
  		}
	})

  	# force tStamp into a "YMD_HMS" format
  	o$tStamp <- as.character(o$tStamp)
  	o$tStamp <- parse_date_time(o$tStamp, c("dmy HMS", "dmY HMS", "ymd HMS"))

  	# add weekday and month
 	o$weekday <- as.factor(weekdays(as.Date(o$tStamp)))
  	o$month <- as.factor(months(as.Date(o$tStamp)))

	return(o)
}

## Use this function to remove blanks from the CF data
fixBlanks <- function(s){

  # take out blanks
  s <- str_replace_all(s," ","_")

  if(is.na(s)){
    s <- "blank"
  }
  return(s)
}
