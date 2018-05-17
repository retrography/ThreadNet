##########################################################################################################
# THREADNET:  readData

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# Functions relating to loading a raw dataset
# These are called exclusively by functions in the readData tab

# main function that gets called by server.R

# Select a File

# Run various validations
	# return to main page if any conditions not met
	# design to allow future additions of other validations

# if file is good, return a dataset to app 


##### OLD FUNCTIONS TO REVIEW #####

# Read in, check, and clean up the data
# need to see "tStamp" in the first column
#' read_occurrences
#'
#' @family ThreadNet_Misc
#'
#' @param inFile
#'
#' @return dataframe with occurrences
#' @export
#'
#' @examples
read_occurrences <- function(inFile){

  # if it's null return null, otherwise do the whole thing...
  if (is.null(inFile))
    return(NULL)

  # read in the table of occurrences
  o=read.csv(inFile$datapath)


  # check the file format.  Put in humorous example if the format is bad
  if (check_file_format(o)=="badformat")
  {o=make_example_DF() }
  else if (check_file_format(o)=="sequence")
  {o=add_relative_timestamps(o,"sequence", 1) }

  # clean up the data -- remove blanks, etc.
  o = cleanOcc(o,cfnames(o))

	# TODO: review where/how this runs -- should be handled in a separate step
  shinyjs::show(selector = "#navbar li a[data-value=choosePOV]")
  shinyjs::hide(selector = "#navbar li a[data-value=visualize]")
  shinyjs::hide(selector = "#navbar li a[data-value=subsets]")
  shinyjs::hide(selector = "#navbar li a[data-value=comparisons]")
  shinyjs::hide(selector = "#navbar li a[data-value=movingWindow]")
  shinyjs::hide(selector = "#navbar li a[data-value=parameterSettings]")
  return(o)}

# This could be improved but is an important logical checkpoint
# just checks that a required field is in the first column
check_file_format = function(o){

  if ((colnames(o)[1] == "tStamp"))
    {return("tStamp")}
  else if ((colnames(o)[1] == "sequence"))
  {return("sequence")}

  else
  {return("badformat")}
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
make_example_DF = function(){
  correct_occ = read.table(text="tStamp actor action object location
                            '2017-4-7 17:52:04' jimmy tosses ball playground
                            '2017-4-7 17:52:12' rover fetches ball playground
                            '2017-5-18 9:05:52' jimmy tosses ball forest
                            '2017-5-18 9:06:24' rover fetches stick forest
                            '2017-5-18 9:10:48' jimmy searches ball forest ", header=TRUE)
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
cleanOcc = function(o, cfnames){

  withProgress(message = "Cleaning Data", value = 0,{

  'denominator for loop'
  n = length(o)

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
  o$tStamp = as.character(o$tStamp)
  o$tStamp = parse_date_time(o$tStamp, c("dmy HMS", "dmY HMS", "ymd HMS"))

  ## Add the category ">other<" for all of the factors to facilitate recoding later
  # This may not be needed anymore... commented out Dec 3 2017
  # o <- as.data.frame(lapply(o, addOther))

  # add weekday and month
  o$weekday = as.factor(weekdays(as.Date(o$tStamp)))
  o$month = as.factor(months(as.Date(o$tStamp)))


  return(o)
}

## Use this function to remove blanks from the CF data
fixBlanks = function(s){

  # take out blanks
  s=str_replace_all(s," ","_")

  if (is.na(s)){
    s="blank"
  }
  return(s)
}
