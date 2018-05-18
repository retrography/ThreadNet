##########################################################################################################
# THREADNET:  NGrams

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# NGram functions

# Counting ngrams is essential to several ThreadNet functions
#' Counts ngrams in a set of threads
#'
#' This function counts n-grams within threads where the length of the thread is greater than n.
#' @family ThreadNet_Core
#'
#' @param o dataframe containing threads
#' @param TN name of column in dataframe that contains a unique thread number for each thread
#' @param CF name of the column in dataframe that contains the events that will form the nodes of the network
#' @param n length of ngrams to count
#'
#' @return a dataframe with ngram, frequency and proportion in descending order
#'
#' @export
count_ngrams <- function(o,TN,CF,n){

  # Need a vector of strings, one for each thread, delimited by spaces
  # the function long_enough filters out the threads that are shorter than n
  # use space for the delimiter here
  text_vector = long_enough( thread_text_vector(o,TN,CF,' '), n, ' ')

  # print("text_vector")
  # print(text_vector)

  ng = get.phrasetable(ngram(text_vector,n))

  # add a column here for the length of the ngram -- useful later!
  ng$len = n

  return(ng)
}

# combined set of frequent ngrams
# add parameter to make maximal a choice
frequent_ngrams <- function(e, TN, CF, minN, maxN, onlyMaximal=TRUE){

  # initialize the output
  ng = count_ngrams(e,TN, CF,minN)

  if (maxN > minN){
    for (i in seq(minN+1,maxN,1)){
      ng = rbind(ng,count_ngrams(e,TN, CF,i)) }
  }
  # remove the rows that happen once and only keep the columns we want
  ng=ng[ng$freq>1,c('ngrams','freq', 'len')]

  # just take the maximal ones if so desired
  if (onlyMaximal) { ng=maximal_ngrams(ng)  }

  # return the set sorted by most frequent
  return(ng[order(-ng$freq),])
}

# this filters out ngrams that are contained within others ('2 2' is part of '2 2 2')

maximal_ngrams <- function(ng){

  # find out if each ngram is contained in all the others
  w = lapply(1:nrow(ng), function(i){
    grep(ng$ngrams[i],ng$ngrams)}
  )

  # get howMany times each one appears
  howMany = lapply(1:length(w), function(i){
    length(w[[i]])}
  )

  # return the ones that are unique
  return(ng[which(howMany==1),])
}

# compute support level for each ngram
# tv = text vectors for the threads
# ng = frequent ngrams data frame
# returns ng data frame with support level added
support_level <- function(tv, ng) {

  # change the commas back to spaces
  tv=str_replace_all(tv, ',' , ' ')

  totalN = length(tv)

  # need to remove whitespace from the trailing edge of the ngrams
  ng$ngrams = trimws(ng$ngrams)

  # find out how many times each ngram is contained in each TV
  ng$support = unlist(lapply(1:nrow(ng), function(i){
    length(grep(ng$ngrams[i],tv)) })
  )/totalN

  # toss in the generativity level
  ng = generativity_level(tv,ng)

  return(ng)
}


# compute the generativity = in-degree and out-degree
generativity_level<- function(tv, ng){

  # for each ngram, look at the next longer size
  # Find the n+1-grams that match (as in the code for maximal ngrams).
  # There are two possibilities -- matching in the first or second position
  # The number of matches in the first position =  the out-degree
  # The number of matches in the second position =  the in-degree
  # if so desired, it should be possible to keep a list.

  # problem is that the tokens can be 1-3 characters long, and there are spaces...

  # Big Idea for frequent n-grams: use the DT:: and let people sort, select and apply all the ngrams they want.
  # Name them using the tokens but with a different delimiter to avoid confusion.  Go Crazy!

  # convert to spaces
  tv=str_replace_all(tv, ',',' ')

  # first get the range we are looking for
  nList = unique(ng$len)

  z=list()

  # loop through them
  for (n in nList){

		 # print(paste('n = ',n))
    #pick the ngrams of length n from the list given
    ngn= ng[ng$len==n,]


    # get ngrams of length n+1 -- make sure the threads are long enough
    ngplus = get.phrasetable(ngram( long_enough(tv,n+1, ' '), n+1))

    # this picks out the ones that match
    w = lapply(1:nrow(ngn), function(i){
      grep(ngn$ngrams[i],ngplus$ngrams)} )

    #print(w)
    # print('z = ')
    zplus = lapply(1:nrow(ngn), function(i){
      str_locate(ngplus$ngrams[w[[i]]],ngn$ngrams[i])  } )

    # print(z)

    z = c(z,zplus)

  }

  # compute the in and out degree
  ng$in_degree = unlist(lapply(1:nrow(ng), function(i){
    zm=z[[i]]
    length( zm[zm[,1]>1,1] )  } ))

  ng$out_degree = unlist( lapply(1:nrow(ng), function(i){
    zm=z[[i]]
    length( zm[zm[,1]==1,1] )  } ))

  # ng$generativity = lapply(1:nrow(ng), function(i) {ng$out_degree[i] * ng$in_degree[i]})

  return(ng)
}

# to avoid errors in count_ngrams, make sure the length of each thread in the text_vector tv is longer than the n-gram size, n
# this gets used in various places so need to pass in the delimiter
long_enough = function(tv,n,delimiter){

  return(tv[ unlist(lapply(1:length(tv), function(i) {length(unlist(strsplit(tv[[i]],delimiter)))>=n})) ])

}
