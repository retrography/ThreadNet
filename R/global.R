##########################################################################################################
# THREADNET:  SHINY GLOBAL

# (c) 2017 Michigan State University. This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# load dependencies
library(shiny)
library(shinyjs)
library(networkD3)
library(visNetwork)
library(plotly)
library(tidyverse)
library(ngram)
library(stringr)
library(stringdist)
library(ggplot2)
library(xesreadR)
library(colorspace)
library(igraph)
library(DT)
library(RColorBrewer)
library(lubridate)

# options for UI dropdowns
visualizations <- c(
	'Threads (event time)',
	'Threads (actual time)',
	'Threads (relative time)',
	'Event network (circle)',
	'Event network (force)',
	'Other networks',
	'Role Maps',
	'Thread Trajectories'
)

# load functions
source("ThreadNet_Core.R")
source("ThreadNet_Misc.R")
source("ThreadNet_Graphics.R")
source("ThreadNet_Metrics.R")
source("Event_Mappings.R")
source("NGrams.R")

# store event mappings
GlobalEventMappings <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Name", "Map"))

# fix for OS X time zone bug
setme <-
  "Sys.setenv(TZ='America/New_York') #some default not get any errors
invisible(loadNamespace('rgeolocate'))
invisible(loadNamespace('httr'))
mytz <- rgeolocate::ip_api(httr::content(httr::GET('https://api.ipify.org?format=json'))[1])[['timezone']]
Sys.setenv(TZ=mytz)"

cat(setme,file=file.path(R.home(),'etc/Rprofile.site'),sep='\n')


