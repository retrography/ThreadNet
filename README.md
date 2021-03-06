# ThreadNet

ThreadNet weaves threads into networks

## Overview

ThreadNet is a tool for visualization of repetitive sequences, such as organizational routines. It emphasizes the role of sequential and temporal context.  It is being created for NSF (SES-1734237) Antecedents of Complexity in Healthcare Routine, a collaborative project between Michigan State University and the University of Rochester Medical Center.  Co-PIs: Brian Pentland and Kenneth Frank (MSU), Julie Ryan Wolf and Alice Pentland (URMC).  The original version of ThreadNet was implemented in MatLab. 

## Installation
ThreadNet is currently not available through CRAN. You can install the package directly rom the source, using `devtools`:

```r
if (!"devtools" %in% installed.packages()[, "Package"]) {
  install.packages("devtools")
}
devtools::install_github('ThreadNet/ThreadNet')
```

## Usage 
In order to start the app:

```r
library(ThreadNet)
ThreadNet()
```

ThreadNet uses an intuitive `R Shiny` graphical user interface that you can explore on your own. For further documentation and sample data see [ThreadNet's institutional homepage](http://routines.broad.msu.edu/ThreadNet).

## File format
ThreadNet reads data in simple .CSV format and .XES format (IEEE standard for process event log data). 

### First column must be either "tStamp" or "sequence"
When using timestamped data, the first column must be called "tStamp".  The timestamps should be in default R format: "yyyy-mm-dd hh:mm:ss"

Alternatively, the first colum can contain ordinal sequences numbers (1, 2, 3...) for each thread. For sequence numbers, the first column must be called "sequence".  

### All other columns contain "contextual factors" that change over time
Think of each row in the .CSV as a moment in time. What contextual factors do you need to describe that moment?  You can include as many contextual factors as you need. The data are treated as case-sensitive text. 
1. Use consistent labels
2. Fill in all the values for every row.  
3. Spaces will be replaced with underscore


