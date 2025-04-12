
# Import Packages ------------------------------------------------------
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))
suppressWarnings(library(gridExtra))

# Set working directory + bring in test files ----------------------------------
setwd("C:/cygwin64/home/vedan/Code/sta141b/Solar1/unzip")

dir = "C:/cygwin64/home/vedan/Code/sta141b/"
source(paste(dir, "/assignment1part2functions.R", sep = ""))

# - find all lines w/ " - Average.." 
# (do an or where you look and see if any of the tables are found) = w1 = first line
# last line will be the next " - " = w2
# w3 = w1 & w2

dir = "C:/cygwin64/home/vedan/Code/sta141b/Solar1/unzip"

fn = paste(dir, "/USA_CA_UC-Davis-University.AP.720576_TMYx.2009-2023.stat", sep = "")

ll = readLines(fn, encoding = "latin1") # lines.list


tables = getTables(ll)

