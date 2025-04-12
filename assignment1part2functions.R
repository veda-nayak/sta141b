
# Import Packages ------------------------------------------------------
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))
suppressWarnings(library(gridExtra))

# Set working directory + bring in test files ----------------------------------

dir = "C:/cygwin64/home/vedan/Code/sta141b/"

setwd(paste(dir, "/Solar1/unzip", sep = ""))

# - find all lines w/ " - " = first line, last line will be the next " - "

getBlocks = # make all blocks
  # Uses: findTitles
  function(ll){
    starts = substring(ll, 1, 3) == " - "
    
    # group the lines based on blocks between trues
    w = starts
    g = cumsum(w)
    
    tot_blocks = split(ll, g)
    names(tot_blocks) = sapply(tot_blocks, `[`, 1)
    
    tot_blocks
    
  }

tableStarts = # get the starting values of the blocks we want
  function(ll){
    
    check_blocks = lapply(getBlocks(ll), findTitles)
    table_starts = which(check_blocks == TRUE)
    
    table_starts
    
  }

findTitles= # Find the blocks that we are interested in
  function(list){
    
    block1_ave_hour = " - Average Hourly Statistics for Opaque Sky Cover" 
    block2_psych = " - Psychrometric Chart Data"
    block3_rel_humid = " - Monthly Statistics for Relative Humidity"
    block4_solar_rad = " - Monthly Statistics for Solar Radiation"
    
    open = unlist(list)
    x = FALSE
    if (grepl(block1_ave_hour, open[1], fixed=TRUE) | 
        grepl(block2_psych, open[1], fixed=TRUE) | 
        grepl(block3_rel_humid, open[1], fixed=TRUE) |
        grepl(block4_solar_rad, open[1], fixed=TRUE)){
      x = TRUE
    }
    x
    # [findTables()function](https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string)
  }

getTables = # Get the tables we are after
  # Uses: getBlocks(), tableStarts()
  function(ll){
    total_blocks = getBlocks(ll)
    table_starts = tableStarts(ll)
    
    correct_blocks = list()
    
    for (i in 1:length(table_starts)){
      correct_blocks[i] <- total_blocks[table_starts[i]]
    }
    
    correct_blocks
  }
