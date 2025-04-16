
# Import Packages ------------------------------------------------------
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))
suppressWarnings(library(gridExtra))
suppressWarnings(library(reshape2))
suppressWarnings(library(data.table))
suppressWarnings(library(janitor))

# Set working directory + bring in test files ----------------------------------

dir = "C:/cygwin64/home/vedan/Code/sta141b/"

wd = paste(dir, "/Solar1/unzip", sep = "")

setwd(wd)

# Get the correct tables -------------------------------------------------------

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
    # CITE [findTables()function](https://stackoverflow.com/questions/10128617/test-if-characters-are-in-a-string)
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

# Monthly Statistics for Relative Humidity -------------------------------------
getHumid =  # Generate the table in one function
            # Uses: getTables() and 'reshape2' package
  function(ll){
    tables = getTables(ll)
    
    humid_txt = tables[[1]][-1] 
    # make it into a text connection since we're using text
    con = textConnection(humid_txt) 
    humid_data = read.delim(con, header = TRUE, stringsAsFactors = FALSE, quote = "", sep = "\t")
    
    humid_data <- humid_data[,2:14]
    
    # Make the maximum + minimum day:hour values more specific
    humid_data[2, 1] <- "Maximum_Day:Hour"
    humid_data[4, 1] <- "Minimum_Day:Hour"
    
    # change column name to make it more logical for later transformations
    colnames(humid_data)[1] <- "Month"
    
    # make the months into their own row
    new_row <- colnames(humid_data)
    humid_data[8,] <- new_row
    
    
    humid_data_transposed <- as.data.frame(t(humid_data))

    humid_data_transposed <- humid_data_transposed %>% row_to_names(row_number = 1)
    # CITE
    
    # Remove whitespace
    humid_data_transposed$`Minimum_Day:Hour` <- gsub('\\s+', '', humid_data_transposed$`Minimum_Day:Hour`)
    humid_data_transposed$`Maximum_Day:Hour` <- gsub('\\s+', '', humid_data_transposed$`Maximum_Day:Hour`)
    
    max_split = strsplit(humid_data_transposed$`Maximum_Day:Hour`, split = ":")
    max_day_hour = sapply(max_split, function(x) as.integer(x[c(1,2)]))
    max_day_hour = t(max_day_hour)
    max_day_hour = as.data.frame(max_day_hour)
    colnames(max_day_hour) <- c('Max_Day', 'Max_Hour')
    
    min_split = strsplit(humid_data_transposed$`Minimum_Day:Hour`, split = ":")
    min_day_hour = sapply(min_split, function(x) as.integer(x[c(1,2)]))
    min_day_hour = t(min_day_hour)
    min_day_hour = as.data.frame(min_day_hour)
    colnames(min_day_hour) <- c('Min_Day', 'Min_Hour')
    
    humid_data_transposed_month <- humid_data_transposed$Month
    humid_data_transposed <- humid_data_transposed[-c(2, 4, 8)]
    
    humid_data_transposed_numeric <- sapply(humid_data_transposed, as.integer)
    
    humid_data_transposed <- bind_cols(humid_data_transposed_month, humid_data_transposed_numeric, max_day_hour, min_day_hour)
    
    colnames(humid_data_transposed)[1] <- "Month"
    
    humid_data_transposed
  }


# Monthly Statistics for Solar Radiation   -------------------------------------
getSolarRad = # Generate the table in one function
              # Uses: getTables() and 'reshape2' package
  function(ll){
    tables = getTables(ll)
    solar_rad_txt = tables[[2]][-1] 
    
    # make it into a text connection since we're using text
    con = textConnection(solar_rad_txt) 
    solar_rad_data = read.delim(con, header = TRUE, stringsAsFactors = FALSE, quote = "", sep = "\t")
    
    solar_rad_data <- solar_rad_data[,2:14]
    colnames(solar_rad_data)[1] = "hour"
    
    # make massive table
    colnames(solar_rad_data)[1] <- "Month"
    solar_rad_data[3, 1] <- "Direct_Max_Day"
    
    # make the months into their own row
    new_row <- colnames(solar_rad_data)
    solar_rad_data[6,] <- new_row
    
    solar_rad_data_transposed <- as.data.frame(t(solar_rad_data))
    solar_rad_data_transposed <- solar_rad_data_transposed %>% row_to_names(row_number = 1)
    
    solar_rad_data_transposed_month <- solar_rad_data_transposed[6]
    solar_rad_data_transposed_numeric <- sapply( solar_rad_data_transposed[c(1:5)], as.numeric )
    
    solar_rad_data_transposed <- bind_cols(solar_rad_data_transposed_month, solar_rad_data_transposed_numeric)
    
    rownames(solar_rad_data_transposed) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    
    solar_rad_data_transposed
    
  }


# Average Hourly Statistics for Opaque Sky Cover -------------------------------
getAvgHourSkyCover = # Generate the table in one function
                     # Uses: getTables() and 'reshape2' package
  function(ll){
    
    tables = getTables(ll)
    # select the correct table
    # based on standerd location in the file so hard-coding 3 is OK
    ave_hour_txt = tables[[3]][-1] 
    # make it into a text connection since we're using text
    con = textConnection(ave_hour_txt) 
    ave_hour_txt = read.delim(con, header = TRUE, stringsAsFactors = FALSE, quote = "", sep = "\t")
    # CITE [Generating a table from a .tsv file](https://stackoverflow.com/questions/51177077/reading-a-tab-separated-file-in-r)
    
    # tidy up table
    ave_hour_txt <- ave_hour_txt[,2:14]
    ave_hour_txt <- ave_hour_txt[1:24,]
    colnames(ave_hour_txt)[1] = "hour"
    
    hours <- seq(1, 24)
    ave_hour_txt$hour <- hours
    
    
    #make massive table
    ave_hour_sky_cover <- as.data.frame(melt(ave_hour_txt, id = 'hour'))
    colnames(ave_hour_sky_cover) <- c('hour', 'month', 'percent_covered')
    
    ave_hour_sky_cover$hour <- gsub('\\s+', '', ave_hour_sky_cover$hour)
    
    
    ave_hour_sky_cover
    
  }

# Psychrometric Chart Data -----------------------------------------------------

# FINISH - The column labels are not correct
getPsyched = # Generate the table in one function
  # Uses: getTables() and 'reshape2' package 
  # (I could possibly consolidate these functions since they have similar elements... 
  # but I think it is simplier for me to organize them like this LOL)
  function(ll){
    tables = getTables(ll)
    
    # select the correct table
    # based on standerd location in the file so hard-coding 3 is OK
    psych_txt = tables[[4]][-1] 
    # make it into a text connection since we're using text
    con = textConnection(psych_txt) 
    psych_txt_raw_data = read.delim(con, header = TRUE, stringsAsFactors = FALSE, quote = "", sep = "\t")
    
    # tidy up table
    psych_txt_raw_data <- psych_txt_raw_data[,2:13]
    
    # might cause problems if you have something in the last column might want to fix it
    colnames(psych_txt_raw_data) <- c('dewpoint', '<=-10','<=-5', "<=0", "<=5", "<=10",	"<=15",	"<=20", "<=25",	"<=30", "<=35", "<=40")
    
    #make massive table
    psych_chart <- as.data.frame(melt(psych_txt_raw_data, id = 'dewpoint'))
    # CITE [melt() function 'id' input](https://ademos.people.uic.edu/Chapter8.html)
    colnames(psych_chart) <- c('dewpoint', 'dry_bulb', 'percent')
    psych_chart <- na.omit(psych_chart)
    psych_chart
  }

# List of Dataframes -----------------------------------------------------------

getDataframes = # Outputs a list of dataframes in this order:
                # rel_humid, solar_rad, ave_hour_sky_cover, psych_chart
  function(fn){
    
    fp = paste(wd, fn, sep = "/")
    
    ll = readLines(fn, encoding = "latin1") # lines.list
    
    rel_humid = getHumid(ll)
    rel_humid$location <- fn
    
    solar_rad = getSolarRad(ll)
    solar_rad$location <- fn
    
    ave_hour_sky_cover = getAvgHourSkyCover(ll)
    ave_hour_sky_cover$location <- fn
    
    psych_chart = getPsyched(ll)
    psych_chart$location <- fn
    
    df_list <- list(rel_humid, solar_rad, ave_hour_sky_cover, psych_chart)
    df_list
    
  }