
# Import Packages
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))
suppressWarnings(library(kableExtra))

# Set working directory + bring in test files
setwd("C:/cygwin64/home/vedan/Code/sta141b")
fn = "test.clm"
tf = "testText.clm"


# Start of fixed width columns
ColStarts = c(1, 7, 9, 16)

# Names for the dataframe columns
dfNames <- c('day', 
  'month', 
  'hour', 
  'diffuse_solar_horizontal', 
  'dry_bulb_temp', 
  'normal_solar_intensity', 
  'wind_speed', 
  'wind_direction', 
  'rel_humidity')

# SMALLER FUNCTIONS
getBlocks =  # get the grouped lines
  function(lines.list)
  {
    # find the starts and ends
    starts = substring(lines.list, 1, 6) == "* day "
    ends = rep(FALSE, length(starts))
    ends[which(starts)+24] <- TRUE
    
    # group the lines based on blocks between trues
    w = starts | ends
    g = cumsum(w)
    ans = split(lines.list, g)
    names(ans) = sapply(ans, `[`, 1)
    ans
  }

getTables = # make the block tables into a list
  # this calls getBlocks() and gets the resulting elements
  # whose names start with "* day "
  function(lines.list)
  {
    block = getBlocks(lines.list)
    w = substring(names(block), 1, 6) == "* day "
    block[w]
  }

splitDataLines = function(dataLines){
  
  strsplit(dataLines, split = ',')
  
}

readTableData = 
  function(tt, colStarts = ColStarts, widths = c(diff(colStarts), nchar(tt[1])), numTables)
  {
    # CREATE THE DATAFRAME
    df <- as.data.frame(matrix(0, 24, ncol = 9))
    colnames(df) <- c('day', 
                      'month', 
                      'hour', 
                      'diffuse_solar_horizontal', 
                      'dry_bulb_temp', 
                      'normal_solar_intensity', 
                      'wind_speed', 
                      'wind_direction', 
                      'rel_humidity')
    
    # DAY AND MONTH INFORMATION
    firstLine = tt[1] # we want to work with the first line to get the day / month information
    # pass a textConnection() to read.fwf since we are n asking
    # it to read a file but to process the contents we have alread read.
    con = textConnection(firstLine)
    ans = read.fwf(con, widths)
    
    day <- ans[2]
    month <- ans[4]
    
    dataLines = tt[-1]
    
    dataPoints <- lapply(dataLines, splitDataLines)
    
    df[,1] <- day
    df[,2] <- month
    
    for (i in 1:24){

      forExtraction <- unlist(dataPoints)
      df[i, 3] <- i
      df[i, 4] <- forExtraction[i*1]
      df[i, 5] <- forExtraction[i*2]
      df[i, 6] <- forExtraction[i*3]
      df[i, 7] <- forExtraction[i*4]
      df[i, 8] <- forExtraction[i*5]
      df[i, 9] <- forExtraction[i*6]

    }
    
    df$day_month <- interaction(df$day, df$month, sep = '-')
    
    as.data.frame(df)
    

  }


makeTables = # read table data >hours 1-24 for each day
  # Top-level/starting function
  function(file, colStarts = ColStarts)
  {
    lines.list = readLines(file) # get me the lines
    tables = getTables(lines.list) # get me tables
    numTables = length(tables)
    ans = lapply(tables, readTableData, colStarts) # read those tables
    
    df <- mergeTables(ans)
  #   df = as.data.frame(ans[1])
  #   colnames(df) <- dfNames
  #   
  #   for (i in 2:length(ans)){
  #     
  #     addition <- as.data.frame(ans[i])
  #     colnames(addition) <- dfNames
  #     df <- bind_rows(df, addition)
  #     
  #   }
  #   
  #   df
  #   df$location <- basename(file)
  #   colnames(df)[colnames(df) == '...10'] <- 'day_month'
  #   df
  #   
  }

fileName = 
  function(name){
    
    paste("C:/cygwin64/home/vedan/Code/sta141b/Solar1/unzip/", name, sep = "")
    
  }

mergeTables = 
  function(df.list){
    
    df = as.data.frame(df.list[1])
    colnames(df) <- dfNames
    
    for (i in 2:length(df.list)){
      
      addition <- as.data.frame(df.list[i])
      colnames(addition) <- dfNames
      df <- bind_rows(df, addition)
      
    }

  df

  }

