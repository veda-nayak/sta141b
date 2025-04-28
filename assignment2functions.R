# Import Packages ------------------------------------------------------
suppressWarnings(library(dplyr))
# suppressWarnings(library(ggplot2))
suppressWarnings(library(gridExtra))
library(dplyr)

# Set working directory + bring in test files ----------------------------------

dir = "C:/cygwin64/home/vedan/Code/sta141b/"

wd = paste(dir, "/alert", sep = "")

setwd(wd)

# Get the blocks ---------------------------------------------------------------

getBlocks = # make all blocks
  # Uses: findTitles
  function(ll){
    
    
    starts = grepl("^\\s*$", ll)
    
    # group the lines based on blocks between trues
    w = starts
    if (w[length(w)] == TRUE) {
      w[1:(length(w)-1)]
      }
    
    g = cumsum(w) # this is giving the starting index to split by
    
    tot_blocks = split(ll, g)
    names(tot_blocks) = sapply(tot_blocks, `[`, 1)
    
    tot_blocks = lapply(tot_blocks, function(x){ x[!is.na(x) & x != ""]})
    
  }

# Extract data from block line 2 -----------------------------------------------

extractLine1 = # get the class and priority from a block's line 2
  function(blocks){
    
    line1s = sapply(blocks, function(x) x[1])
    line1_test = line1s[[2]] #"[**] [1:2009358:5] ET SCAN Nmap Scripting Engine User-Agent Detected (Nmap Scripting Engine) [**]"
    
    line1_pattern = "^\\[\\*\\*\\] \\[[0-9]*:([0-9]*):[0-9]*\\] ([[:alpha:]| ]*).*"
      
    line1_pattern_check = table(grepl(line1_pattern, line1s))
    
    line1_test_extract = gsub(line1_pattern, "\\1;\\2", line1_test)
    
    line1_extract = sapply(line1s, function(x) gsub(line1_pattern, "\\1;\\2", x))
    line1_split = sapply(line1_extract, function(x) strsplit(x, split = ";"))
    
    line1_df = do.call(rbind, line1_split)
    line1_df = as.data.frame(line1_df)
    
    colnames(line1_df) <- c('SnortID', 'Title')
    
    line1_df
    
  }

# Extract data from block line 2 -----------------------------------------------

extractLine2 = # get the class and priority from a block's line 2
  function(blocks){
    
    line2s = sapply(blocks, function(x) x[2])
    # pseudo_line2 = "[Classification: Web Application Attack] [Priority: 1]"
    
    # grepl to to see if classification before priority
    
    line2split = sapply(line2s, function(x) strsplit(x, split = "\\[|\\]"))
    
    line2split = lapply(line2split, function(x){ x[!is.na(x) & x != "" & x != " "]})
    
    Classification = sapply(line2split, function(x) strsplit(x[1], split = ": +")[[1]][2])
    Proximity = sapply(line2split, function(x) strsplit(x[2], split = ": +")[[1]][2])
    
    line2details <- as.data.frame(cbind(Classification, Proximity))
    line2details$Proximity = as.integer(line2details$Proximity)
    
    colnames(line2details) <- c('Classification', 'Proximity')
    
    line2details
    
  }

# TABLE FOR LINE 3 -------------------------------------------------------------


makeNaLine3 = # make missing port values into "NA"
  function(x){
    if (length(x) != 2){
      x[1] = x[1]
      x[2] = "NA"}
    else{
      x[1] = x[1]
      x[2] = x[2]
    }
    x
  }

extractLine3 = # 
  function(blocks){

  line3s = sapply(blocks, function(x) x[3])
  line3test =        "03/16-07:30:00.010000 192.168.202.79:50467 -> 192.168.229.251:80" # blocks[[2]][3]
  line3testUnreach = "03/16-07:30:00.060000 192.168.27.25 -> 192.168.202.100" # blocks[[8]][3]
  
  # Day/Month -------------
  check_day_month_pattern = table(grepl("^([0-9]+)/([0-9]+)-.*", line3s))
  day_month_pattern = "^([0-9]+)/([0-9]+)-.*"
  # day_month_test = gsub(day_month_pattern, "\\1;\\2", line3test)
  day_month = sapply(line3s, function(x) gsub(day_month_pattern, "\\1;\\2", x))
  # day_month_split_test = strsplit(day_month_test, split = ";")
  day_month_split = sapply(day_month, function(x) strsplit(x, split = ";"))
  day_month_df = as.data.frame(do.call(rbind, day_month_split))
  colnames(day_month_df) <- c('Month', 'Day')
  
  # Time ------------------
  check_time_pattern = table(grepl("[0-9]+/[0-9]+-([0-9]+:[0-9]+:[0-9]+.[0-9]*) .*", line3s))
  # time_test = gsub("[0-9]+/[0-9]+-([0-9]+:[0-9]+:[0-9]+.[0-9]*) .*", "\\1", line3test)
  time = sapply(line3s, function(x) gsub("[0-9]+/[0-9]+-([0-9]+:[0-9]+:[0-9]+.[0-9]*) .*", "\\1", x))
  time = list(time)
  time_df = as.data.frame(do.call(cbind, time))
  colnames(time_df) <- c('Time')
  
  # IP -------------------
  check_ip_pattern = table(grepl(".*[0-9]+:[0-9]+:[0-9]+.[0-9]* (.*) +-> +(.*)", line3s))
  # ip_test = gsub(".*[0-9]+:[0-9]+:[0-9]+.[0-9]* (.*) +-> +(.*)", "\\1;\\2", line3test)
  ip = sapply(line3s, function(x) gsub(".*[0-9]+:[0-9]+:[0-9]+.[0-9]* (.*) +-> +(.*)", "\\1;\\2", x))
  ip_split = sapply(ip, function(x) strsplit(x, split = ";"))
  
  source_ip = sapply(ip_split, function(x) x[1])
  source_ip_split = sapply(source_ip, function(x) strsplit(x, split = ":"))
  source_ip_split_updated = sapply(source_ip_split, makeNaLine3)
  source_ip_df = as.data.frame(do.call(rbind, source_ip_split_updated))
  source_ip_df = source_ip_df[1:2]
  
  destination_ip = sapply(ip_split, function(x) x[2])
  destination_ip_split = sapply(destination_ip, function(x) strsplit(x, split = ":"))
  destination_ip_split_updated = sapply(destination_ip_split, makeNaLine3)
  destination_ip_df = as.data.frame(do.call(rbind, destination_ip_split_updated))
  destination_ip_df = destination_ip_df[1:2]
  
  ip_df <- cbind(source_ip_df, destination_ip_df)
  colnames(ip_df) <- c('SourceIP', 'SourcePort', 'DestinationIP', 'DestinationPort')
  
  # Put it together ------
  day_month_time_ip <- cbind(day_month_df, time_df, ip_df)
  
  day_month_time_ip
  
  }

# TABLE FOR LINE 4 -------------------------------------------------------------

makeNaLine4 = # make missing extra values into "NA"
  function(x){
    x[1] = x[1]
    x[2] = x[2]
    x[3] = x[3]
    x[4] = x[4]
    x[5] = x[5]
    x[6] = x[6]
    
    if (length(x) != 7){
      x[7] = "NA"}
    
    else{
      x[7] = x[7]
      }
    x
  }

extractLine4 = 
  function(blocks){

  line4s = sapply(blocks, function(x) x[4])
  # line4s_test = line4s[[2]] #"TCP TTL:127 TOS:0x0 ID:1576 IpLen:20 DgmLen:218 DF"
  
  # check_line_pattern = table(grepl("^([[:graph:]]*) TTL:([0-9]*) TOS:([0-9]*[[:alpha:]]*[0-9]*) ID:([0-9]*) IpLen:([0-9]*) DgmLen:([0-9]*)(.*)", line4s))  

  # line4s_extract_test = gsub("^([[:graph:]]*) TTL:([0-9]*) TOS:([0-9]*[[:alpha:]]*[0-9]*) ID:([0-9]*) IpLen:([0-9]*) DgmLen:([0-9]*)(.*)", "\\1;\\2;\\3;\\4;\\5;\\6;\\7", line4s_test)
  line4_extract = sapply(line4s, function(x) gsub("^([[:graph:]]*) TTL:([0-9]*) TOS:([0-9]*[[:alpha:]]*[0-9]*) ID:([0-9]*) IpLen:([0-9]*) DgmLen:([0-9]*)(.*)", "\\1;\\2;\\3;\\4;\\5;\\6;\\7", x))
  line4_split = sapply(line4_extract, function(x) strsplit(x, split = ";"))
  
  line4_split_updated = sapply(line4_split, makeNaLine4)
  transformed_line4_split_updated = as.data.frame(t(line4_split_updated))
  
  colnames(transformed_line4_split_updated) <- c('Protocol', 'TTL', 'TOC', 'ID', 'IpLen', 'DgmLen', 'Extra')
  
  transformed_line4_split_updated$TTL <- as.integer(transformed_line4_split_updated$TTL)
  transformed_line4_split_updated$ID <- as.integer(transformed_line4_split_updated$ID)
  transformed_line4_split_updated$IpLen <- as.integer(transformed_line4_split_updated$IpLen)
  
  transformed_line4_split_updated
  
  }

# TABLE FOR LINE 5 -------------------------------------------------------------
newLine5 = 
  function(x){
    line5pattern_works = "(.*) Seq: (.*) Ack: (.*) Win: (.*) TcpLen: (.*)"
    
    if (grepl(line5pattern_works, x) == TRUE){
      x = gsub(line5pattern_works, "\\1;\\2;\\3;\\4;\\5", x)
    }
    x
  }

makeNaLine5 = # make missing extra values into "NA"
  function(x){
    
    if (length(x) == 5){
      x[1] = x[1]
      x[2] = x[2]
      x[3] = x[3]
      x[4] = x[4]
      x[5] = x[5]
      }
    
    else{
      x[1] = "NA"
      x[2] = "NA"
      x[3] = "NA"
      x[4] = "NA"
      x[5] = "NA"
    }
    
    x
  }

extractLine5 =  
  function(blocks){
    
    line5s = sapply(blocks, function(x) x[5])
    line5_test = line5s[[2]] # "***AP**F Seq: 0x54831ACE  Ack: 0x328B4920  Win: 0xFA4A  TcpLen: 32"
    new_line_5 = sapply(line5s, newLine5)
    new_line_5 <- as.character(new_line_5)
    line_5_split = sapply(new_line_5, function(x) strsplit(x, split = " ;|;"))
    line5_df = as.data.frame(t(as.data.frame(sapply(line_5_split, makeNaLine5))))
    colnames(line5_df) <- c('TCP_flag', "Seq", 'Ack', 'Window', 'TCP_len')
    line5_df
  }

# Extra Lines ------------------------------------------------------------------

# makeNaExtraLines = # no longer needed(I think), but I am scared to delete lines
#   function(x){
#     
#     if (x[8] == NA){
#       x[1] = x[1]
#       x[2] = x[2]
#       x[3] = x[3]
#       x[4] = x[4]
#       x[5] = x[5]
#       x[6] = x[6]
#     }
#     
#     else{
#       x[1] = "NA"
#       x[2] = "NA"
#       x[3] = "NA"
#       x[4] = "NA"
#       x[5] = "NA"
#     }
#     
#     x
#   }

extractExtraLines = 
  function(blocks){
    # block_lengths = sapply(blocks, function(x) length(x)) # 4 - 11
    extra_lines = sapply(blocks, function(x) x[-c(1, 2, 3, 4, 5)]) # 6 lines in the new situation
    extra_lines_single = sapply(extra_lines, function(x) paste(x, collapse = "\n"))
    extra_lines_df = as.data.frame(extra_lines_single)
  }


# The final DF -----------------------------------------------------------------
# (to the tune of the final countdown)

getLines= 
  function(fn){
    
    fp = paste(wd, fn, sep = "/")
    
    ll = readLines(fn, encoding = "latin1") # lines.list
    
    ll
    
  }
  

makeDF = 
  function(fn){
    
    ll = getLines(fn)
    
    blocks = getBlocks(ll)
    
    line1df <- extractLine1(blocks)
    line2df <- extractLine2(blocks)
    line3df <- extractLine3(blocks)
    line4df <- extractLine4(blocks)
    line5df <- extractLine5(blocks)
    extradf <- extractExtraLines(blocks)
    
    rownums = seq(from = 1, to = count(line1df)[1,1])
    rownames(line1df) <- rownums
    rownames(line2df) <- rownums
    rownames(line3df) <- rownums
    rownames(line4df) <- rownums
    rownames(line5df) <- rownums
    
   
    commonDf <- cbind(line1df, line2df, line3df, line4df, line5df, extradf)
    commonDf <- na.omit(commonDf) # we can do this because I labeled all missing values as "NA" not NA
                                  # it will just remove lines created by accident
    
    # Add a column for the name of the file
    without_ext = tools::file_path_sans_ext(fn)
    
    commonDf$filename <- without_ext
    
    commonDf$Month <- as.integer(commonDf$Month)
    commonDf$Day <- as.integer(commonDf$Day)
    commonDf$DgmLen <- as.integer(commonDf$DgmLen)
    commonDf$TTL <- as.integer(commonDf$TTL)
    commonDf$ID <- as.integer(commonDf$ID)
    commonDf$IpLen <- as.integer(commonDf$IpLen)
    
    commonDf
    
    
  }


