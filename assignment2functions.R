# Import Packages ------------------------------------------------------
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))
suppressWarnings(library(gridExtra))
suppressWarnings(library(reshape2))
suppressWarnings(library(data.table))
# suppressWarnings(library(janitor))

# Set working directory + bring in test files ----------------------------------

dir = "C:/cygwin64/home/vedan/Code/sta141b/"

wd = paste(dir, "/alert", sep = "")

setwd(wd)

fn1 = "test.pcap"
fn2 = "alert.full.maccdc2012_00000.pcap"

# Get the blocks ---------------------------------------------------------------

getBlocks = # make all blocks
  # Uses: findTitles
  function(ll){
    
    starts = grepl("^\\s*$", ll)
    
    # group the lines based on blocks between trues
    w = starts
    g = cumsum(w) # this is giving the starting index to split by
    
    tot_blocks = split(ll, g) # this is where it is going wrong
    names(tot_blocks) = sapply(tot_blocks, `[`, 1)
    
    tot_blocks
    
  }



fp = paste(wd, fn2, sep = "/")

ll = readLines(fn2, encoding = "latin1")

blocks = getBlocks(ll)
blocks = lapply(blocks, function(x){ x[!is.na(x) & x != ""]})

line3_df = extractLine3(blocks)

makeTable = 
  function(fn){
    
    fp = paste(wd, fn, sep = "/")
    
    ll = readLines(fn, encoding = "latin1") # lines.list
    
  }

makeTable(fn1)

# line 3
#  strsplit(blocks[[1]][3], " |:")
# line 4 (validate structure always the same)
# strsplit(blocks[[1]][4], " |:")
# split line 5 for all
# sapply(blocks, function(x) strsplit(x[5], " |:"))
# to do for all lines of a block
# strsplit(blocks[[1]][3-5], " ") # get the common info

# summary(Rprof(filename), rf) --> which functions took the most time

# Extract data from block line 2 -----------------------------------------------

extractLine1 = # get the class and priority from a block's line 2
  function(blocks){
    
    line1s = sapply(blocks, function(x) x[1])
    line1_test = line1s[[2]]
    
    line1_pattern_table = table(grepl("[[:digit:]:]", line1s)) # everything follows this pattern
    line1_pattern_table = table(grepl("^\\[\\*\\*\\]", line1s))
    
  }

# Extract data from block line 2 -----------------------------------------------

extractLine2 = # get the class and priority from a block's line 2
  function(blocks){
    
    line2s = sapply(blocks, function(x) x[2])
    # pseudo_line2 = "[Classification: Web Application Attack] [Priority: 1]"
    
    # grepl to to see if classification before priority
    
    line2split = sapply(line2s, function(x) strsplit(x, split = "\\[|\\]"))
    
    line2split = lapply(line2split, function(x){ x[!is.na(x) & x != "" & x != " "]})
    
    classification = sapply(line2split, function(x) strsplit(x[1], split = ": +")[[1]][2])
    proximity = sapply(line2split, function(x) strsplit(x[2], split = ": +")[[1]][2])
    
    line2details <- as.data.frame(cbind(classification, proximity))
    line2details$proximity = as.integer(line2details$proximity)
    
    line2details
    
  }

# TABLE FOR LINE 3 -------------------------------------------------------------


make2ndNA = # make missing port values into "NA"
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
  source_ip_split_updated = sapply(source_ip_split, make2ndNA)
  source_ip_df = as.data.frame(do.call(rbind, source_ip_split_updated))
  source_ip_df = source_ip_df[1:2]
  
  destination_ip = sapply(ip_split, function(x) x[2])
  destination_ip_split = sapply(destination_ip, function(x) strsplit(x, split = ":"))
  destination_ip_split_updated = sapply(destination_ip_split, make2ndNA)
  destination_ip_df = as.data.frame(do.call(rbind, destination_ip_split_updated))
  destination_ip_df = destination_ip_df[1:2]
  
  ip_df <- cbind(source_ip_df, destination_ip_df)
  colnames(ip_df) <- c('SourceIP', 'SourcePort', 'DestinationIP', 'DestinationPort')
  
  day_month_time_ip <- cbind(day_month_df, time_df, ip_df)
  
  }

# TABLE FOR LINE 5 -------------------------------------------------------------
# line5s = sapply(blocks, function(x) strsplit(x[5], " |:"))
# table(grepl(">", line4s))
# which(grepl(">", line4s) == FALSE) #that's okay, last one is
# 
# blocks[[1]][5] # > TTl is on line 4
# blocks[[166492]][5] # > NA
# 
# # Solution: Add "" as the first element of 1
# # blocks[[1]] <- c("", blocks[[1]])
# # Remove line 166492
# # FINISH
# 
# line5s = sapply(blocks, function(x) strsplit(x[5], " |:"))
# 
# # make a datatable with the line5s
# line5s.table <- do.call(rbind, line5s)
# line5s.table <- as.data.frame(line5s.table)
# 
# # validate this makese sense
# table(line5s.table[2] == "TTL") # output all true
# table(line5s.table[4] == "TOS") # all true
# table(line5s.table[6] == "ID") # all true
# table(line5s.table[8] == "IpLen") # all true
# table(line5s.table[8] == "IpLen") # all true
# table(line5s.table[10] == "DgmLen") # all true
# 
# # rename columns
# colnames(lines.table) <- c("Protocol", "TTL.delete", "TTL", "TOS.delete", "TOS", "ID.delete", "ID", "IpLen.delete", "IpLen", "DgmLen.delete", "DgmLen", "Extra")
# 
# # remove the columns we need to delete
# line5s.table <- line5s.table[,c(1, 3, 5, 7, 9, 10, 11)]

# TABLE FOR LINE 6 -------------------------------------------------------------
# line6s = sapply(blocks, function(x) strsplit(x[6], " |:"))
# 
# #remove empty strings
# line6s= lapply(line6s, function(x){ x[!is.na(x) & x != ""]})
# # CITE: https://stackoverflow.com/questions/58977189/remove-empty-strings-in-a-list-of-lists-in-r
# 
# # Check if the same elements are in each
# table(grepl("Seq", line6s)) # output, about half do not contain the sequence (could make a table)
# notRight6 <- which(grepl("Seq", line6s) == FALSE) # output: these are the lines containing the error type and code 
# 
# 
# # make a datatable with the line5s
# line6s.table <- do.call(rbind, line6s)
# line6s.table <- as.data.frame(line6s.table)
# 
# # OPTION: I could just take the rows that contain type + code 
# # and make them their own column + put NA for the remaining values
# 
# line6s.error.table <- line6s.table %>% filter(V1 == "Type")
# 
# # validate this makese sense
# table(line5s.table[2] == "TTL") # output all true
# table(line5s.table[4] == "TOS") # all true
# table(line5s.table[6] == "ID") # all true
# table(line5s.table[8] == "IpLen") # all true
# table(line5s.table[8] == "IpLen") # all true
# table(line5s.table[10] == "DgmLen") # all true
# 
# # rename columns
# colnames(line5s.table) <- c("Protocol", "TTL.delete", "TTL", "TOS.delete", "TOS", "ID.delete", "ID", "IpLen.delete", "IpLen", "DgmLen.delete", "DgmLen", "Extra")
# 
# # remove the columns we need to delete
# line5s.table <- line5s.table[,c(1, 3, 5, 7, 9, 10, 11)]

