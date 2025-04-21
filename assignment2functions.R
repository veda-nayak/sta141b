# Import Packages ------------------------------------------------------
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))
suppressWarnings(library(gridExtra))
suppressWarnings(library(reshape2))
suppressWarnings(library(data.table))
suppressWarnings(library(janitor))

# Set working directory + bring in test files ----------------------------------

dir = "C:/cygwin64/home/vedan/Code/sta141b/"

wd = paste(dir, "/alert", sep = "")

setwd(wd)

fn = "alert.full.maccdc2012_00000.pcap"

fp = paste(wd, fn, sep = "/")

ll = readLines(fn, encoding = "latin1")

blocks = getBlocks(ll)

# Get the blocks ---------------------------------------------------------------

getBlocks = # make all blocks
  # Uses: findTitles
  function(ll){
    
    starts = grep("^\\s*$", ll)
    browser()
    # group the lines based on blocks between trues
    w = starts
    g = cumsum(w) # this is giving the starting index to split by
    
    tot_blocks = split(ll, g) # this is where it is going wrong
    names(tot_blocks) = sapply(tot_blocks, `[`, 1)
    
    tot_blocks
    
  }

# tableStarts = # get the starting values of the blocks we want
#   function(ll){
# 
#     check_blocks = lapply(getBlocks(ll), findTitles)
#     table_starts = which(check_blocks == TRUE)
# 
#     table_starts
# 
#   }

# Extract data from block line 1 -----------------------------------------------

findTitle = 
  function(block){
    
    
    }

# Extract data from block line 2 -----------------------------------------------

extractLine2 = # get the class and priority from a block's line 2
  function(block){
    
    line2 = block[2]
    # pseudo line2 = "[Classification: Web Application Attack] [Priority: 1]"
    parts = gsub("\\[Classification: (.*)\\] \\[Priority: (.*)\\]", "\\1 ; \\2", line2)
    parts_split = unlist(strsplit(parts, split = " ; ")) 
    
    # parts_split[1] = classification
    # parts_split[2] = priority
    
    parts_split
    
  }

# Extract data from block line 3 -----------------------------------------------

extractLine4 = # month/day/time + IP/port info from a block's line 3
  function(block){
    
    # line3 = block[3]
    line3 = "03/16-07:30:00.840000 192.168.202.79:50465 -> 192.168.229.251:80"
    
    # day-time =  03/16-08:30:07.840000
    # source IP : port
    # destination IP : port

    parts = gsub("([0-9][0-9])\\/([0-9][0-9])-([0-9][0-9]):([0-9][0-9]):([0-9][0-9].[0-9]*) ([0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*):([0-9]*) -> ([0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*):([0-9]*)", "\\1 ; \\2 ; \\3 ; \\4 ; \\5 ; \\6 ; \\7 ; \\8 ; \\9", line3)
    parts_split = unlist(strsplit(parts, split = " ; ")) 
    
    # 1 - month
    # 2 - day
    # 3 - hour
    # 4 - minute
    # 5 - second.millisecond
    # 6 - source IP
    # 7 - source port
    # 8 - destination IP
    # 9 - destination port
    
    parts_split
    
  }


# Extract data from block line 4 -----------------------------------------------

extractLine4 = # BLANK info from a block's line 4
  function(block){
    
    # line4 = block[4]
    line4 = "TCP TTL:64 TOS:0x0 ID:6622 IpLen:20 DgmLen:1420 DF"
    
    parts = gsub("([:alpha:]*) TTL:([0-9]*) TOS:([0-9]*x[0-9]*) ID:([0-9]*) IpLen:([0-9]*) DgmLen:([0-9]*) ([:alpha:]*)", "\\1 ; \\2 ; \\3 ; \\4 ; \\5 ; \\6 ; \\7 ", line4)
    parts_split = unlist(strsplit(parts, split = " ; ")) 
    
    # 1 - protocol
    # 2 - ttl
    # 3 - tos
    # 4 - id
    # 5 - iplen
    # 6 - dgmlen
    # 7 - extra
    
    parts_split
    
  }

# For later -----------------------------------------------
# to get the info for one bloc block[[i]]