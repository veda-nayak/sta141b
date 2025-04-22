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

# split the lines of each block by a space -------------------------------------

splitBlocks = 
  function(block){
    
    
    
  }

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

extractLine3 = # month/day/time + IP/port info from a block's line 3
  function(block){
    
    line3 = block[3]
    # line3 = "03/16-07:30:00.840000 192.168.202.79:50465 -> 192.168.229.251:80"
    
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
    
    line4 = block[4]
    line4 = "TCP TTL:64 TOS:0x0 ID:6622 IpLen:20 DgmLen:1420 DF"
    
    parts = gsub("([:alpha:]*) TTL:([0-9]*) TOS:([0-9]*x[0-9]*) ID:([0-9]*) IpLen:([0-9]*) DgmLen:([0-9]*) ([:alpha:]*)", "\\1 ; \\2 ; \\3 ; \\4 ; \\5 ; \\6 ; \\7 ", line4)
    
    ttl = gsub("TTL:([0-9].*)", "\1", line4)
    
    tos = gsub("TOS:([0-9]*x[0-9]*)", "\\1", line4)
    id = gsub("ID:([0-9]*)", "\\1", line4)
    
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

# Extract data from block line 4 -----------------------------------------------

extractLine5 = # BLANK info from a block's line 5
  function(block){
    
    # line5 = block[5]
    line5 = " ***A**** Seq: 0x35DCBDC1 Ack: 0x8D3EB9C5 Win: 0xB5 TcpLen: 32"
    
    seq = gsub("Seq: ([:alnum:]*) ", "\\1", line5)
    
    
    
  }

# For later --------------------------------------------------------------------
# to get the info for one bloc block[[i]]
# table(grepl("pattern (to check starts with do ^)", ll))

# how can you check that a particular order is always met?
# split by a common value (I think we can use spaces here)
# els[[1]][1:5] --> use regex to say ^t=

# Grab the time
# what the part looks like - [[1]][1] = "t=0000"
# gsub("^t=", "", [[1]][1]) # replace the ^t= with ""
#leaves you with [[1]][1] = 0000
# t = sapply(els, `[`, 1)
# look at the class, check the length, make sure it makes sense
# time = gsub("^t=", "", t)


# Pos
# elsstrsplit(gsub("^pos=", "", els[[1]][3]), ",") --> [[1]] [1] "x" "y" "z"
# pos = sapply(els, `[`, 3)
# pels = strsplit(gsub("^pos=", "", pos), ",")
# gives you a list of vectors with x, y, z elements
# pels2 = do.call(rbind, pels) > this will give you a matrix with the elements you need

# 00:14:bf:b1:97:8d=-65,24300000,3
# I need 4 elements
# How can I split this? split by both an '=' and a ','
# strsplit(tmp, ",|=")
# output is a list of vectors with our 4 elemtns
# strsplit(tmp, "[,=]") does the same
# rest = lapply(els, `[`, -(1:4)) #not the common
# rest2 = strsplit(rest, function(x) do.call(rbind, strsplit(x, "[,=]")))
# output is a list of matrixes, length is equal to the number of lines
# rest3 = do.call(rbind, rest2)

# how do I know I got these right?
# check the class of everything, how many unique addresses do I have
# look at the number of rows for each column, does it match, does this  equal # elements - 4 

fp = paste(wd, fn2, sep = "/")

ll = readLines(fn2, encoding = "latin1")

blocks = getBlocks(ll)

makeTable = 
  function(fn){
    
    fp = paste(wd, fn, sep = "/")
    
    ll = readLines(fn, encoding = "latin1") # lines.list
    
    line2 = extractLine2(fn)
    line2
    
  }

makeTable(fn1)
