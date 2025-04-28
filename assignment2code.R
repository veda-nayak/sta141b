# Set the directories ----------------------------------------------------------
dir = "C:/cygwin64/home/vedan/Code/sta141b"
setwd(dir)

source(paste(dir, "/assignment2functions.R", sep=''))

wd = paste(dir, "/alert", sep = "")

setwd(wd)

# Make both dfs ----------------------------------------------------------------
fns = c("alert.full.maccdc2012_00000.pcap", "alert.full.maccdc2012_00001.pcap")

dfs = sapply(fns, makeDF)
