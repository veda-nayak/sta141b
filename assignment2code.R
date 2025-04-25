dir = "C:/cygwin64/home/vedan/Code/sta141b"
setwd(dir)
source(paste(dir, "/assignment2functions.R", sep=''))

blocks = getBlocks(ll)

line2_df <- extractLine2(blocks)
line3_df <- extractLine3(blocks) # way to validate, is NA in source always NA in destination

