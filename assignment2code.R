# Set the directories ----------------------------------------------------------
dir = "C:/cygwin64/home/vedan/Code/sta141b"
setwd(dir)

source(paste(dir, "/assignment2functions.R", sep=''))

wd = paste(dir, "/alert", sep = "")

setwd(wd)

# Make both dfs ----------------------------------------------------------------
fns = c("alert.full.maccdc2012_00000.pcap", "alert.full.maccdc2012_00001.pcap")

df_0 = makeDF(fns[1])
df_1 = makeDF(fns[2])


# Verify that the the extralines were extracted correctly ----------------------
## df_0 ---------------------------------
blocks_0 = getBlocks(getLines(fns[1]))
blocks_0_length_table = as.data.frame(table(sapply(blocks_0, function(x) length(x))))

withoutMoreThan5_0 =blocks_0_length_table[2,2] + 
                    blocks_0_length_table[3,2] 
withMoreThan5_0 =   blocks_0_length_table[4,2] + 
                    blocks_0_length_table[5,2] + 
                    blocks_0_length_table[6,2] + 
                    blocks_0_length_table[7,2]

noMoreThan5_0 = df_0 %>% filter(extra_lines_single == "")
moreThan5_0 = df_0 %>% filter(extra_lines_single != "")

dim(noMoreThan5_0)[1] == withoutMoreThan5_0 # output = TRUE
dim(moreThan5_0)[1] == withMoreThan5_0 # output = TRUE

## df_1 ---------------------------------

blocks_1 = getBlocks(getLines(fns[2]))
blocks_1_length_table = as.data.frame(table(sapply(blocks_1, function(x) length(x))))

withoutMoreThan5_1 =blocks_1_length_table[2,2]

# FINISH the bottom is giving issues, it is always 2 higher than the other
withMoreThan5_1 =   blocks_1_length_table[3,2] + 
                    blocks_1_length_table[4,2] + 
                    blocks_1_length_table[5,2] + 
                    blocks_1_length_table[6,2]

noMoreThan5_1 = df_1 %>% filter(extra_lines_single == "")
moreThan5_1 = df_1 %>% filter(extra_lines_single != "")

dim(noMoreThan5_1)[1] == withoutMoreThan5_1 # output = TRUE
dim(moreThan5_1)[1] == withMoreThan5_1 # output = FALSE

## Since both outputs == TRUE the extra lines extraction was successful for both 
## dataframes for the blocks with and without at least 5 lines, the extraction for
## the extra lines was successful.
