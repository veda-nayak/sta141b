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

# Get all the urls -------------------------------------------------------------

urls_0 = getUrls(fns[1])
urls_1 = getUrls(fns[2])

# Validate Function Assumptions ------------------------------------------------

ll = getLines(fns[1])
blocks = getBlocks(ll)

## Line 1 ---------------------------------------------------
line1_pattern = "^\\[\\*\\*\\] \\[[0-9]*:([0-9]*):[0-9]*\\] ([[:alpha:]| ]*).*"
snortId_pattern = "\\[[0-9]*:([0-9]*):[0-9]*\\]"
title_pattern = "([[:alpha:]| ]*)"

validSnort = checkValid(blocks, 1, snortId_pattern)
validTitle = checkValid(blocks, 1, title_pattern)

# Since we've validated they're all on line 1, we just need to select one of the patterns as our comparison logical vector to make sure the regular experession is sound. 
linesWithLine1Els = sapply(blocks, function(x) grepl(title_pattern, x[1]))
linesWithLine1RegEx = sapply(blocks, function(x) grepl(line1_pattern, x[1]))

validLine1RegEx = table(linesWithLine1Els == linesWithLine1RegEx) # all are true

## Line 2 ---------------------------------------------------
line1s = sapply(blocks, function(x) x[1])

### Classi ------
validClassi = checkValid(blocks, 2, "Classification:")

### Priority ------
validPriority = checkValid(blocks, 2, "Priority:")

### Since the blocks that contain "Classification" and "Proximity" are the same as those that contain "Classification" and "Proximity" on line 2, my assumption holds. Next, I wanted to see if classification always came before proximity. 
line2s = sapply(blocks, function(x) x[2])
regExMatch_ClassiProxi = table(sapply(line2s, function(x) grepl("\\[Classification: (.*)\\] \\[Priority:", x)))


## Line 3 ---------------------------------------------------

day_month_pattern = "^([0-9]+)/([0-9]+)-.*"
time_pattern = "[0-9]+/[0-9]+-([0-9]+:[0-9]+:[0-9]+.[0-9]*) .*"
ip_pattern = "[0-9]+:[0-9]+:[0-9]+.[0-9]* (.*) +-> +(.*)"

### Day + Month ------

validDayMonth = checkValid(blocks, 3, day_month_pattern)

### Time ------
validTime = checkValid(blocks, 3, time_pattern)

### Day + Month ------
validIp = checkValid(blocks, 3, ip_pattern)


# I can see if # lines w/ TTL == # lines that follow the reg ex I wrote
# Maybe I can check if lines w/ na match lines w/o win

## Line 4 ---------------------------------------------------
line4pattern = "^([[:graph:]]*) TTL:([0-9]*) TOS:([0-9]*[[:alpha:]]*[0-9]*) ID:([0-9]*) IpLen:([0-9]*) DgmLen:([0-9]*)(.*)"
protocol_pattern = "^([[:graph:]]+)"
ttl_pattern = "TTL:([0-9]*)"
tos_pattern = "TOS:([0-9]*[[:alpha:]]*[0-9]*)"
id_pattern = "ID:([0-9]*)"
iplen_pattern = "IpLen:([0-9]*)"
dgmlen_pattern = "DgmLen:([0-9]*)"

validLine4Els = sapply(c(protocol_pattern, ttl_pattern, tos_pattern, id_pattern, id_pattern, iplen_pattern, dgmlen_pattern), function(x) checkValid(blocks, 4, x))

# Since we've validated they're all on line 4, we just need to select one of the patterns as our comparison logical vector.
linesWithLine4Els = sapply(blocks, function(x) grepl(ttl_pattern, x[4]))
linesWithLine4RegEx = sapply(blocks, function(x) grepl(line4pattern, x[4]))

validLine4RegEx = table(linesWithLine4Els == linesWithLine4RegEx) # all TRUE

## Line 5 ---------------------------------------------------
line5pattern_1 = "(.*) Seq: (.*) Ack: (.*) Win: (.*) TcpLen: (.*)"
line5pattern_2 = "(.*)(Seq: (.*))* Ack: (.*) Win: (.*) TcpLen: (.*).*"
# tcpFlag_pattern # can't validate this because the regex is too general so we nede to validate after we validate the rest
seq_pattern = "Seq: (.*)"
ack_pattern = "Ack: (.*)"
win_pattern = "Win: (.*)"
tcplen_pattern = "TcpLen: (.*)"

validLine5Els = sapply(c(seq_pattern, ack_pattern, win_pattern, tcplen_pattern), function(x) checkValid(blocks, 5, x))
# From this, we can see two things:
# 1. Elements "Ack:", "Win:", and "TcpLen:" are all on line 5 (since they all lines containing those values were the same as the lines containing those values on line 5)
# 2. Element "Seq:" was not only on line 5. 3262 lines contained "Seq:" which were not on line 5
        # - I wanted to further investigate these. 

seqNotLine5 = whichNotValid(blocks, 5, seq_pattern)
lines_seqNotLine5 = lapply(seqNotLine5, function(x) blocks[seqNotLine5])

# I looked at a couple of the corresponding blocks, and I saw that some of them contained "DESTINATION UNREACHABLE". I wanted to see if that was the same for all of the blocks that don't match.
lines_WithDestinationunreachable = table(sapply(lines_seqNotLine5, function(x) grepl("DESTINATION UNREACHABLE", x)))


# -----

# Since we've validated they're all on line 5, we just need to select one of the patterns as our comparison logical vector to explore these affects. 
linesWithLine5Els = sapply(blocks, function(x) grepl(seq_pattern, x[5]))

linesWithSeq = sapply(blocks, function(x) grepl(seq_pattern, x[5]))

linesWithLine5RegEx_1 = sapply(blocks, function(x) grepl(line5pattern_1, x[5]))
linesWithLine5RegEx_2 = sapply(blocks, function(x) grepl(line5pattern_2, x[5]))

validLine5RegEx = table(linesWithLine5Els == linesWithLine5RegEx) # all TRUE


## Extra Lines ---------------------------------------------------


# Validate Classes -------------------------------------------------------------
class_0 <- class(df_0)
class_1 <- class(df_1)

# everything is a charecter because there are some NA's 
classes_0 <- as.data.frame(sapply(df_0, function(x) class(x[c(1:25)])))
classes_1 <- as.data.frame(sapply(df_1, function(x) class(x[c(1:25)])))

# Verify that the the extralines were extracted correctly ----------------------
## df_0 ---------------------------------
blocks_0 = getBlocks(getLines(fns[1]))
blocks_0_length_table = as.data.frame(table(sapply(blocks_0, function(x) length(x))))

withoutMoreThan5_0 =blocks_0_length_table[1,2] +
                    blocks_0_length_table[2,2] + 
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

withoutMoreThan5_1 = blocks_1_length_table[1,2] +  blocks_1_length_table[2,2]

withMoreThan5_1 =   blocks_1_length_table[3,2] + 
                    blocks_1_length_table[4,2] + 
                    blocks_1_length_table[5,2] + 
                    blocks_1_length_table[6,2]

noMoreThan5_1 = df_1 %>% filter(extra_lines_single == "")
moreThan5_1 = df_1 %>% filter(extra_lines_single != "")

dim(noMoreThan5_1)[1] == withoutMoreThan5_1 # output = TRUE
dim(moreThan5_1)[1] == withMoreThan5_1 # output = TRUE

## Since both outputs == TRUE the extra lines extraction was successful for both 
## data frames for the blocks with and without at least 5 lines, the extraction for
## the extra lines was successful.

# Validate urls ----------------------------------------------------------------
class_urls_0 = class(urls_0) # char
class_urls_1 = class(urls_1) # char

# Check length
pattern = ".*(htt[p+|s]:\\/\\/.*)].*"

urlsTfTable_0 = table(grepl(pattern, getLines(fns[1]))) 
#I made this table as I was creating the function to see if the resulting values
# based on my regular expressions seemed logical
urlsTfTable_0[2] == length(urls_0) # output = TRUE

urlsTfTable_1 = table(grepl(pattern, getLines(fns[2]))) 
urlsTfTable_1[2] == length(urls_1) # output = TRUE


# Notes from class -------------------------------------------------------------

## 4/29 ---------------------

ll = getLines(fns[1])
blocks = getBlocks(ll)

linesWithWin = sapply(blocks, function(x) grep("Win:", x, value=TRUE)) # do this to confirm that classification is always on line 2 for example
# you can also compare length of each blocks to the number of columns filled out in the final df

table(sapply(linesWithWin, length))
# w  == lines w/o Win:
# sapply(blocks[w])

linesWithTtl = sapply(blocks, function(x) grep("TTL:", x, value=TRUE))
table(sapply(linesWithTtl, length))

# sapply(blocks[!w], function(x) grep("TTL", x, value = TRUE))
# should be a charecter vector, all containing tcp + ttl
# make sure all is TCP --> gsub("^[^ ]+ .*", "\\1", x) # capture everything at the start of the line before the first space

# [^ ] == everything but a space


# alternative for ip
ipRegEx = "^([0-9]{1,3}\\.?){4}"

sapply(blocks, function(x) gregexpr("//]", blocks[[x]][1]))

