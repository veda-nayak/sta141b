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
blocks_0 = getBlocks(ll)

blocks_1 = getBlocks(getLines(fns[2]))

## Line 1 ---------------------------------------------------
line1_pattern = "^\\[\\*\\*\\] \\[[0-9]*:([0-9]*):[0-9]*\\] ([[:alpha:]| ]*).*"
snortId_pattern = "\\[[0-9]*:([0-9]*):[0-9]*\\]"
title_pattern = "([[:alpha:]| ]*)"

validLine1 = sapply(c(snortId_pattern, title_pattern), function(x) checkValid(blocks_0, 1, x))
validLine1Pretty = as.data.frame(validLine1)

rownames(validLine1Pretty) <- c("snortIdPattern", "titlePattern")
colnames(validLine1Pretty) <- c("TRUE")

linesWithLine1Els = sapply(blocks_0, function(x) grepl(title_pattern, x[1]))
linesWithLine1RegEx = sapply(blocks_0, function(x) grepl(line1_pattern, x[1]))

validLine1RegEx = table(linesWithLine1Els == linesWithLine1RegEx) # all are true

## Line 2 ---------------------------------------------------


validLine2 = sapply(c("Classification:", "Priority:"), function(x) checkValid(blocks_0, 2, x))
validLine2Pretty = as.data.frame(validLine2)

rownames(validLine2Pretty) <- c("'Classification:'", "'Priority:'")
colnames(validLine2Pretty) <- c("TRUE")

### Since the blocks that contain "Classification" and "Proximity" are the same as those that contain "Classification" and "Proximity" on line 2, my assumption holds. Next, I wanted to see if classification always came before proximity. 

linesWithLine2Els = sapply(blocks_0, function(x) grepl("Classification:", x[2]))
linesWithLine2RegEx = sapply(blocks_0, function(x) grepl("\\[Classification: (.*)\\] \\[Priority:", x[2]))

validLine2RegEx = table(linesWithLine2Els == linesWithLine2RegEx) # all are true


## Line 3 ---------------------------------------------------

day_month_pattern = "^([0-9]+)/([0-9]+)-.*"
time_pattern = "[0-9]+/[0-9]+-([0-9]+:[0-9]+:[0-9]+.[0-9]*) .*"
ip_pattern = "[0-9]+:[0-9]+:[0-9]+.[0-9]* (.*) +-> +(.*)"


validLine3 = sapply(c(day_month_pattern, time_pattern, ip_pattern), function(x) checkValid(blocks_0, 3, x))
validLine3Pretty = as.data.frame(validLine3) # All TRUE

rownames(validLine3Pretty) <- c("day_month_pattern", "time_pattern", "ip_pattern")
colnames(validLine3Pretty) <- c("TRUE")

## Line 4 ---------------------------------------------------
line4pattern = "^([[:graph:]]*) TTL:([0-9]*) TOS:([0-9]*[[:alpha:]]*[0-9]*) ID:([0-9]*) IpLen:([0-9]*) DgmLen:([0-9]*)(.*)"
protocol_pattern = "^([[:graph:]]+)"
ttl_pattern = "TTL:([0-9]*)"
tos_pattern = "TOS:([0-9]*[[:alpha:]]*[0-9]*)"
id_pattern = "ID:([0-9]*)"
iplen_pattern = "IpLen:([0-9]*)"
dgmlen_pattern = "DgmLen:([0-9]*)"

validLine4Els = sapply(c(protocol_pattern, ttl_pattern, tos_pattern, id_pattern,iplen_pattern, dgmlen_pattern), function(x) checkValid(blocks_0, 4, x))
validLine4Pretty = as.data.frame(validLine4Els)
rownames(validLine4Pretty) <- c("Protocol", "TTL", "TOS", "ID", "IpLen", "DgmLen")

linesWithLine4Els = sapply(blocks_0, function(x) grepl(ttl_pattern, x[4]))
linesWithLine4RegEx = sapply(blocks_0, function(x) grepl(line4pattern, x[4]))

validLine4RegEx = table(linesWithLine4Els == linesWithLine4RegEx) # all TRUE

## Line 5 ---------------------------------------------------
line5pattern_1 = "(.*) Seq: (.*) Ack: (.*) Win: (.*) TcpLen: (.*)"
# line5pattern_2 = "(.*)(Seq: (.*))* Ack: (.*) Win: (.*) TcpLen: (.*).*"
# tcpFlag_pattern # can't validate this because the regex is too general so we nede to validate after we validate the rest
seq_pattern = "Seq: (.*)"
ack_pattern = "Ack: (.*)"
win_pattern = "Win: (.*)"
tcplen_pattern = "TcpLen: (.*)"

validLine5Els = sapply(c(seq_pattern, ack_pattern, win_pattern, tcplen_pattern), function(x) checkValid(blocks_0, 5, x))

validLine5Pretty = as.data.frame(validLine5Els)
validLine5Pretty <- validLine5Pretty[,c(1, 2, 4, 6, 8)]
validLine5Pretty <- as.data.frame(t(validLine5Pretty))
validLine5Pretty <- validLine5Pretty[2:5,]
rownames(validLine5Pretty) <- c("Sequence", "Ack", "Window", "TcpLen")
validLine5Pretty[2:4, 1] <- 0 

## What's happening with Seq: -----------
seqNotLine5 = whichNotValid(blocks_0, 5, seq_pattern)
lines_seqNotLine5 = lapply(seqNotLine5, function(x) blocks_0[x])

lines_WithTypeNoLine5Els = as.data.frame(table(sapply(lines_seqNotLine5, function(x) grepl("Type:", x) == TRUE & grepl("Ack:", x) == FALSE & grepl("Win:", x) == FALSE & grepl("TcpLen:", x) == FALSE)))

ip_no_port= " [0-9]+.[0-9]+.[0-9]+.[0-9]+"
ip_with_port = " [0-9]+.[0-9]+.[0-9]+.[0-9]+:[0-9]+"

lines_WithMiscActivity = as.data.frame(table(sapply(lines_seqNotLine5, function(x) grepl("Misc activity", x))))
lines_NoPort = as.data.frame(table(sapply(lines_seqNotLine5, function(x) grepl(ip_no_port, unlist(x)[3]))))
lines_WithPort = as.data.frame(table(sapply(lines_seqNotLine5, function(x) grepl(ip_with_port, unlist(x)[3]))))

noSeqSummaryTable = rbind(lines_WithTypeNoLine5Els, lines_WithMiscActivity, lines_NoPort, lines_WithPort)
rownames(noSeqSummaryTable) <- c("Blocks With 'Type:' on Line 5 & Missing Remaining Line5 Elements", "Blocks with Misc Activty", "IP-NoPort", "IP-WithPort")

# Since we've validated they're all on line 5, we just need to select one of the patterns as our comparison logical vector to explore these affects. 

# THIS PART IS UNHAPPY
linesWithLine5Els = sapply(blocks_0, function(x) grepl(ack_pattern, x[5]))

linesWithSeq = sapply(blocks_0, function(x) grepl(seq_pattern, x[5]))

linesWithLine5RegEx_1 = sapply(blocks_0, function(x) grepl(line5pattern_1, x[5]))
# linesWithLine5RegEx_2 = sapply(blocks, function(x) grepl(line5pattern_2, x[5]))

# validLine5RegEx_1_vs_2 = table(linesWithLine5RegEx_1 == linesWithLine5RegEx_2) # all TRUE therefore both are equal

validLine5RegEx = table(linesWithLine5Els == linesWithLine5RegEx_1) # all TRUE


## Extra Lines ---------------------------------------------------

extra_lines = extractExtraLines(blocks_0)

block_lens = sapply(blocks_0, length)

verify_extra_lines = table(sapply(c(1:length(blocks_0)) ,function(x) if (block_lens[x] > 5 & extra_lines[x,] != "" | block_lens[x] <= 5 & extra_lines[x,] == "" ){TRUE}else{FALSE}))


# Validate urls ----------------------------------------------------------------

# Check length
pattern = ".*(htt[p+|s]:\\/\\/.*)].*"

urlsTfTable_0 = table(grepl(pattern, getLines(fns[1]))) 

urls_valid_0 = (urlsTfTable_0[2] == length(urls_0)) # output = TRUE so correct # urls were extracted into data table

urlsTfTable_1 = table(grepl(pattern, getLines(fns[2]))) 
urls_valid_1 = (urlsTfTable_1[2] == length(urls_1)) # output = TRUE so correct # urls were extracted into data table

urls_validity = as.data.frame(c(urls_valid_0, urls_valid_1))

colnames(urls_validity) <- c("T/F")
rownames(urls_validity) <- c("00000", "00001")

# Validate Classes -------------------------------------------------------------
class_0 <- class(df_0)
class_1 <- class(df_1)
class_0_1 <- cbind(class_0, class_1)



# everything is a charecter because there are some NA's 
classes_0 <- as.data.frame(sapply(df_0, function(x) class(x[c(1:25)])))
colnames(classes_0) <- c("Class - df_0")
classes_1 <- as.data.frame(sapply(df_1, function(x) class(x[c(1:25)])))
colnames(classes_1) <- c("Class - df_1")
classes_0_1 <- cbind(classes_0, classes_1)

# URLs
class_urls_0 = class(urls_0) # char
class_urls_1 = class(urls_1) # char
class_urls_0_1 <-rbind(class_urls_0, class_urls_1)

## Dimensions ---------------------------------

dims_0_1 = cbind(as.data.frame(dim(df_0)), as.data.frame(dim(df_1)))

length_df = cbind(as.data.frame(length(blocks_0)), as.data.frame(length(blocks_1)))

colnames(dims_0_1) <- c("_00000", "_00001")
colnames(length_df) <- c("_00000", "_00001")

valid_dims = rbind(dims_0_1, length_df)
rownames(valid_dims) <- c("Observations", "Variables", "Number of Blocks")
valid_dims <- valid_dims[c(1, 3, 2), ]

# Check "NA" Values -------------------------------

comparePorts = as.data.frame(table(sapply(c(1:length(blocks_0)) , function(x) whichPortNotNa(df_0, x))))

compareLine5Els = as.data.frame(table(sapply(c(1:length(blocks_0)) , function(x) whichLine5NotNa(df_0, x))))
  
compareNas = cbind(as.data.frame(t(comparePorts)), as.data.frame(t(compareLine5Els)))
colnames(compareNas) <- c("Port NAs", "Line_5_Els_NAs")
rownames(compareNas) <- c("T/F", "Frequency")
  