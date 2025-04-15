
# Import Packages ------------------------------------------------------
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))
suppressWarnings(library(gridExtra))
suppressWarnings(library(reshape2))
suppressWarnings(library(data.table))
suppressWarnings(library(janitor))

# Something cool in class: string = lapply(string, function(x) x[x != ""] )
# day = sapply(string, function(x), x[3]) = sapply(string, `[`, 3) = get all the elements in the third element

# Set working directory + bring in test files ----------------------------------
dir1 = "C:/cygwin64/home/vedan/Code/sta141b/"

wd = paste(dir, "/Solar1/unzip", sep = "")

setwd(wd)

dir2 = "C:/cygwin64/home/vedan/Code/sta141b/"
source(paste(dir2, "/assignment1part2functions.R", sep = ""))

# - find all lines w/ " - Average.." 
# (do an or where you look and see if any of the tables are found) = w1 = first line
# last line will be the next " - " = w2
# w3 = w1 & w2


fn = "USA_CA_Mammoth.Yosemite.AP.723894_TMYx.2009-2023.stat"

fp = paste(dir, fn, sep = "/")

ll = readLines(fn, encoding = "latin1") # lines.list

ave_hour_txt = tables[[3]][-1] 
# make it into a text connection since we're using text
con = textConnection(ave_hour_txt) 
ave_hour_txt_raw_data = read.delim(con, header = TRUE, stringsAsFactors = FALSE, quote = "", sep = "\t")
# CITE [Generating a table from a .tsv file](https://stackoverflow.com/questions/51177077/reading-a-tab-separated-file-in-r)

# tidy up table
ave_hour_txt_raw_data <- ave_hour_txt_raw_data[,2:14]
ave_hour_txt_raw_data <- ave_hour_txt_raw_data[1:24,]
colnames(ave_hour_txt_raw_data)[1] = "hour"

hours <- seq(1, 24)
ave_hour_txt_raw_data$hour <- hours


#make massive table
ave_hour_sky_cover <- as.data.frame(melt(ave_hour_txt_raw_data, id = 'hour'))
colnames(ave_hour_sky_cover) <- c('hour', 'month', 'percent_covered')

ave_hour_sky_cover$hour <- gsub('\\s+', '', ave_hour_sky_cover$hour)


ave_hour_sky_cover

#  ---------------------------------------------------------------------------------------------------

humid_txt = tables[[1]][-1] 
# make it into a text connection since we're using text
con = textConnection(humid_txt) 
humid_raw_data = read.delim(con, header = TRUE, stringsAsFactors = FALSE, quote = "", sep = "\t")

humid_raw_data <- humid_raw_data[,2:14]

# Make the maximum + minimum day:hour values more specific
humid_data[2, 1] <- "Maximum_Day:Hour"
humid_data[4, 1] <- "Minimum_Day:Hour"

# change column name to make it more logical for later transformations
colnames(humid_data)[1] <- "Month"

# make the months into their own row
new_row <- colnames(humid_data)
humid_data[8,] <- new_row

humid_data_transposed <- transpose(humid_data)
# CITE [Transpose Data](https://community.sisense.com/t5/cdt/transposing-tables-in-r-and-python/ta-p/9361)

humid_data_transposed <- humid_data_transposed %>% row_to_names(row_number = 1)
# CITE

humid_data_transposed$max_day <- NA
humid_data_transposed$max_hour <- NA
humid_data_transposed$min_day <- NA
humid_data_transposed$min_hour <- NA

# Remove whitespace
humid_data_transposed$`Minimum_Day:Hour` <- gsub('\\s+', '', humid_data_transposed$`Minimum_Day:Hour`)
humid_data_transposed$`Maximum_Day:Hour` <- gsub('\\s+', '', humid_data_transposed$`Maximum_Day:Hour`)

row_num = as.numeric(dim(humid_data_transposed)[1])

max_split = strsplit(humid_data_transposed$`Maximum_Day:Hour`, split = ":")
max_day = sapply(string, `[`, 1)
max_hour = sapply(string, `[`, 2)

for (i in seq(from=1, to=row_num, by=1)){
  
  max_split = unlist(strsplit(humid_data_transposed[i, 2], split = ":"))
  
  humid_data_transposed[i, 9] <- max_split[1] # max day
  humid_data_transposed[i, 10] <- max_split[2] # max hour
  
  min_split = unlist(strsplit(humid_data_transposed[i, 4], split = ":"))
  
  humid_data_transposed[i, 11] <- min_split[1] # min day
  humid_data_transposed[i, 12] <- min_split[2] # min hour
  
}

humid_data_transposed <- humid_data_transposed[-c(2, 4)]
humid_data_transposed_month <- humid_data_transposed[6]

humid_data_transposed_numeric <- sapply( humid_data_transposed[c(1:5, 7:9)], as.numeric )

humid_data_transposed <- bind_cols(humid_data_transposed_month, humid_data_transposed_numeric)
