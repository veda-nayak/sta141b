library(RCurl)
library(RJSONIO)
library(XML)
library(RSelenium) #web browser vibes in R
library(jsonlite)

# Acknowledgments
# https://cran.r-project.org/web/packages/jsonlite/jsonlite.pdf
# https://stackoverflow.com/questions/3703276/how-to-tell-if-a-file-is-gzip-compressed

dir = "C:/cygwin64/home/vedan/Code/sta141b"
setwd(dir)

source(paste(dir, "har.R", sep = '/'))

u = "view-source:https://redriver.wd5.myworkdayjobs.com/en-US/redrivercareers/job/OFallon-Illinois/Data-Engineer_REQ-3128"

dr = remoteDriver$new()
dr$open

dr$navigate(u)

file = 'data_scientist_page_1.har' # CURRENTLY ONLY HAS PAGE 1
m = readHAR(file)
table(m$mimeType)

names(m)

mj = subset(m, mt2(m$mimeType) == "json")
mj$url

i = grep("search", mj$url) # give me the headers I expect
sapply(mj$url[i], nchar) # all have the sample length, 51

j = fromJSON(mj$content[i[8]])

names(j)
jsapply(j, class)
sapply(j, length)  # hits has the most information
sapply(j$data, length)
names(j$data$rows[[1]])

r = j$hits
rtc = table(sapply(r, class))
rtl = table(sapply(r, length))

# others with /search

hmj = names(mj$requestHeaders[[ i[1] ]])


u = "https://jobs-in-data.com/msearch/indexes/job/search" 
ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/136.0.0.0 Safari/537.36 Edg/136.0.0.0 OS/10.0.26100"
k = "%7B%22categories%22%3A%5B%22necessary%22%5D%2C%22revision%22%3A0%2C%22data%22%3Anull%2C%22consentTimestamp%22%3A%222025-05-26T22%3A20%3A48.807Z%22%2C%22consentId%22%3A%2210efdd0a-5d4b-486c-b6f8-a2b986df26f4%22%2C%22services%22%3A%7B%22necessary%22%3A%5B%5D%2C%22analytics%22%3A%5B%5D%2C%22ads%22%3A%5B%5D%7D%2C%22lastConsentTimestamp%22%3A%222025-05-26T22%3A20%3A48.807Z%22%2C%22expirationTime%22%3A1764022848808%7D"

# Request Payload
# {"q":"Data Scientist",
#  "sort":["date:desc"],
#   "limit":100,
#   "offset":0,
#   "filter":["country = 'United States'"]}

# Handmade body
# {"q":"Data Scientist",
#   "sort":["date:desc"],
#   "limit":100,
#   "offset":0,
#   "filter":["country = 'United States'"]} 


bodyData = list(
  q = "Data Scientist",
  sort = list("date:desc"),
  limit = 100,
  offset = 0,
  filter = list("country = 'United States'")
)

body = toJSON(bodyData, auto_unbox = TRUE)

headers = c(
  'content-type' = 'application/json',
  'content-length' = nchar(body),
  'user-agent' = ua,
  'Cookie' = k,
  'accept' = '*/*',
  'accept-language' = 'en-US,en;q=0.9',
  'accept-encoding' = 'gzip, deflate, br, zstd',
  'authorization' = 'Bearer f8eea7876c3f3ba38b2980bd52aa864fa0eba1e9365bd9f67c8b11f35e9b1366',
  'origin' = 'https://jobs-in-data.com'
)


# headers = c(
#   'content-type' = 'application/json',
#   'content-length' = nchar(body),
#   'user-agent' = ua,
#   'Cookie' = k,
#   'accept' = '*/*',
#   'accept-language' = 'en-US,en;q=0.9',
#   'accept-encoding' = 'gzip, deflate, br, zstd',
#   'authorization' = 'Bearer f8eea7876c3f3ba38b2980bd52aa864fa0eba1e9365bd9f67c8b11f35e9b1366',
#   'origin' = 'https://jobs-in-data.com',
#   'referer' = 'https://jobs-in-data.com/',
#   'sec-fetch-dest' = 'empty',
#   'sec-fetch-mode' = 'cors',
#   'sec-fetch-site' = 'cross-site',
#   'x-meilisearch-client' = 'Meilisearch JavaScript (v0.38.0)',
#   'sec-ch-ua-platform' = 'Windows'
# )


ans = httpPOST(u, httpheader = headers,
               postFields = body,
               verbose = TRUE)


head(ans) # 1f 8b 08 00 00 00 --> 1f and 8b mean it is gzipped --> use parse_gzjson_raw() based on 
          # https://stackoverflow.com/questions/3703276/how-to-tell-if-a-file-is-gzip-compressed
class(ans) # raw


ans_unzipped = parse_gzjson_raw(ans)


res = toJSON(ans_unzipped)

result = fromJSON(res)
