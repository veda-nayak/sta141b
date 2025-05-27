# Written by Duncan Temple Lang

readHAR =
function(file, d = fromJSON(file))
{
    e = d$log$entries
    els = lapply(e, mkEntryDF)
    fix(do.call(rbind, els))
}

fix =
function(x)    
{
    df = as.data.frame(x)

    num = c("time", "blocked", "dns", "connect", "ssl", "send", "wait", 
            "receive", "bodySize", "headersSize", "responseStatus",
            "responseHttpVersion", "responseHeadersSize", "responseBodySize")

    char = c("startedDateTime", "_securityState", "serverIPAddress", "connection", 
             "pageref", "method", "url", "httpVersion", "responseStatusText", "responseRedirectURL", 
             "content", "mimeType", "responseEncoding")

#    df[num] = lapply(df[num], as.numeric)
    #    df[char] = lapply(df[char], as.character)
    df[c(num, char)] = lapply(df[c(num, char)], unlist)

#    vl = c(# "requestHeaders", "requestCookies", "queryString",
#        "postData"
    #       "responseHeaders", "responseCookies"
#           )
    #    df[vl] = lapply(df[vl], unlist, recursive = FALSE)
    df$postData = unlist(df$postData, recursive = FALSE)
    df
}


EntryFields = c(
                 "cache", "pageref", "request", "response", "startedDateTime", "time", "timings",
                # following are not in every request
                "_securityState", "serverIPAddress", "connection" 
              )

EntryFields = c(startedDateTime = "character", request = "list", response = "list", 
                cache = "list", timings = "numeric", time = "numeric", `_securityState` = "character", 
                serverIPAddress = "character", connection = "character", pageref = "character"
                )


asDF1 =
function(x)    
{
    #  structure(as.list(x), row.names = 1L, class = "data.frame")
  structure(x, row.names = 1L, class = "data.frame")    
}


mkEntryDF = 
function(x)
{
    scalar = c("startedDateTime", "_securityState", "serverIPAddress", "connection", "pageref", "time")
    tmp = lapply(x[scalar], orNA)
    names(tmp) = scalar

    #    ans = as.data.frame(tmp)
    ans = tmp

    # timings
    if(length(x$timings))
        ans = c(ans, as.list(x$timings)) # as.data.frame(as.list(x$timings))
    else
        ans = c(ans, list("blocked" = NA,  "dns" = NA, connect = NA, "ssl" = NA, "send" = NA, "wait" = NA, "receive" = NA))
    
    # request, response, cache
    # For now, ignore cache as I have that turned off.

    r1 = mkRequest(x$request)
    ans[names(r1)] = r1

    r2 = mkResponse(x$response)
    ans[names(r2)] = r2

    # cbind(asDF1(ans), r1, r2)

    ans
}


orNA =
function(x)
    if(length(x) == 0) NA else x



queryString = 
    # convert the list of name value vectors into a single vector with the values
    # as the elements and the field name as the names() vector.
function(x)
{

    # w/o this check for length(x), get warnings of the form
    # In structure(v[i + 1L], names = v[i]) :
    #  Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
    #  Consider 'structure(list(), *)' instead.
    if(length(x) == 0)
        return(character())
    
    tmp = matrix(unlist(x), , 2, byrow = TRUE)
    z = tmp[,2]
    names(z) = tmp[,1]
    z
}





# This version creates the entire list and then calls asDF1
# Check gives same result
mkRequest =
function(x)
{
    # scalars
    s = c("bodySize", "method", "url", "httpVersion", "headersSize")
    ans = lapply(x[s], orNA) 

    ans$requestHeaders = queryString(x$headers)
    ans$requestCookies = queryString(x$cookies)
    ans$queryString = queryString(x$queryString)
    ans$postData = list(x$postData) # had queryString() but the postData is a named list not an unnamed list of name-value pairs.

    asDF1(ans)
}



mkResponse = 
function(x)
{
    # scalars
    s = c("status", "statusText", "httpVersion", "redirectURL", "headersSize", "bodySize")
    ans = lapply(x[s], orNA)
    # added this when did not get the same answer as mkResponse0
    names(ans) = paste0("response", capitalize(s))
    

    ans$responseHeaders = queryString(x$headers)
    ans$responseCookies = queryString(x$cookies)
    ans$content = orNA(x$content$text)
    ans$mimeType = orNA(x$content$mimeType)
    ans$responseEncoding = orNA(x$content$encoding)    
    
    asDF1(ans)
}

capitalize =
function(x)
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))

# ----------------------------------mimeType.R----------------------------------

mt2 = mimeType2 =
  # Get the second part of the general/specific type
  function(type)
  {    
    gsub("^[^/]+/", "", sapply(strsplit(type, ";"), `[`, 1))
  }

# ----------------------------------recursiveFind.R-----------------------------

if(FALSE) {
  p = list(a = list(d = c(x = 1, y = 2)), b = list(z = list(t = 2, h = "abc"),
                                                   ff = list(e = 5, gh = 7)))
  find(p, "x")
  find(p, "h")
  find(p, "gh")        
}

find =
  function(x, el, cur = character(), idx = integer())
  {
    if(el %in% names(x)) {
      return(list(path = cur, index = idx))
    }
    
    
    if(!is.list(x))
      return(NULL)
    
    #    print(idx)
    #    print(names(x))
    
    # If we don't have names, the mapply() doesn't do anything as names(x) has length 0 and it makes seq(along = x)
    if(length(names(x)) == 0)
      names(x) = rep("", length(x))
    
    #    if(length(idx) ==3 && all(idx == c(1, 3, 39))) browser()
    #    if(length(names(x)) == 0) recover()
    ans =mapply(function(v, i) {
      #        browser()
      find(x[[i]], el, cur = c(cur, v), idx = c(idx, i))
    },
    names(x), seq(along = x),
    SIMPLIFY = FALSE)
    
    #    ans[!sapply(ans, is.null)]
    ans[sapply(ans, length) > 0 ]
  }


# ----------------------------------send.R--------------------------------------

# send or evaluate a request

send =
  function(req, cookie = character(), postData = character(), curl = getCurlHandle(),
           headers = req$headers, dropEncoding = TRUE,
           response = NULL, ...)
  {
    if("request" %in% names(req)) {
      response = req$response
      req = req$request
    }
    
    op = req$method
    
    url = req$url
    
    qry = req$queryString
    if(length(qry)) 
      url = sprintf("%s?%s", url, combine(qry, collapse = "&"))
    
    
    cookie = if(length(cookie))
      cookie
    else
      combine(req$cookies, collapse = "; ")
    
    url = gsub(" ", "%20", url)
    
    if(is.list(headers))
      headers = queryString(headers)
    
    if(dropEncoding)
      headers = dropEncoding(headers)
    
    ans = getURLContent(url = url, binary = TRUE, cookie = cookie, curl = curl, customrequest = op, httpheader = headers, header = TRUE, ...)
    
    cvtBody(ans, ans$header)
  }


# https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Content-Encoding
cvtBody =
  function(ans, header = ans$header)    
  {
    b = ans$body$body #XX
    enc = header["content-encoding"]
    b2 = switch(enc,
                br = brotli::brotli_decompress(b),
                gzip = Rcompression::gunzip(b),
                b
    )
    
    b2 = cvtContentType(b2, header["content-type"])
    
    attr(b2, "header") = ans$header
    b2
  }

cvtContentType =
  function(data, ct)    
  {
    els = strsplit(ct, ";")[[1]]
    if(els[1] %in% TextContentTypes)
      rawToChar(data)
    else
      data
  }

TextContentTypes =
  c("text/javascript", "application/json", "application/javascript", 
    "text/css", "text/plain", "text/html", 
    "image/svg+xml" 
  )

# "font/woff2", 
# "image/x-icon", "image/vnd.microsoft.icon"
# "font/ttf",
# "image/png", "image/gif", "image/jpeg", "binary/octet-stream", 
# "image/webp",


combine =
  function(x, sep = "=", collapse = "&")
  {
    x = queryString(x)
    paste(names(x), x, sep = sep, collapse = collapse)
  }

dropEncoding =
  function(x)    
  {
    i = match("Accept-Encoding", names(x))
    if(!is.na(i))
      x = x[ -i ]
    
    x
  }












