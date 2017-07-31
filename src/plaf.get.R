

plafURL <- function(urls, origin) {

  library(stringr)
  urls.position = as.numeric(str_sub(origin,3,3))
  url_content = urls[urls.position]
}

plafURL(urls)

fetchplaf <- function(url_content) {
  url_content = plafURL(urls)
  library(RCurl)
  myfile <- RCurl::getURL(url_content)
  plaf <- read.table(textConnection(myfile), header=T)
  return(plaf)
}

fetchplaf(url_content)

