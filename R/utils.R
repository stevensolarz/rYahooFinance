#' Converts given year, month and day to UNIX time-code for API
#'
#' @param YYYY Year; must be 4 digits
#' @param MM Month
#' @param DD Day
#' @param tz Specify timezone based on R base time zones. Default: America/New_York
#'
#' @return UNIX code time-stamp
#' 
convertDate <- function(YYYY,MM,DD,tz="America/New_York") {
  
  if(nchar(YYYY) != 4) {
    stop("Year must be four digits.")
  }
  
  if(!is.numeric(YYYY)|!is.numeric(MM)|!is.numeric(DD)) {
    stop("Year, Month, and Day must all be numeric.")
  }
  
  return(
    as.numeric(as.POSIXct(paste0(YYYY,"-",MM,"-",DD), tz=tz))
  )
}




checkRegion <- function(region) {
  if (is.null(region) || !length(region) || !all(region %in% c('US','CA'))) {
    stop("'region' argument must be one of: 'US','CA'")
  }
}

checkInterval <- function(interval) {
  if (is.null(interval) || !length(interval) || !all(interval %in% c('1m','5m','15m','1d','1wk','1mo'))) {
    stop("'interval' argument must be one of: '1m','5m','15m','1d','1wk','1mo'")
  }
}

checkRange <- function(range) {
  if (is.null(range) || !length(range) || !all(range %in% c('1d','5d','1mo','3mo','6mo','1y','5y','10y','ytd','max'))) {
    stop("'range' argument must be one of: '1d','5d','1mo','3mo','6mo','1y','5y','10y','ytd','max'")
  }
}

checkHistoryType <- function(historyType) {
  
  if (is.null(historyType) || !length(historyType) || !all(historyType %in% c('price','dividend','split','capitalgain'))) {
    stop("'historyType' argument must be one of: 'price','dividend','split','capitalgain")
  }
}