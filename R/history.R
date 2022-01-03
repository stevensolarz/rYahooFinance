
#' Gather the historical data for a given ticker
#'
#' @param security The ticker for the security
#' @param region Default: US; selects the stock exchanges to get security from.
#' @param start_date Must be numeric in this format: YYYYMMDD
#' @param end_date  Must be numeric in this format: YYYYMMDD
#' @param interval 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
yfinHistory <- function(security,region="US",start_date,end_date,interval="1d",historyType="price") {
  
  checkRegion(region)
  
  checkInterval(interval)
  
  checkHistoryType(historyType)
  
  start_date <- convertDate(as.numeric(substr(start_date,1,4)),
                                as.numeric(substr(start_date,5,6)),
                                as.numeric(substr(start_date,7,8)))
  
  end_date <- convertDate(as.numeric(substr(end_date,1,4)),
                              as.numeric(substr(end_date,5,6)),
                              as.numeric(substr(end_date,7,8)))
  
  query <- paste0('v7/finance/download/',
                  security,
                  "?language=en",
                  "&region=",region,
                  "&period1=",start_date,
                  "&period2=",end_date,
                  "&interval=",interval,
                  "&events=",historyType
  )
  
  data <- httr::content(yfinQuery(query))
  
  return(data)
}
