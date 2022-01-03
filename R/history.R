
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
  
  start_date <- convertDate(start_date)
  
  end_date <- convertDate(end_date)
  
  query <- paste0('v7/finance/download/',
                  security,
                  "?language=en",
                  "&region=",region,
                  "&period1=",start_date,
                  "&period2=",end_date,
                  "&interval=",interval,
                  "&events=",historyType
  )
  
  data <- httr::content(yfinQuery(query),show_col_types = FALSE)
  
  return(data)
}
