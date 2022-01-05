#' Title
#'
#' @param security 
#' @param region 
#' @param start_date 
#' @param end_date 
#'
#' @return
#' @export
#'
#' @examples
yfinDividends <- function(security,
           region = "US",
           start_date = NULL,
           end_date = NULL) {
  
    checkRegion(region)
    
    if (!is.null(start_date)) {
      if (is.null(end_date)) {
        end_date <- format(Sys.Date(),"%Y%m%d")
      }
      
      
      start_date <- convertDate(start_date)
      end_date <- convertDate(end_date)
      
      dividendHistory <- paste0(
        'v7/finance/download/',security,
        "?language=en",
        "&region=",region,
        "&period1=",start_date,
        "&period2=",end_date,
        "&events=dividend"
      )
      
      dividendHistory <-
        httr::content(yfinQuery(dividendHistory), show_col_types = FALSE)
      
      return(dividendHistory)
      
    } else {
      
      dividendSummary <- paste0(
        'v11/finance/quoteSummary/',security,
        "?lang=en",
        "&region=",region,
        "&modules=summaryDetail"
      )
      
      dividendSummary <- httr::content(yfinQuery(dividendSummary), show_col_types = FALSE)$quoteSummary$result[[1]]$summaryDetail
      
      
      
      return(list(
        "Dividend"=dividendSummary$dividendRate$fmt,
        "Yield"=dividendSummary$dividendYield$fmt,
        "Ex-Dividend Date"=dividendSummary$exDividendDate$fmt
      ))
      
    }
    
    
}
