#' Title
#'
#' @param security 
#' @param region 
#' @param basePrice 
#' @param quantity 
#' @param tradeDate 
#' @param realtime 
#' @param futureDate 
#' @param summary 
#'
#' @return
#' @export
#'
#' @examples
yfinProfit <- function(security,region,basePrice,quantity=1,tradeDate,realtime=FALSE,futureDate=NULL,summary=FALSE) {
  
  if(is.null(futureDate)) { futureDate = format(Sys.Date(),"%Y%m%d") }
  
  priceData = yfinHistory(security,region,start_date=tradeDate,end_date=futureDate,interval="1d",historyType="price")
  
  ## FEATURE TO ADD:
  ## DRIP and DIVIDENDS
  ## dividends= and drip= to the function
  ## dividendData = yfinHistory(security,region,start_date=tradeDate,end_date=futureDate,interval="1d",historyType="dividend")
  
  
  priceData <- priceData %>%
    mutate(Base = basePrice) %>%
    mutate(`Base Cost` = Base * quantity) %>%
    mutate(`Market Value` = Close * quantity) %>%
    mutate(`Profit/(Loss) %` = (`Market Value` - `Base Cost`) / `Base Cost`)
  
  if(summary==TRUE) {
    
    
    
    return(list(
      "Base Cost" = priceData[nrow(priceData),]$`Base Cost`,
      "Market Value" = priceData[nrow(priceData),]$`Market Value`,
      "Return %" = priceData[nrow(priceData),]$`Profit/(Loss) %`
    ))
  } else {
    return(priceData)
  }
  
}