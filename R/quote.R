#' Pull the most recent quote for a security/ies and specify the fields to pull
#'
#' @param securities String or vector of strings, maximum 10 securities. Must be all from the same region.
#' @param region Default: US; selects the stock exchanges to get security from.
#' @param fields Default c("currency", "symbol", "longName", "regularMarketPrice", "bid", "ask", "marketState")
#'

#' @return Data frame of stock market quote data.
#'
yfinQuotes <- function(securities,
                      regions="US",
                      fields=c("currency", "symbol", "longName", "regularMarketPrice", "bid", "ask", "marketState")) {

  # if regions isn't NULL, validate regions, otherwise default to US
  if(!is.null(regions)) {
    
    # if only one region is provided, make that the region for every security.
    if(length(regions)==1) {
      regions <- rep(regions[1],length(securities))
    }
    
    # 
    if(length(regions)<length(securities)) {
      missing <- length(securities) - length(regions)
      stop(paste0("Please provide a region for each security, provide one region, or leave regions blank. You are missing ",missing," region codes."))
    }
    
    sapply(regions,function(x) {checkRegion(x)})
    
  }
  
  # 
  for(i in 1:length(securities)) {
    if(regions[i] == 'CA' & substr(securities[i],nchar(securities[i])-2,nchar(securities[i])) != '.TO') {
      securities[i] <- paste0(securities[i],".TO")
    }
  }
  
  query <- paste0('v6/finance/quote?',
                  "symbols=",paste(securities,collapse=","),
                  "&language=en")

  data <- yfinQuery(query)

  if(data$status_code == 200) {
    data <- plyr::rbind.fill(lapply(httr::content(data)$quoteResponse$result,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)})) %>%
      dplyr::select(dplyr::all_of(fields))
  }

  if(nrow(data) < length(securities)) {
    warning("Not all tickers were found for the specified regions. If this was not what you expected, please check your tickers and regions and try again.")
  }
  return(data)

}
