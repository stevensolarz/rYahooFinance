#' Pull the most recent quote for a security/ies and specify the fields to pull
#'
#' @param securities String or vector of strings, maximum 10 securities. Must be all from the same region.
#' @param region Default: US; selects the stock exchanges to get security from.
#' @param fields Default c("currency", "symbol", "longName", "regularMarketPrice", "bid", "ask", "marketState")
#'

#' @return Data frame of stock market quote data.
#'
yfinQuotes <- function(securities,
                      region="US",
                      fields=c("currency", "symbol", "longName", "regularMarketPrice", "bid", "ask", "marketState")) {

  checkRegion(region)

  query <- paste0('v6/finance/quote?',
                  "symbols=",paste(securities,collapse=","),
                  "&language=en",
                  "&region=",region)

  data <- yfinQuery(query)

  if(data$status_code == 200) {
    data <- plyr::rbind.fill(lapply(httr::content(data)$quoteResponse$result,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)})) %>%
      dplyr::select(dplyr::all_of(fields))
  }

  return(data)

}
