#' Pull the most recent quote for a security/ies and specify the fields to pull
#'
#' @param symbols String or vector of strings, maximum 10 securities. Must be all from the same region.
#' @param region Default: US; selects the stock exchanges to get security from.
#' @param fields Default c("currency", "symbol", "longName", "regularMarketPrice", "bid", "ask", "marketState"); see the Yahoo Finance API manual for more details about which fields are supported.
#'

#' @return Data frame of stock market quote data.
#'
yfinQuote <- function(symbols,
                      region="US",
                      fields=c("currency", "symbol", "longName", "regularMarketPrice", "bid", "ask", "marketState")) {

  if (is.null(region) || !length(region) || !all(region %in% c('US','CA'))) {
    stop("'region' argument must be one of: 'US','CA'")
  }

  query <- paste0('v6/finance/quote?',
                  "symbols=",paste(symbols,collapse=","),
                  "&language=en",
                  "&region=",region)

  data <- yfinQuery(query)

  if(data$status_code == 200) {
    data <- plyr::rbind.fill(lapply(httr::content(data)$quoteResponse$result,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)})) %>%
      dplyr::select(dplyr::all_of(fields))
  }

  return(data)

}
