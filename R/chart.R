yfinChart <- function(ticker,
                      region="US",
                      range="1mo",
                      interval="1d",
                      events=NULL,
                      comparisons=NULL) {

  if (is.null(range) || !length(range) || !all(range %in% c('1d','5d','1mo','3mo','6mo','1y','5y','10y','ytd','max'))) {
    stop("'range' argument must be one of: '1d','5d','1mo','3mo','6mo','1y','5y','10y','ytd','max'")
  }

  if (is.null(interval) || !length(interval) || !all(interval %in% c('1m','5m','15m','1d','1wk','1mo'))) {
    stop("'interval' argument must be one of: '1m','5m','15m','1d','1wk','1mo'")
  }

  if (is.null(region) || !length(region) || !all(region %in% c('US','CA'))) {
    stop("'region' argument must be one of: 'US','CA'")
  }

  if (is.null(events)) {
    events = ""
  } else if (!length(events) || !all(events %in% c('dividends','splits','all'))) {
    stop("'events' argument must be NULL or one of: 'dividends','splits','all'")
  } else {
    events = paste0(
      "&events=",
      memisc::cases(
        "div"=events=="dividends",
        "split"=events=="splits",
        "div,split"=events=="all"
      )
    )
  }

  if (is.null(comparisons)) {
    comparisons = ""
  } else {
    comparisons = paste0(
      "&comparisons=",
      paste(comparisons,collapse=",")
    )
  }

  query <- paste0('v8/finance/chart/',
                  ticker,
                  "?region=",region,
                  "&range=",range,
                  "&interval=",interval,
                  events,
                  comparisons,
                  "&lang=en")


  data <- yfinQuery(query)

  return(data)

}
