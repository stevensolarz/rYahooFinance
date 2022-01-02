\name{yfinQuery}
\alias{yfinQuery}
\title{yfinQuery}
\usage{
yfinQuery(path)
}
\description{
Run a query against the API. This function is used as the base query for all other functions in this package, or you can input your own custom path
}
\examples{
yfinQuery('/v8/finance/chart/AAPL')
}
