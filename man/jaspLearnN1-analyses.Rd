\name{jaspLearnN1-analyses}
\alias{Treatment}
\alias{Forecasting}
\alias{Network}
\title{Learn N=1 Analysis Entry Points}
\description{
Analysis functions called by the JASP interface. Treatment fits phase-specific
linear trends with AR(1) residual correlation. Forecasting fits ARIMA models.
Network summarizes user-rated perceived causal connections.
}
\usage{
Treatment(jaspResults, dataset = NULL, options)
Forecasting(jaspResults, dataset = NULL, options)
Network(jaspResults, dataset = NULL, options)
}
\arguments{
\item{jaspResults}{The result object supplied by JASP.}
\item{dataset}{An optional data frame. JASP reads assigned columns when needed.}
\item{options}{Analysis options supplied by the corresponding JASP form.}
}
\value{Results are added to \code{jaspResults}. These functions are intended to
be invoked through JASP rather than used as a standalone R interface.}
\details{See the analysis help within JASP for input requirements,
interpretation, and supported options.}
\keyword{internal}
