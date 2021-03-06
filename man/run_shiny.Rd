% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_shiny.R
\name{run_shiny}
\alias{build_shiny}
\alias{run_shiny}
\title{run a quick shiny app to explore changing a function's parameters}
\usage{
run_shiny(call, slider_params, header = "", ...)

build_shiny(call, slider_params, header = "", ...)
}
\arguments{
\item{call}{A character string, or a list of strings, each that evaluates to a call to a plotting function}

\item{slider_params}{A list of vectors providing the ranges for each parameter to make a slider for}

\item{header}{A character string for the app's title}
}
\value{
None.

List with elements ui and server, to be passed to shinyApp()
}
\description{
run a quick shiny app to explore changing a function's parameters
}
\section{Functions}{
\itemize{
\item \code{build_shiny}: Builds a shiny app that can be run with shinyApp()
}}
\examples{
slider_params = list(A = c(1,0,2.5),B = c('a','b','c'))
call = function(A,B) {
  plot(NA,NA,xlim = c(-1,1),ylim = c(-1,1),main = B)
  symbol(0,0,circle = A,add=T,inches=F)
}
run_shiny(call,slider_params,header = 'test')
}

