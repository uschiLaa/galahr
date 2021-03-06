% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{ggtimeline}
\alias{ggtimeline}
\title{Generating the timeline display.}
\usage{
ggtimeline(anchors, current, maxT, breaks, indexVals = NULL)
}
\arguments{
\item{anchors}{Anchor plane indexes in the timeline}

\item{current}{Current projection index in the timeline.}

\item{maxT}{Final projection index limiting the timeline axis.}

\item{breaks}{Breaks for labelling timeline axis.}

\item{indexVals}{Projection pursuit index value as function of time.
(default is NULL)}
}
\value{
Visualisation of timeline
}
\description{
Generating the timeline display.
}
