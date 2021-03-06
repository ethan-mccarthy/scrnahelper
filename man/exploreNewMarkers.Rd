% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploreNewMarkers.R
\name{exploreNewMarkers}
\alias{exploreNewMarkers}
\title{Generates a Feature Plot and a Violin Plot for a given marker}
\usage{
exploreNewMarkers(SeuratObject)
}
\arguments{
\item{seuratObject}{The Seurat object you've been working with}
}
\description{
This function will run a program in the console that allows you to enter a marker of interest. Then the program will generate a Feature and Violin plot of the marker from the Seurat object. This can help determine if a marker is expressed in any of the clusters.
}
\examples{
exploreNewMarkers(r1pos)
}
\keyword{Seurat}
\keyword{explore}
\keyword{markers}
