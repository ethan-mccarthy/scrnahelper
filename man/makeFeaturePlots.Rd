% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeFeaturePlots.R
\name{makeFeaturePlots}
\alias{makeFeaturePlots}
\title{A FeaturePlot-Making Function}
\usage{
makeFeaturePlots(seuratObject, csvFile)
}
\arguments{
\item{seuratObject}{The Seurat object you've been working with}

\item{csvFile}{The CSV files containing a list of markers and the cell type that each marker may indicate}
}
\description{
This function will take a Seurat object and generates individual UMAP FeatureMap plots as a PNG files using markers listed in a CSV file
}
\examples{
makeFeaturePlots(r1pos, "markers_and_CellTypes.csv")
}
\keyword{FeaturePlot}
\keyword{PNG}
\keyword{Seurat}
\keyword{UMAP}
