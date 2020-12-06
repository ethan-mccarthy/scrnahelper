#' Generates a Feature Plot and a Violin Plot for a given marker
#'
#' This function will run a program in the console that allows you to enter a marker of interest. Then the program will generate a Feature and Violin plot of the marker from the Seurat object. This can help determine if a marker is expressed in any of the clusters.
#' @param seuratObject The Seurat object you've been working with
#' @keywords Seurat explore markers
#' @import utils
#' @import Seurat
#' @export
#' @examples
#' exploreNewMarkers(r1pos)

exploreNewMarkers <- function(SeuratObject) {
  booleanVal <- TRUE
  print("This program will make a Feature Plot and Violin Plot for a given marker.")
  print("Enter STOP to quit the program.")
  print("//////////////////////////////////////////")
  while(booleanVal == TRUE){
    markerOfInterest <- readline(prompt= "Enter marker of interest (case sensitive): ")

    if(markerOfInterest == "STOP"){
      booleanVal <- FALSE
    }

    plot1 <- FeaturePlot(SeuratObject, features = markerOfInterest)
    plot2 <- VlnPlot(SeuratObject, features = markerOfInterest)
    print(paste("Printing Feature and Violin Plots of ", markerOfInterest, sep=""))
    print(plot1 + plot2)
    print("//////////////////////////////////////////")
  }
}
