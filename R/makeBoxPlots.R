#' A BoxPlot-Making Function
#'
#' This function will take a Seurat object and generates individual UMAP FeatureMap plots as a PNG files using markers listed in a CSV file
#' @param seuratObject The Seurat object you've been working with
#' @param csvFile The CSV files containing a list of markers and the cell type that each marker may indicate
#' @keywords Seurat FeaturePlot PNG UMAP
#' @import utils
#' @import Seurat
#' @import grDevices
#' @import dittoSeq
#' @export
#' @examples
#' makeBoxPlots(r1pos, "markers_and_CellTypes.csv")

makeBoxPlots <- function(seuratObject, csvFile){

  previousWorkingDir <- getwd()

  # Load the CSV table with all the markers you're interested in
  # The table will also include what the markers might indicate the cell type is
  df <- utils::read.csv(file = csvFile, header = TRUE)
  print("CSV file loaded.")

  featurePlotDir <- paste("BoxPlots_", substitute(seuratObject), sep = "")

  dir.create(path = featurePlotDir)
  setwd(dir = featurePlotDir)
  print(paste("Saving images to: ", getwd(), sep = ""))

  print(paste("This will generate ", nrow(df), " plots.", sep = ""))

  print("////////////////////////////////////")

  # This list will store all the plots that are made by the first for loop
  plot_list <- list()

  for (i in 1:nrow(df)) {
    marker <- df[i, "Markers"]
    plot <- dittoSeq::dittoPlot(seuratObject, marker, group.by = "ident",
                                plots = "boxplot") + theme(plot.title = element_text(hjust = 0.5))
    plot_list[[i]] <- plot
  }

  # Save plots to PNG -> Makes a separate file for each plot.
  # This for loop takes all the plots in the plot_list and saves them individually
  for (i in 1:nrow(df)) {
    marker <- df[i, "Markers"]
    cellType <- df[i, "Cell_Type"]
    file_name <- paste(marker, "_possCellType_", cellType, ".png", sep="")
    grDevices::png(file=file_name, width=600, height=400)
    print(plot_list[[i]])
    grDevices::dev.off()

    print(paste("Created: ", file_name, sep = ""))
  }

  print("////////////////////////////////////")

  # Reset the working directory back to what it was when we started
  setwd(previousWorkingDir)

  print("Done!")
}

