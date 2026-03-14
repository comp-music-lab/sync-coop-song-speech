
## Map figure
rm(list = ls())
collaboratorinfofile <- './data/CollaboratorsPlotDataStage2.csv'
OUTPUTDIR <- './output/figure/'

exclusion <- c()
fileid <- "full"
source("plot_CollaboratorMap.R")

fileid <- "final"
source("plot_CollaboratorMap.R")