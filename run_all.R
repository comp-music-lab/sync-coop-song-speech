#Master script used to perform the analyses reported in:
#Jia, Z., Ozaki, Y., Pavlovich, D. V., Huang, J., Benetos, E., Khasanah, U., Calhoun, S., Chiba, G., Kitayama, Y., Fujii, S., Sadaphal, D. P., Fitch, W. T., Vaida, S., Echim, S., Popescu, T., Shi, Z., Grassi, M., Guiotto Nai Fovino, L., Hajič jr., J., Nuska, P., …, Savage, P. E. (In Principle Accepted). Synchronised group singing enhances social bonding more than group conversation or recitation does: A Registered Report across 30 languages. Peer Community In Registered Reports. Preprint: https://doi.org/10.31234/osf.io/pv3m9_v6 [Peer Community In Registered Reports editorial recommendation and peer review: https://rr.peercommunityin.org/articles/rec?id=890]

#Set working directory
setwd('/Users/psav050/Documents/GitHub/sync-coop-song-speech')#NB: You need to set this to your own local working directory to reproduce the analysis

#Install and load packages
if (!require(remotes)) { install.packages('remotes') } 
remotes::install_github('jorvlan/raincloudplots') 

packages <- c('ggplot2', 'dplyr', 'lavaan', 'plyr', 'cowplot', 'rmarkdown', 
              'readr', 'caTools', 'bitops', 'xfun','psych','knitr','forcats','GPArotation','tidyr','sf','rnaturalearth','rnaturalearthdata','gridExtra','grid')

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(cowplot)
library(dplyr)
library(readr)
library(forcats)
library(psych)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gridExtra)
library(grid)

knitr::opts_chunk$set(fig.width=6, fig.height=3, fig.path='figs/',
                      echo=FALSE, warning=FALSE, message=FALSE)

#Load raincloud plot scripts
source('R_rainclouds.R')
source('summarySE.R')
source('simulateData.R')

##Data preprocessing (removing/cleaning unpublishable data to make it ready for sharing - scripts included for transparency but commented out since original files cannot be shared until this processing step is complete)
OUTPUTDIR <- './data/raw/'
source("mv2preprocessing.R")

##Create Fig. 5 (raincloud plot pre-/post-experiment social bonding)
source ("raincloud.R")

## Map figure
rm(list = ls())
collaboratorinfofile <- './data/CollaboratorsPlotData.csv'
OUTPUTDIR <- './output/figure/'

exclusion <- c()
fileid <- "full"
source("plot_CollaboratorMap.R")

fileid <- "final"
source("plot_CollaboratorMap.R")