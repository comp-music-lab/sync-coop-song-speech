#Master script used to perform the analyses reported in:
#Jia, Z., Ozaki, Y., Pavlovich, D. V., Huang, J., Benetos, E., Khasanah, U., Calhoun, S., Chiba, G., Kitayama, Y., Fujii, S., Sadaphal, D. P., Fitch, W. T., Vaida, S., Echim, S., Popescu, T., Shi, Z., Grassi, M., Guiotto Nai Fovino, L., Hajič jr., J., Nuska, P., …, Savage, P. E. (In Principle Accepted). Synchronised group singing enhances social bonding more than group conversation or recitation does: A Registered Report across 30 languages. Peer Community In Registered Reports. Preprint: https://doi.org/10.31234/osf.io/pv3m9_v6 [Peer Community In Registered Reports editorial recommendation and peer review: https://rr.peercommunityin.org/articles/rec?id=890]

#Set working directory
setwd('/Users/psav050/Documents/GitHub/sync-coop-song-speech')#NB: You need to set this to your own local working directory to reproduce the analysis

#Set output directories (relative to working directory)
stage1raw <-'./stage1(pilot)/data/raw/'
stage1processed <-'./stage1(pilot)/data/processed/'
stage1figs<-'./stage1(pilot)/figures/'
stage2raw <-'./stage2(full)/data/raw/'
stage2processed <-'./stage2(full)/data/processed/'
stage2figs<-'./stage2(full)/figures/'

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

##Data pre-processing (removing/cleaning unpublishable data to make it ready for sharing - scripts included for transparency but commented out since original files cannot be shared until this processing step is complete)
source("01_preprocessing.R")
##load public raw data directly from GitHub
df<-read_csv(file='https://raw.githubusercontent.com/comp-music-lab/sync-coop-song-speech/refs/heads/main/stage2(full)/data/raw/stage2data.csv') #read full raw data file of Stage 2 participant data directly from GitHub
df <- df[-1] #remove ID row
e<-read_csv(file='https://raw.githubusercontent.com/comp-music-lab/sync-coop-song-speech/refs/heads/main/stage2(full)/data/raw/experimentlog.csv') #read full raw data file of Stage 2 experimenter log data directly from GitHub

## Create map (Fig. 2)
rm(list = ls())
collaboratorinfofile <- './data/CollaboratorsPlotData.csv'
OUTPUTDIR <- './output/figure/'

exclusion <- c()
fileid <- "full"
source("plot_CollaboratorMap.R")

fileid <- "final"
source("plot_CollaboratorMap.R")

##Create Fig. 5 (raincloud plot pre-/post-experiment social bonding)
#Load raincloud plot scripts
source('R_rainclouds.R')
source('summarySE.R')
source('simulateData.R')
#Run raincloud analysis
source ("raincloud.R")