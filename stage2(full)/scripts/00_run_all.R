#Master script used to perform the analyses reported in:
#Jia, Z., Ozaki, Y., Pavlovich, D. V., Huang, J., Benetos, E., Khasanah, U., Calhoun, S., Chiba, G., Kitayama, Y., Fujii, S., Sadaphal, D. P., Fitch, W. T., Vaida, S., Echim, S., Popescu, T., Shi, Z., Grassi, M., Guiotto Nai Fovino, L., Hajič jr., J., Nuska, P., …, Savage, P. E. (In Principle Accepted). Synchronised group singing enhances social bonding more than group conversation or recitation does: A Registered Report across 30 languages. Peer Community In Registered Reports. Preprint: https://doi.org/10.31234/osf.io/pv3m9_v6 [Peer Community In Registered Reports editorial recommendation and peer review: https://rr.peercommunityin.org/articles/rec?id=890]

#IMPORTANT: Set working directory to the root of the project (sync-coop-song-speech)
#Set output directories (relative to working directory)
stage2scripts <-'./stage2(full)/scripts/'
stage1raw <-'./stage1(pilot)/data/raw/'
stage1processed <-'./stage1(pilot)/data/processed/'
stage1figs<-'./stage1(pilot)/figures/'
stage2raw <-'./stage2(full)/data/raw/'
stage2processed <-'./stage2(full)/data/processed/'
stage2figs<-'./stage2(full)/outputs/figures/'

#Install and load packages
#IMPORTANT: rstan installation requires configuration of C++. Please also check:
#https://github.com/stan-dev/rstan/wiki/rstan-Getting-Started
if(length(setdiff('rstan', rownames(installed.packages()))) > 0) {
  install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
}

if (!require(remotes)) { install.packages('remotes') } 
remotes::install_github('jorvlan/raincloudplots') 

packages <- c('ggplot2', 'ggridges', 'ggpubr','gridExtra', 'cowplot', 'grid',
              'dplyr', 'plyr', 'tidyr', 'lavaan', 'rmarkdown', 
              'readr', 'caTools', 'bitops', 'xfun','psych','knitr','forcats',
              'GPArotation','sf','rnaturalearth', 'rnaturalearthdata',
              'here', 'rpart', 'posterior')

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

##Data pre-processing (removing/cleaning unpublishable data to make it ready for sharing
#source(file = file.path(stage2scripts,'01_preprocessing.R')) #scripts included for transparency but commented out since original files cannot be shared until this processing step is complete

##load public raw data directly from GitHub
df<-read_csv(file='https://raw.githubusercontent.com/comp-music-lab/sync-coop-song-speech/refs/heads/main/stage2(full)/data/raw/stage2data.csv') #read full raw data file of Stage 2 participant data directly from GitHub
df <- df[-1] #remove ID row
e<-read_csv(file='https://raw.githubusercontent.com/comp-music-lab/sync-coop-song-speech/refs/heads/main/stage2(full)/data/raw/experimentlog.csv') #read full raw data file of Stage 2 experimenter log data directly from GitHub
collabT<-read_csv(file='https://github.com/comp-music-lab/sync-coop-song-speech/raw/refs/heads/main/stage2(full)/data/raw/CollaboratorsPlotData.csv') #read full raw data file of Stage 2 experiment site info directly from GitHub

## Create map (Fig. 2)
fileid <- "stage2"
source(file = file.path(stage2scripts,'02_map.R'))

##Create Fig. 5 (raincloud visualisation of confirmatory analysis comparing pre-/post-experiment social bonding)
#Load raincloud plot scripts
source(file = file.path(stage2scripts,'03a_R_rainclouds.R'))
source(file = file.path(stage2scripts,'03b_summarySE.R'))
#Create raincloud plots
source(file = file.path(stage2scripts,'03c_raincloud.R'))

##Run confirmatory Bayesian analyses (Tables 1, S4, and S14)
#Run analyses for Research Question 1 ("Does singing enhance social bonding?")
source(file = file.path(stage2scripts,'04_confirmatory_analysis_01.R'))

#Run analyses for Research Question 2 ("Does singing enhance social bonding more than)
source(file = file.path(stage2scripts,'05_confirmatory_analysis_02.R'))

##Run other exploratory analyses (Figs. 6 on)
source(knitr::purl(file.path(stage2scripts, '06_exploratory_analyses.Rmd'),
                   output = tempfile(fileext = '.R'), quiet = TRUE))
