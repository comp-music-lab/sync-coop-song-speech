#This code was used in June 2024 to perform the analyses reported in:
#Savage, P. E., et al. "Does singing enhance cooperation more than speaking does? A global experimental Stage 1 Registered Report

#It was partially adapted from code from the following publication:
#Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., Van Langen, J., & Kievit, R. A. (2021). Raincloud plots: A multi-platform tool for robust data visualization. Wellcome Open Research, 4, 63. https://doi.org/10.12688/wellcomeopenres.15191.2


#Set working directory
setwd('/Users/danyapavlovich/Documents/GitHub/sync-coop-song-speech')

#Install and load packages
if (!require(remotes)) { install.packages('remotes') } 
remotes::install_github('jorvlan/raincloudplots') 

packages <- c('ggplot2', 'dplyr', 'lavaan', 'plyr', 'cowplot', 'rmarkdown', 
              'readr', 'caTools', 'bitops', 'xfun','psych','knitr','forcats','GPArotation','tidyr')

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(cowplot)
library(dplyr)
library(readr)
library(forcats)
library(psych)
library(tidyr)

knitr::opts_chunk$set(fig.width=6, fig.height=3, fig.path='figs/',
                      echo=FALSE, warning=FALSE, message=FALSE)

source('R_rainclouds.R')
source('summarySE.R')
source('simulateData.R')

# width and height variables for saved plots
w = 6
h = 3
# Make the figure folder if it doesn't exist yet
dir.create('../figs/tutorial_R/', showWarnings = FALSE)

head(summary_simdat)

#Specify data download location
#file.data <- 'https://raw.githubusercontent.com/compmusiclab/rhythm-coop/master/RhythmPilotData(Fig3).csv'

#Load and pre-process pilot data
df<-read_csv(file='QualtricsPilotData_Merged(2024-05-20).csv')
df<-df[-c(1:2),c(17,19,40:46,52,54:56,62:65,71:74,80:83,89)]
write.csv(df,'keydata.csv')
rep_data<-read_csv(file='keydata.csv')

#Internal consistency analysis of individual cooperation variables (Cronbach's alpha)
psych::alpha(rep_data[,4:7])
psych::omega(rep_data[,4:7])


rep_data$t1<-rowMeans(rep_data[,4:7])
rep_data$t2<-rowMeans(rep_data[,8:11])
rep_data$t3<-rowMeans(rep_data[,12:15])
rep_data$t4<-rowMeans(rep_data[,16:19])
rep_data$t5<-rowMeans(rep_data[,20:23])
rep_data$t6<-rowMeans(rep_data[,24:27])

#Keep only in-person singing/conversation + online recitation data 
rep_data<-rep_data[-c(21:35),]

data_long <- gather(rep_data, time, score, t1:t2, factor_key=TRUE)
colnames(data_long)[1] <- 'Participant'
write.csv(data_long,'keydata_long.csv')

rep_data<-read_csv(file='keydata_long.csv',
                   col_types = cols(group = col_factor(levels = c('GS', 'GC', 'GR')), 
                                    time = col_factor(levels = c('t1', 't2'))))
rep_data<-rep_data[,-1]


#Plot all  groups
sumrepdat <- summarySE(rep_data, measurevar = 'score', groupvars=c('group', 'time'))

head(sumrepdat)

p11 <- ggplot(rep_data, aes(x = time, y = score, fill = group)) +
  geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(time)-.15, y = score, colour = group),position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = time, y = score, fill = group),outlier.shape = NA, alpha = .5, width = .1, colour = 'black')+
  geom_line(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), linetype = 3)+
  geom_point(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), shape = 18) +
  geom_errorbar(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group, ymin = score_mean - sem, ymax = score_mean + sem), width = .05)+
  scale_colour_brewer(palette = 'Dark2')+
  scale_fill_brewer(palette = 'Dark2')+
  ylim(0,100)+ 
  ggtitle('pre-/post-intervention bonding')

ggsave('3Conditions.png', width = w, height = h)

p11
