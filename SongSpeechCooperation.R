#This code was used in November 2024 to perform the analyses reported in:
#Savage, P. E., et al. "Does singing enhance cooperation more than speaking does? A global experimental Stage 1 Registered Report

#It was partially adapted from code from the following publication:
#Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., Van Langen, J., & Kievit, R. A. (2021). Raincloud plots: A multi-platform tool for robust data visualization. Wellcome Open Research, 4, 63. https://doi.org/10.12688/wellcomeopenres.15191.2


#Set working directory
setwd('/Users/psav050/Documents/GitHub/sync-coop-song-speech')

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

#Load and pre-process pilot data
df<-read_csv(file='https://raw.githubusercontent.com/comp-music-lab/sync-coop-song-speech/refs/heads/main/song_August%2021%2C%202024_16.12.csv') #read raw Qualtrics export file directly from GitHub
colnames(df)<-df[1,] #change column names to make clear what they are
names(df)[names(df) == 'What is the "Group ID" listed on the screen?'] <- 'group' #Rename with shorter variable name
df<-df[-c(1:2),] #remove non-data rows
df<- subset(df, Finished=="TRUE") #Remove participants who did not finish - may need to decapitalise to "True" for newer data exports
df<- subset(df, `Response Type`=="IP Address") #Remove test ("Preview") responses
df<-df[-c(1:41),] #Remove preliminary pilot experiments before July 25 2024
df<-df[-c(6),] #Exclude duplicated participant data 
df<-df[1:14,] #Keep only NZ English pilot data (should eventually update this to a more automated exclusion algorithm based on start/stop date/time, completion rate, etc.)
df<-df[,c(17,19,29:31,48,54,56,58,62,66,68,70,73)] #exclude exploratory variables, keeping only social bonding variables for confirmatory analysis
write.csv(df,'keydata.csv')
rep_data<-read_csv(file='keydata.csv')

#Rename variables:
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - "I feel like I am on the same team with the other participants"...4'] <- 'team'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - "I feel like I am on the same team with the other participants"...9'] <- 'team'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - "I feel like I am on the same team with the other participants"...13'] <- 'team'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - “I think I am similar to the other participants’’...5'] <- 'similar'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - “I think I am similar to the other participants’’...10'] <- 'similar'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - “I think I am similar to the other participants’’...14'] <- 'similar'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - ‘‘I trust the other participants”...6'] <- 'trust'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - ‘‘I trust the other participants”...8'] <- 'trust'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - ‘‘I trust the other participants”...12'] <- 'trust'
names(rep_data)[names(rep_data) == 'How close do you feel to all the other participants? - 1...7'] <- 'close'
names(rep_data)[names(rep_data) == 'How close do you feel to all the other participants? - 1...11'] <- 'close'
names(rep_data)[names(rep_data) == 'How close do you feel to all the other participants? - 1...15'] <- 'close'

#combine same variables to measure consistency
bind<-rbind(rep_data[,4:7],rep_data[,8:11],rep_data[,12:15])

#Internal consistency analysis of individual cooperation variables (Cronbach's alpha)
psych::alpha(bind)
psych::omega(bind)

#Average individual scores into an overall cooperation score
rep_data$Pre_Experiment<-rowMeans(rep_data[,4:7]) #pre-experiment baseline average cooperation
rep_data$Post_Experiment<-rowMeans(rep_data[,8:11]) #cooperation after 1st experiment condition
rep_data$t3<-rowMeans(rep_data[,12:15]) #average cooperation at end of exploratory conditions

data_long <- gather(rep_data, time, score, Pre_Experiment:Post_Experiment, factor_key=TRUE)
colnames(data_long)[1] <- 'Participant'
write.csv(data_long,'keydata_long.csv')

rep_data<-read_csv(file='keydata_long.csv',
                   col_types = cols(group = col_factor(levels = c('S', 'C', 'R')), 
                                    time = col_factor(levels = c('Pre_Experiment', 'Post_Experiment'))))
rep_data<-rep_data[,-1]


#Plot all  groups
sumrepdat <- summarySE(rep_data, measurevar = 'score', groupvars=c('group', 'time'))

head(sumrepdat)

p11 <- ggplot(rep_data, aes(x = time, y = score, fill = group)) +
  geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(time)-.15, y = score, colour = group),position = position_jitter(width = .05), size = 1.5, shape = 20)+
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
