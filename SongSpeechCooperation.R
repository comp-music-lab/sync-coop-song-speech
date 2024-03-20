#This code was used on 2024-03-19 to perform the analyses reported in:
#Savage, P. E., et al. "A global test of synchrony and cooperation in song, speech, and chant" (in prep.)

#It was adapted from code from the following publication:
#Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., Van Langen, J., & Kievit, R. A. (2021). Raincloud plots: A multi-platform tool for robust data visualization. Wellcome Open Research, 4, 63. https://doi.org/10.12688/wellcomeopenres.15191.2


#Set working directory
setwd("~/Documents/GitHub/sync-coop-song-speech")

knitr::opts_chunk$set(fig.width=6, fig.height=3, fig.path='figs/',
                      echo=FALSE, warning=FALSE, message=FALSE)

#Install and load packages
if (!require(remotes)) { install.packages("remotes") } 
remotes::install_github('jorvlan/raincloudplots') 

packages <- c("ggplot2", "dplyr", "lavaan", "plyr", "cowplot", "rmarkdown", 
              "readr", "caTools", "bitops", "xfun","psych")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(cowplot)
library(dplyr)
library(readr)
library(forcats)
library(psych)

source("R_rainclouds.R")
source("summarySE.R")
source("simulateData.R")

# width and height variables for saved plots
w = 6
h = 3
# Make the figure folder if it doesn't exist yet
dir.create('../figs/tutorial_R/', showWarnings = FALSE)

head(summary_simdat)

#Specify data download location
#file.data <- "https://raw.githubusercontent.com/compmusiclab/rhythm-coop/master/RhythmPilotData(Fig3).csv"

#Load and pre-process pilot data
rep_data<-read_csv(file="RhythmPilotData(2024-03-20).csv",
                   col_types = cols(group = col_factor(levels = c("synchronous song", "synchronous speech", "asynchronous song", "asynchronous speech")), 
                                    time = col_factor(levels = c("pre-intervention", "post-intervention"))))
rep_data$score<-rowMeans(rep_data[,4:7])

#Internal consistency analysis of individual cooperation variables (Cronbach's alpha)
psych::alpha(rep_data[,4:7])

#Plot all 4 groups
sumrepdat <- summarySE(rep_data, measurevar = "score", groupvars=c("group", "time"))

head(sumrepdat)

p11 <- ggplot(rep_data, aes(x = time, y = score, fill = group)) +
  geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(time)-.15, y = score, colour = group),position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = time, y = score, fill = group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), linetype = 3)+
  geom_point(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), shape = 18) +
  geom_errorbar(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group, ymin = score_mean - sem, ymax = score_mean + sem), width = .05)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ylim(1,7)+ 
  ggtitle("Figure 11: Repeated Measures - Factorial (Extended)")

ggsave('4Conditions.png', width = w, height = h)

p11

#Collapse to compare song vs speech:
rep_data<-read_csv(file="RhythmPilotData(2024-03-20).csv",
                   col_types = cols(group = col_factor(levels = c("synchronous song", "synchronous speech", "asynchronous song", "asynchronous speech")), 
                                    time = col_factor(levels = c("pre-intervention", "post-intervention"))))
rep_data$score<-rowMeans(rep_data[,4:7])
rep_data$group<-ifelse(rep_data$group=="synchronous song"|rep_data$group=="asynchronous song","song","speech")

sumrepdat <- summarySE(rep_data, measurevar = "score", groupvars=c("group", "time"))
head(sumrepdat)

p11 <- ggplot(rep_data, aes(x = time, y = score, fill = group)) +
  geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(time)-.15, y = score, colour = group),position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = time, y = score, fill = group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), linetype = 3)+
  geom_point(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), shape = 18) +
  geom_errorbar(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group, ymin = score_mean - sem, ymax = score_mean + sem), width = .05)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ylim(1,7)+ 
  ggtitle("Figure 11: Repeated Measures - Factorial (Extended)")

ggsave('SongVsSpeech.png', width = w, height = h)

p11


#Collapse to compare synchrony vs asynchrony:
rep_data<-read_csv(file="RhythmPilotData(2024-03-20).csv",
                   col_types = cols(group = col_factor(levels = c("synchronous song", "synchronous speech", "asynchronous song", "asynchronous speech")), 
                                    time = col_factor(levels = c("pre-intervention", "post-intervention"))))
rep_data$score<-rowMeans(rep_data[,4:7])
rep_data$group<-ifelse(rep_data$group=="synchronous song"|rep_data$group=="synchronous speech","synchronous","asynchronous")

sumrepdat <- summarySE(rep_data, measurevar = "score", groupvars=c("group", "time"))
head(sumrepdat)

p11 <- ggplot(rep_data, aes(x = time, y = score, fill = group)) +
  geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(time)-.15, y = score, colour = group),position = position_jitter(width = .05), size = .25, shape = 20)+
  geom_boxplot(aes(x = time, y = score, fill = group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), linetype = 3)+
  geom_point(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group), shape = 18) +
  geom_errorbar(data = sumrepdat, aes(x = as.numeric(time)+.1, y = score_mean, group = group, colour = group, ymin = score_mean - sem, ymax = score_mean + sem), width = .05)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ylim(1,7)+ 
  ggtitle("Figure 11: Repeated Measures - Factorial (Extended)")

ggsave('SynchronousVsAsynchronous.png', width = w, height = h)

p11





###
################Potential code for GLMM suggested by Martin Lang (for somewhat different design/code - see https://github.com/compmusiclab/rhythm-coop for details):

NROW(data)/4 # number of sessions
data$sess <- c(rep(1:8,each=4)) # label each session

hist(data$behavior) # histogram suggests data bounded on the upper boundary

## My preferred approach - hierarchical beta regression
library(glmmTMB)

data$behav.p <- data$behavior/1000 # main DV as percentage
data$behav.p <- (data$behav.p*31 + 0.5)/32 #adapt for beta regression (can't contain 0 and 1)

summary(m1 <- glmmTMB(behav.p ~  (1|sess), family = "beta_family", data = data))
summary(m2 <- glmmTMB(behav.p ~ group + (1|sess), family = "beta_family", data = data))
summary(m3 <- glmmTMB(behav.p ~ group + sex + age + musicianship + (1|sess), family = "beta_family", data = data))

library(DHARMa)

simulationOutput = simulateResiduals(m3) # fit diagnostics
plot(simulationOutput)

library(effects)

summary(allEffects(m3)) # predicted values

## simpler LMM
library(lme4)
summary(m1 <- lmer(behavior ~ (1|sess),  data = data))
summary(m2 <- lmer(behavior ~ group + (1|sess), data = data))
summary(m3 <- lmer(behavior ~ group + sex + age + musicianship + (1|sess), data = data))

summary(allEffects(m3)) # note upper 95% CI for group effect predicts values above 1000
simulationOutput = simulateResiduals(m3)
plot(simulationOutput)

## simple OLS model
summary(m2 <- lm(behavior ~ group, data = data))
summary(m3 <- lm(behavior ~ group + sex + age + musicianship, data = data))

summary(allEffects(m2))
simulationOutput = simulateResiduals(m2)
plot(simulationOutput) # this is the fit of your t-test