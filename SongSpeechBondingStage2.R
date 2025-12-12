#This code was used to perform the analyses reported in:
#Savage, P. E., Ampiah-Bonney, A., Arabadjiev, A., Arhine, A., Ariza, J. F., Arnese, F., Bamford, J. S., Barbosa, B. S., Beck, A.-K., Belyk, M., Benetos, E., Bulbulia, J. A., Cabildo, A., Calhoun, S., Chiba, G., Dabaghi Varnosfaderani, S., Dias, R., Duran, S. I., Echim, S., Færøvik, U., Fitch, W. T., Fujii, S., Gabriel, S., Grassi, M., Guiotto Nai Fovino, L. Haiduk, F., Hajič jr., J., Hansen, N. C., Han, K. Y., He, Y., Hegde, S., Honbolygó, F., Huang, J., Jacoby, N., Jadoul, Y., Jia, Z., Jung, T., Kertész, C., Khasanah, U., Kim, I., Kitayama, Y., Kosachenko, A., Krzyżanowski, W., Kuikuro, U., Kurdova, D., Larrouy-Maestri, P., Mikova, Z., Leongómez, J. D., Liu, F., Lomsadze, T., Loui, P., Ma, Y., McBride, J. M., Mousavi, N., Moya, D., Natsitsabui, R., Nguqu, N., Novembre, G., Nuska, P., Nweke, F. E., Ong, J. H., Opondo, P., Ozaki, Y., Parkinson, H., Parselelo, M. L., Pavlov, Y.G., Pavlovich, D., Perry, G., Pfordresher, P. Q., Pisanski, K., Podlipniak, P., Popescu, T., Proutskova, P., Purdy, S., Ravignani, A., Raviv, L., Reindl, E., Ross, R. M., Sadaphal, D. P., Shakya, S. R., Shi, Z., Shilton, D., Silva-Zurita, J., Soto-Silva, I., Štěpánková, B., Talamini, F., Tarr, B., Thompson, W. F., Tierney, A., Tiratanti, P., Trainor, L., Tunçgenç,  B., Vanden Bosch der Nederlanden, C., Vaida, S. Varella, M. A. C., Youngblood, M., Zariquiey, R. (In Principle Accepted). Does synchronised singing enhance social bonding more than speaking does? A global experimental Stage 1 Registered Report. Peer Community In Registered Reports. Preprint: https://doi.org/10.31234/osf.io/pv3m9 

#It was partially adapted from code from the following publication:
#Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., Van Langen, J., & Kievit, R. A. (2021). Raincloud plots: A multi-platform tool for robust data visualization. Wellcome Open Research, 4, 63. https://doi.org/10.12688/wellcomeopenres.15191.2


#Set working directory
setwd('/Users/psav050/Documents/GitHub/sync-coop-song-speech')#NB: You need to set this to your own local working directory to reproduce the analysis

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

#Load and pre-process data (NB: Full raw files with pilot participant data not shared publicly, just shown for transparency)

#Load and clean experimenter reports (used to help exclude pilot data below):
e<-read_csv(file='/Users/psav050/Documents/Research/Publications/Accepted/Savage et al (2025) PCI-RR Many Voices 2/MV2 real data/Many Voices 2 post-experiment survey (for experimenter)_December 12, 2025_11.29.csv')#import from main Qualtrics account output - not shared publicly
colnames(e)<-e[1,] #change column names to make clear what they are
e<-e[-c(1:2),] #remove non-data rows
e$`IP Address`="NA"#Remove IP address data
e$`Response ID`="NA"#Remove Response ID data
e<-e[-c(1:3,7:9,13:16,25,30,37:38,43,54,58,61:65,68:70,72:74,81,89,99,108),]#Exclude pilot experiments (change from hard-coding?)
e[79,23]<-"9" #correct experimenter entry error
write.csv(e,'experimentlog.csv')

#Load participant data
df<-read_csv(file='/Users/psav050/Documents/Research/Publications/Accepted/Savage et al (2025) PCI-RR Many Voices 2/MV2 real data/SpeechSong_London_Chinese_March 21, 2025_20.44_unifiedrows.csv') #import from separate Qualtrics account output - not shared publicly
df<-df[-c(12,13),] #Exclude duplicated participant data (change from hard-coding?)
d<-read_csv(file='/Users/psav050/Documents/Research/Publications/Accepted/Savage et al (2025) PCI-RR Many Voices 2/MV2 real data/song_December 12, 2025_09.17.csv')#import from main Qualtrics account output - not shared publicly
d<-d[-c(1:244,276:304,320:322,384:392,407,417:421,451:453,455:467,474,491:493,501:514,542:546,566:572,579:591,611,614,623:624,631:718,726:742,758:782,788:795,815:859,877:878,887:889,916:917,928,934,949,971:972,988:990,1016:1020,1026,1033:1035,1042:1048,1057:1067,1085:1091,1098,1105:1106,1127,1137:1141,1137:1155),]#Exclude pilot data (change from hard-coding?)
d[18,19]="R" #fix participant entry error 
d[1,20]="10" #fix participant entry error
d[14,20]="6" #fix participant entry error
d[36,20]="2" #fix participant entry error
d[43,20]="2" #fix participant entry error
d[114,20]="1" #fix participant entry error
d[116,20]="6" #fix participant entry error
d[118,20]="2" #fix participant entry error
d[123,20]="1" #fix participant entry error
d[124,20]="8" #fix participant entry error
d[160,20]="5" #fix participant entry error
d[217,20]="4" #fix participant entry error
d[222,20]="1" #fix participant entry error
d[250,20]="R10" #fix participant entry error
d[256,20]="S2" #fix participant entry error
d[261,20]="C1" #fix participant entry error
d[267,20]="C5" #fix participant entry error
d[270,20]="C4" #fix participant entry error
d[323,20]="3Bog03" #fix participant entry error
d[326,20]="3Bog04" #fix participant entry error
d[349,19]="S" #fix participant entry error
d[349,20]="9" #fix participant entry error
d[351,20]="2" #fix participant entry error
d[353,20]="6" #fix participant entry error
d[354,20]="3" #fix participant entry error
d[357,20]="11" #fix participant entry error
d[368,20]="25" #fix participant entry error
d[373,20]="26" #fix participant entry error
d[416,37]="73" #fix participant entry error
d[440,20]="3" #fix participant entry error
d[450,19]="R" #fix participant entry error
d[451,19]="R" #fix participant entry error
d[452,19]="R" #fix participant entry error
d[453,19]="R" #fix participant entry error
d[454,19]="R" #fix participant entry error
d[479,19]="C" #fix participant entry error
d[520,19]="C" #fix participant entry error
d[524,19]="C" #fix participant entry error
d[534,19]="S" #fix participant entry error
d[538,20]="21" #fix participant entry error


d<-d[-c(21,74,130,277,394:395),] #Exclude duplicated/incomplete participant data (change from hard-coding?)
df<-rbind(df,d)
colnames(df)<-df[1,] #change column names to make clear what they are
#Rename with shorter variable names
names(df)[names(df) == 'What is the "Group ID" listed on the screen?'] <- 'group' 
names(df)[names(df) == "What is the \"Participant ID\" number listed on your consent form?"] <- 'ID'
names(df)[names(df) == "How much do you agree with the following statements? - Please rate “strongly agree” to show you are paying attention"] <- 'attention'
df<-df[-c(1:2),] #remove non-data rows
df<- subset(df, `Response Type`=="IP Address") #Remove test ("Preview") responses
df$`IP Address`="NA"#Remove IP address data
df$`Response ID`="NA"#Remove Response ID data
write.csv(df,'stage2data.csv')
##Start here to reproduce analyses
#df<-read_csv(file='stage2data.csv') #use this instead of GitHub download code below if running directly from computer
df<-read_csv(file='https://raw.githubusercontent.com/comp-music-lab/sync-coop-song-speech/refs/heads/main/stage2data.csv') #read full raw data file of Stage 2 participant data directly from GitHub
df <- df[-1] #remove ID row
df<-df[,c(19,20,22,24,26,28,29,31,33,35,37,40,41,42,44)] #keep only social bonding and key variables for confirmatory analysis

write.csv(df,'keydata.csv')
rep_data<-read_csv(file='keydata.csv')
rep_data<- subset(rep_data, attention>49) #Exclude participants failing attention check

#Rename variables:
names(rep_data)[names(rep_data) == "How much do you agree with the following statements? - ‘‘I trust the other participants”...4"] <- 'trust'
names(rep_data)[names(rep_data) == "How much do you agree with the following statements? - ‘‘I trust the other participants”...10"] <- 'trust'
names(rep_data)[names(rep_data) == "How much do you agree with the following statements? - \"I feel like I am on the same team with the other participants\"...5"] <- 'team'
names(rep_data)[names(rep_data) == "How much do you agree with the following statements? - \"I feel like I am on the same team with the other participants\"...11"] <- 'team'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - “I think I am similar to the other participants’’...6'] <- 'similar'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - “I think I am similar to the other participants’’...12'] <- 'similar'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - “I feel strong ties to the other participants”...7'] <- 'ties'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - “I feel strong ties to the other participants”...13'] <- 'ties'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - \"I have a lot in common with the other participants\"...8'] <- 'common'
names(rep_data)[names(rep_data) == 'How much do you agree with the following statements? - \"I have a lot in common with the other participants\"...15'] <- 'common'
names(rep_data)[names(rep_data) == 'How close do you feel to all the other participants? - 1...9'] <- 'close'
names(rep_data)[names(rep_data) == 'How close do you feel to all the other participants? - 1...16'] <- 'close'


#combine same variables to measure consistency
bind<-rbind(rep_data[,4:9],rep_data[,c(10:13,15:16)])

#Internal consistency analysis of individual bonding variables (Cronbach's alpha)
psych::alpha(bind)
psych::omega(bind)

#Average individual scores into an overall bonding score
rep_data$Pre_Experiment<-rowMeans(rep_data[,4:9]) #pre-experiment baseline average
rep_data<- subset(rep_data, Pre_Experiment<80) #Exclude participants too well-bonded before experiment
rep_data$Post_Experiment<-rowMeans(rep_data[,c(10:13,15:16)]) #post-experiment average

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

#Calculate mean % pre-post increase
(sumrepdat$score_mean[2]-sumrepdat$score_mean[1])/sumrepdat$score_mean[1] #singing
(sumrepdat$score_mean[4]-sumrepdat$score_mean[3])/sumrepdat$score_mean[3] #conversation
(sumrepdat$score_mean[6]-sumrepdat$score_mean[5])/sumrepdat$score_mean[5] #recitation

#Calculate total n
sumrepdat$N[2]+sumrepdat$N[4]+sumrepdat$N[6]



