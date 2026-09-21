#Raincloud plot code to produce Fig. 5 (partially adapted from code from the following publication):
#Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., Van Langen, J., & Kievit, R. A. (2021). Raincloud plots: A multi-platform tool for robust data visualization. Wellcome Open Research, 4, 63. https://doi.org/10.12688/wellcomeopenres.15191.2

# width and height variables for saved plots
w = 6
h = 3
# Make the figure folder if it doesn't exist yet
dir.create('../figs/tutorial_R/', showWarnings = FALSE)

head(summary_simdat)

##Start here to reproduce analyses
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



