### Load data ###
datafilename = "keydata_long_20250525.csv"
rawdatafilename = "stage2data_20250525.csv"
source("h_datalist.R")
datalist <- h_datalist(datafilename, rawdatafilename)

### Plot score difference (overall) ###
idx_indi = sapply(1:dim(datalist$Z)[1], function(i){which(datalist$Z[i,(datalist$M+1):(datalist$M+datalist$N)] == 1)})
df_scorediff = data.frame(
  dy = sapply(1:datalist$N, function(i){datalist$y[idx_indi == i][1] - datalist$y[idx_indi == i][2]}),
  cond = sapply(1:datalist$N, function(i){which(datalist$X[which(idx_indi == i)[1], 2:4] == 1)}),
  coho = sapply(1:datalist$N, function(i){which(datalist$Z[which(idx_indi == i)[1], 1:datalist$M] == 1)}),
  site = sapply(1:datalist$N, function(i){datalist$data$site[which(idx_indi == i)[1]]})
)

library(ggplot2)
g <- ggplot(data=df_scorediff, aes(y=dy, x=factor(cond))) + 
  geom_violin() + geom_point(alpha=0.3) + geom_hline(yintercept=0, alpha=0.5) +
  ylab("Δscore") + xlab("") + 
  scale_x_discrete(breaks=c(1,2,3), labels=c("Singing", "Coversation", "Recitation")) +
  ggtitle("Pre-post social bonding score difference (all sites)") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("./figure/diffscore_allsites.png", g)

plot(g)

### Plot score difference (site-cohort) ###
library(ggpubr)

ggplotlist <- vector(mode="list", length=max(df_scorediff$site))
for(i in 1:max(df_scorediff$site)) {
  ggplotlist[[i]] <- ggplot(data=df_scorediff[df_scorediff$site == i,], aes(y=dy, x=factor(cond))) + 
    geom_violin() + geom_point(alpha=0.6) + geom_hline(yintercept=0, alpha=0.5) +
    ylab("") + xlab("") + 
    scale_x_discrete(breaks=c(1,2,3), labels=c("S", "C", "R")) +
    ylim(-50, 68)
  
  if(i%%3 == 1) ggplotlist[[i]] <- ggplotlist[[i]] + ylab("Δscore")
  else ggplotlist[[i]] <- ggplotlist[[i]] + theme(axis.text.y = element_blank())
}
g <- ggarrange(plotlist=ggplotlist)
g <- annotate_figure(g, top=text_grob("Pre-post social bonding score difference (per site)", 
                                 face = "bold", size = 14))
ggsave("./figure/diffscore_persite.png", g)

plot(g)
