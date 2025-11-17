### Load data ###
datafilename = "keydata_long_20250914.csv"
rawdatafilename = "stage2data_20250914.csv"
source("h_datalist.R")
datalist <- h_datalist(datafilename, rawdatafilename)

### Additional analysis ###
library(ggplot2)
library(ggpubr)

N = datalist$N

df_ggplot <- data.frame(
  cond=sapply(1:(2*N), function(x){ifelse(sum(datalist$X[x, 2:4]) == 0, 0, which(datalist$X[x, 2:4] == 1))}),
  y=datalist$y,
  Site=as.factor(datalist$data$site),
  N=datalist$N_X,
  ID=sapply(1:(2*N), function(x){which(datalist$Z[x, (datalist$M+1):(datalist$M+datalist$N)]==1)})
)
df_ggplot <- merge(df_ggplot[df_ggplot$cond != 0, ], df_ggplot[df_ggplot$cond == 0, ], by="ID")
df_ggplot <- data.frame(ID=df_ggplot$ID, dy=(df_ggplot$y.x - df_ggplot$y.y),
                        cond=df_ggplot$cond.x, Site=df_ggplot$Site.x, N=df_ggplot$N.x)
df_ggplot$N <- as.factor(df_ggplot$N)
levels(df_ggplot$N) <- c('N=4', 'N=5', 'N=6', 'N=7', 'N=8', 'N=9', 'N=10')

condname <- c("Post-intervention (group singing)",
              "Post-intervention (group conversation)", "Post-intervention (group recitation)")

for(i in 1:3) {
  g <- ggplot(data=df_ggplot[df_ggplot$cond==i, ], aes(y=dy, x=N, color=Site)) + 
    geom_violin(trim=TRUE, draw_quantiles=c(0.5)) + geom_point(position=position_dodge(width=0.9)) + geom_hline(yintercept=0, linetype="dashed") + 
    xlab("Group size") + ylab("Δscore") + ylim(-50, 80) + 
    facet_grid(~N, scales='free_x') +
    ggtitle(condname[i]) + 
    theme(plot.title=element_text(hjust=0.5, size=14, face="bold"), legend.position="none", axis.text.x=element_blank())

  ggsave(height=5, width=5, dpi=1200, filename=paste("./figure/exploratory_analysis_02_dscore_", i, ".png", sep=""), plot=g)
  
  g2 <- ggplot(data=df_ggplot[df_ggplot$cond==i, ], aes(y=dy)) + 
    geom_density(fill="#BBBBBB") + geom_hline(yintercept=0, linetype="dashed") + 
    ylim(-50, 80) + xlab("") + ylab("") + 
    theme(axis.text.x=element_blank(), axis.text.y=element_blank(), panel.background=element_rect(fill='white', colour='white'))
  
  ggsave(height=5, width=1, dpi=1200, filename=paste("./figure/exploratory_analysis_02_dscore_", i, "_dens.png", sep=""), plot=g2)
}

plot(g)