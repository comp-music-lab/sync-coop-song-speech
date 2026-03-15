# Overall and group size-wise pre-post experiment social bonding score differences

### Load data ###
datafilename = "keydata_long_20260314.csv"
rawdatafilename = "stage2data_20260314.csv"
explogfilename = "experimentlog_20260314.csv"

source("h_keydata.R")
h_keydata(datafilename, rawdatafilename)

source("h_datalist.R")
datalist <- h_datalist(datafilename, rawdatafilename)

source("h_explog.R")
explog <- h_explog(explogfilename)

### Add group size information ###
N = datalist$N

df_ggplot <- data.frame(
  cond=sapply(1:(2*N), function(x){ifelse(sum(datalist$X[x, 2:4]) == 0, 0, which(datalist$X[x, 2:4] == 1))}),
  y=datalist$y,
  site=as.factor(datalist$data$site),
  N=datalist$N_X,
  ID=sapply(1:(2*N), function(x){which(datalist$Z[x, (datalist$M+1):(datalist$M+datalist$N)]==1)})
)
df_ggplot <- merge(df_ggplot[df_ggplot$cond != 0, ], df_ggplot[df_ggplot$cond == 0, ], by="ID")
df_ggplot <- data.frame(ID=df_ggplot$ID, dy=(df_ggplot$y.x - df_ggplot$y.y),
                        cond=df_ggplot$cond.x, site=df_ggplot$site.x, N_post=df_ggplot$N.x)

df_explog <- data.frame(site=explog$site, cond=explog$cond, N_pre=explog$Number.of.participants..NB..This.may.vary.from.the.number.of.Qualtrics.responses...for.example..if.one.participant.from.a.group.of.10.fails.to.complete.the.Qualtrics.survey..only.9.responses.will.appear.but.the..Number.of.participants..for.that.cohort.is.10..)
df_ggplot <- merge(df_ggplot, df_explog, by=c("site", "cond"))

df_ggplot$N_pre <- as.factor(df_ggplot$N_pre)
levels(df_ggplot$N_pre) <- c('N=5', 'N=6', 'N=7', 'N=8', 'N=9', 'N=10')
df_ggplot$N_post <- as.factor(df_ggplot$N_post)
levels(df_ggplot$N_post) <- c('N=4', 'N=5', 'N=6', 'N=7', 'N=8', 'N=9', 'N=10')

### ggplot  ###
library(ggplot2)
library(ggpubr)
library(patchwork)

condname <- c("Social bonding score change\n(group singing)",
              "Social bonding score change\n(group conversation)",
              "Social bonding score change\n(group recitation)")

g1list <- vector(mode="list", length=3)
g2list <- vector(mode="list", length=3)

for(i in 1:3) {
  g1list[[i]] <- ggplot(data=df_ggplot[df_ggplot$cond==i, ], aes(y=dy, x=N_pre, color=site)) + 
    geom_violin(trim=TRUE, draw_quantiles=c(0.5)) + geom_point(position=position_dodge(width=0.9)) + geom_hline(yintercept=0, linetype="dashed") + 
    xlab("Group size before exclusion") + ylab("Δscore") + ylim(-50, 80) + 
    facet_grid(~N_pre, scales='free_x') +
    ggtitle(condname[i]) + 
    theme(plot.title=element_text(hjust=0.5, size=13, face="bold"), legend.position="none", axis.text.x=element_blank(),
          plot.margin = margin(0, 0, 0, 0, "pt"))

  ggsave(height=5, width=5, dpi=1200, filename=paste("./figure/exploratory_analysis_02_dscore_", i, ".png", sep=""), plot=g1list[[i]])
  
  g2list[[i]] <- ggplot(data=df_ggplot[df_ggplot$cond==i, ], aes(y=dy)) + 
    geom_density(fill="#BBBBBB") + geom_hline(yintercept=0, linetype="dashed") + 
    ylim(-50, 80) + xlab("") + ylab("") + 
    theme(axis.text.x=element_blank(), axis.text.y=element_blank(), panel.background=element_rect(fill='white', colour='white'),
          plot.margin = margin(0, 0, 0, 0, "pt"))
  
  ggsave(height=5, width=1, dpi=1200, filename=paste("./figure/exploratory_analysis_02_dscore_", i, "_dens.png", sep=""), plot=g2list[[i]])
}

g1list[[2]] = g1list[[2]] + labs(y=NULL)
g1list[[3]] = g1list[[3]] + labs(y=NULL)
g <- g1list[[1]] + g2list[[1]] + g1list[[2]] + g2list[[2]] + g1list[[3]] + g2list[[3]] + 
  plot_layout(axes = "collect", widths=c(1, 0.15, 1, 0.15, 1, 0.15))
plot(g)

ggsave(height=4, width=10, dpi=1200, filename=paste("./figure/exploratory_analysis_02_dscore.png", sep=""), plot=g)