### Load data ###
datafilename = "keydata_long_20250712.csv"
rawdatafilename = "stage2data_20250712.csv"
source("h_datalist.R")
datalist <- h_datalist(datafilename, rawdatafilename)

### Run analysis ###
# Use Chuu et al. (2021, AISTATS)'s method instead of the stepping-stone sampling algorithm described in the manuscript
# to compute marginal likelihood to quickly check preliminary results

library(rstan)
library(rpart)

modellist <- vector(mode="list", length=2)
possamplelist <- vector(mode="list", length=2)
ppcsamplelist <- vector(mode="list", length=2)
lnZ = c(0, 0)
stanfile = c("lmm_ssauip.stan", "lmm_ssauip.stan")
model_H0 = c(0, 1)
q = 2
numchains = 6

h_lnhalft <- function(x, nu, s) {
  log(2) + log(gamma((nu+1)/2)) - log(gamma(nu/2)) - log(sqrt(nu*pi*s^2)) + (-(nu+1)/2)*log(1 + 1/nu*x^2/s^2)
}

source("h_standata_03.R")
source("h_Lmd.R")
source("h_nlmvn.R")
source("h_uipmvn.R")
source("h_mvnlik.R")

for(i in 1:2) {
  cat(paste(Sys.time(), ": i = ", i, "\n", sep=""))
  standata <- h_standata(datalist, model_H0[i], q)
  fit_pos <- stan(file = stanfile[i], data = standata, chains = numchains, 
                  warmup = 1000, iter = 2000, cores = 4, refresh = 0,
                  control = list(adapt_gamma=0.05, adapt_kappa=0.75, adapt_t0=10, adapt_delta=0.80, max_treedepth=10, adapt_term_buffer=50))

  print(fit_pos, pars=c("sgm", "s_1", "s_2", "be", "g"))
  modellist[[i]] <- fit_pos
  
  posterior_samples = as.data.frame(fit_pos)
  sgm_pos = t(unname(as.matrix(posterior_samples["sgm"])))
  s_1_pos = t(unname(as.matrix(posterior_samples["s_1"])))
  s_2_pos = t(unname(as.matrix(posterior_samples["s_2"])))
  be_pos = t(unname(as.matrix(posterior_samples[sapply(1:standata$p, function(x){paste("be[", x, "]", sep="")})])))
  r_pos = t(unname(as.matrix(posterior_samples["r"])))
  y_pos = unname(extract(fit_pos, permuted=FALSE, pars="y_rep"))
  ppcsamplelist[[i]] = do.call(cbind, lapply(1:numchains, function(x) {t(y_pos[,x,])}))
  
  possamplelist[[i]] = rbind(
    data.frame(varname="sgm", samples=t(sgm_pos)),
    data.frame(varname="s_1", samples=t(s_1_pos)),
    data.frame(varname="s_2", samples=t(s_2_pos)),
    data.frame(varname="be_1", samples=be_pos[1, ]),
    data.frame(varname="be_2", samples=be_pos[2, ]),
    data.frame(varname="be_3", samples=be_pos[3, ]),
    data.frame(varname="be_4", samples=be_pos[4, ]),
    data.frame(varname="g", samples=1/t(r_pos) - 1)
  )
  
  if(model_H0[i] == 0) {
    possamplelist[[i]] = rbind(possamplelist[[i]], data.frame(varname="be_5", samples=be_pos[5, ]))
  } else {
    possamplelist[[i]] = rbind(possamplelist[[i]], data.frame(varname="be_5", samples=NaN))
  }
  
  M = standata$M
  N = standata$N
  n = standata$n
  idx_u_1 = standata$idx_u_1
  idx_u_2 = standata$idx_u_2
  X = standata$X
  y = standata$y
  qq = standata$qq
  
  h_betapri <- function(X, be, sgm, s_1, s_2, g, M, n, q, qq){h_uipmvn(X, be, sgm, s_1, s_2, g, M, n)}
  
  psi = 
    dbeta(r_pos, 0.01, 0.01*N, log=TRUE) + 
    h_lnhalft(sgm_pos, 2, 1000) +
    h_lnhalft(s_1_pos, 2, 1000) +
    h_lnhalft(s_2_pos, 2, 1000) +
    sapply(1:length(sgm_pos), function(j){h_betapri(X, be_pos[,j], sgm_pos[j], s_1_pos[j], s_2_pos[j], 1/r_pos[j] - 1, M, n, q, qq)}) +
    sapply(1:length(sgm_pos), function(j){h_mvnlik(y, X, be_pos[,j], sgm_pos[j], s_1_pos[j], s_2_pos[j], M, n)})
  
  u = data.frame(t(rbind(r_pos, sgm_pos, s_1_pos, s_2_pos, be_pos)))
  u$psi = c(-psi)
  fit.tree = rpart(psi ~ ., data=u, method="anova", cp=0.008)
  idx_A = unique(fit.tree$where)
  K = length(idx_A)
  lnZ_k = matrix(0, nrow=K, ncol=1)
  for(k in 1:K) {
    u_k = u[fit.tree$where == idx_A[k], ]
    b_k = sapply(u_k, max)
    a_k = sapply(u_k, min)
    idx_psi = order(u_k$psi)
    S = sum(exp(u_k$psi - max(u_k$psi)))
    W = cumsum(exp(u_k$psi[idx_psi] - max(u_k$psi)))
    idx_med = min(which(W/S >= 0.5))
    c_k = u_k$psi[idx_psi[idx_med]]
    lnZ_k[k] = -c_k + sum(log(b_k - a_k))
  }
  lnZ[i] = max(lnZ_k) + log(sum(exp(lnZ_k - max(lnZ_k))))
  
  cat(paste(Sys.time(), ": lnZ (i=", i, ") = ", lnZ[i], "\n", sep=""))
}

cat(paste(Sys.time(), ": lnZ = ", lnZ[1], " vs. ", lnZ[2], "\n", sep=""))

### Plot results (posterior distributions) ###
library(ggridges)
library(ggplot2)
library(ggpubr)

ggplotlist <- vector(mode="list", length=2)
modelname = c("1", "0a")

for(i in 1:2) {
  df_ggplot <- possamplelist[[i]]
  
  ggplotlist[[i]] <- ggplot(df_ggplot, aes(x=samples, y=varname, group=varname)) +
    geom_density_ridges(scale=1.2) +
    theme_ridges() + 
    theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank()) +
    ggtitle(paste("Posterior distribution (H", modelname[i], ")", sep="")) +
    xlim(-10, 60)
}

g <- ggarrange(plotlist=ggplotlist, ncol=2, nrow=1)
g <- annotate_figure(g,
                     top=text_grob(
                       paste("Model comparison (log10BF", paste(modelname, collapse=""), " = ", sprintf("%0.2f", (lnZ[1] - lnZ[2])/log(10)), ")", sep=""), 
                     face="bold", size=16))

ggexport(g, filename="./figure/exploratory_analysis_01_BF.png")

plot(g)

### Plot results (posterior predictive checks) ###
df_ggplot <- data.frame(
  cond=sapply(1:(2*N), function(x){ifelse(sum(datalist$X[x, 2:4]) == 0, 0, which(datalist$X[x, 2:4] == 1))}),
  y=datalist$y,
  y_H0=rowMeans(ppcsamplelist[[2]]),
  y_H1=rowMeans(ppcsamplelist[[1]]),
  Site=as.factor(datalist$data$site)
)

condname <- c("Pre-intervention", "Post-intervention (group singing)",
              "Post-intervention (group conversation)", "Post-intervention (group recitation)")

ggplotlist <- vector(mode="list", length=4)
for(j in 1:4) {
  ggplotlist_j <- vector(mode="list", length=2)
  ggplotlist_j[[1]] <- ggplot(data=df_ggplot[df_ggplot$cond==(j-1), ], aes(x=y, y=y_H0, color=Site)) + geom_point() + geom_abline(slope=1, intercept=0, linetype="dashed") + xlim(0, 100) + ylim(0, 100) + xlab("Observed score") + ylab("Simulated score (H0)") + theme(axis.title.x=element_text(size = 9), axis.title.y=element_text(size = 9))
  ggplotlist_j[[2]] <- ggplot(data=df_ggplot[df_ggplot$cond==(j-1), ], aes(x=y, y=y_H1, color=Site)) + geom_point() + geom_abline(slope=1, intercept=0, linetype="dashed") + xlim(0, 100) + ylim(0, 100) + xlab("Observed score") + ylab("Simulated score (H1)") + theme(axis.title.x=element_text(size = 9), axis.title.y=element_text(size = 9))
  g <- ggarrange(plotlist=ggplotlist_j, ncol=2, nrow=1, legend="none")
  g <- annotate_figure(g, top=text_grob(paste(condname[j], sep=""), face="bold", size=12))
  ggplotlist[[j]] <- g
  
  ggsave(filename=paste("./figure/exploratory_analysis_01_PPC_", j, ".png", sep=""),
         plot=ggplotlist[[j]], width=4, height=2
  )
}

# dummy data to extract legend information
df_dummy <- data.frame(x=1:13, y=1:13,
                       Site=factor(1:13, labels=c("UK [London]", "Indonesia [Surakarta]", "Japan [Kanagawa]",
                                                  "Romania [Cluj]", "India [New Delhi]", "New Zealand [Auckland] (1)",
                                                  "New Zealand [Auckland] (2)", "Italy [Padova]", "Italy [Rome]", 
                                                  "Czech Republic [Prague]", "Thailand [Bangkok]", "UK [Reading]",
                                                  "Nigeria [Lagos]")))
g <- ggplot(data=df_dummy, aes(x=x, y=y, color=Site)) + geom_point() + 
  guides(color=guide_legend(ncol=4, nrow=4, byrow=TRUE))
gl <- as_ggplot(get_legend(g))
ggsave(filename="./figure/exploratory_analysis_legend.png", plot=gl)
plot(gl)

### Additional analysis ###
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

  ggsave(filename=paste("./figure/exploratory_analysis_01_dscore_", i, ".png", sep=""), plot=g)
}

plot(g)

'
traceplot(modellist[[1]], pars=c("sgm", "s_1", "s_2", "be", "g"), inc_warmup=TRUE, nrow=2)
traceplot(modellist[[2]], pars=c("sgm", "s_1", "s_2", "be", "g"), inc_warmup=TRUE, nrow=2)

scorediff <- t(sapply(unique(datalist$data$Participant), function(i){
  data.frame(Participant = i, 
             scorediff = datalist$data$score[datalist$data$Participant == i & datalist$data$time == "Post_Experiment"] - 
               datalist$data$score[datalist$data$Participant == i & datalist$data$time == "Pre_Experiment"],
             site = unique(datalist$data$site[datalist$data$Participant == i]),
             group = unique(datalist$data$group[datalist$data$Participant == i])
  )
}))
'