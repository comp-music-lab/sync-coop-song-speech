### Load data ###
datafilename = "keydata_long_20260118.csv"
rawdatafilename = "stage2data_20260118.csv"

source("h_keydata.R")
h_keydata(datafilename, rawdatafilename)

source("h_datalist.R")
datalist <- h_datalist(datafilename, rawdatafilename)

### Run analysis ###
# Use Chuu et al. (2021, AISTATS)'s method instead of the stepping-stone sampling algorithm described in the manuscript
# to compute marginal likelihood to quickly check preliminary results

library(rstan)
library(rpart)
library(posterior)

consmat = list(
  matrix(c(0, 1, -1, 0), ncol=1),
  matrix(c(0, -1, 1, 0), ncol=1),
  matrix(c(0, 1, 0, -1), ncol=1),
  matrix(c(0, -1, 0, 1), ncol=1)
)

varname = c("be[1]", "be[2]", "be[3]", "be[4]", "s_1", "s_2", "sgm", "g")

rseed = 2059
pattern = c(0, 1, 2, 3)
modellist <- vector(mode="list", length=length(pattern))
possamplelist <- vector(mode="list", length=length(pattern))
posconschainlist <- vector(mode="list", length=length(pattern))
lnZ = rep(0, length(pattern))
stanfile = "lmm_linconuip.stan"
numwarmup = c(1000, 1000, 1000, 1000)
numiter = c(2000, 20000, 2000, 20000)
numchain = 4
rhat_theta <- vector(mode="list", length=length(pattern))

h_lnhalft <- function(x, nu, s) {
  log(2) + log(gamma((nu+1)/2)) - log(gamma(nu/2)) - log(sqrt(nu*pi*s^2)) + (-(nu+1)/2)*log(1 + 1/nu*x^2/s^2)
}

source("h_standata_02.R")
source("h_Lmd.R")
source("h_uipmvn.R")
source("h_mvnlik.R")

for(i in 1:length(pattern)) {
  # posterior inference
  cat(paste(Sys.time(), ": i = ", i, "\n", sep=""))
  standata <- h_standata(datalist, pattern[i])
  fit_pos <- stan(file = stanfile, data = standata, chains = numchain, 
                  warmup = numwarmup[i], iter = numiter[i], cores = 4, refresh = 0)
  print(fit_pos, pars=c("sgm", "s_1", "s_2", "be", "g"))
  modellist[[i]] <- fit_pos
  
  # keep posterior samples satisfying constraints
  theta = extract(modellist[[i]], pars="be", permuted=FALSE, inc_warmup=FALSE)
  idx_cons = sapply(1:numchain, function(k){theta[, k, ] %*% consmat[[i]] > 0})
  N = min(colSums(idx_cons))
  idx_cons_N = sapply(1:numchain, function(k){which(idx_cons[, k])[1:N]})
  
  rhat_theta[[i]] = data.frame(
    varname=character(), mean=double(), sd=double(), 
    `2.5%`=double(), `25%`=double(), `50%`=double(), `75%`=double(), `97.5%`=double(), rhat=double()
  )
  possamplelist[[i]] = data.frame(varname=character(), samples=double(), chain=integer(), t=integer())
  
  for (k in 1:length(varname)) {
    theta = extract(modellist[[i]], pars=varname[k], permuted=FALSE, inc_warmup=FALSE)
    theta_N = sapply(1:numchain, function(k){theta[idx_cons_N[, k], k, ]})
    rhat_theta[[i]] = rbind(rhat_theta[[i]],
                            data.frame(varame=varname[k], mean=mean(theta_N), sd=sd(theta_N),
                                       `2.5%`= quantile(theta_N, 0.025), `25%`= quantile(theta_N, 0.25), 
                                       `50%`= quantile(theta_N, 0.50), `75%`= quantile(theta_N, 0.75),
                                       `97.5%`= quantile(theta_N, 0.975), rhat=rhat(theta_N))
                            )
    possamplelist[[i]] = rbind(
      possamplelist[[i]],
      data.frame(varname=varname[k], samples=c(theta_N), chain=rep(1:numchain, each=N), t=rep(1:N, times=numchain))
    )
  }
  rownames(rhat_theta[[i]]) <- NULL
  print(rhat_theta[[i]])
  possamplelist[[i]]$varname <- sub("[", "_", possamplelist[[i]]$varname, fixed=TRUE)
  possamplelist[[i]]$varname <- sub("]", "", possamplelist[[i]]$varname, fixed=TRUE)
  
  # Bayes factor computation
  sgm_pos = matrix(possamplelist[[i]]$samples[possamplelist[[i]]$varname == "sgm"], nrow=1)
  s_1_pos = matrix(possamplelist[[i]]$samples[possamplelist[[i]]$varname == "s_1"], nrow=1)
  s_2_pos = matrix(possamplelist[[i]]$samples[possamplelist[[i]]$varname == "s_2"], nrow=1)
  r_pos = 1/(matrix(possamplelist[[i]]$samples[possamplelist[[i]]$varname == "g"], nrow=1) + 1)
  be_pos = rbind(
    matrix(possamplelist[[i]]$samples[possamplelist[[i]]$varname == "be_1"], nrow=1),
    matrix(possamplelist[[i]]$samples[possamplelist[[i]]$varname == "be_2"], nrow=1),
    matrix(possamplelist[[i]]$samples[possamplelist[[i]]$varname == "be_3"], nrow=1),
    matrix(possamplelist[[i]]$samples[possamplelist[[i]]$varname == "be_4"], nrow=1)
  )
  
  M = standata$M
  N = standata$N
  n = standata$n
  idx_u_1 = standata$idx_u_1
  idx_u_2 = standata$idx_u_2
  X = standata$X
  y = standata$y

  psi = 
    dbeta(r_pos, 0.01, 0.01*N, log=TRUE) + 
    h_lnhalft(sgm_pos, 2, 1000) +
    h_lnhalft(s_1_pos, 2, 1000) +
    h_lnhalft(s_2_pos, 2, 1000) +
    sapply(1:length(sgm_pos), function(j){h_uipmvn(X, be_pos[,j], sgm_pos[j], s_1_pos[j], s_2_pos[j], 1/r_pos[j] - 1, M, n)}) +
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
cat(paste(Sys.time(), ": lnZ = ", lnZ[3], " vs. ", lnZ[4], "\n", sep=""))

### Plot results ###
library(ggridges)
library(ggplot2)
library(ggpubr)

# Plot posterior distributions
modelname = c("2a", "0b", "2b", "0b")
ggplotlist <- vector(mode="list", length=length(pattern))

for(i in 1:length(pattern)) {
  df_ggplot <- possamplelist[[i]]
  
  df_ggplot$varname[df_ggplot$varname == 'sgm'] = 'σ'
  df_ggplot$varname[df_ggplot$varname == 's_1'] = 'σ (cohort-level)'
  df_ggplot$varname[df_ggplot$varname == 's_2'] = 'σ (individual-level)'
  df_ggplot$varname[df_ggplot$varname == 'g'] = 'g'
  df_ggplot$varname[df_ggplot$varname == 'be_1'] = 'Intercept'
  df_ggplot$varname[df_ggplot$varname == 'be_2'] = 'Singing'
  df_ggplot$varname[df_ggplot$varname == 'be_3'] = 'Conversation'
  df_ggplot$varname[df_ggplot$varname == 'be_4'] = 'Recitation'
  
  df_ggplot$varname <- factor(df_ggplot$varname,
                              levels=c('g', 'σ (individual-level)',
                                       'σ (cohort-level)', 'σ', 'Recitation',
                                       'Conversation', 'Singing', 'Intercept'
  ))
  
  ggplotlist[[i]] <- ggplot(df_ggplot, aes(x=samples, y=varname, group=varname)) +
    geom_density_ridges(scale=1.2) +
    theme_ridges() + 
    theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank()) +
    ggtitle(paste("Posterior distribution (H", modelname[i], ")", sep="")) +
    xlim(-10, 60)
}

g <- ggarrange(ggplotlist[[1]], ggplotlist[[2]], ncol=2, nrow=1)
g <- annotate_figure(g, top=text_grob(
  paste("Model comparison (log10BF", paste(modelname[1:2], collapse="") ,"= ", sprintf("%0.2f", (lnZ[1] - lnZ[2])/log(10)), ")", sep=""),
  face="bold", size=16))
ggsave(g, dpi=1200, height=6, width=9, filename="./figure/confirmatory_analysis_02_1_BF.png")

g <- ggarrange(ggplotlist[[3]], ggplotlist[[4]], ncol=2, nrow=1)
g <- annotate_figure(g, top=text_grob(
  paste("Model comparison (log10BF", paste(modelname[3:4], collapse="") ,"= ", sprintf("%0.2f", (lnZ[3] - lnZ[4])/log(10)), ")", sep=""),
  face="bold", size=16))
ggsave(g, dpi=1200, height=6, width=9, filename="./figure/confirmatory_analysis_02_2_BF.png")

# Plot original chain samples
for (i in 1:length(pattern)) {
  g <- traceplot(modellist[[i]], pars=c("sgm", "s_1", "s_2", "be", "g"), inc_warmup=FALSE, nrow=2)
  figfilename <- paste("./figure/confirmatory_analysis_02_chain_", i, ".png", sep="")
  ggsave(g, dpi=1200, height=6, width=9, filename=figfilename)
}

# Plot chain samples satisfying constraints
for (i in 1:length(pattern)) {
  varname_plot <- unique(possamplelist[[i]]$varname)
  glist <- vector(mode="list", length=length(varname_plot))
  
  for(k in 1:length(glist)) {
    gdata = possamplelist[[i]][possamplelist[[i]]$varname == varname_plot[k], ]
    gdata$chain = as.character(gdata$chain)
    glist[[k]] <- ggplot(data=gdata) + geom_line(aes(x=t, y=samples, group=chain, color=chain)) + 
      labs(title=varname_plot[k], x="", y="") +
      theme(plot.title=element_text(hjust = 0.5),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.background=element_rect(fill='white')
      )
  }
  
  g <- ggarrange(plotlist=glist, nrow=2, ncol=4, common.legend=TRUE, legend="right")
  
  figfilename <- paste("./figure/confirmatory_analysis_02_conschain_", i, ".png", sep="")
  ggsave(g, dpi=1200, height=6, width=9, filename=figfilename)
}