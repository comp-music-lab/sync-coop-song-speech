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

rseed = 2059;
pattern = c(0, 1, 2, 3)
modellist <- vector(mode="list", length=length(pattern))
possamplelist <- vector(mode="list", length=length(pattern))
lnZ = rep(0, length(pattern))
stanfile = "lmm_linconuip.stan"

h_lnhalft <- function(x, nu, s) {
  log(2) + log(gamma((nu+1)/2)) - log(gamma(nu/2)) - log(sqrt(nu*pi*s^2)) + (-(nu+1)/2)*log(1 + 1/nu*x^2/s^2)
}

source("h_standata_02.R")
source("h_Lmd.R")
source("h_uipmvn.R")
source("h_mvnlik.R")

source("chain_stacking.R")
stan_model_object = stan_model("stacking_opt.stan")

for(i in 1:length(pattern)) {
  cat(paste(Sys.time(), ": i = ", i, "\n", sep=""))
  standata <- h_standata(datalist, pattern[i])
  fit_pos <- stan(file = stanfile, data = standata, chains = 4, 
                  warmup = 11000, iter = 12000, cores = 4, refresh = 0)
  print(fit_pos, pars=c("sgm", "s_1", "s_2", "be", "g"))
  modellist[[i]] <- fit_pos
  
  stack_obj = chain_stack(fits=fit_pos, lambda=1.0001, log_lik_char="log_lik")
  print(stack_obj$chain_weights)
  
  sgm_pos = t(mixture_draws(individual_draws=drop(extract(fit_pos, permuted=FALSE, pars="sgm")), weight=stack_obj$chain_weights, random_seed=rseed))
  s_1_pos = t(mixture_draws(individual_draws=drop(extract(fit_pos, permuted=FALSE, pars="s_1")), weight=stack_obj$chain_weights, random_seed=rseed))
  s_2_pos = t(mixture_draws(individual_draws=drop(extract(fit_pos, permuted=FALSE, pars="s_2")), weight=stack_obj$chain_weights, random_seed=rseed))
  be_pos = extract(fit_pos, permuted=FALSE, pars="be")
  be_pos = t(sapply(1:standata$p, function(i) {mixture_draws(individual_draws=be_pos[,,i], weight=stack_obj$chain_weights, random_seed=rseed)}))
  r_pos = t(mixture_draws(individual_draws=drop(extract(fit_pos, permuted=FALSE, pars="r")), weight=stack_obj$chain_weights, random_seed=rseed))
  
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

for (i in 1:length(pattern)) {
  g <- traceplot(modellist[[i]], pars=c("sgm", "s_1", "s_2", "be", "g"), inc_warmup=TRUE, nrow=2)
  figfilename <- paste("./figure/confirmatory_analysis_02_chain_", i, ".png", sep="")
  ggsave(g, dpi=1200, height=6, width=9, filename=figfilename)
}