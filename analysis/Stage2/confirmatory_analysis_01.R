### Load data ###
datafilename = "keydata_long_20250525.csv"
rawdatafilename = "stage2data_20250525.csv"
source("h_datalist.R")
datalist <- h_datalist(datafilename, rawdatafilename)

# test 1
'
datalist$y[datalist$X[,2] == 1] = datalist$y[datalist$X[,2] == 1] + 0.0
'

# test 2
'
be_sim = matrix(c(40, 10, 5, 3), ncol=1)
sgm_sim = 14
s_sim = c(10, 6)
u_sim = matrix(c(rnorm(datalist$M, 0, s_sim[1]), rnorm(datalist$N, 0, s_sim[2])), ncol=1)
y_sim = datalist$X%*%be_sim + datalist$Z%*%u_sim + rnorm(dim(datalist$X)[1], 0, sgm_sim)
datalist$y = c(y_sim)
'

# test 3
'
library(lme4)
df = data.frame(y = datalist$y, x_song = datalist$X[,2], x_conv = datalist$X[,3],
                x_reci = datalist$X[,4],
                z_coho = sapply(1:dim(datalist$Z)[1], function(i){which(datalist$Z[i,1:datalist$M] == 1)}),
                z_indi = sapply(1:dim(datalist$Z)[1], function(i){which(datalist$Z[i,(datalist$M+1):(datalist$M+datalist$N)] == 1)})
)
fm1 <- lmer(y ~ x_song + x_conv + x_reci + (1 | z_coho) + (1 | z_indi), df)
'

### Run analysis ###
# Use Chuu et al. (2021, AISTATS)'s method instead of the stepping-stone sampling algorithm described in the manuscript
# to compute marginal likelihood to quickly check preliminary results

library(rstan)
library(rpart)

modellist <- vector(mode="list", length=2)
lnZ = c(0, 0)
stanfile = c("lmm_ssnlauip.stan", "lmm_ssauip.stan")
model_H0 = c(0, 1)
q = 2

h_lnhalft <- function(x, nu, s) {
  log(2) + log(gamma((nu+1)/2)) - log(gamma(nu/2)) - log(sqrt(nu*pi*s^2)) + (-(nu+1)/2)*log(1 + 1/nu*x^2/s^2)
}

source("h_standata_01.R")
source("h_Lmd.R")
source("h_nlmvn.R")
source("h_uipmvn.R")
source("h_mvnlik.R")

for(i in 1:2) {
  cat(paste(Sys.time(), ": i = ", i, "\n", sep=""))
  standata <- h_standata(datalist, model_H0[i], q)
  fit_pos <- stan(file = stanfile[i], data = standata, chains = 4, 
                  warmup = 1000, iter = 2000, cores = 4, refresh = 0)
  print(fit_pos, pars=c("sgm", "s_1", "s_2", "be", "g"))
  modellist[[i]] <- fit_pos
  
  posterior_samples = as.data.frame(fit_pos)
  sgm_pos = t(as.matrix(posterior_samples["sgm"]))
  s_1_pos = t(as.matrix(posterior_samples["s_1"]))
  s_2_pos = t(as.matrix(posterior_samples["s_2"]))
  be_pos = t(as.matrix(posterior_samples[sapply(1:standata$p, function(x){paste("be[", x, "]", sep="")})]))
  r_pos = t(as.matrix(posterior_samples["r"]))
    
  M = standata$M
  N = standata$N
  n = standata$n
  idx_u_1 = standata$idx_u_1
  idx_u_2 = standata$idx_u_2
  X = standata$X
  y = standata$y
  qq = standata$qq
  
  if(model_H0[i] == 0) {
    h_betapri <- function(X, be, sgm, s_1, s_2, g, M, n, q, qq){h_nlmvn(X, be, sgm, s_1, s_2, g, M, n, q, qq)}
  } else {
    h_betapri <- function(X, be, sgm, s_1, s_2, g, M, n, q, qq){h_uipmvn(X, be, sgm, s_1, s_2, g, M, n)}
  }
  
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

### Plot results ###
library(ggridges)
library(ggplot2)
library(ggpubr)

ggplotlist <- vector(mode="list", length=2)
modelname = c("1", "0a")

for(i in 1:2) {
  posterior_samples = as.data.frame(modellist[[i]])
  
  df_ggplot <- rbind(
    data.frame(varname="sgm", samples=unname(posterior_samples["sgm"])),
    data.frame(varname="s_1", samples=unname(posterior_samples["s_1"])),
    data.frame(varname="s_2", samples=unname(posterior_samples["s_2"])),
    data.frame(varname="be_1", samples=unname(posterior_samples["be[1]"])),
    data.frame(varname="g", samples=unname(1/posterior_samples["r"] - 1))
  )
  
  if (modelname[i] == "1") {
    df_ggplot <- rbind(df_ggplot,
                       data.frame(varname="be_2", samples=unname(posterior_samples["be[2]"])),
                       data.frame(varname="be_3", samples=unname(posterior_samples["be[3]"])),
                       data.frame(varname="be_4", samples=unname(posterior_samples["be[4]"])))
  } else {
    df_ggplot <- rbind(df_ggplot,
                       data.frame(varname="be_2", samples=NaN),
                       data.frame(varname="be_3", samples=unname(posterior_samples["be[2]"])),
                       data.frame(varname="be_4", samples=unname(posterior_samples["be[3]"])))
  }
  
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

ggexport(g, filename="./figure/confirmatory_analysis_01_BF.png")

plot(g)

### Additional analysis ###
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