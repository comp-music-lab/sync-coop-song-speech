### Load data ###
datafilename = "keydata_long_20260314.csv"
rawdatafilename = "stage2data_20260314.csv"

source("h_keydata.R")
h_keydata(datafilename, rawdatafilename)

source("h_datalist.R")
datalist <- h_datalist(datafilename, rawdatafilename)

### Run analysis ###
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

set.seed(4)
stanseeds = c(1, 2, 10, 4)
pattern = c(0, 1, 2, 3)
modellist <- vector(mode="list", length=length(pattern))
lnZ = rep(0, length(pattern))
#stanfile = "lmm_linconuip.stan"
stanfile = "lmm_linuip_all_stable_beta_direct.stan"
#stanfile = "lmm_linuip_B.stan"
numwarmup = c(1000, 1000, 1000, 1000)
numiter = c(2000, 20000, 2000, 20000)
numchains = 4

h_lnhalft <- function(x, nu, s) {
  log(2) + log(gamma((nu+1)/2)) - log(gamma(nu/2)) - log(sqrt(nu*pi*s^2)) + (-(nu+1)/2)*log(1 + 1/nu*x^2/s^2)
}

source("h_standata_02.R")
source("h_Lmd.R")
source("h_uipmvn.R")
source("h_mvnlik.R")

# Collect posterior samples based on stepping-stone sampling
K <- 50
be_vec <- rbeta(K, 0.3, 1)
be_vec <- c(0, be_vec)
print(be_vec)

for(i in c(1, 3, 2, 4)) {
  standata <- h_standata(datalist, pattern[i])

#  for(kk in 1:(K + 1)) {
  for(kk in 22:(K + 1)) {
    standata$beta = be_vec[kk]
    
    cat(paste(Sys.time(), ": i = ", i, " (k = ", kk, ") - start\n", sep=""))
    fit_pos <- stan(file = stanfile, data = standata, chains = numchains, seed = stanseeds[i],
                    warmup = numwarmup[i], iter = numiter[i], cores = 4, refresh = 50,
                    include = TRUE, pars = c("sgm", "s_1", "s_2", "be", "g"))
    
    print(fit_pos, pars=c("sgm", "s_1", "s_2", "be", "g"))
    
    # keep posterior samples satisfying constraints
    theta = extract(fit_pos, pars="be", permuted=FALSE, inc_warmup=FALSE)
    idx_cons = sapply(1:numchains, function(k){theta[, k, ] %*% consmat[[i]] > 0})
    N = min(colSums(idx_cons))
    idx_cons_N = sapply(1:numchains, function(k){which(idx_cons[, k])[1:N]})
    
    rhat_theta = data.frame(
      varname=character(), mean=double(), sd=double(), 
      `2.5%`=double(), `25%`=double(), `50%`=double(), `75%`=double(), `97.5%`=double(), rhat=double()
    )
    
    possamplelist = data.frame(varname=character(), samples=double(), chain=integer(), t=integer())
    
    for (k in 1:length(varname)) {
      theta = extract(fit_pos, pars=varname[k], permuted=FALSE, inc_warmup=FALSE)
      theta_N = sapply(1:numchains, function(k){theta[idx_cons_N[, k], k, ]})
      
      rhat_theta = rbind(rhat_theta,
                         data.frame(varame=varname[k], mean=mean(theta_N), sd=sd(theta_N),
                                    `2.5%`= quantile(theta_N, 0.025), `25%`= quantile(theta_N, 0.25), 
                                    `50%`= quantile(theta_N, 0.50), `75%`= quantile(theta_N, 0.75),
                                    `97.5%`= quantile(theta_N, 0.975), rhat=rhat(theta_N))
      )
      
      possamplelist = rbind(
        possamplelist,
        data.frame(varname=varname[k], samples=c(theta_N), chain=rep(1:numchains, each=N), t=rep(1:N, times=numchains))
      )
    }
    
    # print result
    rownames(rhat_theta) <- NULL
    print(rhat_theta)
    
    be_str <- gsub(".", "", sprintf("%0.6f", standata$beta), fixed=TRUE)
    write.csv(rhat_theta, sprintf("./figure_ss/confirmatory_analysis_02_stats_%s_%d.csv", be_str, i), row.names=TRUE)
    
    possamplelist$varname <- sub("[", "_", possamplelist$varname, fixed=TRUE)
    possamplelist$varname <- sub("]", "", possamplelist$varname, fixed=TRUE)
    saveRDS(
      list(beta=standata$beta, possamplelist=possamplelist), 
      file=sprintf("./figure_ss/confirmatory_analysis_02_possample_%s_%d.rds", be_str, i)
    )
    
    cat(paste(Sys.time(), ": i = ", i, " (k = ", kk, ") - end\n", sep=""))
  }
}

# Compute marginal likelihood and Bayes factors
lnZ <- c(0, 0, 0, 0)
lnr_list <- vector(mode="list", length=length(lnZ))

# Standardize number of samples
N_set = matrix(0, nrow=K+1, ncol=4)
for(i in 1:4) {
  possample_files <- list.files(path="./figure_ss/",
                                pattern=sprintf("^confirmatory_analysis_02_possample_.*_%d\\.rds$", i))
  possample_list <- vector(mode="list", length=length(possample_files))
  
  for(k in 1:length(possample_files)) {
    possample_tmp <- readRDS(paste("./figure_ss/", possample_files[k], sep=""))
    N_set[k, i] = sum(possample_tmp[[2]]$varname == "be_1")
  }
}

N_min_1 <- pmin(N_set[, 1], N_set[, 2])
N_min_2 <- pmin(N_set[, 3], N_set[, 4])
N_min <- cbind(cbind(N_min_1, N_min_1), cbind(N_min_2, N_min_2))

# Compute marginal likelihood
for(i in 1:4) {
  # Read posterior samples
  possample_files <- list.files(path="./figure_ss/",
                                pattern=sprintf("^confirmatory_analysis_02_possample_.*_%d\\.rds$", i))
  
  possample_list <- vector(mode="list", length=length(possample_files))
  beta_list <- vector(mode="numeric", length=length(possample_files))
  
  for(k in 1:length(possample_files)) {
    possample_tmp <- readRDS(paste("./figure_ss/", possample_files[k], sep=""))
    beta_list[k] = possample_tmp[[1]]
    possample_list[[k]] = possample_tmp[[2]]
  }
  
  idx <- order(beta_list)
  beta_list <- beta_list[idx]
  possample_list <- possample_list[idx]
  
  # Compute r
  standata <- h_standata(datalist, pattern[i])
  M = standata$M
  X = standata$X
  y = standata$y
  n = standata$n
  p = standata$p
    
  beta_list <- c(beta_list, 1)
  lnr <- vector(mode="numeric", length=length(possample_files))
    
  for(k in 1:length(possample_files)) {
    possample <- possample_list[[k]]
      
    sgm_pos <- possample["samples"][possample["varname"] == "sgm"]
    s_1_pos <- possample["samples"][possample["varname"] == "s_1"]
    s_2_pos <- possample["samples"][possample["varname"] == "s_2"]
    be_pos <- t(sapply(1:p, function(j){possample["samples"][possample["varname"] == paste("be_", j, sep="")]}))
    
    sgm_pos <- sgm_pos[1:N_min[k, i]]
    s_1_pos <- s_1_pos[1:N_min[k, i]]
    s_2_pos <- s_2_pos[1:N_min[k, i]]
    be_pos <- be_pos[, 1:N_min[k, i]]
    
    sampleloglik = (beta_list[k+1] - beta_list[k]) *
      sapply(1:length(sgm_pos), function(j){h_mvnlik(y, X, be_pos[,j], sgm_pos[j], s_1_pos[j], s_2_pos[j], M, n)})
    lnC = max(sampleloglik)
    lnr[k] = log(sum(exp(sampleloglik - lnC))) + lnC - log(length(sgm_pos))
  }
    
  lnr_list[[i]] <- lnr
  lnZ[i] <- sum(lnr)
  cat(paste(Sys.time(), ": lnZ (i=", i, ") = ", lnZ[i], "\n", sep=""))
}

cat(paste(Sys.time(), ": lnZ = ", lnZ[1], " vs. ", lnZ[2], " (K=", length(possample_files), ")\n", sep=""))
cat(paste(Sys.time(), ": lnZ = ", lnZ[3], " vs. ", lnZ[4], " (K=", length(possample_files), ")\n", sep=""))
saveRDS(lnr_list, file=sprintf("./figure_ss/confirmatory_analysis_02_lnr.rds"))
write.csv(lnZ, "./figure_ss/confirmatory_analysis_02_lnZ.csv", row.names=FALSE)