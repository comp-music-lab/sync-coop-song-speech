### Load data ###
datafilename = "keydata_long_20260314.csv"
rawdatafilename = "stage2data_20260314.csv"

source("h_keydata.R")
h_keydata(datafilename, rawdatafilename)

source("h_datalist.R")
datalist <- h_datalist(datafilename, rawdatafilename)

### data sanity check ###
participantkey <- with(datalist, with(data, paste(site, group, ID, sep='')))
keyfreq <- table(participantkey)
print(keyfreq[keyfreq != 2])

### Run analysis ###
library(rstan)
library(rpart)
library(posterior)

rseed = 300;
modellist <- vector(mode="list", length=2)
stanfile = c("lmm_ssnlauip.stan", "lmm_ssauip.stan")
model_H0 = c(0, 1)
q = 2
numchains = 6

h_lnhalft <- function(x, nu, s) {
  log(2) + log(gamma((nu+1)/2)) - log(gamma(nu/2)) - log(sqrt(nu*pi*s^2)) + (-(nu+1)/2)*log(1 + 1/nu*x^2/s^2)
}

source("h_standata_01.R")
source("h_Lmd.R")
source("h_nlmvn.R")
source("h_uipmvn.R")
source("h_mvnlik.R")

# Collect posterior samples based on stepping-stone sampling
K <- 50
be_vec <- rbeta(K, 0.3, 1)
be_vec <- c(0, be_vec)

for(i in 1:2) {
  standata <- h_standata(datalist, model_H0[i], q)
  
  for(k in 1:(K + 1)) {
    standata$beta = be_vec[k]

    cat(paste(Sys.time(), ": i = ", i, " (k = ", k, ") - start\n", sep=""))
    fit_pos <- stan(file = stanfile[i], data = standata, chains = numchains, 
                    warmup = 1000, iter = 2000, cores = 4, refresh = 0,
                    control = list(adapt_gamma=0.05, adapt_kappa=0.75, adapt_t0=10, adapt_delta=0.80, max_treedepth=10, adapt_term_buffer=50))
  
    print(fit_pos, pars=c("sgm", "s_1", "s_2", "be", "g"))

    # Gather posterior samples via chain stacking
    source("chain_stacking.R")
    stan_model_object = stan_model("stacking_opt.stan")
    stack_obj = chain_stack(fits=fit_pos, lambda=1.0001, log_lik_char="log_lik")
    print(stack_obj$chain_weights)
      
    sgm_pos = t(mixture_draws(individual_draws=drop(extract(fit_pos, permuted=FALSE, pars="sgm")), weight=stack_obj$chain_weights, random_seed=rseed))
    s_1_pos = t(mixture_draws(individual_draws=drop(extract(fit_pos, permuted=FALSE, pars="s_1")), weight=stack_obj$chain_weights, random_seed=rseed))
    s_2_pos = t(mixture_draws(individual_draws=drop(extract(fit_pos, permuted=FALSE, pars="s_2")), weight=stack_obj$chain_weights, random_seed=rseed))
    r_pos = t(mixture_draws(individual_draws=drop(extract(fit_pos, permuted=FALSE, pars="r")), weight=stack_obj$chain_weights, random_seed=rseed))

    be_pos = extract(fit_pos, permuted=FALSE, pars="be")
    be_pos = t(sapply(1:standata$p, function(i) {mixture_draws(individual_draws=be_pos[,,i], weight=stack_obj$chain_weights, random_seed=rseed)}))
   
    # Format samples in data frame
    possamplelist = rbind(
      data.frame(varname="sgm", samples=t(sgm_pos)),
      data.frame(varname="s_1", samples=t(s_1_pos)),
      data.frame(varname="s_2", samples=t(s_2_pos)),
      data.frame(varname="be_1", samples=be_pos[1, ]),
      data.frame(varname="g", samples=1/t(r_pos) - 1)
    )
    
    if(model_H0[i] == 0) {
      possamplelist = rbind(
        possamplelist,
        data.frame(varname="be_2", samples=be_pos[2, ]),
        data.frame(varname="be_3", samples=be_pos[3, ]),
        data.frame(varname="be_4", samples=be_pos[4, ])
      )
    } else {
      possamplelist = rbind(
        possamplelist,
        data.frame(varname="be_2", samples=NaN),
        data.frame(varname="be_3", samples=be_pos[2, ]),
        data.frame(varname="be_4", samples=be_pos[3, ])
      )
    }
    
    # Re-calculate Rhat with resampled samples via model stacking
    rhat_theta = sapply(c("sgm", "s_1", "s_2", "be_1", "be_2", "be_3", "be_4", "g"),
                             function(varname) {
                               c(mean(possamplelist$samples[possamplelist$varname == varname]),
                                 sd(possamplelist$samples[possamplelist$varname == varname]),
                                 quantile(possamplelist$samples[possamplelist$varname == varname], c(0.025, 0.25, 0.50, 0.75, 0.975), na.rm=TRUE),
                                 rhat(possamplelist$samples[possamplelist$varname == varname])
                               )
                             }
    )
    rhat_theta <- t(rhat_theta)
    colnames(rhat_theta)[c(1, 2, 8)] <- c("mean", "sd", "Rhat")
    print(rhat_theta)
    
    be_str <- gsub(".", "", sprintf("%0.4f", standata$beta), fixed=TRUE)
    write.csv(rhat_theta, sprintf("./figure_ss/confirmatory_analysis_01_stats_%s_%d.csv", be_str, i), row.names=TRUE)
    saveRDS(
      list(beta=standata$beta, possamplelist=possamplelist), 
      file=sprintf("./figure_ss/confirmatory_analysis_01_possample_%s_%d.rds", be_str, i)
    )
    
    cat(paste(Sys.time(), ": i = ", i, " (k = ", k, ") - end\n", sep=""))
  }
}

# Compute marginal likelihood and Bayes factors
lnr_list <- vector(mode="list", length=2)
lnZ <- c(0, 0)

for(i in 1:2) {
  # Read posterior samples
  possample_files <- list.files(path="./figure_ss/",
                                pattern=sprintf("^confirmatory_analysis_01_possample_.*_%d\\.rds$", i))
  
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
  standata <- h_standata(datalist, model_H0[i], q)
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
    
    if(model_H0[i] == 0) {
      be_pos <- t(sapply(1:p, function(j){possample["samples"][possample["varname"] == paste("be_", j, sep="")]}))
    } else {
      be_pos <- t(sapply(setdiff(1:(p + length(q)), q), function(j){possample["samples"][possample["varname"] == paste("be_", j, sep="")]}))
    }
    
    sampleloglik = (beta_list[k+1] - beta_list[k]) *
      sapply(1:length(sgm_pos), function(j){h_mvnlik(y, X, be_pos[,j], sgm_pos[j], s_1_pos[j], s_2_pos[j], M, n)})
    lnC = max(sampleloglik)
    lnr[k] = log(sum(exp(sampleloglik - lnC))) + lnC - log(length(sgm_pos))
  }
  
  lnr_list[[i]] <- lnr
  lnZ[i] <- sum(lnr)
  cat(paste(Sys.time(), ": lnZ (i=", i, ") = ", lnZ[i], "\n", sep=""))
}

saveRDS(lnr_list, file=sprintf("./figure_ss/confirmatory_analysis_01_lnr.rds"))
cat(paste(Sys.time(), ": lnZ = ", lnZ[1], " vs. ", lnZ[2], "\n", sep=""))
write.csv(lnZ, "./figure_ss/confirmatory_analysis_01_lnZ.csv", row.names=FALSE)