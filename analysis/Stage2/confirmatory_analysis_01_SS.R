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
    lnr_vec[k] = log(sum(exp(sampleloglik - lnC))) + lnC - log(length(sgm_pos))
  }
  
  lnr_list[[i]] <- lnr_vec
  lnZ[i] <- sum(lnr_vec)
  cat(paste(Sys.time(), ": lnZ (i=", i, ") = ", lnZ[i], "\n", sep=""))
}

saveRDS(lnr_list, file=sprintf("./figure_ss/confirmatory_analysis_01_lnr.rds"))
cat(paste(Sys.time(), ": lnZ = ", lnZ[1], " vs. ", lnZ[2], "\n", sep=""))
write.csv(lnZ, "./figure_ss/confirmatory_analysis_01_lnZ.csv", row.names=FALSE)

### Plot results (posterior distributions) ###
library(ggridges)
library(ggplot2)
library(ggpubr)

ggplotlist <- vector(mode="list", length=2)
modelname = c("1", "0a")

for(i in 1:2) {
  df_ggplot <- possamplelist[[i]]
  
  df_ggplot$varname[df_ggplot$varname == 'sgm'] = 'σ'
  df_ggplot$varname[df_ggplot$varname == 's_1'] = 'σ (cohort-level)'
  df_ggplot$varname[df_ggplot$varname == 's_2'] = 'σ (individual-level)'
  df_ggplot$varname[df_ggplot$varname == 'g'] = 'g'
  df_ggplot$varname[df_ggplot$varname == 'be_1'] = 'Intercept'
  df_ggplot$varname[df_ggplot$varname == 'be_2'] = 'Singing'
  df_ggplot$varname[df_ggplot$varname == 'be_3'] = 'Conversation'
  df_ggplot$varname[df_ggplot$varname == 'be_4'] = 'Recitation'
  df_ggplot$varname <- factor(df_ggplot$varname, levels=c('g', 'σ (individual-level)',
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

g <- ggarrange(plotlist=ggplotlist, ncol=2, nrow=1)
g <- annotate_figure(
  g,
  top=text_grob(
    bquote(bold("Model comparison (" * BF[bold("10a")] == .(sprintf("%1.2e", exp(lnZ[1] - lnZ[2]))) * ")")), 
    size=16
  )
)

ggsave(g, dpi=1200, height=6, width=9, filename="./figure/confirmatory_analysis_01_BF.png")

plot(g)

for (i in 1:2) {
  g <- traceplot(modellist[[i]], pars=c("sgm", "s_1", "s_2", "be", "g"), inc_warmup=TRUE, nrow=2)
  figfilename <- paste("./figure/confirmatory_analysis_01_chain_", i, ".png", sep="")
  ggsave(g, dpi=1200, height=6, width=9, filename=figfilename)
}

### Plot results (posterior predictive checks) ###
df_ggplot <- data.frame(
  cond=sapply(1:(2*N), function(x){ifelse(sum(datalist$X[x, 2:4]) == 0, 0, which(datalist$X[x, 2:4] == 1))}),
  y=datalist$y,
  y_H0=rowMeans(ppcsamplelist[[2]]),
  y_H1=rowMeans(ppcsamplelist[[1]]),
  Site=as.factor(datalist$data$site),
  res_H0=rowMeans(ppcsamplelist[[2]]) - datalist$y,
  res_H1=rowMeans(ppcsamplelist[[1]]) - datalist$y
)

condname <- c("Pre-intervention", "Post-intervention (group singing)",
              "Post-intervention (group conversation)", "Post-intervention (group recitation)")

ggplotlist <- vector(mode="list", length=4)
for(j in 1:4) {
  ggplotlist_j <- vector(mode="list", length=2)
  
  ggplotlist_j[[1]] <- ggplot(data=df_ggplot[df_ggplot$cond==(j-1), ], aes(x=y, y=y_H0, color=Site)) + 
    geom_point() + geom_abline(slope=1, intercept=0, linetype="dashed") + xlim(0, 100) + ylim(0, 100) + 
    xlab("Observed score") + ylab("Simulated score (H0)") + 
    theme(axis.title.x=element_text(size = 9), axis.title.y=element_text(size = 9))
  ggplotlist_j[[2]] <- ggplot(data=df_ggplot[df_ggplot$cond==(j-1), ], aes(x=y, y=y_H1, color=Site)) + 
    geom_point() + geom_abline(slope=1, intercept=0, linetype="dashed") + xlim(0, 100) + ylim(0, 100) + 
    xlab("Observed score") + ylab("Simulated score (H1)") + 
    theme(axis.title.x=element_text(size = 9), axis.title.y=element_text(size = 9))
  g <- ggarrange(plotlist=ggplotlist_j, ncol=2, nrow=1, legend="none")
  g <- annotate_figure(g, top=text_grob(paste(condname[j], sep=""), face="bold", size=12))
  
  ggplotlist[[j]] <- g
  ggsave(filename=paste("./figure/confirmatory_analysis_01_PPC_", j, ".png", sep=""),
         plot=ggplotlist[[j]], width=4, height=2)
}

# dummy data to extract legend information
df_dummy <- data.frame(x=1:30, y=1:30,
                       Site=factor(1:30, labels=c("UK [London](1)", "Indonesia [Surakarta]", "Japan [Kanagawa]",
                                                  "Romania [Cluj]", "India [New Delhi]", "New Zealand [Auckland] (1)",
                                                  "New Zealand [Auckland] (2)", "Italy [Padova]", "Italy [Rome]", 
                                                  "Czech Republic [Prague]", "Thailand [Bangkok]", "UK [Reading]",
                                                  "Nigeria [Lagos]", "Germany [Frankfurt]", "Norway [Bergen]",
                                                  "Colombia [Bogotá]", "Canada [Mississauga]", "Österreich [Innsbruck]",
                                                  "UK [Nottingham]", "Australia [Gold Coast]", "UK [London] (2)",
                                                  "USA [Amherst]", "New Zealand [Auckland] (3)", "Hungary [Budapest]",
                                                  "Russia [Yekaterinburg]", "Poland [Poznań]", "Germany [Kaiserslautern]",
                                                  "Korea [Seoul]", "Georgia [Tbilisi]", "Canada [Hamilton]")))
g <- ggplot(data=df_dummy, aes(x=x, y=y, color=Site)) + geom_point() + 
  guides(color=guide_legend(ncol=6, nrow=5, byrow=TRUE))
gl <- as_ggplot(get_legend(g))
ggsave(filename="./figure/confirmatory_analysis_legend.png", plot=gl, width=12, height=4)
plot(gl)