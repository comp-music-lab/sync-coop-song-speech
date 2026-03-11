# Frequentist analysis

### Load data ###
datafilename = "keydata_long_20260118.csv"
rawdatafilename = "stage2data_20260118.csv"

source("h_keydata.R")
h_keydata(datafilename, rawdatafilename)

source("h_datalist.R")
datalist <- h_datalist(datafilename, rawdatafilename)

# Create data frame to feed data to models
df = data.frame(y = datalist$y, x_song = datalist$X[,2], x_conv = datalist$X[,3],
                x_reci = datalist$X[,4],
                z_coho = sapply(1:dim(datalist$Z)[1], function(i){which(datalist$Z[i,1:datalist$M] == 1)}),
                z_indi = sapply(1:dim(datalist$Z)[1], function(i){which(datalist$Z[i,(datalist$M+1):(datalist$M+datalist$N)] == 1)})
)

df$cond <- factor(
  ifelse(df$x_song == 1, "song", ifelse(df$x_conv == 1, "conv", ifelse(df$x_reci == 1, "reci", "pre"))),
  levels = c("pre", "song", "conv", "reci")
)

# Maximum likelihood-based estimation of the linear mixed model of 1st confirmatory analysis
library(lme4)
fa1 <- lmer(y ~ cond + (1|z_coho) + (1|z_indi), data=df)
fa1info <- summary(fa1)
print(fa1info)
print(confint(fa1))

# y ~ x_song + x_conv + x_reci + (1|z_coho) + (1|z_indi)

# Wald-test approach for the 2nd confirmatory analysis
library(emmeans)

ct <- contrast(
  emmeans(fa1, "cond"),
  list(song_minus_conv = c(0, 1, -1, 0), song_minus_reci = c(0, 1, 0, -1))
)

print(test(ct, side = ">"))

'
β_song <- fa1info$coefficients["condsong", "Estimate"]
β_conv <- fa1info$coefficients["condconv", "Estimate"]
β_reci <- fa1info$coefficients["condreci", "Estimate"]
se_song <- fa1info$coefficients["condsong", "Std. Error"]
se_conv <- fa1info$coefficients["condconv", "Std. Error"]
se_reci <- fa1info$coefficients["condreci", "Std. Error"]
δ_songconv <- β_song - β_conv
se_δ_songconv <- sqrt(se_song^2 + se_conv^2)
z_songconv <- δ_songconv/se_δ_songconv
p_songconv <- 1 - pnorm(z_songconv)
δ_songreci <- β_song - β_reci
se_δ_songreci <- sqrt(se_song^2 + se_reci^2)
z_songreci <- δ_songreci/se_δ_songreci
p_songreci <- 1 - pnorm(z_songreci)
print(c(p_songconv, p_songreci))
'

# CLME approach
'
library(CLME)

model_mats <- model_terms_clme(y ~ cond + (1|z_coho) + (1|z_indi), data=df)
X1 = model_mats$X1[, c(2, 3)]
X2 = model_mats$X1[, c(1, 4)]
cons = list(A=matrix(c(1, 2), ncol=2))
clme_em_mixed(Y=model_mats$Y, X1=X1, X2=X2, U=model_mats$U, constraints=cons)
'