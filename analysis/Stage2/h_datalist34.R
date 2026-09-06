h_datalist34 <- function(hypothesisguessfile, datalist) {
  # read file
  df_hypothesisguess_raw <- read.csv(hypothesisguessfile, header = TRUE)
  
  # filter data with inclusion criteria and hypothesis guess types
  inclusion <- with(df_hypothesisguess_raw, Post_attention_check >= 50 & pre_bonding_score <= 80)
  df_hypothesisguess <- df_hypothesisguess_raw[inclusion, c("X", "Final_agreed_coding", "pre_bonding_score", "post_bonding_score", "Start.Date", "Location.Latitude", "Location.Longitude", "group", "ID")]
  df_hypothesisguess_34 <- df_hypothesisguess[df_hypothesisguess$Final_agreed_coding %in% c(3, 4), ]
  
  # standardize timestamp
  df_hypothesisguess_34$starttime_std <- c(
    format(as.POSIXct(df_hypothesisguess_34$Start.Date[1:13], format="%d/%m/%Y %H:%M"), "%Y-%m-%d %H:%M"),
    format(as.POSIXct(df_hypothesisguess_34$Start.Date[14:nrow(df_hypothesisguess_34)], format="%Y/%m/%d %H:%M"), "%Y-%m-%d %H:%M")
  )
  print(sum(df_hypothesisguess_34$starttime_std == "NA"))
  
  # convert to design matrix-like format
  df_data34 <- with(df_hypothesisguess_34, rbind(
    data.frame(X=X, ID=ID, time="Post_Experiment", score=post_bonding_score,
               starttime_std=starttime_std, latitude=Location.Latitude, longitude=Location.Longitude, group=group),
    data.frame(X=X, ID=ID, time="Pre_Experiment", score=pre_bonding_score,
               starttime_std=starttime_std, latitude=Location.Latitude, longitude=Location.Longitude, group=group)
  ))
  
  # create unique key for each observation
  df_data34$datapointkey <- with(df_data34, paste(time, sprintf("%3.4f", score), starttime_std, latitude, longitude, group, sep=""))
  keyfreq <- table(df_data34$datapointkey)
  print(keyfreq[keyfreq != 1])
  
  # standardize timestamp
  datalist$data$starttime_std <- with(datalist, c(
    format(as.POSIXct(data$starttime[1:42], format="%d/%m/%Y %H:%M"), "%Y-%m-%d %H:%M"),
    format(as.POSIXct(data$starttime[43:nrow(data)], format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M")
  ))
  print(sum(datalist$data$starttime_std == "NA"))
  
  datalist$data$datapointkey <- with(datalist, with(data, paste(time, sprintf("%3.4f", score), starttime_std, latitude, longitude, group, sep="")))
  keyfreq <- table(datalist$data$datapointkey)
  print(keyfreq[keyfreq != 1])
  
  keymatchingtest = sapply(1:nrow(df_data34), function(i) {sum(datalist$data$datapointkey == df_data34$datapointkey[i])})
  print(sum(keymatchingtest != 1))
  
  idx_34 = vector(mode="numeric", length=nrow(datalist$data))
  idx_34 = FALSE
  for(i in 1:nrow(df_data34)) {
    idx_34[datalist$data$datapointkey == df_data34$datapointkey[i]] = TRUE
  }
  idx_34 = which(idx_34)
  
  data_34 = datalist$data[idx_34, ]
  y_34 = datalist$y[idx_34]
  
  cohort_unq = unique(data_34$cohort)
  Z_1 = matrix(0, nrow=length(y_34), ncol=length(cohort_unq))
  for(i in 1:ncol(Z_1)) Z_1[data_34$cohort == cohort_unq[i], i] = 1
  Z_2 = matrix(0, nrow=length(y_34), ncol=length(unique(data_34$Participant)))
  for(i in 1:ncol(Z_2)) Z_2[(2*i-1):(2*i), i] = 1
  Z_34 = cbind(Z_1, Z_2)
  
  N_cond = data.frame(
    song = sapply(unique(data_34$site), function(i){length(unique(data_34$Participant[data_34$site == i & data_34$group == "S"]))}),
    conv = sapply(unique(data_34$site), function(i){length(unique(data_34$Participant[data_34$site == i & data_34$group == "C"]))}),
    reci = sapply(unique(data_34$site), function(i){length(unique(data_34$Participant[data_34$site == i & data_34$group == "R"]))})
  )
  
  datalist_34 <- list(
    X=datalist$X[idx_34, ], y=y_34, Z=Z_34,
    M=ncol(Z_1), N=ncol(Z_2), n=colSums(Z_1), N_cond=N_cond,
    data=data_34,
    N_X=colSums(Z_1)[sapply(1:dim(Z_1)[1], function(i){which(Z_1[i,]==1)})]/2
  )
  
  return(datalist_34)
}