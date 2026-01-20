h_datalist <- function(datafilename, rawdatafilename) {
  data <- read.csv(datafilename, header=TRUE, sep=",")
  rawdata <- read.csv(rawdatafilename, header=TRUE, sep=",")
  
  key_data <- paste(
    data[data$time == "Pre_Experiment", ]$group,   data[data$time == "Pre_Experiment", ]$ID,
    data[data$time == "Pre_Experiment", ]$trust,   data[data$time == "Pre_Experiment", ]$team,
    data[data$time == "Pre_Experiment", ]$similar, data[data$time == "Pre_Experiment", ]$ties,
    data[data$time == "Pre_Experiment", ]$common,  data[data$time == "Pre_Experiment", ]$close, sep=""
  )
  key_rawdata <- paste(
    rawdata$group, rawdata$ID,
    rawdata$How.much.do.you.agree.with.the.following.statements......I.trust.the.other.participants.,
    rawdata$How.much.do.you.agree.with.the.following.statements.....I.feel.like.I.am.on.the.same.team.with.the.other.participants.,
    rawdata$How.much.do.you.agree.with.the.following.statements.....I.think.I.am.similar.to.the.other.participants..,
    rawdata$How.much.do.you.agree.with.the.following.statements.....I.feel.strong.ties.to.the.other.participants.,
    rawdata$How.much.do.you.agree.with.the.following.statements.....I.have.a.lot.in.common.with.the.other.participants.,
    rawdata$How.close.do.you.feel.to.all.the.other.participants....1, sep=""
  )
  
  data$locationinfo = ""
  data$latitude = 0
  data$longitude = 0
  for(i in 1:length(key_data)) {
    j = which(key_rawdata == key_data[i])
    if(length(j) == 1) {
      data$locationinfo[data$Participant == data$Participant[i]] = rawdata$What.city.and.country.are.you.performing.this.experiment.in.[j]
      data$latitude[data$Participant == data$Participant[i]] = rawdata$Location.Latitude[j]
      data$longitude[data$Participant == data$Participant[i]] = rawdata$Location.Longitude[j]
      data$starttime[data$Participant == data$Participant[i]] = rawdata$Start.Date[j]
      data$language[data$Participant == data$Participant[i]] = rawdata$User.Language[j]
    }
  }
  data$site = 0
  data$site[data$Participant %in% data$Participant[1:21]] = 1 # London, UK
  data$site[data$Participant %in% data$Participant[22:47]] = 2 # Surakarta, Indonesia
  data$site[data$Participant %in% data$Participant[48:61]] = 3 # Kanagawa, Japan
  data$site[data$Participant %in% c(data$Participant[62:75], data$Participant[91:98])] = 4 # Cluj-Napoca, România 
  data$site[data$Participant %in% data$Participant[76:90]] = 5 # New Delhi, India
  data$site[data$Participant %in% data$Participant[99:117]] = 6 # Auckland, New Zealand (Chinese cohorts)
  data$site[data$Participant %in% c(data$Participant[118:138], data$Participant[166])] = 7 # Auckland, New Zealand
  data$site[data$Participant %in% data$Participant[139:165]] = 8 # Padova, Italy
  data$site[data$Participant %in% c(data$Participant[167:172], data$Participant[239:243], data$Participant[269:273])] = 9 # Roma, Italy
  data$site[data$Participant %in% data$Participant[173:195]] = 10 # Praha, Česká republika
  data$site[data$Participant %in% data$Participant[196:221]] = 11 # Bangkok, Thailand
  data$site[data$Participant %in% data$Participant[222:238]] = 12 # Reading, UK
  data$site[data$Participant %in% data$Participant[244:268]] = 13 # Lagos, Nigeria
  data$site[data$Participant %in% data$Participant[274:293]] = 14 # Frankfurt, Germany
  data$site[data$Participant %in% data$Participant[294:315]] = 15 # Bergen, Norway
  data$site[data$Participant %in% data$Participant[316:340]] = 16 # Bogotá, Colombia
  data$site[data$Participant %in% data$Participant[341:365]] = 17 # Mississauga, Canada
  data$site[data$Participant %in% c(data$Participant[366:369], data$Participant[411:424], data$Participant[441:449])] = 18 # Innsbruck, Österreich
  data$site[data$Participant %in% c(data$Participant[370:375], data$Participant[380:384], data$Participant[425:429])] = 19 # Nottingham, UK
  data$site[data$Participant %in% c(data$Participant[376:379], data$Participant[405:410], data$Participant[450:454])] = 20 # Gold Coast, Australia
  data$site[data$Participant %in% data$Participant[385:404]] = 21 # London, UK (Russian speakers)
  data$site[data$Participant %in% c(data$Participant[430:440], data$Participant[461:465])] = 22 # Amherst, USA
  data$site[data$Participant %in% c(data$Participant[455:460], data$Participant[466:472], data$Participant[490:495])] = 23 # Auckland, New Zealand (Maori Speakers)
  data$site[data$Participant %in% c(data$Participant[473:489], data$Participant[496:501])] = 24 # Budapest, Hungary
  data$site[data$Participant %in% data$Participant[502:529]] = 25 # Yekaterinburg, Russia
  data$site[data$Participant %in% data$Participant[530:556]] = 26 # Poznań, Poland
  data$site[data$Participant %in% data$Participant[557:577]] = 27 # Kaiserslautern, Germany (Farsi speakers)
  data$site[data$Participant %in% data$Participant[578:603]] = 28 # Seoul, Korea
  data$site[data$Participant %in% data$Participant[604:626]] = 29 # Tbilisi, Georgia
  data$site[data$Participant %in% data$Participant[627:650]] = 30 # Hamilton, Canada
  
  data$cohort = as.numeric(factor(paste(data$site, data$group, sep="")))
  data$idx_song = as.numeric(data$time == "Post_Experiment" & data$group == "S")
  data$idx_conv = as.numeric(data$time == "Post_Experiment" & data$group == "C")
  data$idx_reci = as.numeric(data$time == "Post_Experiment" & data$group == "R")
  data = data[with(data, order(site, group, ID, time)),]
  
  X = as.matrix(data.frame(intercept=1, data$idx_song, data$idx_conv, data$idx_reci))
  y = c(data$score)
  
  Z_1 = matrix(0, nrow=length(y), ncol=length(unique(data$cohort)))
  for(i in 1:ncol(Z_1)) Z_1[data$cohort == i, i] = 1
  Z_2 = matrix(0, nrow=length(y), ncol=length(unique(data$Participant)))
  for(i in 1:ncol(Z_2)) Z_2[(2*i-1):(2*i), i] = 1
  Z = cbind(Z_1, Z_2)
  
  M = ncol(Z_1)
  N = ncol(Z_2)
  n = colSums(Z_1)
  N_cond = data.frame(
    song = sapply(unique(data$site), function(i){length(unique(data$Participant[data$site == i & data$group == "S"]))}),
    conv = sapply(unique(data$site), function(i){length(unique(data$Participant[data$site == i & data$group == "C"]))}),
    reci = sapply(unique(data$site), function(i){length(unique(data$Participant[data$site == i & data$group == "R"]))})
  )
  
  N_X = colSums(Z_1)[sapply(1:dim(Z_1)[1], function(i){which(Z_1[i,]==1)})]/2
  
  return(list(X=X, y=y, Z=Z, M=M, N=N, n=n, N_cond=N_cond, data=data, N_X=N_X))
}