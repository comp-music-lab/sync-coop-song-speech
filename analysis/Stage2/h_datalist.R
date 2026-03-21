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
  data$site[data$Participant %in% c(data$Participant[407:420], data$Participant[437:445])] = 18 # Innsbruck, Österreich
  data$site[data$Participant %in% c(data$Participant[366:371], data$Participant[376:380], data$Participant[421:425])] = 19 # Nottingham, UK
  data$site[data$Participant %in% c(data$Participant[372:375], data$Participant[401:406], data$Participant[446:450])] = 20 # Gold Coast, Australia
  data$site[data$Participant %in% data$Participant[381:400]] = 21 # London, UK (Russian speakers)
  data$site[data$Participant %in% c(data$Participant[426:436], data$Participant[457:461])] = 22 # Amherst, USA
  data$site[data$Participant %in% c(data$Participant[451:456], data$Participant[462:468], data$Participant[486:491])] = 23 # Auckland, New Zealand (Maori Speakers)
  data$site[data$Participant %in% c(data$Participant[469:485], data$Participant[492:497])] = 24 # Budapest, Hungary
  data$site[data$Participant %in% data$Participant[498:525]] = 25 # Yekaterinburg, Russia
  data$site[data$Participant %in% data$Participant[526:548]] = 26 # Poznań, Poland
  data$site[data$Participant %in% data$Participant[549:569]] = 27 # Kaiserslautern, Germany (Farsi speakers)
  data$site[data$Participant %in% data$Participant[570:595]] = 28 # Seoul, Korea
  data$site[data$Participant %in% data$Participant[596:618]] = 29 # Tbilisi, Georgia
  data$site[data$Participant %in% data$Participant[619:642]] = 30 # Hamilton, Canada
  data$site[data$Participant %in% data$Participant[653:675]] = 31 # Jyväskylä, Finland
  data$site[data$Participant %in% c(data$Participant[676:688], data$Participant[717:726])] = 32 # Saint Étienne, France
  data$site[data$Participant %in% c(data$Participant[643:652], data$Participant[689:693])] = 33 # Boston, United States
  data$site[data$Participant %in% data$Participant[694:709]] = 34 # Colombo, Sri Lanka
  data$site[data$Participant %in% c(data$Participant[710:716], data$Participant[727:741])] = 35 # Nijmegen, Nederland
  data$site[data$Participant %in% c(data$Participant[742:757], data$Participant[851:857])] = 36 # Ano Liosia, Greece
  data$site[data$Participant %in% c(data$Participant[758:784])] = 37 # Aarhus, Danmark
  data$site[data$Participant %in% c(data$Participant[785:800])] = 38 # Plovdiv, Bulgaria
  data$site[data$Participant %in% c(data$Participant[801:825], data$Participant[858:858])] = 39 # Accra, Ghana
  data$site[data$Participant %in% c(data$Participant[826:832], data$Participant[887:896])] = 40 # Wien, Austria
  data$site[data$Participant %in% c(data$Participant[833:850])] = 41 # Ann Arbor, USA
  data$site[data$Participant %in% c(data$Participant[859:875])] = 42 # Pucallpa, Perú
  data$site[data$Participant %in% c(data$Participant[876:886], data$Participant[897:901])] = 43 # Durban, South Africa
  
  data$idx_song = as.numeric(data$time == "Post_Experiment" & data$group == "S")
  data$idx_conv = as.numeric(data$time == "Post_Experiment" & data$group == "C")
  data$idx_reci = as.numeric(data$time == "Post_Experiment" & data$group == "R")
  data = data[with(data, order(site, group, ID, time)),]
  cohortname = paste(data$site, data$group, sep="")
  data$cohort = as.numeric(factor(cohortname, levels = unique(cohortname)))
  
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