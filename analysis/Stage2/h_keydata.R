h_keydata <- function(datafilename, rawdatafilename) {
  rawdata <- read.csv(rawdatafilename, header=TRUE, sep=",")
  
  keydata_pre <- data.frame(
    Participant = -1,
    group = rawdata$group,
    ID = rawdata$ID,
    trust = rawdata$How.much.do.you.agree.with.the.following.statements......I.trust.the.other.participants.,
    team = rawdata$How.much.do.you.agree.with.the.following.statements.....I.feel.like.I.am.on.the.same.team.with.the.other.participants.,
    similar = rawdata$How.much.do.you.agree.with.the.following.statements.....I.think.I.am.similar.to.the.other.participants..,
    ties = rawdata$How.much.do.you.agree.with.the.following.statements.....I.feel.strong.ties.to.the.other.participants.,
    common = rawdata$How.much.do.you.agree.with.the.following.statements.....I.have.a.lot.in.common.with.the.other.participants.,
    close = rawdata$How.close.do.you.feel.to.all.the.other.participants....1,
    attention = rawdata$attention,
    time = "Pre_Experiment"
  )
  
  keydata_pre$score <- with(keydata_pre, (trust + team + similar + ties + common + close)/6)
  keydata_pre$Participant <- 1:nrow(keydata_pre)
  
  keydata_post <- data.frame(
    Participant = -1,
    group = rawdata$group,
    ID = rawdata$ID,
    trust = rawdata$How.much.do.you.agree.with.the.following.statements......I.trust.the.other.participants..1,
    team = rawdata$How.much.do.you.agree.with.the.following.statements.....I.feel.like.I.am.on.the.same.team.with.the.other.participants..1,
    similar = rawdata$How.much.do.you.agree.with.the.following.statements.....I.think.I.am.similar.to.the.other.participants...1,
    ties = rawdata$How.much.do.you.agree.with.the.following.statements.....I.feel.strong.ties.to.the.other.participants..1,
    common = rawdata$How.much.do.you.agree.with.the.following.statements.....I.have.a.lot.in.common.with.the.other.participants..1,
    close = rawdata$How.close.do.you.feel.to.all.the.other.participants....1.1,
    attention = rawdata$attention,
    time = "Post_Experiment"
  )
  
  keydata_post$score <- with(keydata_post, (trust + team + similar + ties + common + close)/6)
  keydata_post$Participant <- 1:nrow(keydata_post)
  
  inclusion <- with(keydata_pre, attention >= 50 & score <= 80)
  keydata <- rbind(keydata_pre[inclusion, ], keydata_post[inclusion, ])
  keydata <- keydata[with(keydata, !is.na(score) & !is.nan(score) & !is.null(score) & is.finite(score)), ]
  
  write.csv(keydata, datafilename, row.names=TRUE)
}