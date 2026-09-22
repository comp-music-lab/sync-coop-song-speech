# Script to validate the equality between the data used for the confirmatory analyses and the exploratory analyses

### Load library ###
library(here)

### Read raw data files ###
rawdatafile_repo <- here('stage2(full)', 'data/raw/stage2data.csv') 
rawdata_repo <- read.csv(rawdatafile_repo, header=TRUE, sep=",")

rawdatafile_anal <- here('stage2(full)', 'scripts/scripts_confirmatory_analysis/data/stage2data_20260314.csv')
rawdata_anal <- read.csv(rawdatafile_anal, header=TRUE, sep=",")

if(nrow(rawdata_repo) == nrow(rawdata_anal)) print('Matching number of records...OK')

### Create key and validate it ###
key_repo <- with(rawdata_repo,
                 paste(group,
                       attention,
                       How.much.do.you.agree.with.the.following.statements......I.trust.the.other.participants.,
                       How.much.do.you.agree.with.the.following.statements.....I.feel.like.I.am.on.the.same.team.with.the.other.participants.,
                       How.much.do.you.agree.with.the.following.statements.....I.think.I.am.similar.to.the.other.participants..,
                       How.much.do.you.agree.with.the.following.statements.....I.feel.strong.ties.to.the.other.participants.,
                       How.much.do.you.agree.with.the.following.statements.....I.have.a.lot.in.common.with.the.other.participants.,
                       How.close.do.you.feel.to.all.the.other.participants....1,
                       How.much.do.you.agree.with.the.following.statements......I.trust.the.other.participants..1,
                       How.much.do.you.agree.with.the.following.statements.....I.feel.like.I.am.on.the.same.team.with.the.other.participants..1,
                       How.much.do.you.agree.with.the.following.statements.....I.think.I.am.similar.to.the.other.participants...1,
                       How.much.do.you.agree.with.the.following.statements.....I.feel.strong.ties.to.the.other.participants..1,
                       How.much.do.you.agree.with.the.following.statements.....I.have.a.lot.in.common.with.the.other.participants..1,
                       How.close.do.you.feel.to.all.the.other.participants....1.1,
                       Location.Latitude,
                       Location.Longitude,
                       Start.Date,
                       End.Date, 
                       User.Language,
                       What.is..are..your.1st.native.language.s..,
                       sep='@'
                       )
)

key_anal <- with(rawdata_anal,
                 paste(group,
                       attention,
                       How.much.do.you.agree.with.the.following.statements......I.trust.the.other.participants.,
                       How.much.do.you.agree.with.the.following.statements.....I.feel.like.I.am.on.the.same.team.with.the.other.participants.,
                       How.much.do.you.agree.with.the.following.statements.....I.think.I.am.similar.to.the.other.participants..,
                       How.much.do.you.agree.with.the.following.statements.....I.feel.strong.ties.to.the.other.participants.,
                       How.much.do.you.agree.with.the.following.statements.....I.have.a.lot.in.common.with.the.other.participants.,
                       How.close.do.you.feel.to.all.the.other.participants....1,
                       How.much.do.you.agree.with.the.following.statements......I.trust.the.other.participants..1,
                       How.much.do.you.agree.with.the.following.statements.....I.feel.like.I.am.on.the.same.team.with.the.other.participants..1,
                       How.much.do.you.agree.with.the.following.statements.....I.think.I.am.similar.to.the.other.participants...1,
                       How.much.do.you.agree.with.the.following.statements.....I.feel.strong.ties.to.the.other.participants..1,
                       How.much.do.you.agree.with.the.following.statements.....I.have.a.lot.in.common.with.the.other.participants..1,
                       How.close.do.you.feel.to.all.the.other.participants....1.1,
                       Location.Latitude,
                       Location.Longitude,
                       Start.Date,
                       End.Date, 
                       User.Language,
                       What.is..are..your.1st.native.language.s..,
                       sep='@'
                 )
)

if(length(unique(key_repo)) == length(key_repo)) print('Key uniqueness...OK (repository file)')
if(length(unique(key_anal)) == length(key_anal)) print('Key uniqueness...OK (analysis file)')

if(length(setdiff(key_anal, key_repo)) + length(setdiff(key_repo, key_anal)) == 0) print('Key matching...OK')