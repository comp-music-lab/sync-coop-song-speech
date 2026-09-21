#Load and pre-process data (NB: Full raw files with pilot participant data not shared publicly, just shown for transparency, but commented out using "#")

#Load and clean experimenter reports (used to help exclude pilot data below):
e<-read_csv(file='/Users/psav050/Documents/Research/Publications/Accepted/Jia Ozaki Pavlovich et al (2025) PCI-RR Many Voices 2/MV2 real data/Many Voices 2 post-experiment survey (for experimenter)_March 11, 2026_06.53.csv')#import from main Qualtrics account output - not shared publicly
colnames(e)<-e[1,] #change column names to make clear what they are
e<-e[-c(1:2),] #remove non-data rows
e$`IP Address`="NA"#Remove IP address data
e$`Response ID`="NA"#Remove Response ID data
e<-e[-c(1:3,7:9,13:16,25,30,37:38,43,51,54,58,61:65,68:70,72:74,81,89,99,108,121,126,128,131,135,144,148,150,152,156,160,164),]#Exclude pilot experiments (change from hard-coding?)
e[78,23]<-"9" #correct experimenter entry error
e[107,22]<-"Singing first" #correct experimenter entry error
e[114,24]<-"90" #correct experimenter entry error
e[115,24]<-"90" #correct experimenter entry error
e[116,24]<-"90" #correct experimenter entry error

write.csv(e,file = file.path(OUTPUTDIR,'experimentlog.csv')) #Write processed experiment log data file to data/raw directory

#Load participant data
df<-read_csv(file='/Users/psav050/Documents/Research/Publications/Accepted/Jia Ozaki Pavlovich et al (2025) PCI-RR Many Voices 2/MV2 real data/SpeechSong_London_Chinese_March 21, 2025_20.44_unifiedrows.csv') #import from separate Qualtrics account output - not shared publicly
df<-df[-c(12,13),] #Exclude duplicated participant data (change from hard-coding?)
d<-read_csv(file='/Users/psav050/Documents/Research/Publications/Accepted/Jia Ozaki Pavlovich et al (2025) PCI-RR Many Voices 2/MV2 real data/song_March 11, 2026_06.53.csv')#import from main Qualtrics account output - not shared publicly
d<-d[-c(1:244,276:304,320:322,384:392,407,417:421,451:453,455:467,474,491:493,501:514,542:546,566:572,579:591,611,614,623:624,631:718,726:742,758:782,788:795,815:859,877:878,887:889,916:917,928,934,949,971:972,988:990,1016:1020,1026,1033:1035,1042:1048,1057:1067,1085:1091,1098,1105:1106,1127,1137:1141,1137:1155,1184:1187,1212:1216,1245:1246,1271:1286,1311,1317:1320,1326:1329,1330:1333,1344:1347,1375:1387,1428,1439:1440,1450:1453,1462:1466,1533:1559,1597:1606,1626,1628,1636:1647,1649:1650),]#Exclude pilot data (change from hard-coding?)
d[18,19]="R" #fix participant entry error 
d[1,20]="10" #fix participant entry error
d[14,20]="6" #fix participant entry error
d[36,20]="2" #fix participant entry error
d[43,20]="2" #fix participant entry error
d[114,20]="1" #fix participant entry error
d[116,20]="6" #fix participant entry error
d[118,20]="2" #fix participant entry error
d[123,20]="1" #fix participant entry error
d[124,20]="8" #fix participant entry error
d[160,20]="5" #fix participant entry error
d[217,20]="4" #fix participant entry error
d[222,20]="1" #fix participant entry error
d[250,20]="R10" #fix participant entry error
d[256,20]="S2" #fix participant entry error
d[261,20]="C1" #fix participant entry error
d[267,20]="C5" #fix participant entry error
d[270,20]="C4" #fix participant entry error
d[323,20]="3Bog03" #fix participant entry error
d[326,20]="3Bog04" #fix participant entry error
d[349,19]="S" #fix participant entry error
d[349,20]="9" #fix participant entry error
d[351,20]="2" #fix participant entry error
d[353,20]="6" #fix participant entry error
d[354,20]="3" #fix participant entry error
d[357,20]="11" #fix participant entry error
d[368,20]="25" #fix participant entry error
d[373,20]="26" #fix participant entry error
d[416,37]="73" #fix participant entry error
d[440,20]="3" #fix participant entry error
d[450,19]="R" #fix participant entry error
d[451,19]="R" #fix participant entry error
d[452,19]="R" #fix participant entry error
d[453,19]="R" #fix participant entry error
d[454,19]="R" #fix participant entry error
d[479,19]="C" #fix participant entry error
d[520,19]="C" #fix participant entry error
d[524,19]="C" #fix participant entry error
d[534,19]="S" #fix participant entry error
d[538,20]="21" #fix participant entry error
d[567,20]="4" #fix participant entry error 
d[577,20]="C7" #fix participant entry error
d[579,20]="C8" #fix participant entry error
d[591,20]="S8" #fix participant entry error
d[592,20]="S9" #fix participant entry error
d[593,20]="S5" #fix participant entry error
d[627,20]="7" #fix participant entry error
d[657,20]="07" #fix participant entry error
d[673,20]="08" #fix participant entry error
d[760,20]="6" #fix participant entry error
d[829,20]="27" #fix participant entry error
d[843,20]="R3" #fix participant entry error
d[853,20]="S3" #fix participant entry error
d[856,20]="S6" #fix participant entry error
d[858,20]="S8" #fix participant entry error
d[866,19]="C" #fix participant entry error
d[911,20]="5" #fix participant entry error
d[916,19]="R" #fix participant entry error
d[928,19]="S" #fix participant entry error
d[937,19]="C" #fix participant entry error
d[949,20]="4" #fix participant entry error
d[950,20]="5" #fix participant entry error
d[955,20]="5[duplicate to exclude]" #fix participant entry error
d[976,20]="10" #fix participant entry error
d[985,20]="6" #fix participant entry error

d<-d[-c(21,74,130,277,374:377,394:395,571:574,795,884,925,939),] #Exclude duplicated/incomplete/pilot participant data (change from hard-coding?)
d<- subset(d, `Consent`=="Yes - I want to continue on to the study questions")
df<-rbind(df,d)
colnames(df)<-df[1,] #change column names to make clear what they are

#Rename with shorter variable names
names(df)[names(df) == 'What is the "Group ID" listed on the screen?'] <- 'group' 
names(df)[names(df) == "What is the \"Participant ID\" number listed on your consent form?"] <- 'ID'
names(df)[names(df) == "How much do you agree with the following statements? - Please rate “strongly agree” to show you are paying attention"] <- 'attention'
df<-df[-c(1:2),] #remove non-data rows
df<- subset(df, `Response Type`=="IP Address") #Remove test ("Preview") responses
df$`IP Address`="NA"#Remove IP address data
df$`Response ID`="NA"#Remove Response ID data
write.csv(df,file = file.path(OUTPUTDIR,'stage2data.csv')) #Write processed participant data file to data/raw directory