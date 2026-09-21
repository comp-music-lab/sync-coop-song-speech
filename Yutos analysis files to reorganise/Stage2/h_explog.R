h_explog <- function(datafilename, rawdatafilename) {
  explog <- read.csv(explogfilename, header=TRUE, sep=",")
  
  explog <- explog[!(explog$Experimenter.name == "Florence Nweke" & explog$Start.Date == "2025-07-07 02:48:05"), ]
  
  explog$cond <- ifelse(explog$Experiment.condition == "Singing first", 1,
                        ifelse(explog$Experiment.condition == "Conversation first", 2,
                               ifelse(explog$Experiment.condition == "Recitation first", 3, -1)))
  
  explog$site = -1
  explog$site[explog$Experimenter.name == "Jiawen Huang"] = 1
  explog$site[explog$Experimenter.name == "Uswatun Khasanah"] = 2
  explog$site[explog$Experimenter.name == "Gakuto Chiba"] = 3
  explog$site[explog$Experimenter.name == "Simion Echim"] = 4
  explog$site[explog$Experimenter.name == "Dhwani P Sadaphal"] = 5
  explog$site[explog$Experimenter.name == "Zixuan Jia"] = 6
  explog$site[explog$Experimenter.name %in% c("Nelson shi", "Nelson Shi", "Nelson Shj" )] = 7
  explog$site[explog$Experimenter.name == "Lucrezia Guiotto Nai Fovino"] = 8
  explog$site[explog$Experimenter.name %in% c("Flavia Arnese Chloé Coissac", "Flavia Chloe")] = 9
  explog$site[explog$Experimenter.name %in% c("Jan Hajič")] = 10
  explog$site[explog$Experimenter.name %in% c("Prapatsorn (Anne) Tiratanti", "Prapatsorn Tiratanti")] = 11
  explog$site[explog$Experimenter.name %in% c("YE HE")] = 12
  explog$site[explog$Experimenter.name %in% c("Florence Nweke")] = 13
  explog$site[explog$Experimenter.name %in% c("Neda Mousavi")] = 14
  explog$site[explog$Experimenter.name %in% c("Ulvhild Færøvik")] = 15
  explog$site[explog$Experimenter.name %in% c("Jeronimo Quintero Martinez")] = 16
  explog$site[explog$Experimenter.name %in% c("Anne Cabildo")] = 17
  explog$site[explog$Experimenter.name %in% c("Francesca Talamini")] = 18
  explog$site[explog$Experimenter.name %in% c("Chloe")] = 19
  explog$site[explog$Experimenter.name %in% c("Lee Wolff")] = 20
  explog$site[explog$Experimenter.name %in% c("Polina Proutskova")] = 21
  explog$site[explog$Experimenter.name %in% c("Adwoa Ampiah-Bonney")] = 22
  explog$site[explog$Experimenter.name %in% c("Danya")] = 23
  explog$site[explog$Experimenter.name %in% c("Csaba Kertész")] = 24
  explog$site[explog$Experimenter.name %in% c("Alexandra Kosachenko", "Alexandra")] = 25
  explog$site[explog$Experimenter.name %in% c("Wojtek Krzyżanowski")] = 26
  explog$site[explog$Experimenter.name %in% c("Shahaboddin Dabaghi Varnosfaderani")] = 27
  explog$site[explog$Experimenter.name %in% c("Inkuk Kim")] = 28
  explog$site[explog$Experimenter.name %in% c("Teona Lomsadze")] = 29
  explog$site[explog$Experimenter.name %in% c("Sara Ripley and Katerine Bilous")] = 30
  explog$site[explog$Experimenter.name %in% c("Joshua Bamford")] = 31
  explog$site[explog$Experimenter.name %in% c("Aitana Garcia", "Aitana Garcia Arasco")] = 32
  explog$site[explog$Experimenter.name %in% c("Kaye Han")] = 33
  explog$site[explog$Experimenter.name %in% c("Ruwin Rangeeth Dias", "RUWIN Rangeeth Dias")] = 34
  explog$site[explog$Experimenter.name %in% c("Remi van Casteren")] = 35
  explog$site[explog$Experimenter.name %in% c("Sotirios Kolios")] = 36
  explog$site[explog$Experimenter.name %in% c("Niels Chr. Hansen")] = 37
  explog$site[explog$Experimenter.name %in% c("Dilyana Kurdova, Zoya Mikova", "Dilyana Kurdova")] = 38
  explog$site[explog$Experimenter.name %in% c("Adwoa Arhine")] = 39
  explog$site[explog$Experimenter.name %in% c("Felix Haiduk")] = 40
  explog$site[explog$Experimenter.name %in% c("Yiqing Ma")] = 41
  explog$site[explog$Experimenter.name %in% c("Roberto Zariquiey")] = 42
  explog$site[explog$Experimenter.name %in% c("Nozuko Nguqu")] = 43
  
  return(explog)
}