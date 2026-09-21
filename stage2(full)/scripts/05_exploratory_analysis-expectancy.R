### Load library ###
library(ggplot2)

### Load data ###
datafilepath = "../data/processed/"
df_rawdata <- read.csv(
  paste(datafilepath, "mv2_cleaned_coding_merged.csv", sep=""),
  header = TRUE)

### Data preparation ###
# Extract necessary columns and assign factors to help visualization
df_data_full <- df_rawdata[, c("Final_agreed_coding", "group",
                               "Post_attention_check", "pre_bonding_score")
                           ]

df_data_full$Final_agreed_coding <- factor(df_data_full$Final_agreed_coding,
                                           levels = c(1, 2, 3, 4, 5),
                                           labels = c("SB (H1)", "SB (H2)", "SB (Others)", "Non-SB", "NA")
                                           )

df_data_full$group <- factor(df_data_full$group, 
                            levels = c("S", "C", "R"),
                            labels = c("Singing", "Conversation", "Recitation")
                            )

### Inclusion criteria ###
# Keep data only satisfying the inclusion criteria
inclusion <- with(df_data_full, Post_attention_check >= 50 & pre_bonding_score <= 80)
df_data <- df_data_full[inclusion, ]

### Print frequency statistics ###
# Table 2
freqstat <- t(
  rbind(
    sweep(with(df_data, table(group, Final_agreed_coding)), MARGIN=1, STATS=with(df_data, table(group)), FUN="/"),
    ALL = with(df_data, table(Final_agreed_coding))/nrow(df_data)
  )
)

print(
  matrix(sprintf("%.1f%%", freqstat*100), nrow=nrow(freqstat), dimnames=dimnames(freqstat)),
  quote=FALSE
)

print(
  c(with(df_data, table(group)), ALL=nrow(df_data))
)