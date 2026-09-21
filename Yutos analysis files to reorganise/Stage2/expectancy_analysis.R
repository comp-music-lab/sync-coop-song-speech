### Load library ###
library(ggplot2)

### Load data ###
df_rawdata <- read.csv("./mv2_cleaned_coding_merged.csv", header = TRUE)

### Data preparation ###
df_data_full <- df_rawdata[, c("X", "group", "ID", "pre_bonding_score", "post_bonding_score", 
                          "bonding_post_difference", "Final_agreed_coding",
                          "Pre_bonding_trust", "Pre_bonding_sameteam", "Pre_bonding_similar",
                          "Pre_bonding_ties", "Pre_bonding_common", "Pre_bonding_closeness",
                          "Post_bonding_trust", "Post_bonding_sameteam", "Post_bonding_similar",
                          "Post_bonding_ties", "Post_bonding_common", "Post_bonding_closeness",
                          "Start.Date", "End.Date", "Language", "City_experiment", "Location",
                          "Location.Latitude", "Location.Longitude", "User.Language", "Post_attention_check")
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
inclusion <- with(df_data_full, Post_attention_check >= 50 & pre_bonding_score <= 80)
df_data <- df_data_full[inclusion, ]

### Print frequency statistics ###
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

### Visualize ###
group_colors <- c("SB (H1)" = "#FF2400", "SB (H2)" = "#E0015F",
                  "SB (Others)" = "#FF6347", "Non-SB" = "#0041C2")

g5 <- ggplot(df_data[df_data$Final_agreed_coding != "NA", ], aes(x=bonding_post_difference)) + 
  facet_wrap(~group, nrow=1) + 
  geom_density(aes(group=Final_agreed_coding, colour=Final_agreed_coding), bw="nrd") + 
  labs(x = "Bonding score difference", y = "Probability density", colour = "Guessed hypothesis") + 
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  scale_color_manual(values = group_colors)

plot(g5)

ggsave("./figure/expectancy_analysis_postscorediff.png", plot=g5, width=6, height=3)