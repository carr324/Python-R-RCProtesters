require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(lmerTest)
require(extrafont)

rawData <- read.csv("rcblm2_data_10212017.csv")

responseData <- rawData %>%
  filter(Finished == 1, data_use != 1, data_use != 2) %>%
  select(ResponseId, da_con_trust:data_use) %>%
  gather(TrialType, Choice, da_con_trust:ra_lib_threat) %>%
  arrange(ResponseId, TrialType)

for (i in 1:nrow(responseData)) {
  responseData$Label[i] <- str_split(responseData$Trial[i], "_")[[1]][1]
  responseData$PolAff[i] <- str_split(responseData$Trial[i], "_")[[1]][2]
  responseData$RatingDim[i] <- str_split(responseData$Trial[i], "_")[[1]][3]
}

responseData$Label[responseData$Label=="ra"] <- "Racial Activist"
responseData$Label[responseData$Label=="da"] <- "Diversity Advocate"

responseData$PolAff[responseData$PolAff=="con"] <- "Conservative"
responseData$PolAff[responseData$PolAff=="lib"] <- "Liberal"

responseData$RatingDim[responseData$RatingDim=="emo"] <- "Emotion (1 = Angry, 9 = Happy)"
responseData$RatingDim[responseData$RatingDim=="gender"] <- "Gender (1 = Male, 9 = Female)"
responseData$RatingDim[responseData$RatingDim=="race"] <- "Race (1 = Black, 9 = White)"
responseData$RatingDim[responseData$RatingDim=="threat"] <- "Threat (1 = None, 9 = All)"
responseData$RatingDim[responseData$RatingDim=="trust"] <- "Trust (1 = None, 9 = All)"

summaryData <- group_by(responseData, Label, PolAff, RatingDim) %>%
  summarize(
    AvgChoice = mean(Choice),
    SDChoice = sd(Choice),
    SEMChoice = sd(Choice)/sqrt(length(unique(responseData$ResponseId)))
  )
summaryData$SEMLower <- summaryData$AvgChoice - summaryData$SEMChoice
summaryData$SEMUpper <- summaryData$AvgChoice + summaryData$SEMChoice

responseData$Label <- as.factor(responseData$Label)
responseData$PolAff <- as.factor(responseData$PolAff)
responseData$RatingDim <- as.factor(responseData$RatingDim)

mlm_emo <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Emotion (1 = Angry, 9 = Happy)")) 
mlm_gender <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Gender (1 = Male, 9 = Female)")) 
mlm_race <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Race (1 = Black, 9 = White)")) 
mlm_threat <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Threat (1 = None, 9 = All)")) 
mlm_trust <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Trust (1 = None, 9 = All)")) 

ggplot(data = summaryData, aes(x = RatingDim, y = AvgChoice, color = Label)) +
  geom_pointrange(aes(ymin = SEMLower, ymax = SEMUpper), size = 1) +
  theme_classic() +
  coord_flip() +
  facet_wrap("PolAff") +
  scale_color_grey() +
  # scale_y_continuous(breaks = c(1,2,3,4,5,6), limits = c(1,6)) +
  labs(color = "Composite Label", x = "Rating Dimension", y = "Ratings on Selected Template\nImages (TIs) +/- 1 SEM") +
  theme(text = element_text("Gill Sans MT"),
        axis.text.x  = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        panel.grid.major.y = element_line(color="gray", size=0.1, linetype = "dashed"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"))



