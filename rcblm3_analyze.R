require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(lmerTest)
require(extrafont)

rawData <- read.csv("RC-BLM-Study3_October302017.csv")

responseData <- rawData %>%
  filter(Finished == 1, data_use != 1, data_use != 2) %>%
  select(ResponseId, da_con_trust:data_use) %>%
  gather(TrialType, Choice, da_con_trust:ra_lib_appropriate) %>%
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

responseData$RatingDim[responseData$RatingDim=="emo"] <- "Emotion"
responseData$RatingDim[responseData$RatingDim=="gender"] <- "Gender"
responseData$RatingDim[responseData$RatingDim=="race"] <- "Race"
responseData$RatingDim[responseData$RatingDim=="threat"] <- "Threat"
responseData$RatingDim[responseData$RatingDim=="trust"] <- "Trust"
responseData$RatingDim[responseData$RatingDim=="appropriate"] <- "Protest\nAppropriateness"
responseData$RatingDim[responseData$RatingDim=="join"] <- "Likelihood of\nJoining"
responseData$RatingDim[responseData$RatingDim=="support"] <- "Likelihood of\nSupporting"

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

mlm_emo <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Emotion")) 
mlm_gender <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Gender")) 
mlm_race <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Race")) 
mlm_threat <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Threat")) 
mlm_trust <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Trust")) 

mlm_appropriate <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Protest\nAppropriateness")) 
mlm_join <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Likelihood of\nJoining")) 
mlm_support <- lmer(Choice ~ Label * PolAff + (1|ResponseId), data = filter(responseData, RatingDim == "Likelihood of\nSupporting")) 

supportData <- filter(responseData, RatingDim == "Protest\nAppropriateness" | RatingDim == "Likelihood of\nJoining" | RatingDim == "Likelihood of\nSupporting") %>%
  select(-TrialType) %>%
  spread(RatingDim, Choice)
supportDataAlpha <- psych::alpha(supportData[,8:10])

# Alpha between appropriateness, supporting, and joining is high = 0.8 [0.78, 0.82]
# May want to combine dimensions later for mediation analyses ...

ggplot(data = filter(summaryData, RatingDim != "Protest\nAppropriateness", RatingDim != "Likelihood of\nJoining", RatingDim != "Likelihood of\nSupporting"),
       aes(x = RatingDim, y = AvgChoice, color = Label)) +
  geom_pointrange(aes(ymin = SEMLower, ymax = SEMUpper), size = 1) +
  theme_classic() +
  # coord_flip() +
  facet_wrap("PolAff") +
  scale_color_grey() +
  # scale_y_continuous(breaks = c(2,2,3,4,5,6), limits = c(1,6)) +
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
        panel.grid.major.x = element_line(color="gray", size=0.1, linetype = "dashed"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"))

ggplot(data = filter(summaryData, RatingDim == "Protest\nAppropriateness" | RatingDim == "Likelihood of\nJoining" | RatingDim == "Likelihood of\nSupporting"),
       aes(x = RatingDim, y = AvgChoice, color = Label)) +
  geom_pointrange(aes(ymin = SEMLower, ymax = SEMUpper), size = 1) +
  theme_classic() +
  # coord_flip() +
  facet_wrap("PolAff") +
  scale_color_grey() +
  # scale_y_continuous(breaks = c(2,2,3,4,5,6), limits = c(1,6)) +
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
        panel.grid.major.x = element_line(color="gray", size=0.1, linetype = "dashed"),
        strip.background = element_rect(fill = "black"),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"))

# MEDIATION ANALYSES:
### Use moderated mediation functions in the mediation package:
require(mediation)

modMedData <- dplyr::select(responseData, ResponseId, Choice, Label, PolAff, RatingDim)
modMedData$label_code <- 999
modMedData$polaff_code <- 999

modMedData$label_code[modMedData$Label=="Diversity Advocate"] <- 1
modMedData$label_code[modMedData$Label=="Racial Activist"] <- 2

modMedData$polaff_code[modMedData$PolAff=="Conservative"] <- 1
modMedData$polaff_code[modMedData$PolAff=="Liberal"] <- 2

modMedDataSummary <- dplyr::select(modMedData, -Label, -PolAff) %>%
  spread(RatingDim, Choice)
colnames(modMedDataSummary)[4:11] <- c("app", "emo", "gender", "join", "supp", "race", "threat", "trust")

modMedDataSummary$all_supp <- rowMeans(modMedDataSummary[,c(4,7,8)])

modMedCorTable <- apaTables::apa.cor.table(modMedDataSummary[,c(4:11)], show.conf.interval = TRUE, landscape = TRUE)

### mediator = emo
modMed1_medFit1 <- lm(emo ~ label_code * polaff_code, data = modMedDataSummary)
modMed1_outFit1 <- lm(all_supp ~ emo + label_code * polaff_code, data = modMedDataSummary)
modMed1_medInit1 <- mediate(modMed1_medFit1, modMed1_outFit1, treat = "label_code", mediator = "emo", sims = 500)
modMed1_medModTest1 <- test.modmed(modMed1_medInit1, covariates.1 = list(polaff_code = 1), covariates.2 = list(polaff_code = 2), sims = 500)

### mediator = gender
modMed1_medFit2 <- lm(gender ~ label_code * polaff_code, data = modMedDataSummary)
modMed1_outFit2 <- lm(all_supp ~ gender + label_code * polaff_code, data = modMedDataSummary)
modMed1_medInit2 <- mediate(modMed1_medFit2, modMed1_outFit2, treat = "label_code", mediator = "gender", sims = 500)
modMed1_medModTest2 <- test.modmed(modMed1_medInit2, covariates.1 = list(polaff_code = 1), covariates.2 = list(polaff_code = 2), sims = 500)

### mediator = race
modMed1_medFit3 <- lm(race ~ label_code * polaff_code, data = modMedDataSummary)
modMed1_outFit3 <- lm(all_supp ~ race + label_code * polaff_code, data = modMedDataSummary)
modMed1_medInit3 <- mediate(modMed1_medFit3, modMed1_outFit3, treat = "label_code", mediator = "race", sims = 500)
modMed1_medModTest3 <- test.modmed(modMed1_medInit3, covariates.1 = list(polaff_code = 1), covariates.2 = list(polaff_code = 2), sims = 500)

### mediator = threat
modMed1_medFit4 <- lm(threat ~ label_code * polaff_code, data = modMedDataSummary)
modMed1_outFit4 <- lm(all_supp ~ threat + label_code * polaff_code, data = modMedDataSummary)
modMed1_medInit4 <- mediate(modMed1_medFit4, modMed1_outFit4, treat = "label_code", mediator = "threat", sims = 500)
modMed1_medModTest4 <- test.modmed(modMed1_medInit4, covariates.1 = list(polaff_code = 1), covariates.2 = list(polaff_code = 2), sims = 500)

### mediator = trust
modMed1_medFit5 <- lm(trust ~ label_code * polaff_code, data = modMedDataSummary)
modMed1_outFit5 <- lm(all_supp ~ trust + label_code * polaff_code, data = modMedDataSummary)
modMed1_medInit5 <- mediate(modMed1_medFit5, modMed1_outFit5, treat = "label_code", mediator = "trust", sims = 500)
modMed1_medModTest5 <- test.modmed(modMed1_medInit5, covariates.1 = list(polaff_code = 1), covariates.2 = list(polaff_code = 2), sims = 500)

### Try multiple mediation model using lavaan:
require(lavaan)

multMed <- '
# This is making the latent variable support (which we can do in a more sophisticated fashion here in SEM instead of averaging the 3 variables)
fSupport =~ app + join + supp

# This is building the regression model (DV being regressed on all mediators)
fSupport ~ emo + gender + race + threat + trust 

# Here are mediators being regressed on IVs and interaction Terms (so the path A being moderated)
emo ~ label_code + polaff_code + label_code*polaff_code
gender ~ label_code + polaff_code + label_code*polaff_code
race ~ label_code + polaff_code + label_code*polaff_code
threat ~ label_code + polaff_code + label_code*polaff_code
trust ~ label_code + polaff_code + label_code*polaff_code
'

fit <- sem(multMed, data=modMedDataSummary, bootstrap=5000)

summary(fit, fit.measures=TRUE, standardized=TRUE, modindices=TRUE)
semPaths(fit, layout="tree3","std", edge.label.cex=.5,optimizeLatRes=TRUE, curvePivot=FALSE)


require(boot)
modMedDataSummary2 <- modMedDataSummary
modMedDataSummary2$label_code[modMedDataSummary2$label_code==1] <- 0
modMedDataSummary2$label_code[modMedDataSummary2$label_code==2] <- 1
modMedDataSummary2$polaff_code[modMedDataSummary2$polaff_code==1] <- 0
modMedDataSummary2$polaff_code[modMedDataSummary2$polaff_code==2] <- 1

med1.cov2.mod1 <- function(d, i){q <- d[i,]
return(   (   (lm(q$emo ~ q$label_code*q$polaff_code)$coef[2])    )*
            (lm(q$all_supp ~ q$emo + q$label_code*q$polaff_code + q$gender + q$race + q$threat + q$trust)$coef[2]))}

ind <- boot(modMedDataSummary2, med1.cov2.mod1, R=1000)

boot.ci(ind, type = "bca", conf.=0.95)

# to use my code just leave d and q as it's they are variables for the loops
# just change what's after the $ and change data to your data and it will work!
# of course be mindful the indirect effect will be the indirect effect for whatever it means when your moderator = 0









