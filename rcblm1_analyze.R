require(rcicr)
require(dplyr)
require(tidyr)
require(stringr)

rawData <- read.csv("rcblm1_data_09282017.csv")

cleanData <- rawData %>%
  filter(Finished == 1, data_use != 1, data_use != 2) %>%
  select(ResponseId, age:ra_300) %>%
  gather(Trial, Choice, da_1:ra_300) %>%
  arrange(ResponseId, Trial)

cleanData <- separate(cleanData, Trial, c("RatingCnd", "TrialCode"), "_")

# Generate column for CI choice codes:
cleanData$ChoiceCode_ci <- 999

cleanData$ChoiceCode_ci[cleanData$Choice==1] <- -1
cleanData$ChoiceCode_ci[cleanData$Choice==2] <- 1

cleanData <- filter(cleanData, Choice != "") %>% arrange(ResponseId, TrialCode)

# Create column for political affiliation, and join to main DF:
df_polAtt <- data.frame(pol_att = c(1,2,8,9), pol_att_code = c("liberal", "liberal", "conservative", "conservative"))

cleanData <- plyr::join(cleanData, df_polAtt)

responseData_da_lib <- filter(cleanData, RatingCnd == "da", pol_att_code == "liberal")
responseData_da_con <- filter(cleanData, RatingCnd == "da", pol_att_code == "conservative")
responseData_ra_lib <- filter(cleanData, RatingCnd == "ra", pol_att_code == "liberal")
responseData_ra_con <- filter(cleanData, RatingCnd == "ra", pol_att_code == "conservative")

baseImg <- "base"
rData <- "rcic_seed_1_time_Sep_05_2017_15_22.Rdata"

# Make sure each subject has 300 observations, and check sample size in each cell:
sbjSummary1 <- group_by(cleanData, ResponseId) %>%
  summarize(
    count = n()
  )

sbjSummary2 <- group_by(cleanData, RatingCnd, pol_att_code) %>%
  summarize(
    count = length(unique(ResponseId))
  )

# Generate template images across all subjects and conditions:
generateCI2IFC(responseData_da_lib$TrialCode,
               responseData_da_lib$ChoiceCode_ci,
               baseImg,
               rData,
               targetpath = "./ci_da_lib")

generateCI2IFC(responseData_da_con$TrialCode,
               responseData_da_con$ChoiceCode_ci,
               baseImg,
               rData,
               targetpath = "./ci_da_con")

generateCI2IFC(responseData_ra_lib$TrialCode,
               responseData_ra_lib$ChoiceCode_ci,
               baseImg,
               rData,
               targetpath = "./ci_ra_lib")

generateCI2IFC(responseData_ra_con$TrialCode,
               responseData_ra_con$ChoiceCode_ci,
               baseImg,
               rData,
               targetpath = "./ci_ra_con")
