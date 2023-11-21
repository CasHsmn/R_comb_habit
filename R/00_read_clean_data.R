

# Initiate packages
library(dplyr)
library(qualtRics)
library(car)
library(tibble)
library(tidyr)
library(careless)
library(ggplot2)
library(table1)
library(LaplacesDemon)
library(mousetrap)
library(diptest)
library(e1071)
library(ltm)
library(table1)
library(psych)
library(knitr)
library(kableExtra)
library(usethis)
library(data.table)
library(QuantPsyc)
library(censReg)
library(corrplot)
library(VGAM)
library(AER)
library(broom)
library(mice)
library(patchwork)
library(corrplot)
library(flextable)
library(officer)
library(purrr)
library(reshape2)
library(writexl)
library(lmtest)
library(modelsummary)
library(stats)
library(GPArotation)

install

 select <- dplyr::select

# Set WD
wd <- list()
wd$data   <- "C:/MyData/paper 1/2_habit_comb_food/data/"
wd$output <- "C:/MyData/paper 1/2_habit_comb_food/output/"

# LOAD RAW DATA
{raw_data <- read_survey(paste0(wd$data, "data_food_habit_comb.csv"))
demUK <- read.csv(paste0(wd$data, "demo_UK.csv")) %>%   select(Participant.id, Country.of.residence, Nationality)
demES <- read.csv(paste0(wd$data, "demo_ES.csv")) %>%   select(Participant.id, Country.of.residence, Nationality)
demNL <- read.csv(paste0(wd$data, "demo_NL.csv")) %>%   select(Participant.id, Country.of.residence, Nationality)
demDE <- read.csv(paste0(wd$data, "demo_DE.csv")) %>%   select(Participant.id, Country.of.residence, Nationality)
demEL <- read.csv(paste0(wd$data, "demo_EL.csv")) %>%   select(Participant.id, Country.of.residence, Nationality)
demSV <- read.csv(paste0(wd$data, "demo_SV.csv")) %>%   select(Participant.id, Country.of.residence, Nationality)

dfDem <- bind_rows(demUK,demES, demNL, demDE, demEL, demSV) %>% 
 filter(!Country.of.residence == "CONSENT_REVOKED") 

dfRaw <- raw_data %>% left_join(dfDem, by = join_by(PROLIFIC_PID == Participant.id))

colnames(dfRaw)[colnames(dfRaw) == "Country.of.residence"] = "Country"
}




# DATA CLEANING
{df <- dfRaw %>% # filter no food handling, previews, non consent, failed att check
  filter(is.na(Q2_6) & Status == 0 & consent != 0 & (att_1 == 4 & (att != 4 | att != 5))) %>% 
  select(!c(Status, StartDate,EndDate,Progress,RecordedDate,ResponseId, DistributionChannel, Finished, ref_mot_1_2, ref_mot_2_4, ref_mot_2_5)) %>%
  rowid_to_column(., "ID")
  
df[,c("CAP_3", "CAP_5")] <- 8 - df[,c("CAP_3", "CAP_5")] # reverse scores

# rename and labels socdem
df <- df %>% 
  rename(gender = gen,
         age = Q2,
         adult = adu,
         child = ch,
         edu = Q6,
         employ = Q7,
         income = inc)

df$int_b1 <- df$int_b1 - 5
df$Q38 <- df$Q38 - 5
df$Q43 <- df$Q43 - 5

df$gender <- factor(df$gender, levels=c(1,2,3,4), labels=c("Male", "Female", "Non-binary/third-gender", "Prefer not to say"))

df$income <- factor(df$income, levels=c(28:38,55), labels=c("Less than €1,000",
                                                            "€1,000 - €1,999",
                                                            "€2,000 - €2,999",
                                                            "€3,000 - €3,999",
                                                            "€4,000 - €4,999",
                                                            "€5,000 - €5,999",
                                                            "€6,000 - €6,999",
                                                            "€7,000 - €7,999",
                                                            "€8,000 - €8,999",
                                                            "€9,000 - €9,999",
                                                            "€10,000 - €14,999",
                                                            "€15,000 or more"))

df$employ <- factor(df$employ, levels=c(10,11,9,12,13,14,15,16,7), labels=c("Employed full-time",
                                                                            "Employed part-time",
                                                                            "Self employed",
                                                                            "Unemployed looking for work",
                                                                            "Unemployed not looking for work",
                                                                            "Retired",
                                                                            "Student",
                                                                            "Inability to work",
                                                                            "Other"))

df$edu <- factor(df$edu, levels=c(1,9,10,12,11,13,14,15), labels=c("No formal education",
                                                                   "Primary education",
                                                                   "Secondary education (GCSE)",
                                                                   "College or vocational qualifications (e.g. BTEC, NVQ)",
                                                                   "A-levels or equivalent",
                                                                   "Bachelor's degree or equivalent",
                                                                    "Master's degree or equivalent",
                                                                   "Doctorate or equivalent"))

label(df$age)   <- "Age"
label(df$gender)   <- "Gender"
label(df$edu)    <- "Highest education level"
label(df$income) <- "Net monthly household income"
label(df$employ)   <- "Employment status"

label(df$Q1_28)   <- "Cooked Food"
label(df$Q1_17) <- "Vegetables"
label(df$Q1_18) <- "Fruit"
label(df$Q1_19) <- "Potato"
label(df$Q1_22)   <- "Meat (substitute), fish"
label(df$Q1_23) <- "Sandwich fillings"
label(df$Q1_24) <- "Bread"
label(df$Q1_26) <- "Dairy"
label(df$Q1_27)   <- "Eggs"
label(df$Q1_29) <- "Condiments and sauces"
label(df$Q1_31) <- "None of the above"


label(df$Q14)   <- "Cooked Food"
label(df$veg) <- "Vegetables"
label(df$fruit) <- "Fruit"
label(df$pot) <- "Potato"
label(df$meat)   <- "Meat (substitute), fish"
label(df$Q9) <- "Sandwich fillings"
label(df$Q10) <- "Bread"
label(df$Q12) <- "Dairy"
label(df$Q13)   <- "Eggs"
label(df$Q15) <- "Condiments and sauces"

df[327,"Country"] <- "United Kingdom"
}

table(df$Nationality)

#### PROLIFIC REJECTION ####
# exclude <- raw_data %>% # People whose submission can be rejected for failing both attention checks
#   select(PROLIFIC_PID, Q1...24, att, UserLanguage) %>% 
#   filter(Q1...24 != 4 & (att == 4 | att == 5))
# 
# noninvolvement <- dataframe %>% # people who are not involved in any food handling
#   filter(Q2_6 != 1) 
# 
# nonconsent <- dataframe %>% 
#   filter(consent != 1)

# Boxplot(df$`Duration (in seconds)`) # Some people took very long

# table(dataframe$UserLanguage) # Quite a few people may have changed their language manually, may

# CARELESS RESPONDING CHECK
# Check intra-rater-variability (IRV) - SD across responses
{irv <- df %>%  # Remove responses with very similar answers
    select(CAP_1:aut_mot_1_4 & !att_1) %>% 
    irv(., na.rm=TRUE)
irv <- as.data.frame(irv)
irv <- rowid_to_column(irv, "ID")

irv_q1 <- quantile(irv$irv, 0.25, na.rm=TRUE)
irv_q3 <- quantile(irv$irv, 0.75, na.rm=TRUE)
irv_iqr <- irv_q3 - irv_q1

lower_bound <- irv_q1 - 1.5 * irv_iqr
upper_bound <- irv_q3 + 1.5 * irv_iqr
outliers_rows <- irv %>% filter(irv < lower_bound | irv > upper_bound)

df <- df %>% 
 anti_join(outliers_rows, by="ID")}

#  careless_long <- df %>% 
#    select(CAP_1:aut_mot_1_4 & !att_1) %>% 
#    longstring()
# View(careless_long)
# 
# careless_long <- rowid_to_column(careless_long, "ID")
# Boxplot(careless_long)
# careless_long <- careless_long %>% 
#   left_join(irv, by = "ID")

# Check missing values
#  missing <- df %>% 
#    select(ID, PID, CAP_1:Q45_4 & !att_1)
# # 
# # 
#  incomplete <- missing[!complete.cases(missing), ]
# # 
#  missdata <- missing %>% 
#    filter(rowSums(is.na(missing)) > 5)
# # 
# incomplete <- missing %>% 
#    select(is.na())
# 
# which(is.na(missing))
# # 
#  incomplete <- df[!complete.cases(df), ]
