

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
library(usethis)
library(data.table)
library(QuantPsyc)
library(censReg)
library(plotmath)
library(corrplot)
library(VGAM)
library(AER)
library(broom)

install.packages("AER")

select <- dplyr::select

# Set WD
wd <- list()
wd$data   <- "C:/MyData/paper 1/2_habit_comb_food/data/"
wd$output <- "C:MyData/paper 1/2_habit_comb_food/output/"

# LOAD RAW DATA
raw_data <- read_survey(paste0(wd$data, "Food_habit_COM-B-EN_16+October+2023_10.08.csv"))

# DATA CLEANING
{df <- raw_data %>% # filter no food handling, previews, non consent, failed att check
  filter(is.na(Q2_6) & Status == 0 & consent != 0 & (Q1...29 == 4 & (att != 4 | att != 5))) %>% 
  select(!c(Status, StartDate,EndDate,Progress,RecordedDate,ResponseId, DistributionChannel, Finished)) %>%
  rowid_to_column(., "ID")

df[,c("CAP_3", "CAP_5", "ref_mot_2_5")] <- 8 - df[,c("CAP_3", "CAP_5", "ref_mot_2_5")] # reverse score CAP3 and CAP5

# rename and labels socdem
df <- df %>% 
  rename(gender = Q3...98,
         age = Q2,
         adult = Q4...99,
         child = Q5...100,
         edu = Q6,
         employ = Q7,
         income = Q8...104)

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
label(df$Q3...88) <- "Vegetables"
label(df$Q4...89) <- "Fruit"
label(df$Q5...90) <- "Potato"
label(df$Q8...91)   <- "Meat (substitute), fish"
label(df$Q9) <- "Sandwich fillings"
label(df$Q10) <- "Bread"
label(df$Q12) <- "Dairy"
label(df$Q13)   <- "Eggs"
label(df$Q15) <- "Condiments and sauces"
}

#### PROLIFIC REJECTION ####
# exclude <- dataframe %>% # People whose submission can be rejected for failing both attention checks
#   select(PROLIFIC_PID, Q1...29, att, UserLanguage) %>% 
#   filter(Q1...29 != 4 & (att == 4 | att == 5))
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
    select(CAP_1:aut_mot_1_4 & !Q1...29) %>% 
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

# careless_long <- df %>% 
#   select(CAP_1:aut_mot_1_4 & !Q1...29) %>% 
#   longstring()

# Boxplot(careless_long)

# Check missing values
# missing <- df %>% 
#   select(ID, PROLIFIC_PID, CAP_1:Q45_4 & !Q1...29 & !CAP_DO_1:CAP_DO_5 & !OPP_DO_1:OPP_DO_4)
# 
# 
# incomplete <- missing[!complete.cases(missing), ]
# 
# missdata <- missing %>% 
#   filter(rowSums(is.na(missing)) > 5)
# 
# incomplete <- df %>% 
#   select(is.na())
# 
# incomplete <- df[!complete.cases(df), ]

