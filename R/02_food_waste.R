##### FUNCTIONS FW CONVERSION ####
{serving_spoon <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 25,
      {{col_name}} == 2 ~ 75,
      {{col_name}} == 3 ~ 150,
      {{col_name}} == 4 ~ 250,
      {{col_name}} == 5 ~ 350,
      TRUE ~ {{col_name}}
    ))
}

fruit_piece <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 25,
      {{col_name}} == 2 ~ 50,
      {{col_name}} == 3 ~ 100,
      {{col_name}} == 4 ~ 300,
      {{col_name}} == 5 ~ 500,
      TRUE ~ {{col_name}}
    ))
}

portion <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 75,
      {{col_name}} == 2 ~ 150,
      {{col_name}} == 3 ~ 375,
      {{col_name}} == 4 ~ 675,
      {{col_name}} == 5 ~ 900,
      TRUE ~ {{col_name}}
    ))
}
sandwich_portion <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 10,
      {{col_name}} == 2 ~ 20,
      {{col_name}} == 3 ~ 50,
      {{col_name}} == 4 ~ 90,
      {{col_name}} == 5 ~ 120,
      TRUE ~ {{col_name}}
    ))
}

bread <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 18,
      {{col_name}} == 2 ~ 35,
      {{col_name}} == 3 ~ 400,
      {{col_name}} == 4 ~ 800,
      {{col_name}} == 5 ~ 1200,
      TRUE ~ {{col_name}}
    ))
}

egg <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 30,
      {{col_name}} == 2 ~ 60,
      {{col_name}} == 3 ~ 150,
      {{col_name}} == 4 ~ 270,
      {{col_name}} == 5 ~ 360,
      TRUE ~ {{col_name}}
    ))
}

dairy_portion <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 38,
      {{col_name}} == 2 ~ 150,
      {{col_name}} == 3 ~ 500,
      {{col_name}} == 4 ~ 1000,
      {{col_name}} == 5 ~ 2000,
      TRUE ~ {{col_name}}
    ))
}

sauce <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 10,
      {{col_name}} == 2 ~ 30,
      {{col_name}} == 3 ~ 90,
      {{col_name}} == 4 ~ 225,
      {{col_name}} == 5 ~ 675,
      TRUE ~ {{col_name}}
    ))
}

df <- df %>% 
  serving_spoon(Q14) %>% 
  serving_spoon(veg) %>% 
  serving_spoon(pot) %>% 
  fruit_piece(fruit) %>% 
  portion(meat) %>% 
  sandwich_portion(Q9) %>% 
  bread(Q10) %>% 
  dairy_portion(Q12) %>% 
  egg(Q13) %>% 
  sauce(Q15)

df <- df %>% 
  mutate(fw_total = rowSums(select(., Q14:Q15), na.rm=TRUE))

label(df$fw_total) <- "Total food waste"
units(df$fw_total) <-  "g"
}


##### fw quantification ####
fw_num <- df %>% 
  select(UserLanguage, Q14:Q15)

fw_num[is.na(fw_num)] <- 0

fw_summ_num <- fw_num %>% 
  summarise(across(everything(), ~sum(., na.rm=TRUE)))

fw_mean_num <- fw_num %>% 
  summarise(across(everything(), ~mean(., na.rm=TRUE)))

fw_cat <- df %>% 
  select(UserLanguage, Q1_28:Q1_31)

fw_summ <- fw_cat %>% 
  summarise(across(Q1_28:Q1_31, ~sum(. == 1, na.rm = TRUE))) %>%
  arrange(desc(.))

fw_na_mean <- fw_num %>% 
  group_by(UserLanguage) %>% 
  summarise(across(Q14:Q15, ~mean(., na.rm = TRUE))) %>%
  arrange(desc(.))

# Convert the dataframe to a long format
food_label <- data.frame(
  Variable = c("Q1_28", "Q1_17", "Q1_18", "Q1_19", "Q1_22", "Q1_23", "Q1_24", "Q1_26", "Q1_27", "Q1_29", "Q1_31"),
  Label = c(
    "Cooked Food", "Vegetables", "Fruit", "Potato",
    "Meat (substitute), fish", "Sandwich fillings", "Bread",
    "Dairy", "Eggs", "Condiments and sauces", "None of the above"))

# Convert the dataframe to a long format
amount_food_label <- data.frame(
  Variable = c("Q14", "Q3...88", "Q4...89", "Q5...90", "Q8...91", "Q9", "Q10", "Q12", "Q13", "Q15"),
  Label = c(
    "Cooked Food", "Vegetables", "Fruit", "Potato",
    "Meat (substitute), fish", "Sandwich fillings", "Bread",
    "Dairy", "Eggs", "Condiments and sauces"))

# Distribution food waste
ggplot(df, aes(x=fw_total))+
  geom_histogram(binwidth=60, color="#e9ecef", fill = "#8ab17d", position = 'identity') +
  labs(fill="", x = "Food Waste (g)")

df$fw_total_log <- log(df$fw_total + 1)

ggplot(df, aes(x=fw_total_log))+
  geom_histogram(binwidth = .2, color="#e9ecef", fill = "#8ab17d", position = 'identity') +
  labs(fill="", x = "Food Waste (g)")

# Frequency of type of food wasted
fw_summ_long <- pivot_longer(fw_summ, everything(), names_to = "Variable", values_to = "Value") %>% 
  left_join(food_label, by="Variable")

ggplot(fw_summ_long, aes(x = reorder(Label, -Value), y = Value, fill = Label)) +
  geom_bar(stat = "identity") +
  labs(x = "Food Categories", y = "Number of times wasted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill="none")

fw_summ__na_long <- pivot_longer(fw_summ_na, everything(), names_to = "Variable", values_to = "Value") %>% 
  left_join(food_label, by="Variable")


#### Grams of food wasted by category
fw_total_long <- pivot_longer(fw_summ_num, everything(), names_to = "Variable", values_to = "Value") %>% 
  left_join(amount_food_label, by="Variable")

ggplot(data = fw_total_long, aes(x = reorder(Label, -Value), y = Value, fill = Label)) +
  geom_bar(stat = "identity") +
  labs(title = "Grams of wasted food by category", x = "Food Category", y = "Grams of waste") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill="none")

fw_total_long_avg <- pivot_longer(fw_mean_num, everything(), names_to = "Variable", values_to = "Value") %>% 
  left_join(amount_food_label, by="Variable")

ggplot(data = fw_total_long_avg, aes(x = reorder(Label, -Value), y = Value, fill = Label)) +
  geom_bar(stat = "identity") +
  labs(title = "Grams of Wasted Food by Category", x = "Food Category", y = "Average Grams of Waste") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")

# by country
fw_total_long_avg_na <- pivot_longer(fw_na_mean, Q14:Q15, names_to = "Variable", values_to = "Value") %>% 
  left_join(amount_food_label, by="Variable")

na_names <- c("DE" = "Austria",
              "EL" = "Greece",
              "EN-GB" = "United Kingdom",
              "ES-ES" = "Spain",
              "NL" = "The Netherlands",
              "SV" = "Sweden")

View(na_names)

ggplot(data = fw_total_long_avg_na, aes(x = reorder(Label, Value), y = Value, fill = UserLanguage)) +
  geom_bar(stat = "identity", position="dodge") +
  labs(title = "Average Grams of Wasted Food by Category", x = "Food Category", y = "Average Grams of Waste") +
  theme_minimal() +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill = guide_legend(title = "Country"))+
  scale_fill_manual(labels = na_names, values = c("#264653", "#2a9d8f", "#8ab17d", "#e9c46a", "#f4a261", "#e76f51"))

values = c("#618943", "#82AA57", "#9EBC63", "#C5D86D", "#F2EFBB", "#F9F7DC")
values = c("#264653", "#2a9d8f", "#8ab17d", "#e9c46a", "#f4a261", "#e76f51")

ggplot(df, aes(x = fw_total, y = count)) +
  geom_bar(stat = "identity")

ggplot(data = fw_total_long_avg_na, aes(x = reorder(Label, Value), y = Value, fill = UserLanguage)) +
  geom_bar(stat = "identity", position="dodge") +
  labs(title = "Average Grams of Wasted Food by Category", x = "Food Category", y = "Mean Grams of Waste / week / household") +
  theme_minimal() +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill = guide_legend(title = "Country"))+
  scale_fill_manual(labels = na_names, values = c("#264653", "#2a9d8f", "#8ab17d", "#e9c46a", "#f4a261", "#e76f51"))

# plot of mean waste

fw_mean_na <- df %>% 
  group_by(UserLanguage) %>% 
  summarise(mean_waste = mean(fw_total))

ggplot(fw_mean_na, aes(x = UserLanguage, y = mean_waste, fill = UserLanguage)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Grams of Wasted Food per week per household by Country", x = "Country", y = "Mean Grams of Waste / week / household") +
  scale_x_discrete(labels=na_names)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Country")) +
  scale_fill_manual(labels = na_names, values = c("#264653", "#2a9d8f", "#8ab17d", "#e9c46a", "#f4a261", "#e76f51"))


df %>% 
  group_by(Country) %>% 
  select(Country, Q1_31) %>% 
  table()

# Specific foods per country
df <- df %>% 
  mutate(fw_total = rowSums(select(., Q14:Q15), na.rm=TRUE))

# logistic whether food was wasted
df$anywaste <- ifelse(!is.na(df$Q1_31), 1, 0)

dfc <-  df %>% 
  select(ID, PROLIFIC_PID, CAP_1:Q45_4 & !att_1, age:income & !Q7_7_TEXT, fw_total:anywaste, Q12, Country)

colnames(df)

dfc <- dfc[complete.cases(dfc), ]
df$fw_total_log <- log(df$fw_total + 1)

logifit1 <- glm(anywaste ~ adult + child + age, data=dfc, family="binomial")
logifit2 <- glm(anywaste ~ adult + child + age + psycap + socopp + phyopp + refmot + autmot, data=dfc, family="binomial")

demFit <- as.data.frame(coef(summary(logifit1)))
flexDemFit <- flextable(demFit %>% rownames_to_column("term")) %>% colformat_double(digits=2) %>% bold( ~ 5 < 0.05, j=4)

fit2df <- as.data.frame(coef(summary(logifit2)))
flexFit2 <- flextable(fit2df %>% rownames_to_column(" ")) %>% colformat_double(digits=2)




dfWaste <- dfc %>% filter(fw_total_log != 0)

#lm wasters only
fitWaste1 <- lm(fw_total_log ~ child + adult + age, data = dfWaste)
fitWaste2 <- lm(fw_total_log ~ child + adult + age + psycap + socopp + phyopp + refmot + autmot, data = dfWaste)

flexWaste2 <- flextable(tidy(fitWaste2)) %>% 
  colformat_double(digits=2) %>% 
  bold( ~p.value < 0.05, j=5)

tobitFit <- censReg(fw_total_log ~ child + adult + age + psycap + socopp + phyopp + refmot + autmot, data=dfc)
tobitFitdf <- as.data.frame(coef(summary(tobitFit)))
tobitFlex <- flextable(tobitFitdf %>% rownames_to_column("term")) %>% colformat_double(digits=2)

save_as_docx(`Logistic regression predicting whether people waste or not` = flexFit2, `Linear regression predicting the amount of waste for people who waste` = flexWaste2, `Tobit model predicting food waste` = tobitFlex, path = paste0(wd$output, "model1.docx"))
colnames(dfWaste)

countryBread <- lm(Q12 ~ Country, data = df)
countryFW <- lm(fw_total_log ~ Country, data = df)
summary(countryBread)
summary(countryFW)
anova(countryFW)
