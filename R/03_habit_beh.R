# Behaviour frequency
colnames(df)

hist(df$int_b1)
hist(df$freq_b1)

df <- df %>%
  mutate(stock_hs = ifelse(int_b1 != 1 & freq_b1 != 1, rowMeans(select(., hs_b1_1:hs_b1_4), na.rm = TRUE), NA))
df <- df %>%
  mutate(left_hs = ifelse(Q38 != 1 & Q37 != 1, rowMeans(select(., Q40_1:Q40_4), na.rm = TRUE), NA))
df <- df %>%
  mutate(sense_hs = ifelse(Q43 != 1 & Q42 != 1, rowMeans(select(., Q45_1:Q45_4), na.rm = TRUE), NA))


df %>% 
  select(int_b1:hs_b1_4, stock_hs) %>% 
  filter(int_b1 != 1 & freq_b1 != 1) %>% 
  drop_na() %>% 
  cor() %>% 
  round(2) %>% 
  corrplot(method="color", addCoef.col = "black", diag=FALSE, type="lower", tl.srt=45, tl.col="black")

df %>% 
  select(Q38:Q40_4, left_hs) %>%
  filter(Q38 != 1 & Q37 != 1) %>% 
  drop_na() %>% 
  cor() %>% 
  round(2) %>% 
  corrplot(method="color", addCoef.col = "black", diag=FALSE, type="lower", tl.srt=45, tl.col="black")

df %>% 
  select(Q43:Q45_4 & !att, sense_hs) %>% 
  filter(Q43 != 1 & Q42 != 1) %>% 
  drop_na() %>% 
  cor() %>% 
  round(2) %>% 
  corrplot(method="color", addCoef.col = "black", diag=FALSE, type="lower", tl.srt=45, tl.col="black")

df %>% 
  filter(int_b1 != 1 & freq_b1 != 1) %>% 
  select(stock_hs)

df %>% 
  filter(Q38 != 1 & Q37 != 1) %>% 
  select(left_hs)

df %>% 
  filter(Q43 != 1 & Q42 != 1) %>% 
  select(sense_hs)




hsBeh <- df %>% 
  filter(int_b1 != 1 & freq_b1 != 1) %>% 
  select(stock_hs) %>% 
  filter(Q38 != 1 & Q37 != 1) %>% 
  select(left_hs) %>% 
  filter(Q43 != 1 & Q42 != 1) %>% 
  select(sense_hs) %>% 
  describe() %>% 
  select(n, mean, sd, median, min, max)

behPerftab <- df %>% 
  select(int_b1, Q38, Q43) %>% 
  describe() %>% 
  select(n, mean, sd, median, min, max)

df %>% 
  select(stock_hs) %>% 
  hist()

hist(df$stock_hs)

?hist

table()

behFreqtab <- df %>% 
  select(freq_b1, Q37, Q42) %>% 
  describe() %>% 
  select(n, mean, sd, median, min, max)

save_as_docx(flextable(hsBeh), path = paste0(wd$output, "behaviourSumm.docx"))

write.csv(hsBeh, paste0(wd$output, "hsBeh.txt"), quote = F)

?write.csv

behPerfSumm <- describeBy(behPerf[c(1:3)], behFreq$Country) %>% select(n, mean, sd, median, min, max)
behPerfSumm[['Austria']]

colnames(df)

behFlong <- pivot_longer(behFreqtab, everything())
?pivot_longer

response_labels <- c("Never", "Rarely", "Sometimes", "Often", "Always")

ggplot(behFlong, aes(x = name, y = as.factor(value), fill = as.factor(value)), label = response_labels) +
  geom_bar(stat = "identity") +
  labs(x = "Behavior", y = "Frequency", fill = "Response") +
  scale_x_discrete(labels = c(
    "freq_b1" = "Check Pantry Before Shopping",
    "Q37" = "Save Leftovers for Later",
    "Q42" = "Use Senses to Judge Food"
  )) +
  theme_minimal() +
  geom_text(size = 3, position = position_stack(vjust = .4))+
  theme(legend.position = "top")

ggplot(behFlong, aes(x = name, y = value, fill = as.factor(value))) +
  geom_bar(stat = "identity") +
  labs(x = "Behavior", y = "Frequency", fill = "Response") +
  scale_x_discrete(labels = c(
    "freq_b1" = "Check Pantry Before Shopping",
    "Q37" = "Save Leftovers for Later",
    "Q42" = "Use Senses to Judge Food"
  )) +
  scale_y_continuous(breaks = 1:5, labels = response_labels) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = "Response"))

?Boxplot


?describeBy
colnames

table(df$int_b1)
Boxplot(df$Q38)

?rowMeans

hist(df$stock_hs)


stock_fw <- lm(fw_total ~ stock_hs, data=df)
left_fw <- lm(fw_total ~ left_hs, data=df) # regression of leftover checking and waste of cooked foods
sense_fw <- lm(fw_total ~ sense_hs, data=df)

summary(left_fw)
summary(stock_fw)
summary(sense_fw)
plot(stock_fw)

abline(stock_fw)

ggplot(df, aes(x = stock_hs, y = fw_total)) +
  geom_point() + 
  stat_smooth(method="lm", col="red")


df %>% 
  select(hs_b1_1:hs_b1_4) %>% 
  cor(use="pairwise.complete.obs") %>% 
  alpha()

stockdf <- df %>% 
  select(stock_hs, psycap:autmot) %>% 
  drop_na()
stock_hs_model <- lm(log_stock_hs ~ psycap + socopp + phyopp + refmot + autmot, data = df)
left_hs_model <- lm(left_hs ~ psycap + socopp + phyopp + refmot + autmot, data = df)
sense_hs_model <- lm(sense_hs ~ psycap + socopp + phyopp + refmot + autmot, data = df)

df$log_stock_hs <- sqrt(max(df$stock_hs+1, na.rm=T) - df$stock_hs)
?max
?sqrt
skewness(df$log_stock_hs, na.rm=T)
boxplot(df$stock_hs)

hist(df$log_stock_hs)

hist(df$stock_hs)
hist(df$left_hs)
hist(df$stock_hs)

summary(stock_hs_model)
plot(stock_hs_model)
summary(left_hs_model)
summary(sense_hs_model)

# regression diagnostics
stockdf$resid <- resid(stock_hs_model)
stockdf$stdres <- rstandard(stock_hs_model)
stockdf$stures <- rstudent(stock_hs_model)
stockdf$cook <- cooks.distance(stock_hs_model)
stockdf$dfbeta <- dfbeta(stock_hs_model)
stockdf$dffits <- dffits(stock_hs_model)
stockdf$leverage <- hatvalues(stock_hs_model)
stockdf$covrat <- covratio(stock_hs_model)

stockdf$largeres <- stockdf$stdres > 2 | stockdf$stdres < -2
sum(stockdf$largeres)

stockdf %>% 
  filter(largeres == 1) %>% 
  select(cook, leverage, covrat)

which(stockdf$stdres < -3)

dwt(stock_hs_model)

vif(stock_hs_model) # largest VIF > 10, problem
mean(vif(stock_hs_model)) # mean substantially greater than 1, biased
1/vif(stock_hs_model) # tolerance < 0.2 possible problem. < 0.1 serious problem

# normally distributed residuals
hist(stockdf$stures)

plot(stock_hs_model)
cor(df)

summary(stock_hs_model)
plot(stock_hs_model)
summary(left_hs_model)
summary(sense_hs_model)

hist(df$sense_hs)

habitdf <- df %>% 
  select(int_b1:Q45_4 & !att, fw_total, fw_total)

colnames(df)

fw_habit_lm <- lm(fw_total_log ~ stock_hs + left_hs + sense_hs, data = df)
w_habit <- df %>% 
  filter(fw_total_log != 0) %>% 
  lm(fw_total_log ~ stock_hs + left_hs + sense_hs, data = df)

lm_w_habit <- lm(fw_total_log ~ stock_hs + left_hs + sense_hs, data = w_habit)
summary(lm_w_habit)
plot(lm_w_habit)
vif(lm_w_habit)

fw_habit_tobit <- censReg(fw_total ~ stock_hs + left_hs + sense_hs, data = df)
summary(fw_habit_tobit)
summary(fw_habit_lm)
anova(fw_habit_lm, fw_habit_tobit)
coef(stock_fw_tobit)
plot(fw_habit_lm)

logLik(fw_habit_lm)
logLik(fw_habit_tobit)
vcov(stock_fw_tobit)

View(df$Q7_7_TEXT)

comments <- df %>% 
  select(open_det) %>% 
  filter(!is.na(.)) 

write.csv(comments, paste0(wd$output, "openComments.txt"), row.names = F, quote = F)

colnames(df)

dem_hs_fit <- lm(left_hs ~ gender + edu + employ + income, data = df)
summary(dem_hs_fit)
anovadem <- anova(dem_hs_fit)
TukeyHSD(anovadem)

stock_freq_fit <- lm(int_b1 ~ gender + edu + employ + income + child + adult + age, data = df)
left_freq_fit <- lm(Q38 ~ gender + edu + employ + income + child + adult + age, data = df)
sense_freq_fit <- lm(Q43 ~ gender + edu + employ + income + child + adult + age, data = df)

colnames(df)
tidy(summary(beh_freq))

anova(stock_freq_fit)
anova(left_freq_fit)
anova(sense_freq_fit)

anova(beh_freq)

summary(anovadem)

??tukeyHSD

df$age <- as.numeric(df$age)
df$gender <- as.factor(df$gender)
df$edu <- as.factor(df$edu)
df$employ <- as.factor(df$employ)
df$income <- as.factor(df$income)


fw_hs_fit1 <- lm(fw_total_log ~ age + child + adult, data = comb_items)
fw_hs_fit2 <- lm(fw_total_log ~ age + child + adult + left_hs + sense_hs + stock_hs, data = comb_items)
summ_fw_hs <- summary(fw_hs_fit2)
hmrfw <- anova(fw_hs_fit1, fw_hs_fit2)

fw_comb_filter <- comb_items %>% filter(fw_total_log != 0)
fwf_hs_fit1 <- lm(fw_total_log ~ age + child + adult, data = fw_comb_filter)
fwf_hs_fit2 <- lm(fw_total_log ~ age + child + adult + left_hs + sense_hs + stock_hs, data = fw_comb_filter)
summ_fwf_hs <- summary(fwf_hs_fit2)
hmrfwf <- anova(fwf_hs_fit1, fwf_hs_fit2)

fwn_hs_fit1 <- glm(anywaste ~ age + child + adult, family = "binomial", data = comb_items)
fwn_hs_fit2 <- glm(anywaste ~ age + child + adult + left_hs + sense_hs + stock_hs, family = "binomial", data = comb_items) 
summary(fwn_hs_fit2)
tidy(fwn_hs_fit2)

comb_items$res <- resid(fw_hs_fit2)
comb_items$standardized.residuals<- rstandard(fw_hs_fit2)
fw_comb_filter$studentized.residuals<-rstudent(fwf_hs_fit2)
comb_items$cooks.distance<-cooks.distance(fw_hs_fit2)
comb_items$dfbeta<-dfbeta(fw_hs_fit2)
comb_items$dffit<-dffits(fw_hs_fit2)
comb_items$leverage<-hatvalues(fw_hs_fit2)
comb_items$covariance.ratios<-covratio(fw_hs_fit2)

tidy(dwt(fwf_hs_fit2))
vif(fwf_hs_fit2)
hist(fw_comb_filter$studentized.residuals)

comb_items$largeres <- comb_items$standardized.residuals > 2 | comb_items$standardized.residuals < -2
sum(comb_items$largeres)

comb_items[comb_items$largeres, c("fw_total_log", "left_hs","sense_hs", "stock_hs", "standardized.residuals")]

