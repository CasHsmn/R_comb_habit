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
