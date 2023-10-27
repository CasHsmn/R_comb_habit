# Behaviour frequency
colnames(df)

hist(df$int_b1)
hist(df$freq_b1)

df <- df %>% 
  mutate(stock_hs = rowMeans(select(., hs_b1_1:hs_b1_4), na.rm=TRUE))
df <- df %>% 
  mutate(left_hs = rowMeans(select(., Q40_1:Q40_4), na.rm=TRUE))
df <- df %>% 
  mutate(sense_hs = rowMeans(select(., Q45_1:Q45_4), na.rm=TRUE))


summary(df$stock_hs)
summary(df$left_hs)
summary(df$sense_hs)

df$int_b1 <- df$int_b1 - 5
df$Q38 <- df$Q38 - 5
df$Q43 <- df$Q43 - 5
summary(df$freq_b1)
summary(df$int_b1)
summary(df$Q38)
table(df$freq_b1)

hsBeh <- df %>% 
  select(stock_hs, left_hs, sense_hs) %>% 
  describe() %>% 
  select(n, mean, sd, median, min, max)

behPerftab <- df %>% 
  select(int_b1, Q38, Q43) %>% 
  describe() %>% 
  select(n, mean, sd, median, min, max)

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
left_fw <- lm(Q14 ~ left_hs, data=df) # regression of leftover checking and waste of cooked foods
sense_fw <- lm(fw_total ~ sense_hs, data=df)

summary(left_fw)
plot(stock_fw)

abline(stock_fw)

ggplot(df, aes(x = stock_hs, y = fw_total)) +
  geom_point() + 
  stat_smooth(method="lm", col="red")


df %>% 
  select(hs_b1_1:hs_b1_4) %>% 
  cor(use="pairwise.complete.obs") %>% 
  alpha()

fw_habit_lm <- lm(fw_total ~ stock_hs + left_hs + sense_hs, data = dfc)
fw_habit_tobit <- censReg(fw_total ~ stock_hs + left_hs + sense_hs, data = dfc)
summary(fw_habit_tobit)
summary(fw_habit_lm)
anova(fw_habit_lm, fw_habit_tobit)
coef(stock_fw_tobit)

logLik(fw_habit_lm)
logLik(fw_habit_tobit)
vcov(stock_fw_tobit)

View(df$Q7_7_TEXT)

comments <- df %>% 
  select(open_det) %>% 
  filter(!is.na(.)) 

write.csv(comments, paste0(wd$output, "openComments.txt"), row.names = F, quote = F)

colnames(df)
