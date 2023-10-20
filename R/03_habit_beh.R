# Behaviour frequency
colnames(df)

hist(df$int_b1)
hist(df$freq_b1)

dfc <- dfc %>% 
  mutate(stock_hs = rowMeans(select(., hs_b1_1:hs_b1_4), na.rm=TRUE))
dfc <- dfc %>% 
  mutate(left_hs = rowMeans(select(., Q40_1:Q40_4), na.rm=TRUE))
dfc <- dfc %>% 
  mutate(sense_hs = rowMeans(select(., Q45_1:Q45_4), na.rm=TRUE))


summary(df$stock_hs)
summary(df$left_hs)
summary(df$sense_hs)

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
