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





