# SCALE ANALYSIS

# Descriptives of each COM-B scale
comb <- df %>% 
  select(CAP_1:aut_mot_1_4 & !Q1...29 & !CAP_DO_1:CAP_DO_5 & !OPP_DO_1:OPP_DO_4)

comb <- df %>% 
  select(CAP_1:aut_mot_1_4, !Q1...29, !CAP_DO_1:CAP_DO_5, !OPP_DO_1:OPP_DO_4)

comb <- na.omit(comb)

##### Create var for COM-B components
{
df <- df %>% 
  mutate(psycap = rowSums(select(.,CAP_1:CAP_5)))
df <- df %>% 
  mutate(phyopp = rowSums(select(.,OPP_1:OPP_2)))
df <- df %>% 
  mutate(socopp = rowSums(select(.,OPP_3:OPP_4)))
df <- df %>% 
  mutate(refmot = rowSums(select(.,ref_mot_1_1:ref_mot_2_7)))
df <- df %>% 
  mutate(autmot = rowSums(select(.,aut_mot_1_1:aut_mot_1_4)))
}



alpcap <- df %>% 
  select(CAP_1:CAP_5) %>% 
  alpha(na.rm=TRUE)

df %>% 
  select(OPP_1:OPP_2) %>% 
  alpha(na.rm=TRUE) %>% 
  pull(total$raw_alpha)

df %>% 
  select(OPP_3:OPP_4) %>% 
  alpha(na.rm=TRUE)

df %>% 
  select(ref_mot_1_1:ref_mot_2_7) %>% 
  alpha(na.rm=TRUE)

alpha <- data.frame()


df %>% 
  select(ref_mot_1_1:ref_mot_2_7) %>% 
  cor(use="pairwise.complete.obs")

df %>% 
  select(CAP_1:CAP_5) %>% 
  cor(use="pairwise.complete.obs")

df %>% 
  select(psycap, phyopp, socopp, refmot, autmot) %>% 
  cor(use="pairwise.complete.obs")


?cor

df %>% 
  select(aut_mot_1_1:aut_mot_1_4) %>% 
  alpha(na.rm=TRUE) 

alpha <- df %>% 
  select()

?pull

comb <- comb %>% mutate(psycap = rowMeans(select(., starts_with("CAP_"))),
                          socopp = rowMeans(select(., c(OPP_3:OPP_4))),
                          phyopp = rowMeans(select(., c(OPP_1:OPP_2))),
                          refmot = rowMeans(select(., starts_with(("ref_mot")))),
                          autmot = rowMeans(select(., starts_with(("aut_mot")))))

comb_fw = lm(fw_total ~ psycap + socopp + phyopp + refmot + autmot, data = dfc)
summary(comb_fw)


hist.comb <- ggplot(df, aes(phyopp)) + geom_histogram(aes(y=..density..), colour="black", fill="white")
hist.comb + stat_function(fun=dnorm, args=list(mean = mean(df$phyopp, na.rm=TRUE), sd=sd(df$phyopp, na.rm=TRUE)), colour="black", size=1)

qqplot.comb <- ggplot(dfc, aes(sample=phyopp))+stat_qq()+ stat_qq_line()
qqplot.comb

dfc <-  df %>% 
  select(ID, PROLIFIC_PID, CAP_1:Q45_4 & !Q1...29 & !CAP_DO_1:CAP_DO_5 & !OPP_DO_1:OPP_DO_4, fw_total:psycap)

colnames(df)

dfc <- dfc[complete.cases(dfc), ]

lm.beta(comb_fw)
confint(comb_fw)
dfc$residuals <- resid(comb_fw)
dfc$stdres <- rstandard(comb_fw)
dfc$studres <- rstudent(comb_fw)
dfc$cook <- cooks.distance(comb_fw)
dfc$dfbeta <- dfbeta(comb_fw)
dfc$dffits <- dffits(comb_fw)
dfc$leverage <- hatvalues(comb_fw)
dfc$covrat <- covratio(comb_fw)

dfc$large.res <- dfc$stdres > 2 | dfc$stdres < -2
sum(dfc$large.res)

dfc %>% 
  filter(large.res) %>% 
  select(fw_total:psycap, stdres) %>% 
  print(n=26)

dfc %>% 
  filter(large.res) %>% 
  select(cook, leverage, covrat, stdres) %>% 
  print(n=30)

2*mean(dfc$leverage)

dwt(comb_fw)
1/vif(comb_fw)
mean(vif(comb_fw))

hist(dfc$studres)
plot(comb_fw)



alpha <- data.frame(
  com_b = c("Physical Capability", "Physical Opportunity", "Social Opportunity", "Reflective Motivation", "Automatic Motivation"),
  alpha = numeric(5)
)

column_ranges <- list(
  CAP = c("CAP_1", "CAP_2", "CAP_3", "CAP_4", "CAP_5"),
  OPP = c("OPP_1", "OPP_2"),
  OPP3_4 = c("OPP_3", "OPP_4"),
  ref_mot = c(paste0("ref_mot_1_", 1:8), paste0("ref_mot_2_", 1:7)),
  aut_mot = c(paste0("aut_mot_1_", 1:4))
)
