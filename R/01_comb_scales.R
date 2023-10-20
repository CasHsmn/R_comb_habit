# SCALE ANALYSIS

# Descriptives of each COM-B scale
comb <- df %>% 
  select(CAP_1:aut_mot_1_4 & !Q1...29 & !CAP_DO_1:CAP_DO_5 & !OPP_DO_1:OPP_DO_4)

comb <- df %>% 
  select(CAP_1:aut_mot_1_4, !Q1...29, !CAP_DO_1:CAP_DO_5, !OPP_DO_1:OPP_DO_4)

comb <- na.omit(comb)

##### Create var for COM-B components
{
dfc <- dfc %>% 
  mutate(psycap = rowMeans(select(.,CAP_1:CAP_5)))
dfc <- dfc %>% 
  mutate(phyopp = rowMeans(select(.,OPP_1:OPP_2)))
dfc <- dfc %>% 
  mutate(socopp = rowMeans(select(.,OPP_3:OPP_4)))
dfc <- dfc %>% 
  mutate(refmot = rowMeans(select(.,ref_mot_1_1:ref_mot_2_7)))
dfc <- dfc %>% 
  mutate(autmot = rowMeans(select(.,aut_mot_1_1:aut_mot_1_4)))
}

comb_overall <- dfc %>% 
  select(psycap:autmot)

mean(df$psycap, na.rm=TRUE)
summary(df$psycap)
describe(df$psycap)
describe(df$phyopp)

comb_summary <- comb_overall %>% 
  describe() %>%
  select(mean, sd, median, n)


capalpha <- df %>% 
  select(CAP_1:CAP_5) %>% 
  alpha(na.rm=TRUE)

phyoppalp <- df %>% 
  select(OPP_1:OPP_2) %>% 
  alpha(na.rm=TRUE)

socoppalp <- df %>% 
  select(OPP_3:OPP_4) %>% 
  alpha(na.rm=TRUE)

refmotalp <- df %>% 
  select(ref_mot_1_1:ref_mot_2_7) %>% 
  alpha(na.rm=TRUE)

autmotalp <- df %>% 
  select(aut_mot_1_1:aut_mot_1_4) %>% 
  alpha(na.rm=TRUE) 


comb_summary$alpha <- c(capalpha$total$raw_alpha, phyoppalp$total$raw_alpha, socoppalp$total$raw_alpha, refmotalp$total$raw_alpha, autmotalp$total$raw_alpha)

rownames(comb_summary) <- c("Psychological Capability (5 items)", "Physical Opportunity (2 items)", "Social Opportunity (2 items)", "Reflective Motivation (15 items)", "Automatic Motivation (4 items)")

fa(df[, ref_mot_1_1:ref_mot_2_7])

df %>% 
  select(ref_mot_1_1:ref_mot_2_7) %>% 
  fa(nfactors=2, rotate="varimax")

df %>% 
  select(aut_mot_1_1:aut_mot_1_4) %>% 
  fa(nfactors=1, rotate="varimax")

ev_refmot <- fa_refmot$values

plot(1:length(ev_refmot), ev_refmot, type = "b", xlab = "Factor Number", ylab = "Eigenvalue", main = "Scree Plot")

alpha <- data.frame()


refmotcor <- df %>% 
  select(ref_mot_1_1:ref_mot_2_7) %>% 
  cor(use="pairwise.complete.obs")

View(refmotcor)

refmotcorp <- df %>% 
  select(ref_mot_1_1:ref_mot_2_7) %>% 
  cor.mtest(conf.level = .95)

refmotcorp <- cor.test()

corrplot(refmotcor, method="color", addCoef.col = "black", diag=FALSE, type="lower", tl.srt=45, tl.col="black", p.mat=refmotcorp$p, sig.level=.05, insig="blank", number.cex = .7)

corrplot(refmotcor, method="color",
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE )

df %>% 
  select(CAP_1:CAP_5) %>% 
  cor(use="pairwise.complete.obs")

df %>% 
  select(psycap, phyopp, socopp, refmot, autmot) %>% 
  cor(use="pairwise.complete.obs")


?cor



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
plot(comb_fw)



comb_fw = lm(fw_total ~ psycap + socopp + phyopp + refmot + autmot, data = dfc)
colnames(dfc)
socdem_fw = lm(fw_total ~ gender + adult + child + income + age, data=dfc)
full_fw = lm(fw_total ~ gender + adult + child + income + age + psycap + socopp + phyopp + refmot + autmot, data = dfc)
anova(full_fw)

summary(full_fw)

?lm

colnames(df)
summary(socdem_fw)
anova(socdem_fw)
summary(full_fw)
summary(comb_fw)

# TOBIT MODEL
tobit_fw_comb <- censReg(fw_total ~ psycap + socopp + phyopp + refmot + autmot, data=dfc)
fw_comb_tobit <- vglm(fw_total ~ psycap + socopp + phyopp + refmot + autmot, tobit(), data=df)
fw_soc_tobit <- vglm(fw_total ~ gender + adult + child + income + age, tobit(), data=df)

tidy.tobit <- function(x, ...) {
  class(x) <- "survreg"
  tidy(x, ...)
}

rm(tidy.tobit)

fit <- tobit(fw_total ~ psycap + socopp + phyopp + refmot + autmot, data=df)
fit %>%  summary(tidy)

class(fit)

class(fit) <- "survreg"

fit %>% 
  tidy()

pchisq(2*(logLik(fw_comb_tobit)-logLik(fw_soc_tobit)), df=2, lower.tail=FALSE)
summary(fw_comb_tobit)

dfc$yhat <- fitted(fw_comb_tobit)[,1]
dfc$rr <- resid(fw_comb_tobit, type="response")
dfc$rp <- resid(fw_comb_tobit, type="response")[,1]
par(mfcol=c(2,3))

tobit(fw_total ~ psycap + socopp + phyopp + refmot + autmot, data=df)
?tobit

with(dfc, {
  plot(yhat, rr, main = "Fitted vs Residuals")
  qqnorm(rr)
  plot(yhat, rp, main = "Fitted vs Pearson Residuals")
  qqnorm(rp)
  plot(fw_total, rp, main = "Actual vs Pearson Residuals")
  plot(fw_total, yhat, main = "Actual vs Fitted")
})
??tidy

(r <- with(dfc, cor(yhat, fw_total)))

coeffs(tobit_fw_comb)
table(tobit_fw_comb)

Coefficients(fw_comb_tobit)
summary(fw_comb_tobit)

anova(comb_fw, tobit_fw_comb)
logLik(tobit_fw_comb)
logLik(comb_fw)

plot(tobit_fw_comb)

# Model diagnostics
dfc$residualtob <- resid(tobit_fw_comb)
dfc$studrestob <- rstudent(tobit_fw_comb)
dfc$cooktob <- cooks.distance(tobit_fw_comb)
dfc$dfbetatob <- dfbeta(tobit_fw_comb)
dfc$residualtob <- dffits(tobit_fw_comb)
dfc$residualtob <- covratio(tobit_fw_comb)

dfc$largetobres <- dfc$residualtob > 2 | dfc$residualtob < -2

coef(tobit_fw_comb)
summary(tobit_fw_comb)

colnames(dfc)

ggplot(dfc)+
  geom_smooth(aes(x=psycap, y=fw_total), method="lm", se=FALSE, color="red")+
  geom_point(aes(x=psycap, y = fw_total), colour="red")+
  geom_smooth(aes(x=socopp, y=fw_total),  method="lm", se=FALSE, color="blue")+
  geom_point(aes(x=socopp, y = fw_total), colour="blue")+
  geom_smooth(aes(x=refmot, y=fw_total),  method="lm", se=FALSE, color="green")+
  geom_point(aes(x=refmot, y = fw_total), colour="green")

ggplot(df)+
  geom_smooth(aes(x=psycap, y=fw_total), se = FALSE, method="lm", color="#8ecae6")+
  geom_point(aes(x=psycap, y=fw_total), color="#8ecae6")

?geom_point

df <- data.frame(Component = c("Psychological Capability", "Physical Opportunity", "Social Opportunity", "Reflective Motivation", "Automatic Motivation"),
                 value = c(df$psycap, df$socopp, df$phyopp, df$refmot, df$autmot),
                 fw_total = )

comb_long <- df %>% 
  select(psycap:autmot, fw_total, UserLanguage) %>% 
  pivot_longer(
    cols=c(psycap, socopp, phyopp, refmot, autmot),
    names_to = "Component",
    values_to = "Rating"
  )

comb_labels <- c(
  "psycap" = "Psychological Capability",
  "socopp" = "Social Opportunity",
  "phyopp" = "Physical Opportunity",
  "refmot" = "Reflective Motivation",
  "autmot" = "Automatic Motivation"
)

ggplot(comb_long, aes(x = Rating, y = fw_total, color = Component)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(title = "Regression Lines for Predictors vs. fw_total",
       x = "Predictor Variable",
       y = "fw_total") +
  facet_wrap(~Component, labeller=labeller(Component = comb_labels))+
  guides(color="none")+
  theme_minimal()


hist(df$fw_total)
Boxplot(df$fw_total)

summary(df$psycap)

hist.comb <- ggplot(df, aes(phyopp)) + geom_histogram(aes(y=..density..), colour="black", fill="white")
hist.comb + stat_function(fun=dnorm, args=list(mean = mean(df$phyopp, na.rm=TRUE), sd=sd(df$phyopp, na.rm=TRUE)), colour="black", size=1)

qqplot.comb <- ggplot(dfc, aes(sample=phyopp))+stat_qq()+ stat_qq_line()
qqplot.comb


dfc <-  df %>% 
  select(ID, PROLIFIC_PID, CAP_1:Q45_4 & !Q1...29 & !CAP_DO_1:CAP_DO_5 & !OPP_DO_1:OPP_DO_4, age:income & !Q7_7_TEXT, fw_total:autmot)


colnames(dfc)
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
dwt(tobit_fw_comb)
1/vif(tobit_fw_comb)
vif(tobit_fw_comb)
mean(vif(tobit_fw_comb))
1/vif(comb_fw)
mean(vif(comb_fw))

psycap_fw <- lm(fw_total~psycap, data=df)
socopp_fw <- lm(fw_total~socopp, data=df)
phyopp_fw <- lm(fw_total~phyopp, data=df)
refmot_fw <- lm(fw_total~refmot, data=df)
autmot_fw <- lm(fw_total~autmot, data=df)

plot(psycap_fw)

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
sumtob <- coef(summary(tobit_fw_comb))


lmcoeff <- coef(summary(comb_fw))

lm(fw_total ~ psycap + socopp + phyopp + refmot + autmot, data=df) %>% 
  tidy()
     