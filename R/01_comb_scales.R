# COM-B SCALE ANALYSIS

##### Create var for COM-B components
{
df <- df %>% 
  mutate(psycap = rowMeans(select(.,CAP_1:CAP_5)))
df <- df %>% 
  mutate(phyopp = rowMeans(select(.,OPP_1:OPP_2)))
df <- df %>% 
  mutate(socopp = rowMeans(select(.,OPP_3:OPP_4)))
df <- df %>% 
  mutate(totopp = rowMeans(select(.,OPP_1:OPP_4)))
df <- df %>% 
  mutate(refmot = rowMeans(select(.,ref_mot_1_1:ref_mot_2_7)))
df <- df %>% 
  mutate(autmot = rowMeans(select(.,aut_mot_1_1:aut_mot_1_4)))
}

# table COM-B construct descriptives -----
# Capability
comb_overall <- df %>% 
  select(psycap:autmot)

cap_summ_lang <- df %>%
  select(CAP_1:CAP_5, Country) 

capSummNa <- describeBy(cap_summ_lang, cap_summ_lang$Country, mat=T)  %>% 
  select(group1, vars, n, mean, sd, min, max, range) %>% 
  group_by(group1)

melted_df <- melt(capSummNa, id.vars = c("group1", "vars"))

# Use dcast to pivot the data to the desired format
result_df <- dcast(melted_df, vars ~ group1 + variable, value.var = "value")

result_df <- result_df[-6,]


unique_countries <- unique(gsub("_.*", "", colnames(result_df)[grep("_n", colnames(result_df))]))


# Create separate flextables by country
tables_by_country <- list()

dataframes_by_country <- lapply(unique_countries, function(country) {
  # Filter the columns relevant to the current country
  relevant_cols <- grep(paste0(country, "_"), colnames(result_df))
  relevant_cols <- c(1, relevant_cols)  # Include the 'vars' column
  sub_df <- result_df[, relevant_cols]
  
  # Rename the columns by removing the country prefix
  new_names <- c("vars", gsub(paste0(country, "_"), "", colnames(sub_df)[-1]))
  colnames(sub_df) <- new_names
  
  return(sub_df)
})
names(dataframes_by_country) <- unique_countries

write_xlsx(dataframes_by_country, paste0(wd$output, "capabilityCountry.xlsx"))

result_df <- result_df %>% mutate_at(vars(-vars), funs(round(., 2)))

capCountryFlex <- flextable(result_df) %>% 
  colformat_double(digits=2)
save_as_docx(capCountryFlex, path = paste0(wd$output, "capabilityCountry.docx"))


write.csv(result_df, paste0(wd$output, "capCountry.csv"), row.names=F)


capall_summary <- df %>% 
  select(psycap) %>% 
  describe(na.rm=T) %>% 
  select(n, mean, sd)

capall_summary <- describe(df$psycap) %>% 
  select(n, mean, sd)

cap_summary <- bind_rows(cap_summary, capall_summary)

result_df$vars <- c("I have the skills to handle food to avoid food waste in my household", "I know what I can do to avoid wasting food in my household", "Avoiding food waste is not something I think about", "I have a plan how I can avoid wasting food", "I have too many things on my mind other than avoiding food waste")

capFlexOverall <- flextable(cap_summary, col_keys=c("item", "n", "mean", "sd")) %>% 
  colformat_double(j=c(3,4),digits=2) %>% 
  set_header_labels(item = "Item", mean = "M", sd = "SD") %>% 
  bold(i=6) %>% 
  hline(i=5)

# Opportunity
opp_summary <- df %>%
  select(OPP_1:OPP_4) %>% 
  describe() %>% 
  select(n, mean, sd)

socoppoppall_summary <- describe(df$socopp) %>% 
  select(n, mean, sd)
phyoppoppall_summary <- describe(df$phyopp) %>% 
  select(n, mean, sd)

opp_summary <- bind_rows(opp_summary, socoppoppall_summary, phyoppoppall_summary)

opp_summary$item <- c("I have time to take actions to avoid wasting food", 
                      "I have sufficient resources to avoid wasting food, such as storage containers, storage space, a fridge and freezer", 
                      "People who are important to me think I should avoid wasting food", 
                      "Most of my friends avoid wasting food",
                      "Social Opportunity",
                      "Physical Opportunity")

oppFlexOverall <- flextable(opp_summary, col_keys=c("item", "n", "mean", "sd")) %>% 
  colformat_double(j=c(3:4),digits=2) %>% 
  set_header_labels(item = "Item", mean = "M", sd = "SD") %>% 
  bold(i=6) %>% 
  hline(i=5)

save_as_docx("Capability table" = capFlexOverall, "Opportunity table" = oppFlexOverall, path = paste0(wd$output, "combTable.docx"))
  


comb_summary <- comb_overall %>% 
  describe() %>%
  select(mean, sd, median, n)

# calculate cornbach's alpha's

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
  select(aut_mot_1_1:aut_mot_1_2) %>% 
  alpha(na.rm=TRUE) 

# add alphas to summary table
rownames(comb_summary) <- c("Psychological Capability (5 items)", "Physical Opportunity (2 items)", "Social Opportunity (2 items)", "Reflective Motivation (15 items)", "Automatic Motivation (4 items)")

comb_summary$alpha <- c(capalpha$total$raw_alpha, phyoppalp$total$raw_alpha, socoppalp$total$raw_alpha, refmotalp$total$raw_alpha, autmotalp$total$raw_alpha)


# correlation matrices ----
capcor <- df %>% 
  select(CAP_1:CAP_4) %>% 
  cor(use="pairwise.complete.obs")
phyoppcor <- df %>% 
  select(OPP_1, OPP_2) %>% 
  cor(use="pairwise.complete.obs")
df %>% 
  select(OPP_3, OPP_4) %>% 
  cor(use="pairwise.complete.obs")
refmotcor <- df %>% 
  select(ref_mot_1_1:ref_mot_2_7) %>% 
  cor(use="pairwise.complete.obs")
autmotcor <- df %>% 
  select(aut_mot_1_1:aut_mot_1_4) %>% 
  cor(use="pairwise.complete.obs")


refcorplot <- corrplot(refmotcor, method="color", addCoef.col = "black", diag=FALSE, type="lower", tl.srt=45, tl.col="black", number.cex = .7)
capcorplot <- corrplot(capcor, method="color", addCoef.col = "black", diag=FALSE, type="lower", tl.srt=45, tl.col="black", number.cex = .7)
autmotcorplot <- corrplot(autmotcor, method="color", addCoef.col = "black", diag=FALSE, type="lower", tl.srt=45, tl.col="black", number.cex = .7)

# factor analyses of COM-b constructs

df %>% 
  select(ref_mot_1_1:ref_mot_2_7) %>% 
  fa(nfactors=2, rotate="varimax")

df %>% 
  select(aut_mot_1_1:aut_mot_1_4) %>% 
  fa(nfactors=1, rotate="varimax")

ev_refmot <- fa_refmot$values


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

alpha <- df %>% 
  select()

comb <- comb %>% mutate(psycap = rowMeans(select(., starts_with("CAP_"))),
                          socopp = rowMeans(select(., c(OPP_3:OPP_4))),
                          phyopp = rowMeans(select(., c(OPP_1:OPP_2))),
                          refmot = rowMeans(select(., starts_with(("ref_mot")))),
                          autmot = rowMeans(select(., starts_with(("aut_mot")))))

comb_fw = lm(fw_total ~ psycap + socopp + phyopp + refmot + autmot, data = dfc)
summary(comb_fw)
plot(comb_fw)



comb_fw = lm(fw_totl ~ psycap + socopp + phyopp + refmot + autmot, data = dfc)
colnames(df)
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

fit <- tobit(fw_total ~ psycap + socopp + phyopp + refmot + autmot, data=df)
fit %>%  summary(tidy)

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
  select(ID, PROLIFIC_PID, CAP_1:Q45_4 & !att_1, age:income & !Q7_7_TEXT, fw_total:autmot)


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

summary(lm(fw_total_log ~ psycap + socopp + phyopp + refmot + autmot, data=df))
censReg(fw_total_log ~ psycap + socopp + phyopp + refmot + autmot, data=df)
summary(censReg(fw_total ~ psycap + socopp + phyopp + refmot + autmot, data=df))
tobit_log_fw <- AER::tobit(fw_total_log ~ psycap + socopp + phyopp + refmot + autmot, data=dfc)

plot(lm(fw_total_log ~ psycap + socopp + phyopp + refmot + autmot, data=df))


dfc$restob <- resid(tobit_log_fw)

plot(fitted(tobit_log_fw), dfc$restob)
qqnorm(dfc$restob)

df$restob
