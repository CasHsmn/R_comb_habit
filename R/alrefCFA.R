regdf <- Regression_Dataset[-c(1:2),]

regdf <- regdf %>%
  rename_with(~ gsub("Psych Cap\\.\\.\\.", "psycap_", .), starts_with("Psych Cap..")) %>%
  rename_with(~ gsub("Phys Opp\\.\\.\\.", "phyopp_", .), starts_with("Phys Opp..")) %>%
  rename_with(~ gsub("Soc Opp\\.\\.\\.", "socopp_", .), starts_with("Soc Opp..")) %>%
  rename_with(~ gsub("Aut Mot\\.\\.\\.", "autmot_", .), starts_with("Aut Mot..")) %>%
  rename_with(~ gsub("Ref Mot\\.\\.\\.", "refmot_", .), starts_with("Ref Mot.."))

regdf <- regdf %>% 
  mutate_all(as.numeric)

colnames(regdf)


reg_comb <- '
cap =~ psycap_2 + psycap_3 + psycap_9 + psycap_20
phyopp =~ phyopp_4 + phyopp_5 + phyopp_10 + phyopp_17 + phyopp_18 + phyopp_21
socopp =~ socopp_7 + socopp_27 + socopp_29
refmot =~ refmot_8 + refmot_12 + refmot_15 + refmot_16 + refmot_19 + refmot_22 + refmot_25+ refmot_28 + refmot_30 + refmot_32 + refmot_33
autmot =~  autmot_6 + autmot_11 + autmot_14 + autmot_23 + autmot_24
'
regdf <- as.numeric(reg.df)

regdf %>% 
  select(starts_with("phyopp_")) %>% 
  alpha(na.rm=TRUE)

cfa_reg_comb <- cfa(reg_comb, data=regdf)
fitmeasures(cfa_reg_comb, c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'srmr', 'BIC'))
