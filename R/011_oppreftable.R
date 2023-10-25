oppCountry <- df %>%
  select(OPP_1:OPP_4, Country) 

oppSumm <- describeBy(oppCountry, oppCountry$Country, mat=T)  %>% 
  select(group1, vars, n, mean, sd, min, max, range) %>% 
  group_by(group1)

oppMelt <- melt(oppSumm, id.vars = c("group1", "vars"))

# Use dcast to pivot the data to the desired format
oppdf <- dcast(oppMelt, vars ~ group1 + variable, value.var = "value")

oppdf <- result_df[-5,]

oppTot <- describeBy(df$totopp, df$Country, mat=T) %>% 
  select(group1, vars, n, mean, sd, min, max, range) %>% 
  group_by(group1)

oppTotMelt <- melt(oppTot, id.vars = c("group1", "vars"))
oppTotdf <- dcast(oppTotMelt, vars ~ group1 + variable, value.var = "value")

oppTotdf[1,1] <- "overall"

oppdf <- bind_rows(oppdf, oppTotdf)

colnames <- colnames(oppdf)

new_colnames <- sub("^.*?_", "", colnames)

colnames(oppdf) <- new_colnames
unique_countries <- unique(gsub("_.*", "", colnames(result_df)[grep("_n", colnames(result_df))]))

oppdf <- oppdf[-5,]

write.csv(oppdf, paste0(wd$output, "OpportunityTable.csv"), row.names=F)

colnames <- colnames(df)

# refmot

refCountry <- df %>%
  select(starts_with("ref_mot_"), Country) 

refSumm <- describeBy(refCountry, refCountry$Country, mat=T)  %>% 
  select(group1, vars, mean, sd) %>% 
  group_by(group1)

refMelt <- melt(refSumm, id.vars = c("group1", "vars"))

# Use dcast to pivot the data to the desired format
refdf <- dcast(refMelt, vars ~ group1 + variable, value.var = "value")

refdf <- refdf[-16,]

refTot <- describeBy(df$refmot, df$Country, mat=T) %>% 
  select(group1, vars, mean, sd) %>% 
  group_by(group1)

refTotMelt <- melt(refTot, id.vars = c("group1", "vars"))
refTotdf <- dcast(refTotMelt, vars ~ group1 + variable, value.var = "value")

refTotdf[1,1] <- "0"

refTotdf$vars <- as.numeric(refTotdf$vars)

refdf <- bind_rows(refdf, refTotdf)

refdfFlex <- flextable(refdf)
refdfFlex <- colformat_double(refdfFlex, digits=2)

refdfFlex <- add_header_row(refdfFlex, values = c("","Austria", "Greece", "The Netherlands", "Spain", "Sweden", "United Kingdom"), colwidths = c(1,2,2,2,2,2,2))

save_as_docx(refdfFlex, path = paste0(wd$output, "reflectiveTable.docx"))

save_as_html(refdfFlex, path = paste0(wd$output, "reflectiveTable.html"))

oppdf <- oppdf[-5,]

write.csv(oppdf, paste0(wd$output, "OpportunityTable.csv"), row.names=F)

colnames <- colnames(df)
