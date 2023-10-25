capCountry <- df %>%
  select(starts_with("CAP_"), Country) 

capSumm <- describeBy(capCountry, capCountry$Country, mat=T)  %>% 
  select(group1, vars, mean, sd) %>% 
  group_by(group1)

capMelt <- melt(capSumm, id.vars = c("group1", "vars"))

# Use dcast to pivot the data to the desired format
capdf <- dcast(capMelt, vars ~ group1 + variable, value.var = "value")

capdf <- capdf[-6,]

capTot <- describeBy(df$psycap, df$Country, mat=T) %>% 
  select(group1, vars, mean, sd) %>% 
  group_by(group1)

capTotMelt <- melt(capTot, id.vars = c("group1", "vars"))
capTotdf <- dcast(capTotMelt, vars ~ group1 + variable, value.var = "value")

capTotdf[1,1] <- "0"

capTotdf$vars <- as.numeric(capTotdf$vars)

capdf <- bind_rows(capdf, capTotdf)

capdfFlex <- flextable(capdf)
capdfFlex <- colformat_double(capdfFlex, digits=2)

capdfFlex <- add_header_row(capdfFlex, values = c("","Austria", "Greece", "The Netherlands", "Spain", "Sweden", "United Kingdom"), colwidths = c(1,2,2,2,2,2,2))

save_as_docx(capdfFlex, path = paste0(wd$output, "capabilityTable.docx"))