oppCountry <- df %>%
  select(starts_with("OPP_"), Country) 

oppSumm <- describeBy(oppCountry, oppCountry$Country, mat=T)  %>% 
  select(group1, vars, mean, sd) %>% 
  group_by(group1)

oppMelt <- melt(oppSumm, id.vars = c("group1", "vars"))

# Use dcast to pivot the data to the desired format
oppdf <- dcast(oppMelt, vars ~ group1 + variable, value.var = "value")

oppdf <- oppdf[-5,]

oppTot <- describeBy(df$totopp, df$Country, mat=T) %>% 
  select(group1, vars, mean, sd) %>% 
  group_by(group1)

oppTotMelt <- melt(oppTot, id.vars = c("group1", "vars"))
oppTotdf <- dcast(oppTotMelt, vars ~ group1 + variable, value.var = "value")

oppTotdf[1,1] <- "0"

oppTotdf$vars <- as.numeric(oppTotdf$vars)

oppdf <- bind_rows(oppdf, oppTotdf)

oppdfFlex <- flextable(oppdf)
oppdfFlex <- colformat_double(oppdfFlex, digits=2)

oppdfFlex <- add_header_row(oppdfFlex, values = c("","Austria", "Greece", "The Netherlands", "Spain", "Sweden", "United Kingdom"), colwidths = c(1,2,2,2,2,2,2))

save_as_docx(oppdfFlex, path = paste0(wd$output, "opportunityTable.docx"))