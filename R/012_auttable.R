autCountry <- df %>%
  select(starts_with("aut_mot_"), Country) 

autSumm <- describeBy(autCountry, autCountry$Country, mat=T)  %>% 
  select(group1, vars, mean, sd) %>% 
  group_by(group1)

autMelt <- melt(autSumm, id.vars = c("group1", "vars"))

# Use dcast to pivot the data to the desired format
autdf <- dcast(autMelt, vars ~ group1 + variable, value.var = "value")

autdf <- autdf[-5,]

autTot <- describeBy(df$autmot, df$Country, mat=T) %>% 
  select(group1, vars, mean, sd) %>% 
  group_by(group1)

autTotMelt <- melt(autTot, id.vars = c("group1", "vars"))
autTotdf <- dcast(autTotMelt, vars ~ group1 + variable, value.var = "value")

autTotdf[1,1] <- "0"

autTotdf$vars <- as.numeric(autTotdf$vars)

autdf <- bind_rows(autdf, autTotdf)

autdfFlex <- flextable(autdf)
autdfFlex <- colformat_double(autdfFlex, digits=2)

autdfFlex <- add_header_row(autdfFlex, values = c("","Austria", "Greece", "The Netherlands", "Spain", "Sweden", "United Kingdom"), colwidths = c(1,2,2,2,2,2,2))

save_as_docx(autdfFlex, path = paste0(wd$output, "automaticTable.docx"))