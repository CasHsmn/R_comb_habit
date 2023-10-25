# DEMOGRAPHICS

# Sample characteristics table
samTable <- table1(~ gender + age + adult + child + edu + employ + income + fw_total | Country, data=df, overall=c(left="Total"))
write.csv2(samTable, paste0(wd$output, "sampleTable.csv"), quote = F, sep = ";")

save_as_docx(samTable,path = paste0(wd$output, "sampleTable.docx"))