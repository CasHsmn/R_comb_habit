# fw quantification

fw <- df %>% 
  select(Q1_28:Q15)

fw_cat <- df %>% 
  select(Q1_28:Q1_31)

fw_num <- df %>% 
  select(Q14:Q15)

fw_summ <- fw_cat %>% 
  summarise(across(everything(), ~sum(. == 1, na.rm = TRUE))) %>% 
  arrange(desc(.))

# Convert the dataframe to a long format
fw_summ_long <- pivot_longer(fw_summ, everything(), names_to = "Variable", values_to = "Value")

food_label <- data.frame(
  Variable = c("Q1_28", "Q1_17", "Q1_18", "Q1_19", "Q1_22", "Q1_23", "Q1_24", "Q1_26", "Q1_27", "Q1_29", "Q1_31"),
  Label = c(
    "Cooked Food", "Vegetables", "Fruit", "Potato",
    "Meat (substitute), fish", "Sandwich fillings", "Bread",
    "Dairy", "Eggs", "Condiments and sauces", "None of the above"
  )
)

# Create a bar graph with custom labels
ggplot(data = fw_summ_long, aes(x = reorder(Variable, -Value), y = Value)) +
  geom_bar(stat = "identity") +
  geom_text(data = food_label, aes(x = Variable, y = -10, label = Label), size = 3, color = "black") +
  labs(
    title = "Bar Graph with Custom Labels",
    x = "Food Category",
    y = "Count"
  ) +
  theme(legend.position = "none")


#### Food total
food_totals <- data.frame(
  Food = colnames(fw_num),
  Count = colSums(fw_num, na.rm = TRUE)
)

ggplot(data = food_totals, aes(x = Food, y = Count, label = Food)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label(Food)), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Total Count of Food Items", x = "Food Item", y = "Total Count") +
  theme_minimal()


label(fw_summ$Q1_28)   <- "Cooked Food"
label(fw_summ$Q1_17) <- "Vegetables"
label(fw_summ$Q1_18) <- "Fruit"
label(fw_summ$Q1_19) <- "Potato"
label(fw_summ$Q1_22)   <- "Meat (substitute), fish"
label(fw_summ$Q1_23) <- "Sandwich fillings"
label(fw_summ$Q1_24) <- "Bread"
label(fw_summ$Q1_26) <- "Dairy"
label(fw_summ$Q1_27)   <- "Eggs"
label(fw_summ$Q1_29) <- "Condiments and sauces"
label(fw_summ$Q1_31) <- "None of the above"

label(df$Q14)   <- "Cooked Food"
label(df$Q3...88) <- "Vegetables"
label(df$Q4...89) <- "Fruit"
label(df$Q5...90) <- "Potato"
label(df$Q8...91)   <- "Meat (substitute), fish"
label(df$Q9) <- "Sandwich fillings"
label(df$Q10) <- "Bread"
label(df$Q12) <- "Dairy"
label(df$Q13)   <- "Eggs"
label(df$Q15) <- "Condiments and sauces"


colnames(df)



