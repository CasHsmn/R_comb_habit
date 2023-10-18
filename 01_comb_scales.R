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
  mutate(phycap = rowSums(select(.,CAP_1:CAP_5)))
df <- df %>% 
  mutate(phyopp = rowSums(select(.,OPP_1:OPP_2)))
df <- df %>% 
  mutate(socopp = rowSums(select(.,OPP_3:OPP_4)))
df <- df %>% 
  mutate(refmot = rowSums(select(.,ref_mot_1_1:ref_mot_2_7)))
df <- df %>% 
  mutate(phycap = rowSums(select(.,aut_mot_1_1:aut_mot_1_4)))
}

df %>% 
  select(CAP_1:CAP_5) %>% 
  cronbach.alpha(na.rm=TRUE)

df %>% 
  select(OPP_1:OPP_2) %>% 
  cronbach.alpha(na.rm=TRUE)

df %>% 
  select(OPP_3:OPP_4) %>% 
  cronbach.alpha(na.rm=TRUE)

df %>% 
  select(ref_mot_1_1:ref_mot_2_7) %>% 
  cronbach.alpha(na.rm=TRUE)

df %>% 
  select(aut_mot_1_1:aut_mot_1_4) %>% 
  cronbach.alpha(na.rm=TRUE)

comb <- comb %>% mutate(psycap = rowMeans(select(., starts_with("CAP_"))),
                          socopp = rowMeans(select(., c(OPP_3:OPP_4))),
                          phyopp = rowMeans(select(., c(OPP_1:OPP_2))),
                          refmot = rowMeans(select(., starts_with(("ref_mot")))),
                          autmot = rowMeans(select(., starts_with(("aut_mot")))))

table(df$int_b1)
table(df$Q38)
table(df$Q43)

summary(df$Q14)

##### FUNCTIONS FW CONVERSION 
{serving_spoon <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 25,
      {{col_name}} == 2 ~ 75,
      {{col_name}} == 3 ~ 150,
      {{col_name}} == 4 ~ 250,
      {{col_name}} == 5 ~ 350,
      TRUE ~ {{col_name}}
    ))
}

fruit_piece <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 25,
      {{col_name}} == 2 ~ 50,
      {{col_name}} == 3 ~ 100,
      {{col_name}} == 4 ~ 300,
      {{col_name}} == 5 ~ 500,
      TRUE ~ {{col_name}}
    ))
}

portion <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 75,
      {{col_name}} == 2 ~ 150,
      {{col_name}} == 3 ~ 375,
      {{col_name}} == 4 ~ 675,
      {{col_name}} == 5 ~ 900,
      TRUE ~ {{col_name}}
    ))
}
sandwich_portion <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 10,
      {{col_name}} == 2 ~ 20,
      {{col_name}} == 3 ~ 50,
      {{col_name}} == 4 ~ 90,
      {{col_name}} == 5 ~ 120,
      TRUE ~ {{col_name}}
    ))
}

bread <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 18,
      {{col_name}} == 2 ~ 35,
      {{col_name}} == 3 ~ 400,
      {{col_name}} == 4 ~ 800,
      {{col_name}} == 5 ~ 1200,
      TRUE ~ {{col_name}}
    ))
}

egg <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 30,
      {{col_name}} == 2 ~ 60,
      {{col_name}} == 3 ~ 150,
      {{col_name}} == 4 ~ 270,
      {{col_name}} == 5 ~ 360,
      TRUE ~ {{col_name}}
    ))
}

dairy_portion <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 38,
      {{col_name}} == 2 ~ 150,
      {{col_name}} == 3 ~ 500,
      {{col_name}} == 4 ~ 1000,
      {{col_name}} == 5 ~ 2000,
      TRUE ~ {{col_name}}
    ))
}

sauce <- function(data, col_name){
  data %>% 
    mutate({{col_name}} := case_when(
      {{col_name}} == 1 ~ 10,
      {{col_name}} == 2 ~ 30,
      {{col_name}} == 3 ~ 90,
      {{col_name}} == 4 ~ 225,
      {{col_name}} == 5 ~ 675,
      TRUE ~ {{col_name}}
    ))
}

df <- df %>% 
  serving_spoon(Q14) %>% 
  serving_spoon(Q3...88) %>% 
  serving_spoon(Q5...90) %>% 
  fruit_piece(Q4...89) %>% 
  portion(Q8...91) %>% 
  sandwich_portion(Q9) %>% 
  bread(Q10) %>% 
  dairy_portion(Q12) %>% 
  egg(Q13) %>% 
  sauce(Q15)

df <- df %>% 
  mutate(fw_total = rowSums(select(., Q14:Q15), na.rm=TRUE))
}

