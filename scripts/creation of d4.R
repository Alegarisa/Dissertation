# iNCLUDING COVARIATES IN DATASET TO ADJUST FOR DEMO VARS


```{r}
all <- read_sav(here("data", "parent_hv1_youth_w1.sav"))

# view_df(all) # need to select covariates to include from this dataset 

# Inlcuded in d3:
ggplot(all, aes(factor(q36_hv1))) + 
  geom_bar() 

ggplot(all, aes(factor(q173_3_p1))) + 
  geom_bar()

# TO BE INCLUDED
all$q4_3_text_hv1 # parent's age -- MAKE IT DOUBLE

ggplot(all, aes(factor(participant_p1))) + 
  geom_bar() # Parent role (1 = mom, 2 = dad, 4 = other) -- FACTOR

ggplot(all, aes(as.double(q17_1_text_hv1))) + 
  geom_histogram() # child's age -- MAKE IT DOUBLE

ggplot(all, aes(factor(q231_y1))) + 
  geom_bar() # youth gender (1 = male, 2 = FEMALE) -- FACTOR

ggplot(all, aes(factor(q4_y1))) + 
  geom_bar() # self-reported youth grades -- FACTOR

ggplot(all, aes(factor(q2_y1))) + 
  geom_bar()# youth's grade (1 = 6th, 2 = 7th, 3 = 8th) -- FACTOR

selected <- all %>%
  select(1:4, participant_p1, q173_3_p1, q4_3_text_hv1, q17_1_text_hv1, q36_hv1, q231_y1, q4_y1, q2_y1)

```


# recoding missing variables as N/A
```{r include=FALSE}
# recoding missing values as N/A with function

# vector with missing values in dataset
missing_vals <- c(77, 99, -99)

# function that returns true if values in vector are equal to missing_vals. The function takes a vector x, and specified values of missing data
recode_missing <- function(x, missing_vals = c(77, 99, -99)) {
  test <- x %in% missing_vals
  ifelse(test, NA, x)
}

# function that recodes missing values to NA. The function takes a dataframe with variables with missing data, and specified values of missing data
recode_missing_df <- function(df, missing_vals = c(77, 99, -99)) {
  modify(df, ~recode_missing(.x, missing_vals)) # here uses the function created above
}

covariates_prev <- recode_missing_df(selected) # the function strips out variable labels
```

```{r}
covariates <- covariates_prev %>%
  rename(parent_role = participant_p1) %>%
  mutate(parent_role = as.factor(parent_role),
         parent_age = as.numeric(q4_3_text_hv1),
         parent_age = 2018 - parent_age,
         parent_ed = as.factor(q36_hv1),
         parent_eng_comf = as.factor(q173_3_p1),
         youth_age = as.numeric(q17_1_text_hv1),
         youth_gender = as.factor(q231_y1),
         youth_grades = as.factor(q4_y1),
         youth_grlevel = as.factor(q2_y1)) %>%
  select(1:4, parent_role, parent_age, parent_ed, parent_eng_comf, youth_age, youth_gender, youth_grades, youth_grlevel)

view_df(covariates)  # need to join this to d_w_scales
```


# Loading the dataset (only analysis vars)
```{r}
d <- read_sav(here("data", "d3.sav"))

view_df(d)
```


```{r}
d
```
# Scale creation (15 predictors, 1 outcome)
```{r}
d_w_scales <- d %>%
  rowwise() %>% 
  mutate(hwi_1 = mean(c(q86_p1, q87_p1, q88_p1, q89_p1, q94_p1, q96_p1, q97_p1), na.rm = TRUE),
         hwi_2 = mean(c(q84_p1, q85_p1, q93_p1, q95_p1), na.rm = TRUE),
         hwi_3 = mean(c(q90_p1, q91_p1, q98_p1), na.rm = TRUE),
         monit = mean(c(q137_p1, q139_p1, q140_p1, q143_p1, q144_p1), na.rm = TRUE),
         str_home = mean(c(q23_p1, q24_p1, q25_p1, q26_p1, q27_p1, q28_p1, q29_p1, q30_p1), na.rm = TRUE),
         convos_1 = mean(c(q76_p1, q77_p1, q78_p1, q79_p1), na.rm = TRUE),
         convos_2 = mean(c(q73_p1, q75_p1, q80_p1, q81_p1), na.rm = TRUE),
         convos_3 = mean(c(q68_p1, q69_p1, q70_p1, q71_p1), na.rm = TRUE),
         sbi_1 = mean(c(q31_p1, q32_p1, q33_p1, q34_p1), na.rm = TRUE),
         sbi_2 = mean(c(q36_p1, q37_p1, q38_p1, q39_p1, q40_p1), na.rm = TRUE),
         belong = mean(c(q47_p1, q48_p1, q51_p1, q52_p1, q53_p1), na.rm = TRUE),
         endorse = mean(c(q54_p1, q56_p1, q57_p1), na.rm = TRUE),
         value_ed = mean(c(q105_p1, q106_p1, q107_p1, q108_p1, q109_p1, q110_p1), na.rm = TRUE),
         rel_1 = mean(c(q41_p1, q58_p1, q62_p1, q63_p1, q64_p1, q65_p1, q66_p1, q67_p1), na.rm = TRUE),
         rel_2 = mean(c(q43_p1, q44_p1, q50_p1, q126_p1), na.rm = TRUE),
         engage = mean(c(q83_y1, q85_y1, q86_y1, q87_y1, q88_y1, q89_y1, q90_y1, q91_y1), na.rm = TRUE))

view_df(d_w_scales)
```

# joining parent_w1_hv1 with matching youth wave 1 dataset
```{r}
d4 <- right_join(d_w_scales, covariates)

view_df(d4)

# d4 %>%
#   haven::write_sav(here("data", "d4 - scales & covariates.sav")) # dataset created on July 18, 2022. Dissertation scales and covariates of interest
```

# grand mean center predictors (?)
```{r}
d_w_scales_c <- d_w_scales %>%
  mutate(hwi_1_c = hwi_1 - (round(mean(hwi_1, na.rm = TRUE), 2)),
         hwi_2_c = hwi_2 - mean(hwi_2, na.rm = TRUE),
         hwi_3_c = hwi_3 - mean(hwi_3, na.rm = TRUE),
         monit_c = monit - mean(monit, na.rm = TRUE),
         str_home_c = str_home - mean(str_home, na.rm = TRUE),
         convos_1_c = convos_1 - mean(convos_1, na.rm = TRUE),
         convos_2_c = convos_2 - mean(convos_2, na.rm = TRUE),
         convos_3_c = convos_3 - mean(convos_3, na.rm = TRUE),
         sbi_1_c = sbi_1 - mean(sbi_1, na.rm = TRUE),
         sbi_2_c = sbi_2 - mean(sbi_2, na.rm = TRUE),
         belong_c = belong - mean(belong, na.rm = TRUE),
         endorse_c = endorse - mean(endorse, na.rm = TRUE),
         value_ed_c = value_ed - mean(value_ed, na.rm = TRUE),
         rel_1_c = rel_1 - mean(rel_1, na.rm = TRUE),
         rel_2_c = rel_2 - mean(rel_2, na.rm = TRUE),
         ed_level = factor(q36_hv1),
         eng_comf = factor(q173_3_p1)) # this is not working. it gives me a zero in all the centered vars. Have tried multiple ways of using ()

# summary(d_w_scales)

# first row: 1.857143 - 2.704045 # -0.846902\
d_w_scales$hwi_1 - round(mean(d_w_scales$hwi_1, na.rm = TRUE), 2) # this works

d_w_scales_c <- d_w_scales %>%
  mutate(hwi_1_c = hwi_1 - round(mean(hwi_1, na.rm = TRUE), 2)) # this doesn't, first row of hwi_1_c = -0.002857143
```