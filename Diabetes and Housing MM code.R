
#Load packages
library(tidyverse);library(odbc);library(glue);library(yaml);library(dplyr);
library(RCurl);library(broom);library(data.table);library(knitr)


options(warning.length = 5000)
----------------------------------------------------------------------------------------
#Connect to SQL Server

db_phclaims <- dbConnect(odbc::odbc(),"PHClaims51")


db_apde51 <- dbConnect(odbc(), "PH_APDEStore51")


#Bring in demog: filter for those who meet the inclusion criteria, Only need the data where people are allocated into a single bucket
mcaid_mcare_pha_elig_demo <- dbGetQuery(db_apde51, 
                                        "SELECT * FROM stage.mcaid_mcare_pha_elig_calyear WHERE pop = 1 and full_criteria_12 = 1 and year= 2017")


# Add in additional info/formating that can be useful
mcaid_mcare_pha_elig_demo <- mcaid_mcare_pha_elig_demo %>%
  mutate(
    # Add in plain-text enrollment
    enroll_type_text = case_when(
      enroll_type == "a" ~ "Housing, Medicaid, and Medicare (dual)",
      enroll_type == "hmd" & dual == 1 & !is.na(dual) ~ "Housing, Medicaid, and Medicare (dual)",
      enroll_type == "hmd" & (dual != 1 | is.na(dual)) ~ "Housing and Medicaid (not dual)",
      enroll_type == "hme" ~ "Housing and Medicare (not dual)",
      enroll_type == "md" & (dual != 1 | is.na(dual)) ~ "Medicaid only (not dual)",
      enroll_type == "md" & dual == 1 & !is.na(dual) ~ "Medicaid and Medicare (dual)",
      enroll_type == "mm" ~ "Medicaid and Medicare (dual)",
      enroll_type == "me" ~ "Medicare only (not dual)",
      enroll_type == "h" ~ "Housing only"),
    # Redo age groups to be more relevant to diabetes
    age_yr_diabetes = cut(age_yr, 
                        breaks = c(0, 4.999, 9.999, 17.999, 29.999, 49.999, 64.999, 74.999, 180), 
                        include.lowest = F, 
                        labels = c("<5", "5-9", "10-17", "18-29", "30-49", "50-64", "65-74", "75+"), 
                        ordered_result = T),
    
    # Make pha_agency a factor for better sorting
    pha_agency = fct_relevel(pha_agency, "Non-PHA", "KCHA", "SHA"),
    
    pha_subsidy = as.character(pha_subsidy),
    # Rename subsidy types and make a factor
    pha_subsidy = case_when(
      pha_subsidy == "TENANT BASED/SOFT UNIT" ~ "HCV",
      pha_subsidy == "HARD UNIT" ~ "PH",
      TRUE ~ pha_subsidy),
    pha_subsidy = fct_relevel(pha_subsidy, "Non-PHA", "HCV", "PH"))

  
#### CHRONIC CONDITIONS ####
### CCW Diabetes

#bring in ccw diabetes
ccw_diabetes <- dbGetQuery(db_phclaims, "SELECT DISTINCT * FROM final.mcaid_mcare_claim_ccw
                         WHERE ccw_desc = 'ccw_diabetes'and from_date <= '2017-12-31' and to_date >='2017-01-01'")

ccw_diabetes <- dbGetQuery(db_phclaims, "SELECT DISTINCT * FROM final.mcaid_mcare_claim_ccw
                         WHERE ccw_desc = 'ccw_diabetes'and from_date <= '2017-12-31'")

ccw_diabetes%>%
  summarise(n_distinct(id_apde))

ccw_diabetes <- ccw_diabetes %>% 
  mutate(id_apde = as.integer(id_apde)) %>%
  mutate_at(vars(from_date, to_date), list( ~ as.Date(.)))

# Reshape ccw_diabetes to make it easier to join to demog table
ccw_diabetes_long <- expand.grid(id_apde = unlist(distinct(ccw_diabetes, id_apde)),
                               year = 2017) %>%
  left_join(., ccw_diabetes, by = "id_apde") %>%
  mutate(diabetes = 1L) %>%
  select(-from_date, -to_date, -last_run) %>%
  distinct()

#### Join data together
ccw_diabetes_demog <- mcaid_mcare_pha_elig_demo%>%
 left_join(., ccw_diabetes_long, by = c("id_apde", "year"))%>%
              mutate(diabetes= replace_na(diabetes,0))%>%
  distinct()


#check distinct id_apde 522294 > 522493>522349>584734
ccw_diabetes_demog%>%  #does not match number of observations
  summarise(count=n_distinct(id_apde))

# Descriptive Table 1
----------------------------------------------------------------------------------------------
#overall distribution by PHA agency (denominator)
ccw_diabetes_demog %>% 
  group_by(pha_agency)%>%
  summarise(num = n_distinct(id_apde, na.rm = TRUE))%>%
  mutate(denom= sum(num), prp= num/denom*100)

ccw_diabetes_demog%>%
  group_by(diabetes, age_yr_diabetes,)%>%
  summarise(num = n_distinct(id_apde,na.rm = TRUE))%>%
  mutate(denom = sum(num), prp = num/denom*100)%>%
view()


#diabetes distribition by PHA Agency (numerator)
ccw_diabetes_demog%>%
  group_by(pha_agency)%>%
  summarise(num=sum(diabetes))

#numerator 51141>52551
sum(ccw_diabetes_demog$diabetes)

#diabetes distribtion by PHA Agency
ccw_diabetes_demog%>%
  group_by(pha_agency)%>%
  summarise(num = sum(diabetes),denom= n_distinct(id_apde),)%>%
  mutate( prop= num/denom*100 )%>%
  view()

#diabetes distribution by PHA or Non-PHA
ccw_diabetes_demog%>%
  group_by(pha)%>%
  summarise(num = sum(diabetes),denom= n_distinct(id_apde),)%>%
  mutate( prop= num/denom*100 )%>%
  view()

#age
ccw_diabetes_demog%>%
  group_by(pha,age_yr_diabetes)%>%
  summarise(num = sum(diabetes),denom= n_distinct(id_apde),)%>%
  mutate( prop= num/denom*100 )%>%
  view()


#gender
ccw_diabetes_demog %>% 
  group_by(pha_agency,gender_me) %>%
  summarise( num= n_distinct(id_apde)) %>%
mutate(denom= sum(num),prop= num/denom*100)%>%
  view()

#race and eth
ccw_diabetes_demog%>%
  group_by(pha,race_eth_me) %>%
  summarise(num=n_distinct(id_apde)) %>%
  group_by(pha) %>%
  mutate(denom= sum(num), prop= num/denom*100)%>%
  view()

#age Mean
ccw_diabetes_demog %>%
  group_by(pha_agency)%>%
  summarise(mean(age_yr))

#age Median
ccw_diabetes_demog %>%
  group_by(pha_agency)%>%
  summarise(median(age_yr))

ccw_diabetes_demog%>%
  group_by(pha_agency,age_yr)%>%
  summarise(num= n_distinct(id_apde))%>%
  group_by(pha_agency)%>%
  mutate(denom=sum(num), prop=num/denom*100)%>%
  view()

#mcaid
ccw_diabetes_mcaid <- ccw_diabetes_demog%>%
  filter(enroll_type %in% c("md", "hmd"))


ccw_diabetes_mcaid%>%
  group_by(pha_agency)%>%
  summarise(num = sum(diabetes),denom= n_distinct(id_apde),)%>%
  mutate( prop= num/denom*100 )%>%
  view()

#mcare
ccw_diabetes_mcare <- ccw_diabetes_demog%>%
  filter(enroll_type %in% c("me", "hme"))

ccw_diabetes_mcare%>%
  group_by(pha_agency)%>%
  summarise(num = sum(diabetes),denom= n_distinct(id_apde),)%>%
  mutate( prop= num/denom*100 )%>%
  view()

#regression analysis
-------------------------------------------------------------------------
diabetes_test_crude <- glm(diabetes ~ pha,
                           data= ccw_diabetes_demog,
                           family = "binomial")

summary(diabetes_test_crude)
exp(coef(diabetes_test_crude))
  
diabetes_overall_test <- glm(diabetes ~ pha + age_yr + gender_me + race_eth_me ,
                               data = ccw_diabetes_demog,
                               family = "binomial")

summary(diabetes_overall_test)
exp(coef(diabetes_overall_test))

diabetes_overall_test <- tidy(diabetes_overall_test, conf.int = T, exponentiate = T)

diabetes_overall_test_output <- diabetes_overall_test%>%
  mutate(model = rep("Diabetes comparing PHA/non-PHA", nrow(diabetes_overall_test))) %>%
  select(model, term, estimate, std.error, p.value, conf.low, conf.high)

#mcaid
-------------------------------
diabetes_test_md <- glm(diabetes ~ pha,
                             data= ccw_diabetes_mcaid,
                             family = "binomial")

summary(diabetes_test_md)
exp(coef(diabetes_test_md))

#mcare
--------------------------------
  diabetes_test_me <- glm(diabetes ~ pha,
                          data= ccw_diabetes_mcare,
                          family = "binomial")

summary(diabetes_test_me)
exp(coef(diabetes_test_me))


#data checks
------------------
glimpse(ccw_diabetes_demog)



unique(ccw_diabetes_demog$pt_tot)


mcaid_mcare_pha_elig_demo %>% group_by(id_apde, pha_agency) %>% summarise(cnt = n()) %>% group_by(cnt) %>% summarise(cnt2 = n())