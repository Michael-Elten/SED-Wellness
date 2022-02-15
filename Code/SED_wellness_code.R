#SED Wellness Challenge code

library(googlesheets4)
library(tidyr)
library(dplyr)
library(stringr)
library(janitor)
library(readr)
library(lubridate)
library(ggplot2)
library(openxlsx)

### data import and cleaning ####

gs4_deauth()
sheet_url<-"ENTER_URL_HERE"

data_sheet_name<-"Form Responses 3"
EN_participant_info_sheet_name<-"EN_Responses"
FR_participant_info_sheet_name<-"FR_Responses"

#collecting and merging participant data
en_participants<-read_sheet(ss=sheet_url, sheet=EN_participant_info_sheet_name)%>%
  janitor::clean_names() %>%
  rename(email=please_enter_your_hc_email,
         name=please_enter_a_name_real_or_fake,
         bureau=what_bureau_are_you_in) %>%
  select(email, name, bureau)

fr_participants<-read_sheet(ss=sheet_url, sheet=FR_participant_info_sheet_name) %>%
  janitor::clean_names() %>%
  rename(email=veuillez_saisir_votre_e_mail_hc,
         name=veuillez_entrer_un_nom_vrai_ou_faux,
         bureau=indiquer_le_nom_do_votre_bureau) %>%
  mutate(bureau=ifelse(bureau=="BUREAU DE LA QUALIT? DE L'EAU ET DE L'AIR", "WATER AND AIR QUALITY BUREAU", 
                       ifelse(bureau =="BUREAU DE L'?VALUATION ET DU CONTR?LE DES SUBSTANCES NOUVELLES", "NEW SUBSTANCES ASSESSMENT AND CONTROL BUREAU",bureau))) %>%
  select(email, name, bureau)

all_participants=rbind(en_participants, fr_participants) %>%
  mutate(email=str_to_lower(email))

#check and filter for double-entries
dupl_emails<-all_participants %>%
  count(email) %>%
  filter(n>1)

all_participants_filtered<-all_participants%>%
  distinct(email, .keep_all=TRUE) 

rm(en_participants, fr_participants, all_participants, dupl_emails)
#check duplicate names
dupl_names<-all_participants_filtered %>%
  count(name) %>%
  filter(n>1)
rm(dupl_names)

#importing live data
data_raw<-read_sheet(ss=sheet_url, sheet = data_sheet_name)

data_cleaned<-data_raw %>%
  janitor::clean_names()%>%
  rename(email=please_enter_your_hc_email_address,
         week_start_date=what_is_the_date_of_the_monday_of_the_week_for_which_you_are_entering_data,
         mon_mins=please_enter_the_wellness_minutes_for_this_week_mon,
         tues_mins=please_enter_the_wellness_minutes_for_this_week_tues,
         wed_mins=please_enter_the_wellness_minutes_for_this_week_wed,
         thurs_mins=please_enter_the_wellness_minutes_for_this_week_thurs,
         fri_mins=please_enter_the_wellness_minutes_for_this_week_fri,
         sat_mins=please_enter_the_wellness_minutes_for_this_week_sat,
         sun_mins=please_enter_the_wellness_minutes_for_this_week_sun,
         percent_social=what_percentage_of_these_minutes_was_done_with_a_friend_family_or_pet,
         challenge_completed=did_you_do_ivys_bonus_challenge_this_week) %>%
  filter(!is.na(email)) %>% #filter out blank emails (can't be linked)
  filter(as.Date("2022-02-06")<week_start_date & week_start_date<as.Date("2022-03-06"))%>% #removing entries with start dates outside of the event dates
  mutate(week_start_date=floor_date(week_start_date, unit="week",week_start = 1),          #setting the start date to the Monday for cases of error
         email=str_to_lower(email)) 
  

#Excluded entries - to double check!
excluded_data<-data_raw %>%
  anti_join(data_cleaned, by=c("Timestamp"="timestamp"))


### need to pivot longer the data, and get the largest possible values for each day in a given week 
### (some people might enter 0s for days previously entered)

data_transformed<-data_cleaned %>%
  mutate(week_num=case_when(
    week_start_date==as.Date("2022-02-07") ~ 1,
    week_start_date==as.Date("2022-02-14") ~ 2,
    week_start_date==as.Date("2022-02-21") ~ 3,
    week_start_date==as.Date("2022-02-28") ~ 4,
    TRUE ~ 0)) %>%
  pivot_longer(cols = contains("mins"),
               names_to = "day",
               values_to="minutes") %>%
  mutate(week_start_date=as.Date(week_start_date),
         calendar_date=case_when(
    day=="mon_mins" ~ week_start_date + 1,
    day=="tues_mins" ~ week_start_date +2,
    day=="wed_mins" ~ week_start_date +3,
    day=="thurs_mins" ~ week_start_date +4,
    day=="fri_mins" ~ week_start_date +5,
    day=="sat_mins" ~ week_start_date +6,
    day=="sun_mins" ~ week_start_date +7)) %>%
  mutate(challenge_completed=ifelse(challenge_completed=="Yes",1,0))

#Creating daily dataset that has only one entry per person/day
daily_data<-data_transformed %>%
  arrange(desc(email, calendar_date, minutes)) %>%
  distinct(email, calendar_date, minutes, .keep_all=TRUE) %>%
  left_join(all_participants_filtered, by="email") %>%
  select(timestamp,email, name, bureau, week_num, calendar_date, minutes, percent_social, challenge_completed)

#Creating dataset with info on percent social and challenge completed by week (takes last entered data for a given week)
weekly_info<-daily_data %>%
  arrange(desc(email, week_num, timestamp))%>%
  distinct(email, week_num, .keep_all=TRUE) %>%
  select(email, name, bureau, week_num, percent_social, challenge_completed)

#Calculating weekly adjusted total minutes per person per week
weekly_data<-daily_data %>%
  group_by(email, week_num)%>%
  summarise(weekly_minutes=sum(minutes)) %>%
  ungroup() %>%
  left_join(weekly_info, by=c("email","week_num")) %>%
  mutate(social_bonus=0.5*weekly_minutes*(percent_social/100),
         challenge_bonus=challenge_completed*250,
         adjusted_weekly_minutes=weekly_minutes+social_bonus+challenge_bonus,
         weekly_social_minutes=weekly_minutes*(percent_social/100),
         weekly_solo_minutes=weekly_minutes-weekly_social_minutes) %>%
  arrange(week_num,desc(adjusted_weekly_minutes)) %>%
  select(email, name, bureau, week_num, weekly_minutes, social_bonus, challenge_bonus, adjusted_weekly_minutes, weekly_social_minutes, weekly_solo_minutes)

########## Visualizations    #####

#histogram of week

plot_histogram<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    weekly_data <-weekly_data %>%
      filter(week_num==week_number)
    
    title=str_c("Histogram for week number ",week_number)
  } else{
    title="Histogram for all weeks combined"
  }
  
  ggplot(data=weekly_data, aes(y=adjusted_weekly_minutes))+
    geom_histogram(bins = 15)+
    coord_flip() +
    labs(title=title)+
    theme_bw()
}


plot_histogram(week_number = 1)


########## Helpful Functions #####

## Those yet to submit anything for a given week!
no_submissions<-function(week_number=""){
if (is.numeric(week_number)==TRUE){
weekly_data <-weekly_data %>%
  filter(week_num==week_number)
}
  
missing_data<-all_participants_filtered %>%
  anti_join(weekly_data, by="email") %>%
  select(email)

return(missing_data)

}
all_week_missing_list<-no_submissions()
week_1_missing_list<-no_submissions(week_number = 1)
week_2_missing_list<-no_submissions(week_number = 2)
week_3_missing_list<-no_submissions(week_number = 3)
week_4_missing_list<-no_submissions(week_number = 4)


## Highest ranking team by week!

## to be written once teams are determined



## Stats by bureau
bureau_stats<-function(week_number=""){
if (is.numeric(week_number)==TRUE){
    weekly_data <-weekly_data %>%
      filter(week_num==week_number)
}
  weekly_data %>%
    group_by(bureau)%>%
    summarise(count=n(),
              avg_adj_weekly_minutes=mean(adjusted_weekly_minutes),
              proportion_challenge_completed=sum((challenge_bonus/250)/count)*100) %>%
    arrange(desc(avg_adj_weekly_minutes))
  
}

bureau_stats(week_number = 1)


#### Putting things together ####

export_data<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    weekly_data <-weekly_data %>%
      filter(week_num==week_number)
  }
  stats_by_bureau<-bureau_stats(week_number = week_number)
  no_submission_list<-no_submissions(week_number = week_number)
  
  sheet_list<-list("weekly data"=weekly_data, "bureau stats"=stats_by_bureau, "no submissions"=no_submission_list)
  write.xlsx(sheet_list, "data_export.xlsx")
}
export_data(week_number = 1)


write.csv(weekly_data, "week_1_data_15FEB2022.csv")
write_csv(week_1_missing_list, "./data/week_1_missing_15FEB2022.csv")
