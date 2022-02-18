#SED Wellness Challenge code

library(googlesheets4)
library(tidyr)
library(dplyr)
library(stringr)
library(janitor)
library(readr)
library(lubridate)
library(ggplot2)

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
  select(email, name, bureau, team)

fr_participants<-read_sheet(ss=sheet_url, sheet=FR_participant_info_sheet_name) %>%
  janitor::clean_names() %>%
  rename(email=veuillez_saisir_votre_e_mail_hc,
         name=veuillez_entrer_un_nom_vrai_ou_faux,
         bureau=indiquer_le_nom_do_votre_bureau) %>%
  mutate(bureau=ifelse(bureau=="BUREAU DE LA QUALIT? DE L'EAU ET DE L'AIR", "WATER AND AIR QUALITY BUREAU", 
                       ifelse(bureau =="BUREAU DE L'?VALUATION ET DU CONTR?LE DES SUBSTANCES NOUVELLES", "NEW SUBSTANCES ASSESSMENT AND CONTROL BUREAU",bureau))) %>%
  select(email, name, bureau, team)

all_participants=rbind(en_participants, fr_participants) %>%
  mutate(email=str_to_lower(email),
         bureau=as.factor(bureau),
         team=as.factor(team))

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
  select(timestamp,email, name, bureau, team, week_num, calendar_date, minutes, percent_social, challenge_completed)

#Creating dataset with info on percent social and challenge completed by week (takes last entered data for a given week)
weekly_info<-daily_data %>%
  arrange(desc(email, week_num, timestamp))%>%
  distinct(email, week_num, .keep_all=TRUE) %>%
  
  select(email, name, bureau, team, week_num, percent_social, challenge_completed)

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
  select(email, name, bureau, team, week_num, weekly_minutes, social_bonus, challenge_bonus, adjusted_weekly_minutes, weekly_social_minutes, weekly_solo_minutes)

########## Summarizing data #####

#Stats by team
weekly_data %>%
  group_by(team)%>%
  summarise(members_submitted=n(),
            min_minutes=min(adjusted_weekly_minutes),
            max_minutes=max(adjusted_weekly_minutes),
            median_minutes=median(adjusted_weekly_minutes),
            average_minutes=mean(adjusted_weekly_minutes)) %>%
  arrange(desc(average_minutes))


########## Helpful Functions #####

## Those yet to submit anything for a given week!
list_no_submissions<-function(week_number=""){
if (is.numeric(week_number)==TRUE){
weekly_data <-weekly_data %>%
  filter(week_num==week_number)
}
  
missing_data<-all_participants_filtered %>%
  anti_join(weekly_data, by="email") %>%
  select(email, name, bureau, team)

return(missing_data)

}


#### Putting things together ####

compile_data<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    weekly_data <-weekly_data %>%
      filter(week_num==week_number)
  }
  
}


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


plot_boxplot_by_team<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    weekly_data <-weekly_data %>%
      filter(week_num==week_number)
    
    title=str_c("Boxplot for week number ",week_number)
  } else{
    title="Boxplot for all weeks combined"
  }
  

  ggplot(data=weekly_data)+
    geom_boxplot(aes(x=reorder(team, adjusted_weekly_minutes), y=adjusted_weekly_minutes, group=team, fill=team), outlier.shape = NA,alpha=0.75)+
    geom_point(aes(x=team, y=adjusted_weekly_minutes),position=position_jitter(w = 0.1, h = 0))+
    labs(title=title,
         y="Minutes of activity",
         x="Team name")+
    theme_bw()
}
plot_boxplot_by_team(week_number=1)
  
plot_bargraph_by_team<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    weekly_data <-weekly_data %>%
      filter(week_num==week_number)
    
    title=str_c("Bar graph for week number ",week_number)
  } else{
    title="Bar graph for all weeks combined"
  }
  
  grouped_data<-weekly_data %>%
    group_by(team)%>%
    summarise(total_adj_minutes=sum(adjusted_weekly_minutes))
  
  ggplot(data=grouped_data)+
    geom_bar(aes(x=reorder(team, total_adj_minutes),y=total_adj_minutes, fill=team),stat="identity")+
    geom_text(aes(x=team, y=total_adj_minutes+150, label=round(total_adj_minutes)))+
    labs(title=title,
         y="Wellness Points",
         x="Team")+
    theme_bw()
}
plot_bargraph_by_team(week_number=1)

###### Individual awards ####

#individual stats to calculate:


# Day of week champion
#Sun=1,Mon=2,Wed=3, etc.

day_of_week_champion<-function(week_number="",week_day=""){
  if (is.numeric(week_number)==TRUE){
    daily_data <-daily_data %>%
      filter(week_num==week_number)
  }
  daily_data %>%
    filter(wday(calendar_date)==week_day)%>%
    arrange(desc(minutes))%>%
    select(email, name, bureau, team, minutes)%>%
    head(10)
  
}
day_of_week_champion(week_number=1, week_day = 2)

# Hot out of the gate (Most hours Mon-Wed)
hot_out_the_gate<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    daily_data <-daily_data %>%
      filter(week_num==week_number)
  }
  daily_data %>%
    mutate(part_of_week=ifelse(wday(calendar_date) %in% c(2,3,4), "first_part", "second_part"))%>%
    group_by(email, name, bureau, team, part_of_week)%>%
    summarise(total_minutes=sum(minutes))%>%
    pivot_wider(names_from = part_of_week, values_from=total_minutes)%>%
    mutate(dif=first_part-second_part)%>%
    arrange(desc(dif))%>%
    select(email, name, bureau, team, dif)%>%
    head(10)
  
}
hot_out_the_gate(week_number=1)

# Strong Finish (Most hours Thurs-Sun)
strong_finish<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    daily_data <-daily_data %>%
      filter(week_num==week_number)
  }
  daily_data %>%
    mutate(part_of_week=ifelse(wday(calendar_date) %in% c(2,3,4), "first_part", "second_part"))%>%
    group_by(email, name, bureau, team, part_of_week)%>%
    summarise(total_minutes=sum(minutes))%>%
    pivot_wider(names_from = part_of_week, values_from=total_minutes)%>%
    mutate(dif=first_part-second_part)%>%
    arrange(dif)%>%
    select(email, name, bureau, team, dif)%>%
    head(10)
  
}
strong_finish (week_number=1)

# Social Butterfly (most hours done socially)
social_butterfly<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    weekly_data <-weekly_data %>%
      filter(week_num==week_number)
  }
  weekly_data %>%
    arrange(desc(weekly_social_minutes))%>%
    select(email, name, bureau, team, weekly_social_minutes)%>%
    head(10)
}
social_butterfly(week_number=1)

# Active hermit (most hours done solo)
active_hermit<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    weekly_data <-weekly_data %>%
      filter(week_num==week_number)
  }
  weekly_data %>%
    arrange(desc(weekly_solo_minutes))%>%
    select(email, name, bureau, team, weekly_solo_minutes)%>%
    head(10)
}
active_hermit(week_number=1)

# no days off (no days with 0 minutes)
no_days_off<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    daily_data <-daily_data %>%
      filter(week_num==week_number)
  }
  daily_data %>%
    filter(minutes>0) %>%
    group_by(email, name, bureau, team)%>%
    summarise(days_with_activity=n())%>%
    filter(days_with_activity==7)%>%
    select(email, name, bureau, team, days_with_activity)
}

no_days_off()

# Consistent performer (highest minimum minutes done weekly)
consistent_performer<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    daily_data <-daily_data %>%
      filter(week_num==week_number)
  }
  daily_data %>%
    group_by(email, name, bureau, team)%>%
    summarise(minimum_minutes=min(minutes))%>%
    arrange(desc(minimum_minutes)) %>%
    select(email, name, bureau, team, minimum_minutes)
}
consistent_performer(week_number = 1)

# Big effort (had the day with the biggest minutes)
big_effort<-function(week_number=""){
  if (is.numeric(week_number)==TRUE){
    daily_data <-daily_data %>%
      filter(week_num==week_number)
  }
  daily_data %>%
    arrange(desc(minutes))%>%
    select(email, name, bureau, team, calendar_date, minutes)%>%
    head(10)
}

big_effort(week_number = 1)
