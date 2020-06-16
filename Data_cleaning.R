
cat("\014")
visits<-read.csv("visits.csv",header=T)
visit_details<-read.csv("visit_details.csv",header=T)
prev_physical_details<-read.csv("prev_physical_details.csv",header=T)
prev_visit_details<-read.csv("prev_visit_details.csv",header=T)
prev_visits<-read.csv("prev_visits.csv",header=T)
prev_ward_first_procedure<-read.csv("prev_ward_first_procedure.csv",header=T)
ward_first_procedure<-read.csv("ward_first_procedure.csv",header=T)
xrays_visits<-read.csv("xrays_visits.csv",header=T)

visits_clean<-visits
visits_clean = visits[visits$hospitalization_dur_hours>0,]
#################################################           visits         #########################

visits_clean$age_in_months=((visits_clean$age_years*12)+visits_clean$age_months)

# תאפס משתנים שיוצרים כפילות במידע
visits_clean$hospitalization_date <- NULL
visits_clean$entry_date <- NULL
visits_clean$exit_date <- NULL
visits_clean$hospitalization_dur_days <- NULL
visits_clean$entry_day<- NULL
visits_clean$hospitalization_day<- NULL
visits_clean$exit_day<- NULL
visits_clean$ED_dur_hours<- NULL
visits_clean$hospitalization_dur_hours<- NULL
visits_clean$age_years<- NULL
visits_clean$age_months<- NULL

#תגדיר משתנים פקטוריאליים כפכטוריאליים
visits_clean$outcome = as.factor(visits_clean$outcome)
visits_clean$exit_department=as.factor(visits_clean$exit_department)
visits_clean$first_department=as.factor(visits_clean$first_department)
visits_clean$exit_group= as.factor(visits_clean$exit_group)
visits_clean$exit_unit= as.factor(visits_clean$exit_unit)
visits_clean$first_ward= as.factor(visits_clean$first_ward)
visits_clean$entry_group= as.factor(visits_clean$entry_group)
visits_clean$entry_unit= as.factor(visits_clean$entry_unit)
visits_clean$gender = as.factor(visits_clean$gender)
visits_clean$num_dep = as.factor(visits_clean$num_dep)

#########################################          x ray              ###################################
# תאפס משתנים שיוצרים כפילות במידע
xrays_visits$gender <- NULL
xrays_visits$entry_date <- NULL
xrays_visits$entry_day <- NULL
xrays_visits$exit_date <- NULL
xrays_visits$exit_day <- NULL
xrays_visits$outcome <- NULL
xrays_visits$age_years <- NULL
xrays_visits$age_months <- NULL

#תגדיר משתנים פקטוריאליים כפכטוריאליים
xrays_visits$department=as.factor(xrays_visits$department)

#########################################          visit_details              ###################################
# תאפס משתנים שיוצרים כפילות במידע
visit_details$gender <- NULL
visit_details$entry_date <- NULL
visit_details$entry_day <- NULL
visit_details$exit_date <- NULL
visit_details$exit_day <- NULL
visit_details$age_years <- NULL
visit_details$age_months <- NULL
visit_details$duration_hours<- NULL
visit_details$duration_days<- NULL

#תגדיר משתנים פקטוריאליים כפכטוריאליים
visit_details$seg_type=as.factor(visit_details$seg_type)
visit_details$entry_group=as.factor(visit_details$entry_group)
visit_details$outcome=as.factor(visit_details$outcome)
visit_details$department=as.factor(visit_details$department)                                 

write.csv(visit_details, "C:/Users/User/Desktop/Progs/R/Arik R/Rambam/HomeHospital/visit_details_clean.csv")

#########################################         ward_first_procedure              ###################################
# תאפס משתנים שיוצרים כפילות במידע
ward_first_procedure$gender <- NULL
ward_first_procedure$entry_date <- NULL
ward_first_procedure$entry_day <- NULL
ward_first_procedure$exit_date <- NULL
ward_first_procedure$exit_day <- NULL
ward_first_procedure$age_years <- NULL
ward_first_procedure$age_months <- NULL
ward_first_procedure$duration_hours<- NULL
ward_first_procedure$duration_days<- NULL
ward_first_procedure$corr_duration_hours<- NULL
ward_first_procedure$corr_duration_days<- NULL
ward_first_procedure$first_procedure_date<- NULL
ward_first_procedure$first_procedure_difference_min<- NULL
ward_first_procedure$first_procedure_difference_hours<- NULL

#תגדיר משתנים פקטוריאליים כפכטוריאליים
ward_first_procedure$department=as.factor(ward_first_procedure$department)
ward_first_procedure$physical_department=as.factor(ward_first_procedure$physical_department)
ward_first_procedure$prev_entry_group=as.factor(ward_first_procedure$prev_entry_group)

#################################################################################

#visits train-test allocation
set.seed(1)
rands <- rnorm(nrow(visits_clean))
test <- rands > quantile(rands,0.75)
train <- !test
visits_train <- visits_clean[train,]
visits_test <- visits_clean[test,]
nrow(visits_train)
nrow(visits_test)
