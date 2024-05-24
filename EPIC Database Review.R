#script for database review

#### packages ####
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)

#### Load relevant data #### 

Projects_overview <- read_excel("/Users/dorji/Desktop/Projects/EPIC/EPIC Database Export May 2024.xlsx", sheet = "Projects") %>% 
  select(-LongitudeX,-LatitudeY) %>% #there were 12 instances of duplicates that were matching all columns besides lat and long, removed
  distinct() #Sherab please copy code from my projects_overview object to remove duplicates - done
Projects_detail <- read_excel("/Users/dorji/Desktop/Projects/EPIC/EPIC Database Export May 2024.xlsx", sheet = "Projects Detail")
Finance_detail <- read_excel("/Users/dorji/Desktop/Projects/EPIC/EPIC Database Export May 2024.xlsx", sheet = "Finance Detail")
Projects_metric <- read_excel("/Users/dorji/Desktop/Projects/EPIC/EPIC Database Export May 2024.xlsx", sheet = "Project Metric")

#bk 
# Projects_overview <- read_excel("/Users/killingsworth/Documents/EPIC database/EPIC Database Export May 2024.xlsx", sheet = "Projects") %>%
#   select(-LongitudeX,-LatitudeY) %>% #there were 12 instances of duplicates that were matching all columns besides lat and long, removed
#   distinct()
# Projects_detail <- read_excel("/Users/killingsworth/Documents/EPIC database/EPIC Database Export May 2024.xlsx", sheet = "Projects Detail")
# Finance_detail <- read_excel("/Users/killingsworth/Documents/EPIC database/EPIC Database Export May 2024.xlsx", sheet = "Finance Detail")
# Projects_metric <- read_excel("/Users/killingsworth/Documents/EPIC database/EPIC Database Export May 2024.xlsx", sheet = "Project Metric ")


#### bk function for admin stratification #### 

stratify_admin_mean <- function(data,variable){
  output <- data %>% 
    filter(!is.na(ProgramAdminName),
           !is.na({{variable}})) %>% 
    group_by(ProgramAdminName) %>% 
    summarise(average = mean({{variable}}, na.rm = TRUE))
  return(output)
}

stratify_admin_percent <- function(data,variable){
  output <- data %>% 
    filter(!is.na(ProgramAdminName),
           !is.na({{variable}})) %>% 
    group_by(ProgramAdminName, {{variable}}) %>%
    summarise(n = n(), .groups = 'drop') %>%
    group_by(ProgramAdminName) %>%  # Regroup by ProgramAdminName
    mutate(pct = n / sum(n) * 100)
  return(output)
}

stratify_admin_includeNA <- function(data,variable){
  output <- data %>%
  group_by(ProgramAdminName) %>%
  summarise(
    total_rows = n(),
    non_missing_count = sum(!is.na({{variable}})),
    percentage = (non_missing_count / total_rows) * 100
  )
  return(output)
}




#### PROJECT OVERVIEW CHECKS ####
#### how many distinct Ids, ProjectNo, ProjectName, any discrepancies - Green #### 

distinct_ids_overview <- Projects_overview %>%
  filter(!is.na(Id)) %>%
  summarise(num_distinct_ids = n_distinct(Id))

distinct_projectno_overview <- Projects_overview %>%
  filter(!is.na(ProjectNo)) %>%
  summarise(num_distinct_projectno = n_distinct(ProjectNo))

distinct_projectname_overview <- Projects_overview %>%
  filter(!is.na(ProjectName)) %>%
  summarise(num_distinct_projectname = n_distinct(ProjectName))

#Note: Slight discrepancies to note here: there are 620 unique project IDs but the number decreases slightly to 611 project nos. (there were some NAs here)
#and to 613 project names

#bk
#projects
n_distinct(Projects_overview$Id) #620
n_distinct(Projects_overview$ProjectNo) #612
n_distinct(Projects_overview$ProjectName) #613

#projects detail
n_distinct(Projects_detail$ProjectId) #612, missing 8
n_distinct(Projects_detail$ProjectNo) #604, missing 8
n_distinct(Projects_detail$ProjectDetailId) #611

#finance detail
n_distinct(Finance_detail$ProjectId) #612, missing 8
n_distinct(Finance_detail$ProjectNo) #604, missing 8

#projects metric
n_distinct(Projects_metric$ProjectId) #619, missing 1
n_distinct(Projects_metric$ProjectNo) #611, missing 1

#generally acceptable levels of missing 
#consider finding these specific projects and seeing which admin(s) these are




#### how many multiphase projects and how these are reported, consistently or not ####
extract_base_project <- function(project_no) {
  sub(" Module.*| Project.*", "", project_no)
}

# Add a new column for base project identifier
projects <- Projects_overview %>%
  mutate(BaseProjectNo = sapply(ProjectNo, extract_base_project))

# Group by the base project identifier
multiphase_projects <- projects %>%
  group_by(BaseProjectNo) %>%
  filter(n() > 1) %>%
  distinct(ProjectNo) %>%
  filter(!is.na(ProjectNo))

#Note here: The way I went about this is:
# Creation of function "extract_base_project" to extract the base project number by removing " Module" and " Project" along with any subsequent text.
# Add Base Identifier: A new column BaseProjectNo is added to the data frame, which contains the base project number.
# Group and Filter: The data is grouped by BaseProjectNo, and projects with more than one entry are filtered and kept.
# Count Unique Base Projects: The number of unique base project identifiers is counted.
#Ask BK about this because this is very confusing 
#Probably best to do it in Excel - pause for now and ask KP on Tuesday about how much he wants it

#### average contract amount, removing zeros/NAs ####
filtered_projects <- Projects_overview %>%
  filter(!is.na(ContractAmount) & ContractAmount != 0)
avg_contract_amount <- mean(filtered_projects$ContractAmount)

avg_contract_amount_by_org <- filtered_projects %>%
  group_by(ProgramAdminName) %>%
  summarise(AverageContractAmount = mean(ContractAmount, na.rm = TRUE))

#bk function if want to use
functionavg_contract_amount_by_org <- stratify_admin_mean(filtered_projects,ContractAmount)


#### table of project leads and count of projects ####

standardize_project_lead <- function(project_lead) {
  standardized_names <- c(
    "Pacific Gas and Electric Company" = "PG&E",
    "PG&E" = "PG&E",
    "PACIFIC GAS AND ELECTRIC COMPANY" = "PG&E",
    "San Diego Gas & Electric" = "SDG&E",
    "SDG&E" = "SDG&E",
    "San Diego Gas & Electric Company" = "SDG&E",
    "Southern California Edison" = "SCE",
    "SCE" = "SCE",
    "Southern California Edison Company" = "SCE",
    "California Energy Commission" = "CEC",
    "CEC" = "CEC",
    "Lawrence Berkeley National Laboratory" = "LBNL",
    "Electric Power Research Institute" = "EPRI",
    "Electric Power Research Institute, Inc." = "EPRI",
    "University of California, Los Angeles" = "UCLA",
    "University of California - Davis" = "UC Davis",
    "The Regents of the University of California, Berkeley" = "UC Berkeley",
    "The Regents of the University of California, Merced" = "UC Merced",
    "The Regents of the University of California, on behalf of the Irvine Campus" = "UC Irvine",
    "The Regents of the University of California, Los Angeles" = "UCLA",
    "The Regents of the University of California, San Diego" = "UC San Diego",
    "Regents of the University of California, Davis" = "UC Davis",
    "Regents of the University of California, Los Angeles" = "UCLA",
    "The Regents of the University of California on behalf of the Berkeley campus" = "UC Berkeley",
    "The Regents of the University of California, San Diego" = "UC San Diego",
    "The Regents of the University of California, Merced" = "UC Merced",
    "The Regents of the University of California, on behalf of the Irvine Campus" = "UC Irvine",
    "The Regents of the University of California, Los Angeles" = "UCLA",
    "The Regents of the University of California, San Diego" = "UC San Diego",
    "Regents of the University of California, Davis" = "UC Davis",
    "The Regents of the University of California, Santa Barbara" = "UC Santa Barbara",
    "The Regents of the University of California, Riverside" = "UC Riverside",
    "The Regents of the University of California, Irvine" = "UC Irvine",
    "The Regents of the University of California, Santa Cruz" = "UC Santa Cruz",
    "RCAM Technologies, Inc." = "RCAM Technologies"
  )
  recode(project_lead, !!!standardized_names)
}

Projects_overview <- Projects_overview %>%
  mutate(ProjectLead = standardize_project_lead(ProjectLead))

count_of_projects_by_lead <- Projects_overview %>%
  filter(!is.na(ProjectLead)) %>%
  group_by(ProjectLead) %>%
  summarise(Count = n())

#In this, the way the data has been reported in a way that the same organization has different number of projects because of different naming styles (eg: PG&E and Pacific Gas and Electric)
#Too many edge cases to handle - ask BK and/or HL about this

#### table of ProgramAdminName and count of projects, identify difference between this variable and ProjectLead - Green ####
count_of_projects_by_admin <- Projects_overview %>%
  filter(!is.na(ProgramAdminName)) %>%
  group_by(ProgramAdminName) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count / sum(Count) * 100)

#It's definitely easier to count projects based on ProgramAdmin because there are four major overseers here - interesting to note here is that the utilities have offloaded some of the projects to smaller orgs, 
#making a strong case as theorized by BK that the observations in the ProjectLead variable are program implementers/consultants and the ProgramAdmin folks are the overall administrators/overseers

#### percent of projects active/closed/pending - Green ####
percent_of_projects_by_status <- Projects_overview %>%
  filter(!is.na(ProjectStatus)) %>%
  group_by(ProjectStatus) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

admin_percent_of_projects_by_status <- stratify_admin_percent(Projects_overview,ProjectStatus)



#### percent of projects listed as IsActive = TRUE but ProjectStatus = Closed, consider projectenddate, general exploration of status and date reporting discrepancies - Green ####

projects_active_closed <- Projects_overview %>%
  filter(!is.na(ProjectStatus),
         !is.na(IsActive)) %>%
  filter(IsActive == TRUE, ProjectStatus == "Closed") %>%
  summarise(n = n(),
            total = nrow(Projects_overview),
            percentage = (n() / total) * 100
  )


projects_active_closed_by_org <- Projects_overview %>%
  filter(!is.na(ProjectStatus), !is.na(IsActive)) %>%
  group_by(ProgramAdminName) %>%
  mutate(total_projects = n()) %>%  # Total projects per ProgramAdminName
  filter(IsActive == TRUE, ProjectStatus == "Closed") %>%  # Filter for active and closed projects
  summarise(n = n(), total_projects = first(total_projects), .groups = 'drop') %>%  # Count and keep total projects
  mutate(pct = n / total_projects * 100)


#54 percent have this contradiction
#All of the projects listed as active but closed should be listed as inactive (all of the project end dates have passed)
# When split by org, SDG&E has the most projects that face this contradiction

#### percent start and end dates populated, average duration of project - Green ####
projects_start_end <- Projects_overview %>%
  filter(!is.na(ProjectStartDate) & !is.na(ProjectEndDate)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_overview),
    percentage = (n() / total) * 100
  )

projects_start_end_by_org <- Projects_overview %>%
  group_by(ProgramAdminName) %>%
  mutate(total_projects = n()) %>%  # Total projects per ProgramAdminName
  filter(!is.na(ProjectStartDate) & !is.na(ProjectEndDate)) %>%  # Filter for active and closed projects
  summarise(n = n(), total_projects = first(total_projects), .groups = 'drop') %>%  # Count and keep total projects
  mutate(pct = n / total_projects * 100)

# Inspect the date format
head(Projects_overview$ProjectStartDate)
head(Projects_overview$ProjectEndDate)

# Assuming the date format is "MM/DD/YYYY" or similar, specify the format
average_project_duration <- Projects_overview %>%
  filter(!is.na(ProjectStartDate) & !is.na(ProjectEndDate)) %>%
  mutate(ProjectStartDate = as.Date(ProjectStartDate, format = "%m/%d/%Y"),
         ProjectEndDate = as.Date(ProjectEndDate, format = "%m/%d/%Y"),
         ProjectDuration = as.numeric(ProjectEndDate - ProjectStartDate)/7)
  #%>% summarise(AverageDuration = mean(ProjectDuration, na.rm = TRUE))
  
admin_average_project_duration <- stratify_admin_mean(average_project_duration,ProjectDuration)


#95 percent of projects have populated start and end dates 
#199 weeks(3.8 years) is the avg project duration

#### percent of projects with contact info provided (name and email), any differences by program admin (i.e. which admins are best at reporting this?) ####
projects_contact_info <- Projects_overview %>%
  filter(!is.na(PersonContactFirstName) & !is.na(PersonContactLastName) & !is.na(PersonContactEmail)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_overview),
    percentage = (n() / total) * 100
  )

projects_contact_info_by_org <- Projects_overview %>%
  group_by(ProgramAdminName) %>%
  mutate(total_projects = n()) %>%  # Total projects per ProgramAdminName
  filter(!is.na(PersonContactFirstName) & !is.na(PersonContactLastName) & !is.na(PersonContactEmail)) %>% 
  summarise(n = n(), total_projects = first(total_projects), .groups = 'drop') %>%  # Count and keep total projects
  mutate(pct = n / total_projects * 100)


#83 percent of projects had contact info (name and email)
#PG&E and CEC were the only two program admins that were relatively consistent with providing contact info on their projects (name AND email)
#SCE (n = 23) was more consistent in giving out names than SDG&E
#SDG&E had no information at all (n = 27) compared to SCE

#### percent of projects with cec manager contact info provided (name and email), any differences by program admin (i.e. which admins are best at reporting this?) ####
projects_CEC_contact_info <- Projects_overview %>%
  filter(!is.na(CecMgrContactFirstName) & !is.na(CecMgrContactLastName) & !is.na(CecMgrEmail)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_overview),
    percentage = (n() / total) * 100
  )

projects_CEC_contact_info_by_org <- Projects_overview %>%
  group_by(ProgramAdminName) %>%
  mutate(total_projects = n()) %>%  # Total projects per ProgramAdminName
  filter(!is.na(CecMgrContactFirstName) & !is.na(CecMgrContactLastName) & !is.na(CecMgrEmail)) %>%
  summarise(n = n(), total_projects = first(total_projects), .groups = 'drop') %>%  # Count and keep total projects
  mutate(pct = n / total_projects * 100)

#81 percent of projects had complete CEC contact info (name and email)
#CEC was the only program admin that provided both name and email 
#PG&E and SCE did provide names (but the way they reported it-first and last names were together in the CecMgrContactFirstName column)
#SDG&E either left it blank or the projects that they worked on did not have CEC involvement, so NA (N = 4)

#### PROJECTS DETAIL ####

Projects_detail <- Projects_detail %>%
  left_join(Projects_overview %>% select(Id, ProjectNo, ProgramAdminName, IsActive), by = c("ProjectId" = "Id", "ProjectNo" = "ProjectNo"))

#bk check, this should be zero now
duplicates <- Projects_detail %>% 
  group_by(ProjectId) %>% 
  summarize(nrows = n()) %>% 
  filter(nrows > 1)


Projects_detail <-  Projects_detail %>%
  rename(IsActive_details = IsActive.x, IsActive_overview = IsActive.y)

         
#### check if Projectid and ProjectNo match Id and ProjectNo column on Projects tab - Green ####

Projects_check <- Projects_overview %>%
  rename(Projects_Id = Id)

# Perform left join to check if ProjectId and ProjectNo match Projects_Id and ProjectNo
matched_projects <- Projects_detail %>%
  left_join(Projects_check, by = c("ProjectId" = "Projects_Id", "ProjectNo" = "ProjectNo"))

# Compare the Id and ProjectNo columns to check for matches
matched_projects <- matched_projects %>%
  mutate(Id_Match = ifelse(!is.na(Projects_Id), TRUE, FALSE),
         ProjectNo_Match = ifelse(!is.na(ProjectNo), TRUE, FALSE))


#Ask BK how to do this
distinct_ids_detail <- Projects_detail %>%
  filter(!is.na(ProjectId)) %>%
  summarise(num_distinct_ids = n_distinct(ProjectId))

distinct_projectno_detail <- Projects_detail %>%
  filter(!is.na(ProjectNo)) %>%
  summarise(num_distinct_projectno = n_distinct(ProjectNo))


#Note: Slight discrepancies to note here: there are 620 unique project IDs in the overview but the number decreases slightly to 612 here
#611 projects in the overview but 603 here 


#bk check 
check <- Projects_overview %>% 
  rename("ProjectId" = "Id") %>% 
  mutate(nomatch = !ProjectId %in% Projects_detail$ProjectId) %>% 
  filter(nomatch == TRUE) #these are the 8 ids not in the detail tab, all other ids match
  

#### percent of projects with detailed project description - Green ####

project_description_pct <- Projects_detail %>%
  filter(!is.na(DetailedProjectDescription)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )



#There are 182 projects that have a project description out of the 612 listed 
#ask BK this - excel says there are 171 projects but here it says 182
#bk updated so now 171 

#bk update to get the percentage populated by program admin
project_description_pct_by_org <- Projects_detail %>%
  group_by(ProgramAdminName) %>%
  summarise(
    total_rows = n(),
    non_missing_count = sum(!is.na(DetailedProjectDescription)),
    percentage = (non_missing_count / total_rows) * 100
  )
#CEC is only admin missing detailed project descriptions; they're missing for ~90% of their projects

#### bk added: check which admins are listing "competitive" as detailed description ####
competitive_check <- Projects_detail %>%
  filter(!is.na(DetailedProjectDescription)) %>% 
  group_by(ProgramAdminName) %>%
  summarise(
    listed_competitive = sum(DetailedProjectDescription == "Competitive"),
    total_rows = n(),
    percentage = (listed_competitive / total_rows) * 100)
#cec was the only admin that listed "competitive" for their detailed description. they did this for 93% of the projects they wrote a description for

#### percent of projects with project summary, are there any themes among blank projects? what is their status? - Green ####

project_summary_pct <- Projects_detail %>%
  filter(!is.na(ProjectSummary)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

project_summary_pct_by_org <- stratify_admin_includeNA(Projects_detail,ProjectSummary)

blank_projects_by_org_status <- Projects_detail %>%
  filter(is.na(ProjectSummary)) %>%
  group_by(ProgramAdminName, IsActive_details) %>%
  summarise(n = n(), .groups = 'drop')
  
#98 percent of the projects at least had a project summary
#For the remaining 2 percent (n= 12), ten of them were still technically active - all of them belonged to the CEC

#### percent of projects with project update, are there any themes among blank projects? - Green ####

project_update_pct <- Projects_detail %>%
  filter(!is.na(ProjectUpdate)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

#bk update to get the percentage populated by program admin
project_update_pct_by_org <- stratify_admin_includeNA(Projects_detail,ProjectUpdate)
  

blank_projects_update_by_org_status <- Projects_detail %>%
  filter(is.na(ProjectUpdate)) %>%
  group_by(ProgramAdminName, IsActive_details) %>%
  summarise(n = n(), .groups = 'drop')

#73 percent of the projects had a project update
#For the remaining (n = 167), the majority (n = 137) were no longer active - belonged to a variety of orgs (PG&E was the only one where all the projects were inactive)

#### percent of projects with deliverables, are there any themes among blank projects? - Green ####
project_deliverables_pct <- Projects_detail %>%
  filter(!is.na(Deliverables)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

#bk update to get the percentage populated by program admin
project_deliverable_pct_by_org <- stratify_admin_includeNA(Projects_detail,Deliverables)


blank_projects_deliverable_by_org_status <- Projects_detail %>%
  filter(is.na(Deliverables)) %>%
  group_by(ProgramAdminName, IsActive_details) %>%
  summarise(n = n(), .groups = 'drop')

#33 percent have deliverables listed
#For the remaining (n = 410), the majority (n = 375) were no longer active - all belonged to cec


#### lower priority: percent of projects with StatePolicySupportText, TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers, GettingToScale, KeyInnovations, KeyLearnings, Scalability - red ####

#State Policy Support
statepolicy_pct <- Projects_detail %>%
  filter(!is.na(StatePolicySupportText)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

statepolicy_pct_by_org <- stratify_admin_includeNA(Projects_detail,StatePolicySupportText)

blank_statepolicy_by_org_status <- Projects_detail %>%
  filter(is.na(StatePolicySupportText)) %>%
  group_by(ProgramAdminName, IsActive_details) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)
  
  
#Only 16 percent have state policy support text
#Among those that are blank, the majority (93%) are no longer active

#TechnicalBarriers
techbarriers_pct <- Projects_detail %>%
  filter(!is.na(TechnicalBarriers)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

techbarriers_pct_by_org <- stratify_admin_includeNA(Projects_detail,TechnicalBarriers)

blank_techbarriers_by_org_status <- Projects_detail %>%
  filter(is.na(TechnicalBarriers)) %>%
  group_by(ProgramAdminName, IsActive_details) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#439 percent have state policy support text. 
#CEC  is the admin that has the most projects with technical barriers missing (73% missing). 
#SDG&E has no projects missing

#MarketBarriers
marketbarriers_pct <- Projects_detail %>%
  filter(!is.na(MarketBarriers)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

marketbarriers_pct_by_org <- stratify_admin_includeNA(Projects_detail,MarketBarriers)


blank_marketbarriers_by_org_status <- Projects_detail %>%
  filter(is.na(MarketBarriers)) %>%
  group_by(ProgramAdminName, IsActive_details) %>%
  summarise(n = n(), .groups = 'drop')

#30 percent that mention market barriers. 
#CEC  is the admin that has the most projects with market barriers missing (78% missing). 
#SCE has the least number of projects missing
#Among those that are blank, the majority (92%) are no longer active 

#PolicyAndRegulatoryBarriers
policybarriers_pct <- Projects_detail %>%
  filter(!is.na(PolicyAndRegulatoryBarriers)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

policybarriers_pct_by_org <- stratify_admin_includeNA(Projects_detail,PolicyAndRegulatoryBarriers)


blank_policybarriers_by_org_status <- Projects_detail %>%
  filter(is.na(PolicyAndRegulatoryBarriers)) %>%
  group_by(ProgramAdminName, IsActive_details) %>%
  summarise(n = n(), .groups = 'drop')

#24 percent mentioned policy barriers
#CEC  is the admin that has the most projects with policy and regulatory barriers missing (~81% missing). 
#SCE has the least number of projects missing (43%)
#Among those that are blank, the majority (92%) are no longer active

#GettingToScale
g2s_pct <- Projects_detail %>%
  filter(!is.na(GettingToScale)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

g2s_pct_by_org <- stratify_admin_includeNA(Projects_detail,GettingToScale)

g2s_blank_by_org_status <- Projects_detail %>%
  filter(is.na(GettingToScale)) %>%
  group_by(ProgramAdminName, IsActive_details) %>%
  summarise(n = n(), .groups = 'drop')

#Only 38 percent that mention ways of getting to scale.
#CEC  is the admin that has the most projects with no ways of getting to scale (~73% missing). SDG&E has no projects missing
#Among those that are blank, the majority (90%) are no longer active

#KeyInnovations
innovations_pct <- Projects_detail %>%
  filter(!is.na(KeyInnovations)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

innovations_by_org <- stratify_admin_includeNA(Projects_detail,KeyInnovations)

innovations_blank_by_org_status <- Projects_detail %>%
  filter(is.na(KeyInnovations)) %>%
  group_by(ProgramAdminName, IsActive_details) %>%
  summarise(n = n(), .groups = 'drop')

#42 percent that mention ways of getting to scale. 
#CEC  is the admin that has the most projects with no ways of getting to scale (~70% missing). 
#SDG&E has no projects missing
#Among those that are blank, the majority (90%) are no longer active

#KeyLeanings
leanings_pct <- Projects_detail %>%
  filter(!is.na(KeyLeanings)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

leanings_by_org <- stratify_admin_includeNA(Projects_detail,KeyLeanings)

leanings_blank_by_org_status <- Projects_detail %>%
  filter(is.na(KeyLeanings)) %>%
  group_by(ProgramAdminName, IsActive_details) %>%
  summarise(n = n(), .groups = 'drop')

#Only 27 percent that mention key leanings. 
#CEC is the admin that has the most projects with no key leanings mentioned (~85% missing). 
#SDG&E has no projects missing
#Among those that are blank, the majority (96%) are no longer active

#Scalability
scalability_pct <- Projects_detail %>%
  filter(!is.na(Scalability)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

scalability_by_org <- stratify_admin_includeNA(Projects_detail,Scalability)

scalability_blank_by_org_status <- Projects_detail %>%
  filter(is.na(Scalability)) %>%
  group_by(ProgramAdminName, IsActive_details) %>%
  summarise(n = n(), .groups = 'drop')

#Only 28 percent that mention scalability 
#CEC is the admin that has the most projects missing (~82% missing). SDG&E has least projects missing

# Does IsActive column match same column on Projects tab? - yellow
matched_projects <- Projects_detail %>%
  left_join(Projects_overview, by = c("ProjectId" = "Id", "ProjectNo" = "ProjectNo"))

# Compare the IsActive columns
matched_projects_activity <- Projects_detail %>%
  mutate(IsActive_Match = ifelse(IsActive_overview == IsActive_details, TRUE, FALSE))

# View the result
view <- print(matched_projects_activity %>% select(ProjectId, ProjectNo, IsActive_overview, IsActive_details, IsActive_Match))
summary_isactive_match <- view %>%
  summarise(
    Total = n(),
    Matches = sum(IsActive_Match, na.rm = TRUE),
    Mismatches = sum(!IsActive_Match, na.rm = TRUE), 
    percentage_match = Matches/Total*100
  )

#According to this, 14 percent matches
#Huge discrepancy

#### FINANCE DETAIL ####
Finance_detail <- Finance_detail %>%
  left_join(Projects_overview, by = c("ProjectId" = "Id", "ProjectNo" = "ProjectNo"))

Finance_detail <-  Finance_detail %>%
  rename(ContractAmount_details = ContractAmount.x, ContractAmount_overview = ContractAmount.y)

#### how many distinct Projectid and ProjectNo ####
distinct_ids_finance <- Finance_detail %>%
  summarise(num_distinct_ids = n_distinct(ProjectId))

distinct_projectno_finance <- Finance_detail %>%
  summarise(num_distinct_projectno = n_distinct(ProjectNo))

#612 distinct ids 
#604 distinct project numbers

#check if Projectid and ProjectNo match other tabs


#### what is average CommitedFundingAmount? Percent reporting zero? Which projects are reporting zero? ####
avg_committed_amount <- Finance_detail %>%
  filter(!is.na(CommitedFundingAmount) & CommitedFundingAmount != 0) %>%
  summarise(AverageCommittedAmount = mean(CommitedFundingAmount, na.rm = TRUE))

avg_committed_amount_by_org <- Finance_detail %>%
  filter(!is.na(CommitedFundingAmount) & CommitedFundingAmount != 0) %>%
  group_by(ProgramAdminName) %>%
  summarise(AverageCommittedAmount = mean(CommitedFundingAmount, na.rm = TRUE))

zero_committed_amount <- Finance_detail %>%
  filter(!is.na(CommitedFundingAmount)) %>%
  summarise(
  total_entries = n(),
  zero_committed_funding = sum(CommitedFundingAmount == 0, na.rm = TRUE),
  percentage_zero = (zero_committed_funding / total_entries) * 100
)

zero_committed_amount_by_org <- Finance_detail %>%
  filter(!is.na(CommitedFundingAmount)) %>%
  group_by(ProgramAdminName) %>%
  summarise(
    total_entries = n(),
    zero_committed_funding = sum(CommitedFundingAmount == 0, na.rm = TRUE),
    percentage_zero = (zero_committed_funding / total_entries) * 100
  )

#The avg committed amount across all boards is $2,402,961
#The highest avg committed amount when stratified by org is $2,915,164 (SCE)
#4 percent reported 0 for committed funding
#SDG&E had the most number of projects that reported 0 for committed funding (33%) - PG&E reported no zeros

#### how does committed funding, encumbered funding, and funds expended relate? Is funds expended always < commited + encumbered? ####
# Committed Funding is the total budget.
# Encumbered Funding is the reserved part of the budget.
# Funds Expended is the spent part of the budget.
#Mathematical formula to check for (discuss with BK though): FE < Committed + encumbered

budget_check <- Finance_detail %>%
  filter(
    !is.na(CommitedFundingAmount) & CommitedFundingAmount != 0,
    !is.na(FundsExpendedToDate) & FundsExpendedToDate != 0,
    !is.na(EncumberedFundingAmount) & EncumberedFundingAmount != 0
  ) %>%
  mutate(check = FundsExpendedToDate < (CommitedFundingAmount + EncumberedFundingAmount))
  
view_budget_check <- print(budget_check %>% select(ProjectId, ProjectNo, CommitedFundingAmount, FundsExpendedToDate, EncumberedFundingAmount, check))
summary_budget_check <- view_budget_check %>%
  summarise(
    Total = n(),
    Matches = sum(check, na.rm = TRUE),
    Mismatches = sum(!check, na.rm = TRUE),
    percentage_match = Matches/Total*100
  )

#discuss with BK though

#### what is average funds expended amount? percent reporting zero? which projects are reporting zero? ####
avg_expended_amount <- Finance_detail %>%
  filter(!is.na(FundsExpendedToDate) & FundsExpendedToDate != 0) %>%
  summarise(AverageExpendedAmount = mean(FundsExpendedToDate, na.rm = TRUE))

avg_expended_amount_by_org <- Finance_detail %>%
  filter(!is.na(FundsExpendedToDate) & FundsExpendedToDate != 0) %>%
  group_by(ProgramAdminName) %>%
  summarise(AverageExpendedAmount = mean(FundsExpendedToDate, na.rm = TRUE))

zero_expended_amount <- Finance_detail %>%
  summarise(
    total_entries = n(),
    zero_expended_funding = sum(FundsExpendedToDate == 0, na.rm = TRUE),
    percentage_zero = (zero_expended_funding / total_entries) * 100
  )

zero_expended_amount_by_org <- Finance_detail %>%
  group_by(ProgramAdminName) %>%
  summarise(
    total_entries = n(),
    zero_expended_funding = sum(FundsExpendedToDate == 0, na.rm = TRUE),
    percentage_zero = (zero_expended_funding / total_entries) * 100
  )

#The avg expended amount across all boards is $1,659,082. 
#The highest avg expended amount when stratified by org is $2,422,829 (SCE)
#13 percent reported 0 for expended funding
#SDG&E had the most number of projects that reported 0 for expended funding (33%) - PG&E reported no zeros


#### Does ContractAmount match same column on Projects tab? #### 
matched_amounts <- Finance_detail %>%
  mutate(Matched_Amounts = ifelse(ContractAmount_details == ContractAmount_overview, TRUE, FALSE))

# View the result
view_amount <- print(matched_amounts %>% select(ProjectId, ProjectNo, ContractAmount_overview, ContractAmount_details, Matched_Amounts))
summary_matched_amounts <- view_amount %>%
  summarise(
    Total = n(),
    Matches = sum(Matched_Amounts, na.rm = TRUE),
    Mismatches = sum(!Matched_Amounts, na.rm = TRUE),
    percentage_match = Matches/Total*100
  )

#82 percent of the Contract Amounts in both tabs match
#Ask BK if he thinks this is right

#### PROJECT METRIC ####

Projects_metric <- Projects_metric %>%
  left_join(Projects_overview, by = c("ProjectId" = "Id", "ProjectNo" = "ProjectNo"))

Projects_metric <- Projects_metric %>%
  rename(IsActive_metric = IsActive.x, IsActive_overview = IsActive.y)

#### how many distinct Projectid and ProjectNo ####
distinct_ids_metric <- Projects_metric %>%
  summarise(num_distinct_ids = n_distinct(ProjectId))

distinct_projectno_metric <- Projects_metric %>%
  summarise(num_distinct_projectno = n_distinct(ProjectNo))

#619 distinct ids 
#611 distinct project numbers


#check if Projectid and ProjectNo match other tabs
#Ask BK

check_metric <- Projects_overview %>% 
  rename("ProjectId" = "Id") %>% 
  mutate(nomatch = !ProjectId %in% Projects_metric$ProjectId) %>% 
  filter(nomatch == TRUE) #this is the 1 id missing

#### percent of projects reporting each impact (ElectricitySystemReliabilityImpacts:InformationDissemination) ####

# ElectricitySystemReliabilityImpacts
reli_impacts_pct <- Projects_metric %>%
  filter(!is.na(ElectricitySystemReliabilityImpacts)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_metric),
    percentage = (n() / total) * 100
  )

reli_impacts_by_org <- stratify_admin_includeNA(Projects_metric,ElectricitySystemReliabilityImpacts)


blank_reli_impacts_pct_by_org_status <- Projects_metric %>%
  filter(!is.na(ElectricitySystemReliabilityImpacts)) %>%
  group_by(ProgramAdminName, IsActive_metric) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#Only 23 percent mention electricity system reliability impacts. 
#CEC has the most missing (92% percent missing). SDG&E have no projects missing

# ElectricitySystemSafetyImpacts

safety_impacts_pct <- Projects_metric %>%
  filter(!is.na(ElectricitySystemSafetyImpacts)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_metric),
    percentage = (n() / total) * 100
  )

safety_impacts_pct_by_org <- stratify_admin_includeNA(Projects_metric,ElectricitySystemSafetyImpacts)


blank_safety_impacts_pct_by_org_status <- Projects_metric %>%
  filter(!is.na(ElectricitySystemSafetyImpacts)) %>%
  group_by(ProgramAdminName, IsActive_metric) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#Only 16 percent mention electricity system safety impacts
#EC has the most missing (94% percent missing). SDG&E have least projects missing

# EnviromentalImpactsNonGHG
enviro_impacts_pct <- Projects_metric %>%
  filter(!is.na(EnviromentalImpactsNonGHG)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_metric),
    percentage = (n() / total) * 100
  )

enviro_impacts_pct_by_org <- stratify_admin_includeNA(Projects_metric,EnviromentalImpactsNonGHG)

blank_enviro_impacts_pct_by_org_status <- Projects_metric %>%
  filter(!is.na(EnviromentalImpactsNonGHG)) %>%
  group_by(ProgramAdminName, IsActive_metric) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#35 percent mention non GHG environmental impacts. PG&E has the most missing (~96% missing)
#Among those that are blank, all were active

# ProjectedProjectBenefits
proj_benefits_pct <- Projects_metric %>%
  filter(!is.na(ProjectedProjectBenefits)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_metric),
    percentage = (n() / total) * 100
  )

proj_benefits_pct_by_org <- stratify_admin_includeNA(Projects_metric,ProjectedProjectBenefits)

blank_proj_benefits_pct_by_org_status <- Projects_metric %>%
  filter(!is.na(ProjectedProjectBenefits)) %>%
  group_by(ProgramAdminName, IsActive_metric) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#86 percent mentioned project benefits
#CEC has most missing (~16% missing). SDG&E has none missing
#Among those that are blank, the majority (98%) were active

# RatePayersBenefits
rp_benefits_pct <- Projects_metric %>%
  filter(!is.na(RatePayersBenefits)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_metric),
    percentage = (n() / total) * 100
  )

rp_benefits_pct_by_org <- stratify_admin_includeNA(Projects_metric,RatePayersBenefits)


blank_rp_benefits_pct_by_org_status <- Projects_metric %>%
  filter(!is.na(RatePayersBenefits)) %>%
  group_by(ProgramAdminName, IsActive_metric) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#19 percent mentioned ratepayer benefits
#CEC has most missing (~89% missing). PG&E has the least missing (27%)

# CommunityBenefitsDesc
community_benefits_pct <- Projects_metric %>%
  filter(!is.na(CommunityBenefitsDesc)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_metric),
    percentage = (n() / total) * 100
  )

community_benefits_pct_by_org <- stratify_admin_includeNA(Projects_metric,CommunityBenefitsDesc)

blank_community_benefits_pct_by_org_status <- Projects_metric %>%
  filter(!is.na(CommunityBenefitsDesc)) %>%
  group_by(ProgramAdminName, IsActive_metric) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#Only 5 percent provided a description for community benefits
#CEC had no descriptions listed at all
#SDG&E had the majority of projects that mentioned community benefits (57%)
#Among those that are blank, the majority (91%) were active

# EnergyImpacts
energy_impacts_pct <- Projects_metric %>%
  filter(!is.na(EnergyImpacts)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_metric),
    percentage = (n() / total) * 100
  )

energy_impacts_pct_by_org <- stratify_admin_includeNA(Projects_metric,EnergyImpacts)

blank_energy_impacts_pct_by_org_status <- Projects_metric %>%
  filter(!is.na(EnergyImpacts)) %>%
  group_by(ProgramAdminName, IsActive_metric) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#Only 4 percent mentioned energy impacts
#CEC has all missing. SDG&E has the least missing (38% of its projects have listed energy impacts)
#Among those that are blank, 96 percent were active

# InfrastructureCostBenefits
infra_cb_pct <- Projects_metric %>%
  filter(!is.na(InfrastructureCostBenefits)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_metric),
    percentage = (n() / total) * 100
  )

infra_cb_pct_by_org <- stratify_admin_includeNA(Projects_metric,InfrastructureCostBenefits)

blank_infra_cb_pct_by_org_status <- Projects_metric %>%
  filter(!is.na(InfrastructureCostBenefits)) %>%
  group_by(ProgramAdminName, IsActive_metric) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#20 percent listed infrastructure cost benefits. 
#CEC has most  missing (~87%). 
#SDG&E has the least missing (24%)
#Among those that are blank, 98 percent were active

# OtherImpacts
other_impacts_pct <- Projects_metric %>%
  filter(!is.na(OtherImpacts)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_metric),
    percentage = (n() / total) * 100
  )

other_impacts_pct_by_org <- stratify_admin_includeNA(Projects_metric,OtherImpacts)


blank_other_impacts_pct_by_org_status <- Projects_metric %>%
  filter(!is.na(OtherImpacts)) %>%
  group_by(ProgramAdminName, IsActive_metric) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#Only 4 percent mentioned other impacts
#CEC has all missing. SCE has the least missing (26% of its projects have listed other impacts)
#Among those that are blank, all were still active

# InformationDissemination
info_pct <- Projects_metric %>%
  filter(!is.na(InformationDissemination)) %>%
  summarise(
    n = n(),
    total = nrow(Projects_metric),
    percentage = (n() / total) * 100
  )

info_pct_by_org <- stratify_admin_includeNA(Projects_metric,InformationDissemination)


blank_info_pct_by_org_status <- Projects_metric %>%
  filter(!is.na(InformationDissemination)) %>%
  group_by(ProgramAdminName, IsActive_metric) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#Only 11 percent listed ways to disseminate info. 
#CEC has all missing. 
#SDG&E has the least missing (86% of its projects have listed ways)

#### Does IsActive column match same column on other tabs? ####
matched_projects_metric <- Projects_metric %>%
  mutate(IsActive_Match = ifelse(IsActive_overview == IsActive_metric, TRUE, FALSE))

# View the result
view_metric <- print(matched_projects_metric %>% select(ProjectId, ProjectNo, IsActive_overview, IsActive_metric, IsActive_Match))
summary_isactive_match_metric <- view_metric %>%
  summarise(
    Total = n(),
    Matches = sum(IsActive_Match, na.rm = TRUE),
    Mismatches = sum(!IsActive_Match, na.rm = TRUE), 
    percentage_match = Matches/Total*100
  )

#Only 2 mismatches: pretty much 100% 
#Ask BK though

### SECOND ITERATION ###

# Project Detail #

#percent one or more barriers listed. Which projects have none, pull
barriers_detail <- Projects_detail %>%
  select(ProjectId, DetailedProjectDescription, ProgramAdminName, TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers) %>%
  filter_at(vars(TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers), all_vars(!is.na(.) & . != "None" & . != "N/A" & . != "NA" & . != "n/a" & . != "TBD." & . != "None applicable / discussed" & . != "None applicable / discussed"))


barriers_detail <- Projects_detail %>%
  select(ProjectId, DetailedProjectDescription, ProgramAdminName, TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers) %>%
  filter_at(vars(TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers), all_vars(!is.na(.) & . != "None" & . != "N/A" & . != "NA" & . != "n/a" & . != "TBD." & . != "None applicable / discussed" & . != "None applicable / discussed"))












#BK code
barriers_detail <- Projects_detail %>%
  select(ProjectId, DetailedProjectDescription, ProgramAdminName, TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers) %>%
  mutate(across(c(TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers), ~ na_if(., "None"))) %>%
  mutate(across(c(TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers), ~ na_if(., "N/A"))) %>%
  mutate(across(c(TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers), ~ na_if(., "NA"))) %>%
  mutate(across(c(TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers), ~ na_if(., "n/a"))) %>%
  mutate(across(c(TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers), ~ na_if(., "TBD"))) %>% 
  mutate(across(c(TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers), ~ na_if(., "TBD."))) %>%
  mutate(across(c(TechnicalBarriers, MarketBarriers, PolicyAndRegulatoryBarriers), ~ na_if(., "None applicable / discussed"))) %>% 
  mutate(one_or_more_barrier = ifelse(!is.na(TechnicalBarriers)|!is.na(MarketBarriers)|!is.na(PolicyAndRegulatoryBarriers),1,0))

projects_with_no_barriers <- barriers_detail %>% 
  filter(one_or_more_barrier == 0)




















###
tech_barriers <- Projects_detail %>%
  select(ProjectId, DetailedProjectDescription, ProgramAdminName, TechnicalBarriers) %>%
  filter(!is.na(TechnicalBarriers)) %>%
  filter(TechnicalBarriers != "None" & TechnicalBarriers != "N/A" & TechnicalBarriers != "NA" & 
           TechnicalBarriers != "n/a" & TechnicalBarriers != "TBD" & 
           TechnicalBarriers != "None applicable / discussed" & 
           TechnicalBarriers != "No such barriers are known.") %>%
  summarise(
    n = n(),
    total = nrow(Projects_detail),
    percentage = (n() / total) * 100
  )

tech_barriers_by_org <- stratify_admin_includeNA(Projects_detail,TechnicalBarriers)

no_tech_barriers_by_org <- no_tech_barriers %>%
  group_by(ProgramAdminName) %>%
  summarise(
    n = n(),
    total = nrow(no_tech_barriers),
    percentage = (n / total) * 100
  )


### Additional checks: [matched funds + committed funds] > [encumbered funds] &  >funds expended? ###
budget_check_second <- Finance_detail %>%
  filter(
    !is.na(CommitedFundingAmount) & CommitedFundingAmount != 0,
    !is.na(FundsExpendedToDate) & FundsExpendedToDate != 0,
    !is.na(EncumberedFundingAmount) & EncumberedFundingAmount != 0,
    !is.na(MatchFunding) & MatchFunding != 0
  ) %>%
  mutate(check = (MatchFunding + CommitedFundingAmount) > EncumberedFundingAmount & (MatchFunding + CommitedFundingAmount) > FundsExpendedToDate)

view_budget_check_second <- print(budget_check_second %>% select(ProjectId, ProjectNo, CommitedFundingAmount, FundsExpendedToDate, EncumberedFundingAmount, check))
summary_budget_check_second <- view_budget_check_second %>%
  summarise(
    Total = n(),
    Matches = sum(check, na.rm = TRUE),
    Mismatches = sum(!check, na.rm = TRUE),
    percentage_match = Matches/Total*100
  )

