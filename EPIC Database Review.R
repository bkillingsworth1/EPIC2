#script for database review

#### packages ####
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)

# Load relevant data 

Projects_overview <- read_excel("/Users/dorji/Desktop/Projects/EPIC/EPIC Database Export May 2024.xlsx", sheet = "Projects")
Projects_detail <- read_excel("/Users/dorji/Desktop/Projects/EPIC/EPIC Database Export May 2024.xlsx", sheet = "Projects Detail")
Finance_detail <- read_excel("/Users/dorji/Desktop/Projects/EPIC/EPIC Database Export May 2024.xlsx", sheet = "Finance Detail")
Projects_metric <- read_excel("/Users/dorji/Desktop/Projects/EPIC/EPIC Database Export May 2024.xlsx", sheet = "Project Metric")

### Project overview checks ###
# how many distinct Ids, ProjectNo, ProjectName, any discrepancies - Green 

distinct_ids <- Projects_overview %>%
  distinct(Id)

distinct_projectno <- Projects_overview %>%
  distinct(ProjectNo) %>%
  filter(!is.na(ProjectNo))

distinct_name <- Projects_overview %>%
  distinct(ProjectName) %>%
  filter(!is.na(ProjectName))

#Note: Slight discrepancies to note here: there are 620 unique project IDs but the number decreases slightly to 611 project nos. (there were some NAs here)
#and to 613 project names

# how many multiphase projects and how these are reported, consistently or not
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

# average contract amount, removing zeros/NAs 
filtered_projects <- Projects_overview %>%
  filter(!is.na(ContractAmount) & ContractAmount != 0)
avg_contract_amount <- mean(filtered_projects$ContractAmount)

avg_contract_amount_by_org <- filtered_projects %>%
  group_by(ProgramAdminName) %>%
  summarise(AverageContractAmount = mean(ContractAmount, na.rm = TRUE))

# table of project leads and count of projects

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

# table of ProgramAdminName and count of projects, identify difference between this variable and ProjectLead - Green
count_of_projects_by_admin <- Projects_overview %>%
  filter(!is.na(ProgramAdminName)) %>%
  group_by(ProgramAdminName) %>%
  summarise(Count = n())

#It's definitely easier to count projects based on ProgramAdmin because there are four major overseers here - interesting to note here is that the utilities have offloaded some of the projects to smaller orgs, 
#making a strong case as theorized by BK that the observations in the ProjectLead variable are program implementers and the ProgramAdmin folks are the overall administrators/overseers

# percent of projects active/closed/pending - Green
percent_of_projects_by_status <- Projects_overview %>%
  filter(!is.na(ProjectStatus)) %>%
  group_by(ProjectStatus) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

# percent of projects listed as IsActive = TRUE but ProjectStatus = Closed, consider projectenddate, general exploration of status and date reporting discrepancies - Green

projects_active_closed <- Projects_overview %>%
  filter(IsActive == TRUE, ProjectStatus == "Closed") %>%
  summarise(percentage = (n() / nrow(Projects_overview)) * 100)

projects_active_closed_by_org <- Projects_overview %>%
  group_by(ProgramAdminName) %>%
  filter(IsActive == TRUE, ProjectStatus == "Closed") %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#All of the projects listed as active but closed should be listed as inactive (all of the project end dates have passed)
# When split by org, the CEC has the most projects that face this contradiction, followed by PG&E, SDG&E, and SCE

# percent start and end dates populated, average duration of project - Green
projects_start_end <- Projects_overview %>%
  filter(!is.na(ProjectStartDate) & !is.na(ProjectEndDate)) %>%
  summarise(percentage = (n() / nrow(Projects_overview)) * 100)

# Inspect the date format
head(Projects_overview$ProjectStartDate)
head(Projects_overview$ProjectEndDate)

# Assuming the date format is "MM/DD/YYYY" or similar, specify the format
average_project_duration <- Projects_overview %>%
  filter(!is.na(ProjectStartDate) & !is.na(ProjectEndDate)) %>%
  mutate(ProjectStartDate = as.Date(ProjectStartDate, format = "%m/%d/%Y"),
         ProjectEndDate = as.Date(ProjectEndDate, format = "%m/%d/%Y"),
         ProjectDuration = as.numeric(ProjectEndDate - ProjectStartDate)/7) %>%
  summarise(AverageDuration = mean(ProjectDuration, na.rm = TRUE))
  
# percent of projects with contact info provided (name and email), any differences by program admin (i.e. which admins are best at reporting this?)
projects_contact_info <- Projects_overview %>%
  filter(!is.na(PersonContactFirstName) & !is.na(PersonContactLastName) & !is.na(PersonContactEmail)) %>%
  summarise(percentage = (n() / nrow(Projects_overview)) * 100)

projects_contact_info_by_org <- Projects_overview %>%
  group_by(ProgramAdminName) %>%
  filter(!is.na(PersonContactFirstName) & !is.na(PersonContactLastName) & !is.na(PersonContactEmail)) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#81 percent of projects had contact info (name and email)
#PG&E and CEC were the only two program admins that were relatively consistent with providing contact info on their projects (name AND email)
#SCE (n = 23) was more consistent in giving out names than SDG&E
#SDG&E had no information at all (n = 27) compared to SCE

# percent of projects with cec manager contact info provided (name and email), any differences by program admin (i.e. which admins are best at reporting this?)
projects_CEC_contact_info <- Projects_overview %>%
  filter(!is.na(CecMgrContactFirstName) & !is.na(CecMgrContactLastName) & !is.na(CecMgrEmail)) %>%
  summarise(percentage = (n() / nrow(Projects_overview)) * 100)

projects_CEC_contact_info_by_org <- Projects_overview %>%
  group_by(ProgramAdminName) %>%
  filter(!is.na(CecMgrContactFirstName) & !is.na(CecMgrContactLastName) & !is.na(CecMgrEmail)) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(pct = n / sum(n) * 100)

#80 percent of projects had CEC contact info (name and email)
#CEC was the only program admin that provided both name and email 
#PG&E and SCE did provide names (but the way they reported it-first and last names were together in the CecMgrContactFirstName column)
#SDG&E either left it blank or the projects that they worked on did not have CEC involvement, so NA (N = 4)
