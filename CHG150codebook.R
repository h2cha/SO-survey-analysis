#' ---
#' title: "Code book - 2018 StackOverflow survey"
#' author: "Hyungwon Cha"
#' date: "2018-12-11"
#' ---
#' 

#+ message = FALSE, echo = FALSE, comment = NA
rm(list=ls())
#.libPaths("d:/00C/R_LIB")  
#setwd("D:\\BigData\\classUpload&HW\\Code book_sales_customer churn") 
#getwd()
options(width = 120, scipen=999)

#+ message = FALSE, comment = NA
library(dplyr)
library(knitr)
library(kableExtra)

#---------------------------
#' ## Read data
#' 
survey <- read.csv("stack-overflow-2018-developer-survey/cleansed.csv")
dim(survey)

tables <- list()
updateTables <- function(tables) {
  for(i in names(survey)) {
    t <- table(survey[,i], useNA = "always")
    tables[[i]] <- t
    #filename <- paste("hyunjun/question", i, ".csv", sep = "_")
    #write.csv(t, filename)
  }
  return(tables)
}
tables <- updateTables(tables)
variables <- c("OpenSource",
              "FormalEducation",
              "UndergradMajor",
              "CompanySize",
              "YearsCoding",
              "YearsCodingProf",
              "ConvertedSalary",
              "LanguageWorkedWith",
              "DatabaseWorkedWith",
              "PlatformWorkedWith",
              "FrameworkWorkedWith",
              "OperatingSystem",
              "CheckInCode",
              "WakeTime",
              "HoursOutside",
              "SkipMeals",
              "Exercise",
              "Gender",
              "SexualOrientation",
              "EducationParents",
              "RaceEthnicity",
              "Age")

descriptions <- c("Do you contribute to open source projects?",
                  "Which of the following best describes the highest level of formal education that you’ve completed?",
                  "You previously indicated that you went to a college or university. Which of the following best describes your main field of study (aka 'major')",
                  "Approximately how many people are employed by the company or organization you work for?",
                  "Including any education, for how many years have you been coding?",
                  "For how many years have you coded professionally (as a part of your work)?",
                  "Salary converted to annual USD salaries using the exchange rate on 2018-01-18, assuming 12 working months and 50 working weeks.",
                  "Which of the following programming, scripting, and markup languages have you done extensive development work in over the past year, and which do you want to work in over the next year?  (If you both worked with the language and want to continue to do so, please check both boxes in that row.)",
                  "Which of the following database environments have you done extensive development work in over the past year, and which do you want to work in over the next year?   (If you both worked with the database and want to continue to do so, please check both boxes in that row.)",
                  "Which of the following platforms have you done extensive development work for over the past year?   (If you both developed for the platform and want to continue to do so, please check both boxes in that row.)",
                  "Which of the following libraries, frameworks, and tools have you done extensive development work in over the past year, and which do you want to work in over the next year?",
                  "What is the primary operating system in which you work?",
                  "Over the last year, how often have you checked-in or committed code?",
                  "On days when you work, what time do you typically wake up?",
                  "On a typical day, how much time do you spend outside?",
                  "In a typical week, how many times do you skip a meal in order to be more productive?",
                  "In a typical week, how many times do you exercise?",
                  "Which of the following do you currently identify as? Please select all that apply. If you prefer not to answer, you may leave this question blank.",
                  "Which of the following do you currently identify as? Please select all that apply. If you prefer not to answer, you may leave this question blank.",
                  "What is the highest level of education received by either of your parents? If you prefer not to answer, you may leave this question blank.",
                  "Which of the following do you identify as? Please check all that apply. If you prefer not to answer, you may leave this question blank.",
                  "What is your age? If you prefer not to answer, you may leave this question blank.")
values <- character(22)
for(i in c(1:22)) {
  c <- paste(names(tables[[i+1]]), collapse=" / ")
  values[i] <- c
}


codebook_survey <- data.frame(
  variable = variables,
  Descriptions = descriptions,
  Value = values)

#' &nbsp;
#' &nbsp;
#' 
#-------------------------------------------------------------------------
#' ## A Simple Codebook 
#' 
kable(codebook_survey)

#' &nbsp;
#' 

#' ## Data Dimensions: rows and variables 
#' * **Rows**: 5,190 units (cases)
#' * **Variables**: 23 columns

#--- View(codebook_sales)

