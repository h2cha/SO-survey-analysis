library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)


variables <- c("Hobby",
               "OpenSource",
               "Country",
               "Employment",
               "FormalEducation",
               "UndergradMajor",
               "CompanySize",
               "DevType",
               "YearsCoding",
               "YearsCodingProf",
               "ConvertedSalary",
               "LanguageWorkedWith",
               "DatabaseWorkedWith",
               "PlatformWorkedWith",
               "FrameworkWorkedWith",
               "IDE",
               "OperatingSystem",
               "CheckInCode",
               "WakeTime",
               "HoursComputer",
               "HoursOutside",
               "SkipMeals",
               "Exercise",
               "Gender",
               "SexualOrientation",
               "EducationParents",
               "RaceEthnicity",
               "Age")

survey <- read.csv("stack-overflow-2018-developer-survey/survey_results_public.csv")
#----------------------------------------------------------------------------------
# removing unnecessary columns
cleansed <- select(survey, variables)
#----------------------------------------------------------------------------------
tables <- list()
updateTables <- function(tables) {
  for(i in names(cleansed)) {
    t <- table(cleansed[,i], useNA = "always")
    tables[[i]] <- t
    #filename <- paste("hyunjun/question", i, ".csv", sep = "_")
    #write.csv(t, filename)
  }
  return(tables)
}
#----------------------------------------------------------------------------------
# filtering out the rows where Country is USA
countries <- table(cleansed$Country)
for(i in seq_along(countries)) {
  if (countries[[i]] > 5000) {
    print(countries[i])
  }
}
# United States = 20309
# India = 13721
# Genrmany = 6459
cleansed <- filter(cleansed, Country == "United States")
cleansed <- select(cleansed, -Country)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# filtering out the rows where Employment is full-time
employment <- table(cleansed$Employment)
employment
# Employed full-time 70495 out of 98855
cleansed <- filter(cleansed, Employment == "Employed full-time")
cleansed <- select(cleansed, -Employment)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# selecting only people who describe themselves as a back-end developer
isBackend <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(x) {
    x <- strsplit(x,";")
    for(type in x[[1]]) {
      if (type == "Back-end developer") {
        return(TRUE)
      }
    }
    return(FALSE)
  })
  return(col)
}
cleansed <- filter(cleansed, !is.na(DevType))
cleansed <- filter(cleansed, isBackend(DevType))
cleansed <- select(cleansed, -DevType)
#----------------------------------------------------------------------------------


#----------------------------------------------------------------------------------
# filtering out the rows where ConvertedSalary is not NA
salary <- table(cleansed$ConvertedSalary)
numNA <- dim(cleansed)[1]
for(i in seq_along(salary)) {
  numNA <- numNA - salary[[i]]
}
numNA
# Salary NA = 4535 out of 16031
cleansed <- filter(cleansed, !is.na(ConvertedSalary))
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# cutting out salary range that doesn't seem relavant
cs <- cleansed$ConvertedSalary
for(i in seq(0.9:1, by=0.002)) {
  print(i)
  print(quantile(cs, i))
}
# Choosing $0 - $350,000 as a range as it covers about 95% of data
cleansed <- filter(cleansed, ConvertedSalary <= 350000)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# creating new columns that counts the number of multiple options selected for some columns
countOpts <- function(col, groupF) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    opts <- strsplit(str,";")
    return (length(opts[[1]]))
  })
  col <- sapply(col, groupF)
  return(as.factor(col))
}

groupNumLang <- function(x) {
  if (x >= 11) {
    return("more than 10 languages")
  } else if (x <= 3) {
    return("Less than 4 languages") 
  } else {
    return(x)
  }
}

groupNumDB <- function(x) {
  if (x >= 6) {
    return("more than 5 databases")
  } else {
    return(x)
  }
}
groupNumPlat <- function(x) {
  if (x >= 6) {
    return("more than 5 platforms")
  } else {
    return(x)
  }
}
groupNumFramework <- function(x) {
  if (x >= 4) {
    return("more than 3 frameworks")
  } else {
    return(x)
  }
}
groupNumIDE <- function(x) {
  if (x >= 6) {
    return("more than 5 IDE's")
  } else {
    return(x)
  }
}

cleansed <- dplyr::mutate(cleansed, LanguageWorkedWith = countOpts(LanguageWorkedWith, groupNumLang))
ggplot(cleansed, aes(x=LanguageWorkedWith, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)

cleansed <- dplyr::mutate(cleansed, DatabaseWorkedWith = countOpts(DatabaseWorkedWith, groupNumDB))
ggplot(cleansed, aes(x=DatabaseWorkedWith, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
# numDB seems correlated with one's salary

cleansed <- dplyr::mutate(cleansed, PlatformWorkedWith = countOpts(PlatformWorkedWith, groupNumPlat))
ggplot(cleansed, aes(x=PlatformWorkedWith, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)

cleansed <- dplyr::mutate(cleansed, FrameworkWorkedWith = countOpts(FrameworkWorkedWith, groupNumFramework))
ggplot(cleansed, aes(x=FrameworkWorkedWith, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)


cleansed <- dplyr::mutate(cleansed, IDE = countOpts(IDE, groupNumIDE))
ggplot(cleansed, aes(x=IDE, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
cleansed <- select(cleansed, -IDE)
# Number of IDE seems not correlated with one's salary
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
ggplot(cleansed, aes(x=Hobby, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
summary(dplyr::filter(dplyr::select(cleansed, Hobby, ConvertedSalary), Hobby=="Yes"))
summary(dplyr::filter(dplyr::select(cleansed, Hobby, ConvertedSalary), Hobby=="No"))
# hobby is not significant
cleansed <- select(cleansed, -Hobby)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
ggplot(cleansed, aes(x=OpenSource, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
summary(dplyr::filter(dplyr::select(cleansed, OpenSource, ConvertedSalary), OpenSource=="Yes"))
summary(dplyr::filter(dplyr::select(cleansed, OpenSource, ConvertedSalary), OpenSource=="No"))
# devs who contribute to the open source project tend to earn more money(about $1.6k)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
regroupFormalEducation <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "I never completed any formal education" | str == "Primary/elementary school" |
        str == "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)") {
      return("Less than or equal to Secondary school")
    } else if (str == "Master’s degree (MA, MS, M.Eng., MBA, etc.)" | str == "Other doctoral degree (Ph.D, Ed.D., etc.)" |
               str == "Professional degree (JD, MD, etc.)") {
      return("Master's degree (MA, MS, M.Eng., MBA, etc.) or higher")
    } else {
      return(str)
    }
  })
  return(as.factor(col))
}
cleansed <- filter(cleansed, !is.na(FormalEducation))
cleansed <- mutate(cleansed, FormalEducation = regroupFormalEducation(FormalEducation))
ggplot(cleansed, aes(x=FormalEducation, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
tables <- updateTables(tables)
tables$FormalEducation
# The salary seems to vary depending on one's formal education (at most about $50k)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
regroupMajor <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "A business discipline (ex. accounting, finance, marketing)" | 
        str == "A health science (ex. nursing, pharmacy, radiology)" | 
        str == "A humanities discipline (ex. literature, history, philosophy)" |
        str == "A natural science (ex. biology, chemistry, physics)" |
        str == "A social science (ex. anthropology, psychology, political science)" |
        str == "Fine arts or performing arts (ex. graphic design, music, studio art)" |
        str == "I never declared a major") {
      return("Not related to Computer Science")
    } else if (str == "Another engineering discipline (ex. civil, electrical, mechanical)" |
               str == "Information systems, information technology, or system administration" |
               str == "Mathematics or statistics" |
               str == "Web development or web design") {
      return("Related to Computer Science")
    } else if (str == "Computer science, computer engineering, or software engineering") {
      return("Computer Science") 
    } else {
      return(str)
    }
  })
  return(as.factor(col))
}
ggplot(cleansed, aes(x=UndergradMajor, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
# The salary seems to vary depending on one's undergrad major (at most about $20k)
cleansed <- cleansed <- filter(cleansed, !is.na(UndergradMajor))
cleansed <- mutate(cleansed, UndergradMajor = regroupMajor(UndergradMajor))
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
ggplot(cleansed, aes(x=CompanySize, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
# The larger the company, the more money devs tend to earn
cleansed <- cleansed <- filter(cleansed, !is.na(CompanySize))
tables <- updateTables(tables)
tables$CompanySize
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
regroupYearsCoding <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "0-2 years" | str == "3-5 years") {
      return("Less than 6 years")
    } else if (str == "21-23 years" | str == "24-26 years" | str == "27-29 years" | str == "30 or more years") {
      return("More than 20 years")
    } else {
      return(str)
    }
  })
  return(as.factor(col))
}
cleansed <- filter(cleansed, !is.na(YearsCoding))
cleansed <- mutate(cleansed, YearsCoding = regroupYearsCoding(YearsCoding))
ggplot(cleansed, aes(x=YearsCoding, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
# The years of coding seems positively relative to the salary
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
regroupYearsCodingProf <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "21-23 years" | str == "24-26 years" | str == "27-29 years" | str == "30 or more years") {
      return("More than 20 years")
    } else {
      return(str)
    }
  })
  return(as.factor(col))
}
cleansed <- filter(cleansed, !is.na(YearsCodingProf))
cleansed <- mutate(cleansed, YearsCodingProf = regroupYearsCodingProf(YearsCodingProf))
ggplot(cleansed, aes(x=YearsCodingProf, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
# The years of coding seems positively relative to the salary
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
cleansed <- filter(cleansed, !is.na(OperatingSystem))
regroupOS <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "BSD/Unix") {
      return("Linux-based")
    } else {
      return(str)
    }
  })
  return(as.factor(col))
}
cleansed <- mutate(cleansed, OperatingSystem = regroupOS(OperatingSystem))
ggplot(cleansed, aes(x=OperatingSystem, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
# MacOS > Linux > Windows
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
ggplot(cleansed, aes(x=CheckInCode, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
# The frequency of code check in seems positively relative to the salary
tables <- updateTables(tables)
tables$CheckInCode
cleansed <- filter(cleansed, !is.na(CheckInCode))
regroupCheckIn <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "Never" | str == "Less than once per month" | str == "Weekly or a few times per month") {
      return("Less than a few times per month")
    } else {
      return(str)
    }
  })
  return(as.factor(col))
}
cleansed <- mutate(cleansed, CheckInCode = regroupCheckIn(CheckInCode))
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
ggplot(cleansed, aes(x=WakeTime, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
# The wake time does seems to have correlation with one's salary
tables <- updateTables(tables)
tables$WakeTime
cleansed <- filter(cleansed, !is.na(WakeTime))
regroupWakeTime <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "After 12:01 PM" | str == "Between 11:01 AM - 12:00 PM" | str == "Between 10:01 - 11:00 AM" |
        str == "Between 9:01 - 10:00 AM" | str == "Between 8:01 - 9:00 AM") {
      return("After 8:01 AM")
    } else if (str =="Between 5:00 - 6:00 AM" | str == "Before 5:00 AM") {
      return("Before 6:00 AM")
    } else if (str =="I do not have a set schedule" | str == "I work night shifts") {
      return("Others")
    } else {
      return(str)
    }
  })
  return(as.factor(col))
}
cleansed <- mutate(cleansed, WakeTime = regroupWakeTime(WakeTime))
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
cleansed <- filter(cleansed, !is.na(HoursComputer))
regroupHoursComputer <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "1 - 4 hours" | str == "5 - 8 hours" | str == "Less than 1 hour") {
      return("Less than 9 hours")
    } else {
      return(str)
    }
  })
  return(as.factor(col))
}
cleansed <- mutate(cleansed, HoursComputer = regroupHoursComputer(HoursComputer))
ggplot(cleansed, aes(x=HoursComputer, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
cleansed <- select(cleansed, -HoursComputer)
# hours of computer does seems to correlate with one's salary
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
tables <- updateTables(tables)
tables$HoursOutside
cleansed <- filter(cleansed, !is.na(HoursOutside))
regroupHoursOutside <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "1 - 2 hours" | str == "3 - 4 hours" | str == "Over 4 hours") {
      return("Over 1 hour")
    } else {
      return(str)
    }
  })
  return(as.factor(col))
}
cleansed <- mutate(cleansed, HoursOutside = regroupHoursOutside(HoursOutside))
ggplot(cleansed, aes(x=HoursOutside, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
# The longer they spend time outside, the more money they tend to earn
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
cleansed <- filter(cleansed, !is.na(SkipMeals))
ggplot(cleansed, aes(x=SkipMeals, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
cleansed <- filter(cleansed, !is.na(Exercise))
ggplot(cleansed, aes(x=Exercise, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
regroupEducationParents <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "They never completed any formal education" | str == "Primary/elementary school" |
        str == "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)") {
      return("Less than or equal to Secondary school")
    } else if (str == "Master’s degree (MA, MS, M.Eng., MBA, etc.)" | str == "Other doctoral degree (Ph.D, Ed.D., etc.)" |
               str == "Professional degree (JD, MD, etc.)") {
      return("Master's degree (MA, MS, M.Eng., MBA, etc.) or higher")
    } else {
      return(str)
    }
  })
  return(as.factor(col))
}

cleansed <- filter(cleansed, !is.na(EducationParents))
cleansed <- mutate(cleansed, EducationParents = regroupEducationParents(EducationParents))
ggplot(cleansed, aes(x=EducationParents, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
tables$Age
cleansed <- filter(cleansed, !is.na(Age))
regroupAge <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "45 - 54 years old" | str == "55 - 64 years old" | str == "65 years or older") {
      return("45 years or older")
    } else {
      return(str)
    }
  })
  return(as.factor(col))
}
cleansed <- mutate(cleansed, Age = regroupAge(Age))
ggplot(cleansed, aes(x=Age, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
cleansed <- filter(cleansed, !is.na(Gender))
regroupGender <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "Male" | str == "Female") {
      return(str)
    } else {
      return("Others")
    }
  })
  return(as.factor(col))
}
cleansed <- mutate(cleansed, Gender = regroupGender(Gender))
ggplot(cleansed, aes(x=Gender, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
cleansed <- filter(cleansed, !is.na(SexualOrientation))
regroupSexualOrientation <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "Straight or heterosexual") {
      return(str)
    } else {
      return("Others")
    }
  })
  return(as.factor(col))
}
cleansed <- mutate(cleansed, SexualOrientation = regroupSexualOrientation(SexualOrientation))
ggplot(cleansed, aes(x=SexualOrientation, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
#----------------------------------------------------------------------------------
cleansed <- filter(cleansed, !is.na(RaceEthnicity))
regroupRace <- function(col) {
  col <- as.character(col)
  col <- sapply(col, function(str) {
    if (str == "White or of European descent") {
      return(str)
    } else {
      return("Others")
    }
  })
  return(as.factor(col))
}
cleansed <- mutate(cleansed, RaceEthnicity = regroupRace(RaceEthnicity))
ggplot(cleansed, aes(x=RaceEthnicity, y=ConvertedSalary)) + theme_bw() + geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 4)
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
regroupSalary <- function(col) {
  col <- as.integer(col)
  col <- sapply(col, function(int) {
    if (int < 70000) {
      return("Less than $70,000")
    } else if (70000 <= int & int <= 99999) {
      return("$70,000 - $99,999")
    } else if (100000 <= int & int <= 129999) {
      return("$100,000 - $129,999")
    } else if (130000 <= int & int <= 159999) {
      return("$130,000 - $159,999")
    } else if (int >= 160000) {
      return("More than $160,000")
    } else {
      return(int)
    }
  })
  return(as.factor(col))
}

cleansed <- mutate(cleansed, ConvertedSalary = regroupSalary(ConvertedSalary))
tables <- updateTables(tables)
tables$ConvertedSalary
ggplot(cleansed, aes(x=factor(ConvertedSalary))) + geom_histogram(stat="count")

#----------------------------------------------------------------------------------
names(cleansed)

write.csv(cleansed, "stack-overflow-2018-developer-survey/cleansed.csv")

tables <- updateTables(tables)
