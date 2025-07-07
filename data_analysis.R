library(tidyverse)
library(ggplot2)

data <- read_csv("California State Data Set.csv")

colnames(data)

summary(data)

# Missing values

missingValues <- is.na(data)

# Summarize missing values per column
missingValuesCount <- colSums(missingValues)

# Convert the result to a data frame
missingValuesTable <- data.frame(Column = names(missingValuesCount),
                                 MissingValues = missingValuesCount)

print("Missing values summary:")
print(missingValuesTable)

colnames(data)

# Not considering SERIALNO, GCL, GCM, GCR, so omitting those

refData <- data %>%
  select("AGEP","CIT","COW","ENG","FER", "JWMNP", "MAR", "MIL",
         "SCHL","SEX","WAGP","GCL", "GCM", "GCR", "DIS","NATIVITY",
         "RACAIAN","RACASN","RACBLK","RACNH", "RACNUM", "RACPI", 
         "RACSOR","RACWHT")

colnames(refData)

# By Birth
# 1 .Born in the United States
# 2 .Born in Puerto Rico, Guam, the U.S. Virgin Islands, or .Northern Marianas 
# 3 .Born abroad of U.S. citizen parent or parents 
# 
# Naturalized
# 4 .U.S. citizen by naturalization 
# 
# Not Citizen
# 5 .Not a U.S. citizen

# No missing values here - taking not a Citizen as reference category
refData <- refData %>%
  mutate(
    CITBirth = if_else(CIT <= 3, 1, 0),
    CITNatur = if_else(CIT == 4, 1, 0)
  )

# COW - Current Occupation:  
# COW = 158038 40% will be excluded
# Private
# 1 .Employee of a private for-profit company or business, or of an .individual, 
# for wages, salary, or commissions 
# 2 .Employee of a private not-for-profit, tax-exempt, or .charitable organization 
# 
# Govt
# 3 .Local government employee (city, county, etc.) 
# 4 .State government employee 
# 5 .Federal government employee
# 
# SELF
# 6 .Self-employed in own not incorporated business, professional .practice, or farm 
# 7 .Self-employed in own incorporated business, professional .practice or farm 
# 8 .Working without pay in family business or farm 
# 
# UNEMPLOYED
# 9 .Unemployed and last worked 5 years ago or earlier or never .worked
data <- data[complete.cases(data$COW), ]


# Similarly, category 9 is unemployed, so we could remove that from our dataset

table(data$COW)


refData <- refData %>% #NA's will be taken as reference category
  filter(COW %in% c(1,2,3,4,5,6,7,8)) %>%
  mutate(
    CowP = if_else(COW %in% c(1,2), 1, 0), # interpretation would be 
    CowG = if_else(COW %in% c(3,4,5), 1, 0),
    # CowS = if_else(COW %in% c(6,7,8), 1, 0) to be taken as reference category
    # CowU = if_else(COW %in% c(9), 1, 0) Excluded
    #CowNA = if_else(is.na(COW), 1, 0) #NA would be 1, i.e. those that did not declare
  )

# Ability to speak English b .N/A (less than 5 years old/speaks only English) 
# ENG = 233981 59.8%
# 
# 1 .Very well 
# 2 .Well 
# 
# 3 .Not well 
# 
# 4 .Not at all

refData <- refData %>%
  mutate(
    EngVW = if_else(ENG %in% c(1), 1, 0),
    EngW = if_else(ENG %in% c(2), 1, 0),
    EngNW = if_else(ENG %in% c(3), 1, 0),
    EngNo = if_else(ENG %in% c(4), 1, 0),
    #EngNA = if_else(is.na(ENG), 1, 0) #NA would be 1
   )

sum(is.na(refData$EngW)) # check if still NA's in data

# FER = 301694 77% 
# FER Character 
# 1 Gave birth to child within the past 12 months 
# b .N/A (less than 15 years/greater than 50 years/ male) 
# 1 .Yes 
# 2 .No

refData <- refData %>%
  mutate(
    FER_YES = if_else(FER %in% c(1), 1, 0)
  )
# # Grandparents living grandchildren
# 1. Yes
# 2. No
# N/A - under 30 years old/insitutional GQ

# "Can be a metric to study, family support's effect on earnings for a female or male person:
# 
# but then the dataset should omit the 34% missing values then
# 
# Instead taken GCL_YES: i.e. Those who have them lviing  (taking those who do 
# not have and others as ref category)"

refData <- refData %>%
  mutate(
    GCL_YES = if_else(GCL %in% c(1), 1, 0)
  )

# GCM - Length of time responsible for granchildren 
# Ref category N/A - not responsible for grandchildren/under 30 years old/insitutional GQ

refData <- refData %>%
  mutate(
    GCL_blwYear = if_else(GCM %in% c(1,2), 1, 0),
    GCL_blw4Yr = if_else(GCM %in% c(3,4), 1, 0),
    GCL_5More = if_else(GCM %in% c(5), 1, 0)
  )

# GCR - Grandparents responsible for grandchildren
# Ref: N/A - not living with grandchildren/under 30 years old/insitutional GQ

refData <- refData %>%
  mutate(
    GCR_YES = if_else(GCR %in% c(1), 1, 0)
  )


# Since we removed those who do not work earlier,
# its safe to assume that these people work from home

# refData <- refData %>%
#   mutate(
#     Jwmnpn = if_else(is.na(JWMNP), 1, 0)
#   )

# MAR Character 1 Marital status 
# 1 .Married 
# 
# WSD
# 2 .Widowed 
# 3 .Divorced 
# 4 .Separated 
# 5 .Never married or under 15 years old

refData <- refData %>%
  mutate(
    MarM = if_else(MAR %in% c(1), 1, 0),
    MarWDS = if_else(MAR %in% c(2,3,4), 1, 0)
    #MarNever = if_else(MAR %in% c(5), 1, 0) ref category
  )





# Military service
# b .N/A (less than 17 years old)
# MIL = 69459 17.8%
# 1 .Now on active duty
# 2 .On active duty in the past, but not now
# 3 .Only on active duty for training in Reserves/National Guard
# 4 .Never served in the military

refData <- refData %>%
  mutate(
    MilA = if_else(MIL %in% c(1), 1, 0),
    MilP = if_else(MIL %in% c(2), 1, 0),
    MilRN = if_else(MIL %in% c(3), 1, 0),
    # MilNever = if_else(MIL %in% c(4), 1, 0)
    # MilNA = if_else(is.na(MIL),1, 0) ref category
  )


# "SCHL - Educational attainment
# SCHL = 10070 2.5%
# bb .N/A (less than 3 years old)
#
# NO Scooling
# 01 .No schooling completed
# 02 .Nursery school, preschool
# 03 .Kindergarten
#
# GRADE LEVEL 1-12 
# 04 .Grade 1 05 .Grade 2 06 .Grade 3 07 .Grade 4 08 .Grade 5 09 .Grade 6 
# 10 .Grade 7 11 .Grade 8 12 .Grade 9 13 .Grade 10 14 .Grade 11 
# 15 .12th grade - no diploma
#
# HIGH SCHOOL AND BEYOND
# 16 .Regular high school diploma
# 17 .GED or alternative credential
#
# COLLEGE NOT GRADUATED
# 18 .Some college, but less than 1 year
# 19 .1 or more years of college credit, no degree
#
# 20 .Associate's degree
# 21 .Bachelor's degree
# 22 .Master's degree
# 23 .Professional degree beyond a bachelor's degree - CERTIFICATION
# 24 .Doctorate degree"

refData <- refData %>%
  mutate(
    # SchlNo = if_else(SCHL %in% c(1,2,3), 1, 0), reference category
    SchlGrade = if_else(SCHL %in% c(4,5,6,7,8,9,
                              10,11,12,13,
                              14,15), 1, 0),
    SchlHS = if_else(SCHL %in% c(16,17), 1, 0),
    SchlCNG = if_else(SCHL %in% c(18,19), 1, 0),
    SchlAD = if_else(SCHL %in% c(20), 1, 0),
    SchlBD = if_else(SCHL %in% c(21), 1, 0),
    SchlMD = if_else(SCHL %in% c(22), 1, 0),
    SchlPD = if_else(SCHL %in% c(23), 1, 0),
    SchlDD = if_else(SCHL %in% c(24), 1, 0)
    # SchlNA = if_else(is.na(SCHL),1, 0) we do not have NA's anymore after removing not working
  )

# Sex
# 1 .Male = 0
# 2 .Female = 1


refData <- refData %>%
  mutate(
    SEX = if_else(SEX %in% c(1), 1, 0)
  )

# "Wages or salary income past 12 months (use ADJINC to adjust WAGP to
# constant dollars)
# bbbbbb .N/A (less than 15 years old)
# 0 .None
# 4..999999 .$4 to 999999 (Rounded and top-coded) DEPENDENT VARIABLE"

# no wage - WagNoWage = 1
# no data - WagpNA = 1

# WAGP = 59748 15.2%
# Wage N/A's removed when 'not working' subset removed
# refData <- refData %>%
#   mutate(
#     WagNoWage = if_else(WAGP %in% c(0), 1, 0),
#     WagpNA = if_else(is.na(WAGP),1, 0)
#   )

# Disability recode
# 1 .With a disability
# 2 .Without a disability

refData <- refData %>%
  mutate(
    DIS = if_else(DIS %in% c(1), 1, 0)
  )

# "Nativity
# 1 .Native
# 2 .Foreign born"

refData <- refData %>%
  mutate(
    NATIVITY = if_else(NATIVITY %in% c(1), 1, 0)
  )


wageData<- refData[ ,c("AGEP", "SEX", "WAGP", "NATIVITY", "DIS", "RACAIAN",
                    "RACASN", "RACBLK", "RACNH", "RACNUM", "RACPI", 
                    "RACSOR", "RACWHT", "CITBirth", "CITNatur", 
                    "CowP", "CowG", "EngVW", "EngW", "EngNW", "EngNo", 
                    "FER_YES", "GCL_YES", "GCL_blwYear", "GCL_blw4Yr", 
                    "GCL_5More", "GCR_YES", "MarM", "MarWDS", "MilA", 
                    "MilP", "MilRN", "SchlGrade", "SchlHS", "SchlCNG", 
                    "SchlAD", "SchlBD", "SchlMD", "SchlPD", "SchlDD")]

# Checking Correlation
round(cor(wageData), 4)

# Nativity and CITBirth, CITNatur are highly correlated, so remove Nativity
# Similarly GCR_YES and GCL_5More are highly correlated, ro remove GCR

model1 <- lm(WAGP ~ AGEP + SEX + DIS + RACAIAN +
               RACASN + RACBLK + RACNH +  RACNUM + RACPI +  RACSOR +
               RACWHT + CITBirth + CITNatur + CowP + CowG + EngVW +  
               EngW +   EngNW +  FER_YES +  GCL_YES +   GCL_blwYear +  GCL_blw4Yr + GCL_5More +   MarM + MarWDS +   MilA + MilP + MilRN +  SchlGrade +
               SchlHS +   SchlCNG +  SchlAD + SchlBD + SchlMD + 
               SchlPD +   SchlDD, data = wageData)

summary(model1)
                
# Since none of the race factors are significant, let's try merging some

wageData <- mutate(wageData, RACE_Other = ifelse(RACAIAN == 1 | 
                                                   RACNH == 1 | 
                                                   RACPI == 1, 1, 0))


# Race-only model
raceModel <- lm(WAGP ~ RACASN + RACBLK + RACE_Other + RACWHT, data = wageData)
                
summary(raceModel)



# Interaction Terms - Race & English Speaking ability
# Taking EngVW as interaction term (i.e. English speaking skill - Very well)

raceEngModel <- lm(WAGP ~ RACASN + RACBLK + RACE_Other  + RACASN*EngVW +
                     RACBLK*EngVW + RACE_Other*EngVW + RACWHT*EngVW + EngVW,  
                   data = wageData)

summary(raceEngModel)

                


feSel <- refData[complete.cases(refData$ENG), ]


wageData <- mutate(wageData, OtherRaces = ifelse(RACAIAN == 1 | 
                                                   RACNH == 1 | 
                                                   RACPI == 1, 1, 0))



feSel <- mutate(feSel, 
        # combing American Indian, Hawaiian, and Pacific Islander into a single one
        OtherRaces = ifelse(RACAIAN == 1 | RACNH == 1 | RACPI == 1, 1, 0))

feSel <- mutate(feSel,
        # Combing schooling levels
        lessThanBachelors = ifelse(SchlGrade == 1 | SchlHS == 1 | SchlCNG == 1
                                | SchlAD == 1, 1, 0 ))

feSel <- mutate(feSel,
        # Combining all military into one
        MilitaryActiveOrPast = ifelse(MilA == 1 | MilP == 1, 1, 0))
 

feSel <- feSel %>%
  mutate(
        # Changing nomenclature
        BachelorsDegree = SchlBD,
        MastersDegree = SchlMD,
        TerminalDegree = SchlDD,
        
        WageEarnings = WAGP,
        Age = AGEP,
        Disability = DIS,
        CitizenByBirth = CITBirth,
        PrivateEmployee = CowP,
        GaveBirthRecently = FER_YES,
        GrandParentsSupport = GCL_YES,
        Marrried = MarM,
        EnglishSpeakingVeryWell = EngVW,
        Asian = RACASN, 
        Black = RACBLK,
        White = RACWHT,
        
        )



modelRes <- lm(WageEarnings ~
               Asian + Black +  OtherRaces +
               EnglishSpeakingVeryWell +
               Age + SEX + Disability +
               lessThanBachelors + BachelorsDegree + 
                 MastersDegree + TerminalDegree +
               CitizenByBirth +
               PrivateEmployee +
               Marrried,
             data = feSel)

summary(modelRes)

modelResInteractionTerms <- lm(WageEarnings ~
                 Asian + Black +  OtherRaces +
                   EnglishSpeakingVeryWell +
                 Age + SEX + Disability +
                 lessThanBachelors + BachelorsDegree + 
                   MastersDegree + TerminalDegree +
                 CitizenByBirth +
                 PrivateEmployee +
                 Marrried +
                   Asian*EnglishSpeakingVeryWell +  
                   Black*EnglishSpeakingVeryWell+  
                   OtherRaces*EnglishSpeakingVeryWell,
               data = feSel)

summary(modelResInteractionTerms)

#Standard Deviation
sapply(wageData, sd)


"
All Races include

Age include
Sex inclue
Disability include

RECNUM and RACSOR - not required. omit

CITBirth - further consolidation required to CIT by birth = 1, all else 0 (no missing values)

CowP include - make it only column there is from this type?

GCL_Yes include - others remove no significance

GCM and GCR - more missing data than there is data
MarM include - remove all else
MilM include - remove all else

" 