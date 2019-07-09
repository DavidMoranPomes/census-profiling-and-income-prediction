adult <- read.csv('data/adult.train', header=FALSE)
adult <- rbind(adult, read.csv('data/adult.test', header=FALSE))

names(adult) <- c('Age', 'Work', 'Fnlwgt', 'Education',
                  'EducationNum', 'MaritalStatus', 'Occupation',
                  'Relationship', 'Race', 'Sex', 'CapitalGain',
                  'CapitalLoss', 'WorkingHours', 'NativeCountry',
                  'Income')
adult[adult == '?'] <- NA

## Data cleaning, feature selection / extraction

f <- cut(adult$Age, c(-Inf, 20, 30, 40, 50, 60, Inf))
levels(f)[levels(f) == '(-Inf,20]'] <- '20-'
levels(f)[levels(f) == '(60, Inf]'] <- '60+'
adult$Age <- as.ordered(f)

work <- as.character(adult$Work)
work[work == 'Federal-gov'] <- "Gov"
work[work == 'Local-gov'] <- "Gov"
work[work == 'State-gov'] <- "Gov"
work[work == 'Without-pay'] <- "None"
work[work == 'Never-worked'] <- "None"
work[work == 'Self-emp-inc'] <- "Self"
work[work == 'Self-emp-not-inc'] <- "Self"
adult$Work <- factor(work)

adult$Fnlwgt <- NULL

education <- as.character(adult$Education)
education[education == 'Preschool'] <- 'Non-graduate'
education[education == '1st-4th'] <- 'Non-graduate'
education[education == '5th-6th'] <- 'Non-graduate'
education[education == '7th-8th'] <- 'Non-graduate'
education[education == '9th'] <- 'Non-graduate'
education[education == '10th'] <- 'Non-graduate'
education[education == '11th'] <- 'Non-graduate'
education[education == '12th'] <- 'Non-graduate'
education[education == 'HS-grad'] <- 'Graduate'
education[education == 'Some-college'] <- 'Graduate'
education[education == 'Assoc-voc'] <- 'Associate'
education[education == 'Assoc-acdm'] <- 'Associate'
education[education == 'Bachelors'] <- 'Bachelor'
education[education == 'Masters'] <- 'Master'
education[education == 'Prof-school'] <- 'Doctorate'
l <- c('Non-graduate', 'Graduate', 'Associate',
       'Bachelor', 'Master', 'Doctorate')
adult$Education <- factor(education, l, ordered=T)

adult$EducationNum <- NULL

status <- as.character(adult$MaritalStatus)
status[status == 'Separated'] <- 'Divorced'
status[status == 'Married-spouse-absent'] <- 'Married'
status[status == 'Married-AF-spouse'] <- 'Married'
status[status == 'Married-civ-spouse'] <- 'Married'
adult$MaritalStatus <- factor(status)

occup <- as.character(adult$Occupation)
occup[occup == 'Adm-clerical'] <- 'Administration'
occup[occup == 'Exec-managerial'] <- 'Executive'
occup[occup == 'Prof-specialty'] <- 'Professional'
occup[occup == 'Armed-Forces'] <- 'Non-qualified'
occup[occup == 'Farming-fishing'] <- 'Non-qualified'
occup[occup == 'Handlers-cleaners'] <- 'Non-qualified'
occup[occup == 'Machine-op-inspct'] <- 'Non-qualified'
occup[occup == 'Other-service'] <- 'Non-qualified'
occup[occup == 'Priv-house-serv'] <- 'Non-qualified'
occup[occup == 'Transport-moving'] <- 'Non-qualified'
occup[occup == 'Craft-repair'] <- 'Tech'
occup[occup == 'Tech-support'] <- 'Tech'
occup[occup == 'Protective-serv'] <- 'Protective'

adult$Occupation <- factor(occup)

adult$Relationship <- NULL

race <- as.character(adult$Race)
race[race == 'Amer-Indian-Eskimo'] <- 'Native-American'
race[race == 'Asian-Pac-Islander'] <- 'Asian'
adult$Race <- factor(race)

adult$Capital <- adult$CapitalGain - adult$CapitalLoss
adult$Capital <- cut(adult$Capital/1000, c(-Inf, -2, -0.5, 0.5, 4, Inf), ordered_result=T)
levels(adult$Capital) <- c("HighLoss", "Loss", "ZeroCapital", "Gain", "HighGain")

adult$CapitalGain <- NULL
adult$CapitalLoss <- NULL

f <- 10*round(adult$WorkingHours/10)
f[f > 70] <- 70
f <- as.ordered(f)
levels(f)[levels(f) == '70'] <- '70+'
adult$WorkingHours <- as.ordered(f)

country <- as.character(adult$NativeCountry)
country[country == 'United-States'] <- 'North-America'
country[country == 'Outlying-US(Guam-USVI-etc)'] <- 'North-America'
country[country == 'Canada'] <- 'North-America'

country[country == 'Cambodia'] <- 'Asia'
country[country == 'China'] <- 'Asia'
country[country == 'Columbia'] <- 'Asia'
country[country == 'Hong'] <- 'Asia'
country[country == 'Laos'] <- 'Asia'
country[country == 'Philippines'] <- 'Asia'
country[country == 'South'] <- 'Asia'
country[country == 'Taiwan'] <- 'Asia'
country[country == 'Thailand'] <- 'Asia'
country[country == 'Vietnam'] <- 'Asia'
country[country == 'Japan'] <- 'Asia'
country[country == 'Iran'] <- 'Asia'
country[country == 'India'] <- 'Asia'

country[country == 'Cuba'] <- 'South-America'
country[country == 'Dominican-Republic'] <- 'South-America'
country[country == 'Ecuador'] <- 'South-America'
country[country == 'El-Salvador'] <- 'South-America'
country[country == 'Guatemala'] <- 'South-America'
country[country == 'Haiti'] <- 'South-America'
country[country == 'Honduras'] <- 'South-America'
country[country == 'Jamaica'] <- 'South-America'
country[country == 'Mexico'] <- 'South-America'
country[country == 'Nicaragua'] <- 'South-America'
country[country == 'Peru'] <- 'South-America'
country[country == 'Puerto-Rico'] <- 'South-America'
country[country == 'Trinadad&Tobago'] <- 'South-America'

country[country == 'England'] <- 'Europe'
country[country == 'France'] <- 'Europe'
country[country == 'Germany'] <- 'Europe'
country[country == 'Greece'] <- 'Europe'
country[country == 'Holand-Netherlands'] <- 'Europe'
country[country == 'Hungary'] <- 'Europe'
country[country == 'Ireland'] <- 'Europe'
country[country == 'Italy'] <- 'Europe'
country[country == 'Poland'] <- 'Europe'
country[country == 'Portugal'] <- 'Europe'
country[country == 'Scotland'] <- 'Europe'
country[country == 'Yugoslavia'] <- 'Europe'
adult$NativeCountry <- factor(country)

## Missing Data

adult$Work[is.na(adult$Work)] <- 'None'
adult$Occupation[is.na(adult$Occupation)] <- 'Non-qualified'

adult <- mice::complete(mice::mice(adult, m=1))

names(adult) <- c('Age', 'Employer', 'Education',
                  'Married', 'Job', 'Race', 'Sex',
                  'WorkingHours', 'Origin', 'Income', 'Capital')

adult <- adult[, c('Age', 'Sex', 'Education', 'Married',
                   'Job', 'Employer', 'WorkingHours',
                   'Origin', 'Race', 'Capital', 'Income')]

save(adult, file='data/adult.rds')
