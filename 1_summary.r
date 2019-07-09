adult <- read.csv('data/adult.train', header=FALSE)
adult <- rbind(adult, read.csv('data/adult.test', header=FALSE))

names(adult) <- c('Age', 'Work', 'Fnlwgt', 'Education',
                  'EducationNum', 'MaritalStatus', 'Occupation',
                  'Relationship', 'Race', 'Sex', 'CapitalGain',
                  'CapitalLoss', 'WorkingHours', 'NativeCountry',
                  'Income')
adult[adult == '?'] <- NA
adult <- droplevels(adult)

attach(adult)

par(mfrow=c(2, 2))
plot(density(Age), main='Density plot of Age', ylab='Density')
plot(Work, main='Barplot of Work', ylab='Frequency')
plot(density(Fnlwgt), main='Density plot of Fnlwgt', ylab='Density')
plot(Occupation, main='Barplot of Occupation', ylab='Frequency')

par(mfrow=c(2, 2))
plot(Education, main='Barplot of Education', ylab='Frequency')
plot(density(EducationNum), main='Density plot of EducationNum', ylab='Density')
plot(MaritalStatus, main='Barplot of MaritalStatus', ylab='Frequency')
plot(Relationship, main='Barplot of Relationship', ylab='Frequency')

par(mfrow=c(2, 2))
plot(Race, main='Barplot of Race', ylab='Frequency')
plot(Sex, main='Barplot of Sex', ylab='Frequency')
plot(density(WorkingHours), main='Density plot of WorkingHours', ylab='Density')
plot(Income, main='Barplot of Income', ylab='Frequency')

par(mfrow=c(2, 1))
plot(density(CapitalGain), main='Density plot of CapitalGain', ylab='Density')
plot(density(CapitalLoss), main='Density plot of CapitalLoss', ylab='Density')


par(mfrow=c(2, 1))
plot(NativeCountry, main='Barplot of NativeCountry', ylab='Frequency')
plot(NativeCountry[NativeCountry != 'United-States'], main='Barplot of NativeCountry', ylab='Frequency')

## Bivariate analysis

n <- sort(unique(EducationNum))
l <- sapply(n, function(x) Education[EducationNum == x][1])
Education <- factor(Education, l, ordered=T)

par(mfrow=c(1, 2))
plot(Education, main='Barplot of Education', ylab='Frequency')
hist(EducationNum)

par(mfrow=c(1, 1))
plot(EducationNum ~ Education, main='Education vs EducationNum')
#Notice that both are exactly equal!

plot(CapitalLoss ~ CapitalGain, main='CapitalGain vs CapitalLoss')
any(CapitalGain != 0 & CapitalLoss != 0)
# Only one of the two is true at the same time!
# Combine them into a single, signed variable


par(mfrow=c(1, 2))
plot(Work ~ Occupation, main='Occupation vs Work')
plot(MaritalStatus ~ Relationship, main='Marital Status vs Relationship')
# Both categories should be reduced before any more conclusions

mice::md.pattern(adult)
#Only 3 variables have missing data: Work, Occupation and NativeCountry

# When only Occupation is missing, Work is always Never-worked
# It might mean that people do not have a particular occupation,
#   because they never worked.
# In these cases, they might be changed to "Non-qualified"

# When instances have 2 missings, it is always Work + Occupation
# It might also mean that people never worked
# In these cases, they might be changed to "None" + "Non-qualified"

# When instances have 1 missing, it is always NativeCountry

#Only 46 instances have all three missings
# In these cases, work and occupation can be infered the same way
# NativeCountry cannot be automatically infered

# Some missing value imputation can be done with the Occupation
