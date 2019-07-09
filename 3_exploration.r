load('data/adult.rds')

attach(adult)

## Univariate analysis

par(mfrow=c(2, 2))
plot(Age, main='Age', ylim=c(0, nrow(adult)))
plot(Sex, main='Sex', ylim=c(0, nrow(adult)))
plot(Education, main='Education', ylim=c(0, nrow(adult)))
plot(Married, main='Married', ylim=c(0, nrow(adult)))

par(mfrow=c(2, 2))
plot(Job, main='Job', ylim=c(0, nrow(adult)))
plot(Employer, main='Employer', ylim=c(0, nrow(adult)))
plot(WorkingHours, main='Working Hours', ylim=c(0, nrow(adult)))
plot(From, main='Origin', ylim=c(0, nrow(adult)))

par(mfrow=c(2, 2))
plot(Race, main='Race', ylim=c(0, nrow(adult)))
plot(Capital, main='Capital', ylim=c(0, nrow(adult)))
plot(Income, main='Income', ylim=c(0, nrow(adult)))

## Bivariate analysis

par(mfrow=c(2, 2))
plot(Income ~ Age, main='Income vs Age')
plot(Income ~ Sex, main='Income vs Sex')
plot(Income ~ Education, main='Income vs Education')
plot(Income ~ Married, main='Income vs Married')

par(mfrow=c(2, 2))
plot(Income ~ WorkingHours, main='Income vs Working Hours')
plot(Income ~ Capital, main='Income vs Capital')
plot(Income ~ Origin, main='Income vs Origin')
plot(Income ~ Race, main='Income vs Race')

par(mfrow=c(2, 1))
plot(Income ~ Employer, main='Income vs Employer')
plot(Income ~ Job, main='Income vs Job')

## Outlier detection

# Since all variables are categorical, there is no concept of "outlier" other than a certain
# combination of variable values being less likely to happen.
# Also, discretization helps with the dealing of the outlier problem in some sense.
