load('data/adult_split.rds')

chisq.test(adult$Age, adult$Set)
chisq.test(adult$Sex, adult$Set)
chisq.test(adult$Education, adult$Set)
chisq.test(adult$Married, adult$Set)
chisq.test(adult$Employer, adult$Set)
chisq.test(adult$Job, adult$Set)
chisq.test(adult$WorkingHours, adult$Set)
chisq.test(adult$Origin, adult$Set)
chisq.test(adult$Race, adult$Set)
chisq.test(adult$Capital, adult$Set)
chisq.test(adult$Income, adult$Set)

tb <- rbind(
  table(adult$Age, adult$Set),
  table(adult$Sex, adult$Set),
  table(adult$Education, adult$Set),
  table(adult$Married, adult$Set),
  table(adult$Employer, adult$Set),
  table(adult$WorkingHours, adult$Set),
  table(adult$Origin, adult$Set),
  table(adult$Race, adult$Set),
  table(adult$Capital, adult$Set),
  table(adult$Income, adult$Set)
)

chisq.test(tb)
