load('data/adult.rds')

set.seed(1234)
train <- sample(nrow(adult), 0.5*nrow(adult))

set <- logical(nrow(adult))
set[train] <- TRUE

adult$Set <- factor(ifelse(set, 'Train', 'Test'))

save(adult, file='data/adult_split.rds')
