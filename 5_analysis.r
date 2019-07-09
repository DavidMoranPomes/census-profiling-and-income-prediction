load('data/adult_split.rds')
ix <- which(adult$Set == 'Test')

## MCA

adult.mca <- FactoMineR::MCA(adult[,-12], quali.sup=11, ind.sup=ix, graph=FALSE)

plot(adult.mca$eig[,1], type='b', pch=19, main="screeplot of the eigenvalues", xlab="Dimension", ylab='Eigenvalue')
barplot(adult.mca$eig[,3], main="Cumulative % of variance", xlab="Dimension")

plot(adult.mca$eig[,1], type='b', pch=19, main="screeplot of the eigenvalues", xlab="Dimension", ylab='Eigenvalue')
abline(h=adult.mca$eig[4, 1], lty=2)
barplot(adult.mca$eig[,3], main="Cumulative % of variance", xlab="Dimension")
abline(h=adult.mca$eig[4, 3], lty=2)

adult.mca <- FactoMineR::MCA(adult[,-12], quali.sup=11, ind.sup=ix, graph=FALSE, ncp=4)

## Visualization

view <- function(var.name='', fp=1) {
  fct <- fp*2 + (-1:0)
  if (var.name == '') {
    plot(adult.mca$ind$coord[, fct],
         pch=19, cex=0.5,
         main=paste0('Factorial plane ', fp))
  } else {
    var <- adult[-ix, var.name]
    lvls <- levels(var)
    cl <- as.integer(var) + 1
    cols <- 2:(length(lvls)+1)
    categs <- adult.mca$var$coord[lvls, fct]
    plot(adult.mca$ind$coord[, fct],
         pch=19, cex=0.5,
         main=paste0('Factorial plane ', fp, ': ', var.name), col=cl)
    points(categs, pch=24, cex=2,
           bg=cols, lwd=2)
    legend('topleft', legend=lvls, fill=cols)
  }
}

view.income <- function(fp=1) {
  var <- adult[-ix, 'Income']
  lvls <- levels(var)
  cl <- as.integer(var) + 1
  cols <- 2:(length(lvls)+1)
  fct <- fp*2 + (-1:0)
  categs <- adult.mca$quali.sup$coord[lvls, fct]
  plot(adult.mca$ind$coord[, fct],
       pch=19, cex=0.5,
       main=paste0('Factorial plane ', fp, ': Income'), col=cl)
  points(categs, pch=24, cex=2,
         bg=cols, lwd=2)
  legend('topleft', legend=lvls, fill=cols)
}

fp <- 1
view('', fp)
view('Age', fp)
view('Sex', fp)
view('Education', fp)
view('Married', fp)
view('Job', fp)
view('Employer', fp)
view('WorkingHours', fp)
view('Capital', fp)
view('Origin', fp)
view('Race', fp)

view.income(fp)


## Latent factors

desc <- FactoMineR::dimdesc(adult.mca, axes=1:4)

do.desc <- function(d=1) {
  desc.d <- desc[[paste('Dim',  d)]]
  vars <- desc.d$quali[desc.d$quali[,1] > 0.25, 1]
  mods <- Reduce(c, sapply(adult[,names(vars)], levels))
  mods <- mods[mods %in% row.names(desc.d$category)]
  list(vars=t(t(vars)), mods=t(t(desc.d$category[mods, 1])))
}

desc1 <- do.desc(1)
desc2 <- do.desc(2)
desc3 <- do.desc(3)
desc4 <- do.desc(4)



## Clustering
memory.limit(10000000000)
clust.1 <- FactoMineR::HCPC(adult.mca, method='ward', consol=FALSE, kk=10000, nb.clust=3)
cl.1 <- clust.1$data.clust$clust
ch.1 <- fpc::calinhara(adult.mca$ind$coord, cl.1, 3)

clust.2 <- FactoMineR::HCPC(adult.mca, method='ward', consol=FALSE, kk=10000, nb.clust=6)
cl.2 <- clust.2$data.clust$clust
ch.2 <- fpc::calinhara(adult.mca$ind$coord, cl.2, 6)

plot(adult.mca$ind$coord[,1:2], pch=19, cex=0.5,
     main='Clustering: Level 2', col=cl.1)
legend('topleft', legend=paste('Cluster', levels(cl.1)), fill=1:length(levels(cl.1)))

plot(adult.mca$ind$coord[,1:2], pch=19, cex=0.5,
     main='Clustering: Level 1', col=cl.2)
legend('topleft', legend=paste('Cluster', levels(cl.2)), fill=1:length(levels(cl.2)))


profile.1 <- FactoMineR::catdes(clust.1$data.clust, 12)

prof1.1 <- profile.1$category$`1`
prof1.2 <- profile.1$category$`2`
prof1.3 <- profile.1$category$`3`

profile.2 <- FactoMineR::catdes(clust.2$data.clust, 12)

prof2.1 <- profile.2$category$`1`
prof2.2 <- profile.2$category$`2`
prof2.3 <- profile.2$category$`3`
prof2.4 <- profile.2$category$`4`
prof2.5 <- profile.2$category$`5`
prof2.6 <- profile.2$category$`6`


cl.f <- cl.2
cl.f[cl.f == 3] <- 2
cl.f[cl.f == 4] <- 3
cl.f[cl.f == 6] <- 3
cl.f[cl.f == 5] <- 4
droplevels(cl.f)

plot(adult.mca$ind$coord[,1:2], pch=19, cex=0.5,
     main='Clustering: Interpretation', col=cl.f1)
legend('topleft',
       legend=c(
         'Low-Class',
         'Middle-Class',
         'High-Class',
         'Asian'),
       fill=1:4)

plot(adult.mca$ind$coord[,1:2], pch=19, cex=0.5,
     main='Clustering: Interpretation', col=cl.2)
legend('topleft',
       legend=c(
         'Low-Class',
         'Middle-Class / Low-Qualified',
         'Middle-Class / High-Qualified',
         'High-Class / Low-Qualified',
         'Asian',
         'High-Class / High-Qualified'),
       fill=1:5)
