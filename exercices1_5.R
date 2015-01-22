#exo1
x <- c(5.4, 6.1, 6.2, NA, 6.2, 5.6, 19.0, 6.3)
x[4] <- mean(c(x[1:3],x[5:6],x[8]))
x[7] <- x[4]

#exo2
tx=factor(rep(c("std","std","std","new","new","new"),length.out=60),levels=c("std","new"))
tx=factor(tx,levels=c("std","new"),labels=c("old","new"))
tx=factor(tx,levels=c("old","new"),labels=c("new","old"))

#exo3
data(birthwt, package="MASS")

mean(birthwt$ht)

mean(subset(birthwt$bwt,birthwt$smoke==1,birthwt$ht==0))

subset(birthwt$bwt,birthwt$lwt<quantile(birthwt$lwt)[2])

sort(subset(birthwt$bwt,birthwt$lwt<quantile(birthwt$lwt)[2]))[1:5]

library(lattice)
birthwt$ptl_binary=birthwt$ptl
for (i in 1:nrow(birthwt)) {
  if (birthwt$ptl[i]==0){
    birthwt$ptl_binary[i]=0
  }
  if (birthwt$ptl[i]>0){
    birthwt$ptl_binary[i]=1
  }
}
birthwt$ptl_binary=factor(birthwt$ptl_binary,labels=c("no previous premature labor","at least one"))
histogram(~bwt | ptl_binary, data = birthwt, type = "percent")

boxplot(birthwt$age)
boxplot(birthwt$lwt)
boxplot(birthwt$ptl)
boxplot(birthwt$ftv)
boxplot(birthwt$bwt)

#exo4
d <- data.frame(height = rnorm(40, 170, 10), class = sample(LETTERS[1:2], 40, rep = TRUE))
d$height[sample(1:40, 1)] <- 220
d$class[d$height == max(d$height)]

#exo5
setwd("Etudes\\ENS\\M2\\AS\\AS_Cogmaster\\R\\r-cogstats\\data")
lungcancer = read.table('lungcancer.txt',header=T)
lungcancer$age=as.numeric(lungcancer$age)
lungcancer$cens=as.factor(lungcancer$cens)
summary(lungcancer)
hist(lungcancer$time)
hist(lungcancer$age)
for (i in 1:nrow(lungcancer)) {
  if (lungcancer$vital.capac[i]=="low "){
    lungcancer$vital.capac[i]="low"
  }
}

lungcancer$vital.capac<- factor(lungcancer$vital.capac,exclude=NULL)

#erreur probable dans la colonne 'cens', où il y a une seule valeur '2'
#erreur dans la colonne 'time' : certaines valeurs sont négatives, ou non numériques