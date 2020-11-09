#top50contry <- read_csv("top50contry.csv")
#top50world <- top50contry[c(1:50),]
#top50world2 <- top50world[,c(2,7:16)]
#top50world2.eucdist <- dist(top50world2)
#top50world2.euccs <- cmdscale(top50world2.eucdist, k=10, eig=TRUE) #set k again later
#plot(1:50, top50world2.euccs$eig,, xlab="Eigenvalue number", ylab="Eigenvalue")
#results <- top50world2.euccs[[1]]
#plot(results[,1],results[,2])



#> top50world[which(top50world$`top genre`== "dance pop"),]
#> top50world[which(top50world$`year`== 2019),][,1]


top50contry <- read_csv("top50contry.csv")
top50world <- top50contry[c(1:50),]
top50world2 <- top50world[,c(2,7:15)]
top50world2.eucdist <- dist(top50world2, method = "euclidean")

top50world2.euccs <- isoMDS(top50world2.eucdist)

top50world2.euccs <- cmdscale(top50world2.eucdist, k=10, eig=TRUE) #set k again later
plot(1:50, top50world2.euccs$eig,, xlab="Eigenvalue number", ylab="Eigenvalue")
abline(h=0)
title(main = "Eigenvalues for top 50 songs in World using Canberra")

#results <- top50world2.euccs[[1]]
#plot(results[,1],results[,2])

alim <- c(-150, 250)
blim <- c(-70,90)


oldpar <- par(pty="s")
plot(top50world2.euccs$points[,1], top50world2.euccs$points[,2], type="n",
     xlim=alim, ylim=blim, xlab="1st reduced dimension", ylab="2nd reduced dimension")
#plot(top50world2.euccs$points[,1], top50world2.euccs$points[,2], type="n",
#     xlab="1st reduced dimension", ylab="2nd reduced dimension")
text(top50world2.euccs$points[,1], top50world2.euccs$points[,2], lab=dimnames(top50world2)[[1]])
title(main = "top 50 songs on Spotify in World (isoMDS)")
par(oldpar)


plot(1:50, pvarexp(top50world2.euccs$eig), ylim=c(0, 150),xlab="Eigenvalue number",ylab="Cumulative Sum/Sum of Eigenvalue")
title(main = "CumSum plot")

worldkmeans <- kmeans(x=top50world2.euccs$points, centers = 3, nstart = 100)
plot(top50world2.euccs$points[,1], top50world2.euccs$points[,2], xlab="1st dimension", ylab = "2nd dimension", type="n")
text(top50world2.euccs$points[,1], top50world2.euccs$points[,2],col=worldkmeans$cluster,lab=worldkmeans$cluster)
title(main="k means clustered plot for top 50 Chile")










#

country_plot <- function(country){
  
  countries <- unique(top50contry[["country"]])
  index <-  match(country,countries)
  i_s <- ((index-1)*50) +1
  
  
  top50contry <- read_csv("top50contry.csv")
  top50world <- top50contry[c(i_s:i_s+49),]
  top50world2 <- top50world[,c(2,7:16)]
  top50world2.eucdist <- dist(top50world2)
  top50world2.euccs <- cmdscale(top50world2.eucdist, k=2, eig=TRUE) #set k again later
  plot(1:50, top50world2.euccs$eig,, xlab="Eigenvalue number", ylab="Eigenvalue")
  results <- top50world2.euccs[[1]]
  plot(results[,1],results[,2])
}

