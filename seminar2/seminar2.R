cities <- read.csv(file = "seminar2/cities15000AU.csv")

cities2 <- subset(cities,select=c("latitude", "longitude", "population"))


rownames(cities2) <- cities$name
head(cities2, n=4)


cities2[100:109,]


# find maximum norm to find distance
round(dist(cities2[100:109,],method = "maximum"))

# find the manhattan norm to find distance 
round(dist(cities2[100:109,],method = "manhattan"))


# find the minkowski norm to find distance for p=1
round(dist(cities2[100:109,],method = "minkowski", p=1))

## manhattan and minowski of p=1 produce the same results 





# find the euclidean norm to find distance
round(dist(cities2[100:109,],method = "euclidean"))

# find the minkowski norm to find distance for p=2
round(dist(cities2[100:109,],method = "minkowski", p=2))




###
###
#### scalle the data first  ####

scaledcities <- scale(cities2)
scaledcities[100:109,]

(dist(scaledcities[100:109,],method = "euclidean"))
(dist(scaledcities[100:109,],method = "minkowski", p=2))


