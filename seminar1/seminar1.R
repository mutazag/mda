df <- read.csv("C:/git/mda/seminar1/boston.csv")

round(cor(df),2)


pairs(df)


stars(df)


wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
                   sep = ",")

colnames(wine) <- c("Cult", "Alc", "MalAcid", "Ash", "AshAlk", "Mag", "TotPhen",
                    "Flav", "NonFlav", "Proant", "Color", "Hue", "OD280OD315", "Proline")

summary(wine)
