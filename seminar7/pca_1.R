library(tidyverse)

### SESSION 7
# PCA of bank notes
notes <- read.csv('./Notes.csv')

notes %>% head

# Add column with r for real; c for counterfeit
notes <- notes %>%
  mutate(type = ifelse(Status == 'Real', 'r', 'c'))


# select the x variables only and calc the variance covariance matrix 
notes_variables <- notes %>% select (-c(Status, type))
notes_cov <- notes_variables  %>% cov


# You can cross reference the eigenvalues with the pca results
# Very small values in each eigenvector will be dropped in the pca loadings 
eigen_things <- eigen(notes_cov)

# this will be the reported as standard deviation in the PCA summary 
sqrt(eigen_things$values)


# Compute principal components
notes_pc <- princomp(notes_variables)
summary(notes_pc, loadings = TRUE) # loadings = TRUE gives you the weightings by variable for each PC
# Look for the elbow in the screeplot
screeplot(notes_pc, npcs = 7, type = 'lines')
# Biplot - by default puts pc1 on x and pc2 on y
biplot(notes_pc, xlabs = notes$type, cex = c(0.8, 1.4), 
       expand = 0.9, col = c("blue", "black"))
# Biplot with chosen pcs by declaring choices = 
biplot(notes_pc, choices = 2:3, xlabs = notes$type, cex = c(0.8, 1.4), 
       expand = 0.9, col = c("blue", "black"))
biplot(notes_pc, choices = c(1,3), xlabs = notes$type, cex = c(0.8, 1.4), 
       expand = 0.9, col = c("blue", "black"))
