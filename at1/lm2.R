# https://archive.ics.uci.edu/ml/datasets/Energy+efficiency 

# 
# Data Set Information:
#   
#   We perform energy analysis using 12 different building shapes simulated in Ecotect. The buildings differ with respect to the glazing area, the glazing area distribution, and the orientation, amongst other parameters. We simulate various settings as functions of the afore-mentioned characteristics to obtain 768 building shapes. The dataset comprises 768 samples and 8 features, aiming to predict two real valued responses. It can also be used as a multi-class classification problem if the response is rounded to the nearest integer.
# 
# 
# Attribute Information:
#   
#   The dataset contains eight attributes (or features, denoted by X1...X8) and two responses (or outcomes, denoted by y1 and y2). The aim is to use the eight features to predict each of the two responses.
# 
# Specifically:
#   X1 Relative Compactness
# X2 Surface Area
# X3 Wall Area
# X4 Roof Area
# X5 Overall Height
# X6 Orientation
# X7 Glazing Area
# X8 Glazing Area Distribution
# y1 Heating Load
# y2 Cooling Load

# https://www.kaggle.com/elikplim/eergy-efficiency-dataset/notebooks


eng_Df <- readxl::read_xlsx(path='ENB2012_data.xlsx')
colnames(eng_Df) <- c('relative_compactness', 'surface_area', 'wall_area', 'roof_area', 'overall_height',
                'orientation', 'glazing_area', 'glazing_area_distribution', 'heating_load', 'cooling_load')
head(as.data.frame(eng_Df))


plot(eng_Df)


var(eng_Df$relative_compactness, eng_Df$surface_area, na.rm=TRUE)
round(cor(eng_Df),2)
cov2cor(as.matrix(eng_Df))
str(eng_Df)

sum(is.na(eng_Df))
library(corrplot)
library(Hmisc)

corrplot(as.matrix(eng_Df))
