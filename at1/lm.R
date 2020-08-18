# https://archive.ics.uci.edu/ml/datasets/Airfoil+Self-Noise 

# Data Set Information:
#
# The NASA data set comprises different size NACA 0012 airfoils at various wind
# tunnel speeds and angles of attack. The span of the airfoil and the observer
# position were the same in all of the experiments.
#
#
# Attribute Information:
#
# This problem has the following inputs: 
# 1. Frequency, in Hertzs.
# 2. Angle of # attack, in degrees. 
# 3. Chord length, in meters. 
# 4. Free-stream velocity, in meters per second. 
# 5. Suction side displacement thickness, in meters.
#
# The only output is: 
# 6. Scaled sound pressure level, in decibels.

df <- read.table(file='airfoil_self_noise.dat', sep ='\t')
colnames(df) <- c('freq', 'angle', 'chord_length', 'free_stream_velocity', 'suc_disp_thick', 'sound_db')
head(df)

pairs(df)

# https://www.researchgate.net/publication/282988800_Airfoil_Self_Noise_Prediction_Using_Linear_Regression_Approach