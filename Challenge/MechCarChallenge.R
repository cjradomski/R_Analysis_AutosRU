# Import MechaCar dataset
mechaCar_data <- read.csv('MechaCar_mpg.csv')
head(mechaCar_data)

# Create a matrix of the numeric columns from the mechaCar data and generate correlation matrix
used_matrix <- as.matrix(mechaCar_data[,c("vehicle.length", "vehicle.weight", "spoiler.angle", "ground.clearance", "mpg")])
cor(used_matrix)
# It appears vehicle.length has the strongest correllation with mpg (0.61) and the other numerical values do not (below 0.5)

# Create a multiple linear regression model to predict mpg
lm(mpg ~  vehicle.length + vehicle.weight + spoiler.angle + ground.clearance, data=mechaCar_data)

# Generate summary of multiple linear regression model
summary(lm(mpg ~  vehicle.length + vehicle.weight + spoiler.angle + ground.clearance, data=mechaCar_data))
# R squared value = 0.7 indicating the model can predict correctly roughly 70% of the time, 
# p-value is well below significance level of 0.05 so the null hypothesis is rejected and the slope(s) is not zero



# Import Suspension_coil dataset
suspensionCoil_data <- read.csv('Suspension_Coil.csv')
head(suspensionCoil_data)

# Create summary table of suspension coil data
summarize_suspensionCoil <- suspensionCoil_data %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variance_PSI=var(PSI), SD_PSI=sd(PSI))  
summarize_suspensionCoil

# Perform t-test to determine if the suspension coil's PSI data is statistically different from the mean population results of 1,500
t.test(suspensionCoil_data$PSI, mu=1500)
# the p-value is above significance lvl of 0.05 therefore we cannot reject the null hypothesis and the two means are statisitcally similar
