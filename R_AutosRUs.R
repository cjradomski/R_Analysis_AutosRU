# Visualize the distribution of driven miles for the used vehicle data set
population_table <- read.csv('used_car_data.csv', check.names = F, stringsAsFactors = F)
plt <- ggplot(population_table, aes(x=log10(Miles_Driven))) #Use log10 to make mileage data more normal since it is right skewed
plt + geom_density() 

# Create a sample dataset
sample_table <- population_table %>% sample_n(50) #randomly select 50 data points
plt <- ggplot(sample_table, aes(x=log10(Miles_Driven)))
plt + geom_density()

# Perform t-test on Miles driven in sample_table
t.test(log10(sample_table$Miles_Driven), mu=mean(log10(population_table$Miles_Driven))) 
# the p-value is above significance lvl of 0.05 therefore we cannot reject the null hypothesis and the two means are statisitcally similar


# Generate two samples 
sample_table <- population_table %>% sample_n(50)
sample_table2 <- population_table %>% sample_n(50)

# Perform two sample t.test on Miles driven in sample_table and sample_table2
t.test(log10(sample_table$Miles_Driven), log10(sample_table2$Miles_Driven))
# the p-value is above significance lvl of 0.05 therefore we cannot reject the null hypothesis and the two means are statisitcally similar

# Import modified mpg dataset and generate two data samples
mpg_data <- read.csv('mpg_modified.csv')
mpg_1999 <- mpg_data %>% filter(year==1999)
mpg_2008 <- mpg_data %>% filter(year==2008)

# Perform paired t-test to determine if there is a statistical difference in overall highwya fuel efficiency in vehicles made between 1999 vs 2008
t.test(mpg_1999$hwy, mpg_2008$hwy, paired = T) 
# the p-value is above significance lvl of 0.05 therefore we cannot reject the null hypothesis and the two means are statisitcally similar

# Clean mtcars *built-in* dataset to prepare for ANOVA
mtcars_filt <- mtcars[,c("hp", "cyl")]
mtcars_filt$cyl <- factor(mtcars_filt$cyl) #convert numeric cyl column to factor

# Perform ANOVa test to determine if there is any statistical difference in the horsepower of a vehicel based on its engine type (cyl)
summary(aov(hp ~ cyl, data=mtcars_filt))
# the p-value is lower than a significance level of 0.05 and therefore the null hypothesis is rejected and there is a significant difference in horsepower between at least one engine type and the others

# Plot horsepower and quarter-mile race time in the mtcars dataset using geom_point()
plt <- ggplot(mtcars, aes(x=hp, y=qsec))
plt + geom_point()

# Calculate the correlation coefficeint between horsepower and quarter-mile race time
cor(mtcars$hp, mtcars$qsec)
# -0.7 indicates a strong negative correlation


# Read-in used cars dataset
used_cars <- read.csv('used_car_data.csv', stringsAsFactors = F)
head(used_cars)

# Plot vehicle miles driven and selling price
plt <- ggplot(used_cars, aes(x=Miles_Driven, y=Selling_Price))
plt + geom_point()

# Calculate the correlation coefficient between vehicle miles driven and selling price
cor(used_cars$Miles_Driven, used_cars$Selling_Price)
# 0.02 indicates a negligible correlation

# Create a matrix of the numeric columns from the used cars data frame and generate correlation matrix
used_matrix <- as.matrix(used_cars[,c("Selling_Price", "Present_Price", "Miles_Driven")])
cor(used_matrix)


# Create a linear regression model to test whether or not quarter mile race time can be predicted from horsepower in the mtcars dataset
summary(lm(qsec ~hp, mtcars))
# r squared value of 0.5 indicates that roughly 50% of predictions are correct, p-value of 5.7e-06 indicates a p-value much smaller than 0.05%, so null hypothesis is rejected, which means slope is not zero

# Visualize the fitted line from linear egression model with dataset
model <- lm(qsec ~ hp, mtcars)
yvals <- model$coefficients['hp']*mtcars$hp +
  model$coefficients['(Intercept)']

plt <- ggplot(mtcars, aes(x=hp, y=qsec))
plt + geom_point() + geom_line(aes(y=yvals), color = "red")


# Create a multiple linear regression model to predict quarter-mile-time
lm(qsec ~ mpg + disp +drat + wt + hp, data=mtcars)

# Generate summary of multiple linear regression model
summary(lm(qsec ~ mpg + disp +drat + wt + hp, data=mtcars))


# Build a contingency table for vehicle class across 1999 and 2008 in mpg dataset
tbl <- table(mpg$class, mpg$year)

# Perform Chi Squared Test on contingency table
chisq.test(tbl)
# with p-value of 0.98 > 0.05, the null hypothesis is rejected. 
# There is no difference in distribution of vehicle class across 1999 and 2008


