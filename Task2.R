setwd("C:\Users\bharathkumar\Desktop\Predictive analysis")

# Read the data set
wb <- read.csv(file = "Vic 2013 LGA Profiles NoPc1.csv", header = TRUE)
View(wb)

##### Make visual inspection of data

# Let's look at the data
lga <- wb$House.21
Medical <- wb$Medical.3
Medical_1 <- wb$Medical.13
Medical_2 <- wb$Medical.19
wellbeing <- wb$WellBeing.5
Health <- wb$Health.15

# Creating a table for the selected data
selected.wb <- data.frame(lga,Medical,Medical_1,Medical_2,Health)

pairs(selected.wb[-1], col="blue")

# A more comprehensive look at the same data
install.packages("psych", dependencies = TRUE)
library("psych")
pairs.panels(selected.wb[-1], col="red")



# Plot with categorical variable as conditioning var
# Explain the panel and how it relates to variables
coplot(Health ~ Medical | Medical_1, pch=19, data=selected.wb, col="blue")


# Add regression lines to the plot
panel.lm = function(x, y, ...) {
  tmp <- lm(y ~ x, na.action = na.omit)
  abline(tmp, col="red")
  points(x, y, ...) 
}

# Now plot with regression lines
coplot(Health ~ Medical | Medical_1, pch=19, data=selected.wb, col="blue",
       panel = panel.lm)
coplot(Health ~ Medical | Medical_2 + Medical_1, pch=19, data=selected.wb, col="blue",
       panel = panel.lm )



##### Develop a model


# Let us look at the vars correlations
cor(selected.wb[-1])


model <- lm(Health ~ Medical + Medical_1 + Medical_2, data=selected.wb)
summary(model)


model <- lm(Health ~ Medical * Medical_1 * Medical_2, data=selected.wb)
summary(model)

# Remove the var with the largest P value
model <- lm(Health ~ Medical * Medical_1, data=selected.wb)
summary(model)
plot(model)



# Define level of reliance
summary(selected.wb$Medical)
range(Medical_1)
MedicalMin = 0.02200
Medical1Q = 0.03950
MedicalMean = 0.04871
Medical3Q = 0.05850
MedicalMax = 0.08500
Medical_1LowRange = 0 : 79

# Prediction
PredMedicalMin = predict(model, data.frame(Medical=MedicalMin, Medical_1=Medical_1LowRange))
PredMedical1Q = predict(model, data.frame(Medical=Medical1Q, Medical_1=Medical_1LowRange))
PredMedicalMean = predict(model, data.frame(Medical=MedicalMean, Medical_1=Medical_1LowRange))
PredMedical3Q = predict(model, data.frame(Medical=Medical3Q, Medical_1=Medical_1LowRange))
PredMedicalMax = predict(model, data.frame(Medical=MedicalMax, Medical_1=Medical_1LowRange))

# Plot it all and wonder
plot(Health ~ Medical, data=selected.wb, xlim=c(min(Medical_1LowRange), max(Medical_1LowRange)), 
     xlab = "Medical_1", ylab = "Health",
     main = "Influence of Medical and Medical_1 on Health",
     sub = "Multiple regression",
     col="gray")
lines(Medical_1LowRange, PredMedicalMin, col="blue")
lines(Medical_1LowRange, PredMedical1Q, col="green")
lines(Medical_1LowRange, PredMedicalMean, col="yellow")
lines(Medical_1LowRange, PredMedical3Q, col="darkorange")
lines(Medical_1LowRange, PredMedicalMax, col="red")



# Consider glm and gls, especially for logistic prediction


install.packages("rgl", dependencies = TRUE)
library(rgl)

install.packages("car", dependencies = TRUE)
library("car")

# Let us check influence of pairs of factors on sales
scatter3d(x=selected.wb$Medical, z=selected.wb$Medical_2, y=selected.wb$Health)
scatter3d(x=selected.wb$Medical, z=selected.wb$Medical_2, y=selected.wb$Health)
scatter3d(x=selected.wb$Medical, z=selected.wb$Medical_1, y=selected.wb$Health)

# Can we differentiate regression depending on the volume of sales
Health.class <- ifelse(selected.wb$Health < 9, "Low", ifelse(selected.wb$Health < 18, "Medium", "High"))
scatter3d(Health ~ Medical * Medical_1, data=selected.wb, groups=factor(Health.class))


# Save the last plot to a JPG file
rgl.snapshot(filename = "last-plot.jpg")
