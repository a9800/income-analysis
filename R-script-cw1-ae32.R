# Importing the csv as a dataframe
df = read.csv("/home/abdelrahman/Desktop/UNI/HW/Y4/F20SA/CW/income.csv",header=TRUE)

# Getting the incomes column of the dataframe
incomes = df$x
#install.packages("Pareto")
library(Pareto)

# Work out the mean, sd, variance, and other metrics of the data
mean = mean(incomes)
summary = summary(incomes)
sd = sd(incomes)
variance = var(incomes)

# print metrics
print(summary)
cat("Standard Deviation:",sd)
cat("Variance:",variance)

# Create histograms of incomes
hist(incomes, xlab='Incomes, £')

# initialize logsum to 0
logsum = 0

# get the sum of log of each income int the dataframe
for (i in 1:length(incomes)){
  logsum = logsum + log(incomes[i])
}

# Calculate Xm, alpha
xm = 80000
n = length(incomes)
a = n/(logsum-(n*log(xm)))
print(a)

# Simulate 1000 Pareto distributions of 1000 samples each using parameters 
# derived above
paretoDistMeans = numeric(1000)
for (i in 1:1000){
  pareto = rPareto(1000,xm,a)
  paretoDistMeans[i] = mean(pareto)
}

# Plot histogram of Pareto Simulation Means
h <- hist(paretoDistMeans,xlab="Pareto Distribution Mean", 
     main="Histogram of Pareto Distribution Means", breaks =80)

xfit <- seq(min(paretoDistMeans), max(paretoDistMeans), length = 40)
yfit <- dnorm(xfit, mean = mean(paretoDistMeans), sd = sd(paretoDistMeans))
yfit <- yfit * diff(h$mids[1:2]) * length(paretoDistMeans)
lines(xfit, yfit, col = "black", lwd = 2)

# Get metrics of Pareto Simulation Means
summary(paretoDistMeans)
sd(paretoDistMeans)
var(paretoDistMeans)

# Using the approximate normal distribution of Y' to calculate probability
# P[Y'<y] where y is the mean of incomes in 2020
pnorm(mean, mean(paretoDistMeans),sd(paretoDistMeans))

# Calculating probability using lower alpha interval
alower  = a - (1.96*sqrt(1/(n/a^2)))
print(alower)
alParetoDistMeans = numeric(1000)
for (i in 1:1000){
  pareto = rPareto(1000,xm,alower)
  alParetoDistMeans[i] = mean(pareto)
}

pnorm(mean, mean(alParetoDistMeans),sd(alParetoDistMeans))

# Calculating probability using upper alpha interval
ahigher = a + (1.96*sqrt(1/(n/a^2)))
print(ahigher)
ahParetoDistMeans = numeric(1000)
for (i in 1:1000){
  pareto = rPareto(1000,xm,ahigher)
  ahParetoDistMeans[i] = mean(pareto)
}

pnorm(mean, mean(ahParetoDistMeans),sd(ahParetoDistMeans))

