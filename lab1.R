# Set the path beforehand
# setwd("C:/Users/luil/Desktop/Spring 2023/Data Analytics")
# print(getwd())

# Read Excel sheet
# install.packages("readxl")
library("readxl")
EPI_xls <- read_excel("EPI_data2010.xls", sheet="EPI2010_onlyEPIcountries", na="NA")
View(EPI_xls)
str(EPI_xls)

# Code from lab1_data
EPI_data <- read.csv(file='2010EPI_data.csv', header=TRUE, skip=1, na.strings = "..")
str(EPI_data)
#or
#EPI_data <- read.xlsx(”<path>/2010EPI_data.xlsx")
# Note: replace default data frame name – cannot start with numbers!
View(EPI_data)
#
attach(EPI_data) 	# sets the ‘default’ object
# fix(EPI_data) 	# launches a simple data editor
EPI 			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array

# Code from lab1_summary
summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI);qqline(EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

# Do same exploration for two other variables in EPI_data
# DALY
DALY
summary(DALY)
fivenum(DALY)
stem(DALY)
hist(DALY)
hist(DALY, seq(0., 95., 2.0), prob=TRUE)
lines(density(DALY, na.rm=TRUE, bw=1.))
rug(DALY)

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(DALY);qqline(DALY)
x <- seq(0,95,2)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

# BIODIVERSITY
BIODIVERSITY
summary(BIODIVERSITY)
fivenum(BIODIVERSITY)
stem(BIODIVERSITY)
hist(BIODIVERSITY)
hist(BIODIVERSITY, seq(0., 100., 2.0), prob=TRUE)
lines(density(BIODIVERSITY, na.rm=TRUE, bw=1.))
rug(BIODIVERSITY)

plot(ecdf(BIODIVERSITY), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(BIODIVERSITY);qqline(BIODIVERSITY)
x <- seq(0,100,2)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

# Comparing distributions
boxplot(EPI,DALY)
qqplot(EPI,DALY)

boxplot(EPI,ENVHEALTH,ECOSYSTEM,DALY,AIR_H,WATER_H,AIR_E,WATER_E,BIODIVERSITY)
qqplot(EPI,ENVHEALTH)
qqplot(EPI,ECOSYSTEM)
qqplot(EPI,BIODIVERSITY)
qqplot(AIR_H,WATER_H)
qqplot(AIR_H,AIR_E)
qqplot(WATER_H,WATER_E)

# DISTRIBUTIONS
help(distributions)

# FILTERING
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30.,95.,1.0), prob=TRUE)
lines(density(Eland, na.rm=TRUE, bw=1.))
rug(Eland)
plot(ecdf(Eland), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(Eland);qqline(Eland)

# Almost all places with no surface water are deserts
SurfaceWater_Desert <- No_surface_water[!Desert]
DesertWater <- SurfaceWater_Desert[!is.na(SurfaceWater_Desert)]
fivenum((DesertWater))
hist(DesertWater)

# No deserts have high population density
DesertPopulation <- Desert[High_Population_Density]
DesertPop <- DesertPopulation[!is.na(DesertPopulation)]
fivenum(DesertPop)

# No areas with no surface water have high population density
WaterPopulation <- No_surface_water[High_Population_Density]
WaterPop <- WaterPopulation[!is.na(WaterPopulation)]
fivenum(WaterPop)

# How to filter on categories
unique(EPI_regions) # Find all possible EPI regions
isSouthAsia = EPI_regions=='South Asia'
EPI_South_Asia_NA <- EPI[isSouthAsia]
EPI_South_Asia <- EPI_South_Asia_NA[!is.na(EPI_South_Asia_NA)]
summary(EPI_South_Asia)

hist(EPI_South_Asia)
lines(density(EPI_South_Asia, na.rm=TRUE, bw=1.))
rug(EPI_South_Asia)
plot(ecdf(EPI_South_Asia), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_South_Asia);qqline(EPI_South_Asia)

unique(GEO_subregion)
isSouthAsia = GEO_subregion=='South Asia'
EPI_South_Asia_NA <- EPI[isSouthAsia]
EPI_South_Asia_GEO <- EPI_South_Asia_NA[!is.na(EPI_South_Asia_NA)]
summary(EPI_South_Asia_GEO)

boxplot(EPI_South_Asia, EPI_South_Asia_GEO)

#other data
GRUMP_data <- read.csv(file="GPW3_GRUMP_SummaryInformation_2010.csv")

