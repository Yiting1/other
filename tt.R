#loadData.R
### Load Data ###
tmpFileName <- "Data.zip"
if(!file.exists(tmpFileName)) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL, tmpFileName)
  unzip(tmpFileName)
}


#plot1.R
library(plyr)

### Load Data ###
tmpFileName <- "Data.zip"
if(!file.exists(tmpFileName)) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL, tmpFileName)
  unzip(tmpFileName)
}

### Read Data ###
EMI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")

# plot(summarySCC_PM25$Emissions, summarySCC_PM25$year)
aggEMI <- aggregate(Emissions ~ year, EMI, sum)

barplot(aggEMI$Emissions/10^6, names.arg = aggEMI$year, 
        xlab = "Year", ylab = "Emissions")

### The answer is YES
dev.copy(png, filename="plot1.png", width=480, height=480)
dev.off ()


#plot2.R
### plot2.R ###
library(plyr)

### Load Data ###
tmpFileName <- "Data.zip"
if(!file.exists(tmpFileName)) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL, tmpFileName)
  unzip(tmpFileName)
}

### Read Data ###
EMI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")

balEMI <- subset(EMI, EMI$fips=="24510")

# plot(summarySCC_PM25$Emissions, summarySCC_PM25$year)
aggBalEMI <- aggregate(Emissions ~ year, balEMI, sum)

barplot(aggBalEMI$Emissions, names.arg = aggBalEMI$year, 
        xlab = "Year", ylab = "Emissions")

### The answer is YES
dev.copy(png, filename="plot2.png", width=480, height=480)
dev.off ()

#plot3.R
library(ggplot2)

### Load Data ###
tmpFileName <- "Data.zip"
if(!file.exists(tmpFileName)) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL, tmpFileName)
  unzip(tmpFileName)
}

### Read Data ###
EMI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")

balEMI <- subset(EMI, EMI$fips=="24510")

aggBalEMI <- aggregate(Emissions ~ year+type, balEMI, sum)

ggplot(data=aggBalEMI, aes(x=year, y=Emissions, group=type, color=type)) + geom_line() + geom_point( size=4, shape=21, fill="white") + xlab("Year") + ylab("Emissions (tons)") 

dev.copy(png, filename="plot3.png", width=480, height=480)
dev.off ()


#plot4.R
library(plyr)
library(ggplot2)

### Load Data ###
tmpFileName <- "Data.zip"
if(!file.exists(tmpFileName)) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL, tmpFileName)
  unzip(tmpFileName)
}

### Read Data ###
EMI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")

coalSCC <- SCC[grepl('Coal', SCC$Short.Name, fixed=T), ]
coalEMI <- merge(x=EMI, y=coalSCC, by="SCC")

aggCoalEMI <- aggregate(Emissions ~ year, coalEMI, sum)

ggplot(data = aggCoalEMI, aes(x=year, y=Emissions)) + geom_line() + geom_text(aes(label=Emissions), vjust=1)+ geom_point( size=4, shape=21, fill="white") + xlab("Year") + ylab("Emissions (thousands of tons)" ) + ggtitle("Total United States PM2.5 Coal Emissions")

### The answer is YES
dev.copy(png, filename="plot4.png", width=480, height=480)
dev.off ()


#plot5.R
library(plyr)
library(ggplot2)

### Load Data ###
tmpFileName <- "Data.zip"
if(!file.exists(tmpFileName)) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL, tmpFileName)
  unzip(tmpFileName)
}

### Read Data ###
EMI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")

motorSCC <- SCC[grepl('vehicle', SCC$SCC.Level.Two, ignore.case=T), ]
motorEMI <- merge(x=EMI, y=motorSCC, by="SCC")
motorBalEMI <- subset(motorEMI, motorEMI$fips=="24510")

aggMotorBalEMI <- aggregate(Emissions ~ year, motorBalEMI, sum)

ggplot(data = aggMotorBalEMI, aes(x=year, y=Emissions)) + geom_line() + geom_text(aes(label=Emissions), vjust=-2)+ geom_point( size=4, shape=21, fill="white") + xlab("Year") + ylab("Emissions (thousands of tons)" ) + ggtitle("Baltimore Emissions from Motorcycle")

dev.copy(png, filename="plot5.png", width=480, height=480)
dev.off ()


#plot6.R
library(plyr)
library(ggplot2)

### Load Data ###
tmpFileName <- "Data.zip"
if(!file.exists(tmpFileName)) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL, tmpFileName)
  unzip(tmpFileName)
}

### Read Data ###
EMI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")

motorSCC <- SCC[grepl('vehicle', SCC$SCC.Level.Two, ignore.case=T), ]
motorEMI <- merge(x=EMI, y=motorSCC, by="SCC")
motorBalEMI <- subset(motorEMI, motorEMI$fips=="24510" )
motorLAEMI <-  subset(motorEMI, motorEMI$fips=="06037")
motorBalEMI$city <- "Baltimore"
motorLAEMI$city <- "Los Angeles"
motorBalAndLAEMI <- rbind(motorBalEMI, motorLAEMI)

aggMotorBalAndLAEMI <- aggregate(Emissions ~ year+city, motorBalAndLAEMI, sum)

g <- ggplot(aggMotorBalAndLAEMI, aes(year, Emissions, color = city))
g + geom_line() + xlab("Year") + ylab(expression("Total PM2.5 Emissions")) + ggtitle("Total Emissions from motor vehicle in Baltimore and Los Angeles")

dev.copy(png, filename="plot6.png", width=480, height=480)
dev.off ()


#readData.R
### Read Data ###
EMI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")
