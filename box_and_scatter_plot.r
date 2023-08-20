# Read csv file
college <- read.csv("/Users/darkospy/Desktop/R Studio/college.csv", header=TRUE)

# If the college is private, change the second column value to private, otherwise, change it to public
college$Private[college$Private == "Yes"] <- c("Private")
college$Private[college$Private == "No"] <- c("Public")
# Rename the second variable in the college data set from “Private to “Type”. 
colnames(college)[2] <- c("Type")

# create parallel box plot by type of institution to investigate instructional spending
boxplot(college$Expend~college$Type,
        main="University Instructional Spending per Type of Institution", sub = "777 universities in 1995",
        xlab="Type of Institution", ylab="Instructional Spending",las=1, col="royalblue")

# create parallel box plot by type of institution to investigate instructional spending (reduced y axis)
boxplot((college$Expend/1000)~college$Type,
        main="University Instructional Spending per Type of Institution", sub = "777 universities in 1995",
        xlab="Type of Institution", ylab="Instructional Spending",las=1, col="royalblue")
# find the median of expend for the universities based on type
expend_type <- tapply (college$Expend, college$Type, FUN=median)
expend_type
# summary of the expend of colleges
summary(college$Expend)

# create parallel box plot by type of institution to investigate student-to-faculty ratio
boxplot(college$S.F.Ratio~college$Type,
        main="University SF Ratio per Type of Institution", sub = "777 universities in 1995",
        xlab="Type of Institution", ylab="SF Ration",las=1, col="royalblue")

# find the median of SF Ratio for the universities based on type
SFRaio_type <- tapply (college$S.F.Ratio, college$Type, FUN=median)
SFRaio_type
# summary of the SF Ratio of colleges
summary(college$S.F.Ratio)

# set the color template for scatter plot
color_type = c("blue", "red")[college$Type]
# create scatter plot of relationship between instructional spending and new students from the top 10% of their high school class
plot(college$Top10perc, college$Expend, col=color_type)
# legend for scatter plot of relationship between instructional spending and student-faculty ratio
legend("topright", col= c("blue", "red"), c("Private", "Public"), pch=1, inset=.04)
# create line of best fit for scatter plot
abline(lm(college$Expend~college$Top10perc))
# create a fancy regression table
summary(lm(college$Expend~college$Top10perc))
