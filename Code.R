#Load Necessary Packages 
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(ggmap)
library(tigris)
library(tidycensus)
library(sf)
library(viridis)
library(forcats)


# Data Preparation
# Removed setwd() here

dallas_data <- read.csv("dallas_crime.csv")
denver_data <- read.csv("denver_data.csv")
denver_income <- read.csv("Income_Poverty_(Census_Tracts).csv")
dallas_shp <- st_read("STREETS.shp")
zip_shape <- st_read("DallasZipCodes_2018.shp")
dallas_poverty <- read.csv("indicator_data_download_20230420.csv")

names(dallas_data) <- c("IncidentNum", "ArrestYear", "ArrestDate", "ArrestTime", "ArrestAddress", "ArrestZipCode", "Latitude", "Longitude", "ArrestCity", "ArrestState", "ArrestDay", "ArrestLocation", "ArrestWeapon", "ArresteeAge", "ArresteeRace", "ArresteeSex", "DrugRelated", "DrugType")

names(denver_data) <- c("IncidentId", "OffenseId", "OffenseCode", "OffenseCodeExtension", "OffenseTypeId", "DrugRelated", "FirstOccuranceDate", "LastOccuranceDate", "ReportedDate", "IncidentAddress", "Geo_x", "Geo_y", "Geo_lon", "Geo_lat", "DistrictId", "PrecinctId", "NeighborhoodId", "IsCrime", "IsTraffic", "VictimCount")

# Since Denver is from 2018-2020
#dallas_data <- subset(dallas_data, dallas_data$ArrestYear >= 2018)

denver_data <- denver_data[, c(- 1, - 2, - 3, - 4, -7, -8)]
dallas_data <- dallas_data[, c(-1, -4, -12, -16)]

# Question 1
dallas_data <- subset(dallas_data, dallas_data$ArrestYear <= 2021)
ggplot(data = dallas_data, aes(x = ArrestYear, color = "magenta")) + 
  geom_line(stat = "count") + labs(title = "Dallas Crime Over Time") +
  theme(legend.position = "none")

denver_data$ReportedDateStandard <- as.Date(denver_data$ReportedDate, format = "%m/%d/%Y")
denver_2017 <- subset(denver_data, ReportedDateStandard >= as.Date("2017-01-01") & ReportedDateStandard <= as.Date("2017-12-31"))
denver_2017$ArrestYear <- format(denver_2017$ReportedDateStandard, "%Y")

denver_data$ReportedDateStandard <- as.Date(denver_data$ReportedDate, format = "%m/%d/%Y")
denver_2018 <- subset(denver_data, ReportedDateStandard >= as.Date("2018-01-01") & ReportedDateStandard <= as.Date("2018-12-31"))
denver_2018$ArrestYear <- format(denver_2018$ReportedDateStandard, "%Y")

denver_data$ReportedDateStandard <- as.Date(denver_data$ReportedDate, format = "%m/%d/%Y")
denver_2019 <- subset(denver_data, ReportedDateStandard >= as.Date("2019-01-01") & ReportedDateStandard <= as.Date("2019-12-31"))
denver_2019$ArrestYear <- format(denver_2019$ReportedDateStandard, "%Y")

denver_data$ReportedDateStandard <- as.Date(denver_data$ReportedDate, format = "%m/%d/%Y")
denver_2020 <- subset(denver_data, ReportedDateStandard >= as.Date("2020-01-01") & ReportedDateStandard <= as.Date("2020-12-31"))
denver_2020$ArrestYear <- format(denver_2020$ReportedDateStandard, "%Y")

denver_data$ReportedDateStandard <- as.Date(denver_data$ReportedDate, format = "%m/%d/%Y")
denver_2021 <- subset(denver_data, ReportedDateStandard >= as.Date("2021-01-01") & ReportedDateStandard <= as.Date("2021-12-31"))
denver_2021$ArrestYear <- format(denver_2021$ReportedDateStandard, "%Y")

denver_data$ReportedDateStandard <- as.Date(denver_data$ReportedDate, format = "%m/%d/%Y")
denver_2022 <- subset(denver_data, ReportedDateStandard >= as.Date("2022-01-01") & ReportedDateStandard <= as.Date("2022-12-31"))
denver_2022$ArrestYear <- format(denver_2022$ReportedDateStandard, "%Y")

denver_data <- rbind(denver_2017, denver_2018, denver_2019,denver_2020,denver_2021,denver_2022)

ggplot(data = denver_data, aes(x = ArrestYear, group = IsCrime, color = "lavender")) + geom_line(stat = "count") + labs(title = "Denver Crime Over Time") +
  theme(legend.position = "none")

dallas_data_drugs <- subset(dallas_data, dallas_data$DrugRelated == "Yes")
ggplot(data = dallas_data_drugs, aes(x = ArrestYear, color = "purple")) + geom_line(stat = "count") + labs(title = "Dallas Drug Crime Over Time") +
  theme(legend.position = "none")

denver_data_drugs <- subset(denver_data, denver_data$DrugRelated == "drug-alcohol")
ggplot(data = denver_data_drugs, aes(x = ArrestYear, group = IsCrime, color = "pink")) + geom_line(stat = "count") + labs(title = "Denver Drug Crime Over Time") +
  theme(legend.position = "none")

# Question 2
# Filter to find necessary info
denver_drugs <- filter(denver_data, DrugRelated == "drug-alcohol")
dallas_drugs <- filter(dallas_data, DrugRelated == "Yes")
denver_drugs$ReportedDate <- format(as.Date(denver_drugs$ReportedDate, format =  "%m/%d/%Y"), "%Y")
denver_drugs <- denver_drugs[which(denver_drugs$OffenseTypeId != "liquor-possession"),]
denver_drugs <- denver_drugs[which(denver_drugs$OffenseTypeId != "liquor-sell"),]
dallas_data[dallas_data == ""] <- NA
denver_drugs$DrugRelated <- str_replace(denver_drugs$DrugRelated, "drug-alcohol", "Yes")


# Removed values not connect to drugs

# Changed Values so it would match up with dallas_data
# Joined both data sets
City = c("Denver")
denver_drugs <- cbind(denver_drugs, City)
City = c("Dallas")

dallas_drugs <- cbind(dallas_drugs, City)
denver_2019 <- denver_drugs[which(denver_drugs$ReportedDate %in% 2019),]
den_total_2019 <- as.data.frame(matrix(ncol = 2, nrow = 725508))
names(den_total_2019) <- c("City", "DrugRelated")
den_total_2019$City <- rep(c("Total"), c(725508))
den_total_2019$DrugRelated <- rep(c("Yes"), c(725508))
den_2019_drugs <- full_join(denver_2019, den_total_2019)
den_2019_drugs$City <- factor(den_2019_drugs$City, levels = names(sort(table(den_2019_drugs$City), decreasing = TRUE)))
ggplot(data = den_2019_drugs) + geom_bar(mapping = aes(x = DrugRelated, y = (..count..)/sum(..count..), fill = City), position = "stack") +
  scale_y_continuous(labels = percent) +
  labs(title = "Denver 2019 Drug/Narcotic Relation", x = "Drug Related", fill = "Percentage") +
  theme_minimal()


denver_2020 <- denver_drugs[which(denver_drugs$ReportedDate %in% 2020),]
den_total_2020 <- as.data.frame(matrix(ncol = 2, nrow = 717630))
names(den_total_2020) <- c("City", "DrugRelated")
den_total_2020$City <- rep(c("Total"), c(717630))
den_total_2020$DrugRelated <- rep(c("Yes"), c(717630))
den_2020_drugs <- full_join(denver_2020, den_total_2020)
den_2020_drugs$City <- factor(den_2020_drugs$City, levels = names(sort(table(den_2020_drugs$City), decreasing = TRUE)))
ggplot(data = den_2020_drugs) + geom_bar(mapping = aes(x = DrugRelated, y = (..count..)/sum(..count..), fill = City), position = "stack") +
  scale_y_continuous(labels = percent) +
  labs(title = "Denver 2020 Drug/Narcotic Relation", x = "Drug Related", fill = "Percentage") +
  theme_minimal()

dallas_2019 <- dallas_drugs[which(dallas_drugs$ArrestYear %in% 2019),]
dal_total_2019 <- as.data.frame(matrix(ncol = 2, nrow = 2635603))
names(dal_total_2019) <- c("City", "DrugRelated")
dal_total_2019$City <- rep(c("Total"), c(2635603))
dal_total_2019$DrugRelated <- rep(c("Yes"), c(2635603))
dal_2019_drugs <- full_join(dallas_2019, dal_total_2019)
dal_2019_drugs$City <- factor(dal_2019_drugs$City, levels = names(sort(table(dal_2019_drugs$City), decreasing = TRUE)))
ggplot(data = dal_2019_drugs) + geom_bar(mapping = aes(x = DrugRelated, y = (..count..)/sum(..count..), fill = City), position = "stack") +
  scale_y_continuous(labels = percent) +
  labs(title = "Dallas 2019 Drug/Narcotic Relation", x = "Drug Related", fill = "Percentage") +
  theme_minimal()

dallas_2020 <- dallas_drugs[which(dallas_drugs$ArrestYear %in% 2020),]
dal_total_2020 <- as.data.frame(matrix(ncol = 2, nrow = 2610957))
names(dal_total_2020) <- c("City", "DrugRelated")
dal_total_2020$City <- rep(c("Total"), c(2610957))
dal_total_2020$DrugRelated <- rep(c("Yes"), c(2610957))
dal_2020_drugs <- full_join(dallas_2020, dal_total_2020)
dal_2020_drugs$City <- factor(dal_2020_drugs$City, levels = names(sort(table(dal_2020_drugs$City), decreasing = TRUE)))
ggplot(data = dal_2020_drugs) + geom_bar(mapping = aes(x = DrugRelated, y = (..count..)/sum(..count..), fill = City), position = "stack") +
  scale_y_continuous(labels = percent) +
  labs(title = "Dallas 2020 Drug/Narcotic Relation", x = "Drug Related", fill = "Percentage") +
  theme_minimal()

# Calculate % difference of population involved with drugs between Dallas and Denver in 2019
pop_diff_2019 <- round(((length(which(denver_2019$DrugRelated == "Yes")) / 725508) - (length(which(dallas_2019$DrugRelated == "Yes")) / 2635603))*100, 2)
cat("% difference in drug related crime between Denver and Dallas in 2019: ", pop_diff_2019, "%\n")

# Calculate % difference of population involved with drugs between Dallas and Denver in 2020
pop_diff_2020 <- round(((length(which(denver_2020$DrugRelated == "Yes")) / 717630) - (length(which(dallas_2020$DrugRelated == "Yes")) / 2610957))*100, 2)
cat("% difference in drug related crime between Denver and Dallas in 2020: ", pop_diff_2020, "%\n")

# Question 3
dallas_2018 <- subset(dallas_data, dallas_data$ArrestYear == "2018")
dallas_2019 <- subset(dallas_data, dallas_data$ArrestYear == "2019")
dallas_2020 <- subset(dallas_data, dallas_data$ArrestYear == "2020")
dallas_2018_drug <- subset(dallas_2018, dallas_2018$DrugRelated == "Yes")
dallas_2019_drug <- subset(dallas_2019, dallas_2019$DrugRelated == "Yes")
dallas_2020_drug <- subset(dallas_2020, dallas_2020$DrugRelated == "Yes")

denver_data$ReportedDateStandard <- as.Date(denver_data$ReportedDate, format = "%m/%d/%Y")
denver_2018 <- subset(denver_data, ReportedDateStandard >= as.Date("2018-01-01") & ReportedDateStandard <= as.Date("2018-12-31"))
denver_2018$ArrestYear <- format(denver_2018$ReportedDateStandard, "%Y")
denver_data$ReportedDateStandard <- as.Date(denver_data$ReportedDate, format = "%m/%d/%Y")
denver_2019 <- subset(denver_data, ReportedDateStandard >= as.Date("2019-01-01") & ReportedDateStandard <= as.Date("2019-12-31"))
denver_2019$ArrestYear <- format(denver_2019$ReportedDateStandard, "%Y")
denver_data$ReportedDateStandard <- as.Date(denver_data$ReportedDate, format = "%m/%d/%Y")
denver_2020 <- subset(denver_data, ReportedDateStandard >= as.Date("2020-01-01") & ReportedDateStandard <= as.Date("2020-12-31"))
denver_2020$ArrestYear <- format(denver_2020$ReportedDateStandard, "%Y")

denver_2018_drug <- subset(denver_2018, denver_2018$DrugRelated == "drug-alcohol")
denver_2019_drug <- subset(denver_2019, denver_2019$DrugRelated == "drug-alcohol")
denver_2020_drug <- subset(denver_2020, denver_2020$DrugRelated == "drug-alcohol")

# Marijuana between 2 cities
dal_marijuana_rows_2018 <- grep("Marijuana", dallas_2018_drug$DrugType)
dal_marijuana_rows_2019 <- grep("Marijuana", dallas_2019_drug$DrugType)
dal_marijuana_rows_2020 <- grep("Marijuana", dallas_2020_drug$DrugType)
den_marijuana_rows_2018 <- grep("marijuana", denver_2018_drug$OffenseTypeId)
den_marijuana_rows_2019 <- grep("marijuana", denver_2019_drug$OffenseTypeId)
den_marijuana_rows_2020 <- grep("marijuana", denver_2020_drug$OffenseTypeId)

marijuana_df <- data.frame(c(length(dal_marijuana_rows_2018), length(dal_marijuana_rows_2019), length(dal_marijuana_rows_2020), length(den_marijuana_rows_2018), length(den_marijuana_rows_2019), length(den_marijuana_rows_2020)), c("Dallas", "Dallas", "Dallas", "Denver", "Denver", "Denver"), c("2018", "2019", "2020", "2018", "2019", "2020"))
names(marijuana_df) <- c("Count", "City", "Year")

ggplot(data = marijuana_df, aes(x = Year, y = Count, fill = City)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Marijuana Counts For Dallas and Denver")

# Question 4
# Filter data to only be for 2018 up
dallas_data <- subset(dallas_data, dallas_data$ArrestYear >= 2018)


# Count the number of times an arrest occured at a zip code
dallas_zip <- dallas_data %>%
  count(dallas_data$ArrestZipCode)
names(dallas_zip) <- c("ZipCode", "Number")
# Merge the count of arrest to the shape file
merged_zip <- left_join(zip_shape, dallas_zip, by = "ZipCode")
# Create a ggplot that represents low amount of crime with blue and high amounts of crime with red corresponding with the zip code
ggplot(data = merged_zip) +
  geom_sf(aes(fill = Number)) +
  scale_fill_gradient(low = "blue", high = "red") +
  ggtitle("Number of Crimes in Dallas")
# Count the number of drug related crimes in dallas and grouped each based on zip code
dallas_zip_drugs <- dallas_data %>%
  group_by(dallas_data$ArrestZipCode, dallas_data$DrugRelated) %>%
  count(dallas_data$DrugRelated)
# Removed from the data frame rows that had “No” and “Unknown” in drug related cases
indices <- which(dallas_zip_drugs$`dallas_data$DrugRelated` != "No")
dallas_zip_drugs <- dallas_zip_drugs[indices, ]
indices <- which(dallas_zip_drugs$`dallas_data$DrugRelated` != "Unknown")
dallas_zip_drugs <- dallas_zip_drugs[indices, ]
names(dallas_zip_drugs) <- c("ZipCode", "DrugRelated","Number")
# Merge the count of drug related cases with the shapefile
merged_zip <- left_join(zip_shape, dallas_zip_drugs, by = "ZipCode")
# Mapped the areas where drug related crimes occurred with the the corresponding zip code
ggplot(data = merged_zip) +
  geom_sf(aes(fill = Number)) +
  scale_fill_gradient(low = "blue", high = "red") +
  ggtitle("Number of Drug Related Crimes in Dallas")


denver_drugs <- filter(denver_data, DrugRelated == "drug-alcohol")
denver_drugs <- denver_drugs[which(denver_drugs$OffenseTypeId != "liquor-possession"),]
denver_drugs <- denver_drugs[which(denver_drugs$OffenseTypeId != "liquor-sell"),]
denver_drugs <- denver_drugs[which(denver_drugs$OffenseTypeId != "liquor-manufacturing"),]
denver_drugs <- denver_drugs[which(denver_drugs$OffenseTypeId != "liquor-other-viol"),]
denver_drugs$ReportedDate <- format(as.Date(denver_drugs$ReportedDate, format =  "%m/%d/%Y"), "%Y")
denver_drugs <- subset(denver_drugs, denver_drugs$ReportedDate >= 2018)

denver_map <- denver_drugs %>% 
  filter(
    between(denver_drugs$Geo_lon, -105.3809, -104.3275),
    between(denver_drugs$`Geo_lat`, 39.5718, 39.9571)
  )
qmplot(Geo_lon, `Geo_lat`, data = denver_map, maptype = "toner-lite", color = factor(OffenseTypeId), main = "Denver Drug Cases")

# Question 5
# For the entirety of the dallas dataset

# Overall version
age_breaks <- cut(dallas_data$ArresteeAge, breaks = seq(0, 100, by = 10))
age_df <- data.frame(table(age_breaks))
names(age_df) <- c("Breaks", "Count")

ggplot(data = dallas_data) + 
  geom_bar(mapping = aes(x = ArresteeAge, fill = cut(ArresteeAge, 
                                                     breaks = seq(0, 100, by = 10))), color = "black", position = "identity") + 
  scale_x_continuous(name = "Age", breaks = seq(0, 100, by = 5)) + 
  labs(title = "Overall Age Distribution", x = "Age", y = "Count", fill = "Age Group") + 
  geom_vline(xintercept = mean(dallas_data$ArresteeAge), color = "red", size = 0.5, linetype = "dashed") +
  geom_text(aes(x = mean(dallas_data$ArresteeAge), y = 100, label = "Mean Age"), color = "blue", size = 3, vjust = -30)

# Drugs
dallas_drugs <- subset(dallas_data, dallas_data$DrugRelated == "Yes")

# Drugs version
d_age_breaks <- cut(dallas_drugs$ArresteeAge, breaks = seq(0, 100, by = 10))
d_age_df <- data.frame(table(d_age_breaks))
names(d_age_df) <- c("Breaks", "Count")

ggplot(data = dallas_drugs) + 
  geom_bar(mapping = aes(x = ArresteeAge, fill = cut(ArresteeAge, 
                                                     breaks = seq(0, 100, by = 10))), color = "black", position = "identity") + 
  scale_x_continuous(name = "Age", breaks = seq(0, 100, by = 5)) + 
  labs(title = "Drug Crime Age Distribution", x = "Age", y = "Count", fill = "Age Group") + 
  geom_vline(xintercept = mean(dallas_data$ArresteeAge), color = "red", size = 0.5, linetype = "dashed") +
  geom_text(aes(x = mean(dallas_data$ArresteeAge), y = 100, label = "Mean Age"), color = "blue", size = 3, vjust = -20)

# Conclusion
# The age group 20 - 30 has the most crimes in Dallas consistently over the years

# Question 6
# Question: What period of time is more probable to have crime occur in Dallas & Denver (Day of the week)

days_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
dallas_days_df <- data.frame(table(dallas_data$ArrestDay))
names(dallas_days_df) <- c("Days", "Counts")
# dallas_days_df$Labels <- factor(dallas_days_df$Days, levels = days_order)

# Set the color scale for the heatmap
color_scale <- scale_fill_gradient(low = "#ABCDEF", high = "#FF0000")

# Create a Dallas heatmap
ggplot(dallas_days_df, aes(x = "Weekday", y = Days, fill = Counts)) +
  geom_tile() + scale_fill_gradient(low = "lightblue", high = "darkred") + 
  labs(title = "Dallas Heatmap of Days of Week vs Crime Counts") + theme_minimal()

denver_dates <- as.Date(denver_data$ReportedDate, format = "%m/%d/%Y")
denver_data$ArrestDay <- weekdays(denver_dates)

denver_days_df <- data.frame(table(denver_data$ArrestDay))
names(denver_days_df) <- c("Days", "Counts")

# Create a Denver heatmap
ggplot(denver_days_df, aes(x = "Weekday", y = Days, fill = Counts)) +
  geom_tile() + scale_fill_gradient(low = "lightblue", high = "darkred") + 
  labs(title = "Denver Heatmap of Days of Week vs Crime Counts") + theme_minimal()

# Dallas Chi-Square Test
dallas_crime_table <- table(dallas_days_df$Days, dallas_days_df$Counts)
dallas_chi_result <- chisq.test(dallas_crime_table)
print(dallas_chi_result)
# P-value comes to 0.227 so since it is greater than 0.05, there is not enough evidence to disprove the null hypothesis (null hypothesis: There is no association between the crime count and days of week)

# Question 7
dallas_weapon <- subset(dallas_data, dallas_data$ArrestYear >= 2018)
dallas_weapon <- subset(dallas_weapon, dallas_weapon$ArrestWeapon != "Unarmed")
dallas_weapon <- subset(dallas_weapon, dallas_weapon$ArrestWeapon != "None")

zip_counts <- table(dallas_weapon$ArrestZipCode)
zip_counts <- sort(zip_counts, decreasing = TRUE)
zip_df <- data.frame(zip_counts)
names(zip_df) <- c("Zip", "Count")
top_15_zip <- head(zip_df, 15)
dallas_weapon <- subset(dallas_weapon, ArrestZipCode %in% top_15_zip$Zip)

ggplot(dallas_weapon) + geom_bar(mapping = aes(x = factor(ArrestZipCode), fill = ArrestWeapon)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

zip_df$Zip <- as.numeric(zip_df$Zip)
correlation <- cor(zip_df$Zip, zip_df$Count, method = "spearman")
print(correlation)

# Question 8
# Question: Which ethnic group has the highest crime related cases in Dallas?

# Group the data by ethnicity and calculate the total number of crime-related cases for each ethnicity

ethnicity_counts <- data.frame(table(dallas_data$ArresteeRace))
ethnicity_counts <- na.omit(ethnicity_counts)
names(ethnicity_counts) <- c("ArresteeRace", "Total")
ethnicity_counts <- ethnicity_counts[order(-ethnicity_counts$Total), ]

# Calculate percentage of each group
ethnicity_counts$Percent <- ethnicity_counts$Total / sum(ethnicity_counts$Total) * 100

# Create pie chart
ggplot(ethnicity_counts, aes(x = "", y = Percent, fill = ArresteeRace)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = " Pie Chart of Dallas Crimes Related by Race") +
  theme_void()

# Drug Related
drug_related <- subset(dallas_data, dallas_data$DrugRelated == "Yes")
ethnicity_counts <- data.frame(table(drug_related$ArresteeRace))
ethnicity_counts <- na.omit(ethnicity_counts)
names(ethnicity_counts) <- c("ArresteeRace", "Total")
ethnicity_counts <- ethnicity_counts[order(-ethnicity_counts$Total), ]

# Calculate percentage of each group
ethnicity_counts$Percent <- ethnicity_counts$Total / sum(ethnicity_counts$Total) * 100

# Create pie chart
ggplot(ethnicity_counts, aes(x = "", y = Percent, fill = ArresteeRace)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = " Pie Chart of Dallas Crimes Related by Race and Drugs") +  
  theme_void()

# Question 9
dallas_armed <- subset(dallas_data, dallas_data$ArrestWeapon != "Unarmed" & dallas_data$ArrestWeapon != "None")
dallas_handgun <- dallas_armed[grep("gun", dallas_armed$ArrestWeapon), ]
dallas_more_guns <- dallas_armed[grep("Gun", dallas_armed$ArrestWeapon), ]
dallas_guns <- subset(dallas_armed, dallas_armed$ArrestWeapon == "Other Firearm")
dallas_firearms <- rbind(dallas_handgun, dallas_guns, dallas_more_guns)
dallas_firearms$ArrestYear <- factor(dallas_firearms$ArrestYear)

dallas_total_crime <- sum(dallas_data$ArrestState == "Texas")
dallas_gun_crime <- sum(dallas_firearms$ArrestState == "Texas")

# Percent of Firearm Crime
total_percent <- (dallas_gun_crime / dallas_total_crime) * 100
print(total_percent)

# Association to Drugs
dallas_drug_firearms <- subset(dallas_firearms, dallas_firearms$DrugRelated == "Yes")
dallas_drug_firearms <- dallas_drug_firearms[dallas_drug_firearms$DrugType != "",]

# Filter for arrests with drugs and firearms
dallas_drug_firearms <- subset(dallas_firearms, dallas_firearms$DrugRelated == "Yes")

# Count the number of people who had drugs and guns, handguns, shotguns
drug_firearms_count <- sum(dallas_drug_firearms$ArrestState == "Texas")

# Print the results
cat("Number of people who had drugs and guns, handguns, or shotguns:", drug_firearms_count, "\n")

# Percent of Drug Firearm Crime
drug_percent <- (drug_firearms_count / dallas_total_crime) * 100
print(drug_percent)

dallas_total_drug_crime <- sum(dallas_drugs$ArrestState == "Texas")

# Percent of Drug Firearm Crime of all Drug Crimes
drug2_percent <- (drug_firearms_count / dallas_total_drug_crime ) * 100
print(drug2_percent)

dallas_firearms_zip <- subset(dallas_firearms, select = ArrestZipCode)
dallas_firearms_df <- data.frame(table(dallas_firearms_zip))
names(dallas_firearms_df) <- c("Zipcode", "Count")

ggplot(data = dallas_firearms, aes(x = ArrestYear, y = ArresteeAge)) +
  geom_boxplot(aes(fill = ArrestYear)) + labs(title = "Distribution of Ages and Arrest Year for Armed Men", xlab = "Year", ylab = "Age")

drugged_firearms <- subset(dallas_firearms, dallas_firearms$DrugRelated == "Yes")

ggplot(data = drugged_firearms, aes(x = ArrestYear, y = ArresteeAge)) +
  geom_boxplot(aes(fill = ArrestYear)) + labs(title = "Distribution of Drugged Armed Criminals Ages and Arrest Year", xlab = "Year", ylab = "Age")

# Question 10
address_counts <- table(dallas_data$ArrestAddress)
address_counts <- sort(address_counts, decreasing = TRUE)
address_df <- data.frame(address_counts)
names(address_df) <- c("Address", "Count")

drug_dallas <- subset(dallas_data, dallas_data$DrugRelated == "Yes")
drug_addy_counts <- table(drug_dallas$ArrestAddress)
drug_addy_counts <- sort(drug_addy_counts, decreasing = TRUE)
drug_address_df <- data.frame(drug_addy_counts)
names(drug_address_df) <- c("Address", "Count")

top_10_addy <- head(address_df, 10)
top_10_drugAddy <- head(drug_address_df, 10)

ggplot(data = top_10_addy, aes(x = Address, y = Count, fill = Address)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Top 10 Streets With Highest Counts of General Crime") + geom_text(data = top_10_addy, aes(label = Count), size=3, vjust=1.5)

ggplot(data = top_10_drugAddy, aes(x = Address, y = Count, fill = Address)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Top 10 Streets With Highest Counts of Drug Crime") + geom_text(data = top_10_drugAddy, aes(label = Count), size=3, vjust=1.5)

# Merge Dataframes for Displaying Top 10 Violent Streets with Drug Relations
top_10_merge_df <- merge(top_10_addy, drug_address_df, by = "Address", all.x = TRUE)
top_10_merge_df[is.na(top_10_merge_df)] <- 0
names(top_10_merge_df) <- c("Address", "Count", "DrugCount")

ggplot(data = top_10_merge_df, aes(x = Address, y = Count, fill = Address)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Top 10 Streets With Highest Counts of General Crime With Drug Related") + geom_text(data = top_10_merge_df, aes(label = DrugCount), size=3, vjust=1.5)

# Question 11
denver_income$Tract_Name <- gsub("(.*),.*", "\\1", denver_income$Tract_Name)
denver_income$Tract_Name <- gsub("(.*),.*", "\\1", denver_income$Tract_Name)

names(denver_income)
st <- states()
plot(st$geometry)
co_counties <- counties("CO")
plot(co_counties$geometry)
den_tracts <- tracts("CO", "Denver")
plot(den_tracts$geometry)
colnames(denver_income)[4] <- "NAMELSAD"
denver_map_join <- den_tracts %>%
  left_join(denver_income, by = c("NAMELSAD" = "NAMELSAD"))
ggplot(denver_map_join, aes(fill = Percent_Poverty_AllPeople_Income_Below_Pov_Level)) + 
  geom_sf(color = "white", lwd = 0.2) +
  theme_void() +
  labs(fill  = "Income Below Federal Poverty Level")

# For Dallas
dallas_poverty <- dallas_poverty %>%
  group_by(Location) %>%
  summarize(avg_value = mean(Indicator.Rate.Value, na.rm = TRUE))

names(dallas_poverty) <- c("ZipCode", "Percent_Below_Fed_Poverty")

merged_zip <- left_join(zip_shape, dallas_poverty, by = "ZipCode")

ggplot(data = merged_zip) +
  geom_sf(aes(fill = Percent_Below_Fed_Poverty)) +
  scale_fill_gradient(low = "blue", high = "red")