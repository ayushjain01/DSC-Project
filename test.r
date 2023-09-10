# Install and load the dplyr package if you haven't already
install.packages("dplyr")
library(dplyr)

# Import all tables
profile_data <- read.csv("ias-profile.csv")
education_data <- read.csv("ias-education.csv")
experience_data <- read.csv("ias-experience.csv")

#Convert into Date data
profile_data$Allotment_Year <- as.Date(profile_data$Allotment_Year, format = "%d/%m/%Y")
profile_data$Date_of_Birth <- as.Date(profile_data$Date_of_Birth, format = "%d/%m/%Y")
profile_data$Date_of_Joining <- as.Date(profile_data$Date_of_Joining, format = "%d/%m/%Y")

#find median dates for all columns and replace NA with median dates
median_allotment_year <- median(profile_data$Allotment_Year, na.rm = TRUE)
profile_data$Allotment_Year[is.na(profile_data$Allotment_Year)] <- median_allotment_year

median_dob <- median(profile_data$Date_of_Birth, na.rm = TRUE)
profile_data$Date_of_Birth[is.na(profile_data$Date_of_Birth)] <- median_dob

median_doj <- median(profile_data$Date_of_Joining, na.rm = TRUE)
profile_data$Date_of_Joining[is.na(profile_data$Date_of_Joining)] <- median_doj

# Convert Cadre column to zones
profile_data <- profile_data %>%
  mutate(
    Cadre = case_when(
      Cadre %in% c("A G M U T", "Jammu & Kashmir", "Himachal Pradesh", "Uttarakhand", "Punjab", "Rajasthan", "Haryana") ~ "Zone-I",
      Cadre %in% c("Uttar Pradesh", "Bihar", "Jharkhand", "Odisha") ~ "Zone-II",
      Cadre %in% c("Gujarat", "Maharashtra", "Maharastra", "Madhya Pradesh", "Chhattisgarh", "Chhasttisgarh") ~ "Zone-III",
      Cadre %in% c("West Bengal", "Sikkim", "Assam Meghalya", "Manipur", "Manipur-Tripura", "Nagaland", "Tripura") ~ "Zone-IV",
      Cadre %in% c("Telangana", "Andhra Pradesh", "Karnataka", "Tamil Nadu", "Kerala") ~ "Zone-V",
      TRUE ~ "Unknown"  # Assign "Unknown" for any other cadres not in the specified zones
    )
  )
profile_data$Cadre <- factor(profile_data$Cadre, levels = c("Zone-I", "Zone-II", "Zone-III", "Zone-IV", "Zone-V", "Unknown"))

# Replace "-" in "Mother_Tongue" with a blank
profile_data$Mother_Tongue <- ifelse(
  profile_data$Mother_Tongue == "-", 
  "", 
  profile_data$Mother_Tongue
)


# Replace "Languages_Known" with "Mother_Tongue" if "Languages_Known" is blank
profile_data$Languages_Known <- ifelse(
  profile_data$Languages_Known == "", 
  profile_data$Mother_Tongue, 
  profile_data$Languages_Known
)

# Convert Gender column to factors "M" and "F"
profile_data$Gender <- factor(profile_data$Gender, levels = c("Male", "Female"))

# Split the "Languages_Known" values by space
language_lists <- strsplit(profile_data$Languages_Known, " ")

# Count the number of languages for each row
profile_data$Languages_Known <- sapply(language_lists, length)

# Summary statistics of Table 1
print(summary(profile_data))
