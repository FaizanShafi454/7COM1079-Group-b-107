
library(readr)

# Read dataset
df <- read_csv("crimes_against_women_2001-2014 (2).csv")

# Clean column names
names(df)[2] <- "state"
names(df)[3] <- "district"
names(df)[7] <- "cruelty"

# REMOVE TOTAL ROWS (not keep only them!)
df2 <- subset(df, district != "TOTAL" & district != "DELHI UT TOTAL")

# Define regions
north_india_states <- c("DELHI", "UTTAR PRADESH", "RAJASTHAN", "PUNJAB", "HARYANA")
south_india_states <- c("KERALA", "TAMIL NADU", "KARNATAKA", "ANDHRA PRADESH")

# Create region column
df2$region <- ifelse(df2$state %in% north_india_states, "North India",
                     ifelse(df2$state %in% south_india_states, "South India", NA))

# Keep only North and South India
df2 <- subset(df2, !is.na(region))

# Remove NA values in cruelty
df2 <- subset(df2, !is.na(cruelty))

# Remove zero values (optional)
df2 <- subset(df2, cruelty > 0)

# BOX PLOT (main visualization)
boxplot(cruelty ~ region, data = df2,
        xlab = "Geographic Region", 
        ylab = "Cruelty by Husband or Relatives (Count)",
        main = "Cruelty Cases: North vs South India",
        col = c("lightblue", "lightgreen"))

# TWO HISTOGRAMS SIDE-BY-SIDE (supplementary)
par(mfrow = c(1, 2))  # Show 2 plots side-by-side

hist(df2$cruelty[df2$region == "South India"], 
     main = "South India", 
     xlab = "Cruelty Cases",
     col = "lightgreen")

hist(df2$cruelty[df2$region == "North India"], 
     main = "North India", 
     xlab = "Cruelty Cases",
     col = "lightblue")

# Reset plot layout
par(mfrow = c(1, 1))

# Statistical test - Mann-Whitney U test
test_result <- wilcox.test(cruelty ~ region, data = df2)
print(test_result)

# Summary statistics
cat("\n--- Summary Statistics ---\n")
cat("North India:\n")
print(summary(df2$cruelty[df2$region == "North India"]))
cat("\nSouth India:\n")
print(summary(df2$cruelty[df2$region == "South India"]))