#setwd and load packages
setwd("~/Desktop/PhD/FAO/Data")
library(tidyverse)
library(dplyr)
library(tidyr)
library(matrixStats)
library(ggplot2)
library(blandr)
library(readxl)
library(reshape)
library(readr)
library(cowplot)
library(scales)
library(gridExtra)
library(extrafont)
library(grid)
library(png)
library(ggridges)
library(ggpattern)
library(RColorBrewer)
library(patchwork)
library(tibble)
library(forcats)
library(officer)
library(flextable)


# you will need to download the appropriate FAO datasets.
# for this code, we downloaded the data of each aggregated food category separately
# this allowed for the assignation of the food categories to all the individual items
# for analysis by aggregated food category

# function to read in data
transfunc <- function(file_path, catname, col_prefix) {
  # Read CSV file
  data <- read_csv(file_path)
  # Keep only the specified columns
  data <- data %>% select(Area, Item, Year, Value)
  # Reshape the data from wide to long format
  data_long = data %>% 
    spread(Year, Value)
  
  # Change the column names to old/new to prepare for merge
  old_names <- c("2010", "2011", "2012", "2013")
  new_names <- paste0("Y", old_names, col_prefix)
  for (i in 1:length(old_names)) {
    names(data_long)[names(data_long) == old_names[i]] <- new_names[i]
  }
  
  # Add a new variable
  data_long$category <- rep(catname, nrow(data_long))
  # Return the modified dataset
  return(data_long)
}

# Define the file paths and category names
file_base <- c("vegetables", "alcoholic beverages", "animal fats",
               "aquatic products, other", "cereals - excluding beer", "eggs",
               "fish, seafood", "fruits - excluding wine", "meat",
               "milk - excluding butter", "miscellaneous", "offals",
               "oilcrops", "pulses", "spices", "starchy roots",
               "stimulants", "sugar & sweeteners", "sugar crops",
               "treenuts", "vegetable oils")

old_file_paths <- paste0("old ", file_base, ".csv")
new_file_paths <- paste0("new ", file_base, ".csv")
catnames <- file_base

# Initialize an empty data frame to store the modified datasets
combined_modified_data <- data.frame()

# Loop through the datasets and apply the transfunc function
for (i in 1:length(old_file_paths)) {
  old_data <- transfunc(old_file_paths[i], catnames[i], "old")
  new_data <- transfunc(new_file_paths[i], catnames[i], "new")
  
  # Merge old and new datasets for each category
  merged_data <- full_join(old_data, new_data, by = c("Area", "Item", "category"))
  
  # Combine the merged datasets
  combined_modified_data <- rbind(combined_modified_data, merged_data)
}

dat <- combined_modified_data
#identify items that contain an NA value
test1 <- dat[rowSums(is.na(dat)) > 0,] #2,257
#delete NA items
final <- dat[(complete.cases(dat)), ]

dat <- final
#create variable that avgs all old dataset values per item
dat$oldfoodavg = ((dat$Y2010old + dat$Y2011old + dat$Y2012old + dat$Y2013old)/4)
#create variable that avgs all new dataset values per item
dat$newfoodavg = ((dat$Y2010new + dat$Y2011new + dat$Y2012new + dat$Y2013new)/4)
##create variable that calculates absolute difference between items
dat$avg <- (dat$newfoodavg + dat$oldfoodavg)/2
dat$diff <- dat$oldfoodavg - dat$newfoodavg
dat$absdiff <- abs(dat$diff)
#add variable for items that had a large change
dat <- dat %>%
  mutate(largechange = case_when(
    abs(newfoodavg - oldfoodavg) > 3.65 ~ 1,
    TRUE ~ 0
  ))
table(dat$largechange)
923/13444 #6.9%
#add variable for tiems that had no change
dat <- dat %>%
  mutate(nochange = case_when(
    abs(newfoodavg - oldfoodavg) ==0 ~ 1,
    TRUE ~ 0
  ))
table(dat$nochange)
3015/13444 #22.4%



subset_dat <- subset(dat, Area == "United Kingdom of Great Britain and Northern Ireland" & category == "meat")



#ITEM CATEGORIES
#read in data
old <- read.csv('olditemcats.csv')
new <- read.csv('newitemcats.csv')
#keep only variables needed for analysis
old <- old[ , which(names(old) %in% c("Area", "Item.Code", "Item", "Year", "Value"))]
new <- new[ , which(names(new) %in% c("Area", "Item.Code", "Item", "Year", "Value"))]
#trasform dataset to wide (from long)
old <- old %>%
  spread(Year, Value)
new <- new %>%
  spread(Year, Value)
#rename columns (to prepare for merge)
colnames(old) <- c("Area", "Item", "Y2010old", "Y2011old", "Y2012old", "Y2013old")
colnames(new) <- c("Area", "Item", "Y2010new", "Y2011new", "Y2012new", "Y2013new")
#merge datasets
joined <- left_join(new, old, by = c("Area", "Item"))
#all items that contain an NA value (mainly Serbia? For missing 2 years of data it seems)
test1 <- joined[rowSums(is.na(joined)) > 0,] #178
#delete old dataset all-NA items (these items exist only in the new dataset)
final <- joined[(complete.cases(joined)), ]
datcat <- final
#create variable that avgs all old dataset values per item
datcat$oldfoodavg = ((datcat$Y2010old + datcat$Y2011old + datcat$Y2012old + datcat$Y2013old)/4)
#create variable that avgs all new dataset values per item
datcat$newfoodavg = ((datcat$Y2010new + datcat$Y2011new + datcat$Y2012new + datcat$Y2013new)/4)
#turn NAs to zeros (to allow for addition when some years are missing
##create variable that calculates absolute difference between items
datcat$avg <- (datcat$newfoodavg + datcat$oldfoodavg)/2
datcat$diff <- datcat$oldfoodavg - datcat$newfoodavg
datcat$absdiff <- abs(datcat$diff)
#add variable for items that had a large change
datcat <- datcat %>%
  mutate(largechange = case_when(
    abs(newfoodavg - oldfoodavg) > 3.65 ~ 1,
    TRUE ~ 0
  ))
table(datcat$largechange)
861/3466 #24.8%
#add variable for tiems that had no change
datcat <- datcat %>%
  mutate(nochange = case_when(
    abs(newfoodavg - oldfoodavg) ==0 ~ 1,
    TRUE ~ 0
  ))
table(datcat$nochange)
307/3466 #8.8%



#combining datasets -- prepare harmonization by adding a category variable 
datcat$category <- datcat$Item

# Add the 'isag' variable to both datasets to label whether or not the item is
#an individual item or if it's an aggregated category item
dat <- dat %>%
  mutate(isag = 0)
datcat <- datcat %>%
  mutate(isag = 1)
# Convert the 'category' variable in 'datcat' to lowercase
datcat <- datcat %>%
  mutate(category = tolower(category))
# Merge the two datasets
merged_data <- bind_rows(dat, datcat)
#just a wee check to see if there are any NA values [there should be none])
num_NAs <- sum(is.na(merged_data))


#add population
# Read the datasets
newpop <- read.csv("newpop.csv")
oldpop <- read.csv("oldpop.csv")
pop <- read.csv("2020pop.csv")
# Select the specified columns
newpop <- newpop %>% select(Area, Item, Year, Value)
oldpop <- oldpop %>% select(Area, Item, Year, Value)
pop <- pop %>% select(Area, Item, Year, Value)
oldpop <- oldpop %>%
  spread(Year, Value)
newpop <- newpop %>%
  spread(Year, Value)
pop <- pop %>%
  spread(Year, Value) %>%
  select(-Item) %>%
  dplyr::rename(population = `2020`)
oldpop <- oldpop %>%
  select(-Item) %>%
  mutate(oldpopavg = (oldpop$`2010` + oldpop$`2011` + oldpop$`2012` + oldpop$`2013`) / 4) %>%
  select(-`2010`,-`2011`,-`2012`,-`2013`)
newpop <- newpop %>%
  select(-Item) %>%
  mutate(newpopavg = (newpop$`2010` + newpop$`2011` + newpop$`2012` + newpop$`2013`) / 4) %>%
  select(-`2010`,-`2011`,-`2012`,-`2013`)

mergedfinal <- bind_rows(merged_data, pop, oldpop, newpop)
mergedfinal <- mergedfinal %>%
  #group by ID
  group_by(Area) %>%
  arrange(Area) %>%
  fill(population, oldpopavg, newpopavg) %>%
  fill(population, oldpopavg, newpopavg, .direction = "up") %>%
  filter(!is.na(Item))

dat <- mergedfinal

# Split the dataset
dat_isag0 <- dat[dat$isag == 0, ]
dat_isag1 <- dat[dat$isag == 1, ]

#calculate MOR and CV

#MOR - all items no ag food cats
# Create a new variable "MOR" and set it to 0
dat_isag0$MOR <- 0

# Calculate the ratio for each year and row-wise average
ratio_matrix <- dat_isag0[, c("Y2010old", "Y2010new", "Y2011old", "Y2011new", "Y2012old", "Y2012new", "Y2013old", "Y2013new")]
ratio_matrix <- t(apply(ratio_matrix, 1, function(x) ifelse(x[c(TRUE, FALSE)] != 0, x[c(FALSE, TRUE)] / x[c(TRUE, FALSE)], NA)))
dat_isag0$MOR <- rowMeans(ratio_matrix, na.rm = TRUE)
#check the mean MOR across all items (no ag food cats)
mean(dat_isag0$MOR, na.rm = TRUE) #1.49 (new items are, on average, 49% higher)

#MOR - ag food cats
# Create a new variable "MOR" and set it to 0
dat_isag1$MOR <- 0

# Calculate the ratio for each year and row-wise average
ratio_matrix <- dat_isag1[, c("Y2010old", "Y2010new", "Y2011old", "Y2011new", "Y2012old", "Y2012new", "Y2013old", "Y2013new")]
ratio_matrix <- t(apply(ratio_matrix, 1, function(x) ifelse(x[c(TRUE, FALSE)] != 0, x[c(FALSE, TRUE)] / x[c(TRUE, FALSE)], NA)))
dat_isag1$MOR <- rowMeans(ratio_matrix, na.rm = TRUE)
#check the mean MOR across all ag food cats
mean(dat_isag1$MOR, na.rm = TRUE) #2.74 (new items are, on average, 274% higher)

#CV - all items no ag food cats
#CV -  ag food cats
# calculate the ratio columns
dat_isag0$Y2010ratio <- dat_isag0$Y2010new / dat_isag0$Y2010old
dat_isag0$Y2011ratio <- dat_isag0$Y2011new / dat_isag0$Y2011old
dat_isag0$Y2012ratio <- dat_isag0$Y2012new / dat_isag0$Y2012old
dat_isag0$Y2013ratio <- dat_isag0$Y2013new / dat_isag0$Y2013old
# Calculate standard deviation of specified columns
sd <- apply(dat_isag0[, c("Y2010ratio", "Y2011ratio", "Y2012ratio", "Y2013ratio")], 1, sd)
# Add new column with standard deviation values to the dataframe
dat_isag0$sd <- sd
#add cv variable
dat_isag0$cv <- (dat_isag0$sd/dat_isag0$MOR)*100


#CV -  ag food cats
# calculate the ratio columns
dat_isag1$Y2010ratio <- dat_isag1$Y2010new / dat_isag1$Y2010old
dat_isag1$Y2011ratio <- dat_isag1$Y2011new / dat_isag1$Y2011old
dat_isag1$Y2012ratio <- dat_isag1$Y2012new / dat_isag1$Y2012old
dat_isag1$Y2013ratio <- dat_isag1$Y2013new / dat_isag1$Y2013old
# Calculate standard deviation of specified columns
sd <- apply(dat_isag1[, c("Y2010ratio", "Y2011ratio", "Y2012ratio", "Y2013ratio")], 1, sd)
# Add new column with standard deviation values to the dataframe
dat_isag1$sd <- sd
#add cv variable
dat_isag1$cv <- (dat_isag1$sd/dat_isag1$MOR)*100










#################FIGURE 1 ###########################


plot_isag0 <- ggplot(dat_isag0, aes(x = avg, y = diff)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_hline(yintercept = mean(dat_isag0$diff, na.rm = TRUE), linetype = "dashed", colour = "blue", size = 0.5, alpha = 0.6) +
  geom_hline(yintercept = mean(dat_isag0$diff, na.rm = TRUE) - (1.96 * sd(dat_isag0$diff, na.rm = TRUE)), linetype = "dashed", colour = "red", size = 0.5, alpha = 0.6) +
  geom_hline(yintercept = mean(dat_isag0$diff, na.rm = TRUE) + (1.96 * sd(dat_isag0$diff, na.rm = TRUE)), linetype = "dashed", colour = "red", size = 0.5, alpha = 0.6) +
  ylab("Diff. of new & old method (kg/capita/yr)") +
  xlab("Mean of new & old method (kg/capita/yr)") +
  xlim(0, 375) +
  ylim(-130, 200) +
  theme_classic() +
  ggtitle("Individual items") +
  theme(text = element_text(family = "Times"))

plot_isag1 <- ggplot(dat_isag1, aes(x = avg, y = diff)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_hline(yintercept = mean(dat_isag1$diff, na.rm = TRUE), linetype = "dashed", colour = "blue", size = 0.5, alpha = 0.6) +
  geom_hline(yintercept = mean(dat_isag1$diff, na.rm = TRUE) - (1.96 * sd(dat_isag1$diff, na.rm = TRUE)), linetype = "dashed", colour = "red", size = 0.5, alpha = 0.6) +
  geom_hline(yintercept = mean(dat_isag1$diff, na.rm = TRUE) + (1.96 * sd(dat_isag1$diff, na.rm = TRUE)), linetype = "dashed", colour = "red", size = 0.5, alpha = 0.6) +
  ylab("Diff. of new & old method (kg/capita/yr)") +
  xlab("Mean of new & old method (kg/capita/yr)") +
  xlim(0, 375) +
  ylim(-130, 200) +
  theme_classic() +
  ggtitle("Aggregated item categories") +
  theme(text = element_text(family = "Times"))

# Set the width for the saved plot
width <- 5

# Save each plot separately
ggsave("plot_isag0.png", plot = plot_isag0, width = width, height = 5, dpi = 300)
ggsave("plot_isag1.png", plot = plot_isag1, width = width, height = 5, dpi = 300)

ggsave("~/Desktop/PhD/FAO/Figure 1 - Combined BA plot EPSplt1.pdf", plot_isag0, width = 5, height = 5, device = "pdf")
ggsave("~/Desktop/PhD/FAO/Figure 1 - Combined BA plot EPSplt2.pdf", plot_isag1, width = 5, height = 5, device = "pdf")

# Combine the saved plots side by side
plot_isag0 <- rasterGrob(readPNG("plot_isag0.png"), interpolate = TRUE)
plot_isag1 <- rasterGrob(readPNG("plot_isag1.png"), interpolate = TRUE)
combined_plot <- grid.arrange(plot_isag0, plot_isag1, ncol = 2)

# Save the plot to a folder on your Desktop called "FAO"
ggsave("~/Desktop/PhD/FAO/Figure 1 - Combined BA plot.png", combined_plot, width = 10, height = 6, dpi = 300)
ggsave("~/Desktop/PhD/FAO/Figure 1 - Combined BA plot PDF.pdf", combined_plot, width = 10, height = 6, device = "pdf")
ggsave("~/Desktop/PhD/FAO/Figure 1 - Combined BA plot EPS.eps", combined_plot, width = 10, height = 6, device = "eps")

blandr.statistics(dat_isag1$oldfoodavg, dat_isag1$newfoodavg, sig.level = 0.95)



















################### FIGURE 2 ######################





test <- dat_isag1 %>%
  group_by(Item) %>%
  summarise(
    n = n(),
    meandiff = mean(absdiff, na.rm=T),
    avgwt = mean((oldfoodavg + newfoodavg)/2, na.rm=T)) 


# Reorder the items from largest to smallest meandiff
test$Item <- with(test, reorder(Item, -meandiff))

# Create the scatterplot with point size based on avgwt
plot <- ggplot(test, aes(x = Item, y = meandiff, size = avgwt)) +
  geom_point(color = "#C6DBEF") +
  geom_hline(yintercept = 5.6, linetype = "dashed", color = "black") +
  annotate("text", x = length(unique(test$Item)), y = 5.6, label = "mean abs. diff. (5.6 kg)", vjust = -0.5, hjust = 1, size = 4, family = "Avenir") +
  labs(y = "Abs. diff. between methods (kg/capita/yr)", size = "Avg. Weight*", family = "Avenir") +
  theme_classic() +
  theme(axis.text.x = element_blank(), legend.position = "right", text = element_text(family = "Avenir")) +
  scale_size_continuous(range = c(2, 12), 
                        breaks = c(25, 50, 75),
                        labels = c("25kg", "50kg", "75kg"), 
                        name = "Avg. Weight*", 
                        guide = guide_legend(title.position = "top", label.position = "left", title.hjust = 0.5, title.vjust = -0.2)) +
  guides(size = guide_legend(override.aes = list(color = "#C6DBEF", shape = 16), keywidth = 1.5, keyheight = 1.5)) # Override default shape and color of the legend

# Add x-axis labels below plot
x_labels <- paste(test$Item, collapse = " | ")
plot <- plot + labs(x = NULL) +
  theme(plot.title = element_text(hjust = 0, size = 12, face = "bold", family = "Avenir"),
        axis.text.x = element_text(size = 11.8, angle = 45, hjust = 1, vjust = 1, family = "Avenir"),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(1, 1, 2, 3), "lines"), 
        text = element_text(family = "Avenir")) # Adjust the bottom margin

label_plot <- ggplot() +
  theme_void() +
  labs(x = "Aggregated food categories") +
  theme(plot.margin = margin(t = 0, r = 0, b = 75, l = 0, unit = "pt"),
        axis.title.x = element_text(size = 12, family = "Avenir"))

# Combine the plots using cowplot
plot1 <- plot_grid(plot, label_plot, nrow = 2, rel_heights = c(0.9, 0.1))

# Save the plot to a folder on your Desktop called "FAO"
ggsave("~/Desktop/FAO/Figure 2 - Ag. food cats diff plot.png", plot1, width = 10, height = 8, dpi = 300)





#########################FIGURE 3 ################################

#define percentage 
dat_isag0 <- dat_isag0 %>%
  #grouping by country
  group_by(Area) %>%
  #Sorting by country
  arrange(Area) %>% 
  # A handy bit of code that lets you count observations within group (specified above with group_by)
  mutate(nitems=ave(1:length(Area), Area, FUN = seq_along)) %>%
  #get max no. items
  mutate(totalitems=max(nitems)) %>%
  #tally up no. of items in nochange and largechange variables
  mutate(nochangetot = cumsum(nochange)) %>%
  mutate(nochangetot = max(nochangetot)) %>%
  mutate(largechangetot = cumsum(largechange)) %>%
  mutate(largechangetot = max(largechangetot)) %>%
  #calculate percent variables
  mutate(nochangeperc = nochangetot/totalitems) %>%
  mutate(largechangeperc = largechangetot/totalitems) %>%
  mutate(smallchangeperc = 1-(largechangeperc+nochangeperc))


#stacked barplot

# Subset the data
# choose countries that are largest by population in each world region,
# in addition to the countries mentioned in the text as the extremes of the variables
subset_countries <- c("United Arab Emirates", "United States of America", "India", "Brazil", "Germany", "China, mainland", "Bangladesh", "Indonesia", "Japan", "Nigeria")
dat_isag0_subset <- dat_isag0 %>% filter(Area %in% subset_countries)

# Rename countries
dat_isag0_subset$Area <- recode(dat_isag0_subset$Area, `United Arab Emirates` = "UAE", `China, mainland` = "China", `United States of America` = "USA")

# Calculate average for each variable per country
dat_isag0_avg <- dat_isag0_subset %>%
  group_by(Area) %>%
  summarise(largechangeperc_avg = mean(largechangeperc),
            smallchangeperc_avg = mean(smallchangeperc),
            nochangeperc_avg = mean(nochangeperc))

print(dat_isag0_avg)

# Reshape data for ggplot2
dat_isag0_long <- pivot_longer(dat_isag0_avg, cols = c(largechangeperc_avg, smallchangeperc_avg, nochangeperc_avg), names_to = "Variable", values_to = "Value")

# Specify colors for each variable
variable_colors <- c("largechangeperc_avg" = "#08306B", "smallchangeperc_avg" = "#2171B5", "nochangeperc_avg" = "#C6DBEF")

# Custom labeling function
label_custom_percent <- function(x) {
  paste0(x * 100, "%")
}

# Reorder the factor levels of the 'Variable' column so that no change is on bottom
dat_isag0_long$Variable <- fct_relevel(dat_isag0_long$Variable, "largechangeperc_avg", "smallchangeperc_avg", "nochangeperc_avg")

# Create a stacked barplot
plot <- ggplot(dat_isag0_long, aes(x = Area, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", alpha = 1) +
  scale_fill_manual(values = variable_colors, labels = c("Large change", "Small change", "No change")) +
  scale_y_continuous(labels = label_custom_percent) +
  labs(x = "Country", y = "Percentage of all items") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Avenir", size = 12),
        axis.text.y = element_text(family = "Avenir", size = 12),
        axis.title.x = element_text(family = "Avenir", size = 12),
        axis.title.y = element_text(family = "Avenir", size = 12),
        legend.text = element_text(family = "Avenir", size = 12),
        legend.title = element_blank())

print(plot)

# Save the plot to a folder on your Desktop called "FAO"
ggsave("~/Desktop/FAO/Figure 3 - no change & large change items by country.png", plot, width = 10, height = 8, dpi = 300)





























###############################FIGURE 4#####################

# Read in data
old <- read.csv('FoodBalanceSheetsHistoric_E_All_Data.csv')
new <- read.csv('FoodBalanceSheets_E_All_Data.csv')

# Select variables and filter data for meat analysis
select_vars <- function(df, years) {
  df <- df[df$Element == "Food supply quantity (kg/capita/yr)" &
             df$Area == "United Kingdom of Great Britain and Northern Ireland" &
             df$Item == "Meat", c("Area", "Item.Code", "Item", "Element", years)]
  df$Area <- gsub("United Kingdom of Great Britain and Northern Ireland", "UK", df$Area)
  df <- df[, -c(2:4)]
  colnames(df) <- c("Data", years)
  return(df)
}

old_years <- paste("Y", 1980:2013, sep="")
new_years <- paste("Y", 2010:2018, sep="")

uko <- select_vars(old, old_years)
ukn <- select_vars(new, new_years)

# Convert from kg/yr to g/d
convert_to_g_per_day <- function(df, years) {
  df[years] <- lapply(df[years], function(x) (x * 1000) / 365)
  return(df)
}

old_years <- paste("Y", 1980:2013, sep="")
new_years <- paste("Y", 2010:2018, sep="")

uko <- convert_to_g_per_day(uko, old_years)
ukn <- convert_to_g_per_day(ukn, new_years)

# Create a copy of the old data
adj <- uko
# Multiply 'old' data by UK AOR of 0.94493782
adj_factor <- 0.94493782
old_years <- paste("Y", 1980:2013, sep="")

for (year in old_years) {
  adj[[year]] <- uko[[year]] * adj_factor
}

# Add in 'new data'
new_years <- paste("Y", 2014:2018, sep="")
for (year in new_years) {
  adj[[year]] <- ukn[[year]]
}

#difference between 2008 - 2018 - +4.9%
((adj$`2018`-adj$`2008`)/adj$`2008`)*100
#rename column in adjusted dataset
adj$Data[adj$Data == 'UK old'] <- 'UK old, adjusted'


#transpose data
ukot <- as.data.frame(t(uko))
ukot <- as.data.frame(ukot[-1,])
colnames(ukot) <- c("uko")
uknt <- as.data.frame(t(ukn))
uknt <- as.data.frame(uknt[-1,])
colnames(uknt) <- c("ukn")
adjt <- as.data.frame(t(adj))
adjt <- as.data.frame(adjt[-1,])
colnames(adjt) <- c("adj")
#combine

#combine
forplot <- ukot
forplot[nrow(forplot) + 5,] <- NA #add 5 spaces at the end of forplot
#add columns to uknt to make it compatible for joining
num_nas_to_add <- 30
for (i in 1:num_nas_to_add) {
  uknt <- rbind(c(NA), uknt)
}
#combine data
forplot$ukn <- uknt$ukn
forplot$adj <- adjt$adj
#rename rows and columns
colnames(forplot) <- c("Old", "New", "Adjusted")
row.names(forplot) <- c("1980", "1981", "1982", "1983",
                        "1984", "1985", "1986", "1987", "1988",
                        "1989", "1990", "1991", "1992", "1993",
                        "1994", "1995", "1996", "1997", "1998",
                        "1999", "2000", "2001", "2002", "2003",
                        "2004", "2005", "2006", "2007", "2008",
                        "2009", "2010", "2011", "2012", "2013",
                        "2014", "2015", "2016", "2017", "2018")
#add column for the year
forplot$Year <- row.names(forplot)
#change data to numeric
forplot$Old <- as.numeric(forplot$Old)
forplot$New <- as.numeric(forplot$New)
forplot$Adjusted <- as.numeric(forplot$Adjusted)
forplot$Year <- as.numeric(forplot$Year)
#convert to one-column long for graphing
d <- melt(forplot, id.vars="Year")
e <- head(d,-5)
#make zeh plot
abbreviate_year <- function(year) {
  if (is.na(year)) {
    return(NA)
  }
  
  year <- round(year)
  if (year %% 100 == 0 || year == 1980) {
    return(as.character(year))
  } else {
    return(sprintf("'%02d", year %% 100))
  }
}
plot <- ggplot(data=na.omit(e), aes(x=Year, y = value, group = variable, color = variable)) +
  geom_line(size = 1) +
  geom_point(shape = 20, alpha = 0.05) +
  ylab("Meat supply (g/capita/day)") +
  theme_classic(base_family = "Times") +
  theme(axis.text.x = element_text(size=12, angle=45, vjust=0.5, family = "Times")) +
  theme(axis.text.y = element_text(size=12, family = "Times")) +
  scale_color_manual(name="Dataset",
                     labels=c("Old", "New", "Adjusted"),
                     values=c("#EF3B2C", "#41AE76", "#9ECAE1")) +
  scale_y_continuous(breaks = breaks_pretty(10)) +
  scale_x_continuous(breaks = breaks_pretty(15),
                     labels = function(x) sapply(x, abbreviate_year))

print(plot)

ggsave("~/Desktop/PhD/FAO/Figure 4 - UK meat supply, adjusted and unadjusted 1980-2018.png", plot, width = 8, height = 6, dpi = 300)
ggsave("~/Desktop/PhD/FAO/Figure 2 - UK meat supply, adjusted and unadjusted 1980-2018 PDF.pdf", plot, width = 8, height = 6, device = "pdf")
ggsave("~/Desktop/PhD/FAO/Figure 2 - UK meat supply, adjusted and unadjusted 1980-2018 EPS.eps", plot, width = 8, height = 6, device = "eps")






##########################Supplemental figure#########################


# FAO data --------------------------------------------------------------------------------------------------------
comm_bal_1 <- rbind(
  read.csv(file.path("FoodBalanceSheets_E_All_Data_Norm.csv")) %>% mutate(Methodology="New"),
  read.csv(file.path("FoodBalanceSheetsHistoric_E_All_Data_Norm.csv")) %>% mutate(Methodology="Old")
) %>%
  rename_with(~ gsub("[\\.]+", "", .x)) %>%
  # remove duplicate eggs and milk totals, as they have the same Item text name as the individual items
  filter(!(ItemCode %in% c(2948, 2949, 2899)), !(Item %in% c("Population", "Grand Total")),
         Element %in% c("Domestic supply quantity", "Seed", "Processing", "Food", "Feed", "Production", 
                        "Stock Variation", "Import Quantity", "Export Quantity", "Other uses (non-food)", 
                        "Losses", "Residuals", "Food supply (kcal/capita/day)")
  ) %>%
  mutate(Value = as.numeric(Value),
         Value = case_when(
           Unit == "1000 tonnes" ~ Value / 1000,
           Unit == "tonnes" ~ Value / 1000000,
           T ~ Value),
         Unit = if_else(Unit %in% c("1000 tonnes", "tonnes"), "Mt", Unit),
         Year = as.integer(Year)
  ) %>%
  mutate(Item = case_when(
    Item == "Groundnuts" ~ "Groundnuts (Shelled Eq)",
    Item == "Rice (Milled Equivalent)" ~ "Rice and products",
    TRUE ~ Item)) %>%
  mutate(Value = if_else(Element == "stockVar" & Year <= 2009, -Value, Value)) # Different meaning of sign in new data

# Milk --------------------------------------------------------------------------------------------------------

p <- comm_bal_1 %>%
  filter(Item=="Milk - Excluding Butter", Area == "World", Year %in% 2008:2018) %>%
  filter(!(Element %in% c("Domestic supply quantity", "Food supply (kcal/capita/day)"))) %>%
  ggplot(aes(x=Year, y=Value, colour=Methodology)) +
  geom_line() +
  geom_point(size=1) +
  scale_x_continuous(breaks=seq(2008, 2018, 2)) +
  scale_y_continuous(name="Quantity (Mt)") +
  facet_wrap(. ~ Element) +
  theme_classic() +
  scale_colour_manual(values=c("Old"="red", "New"="darkgreen"), name="Dataset")

p

ggsave("~/Documents/PhD/Other/FBS comment/milk.png", p, width = 11, height = 6)

















#########################SI TABLE 1##############################

# ag food cats, cols: abs diff, MOR, CV
# calculate averages by category
df_avg <- dat_isag1 %>%
  group_by(category) %>%
  summarize(oldfoodavg = mean(oldfoodavg, na.rm = TRUE),
            newfoodavg = mean(newfoodavg, na.rm = TRUE),
            absdiff = mean(absdiff, na.rm = TRUE),
            MOR = mean(MOR, na.rm = TRUE),
            cv = mean(cv, na.rm = TRUE))

# view resulting table
df_avg
#round data to 2 decimals
df_avg[,2:ncol(df_avg)] <- round(df_avg[,2:ncol(df_avg)], 2)
# Create a flextable object from the df_avg data frame
ft <- flextable(df_avg)
# Set table style with horizontal and vertical lines
ft <- border_outer(ft, border = fp_border(color = "black", width = 1))
ft <- border_inner_h(ft, border = fp_border(color = "black", width = 1))
ft <- border_inner_v(ft, border = fp_border(color = "black", width = 1))
# Add table caption
ft <- set_caption(ft, "Averages by Category")
# Write table to Word document
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
# Save the output to the "FAO" folder on your desktop
output_path <- file.path("~/Desktop/FAO/output_table.docx")
print(doc, target = output_path)



###########################SI TABLE 2##################################

# calculate averages by category
df_avg <- dat_isag0 %>%
  group_by(Area) %>%
  summarize(nochangeperc = mean(nochangeperc*100, na.rm = TRUE),
            largechangeperc = mean(largechangeperc*100, na.rm = TRUE),
            smallchangeperc = mean(smallchangeperc*100, na.rm = TRUE))

# round the numerical columns except for the "Area" column to 1 decimal place
df_avg[, 2:ncol(df_avg)] <- round(df_avg[, 2:ncol(df_avg)], 1)

# view resulting table
df_avg


# Create a flextable object from the df_avg data frame
ft <- flextable(df_avg)
# Set table style with horizontal and vertical lines
ft <- border_outer(ft, border = fp_border(color = "black", width = 1))
ft <- border_inner_h(ft, border = fp_border(color = "black", width = 1))
ft <- border_inner_v(ft, border = fp_border(color = "black", width = 1))
# Add table caption
ft <- set_caption(ft, "Averages by Country")
# Write table to Word document
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
# Save the output to the "FAO" folder on your desktop
output_path <- file.path("~/Desktop/FAO/output_table.docx")
print(doc, target = output_path)





