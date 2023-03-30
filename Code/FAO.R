#setwd and load packages
setwd("~/Desktop/Data")
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
library(grid)
library(png)
library(ggridges)
library(ggpattern)
library(RColorBrewer)
library(patchwork)
library(tibble)
library(BlandAltmanLeh)



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
  theme(text = element_text(family = "Avenir"))

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
  theme(text = element_text(family = "Avenir"))

# Set the width for the saved plot
width <- 5

# Save each plot separately
ggsave("plot_isag0.png", plot = plot_isag0, width = width, height = 5, dpi = 300)
ggsave("plot_isag1.png", plot = plot_isag1, width = width, height = 5, dpi = 300)

# Combine the saved plots side by side
plot_isag0 <- rasterGrob(readPNG("plot_isag0.png"), interpolate = TRUE)
plot_isag1 <- rasterGrob(readPNG("plot_isag1.png"), interpolate = TRUE)
combined_plot <- grid.arrange(plot_isag0, plot_isag1, ncol = 2)

# Print the combined plot
print(combined_plot)

# Save the plot to a folder on your Desktop called "FAO"
ggsave("~/Desktop/FAO/Figure 1 - Combined BA plot.png", combined_plot, width = 10, height = 8, dpi = 300)


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
                        labels = c("<25kg", "50kg", ">75kg"), 
                        name = "Avg. Weight*", 
                        guide = guide_legend(title.position = "top", label.position = "left", title.hjust = 0.5, title.vjust = -0.2)) +
  guides(size = guide_legend(override.aes = list(color = "#C6DBEF", shape = 16), keywidth = 1.5, keyheight = 1.5)) # Override default shape and color of the legend

# Add x-axis labels below plot
x_labels <- paste(test$Item, collapse = " | ")
plot <- plot + labs(x = NULL) +
  theme(plot.title = element_text(hjust = 0, size = 12, face = "bold", family = "Avenir"),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1, family = "Avenir"),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(1, 1, 2, 3), "lines"), 
        text = element_text(family = "Avenir")) # Adjust the bottom margin

label_plot <- ggplot() +
  theme_void() +
  labs(x = "Aggregated food categories") +
  theme(plot.margin = margin(t = 0, r = 0, b = 75, l = 0, unit = "pt"),
        axis.title.x = element_text(size = 10, family = "Avenir"))

# Combine the plots using cowplot
plot1 <- plot_grid(plot, label_plot, nrow = 2, rel_heights = c(0.9, 0.1))


# Display the plot
print(plot1)

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
  mutate(largechangeperc = largechangetot/totalitems)


## Filter the dataset to include only the desired countries
dat_filtered <- dat_isag0[dat_isag0$Area %in% c("United Arab Emirates", "Norway", "Canada", "Rwanda", "Germany", "China, mainland", "Bangladesh", "Hungary"),]

# Rename China, mainland and United Arab Emirates to China and UAE, respectively
dat_filtered$Area <- gsub("China, mainland", "China", dat_filtered$Area)
dat_filtered$Area <- gsub("United Arab Emirates", "UAE", dat_filtered$Area)

# Reshape the data into a long format
dat_long <- dat_filtered %>% pivot_longer(cols = c("nochangeperc", "largechangeperc"), names_to = "variable", values_to = "value")

# Split the data into two separate data frames
dat_nochange <- dat_long %>% filter(variable == "nochangeperc")
dat_largechange <- dat_long %>% filter(variable == "largechangeperc")


# Remove the first color from the "Blues" palette (it was too light, it wasn't really showing up, but i like the rest of the colors :])
palette <- brewer.pal(n = length(unique(dat_long$Area)) + 1, name = "Blues")[-1]
# Assign a color from the modified palette to each country
country_colors <- setNames(palette, unique(dat_long$Area))


plot1 <- ggplot(dat_nochange, aes(x = Area, y = value, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = country_colors) +
  theme_classic() +
  xlab("Country") +
  ylab("Percentage of total items") +
  ggtitle("No change") +
  theme(plot.title = element_text(hjust = 0, family = "Avenir", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "Avenir"),
        legend.position = "none",
        text = element_text(family = "Avenir", size = 10)) +
  scale_y_continuous(expand = c(0, 0.05), limits = c(0, 1), labels = scales::percent_format()) +
  geom_hline(yintercept = 0.224, linetype = "dashed", color = "black") +
  annotate("text", x = Inf, y = 0.224, label = "mean % (22.4)", hjust = 4.1, vjust = -0.5, family = "Avenir", size = 3, color = "black")

plot2 <- ggplot(dat_largechange, aes(x = Area, y = value, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Country", values = country_colors) +
  theme_classic() +
  xlab("Country") +
  ylab("Percentage of total items") +
  ggtitle("Large change (>3.65kg/capita/yr)") +
  theme(plot.title = element_text(hjust = 0, family = "Avenir", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "Avenir"),
        text = element_text(family = "Avenir", size = 10)) +
  scale_y_continuous(expand = c(0, 0.05), limits = c(0, 1), labels = scales::percent_format()) +
  geom_hline(yintercept = 0.069, linetype = "dashed", color = "black") +
  annotate("text", x = Inf, y = 0.069, label = "mean % (6.9)", hjust =3.5, vjust = -0.5, family = "Avenir", size = 3, color = "black")


# Combine the plots using the patchwork package
plot1 + plot2
plot <- plot1 + plot2


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
#plot
plot <- ggplot(data=na.omit(e), aes(x=Year, y = value, group = variable, color = variable)) +
  #add connecting lines for the datapoints
  geom_line() +
  #make the plot points smaller
  geom_point(shape = 20, alpha = 0.05) +
  #add label to the yaxis
  ylab("Meat supply (g/capita/day)") +
  #set theme and font
  theme_classic(base_family = "Avenir") +
  theme(axis.text.x = element_text(size=10, angle=45, vjust=0.5, family = "Avenir")) +
  theme(axis.text.y = element_text(size=10, family = "Avenir")) +
  #color the lines
  scale_color_manual(name="Dataset",
                     labels=c("Old", "New", "Adjusted"),
                     values=c("#EF3B2C", "#41AE76", "#9ECAE1")) +
  #change axis count intervals
  scale_y_continuous(breaks = breaks_pretty(10)) +
  scale_x_continuous(breaks = breaks_pretty(15))

ggsave("~/Desktop/FAO/Figure 4 - UK meat supply, adjusted and unadjusted 1980-2018.png", plot, width = 10, height = 8, dpi = 300)


















##########################Supplemental figure#########################


# FAO data --------------------------------------------------------------------------------------------------------
comm_bal_1 <- rbind(
  read.csv(file.path("FoodBalanceSheets_E_All_Data.csv")) %>% mutate(Methodology="New"),
  read.csv(file.path("FoodBalanceSheetsHistoric_E_All_Data.csv")) %>% mutate(Methodology="Old")
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












































################POTENTIAL FUTURE PLOTS, BUT NEED SOME WORK###################






## Filter the dataset to include only the desired countries
dat_filtered <- dat_isag0[dat_isag0$Area %in% c("United Arab Emirates", "Norway", "Canada", "Rwanda", "Germany", "China, mainland", "Bangladesh", "Hungary"),]

# Rename China, mainland and United Arab Emirates to China and UAE, respectively
dat_filtered$Area <- gsub("China, mainland", "China", dat_filtered$Area)
dat_filtered$Area <- gsub("United Arab Emirates", "UAE", dat_filtered$Area)

# Reshape the data into a long format
dat_long <- dat_filtered %>% pivot_longer(cols = c("nochangeperc", "largechangeperc"), names_to = "variable", values_to = "value")

# Split the data into two separate data frames
dat_nochange <- dat_long %>% filter(variable == "nochangeperc")
dat_largechange <- dat_long %>% filter(variable == "largechangeperc")

# Create the color palette
palette <- brewer.pal(n = length(unique(dat_long$Area)), name = "Blues")
country_colors <- setNames(palette, unique(dat_long$Area))

plot1 <- ggplot(dat_nochange, aes(x = Area, y = value, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = country_colors) +
  theme_classic() +
  xlab("Country") +
  ylab("Percentage of total items") +
  ggtitle("No change") +
  theme(plot.title = element_text(hjust = 0, family = "Avenir", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "Avenir"),
        legend.position = "none",
        text = element_text(family = "Avenir", size = 10)) +
  scale_y_continuous(expand = c(0, 0.05), limits = c(0, 1), labels = scales::percent_format())

plot2 <- ggplot(dat_largechange, aes(x = Area, y = value, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Country", values = country_colors) +
  theme_classic() +
  xlab("Country") +
  ylab("Percentage of total items") +
  ggtitle("Large change (>3.65kg/capita/yr)") +
  theme(plot.title = element_text(hjust = 0.5, family = "Avenir", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "Avenir"),
        text = element_text(family = "Avenir", size = 10)) +
  scale_y_continuous(expand = c(0, 0.05), limits = c(0, 1), labels = scales::percent_format())

# Combine the plots using the patchwork package
plot1 + plot2



















# Subset the dataset to the specified countries and categories
dat_isag0_sub <- dat_isag0 %>%
  filter(Area %in% c("United Arab Emirates", "Norway", "Canada", "Rwanda", "Germany", "China, mainland") &
           category %in% c("milk - excluding butter", "cereals - excluding beer", "fruits - excluding wine",
                           "sugar & sweeteners", "vegetables", "meat")) %>%
  mutate(Area = recode(Area, "United Arab Emirates" = "UAE", "China, mainland" = "China"))

# Create a new dataset to visualize the boxplots side by side
dat_isag0_melt <- dat_isag0_sub %>%
  pivot_longer(cols = c("oldfoodavg", "newfoodavg"), names_to = "type", values_to = "value")

# Define colors for each country
country_colors <- c("UAE" = "#FF0000", "Norway" = "#FFA500", "Canada" = "#FFFF00",
                    "Rwanda" = "#008000", "Germany" = "#0000FF", "China" = "#4B0082")

# Define light and dark colors for oldfoodavg and newfoodavg
oldfoodavg_colors <- paste0(country_colors, "B3")
newfoodavg_colors <- paste0(country_colors, "66")

# Create the boxplot
plot <- ggplot(dat_isag0_melt, aes(x = Area, y = value, fill = interaction(Area, type))) +
  geom_boxplot(position = position_dodge(0.8)) +
  facet_wrap(~category, scales = "free", ncol = 2) +
  scale_fill_manual(values = c(oldfoodavg_colors, newfoodavg_colors)) +
  labs(x = "Country", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(plot)





















































































##############SANDBOX, come back to this later maybe##########












#comparisons by country (across all item categories)
test <- dat_isag1 %>%
  group_by(Area) %>%
  summarise(
    n = n(),
    meandiff = mean(absdiff, na.rm=T))

mean(dat_isag0$absdiff)
mean(dat_isag1$absdiff)


test <- dat_isag1 %>%
  group_by(Item) %>%
  summarise(
    n = n(),
    meandiff = mean(absdiff, na.rm=T))

cor(dat_isag1$absdiff,dat_isag1$Area)

test <- dat_isag0 %>%
  group_by(Item) %>%
  summarise(
    n = n(),
    meandiff = mean(absdiff, na.rm=T))


test <- dat %>%
  group_by(Area) %>%
  summarise(
    n = n(),
    meandiff = mean(absdiff, na.rm=T))
test <- dat %>%
  group_by(Area) %>%
  summarise(
    n = n(),
    nochange = mean(nochange, na.rm=T))
test <- dat %>%
  group_by(Area) %>%
  summarise(
    n = n(),
    largechange = mean(largechange, na.rm=T))
test <- datcat %>%
  group_by(Area) %>%
  summarise(
    n = n(),
    nochange = mean(nochange, na.rm=T))
test <- datcat %>%
  group_by(Area) %>%
  summarise(
    n = n(),
    largechange = mean(largechange, na.rm=T))

#comparisons by item category (across all countries)
test <- datcat %>%
  group_by(Item) %>%
  summarise(
    n = n(),
    meandiff = mean(absdiff, na.rm=T))
test <- datcat %>%
  group_by(Item) %>%
  summarise(
    n = n(),
    nochange = mean(nochange, na.rm=T))
test <- datcat %>%
  group_by(Item) %>%
  summarise(
    n = n(),
    largechange = mean(largechange, na.rm=T))


filter(datcat, Item == "Milk - Excluding Butter")









