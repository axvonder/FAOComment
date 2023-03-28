#setwd and load packages
setwd("~/Desktop//Data")
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

# Create a blank plot for the x-axis label
label_plot <- ggdraw() + draw_label("Aggregated food categories", size = 10, x = 0.5, hjust = 0.5) +
  theme(plot.margin = margin(t = 0, r = 0, b = 75, l = 0, unit = "pt"))

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




















##################FIGURE 1, BLAND ALTMAN PLOT#################33


blandr.output.text(dat$oldfoodavg, dat$newfoodavg, sig.level=0.95)
blandr.draw( dat$oldfoodavg , dat$newfoodavg)
blandr.draw( dat$oldfoodavg , dat$newfoodavg, plotter="rplot" )
blandr.draw( dat$oldfoodavg , dat$newfoodavg, ciDisplay = FALSE)

vignette.chart <- blandr.draw( dat$oldfoodavg , dat$newfoodavg )
wright.stats <- blandr.statistics( dat$oldfoodavg , dat$newfoodavg )
wright.plot <- blandr.plot.limits (wright.stats )
vignette.chart.2 <- vignette.chart +
  ggplot2::coord_cartesian(xlim=c( wright.plot$x_lower , wright.plot$x_upper ) , ylim=c( wright.plot$y_lower , wright.plot$y_upper ) ) +
  xlab("Mean of new & old method (kg/capita/yr)") +
  ylab("Difference of new & old method (kg/capita/yr)") +
  ggtitle("") +
  theme_classic()
vignette.chart.2

summary(wright.stats)





#from scratch BA plot
ggplot(dat, aes(x = avg, y = diff)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_hline(yintercept = mean(dat$diff), linetype = "dashed", colour = "blue", size = 0.5, alpha = 0.6) +
  geom_hline(yintercept = mean(dat$diff) - (1.96 * sd(dat$diff)), linetype = "dashed", colour = "red", size = 0.5, alpha = 0.6) +
  geom_hline(yintercept = mean(dat$diff) + (1.96 * sd(dat$diff)), linetype = "dashed", colour = "red", size = 0.5, alpha = 0.6) +
  ylab("Difference of new & old method (kg/capita/yr)") +
  xlab("Mean of new & old method (kg/capita/yr)") +
  xlim(0, 375) +
  ylim(-130, 200) +
  theme_classic()


cor.test(dat$oldfoodavg,dat$newfoodavg)


#from scratch BA plot
ggplot(datcat, aes(x = avg, y = diff)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_hline(yintercept = mean(datcat$diff), linetype = "dashed", colour = "blue", size = 0.5, alpha = 0.6) +
  geom_hline(yintercept = mean(datcat$diff) - (1.96 * sd(datcat$diff)), linetype = "dashed", colour = "red", size = 0.5, alpha = 0.6) +
  geom_hline(yintercept = mean(datcat$diff) + (1.96 * sd(datcat$diff)), linetype = "dashed", colour = "red", size = 0.5, alpha = 0.6) +
  ylab("Difference of new & old method (kg/capita/yr)") +
  xlab("Mean of new & old method (kg/capita/yr)") +
  xlim(0, 375) +
  ylim(-130, 200) +
  theme_classic()


cor.test(datcat$oldfoodavg,datcat$newfoodavg)












################ FIGURE 2 - UK MEAT TREND ANALYSIS ##############
#read in data
old <- read.csv('FoodBalanceSheetsHistoric_E_All_Data.csv')
new <- read.csv('FoodBalanceSheets_E_All_Data.csv')
#keep only variables needed for meat analysis
olduk <- old[ , which(names(old) %in% c("Area", "Item.Code", "Item", "Element",
                                        "Y1980", "Y1981", "Y1982", "Y1983",
                                        "Y1984", "Y1985", "Y1986", "Y1987", "Y1988",
                                        "Y1989", "Y1990", "Y1991", "Y1992", "Y1993",
                                        "Y1994", "Y1995", "Y1996", "Y1997", "Y1998",
                                        "Y1999", "Y2000", "Y2001", "Y2002", "Y2003",
                                        "Y2004", "Y2005", "Y2006", "Y2007",
                                        "Y2008", "Y2009", "Y2010", "Y2011", "Y2012",
                                        "Y2013"))]
newuk <- new[ , which(names(new) %in% c("Area", "Item.Code", "Item", "Element",
                                        "Y2010", "Y2011", "Y2012", "Y2013", "Y2014",
                                        "Y2015", "Y2016", "Y2017", "Y2018"))]
#only looking at food supply quantity
olduk1 <- olduk[(olduk$Element == "Food supply quantity (kg/capita/yr)"), ]
newuk1 <- newuk[(newuk$Element == "Food supply quantity (kg/capita/yr)"), ]
#only for uk
olduk2 <- olduk1[(olduk1$Area == "United Kingdom of Great Britain and Northern Ireland"), ]
newuk2 <- newuk1[(newuk1$Area == "United Kingdom of Great Britain and Northern Ireland"), ]
#only for meat
olduk3 <- olduk2[(olduk2$Item == "Meat"), ]
newuk3 <- newuk2[(newuk2$Item == "Meat"), ]
#change name (get ready to combine)
olduk3$Area[olduk3$Area == 'United Kingdom of Great Britain and Northern Ireland'] <- 'UK old'
newuk3$Area[newuk3$Area == 'United Kingdom of Great Britain and Northern Ireland'] <- 'UK new'
#get rid of variables not needed
uko <- olduk3[ , -which(names(olduk3) %in% c("Item.Code", "Item", "Element"))]
ukn <- newuk3[ , -which(names(newuk3) %in% c("Item.Code", "Item", "Element"))]
#change column names
colnames(uko) <- c("Data", "1980", "1981", "1982", "1983",
                   "1984", "1985", "1986", "1987", "1988",
                   "1989", "1990", "1991", "1992", "1993",
                   "1994", "1995", "1996", "1997", "1998",
                   "1999", "2000", "2001", "2002", "2003",
                   "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")
colnames(ukn) <- c("Data", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
#convert from kg/yr to g/d
uko$`1980` <- ((uko$`1980`*1000)/365)
uko$`1981` <- ((uko$`1981`*1000)/365)
uko$`1982` <- ((uko$`1982`*1000)/365)
uko$`1983` <- ((uko$`1983`*1000)/365)
uko$`1984` <- ((uko$`1984`*1000)/365)
uko$`1985` <- ((uko$`1985`*1000)/365)
uko$`1986` <- ((uko$`1986`*1000)/365)
uko$`1987` <- ((uko$`1987`*1000)/365)
uko$`1988` <- ((uko$`1988`*1000)/365)
uko$`1989` <- ((uko$`1989`*1000)/365)
uko$`1990` <- ((uko$`1990`*1000)/365)
uko$`1991` <- ((uko$`1991`*1000)/365)
uko$`1992` <- ((uko$`1992`*1000)/365)
uko$`1993` <- ((uko$`1993`*1000)/365)
uko$`1994` <- ((uko$`1994`*1000)/365)
uko$`1995` <- ((uko$`1995`*1000)/365)
uko$`1996` <- ((uko$`1996`*1000)/365)
uko$`1997` <- ((uko$`1997`*1000)/365)
uko$`1998` <- ((uko$`1998`*1000)/365)
uko$`1999` <- ((uko$`1999`*1000)/365)
uko$`2000` <- ((uko$`2000`*1000)/365)
uko$`2001` <- ((uko$`2001`*1000)/365)
uko$`2002` <- ((uko$`2002`*1000)/365)
uko$`2003` <- ((uko$`2003`*1000)/365)
uko$`2004` <- ((uko$`2004`*1000)/365)
uko$`2005` <- ((uko$`2005`*1000)/365)
uko$`2006` <- ((uko$`2006`*1000)/365)
uko$`2007` <- ((uko$`2007`*1000)/365)
uko$`2008` <- ((uko$`2008`*1000)/365)
uko$`2009` <- ((uko$`2009`*1000)/365)
uko$`2010` <- ((uko$`2010`*1000)/365)
uko$`2011` <- ((uko$`2011`*1000)/365)
uko$`2012` <- ((uko$`2012`*1000)/365)
uko$`2013` <- ((uko$`2013`*1000)/365)
ukn$`2010` <- ((ukn$`2010`*1000)/365)
ukn$`2011` <- ((ukn$`2011`*1000)/365)
ukn$`2012` <- ((ukn$`2012`*1000)/365)
ukn$`2013` <- ((ukn$`2013`*1000)/365)
ukn$`2014` <- ((ukn$`2014`*1000)/365)
ukn$`2015` <- ((ukn$`2015`*1000)/365)
ukn$`2016` <- ((ukn$`2016`*1000)/365)
ukn$`2017` <- ((ukn$`2017`*1000)/365)
ukn$`2018` <- ((ukn$`2018`*1000)/365)
#multiply 'old' data by UK AOR of 0.94493782
adj <- uko
adj$`1980` <- (uko$`1980`*0.94493782)
adj$`1981` <- (uko$`1981`*0.94493782)
adj$`1982` <- (uko$`1982`*0.94493782)
adj$`1983` <- (uko$`1983`*0.94493782)
adj$`1984` <- (uko$`1984`*0.94493782)
adj$`1985` <- (uko$`1985`*0.94493782)
adj$`1986` <- (uko$`1986`*0.94493782)
adj$`1987` <- (uko$`1987`*0.94493782)
adj$`1988` <- (uko$`1988`*0.94493782)
adj$`1989` <- (uko$`1989`*0.94493782)
adj$`1990` <- (uko$`1990`*0.94493782)
adj$`1991` <- (uko$`1991`*0.94493782)
adj$`1992` <- (uko$`1992`*0.94493782)
adj$`1993` <- (uko$`1993`*0.94493782)
adj$`1994` <- (uko$`1994`*0.94493782)
adj$`1995` <- (uko$`1995`*0.94493782)
adj$`1996` <- (uko$`1996`*0.94493782)
adj$`1997` <- (uko$`1997`*0.94493782)
adj$`1998` <- (uko$`1998`*0.94493782)
adj$`1999` <- (uko$`1999`*0.94493782)
adj$`2000` <- (uko$`2000`*0.94493782)
adj$`2001` <- (uko$`2001`*0.94493782)
adj$`2002` <- (uko$`2002`*0.94493782)
adj$`2003` <- (uko$`2003`*0.94493782)
adj$`2004` <- (uko$`2004`*0.94493782)
adj$`2005` <- (uko$`2005`*0.94493782)
adj$`2006` <- (uko$`2006`*0.94493782)
adj$`2007` <- (uko$`2007`*0.94493782)
adj$`2008` <- (uko$`2008`*0.94493782)
adj$`2009` <- (uko$`2009`*0.94493782)
adj$`2010` <- (uko$`2010`*0.94493782)
adj$`2011` <- (uko$`2011`*0.94493782)
adj$`2012` <- (uko$`2012`*0.94493782)
adj$`2013` <- (uko$`2013`*0.94493782)
#add in 'new data'
adj$`2014` <- ukn$`2014`
adj$`2015` <- ukn$`2015`
adj$`2016` <- ukn$`2016`
adj$`2017` <- ukn$`2017`
adj$`2018` <- ukn$`2018`
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
forplot <- ukot
forplot[nrow(forplot) + 5,] <- NA
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
forplot$ukn <- uknt$ukn
forplot$adj <- adjt$adj
colnames(forplot) <- c("Old", "New", "Adjusted")
row.names(forplot) <- c("1980", "1981", "1982", "1983",
                        "1984", "1985", "1986", "1987", "1988",
                        "1989", "1990", "1991", "1992", "1993",
                        "1994", "1995", "1996", "1997", "1998",
                        "1999", "2000", "2001", "2002", "2003",
                        "2004", "2005", "2006", "2007", "2008",
                        "2009", "2010", "2011", "2012", "2013",
                        "2014", "2015", "2016", "2017", "2018")
forplot$Year <- row.names(forplot)
forplot$Old <- as.numeric(forplot$Old)
forplot$New <- as.numeric(forplot$New)
forplot$Adjusted <- as.numeric(forplot$Adjusted)
forplot$Year <- as.numeric(forplot$Year)
d <- melt(forplot, id.vars="Year")
e <- head(d,-5)
#plot
ggplot(data=na.omit(e), aes(x=Year, y = value, group = variable, color = variable)) +
  #add connecting lines for the datapoints
  geom_line() +
  #make the plot points smaller
  geom_point(shape = 20, alpha = 0.05) +
  #add label to the yaxis
  ylab("Meat supply (g/capita/day)") +
  #set theme
  theme_classic() +
  theme(axis.text.x = element_text(size=7, angle=90)) +
  theme(axis.text.y = element_text(size=7)) +
  #color the lines
  scale_color_manual(name="Dataset",
                     labels=c("Old", "New", "Adjusted"),
                     values=c("tomato2", "green4", "skyblue")) +
  #change axis count intervals
  scale_y_continuous(breaks = scales::breaks_pretty(10)) +
  scale_x_continuous(breaks = scales::breaks_pretty(15))






































#read in data
dataX <- "old eggs.csv"
catname <- "eggs"

X <- "old eggs.csv"
Y <- "eggs"

Xform <- function(dataX, catname) {
  old <- read.csv(dataX)
  old <- old[ , which(names(old) %in% c("Area", "Item.Code", "Item", "Year", "Value"))]
  old <- reshape(old, idvar = "Area", timevar = "Year", direction = "wide")
  names(old)[names(old) == 'Value.2010'] <- '2010old'
  names(old)[names(old) == 'Value.2011'] <- '2011old'
  names(old)[names(old) == 'Value.2012'] <- '2012old'
  names(old)[names(old) == 'Value.2013'] <- '2013old'
  names(old)[names(old) == 'Item.2010'] <- 'Item'
  old <- old[ , -which(names(old) %in% c("Item.2011", "Item.2012", "Item.2013"))]
  old$category <- catname
}

Xform(dataX, catname)
Xform("old eggs.csv", "eggs")
Xform(X,Y)



old <- read.csv(dataX)
old <- old[ , which(names(old) %in% c("Area", "Item.Code", "Item", "Year", "Value"))]
old <- reshape(old, idvar = "Area", timevar = "Year", direction = "wide")
names(old)[names(old) == 'Value.2010'] <- '2010old'
names(old)[names(old) == 'Value.2011'] <- '2011old'
names(old)[names(old) == 'Value.2012'] <- '2012old'
names(old)[names(old) == 'Value.2013'] <- '2013old'
names(old)[names(old) == 'Item.2010'] <- 'Item'
old <- old[ , -which(names(old) %in% c("Item.2011", "Item.2012", "Item.2013"))]
old$category <- catname







transf.data <- function (dataX) {
  dataX <- dataX[ , which(names(dataX) %in% c("Area", "Item.Code", "Item", "Element",
                                        "Y2010", "Y2011", "Y2012", "Y2013"))]
  names(dataX)[names(dataX) == 'Y2010'] <- 'Y2010old'
}

transf.data(old)


old <- read.csv('old alcoholic beverages.csv')
old <- old[ , which(names(old) %in% c("Area", "Item.Code", "Item", "Element",
                                       "Y2010", "Y2011", "Y2012", "Y2013"))]
names(old)[names(old) == 'Y2010'] <- 'Y2010old'
old$

old <- read.csv('oldallitemsnoag.csv')
new <- read.csv('newallitemsnoag.csv')












#read in datasets
old <- read.csv('FoodBalanceSheetsHistoric_E_All_Data.csv')
new <- read.csv('FoodBalanceSheets_E_All_Data.csv')
#keep columns needed for comparison
old1 <- old[ , which(names(old) %in% c("Area", "Item.Code", "Item", "Element",
                                       "Y2010", "Y2011", "Y2012", "Y2013"))]
new1 <- new[ , which(names(new) %in% c("Area", "Item.Code", "Item", "Element",
                                       "Y2010", "Y2011", "Y2012", "Y2013"))]
#only looking at food supply quantity
old2 <- old1[(old1$Element == "Food supply quantity (kg/capita/yr)"), c(1:8)]
new2 <- new1[(new1$Element == "Food supply quantity (kg/capita/yr)"), c(1:8)]
#change names of old columns (preparing to be incorporated into 1 dataset)
names(old2)[names(old2) == 'Y2010'] <- 'Y2010old'
names(old2)[names(old2) == 'Y2011'] <- 'Y2011old'
names(old2)[names(old2) == 'Y2012'] <- 'Y2012old'
names(old2)[names(old2) == 'Y2013'] <- 'Y2013old'
#merge datasets
joined <- left_join(new2, old2, by = c("Area", "Item.Code", "Item", "Element"))
#remove all items that do not have a match across datasets
joinedcut <- joined[complete.cases(joined[ , 5:12]),]
dat <- joinedcut
#remove regions, leave only countries
regions <- c("Net Food Importing Developing Countries",
             "Low Income Food Deficit Countries",
             "Small Island Developing States",
             "Land Locked Developing Countries",
             "Least Developed Countries",
             "European Union (27)",
             "Polynesia",
             "Micronesia",
             "Melanesia",
             "Australia and New Zealand",
             "Oceania",
             "Western Europe",
             "Southern Europe",
             "Northern Europe",
             "Eastern Europe",
             "Europe",
             "Western Asia",
             "South-eastern Asia",
             "Southern Asia",
             "Eastern Asia",
             "Central Asia",
             "Asia",
             "South America",
             "Caribbean",
             "Central America",
             "Northern America",
             "Americas",
             "Western Africa",
             "Southern Africa",
             "Northern Africa",
             "Middle Africa",
             "Eastern Africa",
             "Africa",
             "World")
condition <- dat$Area %in% regions
dat$ZZZZ[condition] <- "deleteme"
dat <- subset(dat, is.na(dat$ZZZZ))
dat <- dat[, -13]
#add variables for avg offset ratio per year
dat$Y2010offset <- (dat$Y2010/dat$Y2010old)
dat$Y2011offset <- (dat$Y2011/dat$Y2011old)
dat$Y2012offset <- (dat$Y2012/dat$Y2012old)
dat$Y2013offset <- (dat$Y2013/dat$Y2013old)
#add variable for avg offset ratio across 4 years
dat$avgoffset <- rowMeans(dat[, c("Y2010offset", "Y2011offset", "Y2012offset", "Y2013offset")])
#######COMPUTATIONS###########
#how many items with an avg offset ratio within 5% of 1
table(cut(dat$avgoffset, breaks = seq.int(from = 0.95, to = 1.05, by = 0.1)))
#6704
#remove all items with NA for avg. offset
#NAs caused by 0 values for the items in either 1 or both of the datasets (n = 3,020)
dat1 <- dat[complete.cases(dat[ , 17]),]
#get rid of infinity values for avg. offset (n = 200)
dat2 <- dat1 %>% filter(is.finite(avgoffset))
#get rid of AOR values ==0 (n = 265)
dat3 <- dat2 %>% filter(!avgoffset == 0)
#create 'absolute offset' variable (change relative to 1.00)
dat3 <- dat3 %>%
  mutate(abAOR = case_when(
    avgoffset == 1 ~ 0,
    avgoffset > 1 ~ ((avgoffset - 1)*100),
    avgoffset < 1 ~ (abs((avgoffset -1)*100)),
    TRUE ~ -99
    ))
#how many items with an avg offset ratio within 5% of 1 (n = 6,704)
analysis <- dat3[(dat3$abAOR > 5), ] # n = 8,406
#avg avgoffset value looking at abAOR
mean(dat3$abAOR) #49.27
#median avgoffset value looking at abAOR
median(dat3$abAOR) #7.88
#avg avgoffset value looking at AOR
mean(dat3$avgoffset) #1.278127
#median AOR
median(dat3$avgoffset)
median(abv1$avgoffset)
median(blw1$avgoffset)
#subset data in values >/< 1 (anything with a diversion from 1.00, suggesting a change)
abv1 <- dat3[(dat3$avgoffset > 1), ]
blw1 <- dat3[(dat3$avgoffset < 1), ]
#mean of items >1 - 2.012
mean(abv1$avgoffset)
#mean of items <1 - 0.805
mean(blw1$avgoffset)
#subset data for values ==1 --- 1,447
eq1 <- dat3[(dat3$avgoffset == 1), ]
#calculating sd... first pull out yearly offset columns
finaldat <- dat3[, 14:17]
#calculate sd across the yearly offsets
dat3$sd <- apply(finaldat, 1, sd)
#calculate average cv across the yearly offsets against the AOR
dat3$cv <- (dat3$sd/finaldat$avgoffset)*100
#mean CV across all items (n = 9.34)
mean(dat3$cv, na.rm = TRUE)
#calculate mean CVs by AORs >5% & <5%
test1 <- dat3[ which( finaldat$avgoffset <0.95 | finaldat$avgoffset >1.05), ]
mean(test1$cv, na.rm = TRUE) #13.21
test2 <- dat3[ which( finaldat$avgoffset >0.95 & finaldat$avgoffset <1.05), ]
mean(test2$cv, na.rm = TRUE) #3.31
#write mutated dataset to csv
write.csv(dat3,"~/Desktop/FAO/mutatedFAO.csv", row.names = F)

dat <- read.csv('mutatedFAO.csv')
#create variable that avgs all old dataset values per item
dat$oldfoodavg = ((dat$Y2010old + dat$Y2011old + dat$Y2012old + dat$Y2013old)/4)
#create variable that avgs all new dataset values per item
dat$newfoodavg = ((dat$Y2010 + dat$Y2011 + dat$Y2012 + dat$Y2013)/4)
#creat variable that calculates absolute difference between items
dat$diff <- dat$newfoodavg - dat$oldfoodavg
dat$absdiff <- abs(dat$diff)
dat$avgdiff <- (dat$newfoodavg+dat$oldfoodavg)/2
mean(dat$avgdiff)

test <- dat %>%
  group_by(Area) %>%
  summarise(
    n = n(),
    meandiff = mean(avgdiff, na.rm=T))
test <- test %>%
  arrange(meandiff)

dat <- dat %>%
  mutate(largechange = case_when(
    abs(newfoodavg - oldfoodavg) > 3.65 ~ 1,
    TRUE ~ 0
  ))
table(dat$largechange)
1702/13837 #12.3%
dat <- dat %>%
  mutate(nochange = case_when(
    abs(newfoodavg - oldfoodavg) ==0 ~ 1,
    TRUE ~ 0
  ))
table(dat$nochange)
1411/13837 #10.2%

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

test <- dat %>%
  group_by(Area) %>%
  summarise(
    n = n(),
    meancv = mean(meancv, na.rm=T))
#bland altman plot
#y = difference of values (newfoodavg - oldfoodavg); "diff"
#x = average of values ((newfoodavg + oldfoodavg)/2); "avgdiff"

blandr.output.text(dat$diff, dat$avgdiff, sig.level=0.95)
blandr.draw( dat$diff , dat$avgdiff)
blandr.draw( dat$diff , dat$avgdiff, plotter="rplot" )
blandr.draw( dat$diff , dat$avgdiff, ciDisplay = FALSE)

vignette.chart <- blandr.draw( dat$diff , dat$avgdiff )
vignette.chart <- vignette.chart + annotate("text", label= "example text")
vignette.chart


wright.stats <- blandr.statistics( dat$diff , dat$avgdiff )
wright.plot <- blandr.plot.limits (wright.stats )
vignette.chart.2 <- vignette.chart +
  ggplot2::coord_cartesian(xlim=c( wright.plot$x_lower , wright.plot$x_upper ) , ylim=c( wright.plot$y_lower , wright.plot$y_upper ) ) +
  xlab("Mean of new & old method (kg/capita/yr)") +
  ylab("Difference of new & old method (kg/capita/yr)") +
  ggtitle("") +
  theme_classic()
vignette.chart.2


summary(wright.stats)

#























#print out average CV & AOR values by item and by country
testAreaAOR <- dat3 %>%
  group_by(Area) %>%
  dplyr::summarize(Mean = mean(abAOR, na.rm=TRUE))
testAreaCV <- dat3 %>%
  group_by(Area) %>%
  dplyr::summarize(Mean = mean(cv, na.rm=TRUE))
testItemAOR <- dat3 %>%
  group_by(Item) %>%
  dplyr::summarize(Mean = mean(abAOR, na.rm=TRUE))
testItemCV <- dat3 %>%
  group_by(Item) %>%
  dplyr::summarize(Mean = mean(cv, na.rm=TRUE))
write.csv(testAreaAOR, 'CountryAOR.csv', row.names = FALSE)
write.csv(testAreaCV, 'CountryCV.csv', row.names = FALSE)
write.csv(testItemAOR, 'ItemAOR.csv', row.names = FALSE)
write.csv(testItemCV, 'ItemCV.csv', row.names = FALSE)



ggplot(dat, aes(oldfoodavg,avgoffset)) +
  geom_point() 
  


##################### COUNTRY ANALYSIS ###################

dat <- read.csv('mutatedFAO.csv')
#create variable that avgs all old dataset values per item
dat$oldfoodavg = ((dat$Y2010old + dat$Y2011old + dat$Y2012old + dat$Y2013old)/4)
#create variable that avgs all new dataset values per item
dat$newfoodavg = ((dat$Y2010 + dat$Y2011 + dat$Y2012 + dat$Y2013)/4)
#log plot of avg food weight vs AOR (old method)
ggplot(dat, aes(x= oldfoodavg, y=avgoffset, size=oldfoodavg)) + #size=population (when merged in)
  geom_point(alpha=0.2)+ #scale_y_log10() +
  scale_size_continuous(range = c(.1,4)) +
  xlab("Average item weight in old dataset") +
  ylab("Difference between old and new values") +
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=12, face="bold"),
        axis.title.x=element_text(size=12, face="bold")) +
  scale_fill_continuous(name="Population") +
  theme_bw()

#log plot of avg food weight vs AOR (new method)
ggplot(dat, aes(x= newfoodavg, y=avgoffset, size=newfoodavg)) + geom_point(alpha=0.2)+ scale_y_log10() + scale_size_continuous(range = c(.1,4))
#number of avg changes >10g
dat <- dat %>%
  mutate(largechange = case_when(
    abs(newfoodavg - oldfoodavg) > 3.65 ~ 1,
    TRUE ~ 0
  ))
table(dat$largechange)
#number of any changes >10g/day per item
dat <- dat %>%
  mutate(anylargechange = case_when(
    abs(Y2010 - Y2010old) > 3.65 ~ 1,
    abs(Y2011 - Y2011old) > 3.65 ~ 1,
    abs(Y2012 - Y2012old) > 3.65 ~ 1,
    abs(Y2013 - Y2013old) > 3.65 ~ 1,
    TRUE ~ 0
  ))
table(dat$anylargechange)
#count number of items where a >10g change occurred per country
dat <- dat %>%
  #grouping by item and Country
  group_by(Area) %>%
  # A handy bit of code that lets you count observations within group (specified above with group_by)
  mutate(n_items=ave(1:length(Area), Area, FUN = seq_along)) %>%
  #new variable which is cumulative sum of the trick variable created above
  mutate(n_bigchange=sum(anylargechange)) %>%
  ungroup()
#count total number of items
test <- dat %>% group_by(Area) %>%
  summarise(n_items = max(n_items)) %>%
  ungroup()
names(test)[names(test) == 'n_items'] <- 'totalitems'
#merge
final <- merge(dat,test, by = "Area")
#calculate % of items that have a big change
final$percentbigchange <- (final$n_bigchange/final$totalitems)*100

meat <- final[(final$Item == "Meat"), ]
table(meat$anylargechange)


pop <- new[ , which(names(new) %in% c("Area", "Element", "Y2019"))]
pop <- pop[(pop$Element == "Total Population - Both sexes"), ]
pop <- pop[ , c(1,3)]
names(pop)[names(pop) == 'Y2019'] <- 'population'

graph <- meat[(meat$Area == "India" |
                 meat$Area == "Chile" |
                 meat$Area == "Rwanda" |
                 meat$Area == "United States of America" |
                 meat$Area == "United Kingdom of Great Britain and Northern Ireland" |
                 meat$Area == "Colombia" |
                 meat$Area == "Netherlands" |
                 meat$Area == "Albania" |
                 meat$Area == "Mongolia" |
                 meat$Area == "Fiji" |
                 meat$Area == "Pakistan" |
                 meat$Area == "Gambia"), ]

graph <- merge(graph, pop, by = "Area")
graph$region <- c("Europe", "Americas", "Americas", "Asia Pacific", "Middle East/Africa",
                  "Asia Pacific", "Asia Pacific", "Europe", "Middle East/Africa",
                  "Middle East/Africa", "Europe", "Americas")

zz <- merge(final, pop, by = "Area")

m1 <- lm(population ~ percentbigchange, data = zz)
summary(m1)
cor.test(zz$population, zz$percentbigchange, method = "pearson")
cor.test(graph$population, graph$percentbigchange, method = "pearson")
cor.test(zz$newfoodavg, zz$abAOR, method = "pearson")

ggplot(graph, aes(x = Area, y = percentbigchange, size = population, color = Area, group = region)) + geom_point(alpha = 0.5) +  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

print <- zz[(zz$Item == "Meat"), ]
write.csv(print, 'worlddata.csv', row.names = FALSE)


###################### WORLD MAP #####################

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = zz) +
  geom_sf()




################ UK MEAT ANALYSIS ##############

olduk <- old[ , which(names(old) %in% c("Area", "Item.Code", "Item", "Element",
                                        "Y1980", "Y1981", "Y1982", "Y1983",
                                        "Y1984", "Y1985", "Y1986", "Y1987", "Y1988",
                                        "Y1989", "Y1990", "Y1991", "Y1992", "Y1993",
                                        "Y1994", "Y1995", "Y1996", "Y1997", "Y1998",
                                        "Y1999", "Y2000", "Y2001", "Y2002", "Y2003",
                                        "Y2004", "Y2005", "Y2006", "Y2007",
                                        "Y2008", "Y2009", "Y2010", "Y2011", "Y2012",
                                        "Y2013"))]
newuk <- new[ , which(names(new) %in% c("Area", "Item.Code", "Item", "Element",
                                       "Y2010", "Y2011", "Y2012", "Y2013", "Y2014",
                                       "Y2015", "Y2016", "Y2017", "Y2018"))]
#only looking at food supply quantity
olduk1 <- olduk[(olduk$Element == "Food supply quantity (kg/capita/yr)"), ]
newuk1 <- newuk[(newuk$Element == "Food supply quantity (kg/capita/yr)"), ]
#only for uk
olduk2 <- olduk1[(olduk1$Area == "United Kingdom of Great Britain and Northern Ireland"), ]
newuk2 <- newuk1[(newuk1$Area == "United Kingdom of Great Britain and Northern Ireland"), ]
#only for meat
olduk3 <- olduk2[(olduk2$Item == "Meat"), ]
newuk3 <- newuk2[(newuk2$Item == "Meat"), ]
#change name (get ready to combine)
olduk3$Area[olduk3$Area == 'United Kingdom of Great Britain and Northern Ireland'] <- 'UK old'
newuk3$Area[newuk3$Area == 'United Kingdom of Great Britain and Northern Ireland'] <- 'UK new'
#get rid of variables not needed
uko <- olduk3[ , -which(names(olduk3) %in% c("Item.Code", "Item", "Element"))]
ukn <- newuk3[ , -which(names(newuk3) %in% c("Item.Code", "Item", "Element"))]
#change column names
colnames(uko) <- c("Data", "1980", "1981", "1982", "1983",
                   "1984", "1985", "1986", "1987", "1988",
                   "1989", "1990", "1991", "1992", "1993",
                   "1994", "1995", "1996", "1997", "1998",
                   "1999", "2000", "2001", "2002", "2003",
                   "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")
colnames(ukn) <- c("Data", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
#convert from kg/yr to g/d
uko$`1980` <- ((uko$`1980`*1000)/365)
uko$`1981` <- ((uko$`1981`*1000)/365)
uko$`1982` <- ((uko$`1982`*1000)/365)
uko$`1983` <- ((uko$`1983`*1000)/365)
uko$`1984` <- ((uko$`1984`*1000)/365)
uko$`1985` <- ((uko$`1985`*1000)/365)
uko$`1986` <- ((uko$`1986`*1000)/365)
uko$`1987` <- ((uko$`1987`*1000)/365)
uko$`1988` <- ((uko$`1988`*1000)/365)
uko$`1989` <- ((uko$`1989`*1000)/365)
uko$`1990` <- ((uko$`1990`*1000)/365)
uko$`1991` <- ((uko$`1991`*1000)/365)
uko$`1992` <- ((uko$`1992`*1000)/365)
uko$`1993` <- ((uko$`1993`*1000)/365)
uko$`1994` <- ((uko$`1994`*1000)/365)
uko$`1995` <- ((uko$`1995`*1000)/365)
uko$`1996` <- ((uko$`1996`*1000)/365)
uko$`1997` <- ((uko$`1997`*1000)/365)
uko$`1998` <- ((uko$`1998`*1000)/365)
uko$`1999` <- ((uko$`1999`*1000)/365)
uko$`2000` <- ((uko$`2000`*1000)/365)
uko$`2001` <- ((uko$`2001`*1000)/365)
uko$`2002` <- ((uko$`2002`*1000)/365)
uko$`2003` <- ((uko$`2003`*1000)/365)
uko$`2004` <- ((uko$`2004`*1000)/365)
uko$`2005` <- ((uko$`2005`*1000)/365)
uko$`2006` <- ((uko$`2006`*1000)/365)
uko$`2007` <- ((uko$`2007`*1000)/365)
uko$`2008` <- ((uko$`2008`*1000)/365)
uko$`2009` <- ((uko$`2009`*1000)/365)
uko$`2010` <- ((uko$`2010`*1000)/365)
uko$`2011` <- ((uko$`2011`*1000)/365)
uko$`2012` <- ((uko$`2012`*1000)/365)
uko$`2013` <- ((uko$`2013`*1000)/365)
ukn$`2010` <- ((ukn$`2010`*1000)/365)
ukn$`2011` <- ((ukn$`2011`*1000)/365)
ukn$`2012` <- ((ukn$`2012`*1000)/365)
ukn$`2013` <- ((ukn$`2013`*1000)/365)
ukn$`2014` <- ((ukn$`2014`*1000)/365)
ukn$`2015` <- ((ukn$`2015`*1000)/365)
ukn$`2016` <- ((ukn$`2016`*1000)/365)
ukn$`2017` <- ((ukn$`2017`*1000)/365)
ukn$`2018` <- ((ukn$`2018`*1000)/365)
#multiply 'old' data by UK AOR of 0.94493782
adj <- uko
adj$`1980` <- (uko$`1980`*0.94493782)
adj$`1981` <- (uko$`1981`*0.94493782)
adj$`1982` <- (uko$`1982`*0.94493782)
adj$`1983` <- (uko$`1983`*0.94493782)
adj$`1984` <- (uko$`1984`*0.94493782)
adj$`1985` <- (uko$`1985`*0.94493782)
adj$`1986` <- (uko$`1986`*0.94493782)
adj$`1987` <- (uko$`1987`*0.94493782)
adj$`1988` <- (uko$`1988`*0.94493782)
adj$`1989` <- (uko$`1989`*0.94493782)
adj$`1990` <- (uko$`1990`*0.94493782)
adj$`1991` <- (uko$`1991`*0.94493782)
adj$`1992` <- (uko$`1992`*0.94493782)
adj$`1993` <- (uko$`1993`*0.94493782)
adj$`1994` <- (uko$`1994`*0.94493782)
adj$`1995` <- (uko$`1995`*0.94493782)
adj$`1996` <- (uko$`1996`*0.94493782)
adj$`1997` <- (uko$`1997`*0.94493782)
adj$`1998` <- (uko$`1998`*0.94493782)
adj$`1999` <- (uko$`1999`*0.94493782)
adj$`2000` <- (uko$`2000`*0.94493782)
adj$`2001` <- (uko$`2001`*0.94493782)
adj$`2002` <- (uko$`2002`*0.94493782)
adj$`2003` <- (uko$`2003`*0.94493782)
adj$`2004` <- (uko$`2004`*0.94493782)
adj$`2005` <- (uko$`2005`*0.94493782)
adj$`2006` <- (uko$`2006`*0.94493782)
adj$`2007` <- (uko$`2007`*0.94493782)
adj$`2008` <- (uko$`2008`*0.94493782)
adj$`2009` <- (uko$`2009`*0.94493782)
adj$`2010` <- (uko$`2010`*0.94493782)
adj$`2011` <- (uko$`2011`*0.94493782)
adj$`2012` <- (uko$`2012`*0.94493782)
adj$`2013` <- (uko$`2013`*0.94493782)
#add in 'new data'
adj$`2014` <- ukn$`2014`
adj$`2015` <- ukn$`2015`
adj$`2016` <- ukn$`2016`
adj$`2017` <- ukn$`2017`
adj$`2018` <- ukn$`2018`
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
forplot <- ukot
forplot[nrow(forplot) + 5,] <- NA
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
uknt <- rbind(c(NA), uknt)
forplot$ukn <- uknt$ukn
forplot$adj <- adjt$adj
colnames(forplot) <- c("Old", "New", "Adjusted")
row.names(forplot) <- c("1980", "1981", "1982", "1983",
                        "1984", "1985", "1986", "1987", "1988",
                        "1989", "1990", "1991", "1992", "1993",
                        "1994", "1995", "1996", "1997", "1998",
                        "1999", "2000", "2001", "2002", "2003",
                        "2004", "2005", "2006", "2007", "2008",
                        "2009", "2010", "2011", "2012", "2013",
                        "2014", "2015", "2016", "2017", "2018")
forplot$Year <- row.names(forplot)

forplot$Old <- as.numeric(forplot$Old)
forplot$New <- as.numeric(forplot$New)
forplot$Adjusted <- as.numeric(forplot$Adjusted)
forplot$Year <- as.numeric(forplot$Year)



d <- melt(forplot, id.vars="Year")
e <- head(d,-5)



ggplot(data=na.omit(e), aes(x=Year, y = value, group = variable, color = variable)) +
  geom_line() +
  geom_point(shape = 20, alpha = 0.05) +
  ylab("Meat supply (g/capita/day)") +
  theme_classic() +
  theme(axis.text.x = element_text(size=7, angle=90)) +
  theme(axis.text.y = element_text(size=7)) +
  scale_color_manual(name="Dataset",
                     labels=c("Old", "New", "Adjusted"),
                     values=c("tomato2", "green4", "skyblue")) +
  scale_y_continuous(breaks = scales::breaks_pretty(10)) +
  scale_x_continuous(breaks = scales::breaks_pretty(15))









