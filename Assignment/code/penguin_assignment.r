#Set working directory to OWN general directory containing data, code, and figure files!
setwd("C:/Users/Tay/OneDrive - Nexus365/Year 3/Computing skills/Representing data/Assignment")

#load libraries and functions
source("code/libraries.r")
source("code/functions.r")

#load penguin data
write.csv(penguins_raw,paste0("data_raw/penguins_raw.csv"))
penguins_raw<-read.csv("data_raw/penguins_raw.csv")

#clean data and save as .csv file
penguins_clean<-cleaning(penguins_raw)
write.csv(penguins_clean, "data_clean/penguins_clean.csv")

## QUESTION:
## whether male Gentoo and male Adelie penguins from Biscoe Island differ in bill length

# remove NA values form key variables, and reduce data frame to key variables
# e.g. species, island, sex, culmen length
cleaned_na_data<-remove_empty_culmen_length(penguins_clean) %>%
  remove_empty_sex() %>%
  remove_empty_island()
  
# filter data to only contain males
cleaned_male_data<-filter(cleaned_na_data, sex == "MALE")
cleaned_island_data<-filter(cleaned_male_data, island == "Biscoe")
# save filtered data frame
write.csv(cleaned_island_data, paste0("data_clean/filtered_data.csv"))

# calculate mean of each species
adelie<-filter(cleaned_island_data, species=="Adelie Penguin (Pygoscelis adeliae)")
mean_adelie<-mean(adelie$culmen_length_mm)

gentoo<-filter(cleaned_island_data, species=="Gentoo penguin (Pygoscelis papua)")
mean_gentoo<-mean(gentoo$culmen_length_mm)


# plot histogram(s)
plot_culmen_length_figure(cleaned_island_data)

#save figure
save_plot_svg(cleaned_island_data, "figures/fig01_vector.svg", size=22, scaling=1)
save_plot_png(cleaned_island_data, "figures/fig01_png.png", size=22, res=600, scaling=1)

## Statistical test

# Must first test assumptions of two-sample T-test:
# 1) Normal distribution of both groups
par(mfrow = c(1, 2))
qqnorm(subset(cleaned_island_data, species == "Adelie Penguin (Pygoscelis adeliae)")$culmen_length_mm)
qqline(subset(cleaned_island_data, species == "Adelie Penguin (Pygoscelis adeliae)")$culmen_length_mm)

qqnorm(subset(cleaned_island_data, species == "Gentoo penguin (Pygoscelis papua)")$culmen_length_mm)
qqline(subset(cleaned_island_data, species == "Gentoo penguin (Pygoscelis papua)")$culmen_length_mm)
# Adelie seems normally distributed, but Gentoos seem to be right-skewed

# transform data
cleaned_transformed_data<-data.frame(cleaned_island_data, log_culmen_length= log(cleaned_island_data$culmen_length_mm))
write.csv(cleaned_transformed_data,paste0("data_clean/cleaned_transformed_data.csv"))

# check normality of transformed data
par(mfrow = c(1, 2))
qqnorm(subset(cleaned_transformed_data, species == "Adelie Penguin (Pygoscelis adeliae)")$log_culmen_length)
qqline(subset(cleaned_transformed_data, species == "Adelie Penguin (Pygoscelis adeliae)")$log_culmen_length)

qqnorm(subset(cleaned_transformed_data, species == "Gentoo penguin (Pygoscelis papua)")$log_culmen_length)
qqline(subset(cleaned_transformed_data, species == "Gentoo penguin (Pygoscelis papua)")$log_culmen_length)
# transformed Gentoo data still not normally distributed, must use non-parametric test

## Mann-Whitney U/ Wilcoxon Test
wilcox.test(culmen_length_mm~species, data = cleaned_island_data)
# p-value = 6.549e-12, means are significantly different from each other
# Gentoos have a greater mean bill length than Adelie penguins


