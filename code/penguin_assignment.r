#Set working directory to assignment directory containing code and figure files!

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
penguins_clean_filtered<-remove_empty_culmen_length(penguins_clean) %>%
  remove_empty_sex() %>%
  remove_empty_island() %>%
  filter(sex == "MALE") %>%
  filter(island == "Biscoe")
  
# save filtered data
write.csv(penguins_clean_filtered, paste0("data_clean/penguins_clean_filtered.csv"))

# calculate mean of each species
mean_adelie<-mean(subset(penguins_clean_filtered, species == "Adelie Penguin (Pygoscelis adeliae)")$culmen_length_mm)
mean_gentoo<-mean(subset(penguins_clean_filtered, species == "Gentoo penguin (Pygoscelis papua)")$culmen_length_mm)


# plot histogram(s)
plot_culmen_length_figure(penguins_clean_filtered)

#save figure
save_plot_svg(penguins_clean_filtered, "figures/fig01_vector.svg", size=22, scaling=1)
save_plot_png(penguins_clean_filtered, "figures/fig01_png.png", size=22, res=600, scaling=1)

## Statistical test

# Must first test assumptions of two-sample T-test:
# 1) Normal distribution of both groups
par(mfrow = c(1, 2))
qqnorm(subset(penguins_clean_filtered, species == "Adelie Penguin (Pygoscelis adeliae)")$culmen_length_mm)
qqline(subset(penguins_clean_filtered, species == "Adelie Penguin (Pygoscelis adeliae)")$culmen_length_mm)

qqnorm(subset(penguins_clean_filtered, species == "Gentoo penguin (Pygoscelis papua)")$culmen_length_mm)
qqline(subset(penguins_clean_filtered, species == "Gentoo penguin (Pygoscelis papua)")$culmen_length_mm)
# Adelie seems normally distributed, but Gentoos seem to be right-skewed

# transform data
transformed_data<-data.frame(penguins_clean_filtered, log_culmen_length= log(penguins_clean_filtered$culmen_length_mm))
write.csv(transformed_data,paste0("data_clean/transformed_data.csv"))

# check normality of transformed data
par(mfrow = c(1, 2))
qqnorm(subset(cleaned_transformed_data, species == "Adelie Penguin (Pygoscelis adeliae)")$log_culmen_length)
qqline(subset(cleaned_transformed_data, species == "Adelie Penguin (Pygoscelis adeliae)")$log_culmen_length)

qqnorm(subset(cleaned_transformed_data, species == "Gentoo penguin (Pygoscelis papua)")$log_culmen_length)
qqline(subset(cleaned_transformed_data, species == "Gentoo penguin (Pygoscelis papua)")$log_culmen_length)
# transformed Gentoo data still not normally distributed, must use non-parametric test

## Mann-Whitney U/ Wilcoxon Test
wilcox.test(culmen_length_mm~species, data = penguins_clean_filtered)
# p-value = 6.549e-12, means are significantly different from each other
# Gentoos have a greater mean bill length than Adelie penguins


