cleaning<-function(penguins_raw){
  penguins_raw %>%
    clean_names() %>%
    remove_empty(c("rows","cols"))%>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

remove_empty_culmen_length <- function(data_clean){
  data_clean %>%
    filter(!is.na(culmen_length_mm)) %>%
    select(species, island, sex, culmen_length_mm)
}

remove_empty_sex <- function(data_clean){
  data_clean %>%
    filter(!is.na(sex))%>%
    select(species, island, sex, culmen_length_mm)
}

remove_empty_island <- function(data_clean){
  data_clean %>%
    filter(!is.na(island))%>%
    select(species, island, sex, culmen_length_mm)
}

plot_culmen_length_figure <- function(cleaned_island_data){
    ggplot(cleaned_island_data,aes(x=culmen_length_mm, fill=species)) + 
    geom_histogram(position = "identity", alpha = 0.5, bins = 25, col="white")+
    scale_fill_manual(values=c("Adelie Penguin (Pygoscelis adeliae)"= "#07004D", 
                               "Gentoo penguin (Pygoscelis papua)" = "#9E1946"))+ #manually setting colours
    scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14))+ #manually setting y-axis scale
    labs(title ="Culmen Lengths of Male Adelie and Gentoo Penguins on Biscoe Island", 
         x= "Culmen Length (mm)", y="Count", fill= "Species", 
         caption = "(Figure 1) Histogram showing distribution of culmen lengths, with species' means plotted as dotted lines.")+ #changing labels
    geom_vline(xintercept=mean_adelie, color="black", linetype="dashed", size=1)+ #adding dotted line for Adelie mean 
    annotate("text", x=42.4, y=14, label="P. adeliae", fontface = "italic")+ # labelling Adelie line with name
    annotate("text", x=41.9, y=-0.4, label=round(mean_adelie,3))+ # labelling Adelie line with mean
    geom_vline(xintercept=mean_gentoo, color="black", linetype="dashed", size=1.25)+ #adding dotted line for Gentoo mean
    annotate("text", x=51.2, y=14, label="P. papua", fontface = "italic")+ # labelling Gentoo line with name
    annotate("text", x=50.8, y=-0.4, label=round(mean_gentoo,3))+ # labelling Gentoo line with mean
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)) #centering title and caption text
}

save_plot_png <- function(data, filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  figure <- plot_culmen_length_figure(data)
  print(figure)
  dev.off()
}

save_plot_svg <- function(data, filename, size, scaling){
  size_inches=size/2.54
  svglite(filename, width   =  size_inches, 
          height  =  size_inches,
          scaling =  scaling)
  figure <- plot_culmen_length_figure(data)
  print(figure)
  dev.off()
}
