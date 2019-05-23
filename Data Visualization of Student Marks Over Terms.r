######################################################################################
#title: "Data Visualization of Student Marks Over Terms"
#author: "Dorcas"
#################################################################################


#clearing everything 
rm(list=ls())

library(dplyr)
library(ggplot2)
library(gganimate)
library(RColorBrewer)
library(png)
library(gifski)
library(knitr)
library(animation)



#Loading the data(in long format)
#data<-read_excel("C:\\Users\\Dorcas\\Documents\\CcHUB Education\\relearn\\Raw Data\\longdata.xlsx")
data<-read.csv("edit path")
data$term<-as.integer(data$term)
str(data)


#generating colors
n=21 #no specific reason for choosing 21. it is purely ramdom. This is the number of palettes you would want in the palette. Minimum is usually 3
colors_pal<-brewer.pal.info[brewer.pal.info$category=='qual',] #choosing the type of color pallete (name)
col_vector = unlist(mapply(brewer.pal, colors_pal$maxcolors, rownames(colors_pal))) #creatingthe colors vector


#Checking for missing values
sapply(data, function(x) sum(is.na(x)))


#calculating average marks for each term first overall, then for each treatment and each subject 
average_marks_overall<-data %>% group_by(term) %>% summarise(Performance = mean(marks, na.rm = T)) %>% as.data.frame()

average_marks_group<-data %>% group_by(term, group) %>% summarise(Performance = mean(marks, na.rm = T)) %>% as.data.frame()

average_marks_control<-data %>% subset(group=="control") %>% group_by(term, group) %>% summarise(Performance = mean(marks, na.rm = T)) %>% as.data.frame()

average_marks_treatment<-data %>% subset(group=="treatment") %>% group_by(term, group) %>% summarise(Performance = mean(marks, na.rm = T)) %>% as.data.frame()

average_marks_by_subject<-data %>% group_by(term,modules) %>% summarise(Performance = mean(marks, na.rm = T)) %>% as.data.frame()


#Plotting term-wise performance for treatment
#treatment_plot
ggplot(average_marks_group, aes(group,Performance,fill = group)) + 
   geom_bar(stat = "identity", width = 0.5) + theme_classic() + coord_flip() + 
   labs(title = 'Term: {frame_time}', x = 'Group', y = 'Average Marks') + transition_time(term) + 
   ggtitle("Overall Student Performance Over the Terms") + 
   theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
   scale_fill_manual(values = sample(col_vector, n))


#plotting term-wise performance per subject

#treatment_plot
ggplot(average_marks_by_subject, aes(modules,Performance,fill = modules)) + 
   geom_bar(stat = "identity", width = 0.5) + theme_classic() + coord_flip() + 
   labs(title = 'Term: {frame_time}', x = 'Subject', y = 'Average Marks') + transition_time(term) + 
   ggtitle("Overall Student Performance Over the Terms") + 
   theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
   scale_fill_manual(values = sample(col_vector, n))


# trying a different format of a plot

p <- ggplot(average_marks_overall, aes(as.factor(term), Performance,fill = Performance)) +
   geom_col() + scale_fill_distiller(palette = "RdPu", direction = 1) + 
   ggtitle("Overall Marks over the Year") + labs(x="Term",y="Average Marks") + 
   theme_minimal() +
   theme(
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "white"),
      panel.ontop = TRUE, plot.title = element_text(hjust = 0.5), 
      axis.text.x = element_text(angle=90))

p + transition_states(term, wrap = FALSE) + shadow_mark()






















