
############# Install Packages & Libraries #################
install.packages(c(
  "readxl", "colorBlindness","corrplot", "ggmap", "gt","gtsummary", "directlabels", "dplyr", "ggforce", "gghighlight", 
  "ggnewscale", "ggplot2", "GGally", "gganimate", "ggraph", "ggrepel", "gridExtra", "ggtext", "ggthemes", 
  "hexbin", "tidyverse", "Hmisc", "mapproj", "maps", "munsell", "ozmaps", 
  "paletteer", "patchwork", "rmapshaper", "scico", "seriation", "sf", 
  "stars", "tidygraph", "tidyr", "wesanderson", "VIM", "summarytools" 
)) #found some of the libraries on resources from canvas posted by the professor
library(ggmap)
library(ggmap)
library(corrplot)
library(Hmisc)
library(GGally)
library(ggpubr)
library(gganimate)
library(gridExtra)  
library(forcats)
library(gt)
library(gtsummary)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(VIM)
library(tidyverse)
library(summarytools)
library(plotly)
library(scales)
library(reshape2)
library(ozmaps)

############################################################


#############################################################
########## Import dataset ############
#############################################################
vehicle.data<- read_excel("failure_by_vehicle_make_model_age_report1_2021.xlsx")
vehicle.data.copy<- read_excel("failure_by_vehicle_make_model_age_report1_2021.xlsx")

#############################################################
########### Dataset Summary ############
#############################################################
View(vehicle.data)
head(vehicle.data)
dfSummary(vehicle.data, 
          graph.magnif = 0.75, 
          na.col = FALSE, 
          varnumbers = FALSE,  
          valid.col = FALSE,  
          report.title = "Summary of the Vehicle Road worthiness test dataset") 

#############################################################
##### Dataset Summary in table using gt() table function ####
#############################################################
vehicle.data.summary<- read_excel("Dataset summary.xlsx")
View(vehicle.data.summary)

table.vehicle.data.summary<-
  gt(vehicle.data.summary)%>%
  tab_options(
    column_labels.background.color = "#d0d0d0")

table.vehicle.data.summary
table.vehicle.data.summary %>% gtsave("table_sum.png", expand = 10) #to save table

is.na(vehicle.data) #no missing value

#############################################################
########### Visualisation #######################
#############################################################

#Here I combine first 3 columns into a single column and add to dataframe
vehicle.data$vehicle<- paste(vehicle.data$`Vehicle Make`,vehicle.data$`Vehicle Model`,
                             vehicle.data$`Year Of Birth`,sep = "-")

vehicle.data <- vehicle.data %>% relocate(vehicle, .before = `Vehicle Make`)

#extracting first five observations
first_5_rows<- head(vehicle.data, n=5L )
View(first_5_rows)

first_5_rows<- first_5_rows[c(1,2,3,4,5)] #Subseting first 4 columns for presentation 

#table for presentation
f5_rows<- gt(first_5_rows)%>%
  tab_options(
  column_labels.background.color = "#d0d0d0")

f5_rows %>% gtsave("table_f5.png", expand = 10) #to save table
 
#fetching total number of cars tested
total_no_of_cars_tested<- sum(vehicle.data$Total)
total_no_of_cars_tested

#convert to factor
vehicle.data$`Vehicle Make` <- as.factor(vehicle.data$`Vehicle Make`)

#count of levels in vehicle make
level_count <- nlevels(vehicle.data$`Vehicle Make`)
level_count


#Filtering vehicle-make by sum total 
df_vehicle<-
  vehicle.data %>%
  group_by(`Vehicle Make`) %>%
  summarise(Total = sum(Total, na.rm=TRUE))
View(df_vehicle)  

#barplot of Vehicle make vs Total car tested
vehicle.data_bar<-
  df_vehicle %>%
  mutate(`Vehicle Make` = fct_reorder(`Vehicle Make`,Total)) %>%
  ggplot(aes(x=`Vehicle Make`, y=Total)) +
  geom_bar(stat="identity", fill="#63d0de",width=0.3) +
  coord_flip()+
  geom_text(aes(label=Total), hjust=-0.05)+
  ylab("Total Vehicles Tested")+ xlab("")

vehicle.data_bar

#for interactive plot
vehicle.data_bar %>% ggplotly  


#Filtering all unique cars with 100% pass rates
top.car.pass_Rate<- 
  vehicle.data %>% top_n(5, `PASS %`)

View(top.car.pass_Rate)

#How many cars have 100% pass rates
nrow(top.car.pass_Rate)

#subset the vehicle full name and Total tested 
top.car.pass_Rate_Total_tested<- select(top.car.pass_Rate, vehicle, Total, `Vehicle Make`)

#bar chat of cars with 100% pass rates by car Toyota brand/model
ggplot(top.car.pass_Rate_Total_tested, 
       aes(fill=`Vehicle Make`, y=Total, x=vehicle)) + 
  geom_bar(stat="identity", width = 0.6)+ coord_flip()+
  geom_text(aes(label=Total), hjust=-1)+ ylim(0,35)+
  ylab("No. of Vehicles Tested")+ xlab("")+
  ggtitle(label = "Vehicles with 100% pass rate vs quantity tested in Ireland (2021)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 9, face = "bold"))

#Filtering top 5 tested vehicle make/brand with Dplyr
top5.vehicles <-
  df_vehicle %>%
  top_n(5, Total)

#numbers of vehicle make/brand
length(unique(vehicle.data$`Vehicle Make`))

#Barplot of Top 5 tested vehicle make/brand
top5_plot<-
  top5.vehicles %>%
  mutate(`Vehicle Make` = fct_reorder(`Vehicle Make`,Total)) %>%
  ggplot(aes(x=`Vehicle Make`, y=Total)) +
  geom_bar(stat="identity", fill="#63d0de",width=0.3) +
  geom_text(aes(label=Total), vjust=-0.5)+
  ylab("No. of Vehicles Tested")+ xlab("")+ ylim(0, 220000)+
  ggtitle(label = "Top 5 tested Vehicle Make/Brand in Ireland (2021)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 12, face = "bold"))


#Filtering least 5 tested vehicle make/brand
least5.vehicles <-
  df_vehicle %>%
  top_n(-5, Total)

#Barplot of Top 5 tested vehicle make/brand
least5_plot<-
  least5.vehicles %>%
  mutate(`Vehicle Make` = fct_reorder(`Vehicle Make`,Total)) %>%
  ggplot(aes(x=`Vehicle Make`, y=Total)) +
  geom_bar(stat="identity", fill="#56c3ab",width=0.3) +
  geom_text(aes(label=Total), vjust=-0.5)+
  ylab("No. of Vehicles Tested")+ xlab("")+ ylim(0, 7)+
  ggtitle(label = "Least 5 tested Vehicle Make/Brand in Ireland (2021)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 12, face = "bold"))

#place both plots side by side for comparison with gridExtra library
grid.arrange(top5_plot, least5_plot, ncol = 2)   

#Filtering pass and fail per vehicle models
df_vehicle_pass_fail<-
  vehicle.data %>%
  group_by(`Vehicle Make`) %>%
  summarise(Total = sum(Total, na.rm=TRUE),
            Pass = sum(PASS, na.rm = TRUE),
            Fail = sum(FAIL, na.rm = TRUE))
View(df_vehicle_pass_fail) #dataframe of pass and fail per vehicle models

#Add pass rate per vehicle make to the dataframe above
df_vehicle_pass_fail$`Pass.rate(%)`<- 
  df_vehicle_pass_fail$Pass / df_vehicle_pass_fail$Total * 100

#Add Fail rate per vehicle make to the dataframe above
df_vehicle_pass_fail$`Fail.rate(%)`<- 
  100 - df_vehicle_pass_fail$`Pass.rate(%)`

View(df_vehicle_pass_fail) #View the updated dataframe

#barplot of Pass rate per Vehicle make/brand
vehicle.pass_rate_plot<-
  df_vehicle_pass_fail %>%
  mutate(`Vehicle Make` = fct_reorder(`Vehicle Make`,`Pass.rate(%)`)) %>%
  ggplot(aes(x = `Vehicle Make`, y = `Pass.rate(%)`/100, fill=`Vehicle Make`,
             label=scales::percent(`Pass.rate(%)`/100))) +
  geom_col(width=0.3)+
  theme(legend.position="none")+
  coord_flip()+
  ylab("Test Pass Rate")+ xlab("")+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 8))+
  geom_text(nudge_y= 0.035, size = 3, fontface="bold")+
  ggtitle(label = "NCT Test Pass rate by Vehicle Make/Brand in Ireland (2021)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 7, face = "bold"))

vehicle.pass_rate_plot

#for interactive plot
vehicle.pass_rate_plot %>% ggplotly

#Filtering 10 popular vehicle brand to compare their pass rates
Top10.popular.cars<-
  df_vehicle_pass_fail[df_vehicle_pass_fail$`Vehicle Make` %in% 
                         c('TOYOTA','VOLKSWAGEN','TESLA','HONDA','FORD',
                           'AUDI','HYUNDAI','BMW','MERCEDES BENZ','NISSAN'),]

#Barplot of the top 10 popoluar cars pass rate
Top10.vehicle.pass_rate_plot<-
  Top10.popular.cars %>%
  mutate(`Vehicle Make` = fct_reorder(`Vehicle Make`,`Pass.rate(%)`)) %>%
  ggplot(aes(x = `Vehicle Make`, y = `Pass.rate(%)`/100, fill=`Vehicle Make`,
             label=scales::percent(`Pass.rate(%)`/100)))+
  geom_col(width=0.8,show.legend = FALSE)+
  coord_flip()+
  ylab("Test Pass Rate")+ xlab("")+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 8))+
  geom_text(nudge_y= -0.065, size = 5, color="white", fontface="bold")+
  ggtitle(label = "Top 10 Popular Vehicle Make/Brand Pass rate in Ireland (2021)",
          subtitle = "Note that this selection is based on data from Ireland car blogs and news")+ 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 10, face = "bold"))


#add animation layer to the bar plot
animateTop10<- Top10.vehicle.pass_rate_plot + 
  transition_states(`Vehicle Make`, transition_length = 2, state_length = 1) + shadow_mark() +
  enter_grow() + 
  exit_fade(alpha = 1)

#control animation speed
animate(animateTop10, fps = 6)  

anim_save("boxplot.gif", animateTop10, renderer = gifski_renderer())#save animated plot


#subset all Toyota cars
Toyota.cars<- vehicle.data[vehicle.data$`Vehicle Make` == "TOYOTA",]
View(Toyota.cars)

length(unique(Toyota.cars$`Vehicle Model`)) #38 unique models of toyota

#Filter Toyota car models 
df_Toyota_model<-
  Toyota.cars %>%
  group_by(`Vehicle Model`) %>%
  summarise(Total = sum(Total, na.rm=TRUE),
            Pass = sum(PASS, na.rm = TRUE),
            Fail = sum(FAIL, na.rm = TRUE))
View(df_Toyota_model)  

nrow(df_Toyota_model)  #38 unique models of toyota

#Add pass rate per model (Toyota) 
df_Toyota_model$`Pass.rate(%)`<- 
  df_Toyota_model$Pass / df_Toyota_model$Total * 100

#Add Fail rate per model (Toyota) 
df_Toyota_model$`Fail.rate(%)`<- 
  100 - df_Toyota_model$`Pass.rate(%)`

#Barplot of pass rates of Toyota cars by model
df_Toyota_model_plot<- df_Toyota_model %>%
  mutate(`Vehicle Model` = fct_reorder(`Vehicle Model`,`Pass.rate(%)`)) %>%
  ggplot(aes(x = `Vehicle Model`, y = `Pass.rate(%)`/100,
             label=scales::percent(`Pass.rate(%)`/100)))+
  geom_col(width=0.8, fill= "#01bfc5")+
  theme(legend.position="none")+
  coord_flip()+
  ylab("Test Pass Rate")+ xlab("")+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 8))+
  geom_text(nudge_y= -0.065, size = 3, color="white", fontface="bold")+
  ggtitle(label = "Toyota Cars' Pass rates by model in Ireland (2021)")+ 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 8, face = "bold"))

df_Toyota_model_plot

#Filter top 5 Toyota models by number of cars tested
Top5.Toyota.models.tested <-
  df_Toyota_model %>%
  top_n(5, Total)

#Barplot of Top tested Toyota cars by model
Top5.Toyota.models.tested_plot<-
  Top5.Toyota.models.tested %>%
  mutate(`Vehicle Model` = fct_reorder(`Vehicle Model`,Total)) %>%
  ggplot(aes(x=`Vehicle Model`, y=Total)) +
  geom_bar(stat="identity", fill="#039295",width=0.8) +
  geom_text(aes(label=Total), vjust=-0.5, fontface = "bold", size=5)+
  ylab("Total Vehicles Tested")+ xlab("")+ ylim(0,60000)+
  ggtitle(label = "Top 5 most tested Toyota Cars by model in Ireland (2021)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 9, face = "bold"))

#Barplot of Top tested Toyota cars by model and their pass rate
Top5.Toyota.models.passRate_plot<-
  Top5.Toyota.models.tested %>%
  mutate(`Vehicle Model` = fct_reorder(`Vehicle Model`,`Pass.rate(%)`)) %>%
  ggplot(aes(x = `Vehicle Model`, y = `Pass.rate(%)`/100,
             label=scales::percent(`Pass.rate(%)`/100)))+
  geom_col(width=0.8, fill= "#cbb22a")+
  theme(legend.position="none")+
  # coord_flip()+
  ylab("Test Pass Rate")+ xlab("")+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 8))+
  geom_text(nudge_y= -0.065, size = 5, color="white", fontface="bold")+
  ggtitle(label = "Pass rate of Top 5 tested Toyota cars by model in Ireland (2021)")+ 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 9, face = "bold"))

#place both plots side by side for comparison with gridExtra library
grid.arrange(Top5.Toyota.models.tested_plot, Top5.Toyota.models.passRate_plot, ncol = 2)   


#Scatterplot of ALL Toyota cars' quantity vs their pass rate

all_Toyota_scatterplot<- ggplot(Toyota.cars, aes(Total, `PASS %`)) +
  geom_point()+geom_smooth()+
  ylab("Test Pass Rate in percentages (%)")+ xlab("Quantities of Cars Tested")+
  ggtitle(label = "Scatter plot of Toyota cars vs Pass rates in Ireland (2021)",
          subtitle = "Quantities of All Toyota Cars Tested and their corresponding pass rates (%)")+ 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 12, face = "bold"))

#filter 6 popular car brands
Popular.6.car.brands<-
  vehicle.data[vehicle.data$`Vehicle Make` %in% c("BMW", "FORD", "MERCEDES BENZ","AUDI",
                                                  "HONDA", "VOLKSWAGEN"), ]

  
facet_scatterplot_6popular<- ggplot(Popular.6.car.brands, aes(Total, `PASS %`)) +
  geom_point()+geom_smooth(se=FALSE)+
  ylab("Test Pass Rate in percentages (%)")+ xlab("Quantities of Cars Tested")+
  ggtitle(label = "Facet Scatter plot of 6 popular car brands tested vs Pass rates in Ireland (2021)",
          subtitle = "Quantities of 6 popular car brands Tested and their corresponding pass rates (%)")+ 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 12, face = "bold"))
  
facet_scatterplot_6popular + facet_wrap(~`Vehicle Make`)


#Boxplot 
boxplot_6popular<- ggplot(Popular.6.car.brands, aes(`Vehicle Make`, `PASS %`, fill = `Vehicle Make`)) +
  geom_boxplot() +
  geom_jitter(aes(`Vehicle Make`, `PASS %`), 
              width = 0.2, shape = 21, size = 1, alpha = 0.2)+
  coord_flip()+
  ylab("Test Pass Rate in percentages (%)")+ xlab("")+
  ggtitle(label = "Box plot of test pass rates by 6 popular car brands in Ireland (2021)",
          subtitle = "The distribution of pass rates of 6 chosen popular vehicle makes ")+ 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 12, face = "bold"))

#Violin plot
Violinplot_6popular<- ggplot(Popular.6.car.brands, aes(x=`Vehicle Make`, y=Total, fill=`Vehicle Make`)) +
  geom_violin()+
  coord_flip()+
  ylab("Quantity of Cars tested")+ xlab("")+
  ggtitle(label = "Violin plot of Quantity of cars tested (6 popular car brands) in Ireland (2021)",
          subtitle = "The distribution of the Quantity of  6 chosen popular vehicle makes tested")+ 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 12, face = "bold"))

#place both plots side by side for comparison with gridExtra library
grid.arrange(boxplot_6popular, Violinplot_6popular, ncol = 2)


#THE 14 INDIVIDUAL TESTS
#Filtering the dataset grouped by Vehicle make and recalcluated percentages by Vehicle make
vehicle.data_group_by_brand<- vehicle.data %>%
  group_by(`Vehicle Make`) %>%
  summarise(Total = sum(Total, na.rm=TRUE),
            Pass = sum(PASS, na.rm = TRUE),
            Fail = sum(FAIL, na.rm = TRUE),
            `Pass Rate %` = sum(PASS/Total*100, na.rm = TRUE),
            `Fail Rate %` = sum(100-`Pass Rate %`),
            `Vehicle and Safety Equipment` = sum(`Vehicle and Safety Equipment`, na.rm=TRUE),
            `Vehicle and Safety Equipment %` = sum(`Vehicle and Safety Equipment`/Total*100, na.rm=TRUE),
            `Lighting and Electrical` = sum(`Lighting and Electrical`, na.rm=TRUE),
            `Lighting and Electrical %` = sum(`Lighting and Electrical`/Total*100, na.rm=TRUE),
            `Steering and Suspension` = sum(`Steering and Suspension`, na.rm=TRUE),
            `Steering and Suspension %` = sum(`Steering and Suspension`/Total*100, na.rm=TRUE),
            `Braking Equipment` = sum(`Braking Equipment`, na.rm = TRUE),
            `Braking Equipment %` = sum(`Braking Equipment`/Total*100, na.rm = TRUE),
            `Wheels and Tyres` = sum(`Wheels and Tyres`, na.rm = TRUE),
            `Wheels and Tyres %` = sum(`Wheels and Tyres`/Total*100, na.rm = TRUE),
            `Engine Noise and Exhaust` = sum(`Engine Noise and Exhaust`, na.rm = TRUE),
            `Engine Noise and Exhaust %` = sum(`Engine Noise and Exhaust`/Total*100, na.rm = TRUE),
            `Chassis and Body` = sum(`Chassis and Body`, na.rm = TRUE),
            `Chassis and Body %` = sum(`Chassis and Body`/Total*100, na.rm = TRUE),
            `Side Slip Test` = sum(`Side Slip Test`, na.rm = TRUE),
            `Side Slip Test %` = sum(`Side Slip Test`/Total*100, na.rm = TRUE),
            `Suspension Test` = sum(`Suspension Test`, na.rm = TRUE),
            `Suspension Test %` = sum(`Suspension Test`/Total*100, na.rm = TRUE),
            `Light test` = sum(`Light test`, na.rm = TRUE),
            `Light test %` = sum(`Light test`/Total*100, na.rm = TRUE),
            `Brake Test` = sum(`Brake Test`, na.rm = TRUE),
            `Brake Test %` = sum(`Brake Test`/Total*100, na.rm = TRUE),
            `Emmissions` = sum(`Emmissions`, na.rm = TRUE),
            `Emmissions %` = sum(`Emmissions`/Total*100, na.rm = TRUE),
            `OTHER` = sum(`OTHER`, na.rm = TRUE),
            `OTHER %` = sum(`OTHER`/Total*100, na.rm = TRUE),
            `Incompletable` = sum(`Incompletable`, na.rm = TRUE),
            `Incompletable %` = sum(`Incompletable`/Total*100, na.rm = TRUE)
            )

View(vehicle.data_group_by_brand)

#scatterplot matrix
vehicle.data_group_by_brand_sc_matrix<- ggpairs(data = 
          vehicle.data_group_by_brand[, c(3,7,9,11,13,15,17,19,21,23,25,27,29,31,33)])

#split (first 7 tests with PASS and FAIL)
vehicle.data_group_by_brand_sc_matrix7<- 
  ggpairs(data = vehicle.data_group_by_brand[, c(3,4,7,9,11,13,15,17,19)],
          title = "First 7 tests with PASS and FAIL")+
  theme(plot.title = element_text(hjust = 0.5))
  
#scatterplot of the Vehicle and Safety Equipment Test
scatterplot_pass_fail_VSE<- 
  ggplot(vehicle.data_group_by_brand, aes(`Vehicle and Safety Equipment`, Pass)) +
  geom_point(size=5)+geom_smooth(method = "lm")+
  geom_jitter(aes(`Vehicle and Safety Equipment`, Fail), 
              width = 0.2, shape = 18, size = 5, alpha = 0.5)+ #to add the fail rate
  ylab("Pass and Fail")+ xlab("Vehicle and Safety Equipment Test")+
  ggtitle(label = "Scatter plot of the Vehicle and Safety Equipment Test",
          subtitle = "The Vehicle and Safety Equipment Test (failed) vs No. of Total General Test Pass and Fail")+ 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 9, face = "bold"))

scatterplot_BE_WT<- 
  ggplot(vehicle.data_group_by_brand, aes(x=`Braking Equipment`, y=`Wheels and Tyres`)) +
  geom_point()+geom_smooth(method = "lm")+
  stat_cor(label.x = 1, label.y = 28000, 
           label.sep = "=")+ #adding correlation coefficient
  ylab("Wheels and Tyres")+ xlab("Braking Equipment")+
  ggtitle(label = "Scatter plot of Braking Equipment vs Wheels and Tyres ")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 9, face = "bold"))
          

#Correlation matrix
vehicle.data.cor<- 
  cor(vehicle.data_group_by_brand[, c(3,4,7,9,11,13,15,17,19,21,23,25,27,29,31,33)],
      method = c("spearman")) #-----makes a correlation matrix of selected column

#assigning the subset of dataframe to a variable for easy referencing
subset.vehicle.data<- 
  vehicle.data_group_by_brand[, c(3,4,7,9,11,13,15,17,19,21,23,25,27,29,31,33)]

#Hmisc package to run the correlation matrix with p-values
vehicle.data.rcorr = rcorr(as.matrix(subset.vehicle.data))
vehicle.data.rcorr

#extracting correlation coeefficient and p-values 
subset.vehicle.data.coef = vehicle.data.rcorr$r
subset.vehicle.data.pval = vehicle.data.rcorr$P

#Correlation plot with corrplot funtion
corrplot(vehicle.data.cor, tl.col="black", 
         main="Correlation Plot of the individual NCT tests in Ireland (2021)")

#make it like heatmap with correlation coefficients on the tiles ---- add method="color" 
corrplot(vehicle.data.cor, tl.col="black", method = "color",
         addCoef.col = "white", number.cex = 0.5,
         main="Correlation Plot of the individual NCT tests in Ireland (2021)")



#SUBSET TOP TEN CARS 
Top10.full.car.brands<-
  vehicle.data_group_by_brand[vehicle.data_group_by_brand$`Vehicle Make` %in% 
                                c("BMW", "FORD", "MERCEDES BENZ","AUDI",
                                  "HONDA", "VOLKSWAGEN","TOYOTA","HYUNDAI",
                                  "NISSAN","TESLA"), ]

#Barplot of Brake Test failed by Top 10 Car brands in Ireland (2021)
Top10.full.car.brands %>%
  mutate(`Vehicle Model` = fct_reorder(`Vehicle Make`,`Brake Test %`)) %>%
  ggplot(aes(x = `Vehicle Make`, y = `Brake Test %`/100,
             label=scales::percent(`Brake Test %`/100),fill= `Vehicle Make`))+
  geom_col(width=0.8)+
  # theme(legend.position="none")+
  coord_flip()+
  ylab("Brake Test Fail")+ xlab("")+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 10))+
  geom_text(nudge_y=-0.02, size = 5, color="white", fontface="bold")+
  ggtitle(label = "Brake Test failed by Top 10 Car brands in Ireland (2021)")+ 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 9, face = "bold"))


##################################################################
##################### DATASET 2 ##################################

#Import dataset
test.center.data<- read_excel("Total Pass Rates_2018-2022.xlsx")
test.center.data.copy<- test.center.data #copy of data

View(test.center.data)

#convert the years column from proportion to percentage
test.center.data$`2018_Pass_rates_%`<- test.center.data$`2018_Pass_rates_%`*100 #2018 
test.center.data$`2019_Pass_rates_%`<- test.center.data$`2019_Pass_rates_%`*100 #2019
test.center.data$`2020_Pass_rates_%`<- test.center.data$`2020_Pass_rates_%`*100 #2020
test.center.data$`2021_Pass_rates_%`<- test.center.data$`2021_Pass_rates_%`*100 #2021

names(test.center.data) #having problems with the 2022 column name
names(test.center.data)[6]<- "2022_Pass_rates_%" #changing the column name to look like others

test.center.data$`2022_Pass_rates_%`<- test.center.data$`2022_Pass_rates_%`*100 #2022

#modify column names for easy reference
names(test.center.data)[c(1,2,3,4,5,6)] <- c("Test_Centre","2018", "2019", "2020", "2021", "2022")

View(test.center.data)

#data table presentation for report using gt()
table.test.center.data.summary <- 
  gt(head(test.center.data)) %>%
  tab_options(column_labels.background.color = "#d0d0d0") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_header(title = "Driving Test Pass Rate by  Test Centre in Ireland (2018-2022)")

  
table.test.center.data.summary

#Convert the dataset from wide to long format
test.center.data.df <- tidyr::pivot_longer(test.center.data, cols = c("2018", "2019", "2020", "2021", "2022"), 
                               names_to = "Year", values_to = "Pass_Rates")

View(test.center.data.df)

#convert year to factor
test.center.data.df$Year <- as.factor(test.center.data.df$Year)

# line plot to show Pass rates over the years by test centres
ggplot(test.center.data.df, aes(x = Year, y = Pass_Rates, group = Test_Centre, color = Test_Centre)) +
  geom_line() +
  labs(x = "Year", y = "Pass Rates (%)") +
  theme_bw()+ #using this theme so we can clearly see the lines
  ggtitle(label = "Pass Rates by Test Centre over 5 Years")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 12, face = "bold"))

#Get top 5 most performing centres over the years
#First Calculate the mean pass rates for each test centre over the years
test.center.data_mean <- 
  test.center.data.df %>%
  group_by(Test_Centre) %>%
  summarize(mean.pass.rates = mean(Pass_Rates))

test.center.data_mean 

# Select the top 5 test centres based on the mean pass rates
top5.centres <- 
  test.center.data_mean %>%
  top_n(5, mean.pass.rates) %>%
  left_join(test.center.data.df, by = "Test_Centre")

View(top5.centres)





# library(openxlsx)
write.xlsx(test.center.lon.lat, "output3.xlsx", rowNames = FALSE)
##################################################################

#Plot top 5 centres based on mean pass rates
ggplot(top5.centres, aes(x = Year, y = Pass_Rates, group = Test_Centre, color = Test_Centre)) +
  geom_line() +
  labs(x = "Year", y = "Pass Rates (%)") +
  theme_bw()+
  ggtitle(label = "Top 5 Average Pass Rates by Test Centre over the Years",
          subtitle = "Test Centres with the Top 5 Mean Pass Rates over 5 years")+ 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5) )+
  theme(axis.text = element_text(size = 12, face = "bold"))

#If I had more time, I will plot the animated time series to see the evolution of the top 5 centers over the years


################# MAPS ########################

#Let's retrieve map of Ireland

install.packages("tmap")
library(tmap)
library(sf)

# convert dataframe to sf library object
test.centers.sf <- st_as_sf(test.center.data, coords = c("Longitude", "Latitude"), crs = 4326)

#I've got error loading the maps package, If I had more time, I would solve this issue

####################################################################
install.packages("leaflet")
library(leaflet)
install.packages("leaflet.extras")
library(leaflet.extras)
install.packages("htmlwidgets")
library(htmlwidgets)
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library(rnaturalearthdata)
library(rnaturalearth)

world_map <- ne_countries(scale = "medium", returnclass = "sf")
ireland_map <- world_map %>% filter(admin == "Ireland")

test_centers <- data.frame(
  Test_Centre = c("Cavan", "Clifden", "Cork", "Loughrea", "Monaghan"),
  Latitude = c(53.9908, 53.4875, 51.8969, 53.198, 54.2481),
  Longitude = c(-7.36, -10.0217, -8.4863, -8.5667, -6.9689)
)


ggplot() +
  geom_sf(data = ireland_map, fill = "white", color = "black") +
  geom_point(data = test_centers, aes(x = Longitude, y = Latitude, color = Test_Centre), size = 4) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Top 5 Test Centers' Pass rate in Ireland (2018-2022)") +
  theme(plot.title = element_text(hjust = 0.5))







         
 


