#title : analysing the forest fire using visualization techniques
#the project is all about collecting the data related to forest fires and preprocessing it
#hence visualizing using the appropriate variables from the dataset

#the library readr is used to read the csv file from the environment
library("readr")

#the data is read using read_csv and stored inside the variable forest_data
forest_data <- read_csv("C:/Users/USER/Desktop/dataquest-R/forestfires.csv")

#the dimension is check using the dim(), it has 517 rows and 13 columns
dim(forest_data)

#the column names are noted using colnames
#it has X,Y, month,day ,FFMC,DMC,DC,ISI,temp,wind,rain,area
colnames(forest_data)

#the view is used to view the tibble 
View(forest_data)

#the ggplot2 is installed on R environment which is the library used in visualization
install.packages("ggplot2")
library("ggplot2")

#the dplyr is used in data manipulation
library("dplyr")

#the data is loaded from the forest fire and grouped by different months and hence the 
#summarize function is used to sum up the total number of months in each group
forest_fire_months <- forest_data %>% group_by(month) %>% summarise(no_of_months=n())

forest_fire_months
#apr     9
#aug    184
#dec     9
#feb    20
#jan     2
#jul    32
#jun    17
#mar    54
#may    2
#nov    1
#oct    15
#sep    172
#we could observe that september and august has the highest number of forest fires reported

#the data is loaded from forest_data and then grouped by day and summarize function is then
#used to calculate total number of forest fire reports in each grouped days
forest_fire_days <- forest_data %>% group_by(day) %>% summarise(no_of_days=n())

forest_fire_days
#fri  85
#mon  74
#sat  84
#sun  95
#thu  61
#tue  64
#wed  54
#we could observe that the highest forest fires reported are on sunday and friday

#the ggplot is then used with aes having x axis is month and y axis as no_of_months
#the geom_bar is used along with the identitity in stats parameter. Theme for background
#is white. Default happens to be a grid
ggplot(data = forest_fire_months)+
  aes(x=month,y=no_of_months)+
  geom_bar(stat = "identity")+
  theme(panel.background = element_rect(fill = "white"))

#the ggplot is then used with aes having x axis is day and y axis as no_of_days
#the geom_bar is used along with the identitity in stats parameter. Theme for background
#is white. Default happens to be a grid
ggplot(data = forest_fire_days)+
  aes(x=day,y=no_of_days)+
  geom_bar(stat = "identity")+
  theme(panel.background = element_rect(fill = "white"))

#the data is loaded from forest_data using the pipe operator and then the month and days are
#converted to factors
forest_data <- forest_data %>% mutate(
  month = factor(month,levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")),
  day = factor(day,levels = c("sun","mon","tue","wed","thurs","fri","sat"))
)

#the user defined function is built to plot the boxplot which uses 2 variables x and y as the
#formal parameter ,the data is loaded from forest_data, the aes_string is used to take the 
#input column from the function and hence the geom_boxplot is used by setting background 
#theme to white.
create_box_plot <- function(x,y){
  ggplot(data = forest_data)+
  aes_string(x=x,y=y)+
    geom_boxplot()+
    theme(panel.background = element_rect(fill = "white"))
}

# the x_var_month is named as month using name function from 3rd column of the data
x_var_month <- names(forest_data)[3]
x_var_month

# the x_var_day  is named as day using name function from 4th column of the data
x_var_day <- names(forest_data)[4]
x_var_day

# the y_var  is named using name function from 5th to 12th columns of the data
y_var <- names(forest_data)[5:12]
y_var

#the map2 is used to map the variables which takes 2 variables x_var_month and y_var
#along with the function used to plot the boxplot
month_box <- map2(x_var_month,y_var,create_box_plot)
month_box
# it is observed that august has the highest extreme forest fire reports along with july
# and march from the boxplot

#the map2 is used to map the variables which takes 2 variables x_var_day and y_var
#along with the function used to plot the boxplot
day_box <- map2(x_var_day,y_var,create_box_plot)
day_box
#it is observed that fri, sun , tuesday has the extreme forest fire reports from the boxplot

#the user defined function is built for plotting the scatter plot using forest_data at 
#the data parameter and aes_string is used to take the input from the formal paramters of 
#the function . The geom_point() is then used to plot the scatter plot with background as 
#white
scatter_plot <- function(x,y){
  ggplot(data = forest_data)+
    aes_string(x=x,y=y)+
    geom_point()+
    theme(panel.background = element_rect(fill = "white"))
}

#the x_var_scatter is named using names function from 5th to 12th column of the forest_data
x_var_scatter = names(forest_data)[5:12]
x_var_scatter

#the y_var_scatter is named using names function from 5th to 12th column of the forest_data
y_var_scatter = names(forest_data)[13]
y_var_scatter

#the map2 is used to map the x_var_scatter and y_var_scatter along with user defined
#scatter plot function
scatter <- map2(x_var_scatter,y_var_scatter,scatter_plot)
scatter
#it is observed from the scatter plot that,as the rain is less the area is more extreme 
