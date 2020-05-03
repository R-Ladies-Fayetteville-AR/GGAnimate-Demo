#### Workshop for R-ladies 5-7-20

############ section 0: loading in libraries and datasets ############

library("tidyverse")
library("mapdata")
library("ggthemes")
library("gganimate")
library('Hmisc')

#downloading county case data
county_cases <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv",
                         stringsAsFactors = F)
#extracting only arkansas cases
ar_cases <- filter(county_cases, State == "AR")

#downloading lat and long county data
county_map <- map_data("county")
#extracting AR lat and long
ar_map <- subset(county_map, region == "arkansas") 

str(ar_cases)
str(ar_map)





############ section 1: cleaning data for merging ############

### now we have arkansas counties and their total number of confirmed cases and
### corresponding county lat and long values for mapping but we need to combine/merge
### these datasets by "county"

ar_cases$County.Name[3]
ar_map$subregion[100]

#matching county data between the two datasets using some text edit functions
#first convert all county names to lowercase format
ar_cases$County.Name <- tolower(ar_cases$County.Name)
#then remove the county label
ar_cases$County.Name <- str_remove(ar_cases$County.Name, " county")
#remove any "." in the name
ar_cases$County.Name <- str_remove(ar_cases$County.Name, "[.]")
#then remove any remaining whitespace around the characters
ar_cases$County.Name <- trimws(ar_cases$County.Name)
colnames(ar_map)[6] <- c("County.Name")

#now we've got something to merge on that matches in each dataset
ar_cases$County.Name[3]
ar_map$County.Name[100]
ar_cases$County.Name[3] == ar_map$County.Name[100]





############ section 2: merging and further data cleaning ############

#merging datasets and reordering them (this was important for geom_polygon to construct county lines in order)
cases_and_map <- merge(ar_map, ar_cases, by = "County.Name", all = T, sort = F)
cases_and_map <- cases_and_map[order(cases_and_map$order),]

#for gganimate dataset needs to be in "long" format for the dates

# creating one column with all the dates, and a new column with all the cases for each date by county
cases_map_year <- pivot_longer(cases_and_map, cols = starts_with("X"), names_to = "Date", values_to = "Cases")
# now we need to make those dates into actual dates and date format using some more text edit functions
cases_map_year$Date <- str_remove(cases_map_year$Date, "X") #remove the random X character
cases_map_year$Date <- gsub("[.]", "-", cases_map_year$Date) #switch the periods to - for date format
cases_map_year$Date <- as.Date(cases_map_year$Date, format = "%m-%d-%y") #convert to date format based on month day year

## removing NAs and replacing with 0s for gganimate, also removing a lot of the 0 case data leading up to 3-11-20
#NOTE: gganimate does not allow ANY NAs in the data

#removing unnecessary columns
cases_map_year[,7:9] = NULL
#creating a copy of the data so we can use it for reference and checking our work
cases_map_year_0 <- cases_map_year
#setting NAs in the "Cases" column to 0
cases_map_year_0$Cases[is.na(cases_map_year_0$Cases)] <- 0
#removing any extra NAs in the dataset
cases_map_year_0<- cases_map_year_0[complete.cases(cases_map_year_0),]
#removing early dates that had no cases
cases_map_year_0 <- cases_map_year_0[cases_map_year_0$Date > "2020-03-10",]

## needed to create an extra dataset to reference in the ggplot to label our map with county names
## this dataset creates a mean point within the county lat/long to pin the county name label
cnames <- aggregate(cbind(long, lat) ~ County.Name, data=cases_map_year_0,
                    FUN=function(x)mean(range(x)))
#captializes the first letter of the string
cnames$County.Name <- capitalize(cnames$County.Name)
#fixing our one case of 2 separate strings and fixing the formatting
cnames[cnames$County.Name == "St francis",1] <- "St. Francis"

## current total cases to add to the graph
total_cases <- as.character(sum(ar_cases[,length(ar_cases)])) #calculates the total number of cases and converts to character format
total_title <- paste("Total Confirmed Cases in AR:", total_cases) #storing the full character string




############ section 3: animated graph (gif) ############

#start with our base layer
layer1 <- ggplot()
layer1

#adjusting coordinates so that AR looks natural ;) not square
layer2 <- layer1 + coord_fixed(1.3)
layer2

#adding our map data with county lines and case information as our color fill
layer3 <- layer2 + geom_polygon(data = cases_map_year_0, mapping = aes(x = long,
                                                                       y = lat,
                                                                       group = group,
                                                                       fill = Cases), 
                                color = "black")
layer3

#adjusting the continuous color gradient so we can better see differences between number groups
layer4 <- layer3 + scale_fill_gradientn(limits = c(0, max(cases_map_year_0$Cases)), 
                                        colours=c("white", "darkorange1", "darkmagenta", "navyblue"))
layer4

#remove the background
layer5 <- layer4 + theme_void()
layer5

#add title, caption, and subtitle to label everything appropriately
layer6 <- layer5 + labs(title = 'Number of COVID-19 Cases on {as.character(frame_time, format = "%m-%d-%y")}',
                        caption = 'Data source: usafacts.org',
                        subtitle = total_title)
layer6

#add county labels
layer7 <- layer6 + geom_label(data = cnames, aes(long, lat, label = County.Name, group = NULL), size=3)
layer7

#adjust the font sizes and positions of the text elements on the graph
layer8 <- layer7 + theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
                         plot.subtitle = element_text(hjust = 0.5, size = 13), 
                         plot.caption = element_text(hjust = 0.95, size = 13),
                         legend.title=element_text(size=17), 
                         legend.text=element_text(size=15), 
                         legend.position = c(0.8, 0.2))
layer8

#here is where we use gganimate to cycle through the dates!
layer9 <- layer8 + transition_time(as.Date(Date))
layer9


## simple right? ;)

# all layers put together in one continous code by adding "+" between each layer and storing it all in "gif"
gif <- ggplot() +
  coord_fixed(1.3) + #allows us to manipulate the aspect ratio of the coordinates, increasing makes the y axis units longer than the x
  geom_polygon(data = cases_map_year_0, mapping = aes(x = long,
                                                      y = lat,
                                                      group = group,
                                                      fill = Cases), 
               color = "black") +
  scale_fill_gradientn(limits = c(0, max(cases_map_year_0$Cases)), colours=c("white", "darkorange1", "darkmagenta", "navyblue")) +
  theme_void() +
  labs(title = 'Number of COVID-19 Cases on {as.character(frame_time, format = "%m-%d-%y")}',
       caption = 'Data source: usafacts.org',
       subtitle = total_title) +
  geom_label(data = cnames, aes(long, lat, label = County.Name, group = NULL), size=3.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 15), 
        plot.caption = element_text(hjust = 0.95, size = 15),
        legend.title=element_text(size=17), 
        legend.text=element_text(size=15), 
        legend.position = c(0.8, 0.2)) +
  transition_time(as.Date(Date))

#you can animate it in Rstudio this way:
#animate(gif, height = 800, width = 700, fps = 5)

#Save the file with the date in the name so that it doesn't write over any other files in my folder
filename <- paste0("AR_cases_", format(Sys.Date(), "%m-%d-%Y"), ".gif")
#here fps = frames per second
anim_save(filename, fps = 5, height = 800, width = 700, gif)

