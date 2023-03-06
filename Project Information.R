library(tidyverse)

crime = read_delim("SPD_Crime_Data__2008-Present.csv")
crime = read_delim("C:/Users/AquaP/OneDrive/Desktop/Info201/Project 1/data/crime2.csv")
View(crime)

crime %>% 
  summarize(n = n_distinct(`Offense Parent Group`))

crime %>% 
  summarize(n = n_distinct(`Crime Against Category`))

crime %>% 
  summarize(n = n_distinct(``))

crime %>% 
  select(`Offense Start DateTime`) %>% 
  head(2)

sampleCrime <- crime %>% 
  sample_n(5000)
sampleCrime %>% 
  mutate(datech = substr(`Offense Start DateTime`,1,10)) %>% 
  mutate(date = as.Date(datech, format = "%m/%d/%Y")) %>%
  sample_n(10) %>% 
  arrange(date) %>% 
  select(`Report Number`,date)

plotter <- sampleCrime %>% 
  mutate(datech = substr(`Offense Start DateTime`,1,10)) %>% 
  mutate(date = as.Date(datech, format = "%m/%d/%Y"))

plotter <- sampleCrime %>% 
  mutate(datech = substr(`Offense Start DateTime`,1,10)) %>% 
  mutate(date = as.Date(datech, format = "%m/%d/%Y")) %>% 
  filter(as.numeric(format(date,'%Y'))>2008)
  
as.character(crime %>% 
               )
#  05/18/2019 01:23:00 PM

sampleCrime2 <- plotter %>% 
  select(date,`Offense ID`) %>%
  group_by(as.numeric(format(date,'%m'))) %>% 
  na.omit() %>% 
  summarize(n = n_distinct(`Offense ID`))
sampleCrime2
ggplot(sampleCrime2) +
  geom_line(aes(x =`as.numeric(format(date, "%m"))`,y = n))

sampleCrime %>% 
  summarize(n=n_distinct(Longitude))
sampleCrime %>% 
  summarize(n=n_distinct(Latitude))

plotter %>% 
  filter(plotter$date)

crime %>% 
  sample_n(5000) %>% 
  mutate(datech = substr(`Offense Start DateTime`,1,10)) %>% 
  mutate(date = as.Date(datech, format = "%m/%d/%Y")) %>% 
  filter(as.numeric(format(date,'%Y'))>2008 & as.numeric(format(date,'%Y'))<2023) %>% 
  select(date,`Offense ID`) %>%
  group_by(as.numeric(format(date,'%Y'))) %>% 
  na.omit() %>% 
  summarize(n = n_distinct(`Offense ID`)) %>% 
ggplot() +
  geom_line(aes(x =`as.numeric(format(date, "%Y"))`,y = n))+
  ggtitle("Amount of Crime reports per year from the sample")+
  xlab("Year") +
  ylab("Number of Reports that year")
crime2 <- crime %>% 
  select(`Report Number`,`Offense ID`, `Offense Start DateTime`,`Crime Against Category`)

write.csv(crime2, "C:/Users/AquaP/OneDrive/Desktop/Info201/Project 1/data/crime2.csv", row.names=FALSE)
  

paste(               crime %>% 
                       group_by(crime$`Crime Against Category`) %>%
                       summarize(n=n_distinct(`Offense ID`)) %>% 
                       arrange(rank(desc(n))) %>% 
                       pull(`crime$\`Crime Against Category\``))
