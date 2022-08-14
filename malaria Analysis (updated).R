


File_location = "C:/Users/chunh/OneDrive/Desktop/Part 1/"

#extraction Of DATA 
Data_malaria_inc        = read.csv(paste(File_location,"malaria_inc.csv",sep = ""), header=TRUE)
Data_malaria_deaths     = read.csv(paste(File_location,"malaria_deaths.csv",sep = ""), header=TRUE)
Data_malaria_deaths_age = read.csv(paste(File_location,"malaria_deaths_age.csv",sep = ""), header=TRUE) [,-1]


##################
#Project 1
#Incidence of Malaria by country across time 
#1. Identify countries from the list of Entity
#2. Identify country with highest incidence at each time point
#3. Plot graph (heatmap)


colnames(Data_malaria_inc)[4] = "Incidence"

#Segregation of DATA1 into entity with 'code' and no 'code'  
Missing_Code_data  = subset(Data_malaria_inc, Data_malaria_inc$Code=="") 
Complete_Code_data = subset(Data_malaria_inc, Data_malaria_inc$Code!="") 


#Selection of country with highest incidence rate for each year
Date_range = unique(Complete_Code_data$Year)
Max_incidence_country = c()

for (i in 1:length(Date_range))
{
  Data1 = Complete_Code_data[Complete_Code_data $Year == Date_range[i],]
  Max_incidence_country = rbind(Max_incidence_country,Data1[which.max(Data1[,"Incidence"]),])
}


#install.packages("ggplot2")
library("ggplot2")
p <- ggplot(Complete_Code_data, aes(x=Year, y=Entity,fill = Incidence)) + 
        geom_tile(colour = "black") + 
        ggtitle("Incidence of Malaria by Country across Time ") + theme(axis.title = element_text(face="bold")) +
        theme_minimal() + scale_x_continuous(expand=c(0,0)) + 
        scale_y_discrete(expand=c(0,0)) +
        scale_fill_gradient(low = "white",high = "#D52724") + 
        theme(axis.text = element_text(size = 5.5,colour = "black"),axis.text.x = element_text(size = 11), axis.title = element_text(size = 15))

p + geom_tile(data = Max_incidence_country[,-2], fill = NA, color = "black", size = 0.8)

# Summary
# The plot shows the incidence of Malaria by country across time. y-axis is country. x-axis is year. Darker the shade, higher the incidence.
# Boxed out cell indicates highest incidence for the particular year. 
# Turkey at Y2000 has the overall highest incidence across studied years and countries
# Turkey, Burkina Faso, Mali have the highest incidence at respective years (2000, 2005-2010, 2015) among the studied countries

##################
#Project 2
#Malaria Death rate by country across time 
#1. Identify countries from the list of Entity
#2. Identify countries with reported deaths
#3. Identify country with highest death date at each time point
#4. Plot graph (heatmap)


colnames(Data_malaria_deaths)[4] = "DeathRate"

#Segregation of DATA2 into entity with 'code' and no 'code'  
Missing_Code_data2  = subset(Data_malaria_deaths, Data_malaria_deaths$Code=="") 
Complete_Code_data2 = subset(Data_malaria_deaths, Data_malaria_deaths$Code!="") 

#remove country with 0 death rate from DATA2 (entity with 'code')
Aggregated_Country_DeathRate = aggregate(Complete_Code_data2$DeathRate, by=list(Category=Complete_Code_data2$Entity), FUN=sum)
Country_Death_e_0 = Aggregated_Country_DeathRate$Category[Aggregated_Country_DeathRate$x==0] 
Country_Death_m_0 = Aggregated_Country_DeathRate$Category[Aggregated_Country_DeathRate$x>0] 
Complete_Code_data2 = Complete_Code_data2[Complete_Code_data2$Entity %in% Country_Death_m_0,]


#Selection of country with highest death rate for each year
Date_range2 = unique(Complete_Code_data2$Year)
Max_deathrate_country2 = c()

for (i in 1:length(Date_range2))
{
  Data2 = Complete_Code_data2[Complete_Code_data2 $Year == Date_range2[i],]
  Max_deathrate_country2 = rbind(Max_deathrate_country2,Data2[which.max(Data2[,"DeathRate"]),])
}


#install.packages("ggplot2")
library("ggplot2")
p <- ggplot(Complete_Code_data2, aes(x=Year, y=Entity,fill = DeathRate)) + 
  geom_tile(colour = "black") + 
  ggtitle("Malaria Death Rate by Country across Time ") + theme(axis.title = element_text(face="bold")) +
  theme_minimal() + scale_x_continuous(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_gradient(low = "white",high = "#D52724") + 
  theme(axis.text = element_text(size = 5.5,colour = "black"),axis.text.x = element_text(size = 11), axis.title = element_text(size = 15))

p + geom_tile(data = Max_deathrate_country2[,-2], fill = NA, color = "black", size = 0.8)


# Summary
# The plot shows the death rate of Malaria by country across time. y-axis is country. x-axis is year. Darker the shade, higher the incidence.
# Boxed out cell indicates highest death rate for the particular year. 
# Uganda, Sierra Leone, Burkina Faso have the highest death rate at respective years (1990-1996, 1997-2009, 2010-2016) among the studied countries
# Although Turkey & Mali have the highest incidence rate in 2000 & 2015, respectively, it does not correspond to highest death rate
# Burkina Faso has the highest incidence rate and death rate in 2010

##################
#Project 3
#Malaria deaths by age across time (specific entity)
#1. Section and organize raw data in a list according to different entities  
#2. Select entity of interest
#3. Plot line graph

entity_grp = unique(Data_malaria_deaths_age$entity)
Age_grp    = unique(Data_malaria_deaths_age$age_group) [c(1,3,4,5,2)]
Data_malaria_deaths_age$age_group = factor(Data_malaria_deaths_age$age_group, levels = Age_grp)


#create a blank lists (listed by entity)
Data_list = vector(mode='list', length=length(entity_grp))
names(Data_list) = entity_grp

#DATA list by entity
for (i in 1:length(entity_grp)) 
{
  Data_list[[i]] = Data_malaria_deaths_age[Data_malaria_deaths_age$entity == entity_grp[i],]
}
  
#Select entity of interest
Interest = "World"
DATA_PLOT = Data_list[[Interest]]
Max_Data = DATA_PLOT[which.max(DATA_PLOT $deaths),] [,c("year","deaths")] #identify the year & Age grp with highest death rate 


#install.packages("ggplot2")
#install.packages("scales")
library("ggplot2")
library("scales")
p = ggplot(DATA_PLOT, aes(x=year, y=deaths, group=age_group)) + 
        geom_line(aes(color=age_group),size = 1.25) +  
        geom_point(aes(color=age_group),size = 2.50) + 
        scale_color_manual(values = c("#FF1D1D", "#FF921D", "#FCFA90","#A8FC90", "#90FCFA"))+
        theme_classic() +
        scale_y_continuous(labels = comma)+
        scale_x_continuous(breaks = sort(unique(DATA_PLOT$year)) [seq(1,length(unique(DATA_PLOT$year)),by=2)])+
        ggtitle(paste(" Malaria Deaths in the",Interest ,"by Age across Time")) + theme(plot.title = element_text(color="black", size=25, face="bold.italic"))+
        xlab("Year") + ylab("Death Rate") + theme(axis.title = element_text(face="bold")) +
        labs(colour = "Age Group") + 
        theme(legend.position="top", legend.box.background = element_rect(color="black", size=1), legend.title = element_text(face = "bold"))

p +annotate("rect", xmin = Max_Data$year-0.5, xmax = Max_Data$year+0.5, ymin = Max_Data$deaths-Max_Data$deaths/100, ymax = Max_Data$deaths+Max_Data$deaths/100,alpha = 0.4)


# Summary
# The plot shows the global death rate by age across time. y-axis is death rate. x-axis is year.
# Grey box indicates highest death rate by year and age group. 
# Age Group: Under 5 has the highest Death Rate across Y1990-Y2016.
# Age Group: 70 or older has the lowest Death Rate across Y1990-Y2016.
# The result shows correlation of Age and death Rate likely due to acquired immunity. 
# Younger population are more vulnerable to malaria.







