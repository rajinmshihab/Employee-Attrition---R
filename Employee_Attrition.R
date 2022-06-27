#------------------------------STUDENT INFO------------------------------------#
# NAME: RAJIN MUHTADEE SHIHAB
# TPNO: TP059508
#------------------------------------------------------------------------------#


#--------------------------------LIBRARY---------------------------------------#
install.packages("ggplot2")
install.packages("plotrix")
install.packages("plyr")
install.packages("dplyr")
install.packages("biscale")
library(biscale)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotrix)
#------------------------------------------------------------------------------#

#--------------------------------IMPORT & CLEANUP------------------------------#
e_attrition = read.csv("D:\\University\\Semester - 3\\PFDA - Programming for Data Analysis (092021-MHJ)\\Assignments\\employee_attrition.csv",header = TRUE)


View(e_attrition)


#--------------------------------ANALYSIS--------------------------------------#

#1-1

AgeFact <- as.data.frame.matrix(e_attrition %>%
                                  group_by(age)  %>%
                                  select(STATUS) %>%
                                  table()  )
View(AgeFact)

ggplot() + 
  geom_bar(aes(y = ..count.. , x = as.factor(age), fill = as.factor(STATUS)),
           data = e_attrition, position = position_dodge()) + 
  labs(y = "Active Status", x = "Age", title = "Active Status for Age")

#1-2

GenFact <- as.data.frame.matrix(e_attrition %>%
                                  group_by(gender_full)  %>%
                                  select(STATUS) %>%
                                  table()  )
View(GenFact)

ggplot() + 
  geom_bar(aes(y = ..count.. , x = as.factor(gender_full), fill = as.factor(STATUS)),
           data = e_attrition, width = 0.5, position = position_dodge()) + 
  labs(y = "Active Status", x = "Gender", title = "Active Status for Gender")


#1-3

DepFact <- as.data.frame.matrix(e_attrition %>%
                                  group_by(department_name)  %>%
                                  select(STATUS) %>%
                                  table()  )
View(DepFact)

ggplot() + 
  geom_bar(aes(y = ..count.. , x = as.factor(department_name), fill = as.factor(STATUS)),
           data = e_attrition, width = 0.5, position = position_dodge()) + 
  labs(y = "Active Status", x = "Department", title = "Active Status in Departments") + 
  coord_flip()

#1-4

e_attrition %>% distinct(STATUS_YEAR)

YrFact <- as.data.frame.matrix(e_attrition %>%
                                  group_by(STATUS_YEAR)  %>%
                                  select(STATUS) %>%
                                  table()  )
View(YrFact)

ggplot() + 
  geom_bar(aes(y = ..count.. , x = as.factor(STATUS_YEAR), fill = as.factor(STATUS)),
           data = e_attrition, width = 0.5, position = position_dodge()) + 
  labs(y = "Active Status", x = "Year", title = "Active Status during Years") + 
  coord_flip()

#1-5

e_attrition %>% distinct(city_name)

CityFact <- as.data.frame.matrix(e_attrition %>%
                                 group_by(city_name)  %>%
                                 select(STATUS) %>%
                                 table()  )
View(CityFact)

ggplot() + 
  geom_bar(aes(y = ..count.. , x = as.factor(city_name), fill = as.factor(STATUS)),
  data = e_attrition, width = 0.5, position = position_dodge()) +
  scale_fill_manual(values=c("cyan", "orange")) + 
  labs(y = "Active Status", x = "City", title = "Active Status in Cities") + 
  coord_flip()


#2-1

e_attrition %>% distinct(department_name)

DePerf <- as.data.frame.matrix(e_attrition %>%
                                 group_by(department_name)  %>%
                                 select(length_of_service) %>%
                                 table()  )
View(DePerf)

ggplot(e_attrition, aes(x = department_name, y = length_of_service)) +
  geom_point() + coord_flip()  +
  labs(y = "Length of Service", x = "Department", title = "Service Length in Departments")

#2-2

e_attrition %>% distinct(store_name)

TitPerf <- as.data.frame.matrix(e_attrition %>%
                                 group_by(job_title)  %>%
                                 select(length_of_service) %>%
                                 table()  )
View(TitPerf)

ggplot(e_attrition, aes(x = job_title, y = length_of_service)) +
  geom_bin2d() + coord_flip() +
  labs(y = "Length of Service", x = "Job Title", title = "Service Length among Jobs")

#2-3

e_attrition %>% distinct(city_name)

ShopPerf <- as.data.frame.matrix(e_attrition %>%
                                   group_by(city_name)  %>%
                                   select(length_of_service) %>%
                                   table()  )
View(ShopPerf)

ggplot(e_attrition, aes(x = city_name, y = length_of_service)) +
  geom_bin2d() + coord_flip() +
  labs(y = "Length of Service", x = "Job Title", title = "Service Length in Shops")

       
#2-4

e_attrition %>% distinct(gender_full)

GenPerf <- as.data.frame.matrix(e_attrition %>%
                                   group_by(gender_full)  %>%
                                   select(length_of_service) %>%
                                   table()  )
View(GenPerf)
       
ggplot(e_attrition, aes(x = gender_full, y = length_of_service)) +
  geom_bin2d() + coord_flip() +
  labs(y = "Length of Service", x = "Gender", title = "Service Length between Male and Female Employees")

#2-5 

e_attrition %>% distinct(BUSINESS_UNIT)

BuPerf <- as.data.frame.matrix(e_attrition %>%
                                  group_by(BUSINESS_UNIT)  %>%
                                  select(length_of_service) %>%
                                  table()  )
View(BuPerf)

ggplot(e_attrition, aes(x = BUSINESS_UNIT, y = length_of_service)) +
  geom_bin2d() +
  labs(y = "Length of Service", x = "Business Units", title = "Service Length among Business Units")

#3-1

TermType <- as.data.frame(e_attrition %>%
                                 group_by(termtype_desc)  %>%
                                 select(termtype_desc) %>%
                                 table()  )
names(TermType)[1] <- 'Type'
names(TermType)[2] <- 'Number'

View(TermType)

pie(TermType$Number, labels=TermType$Number, main = "Types of Termination Comparison",
    radius = 1.2, col = c("Red","Green","Blue"))
  legend("bottomleft", c("Involuntary","Active","Voluntary"), fill = c("Red","Green","Blue"))
  
#3-2
  
e_attrition %>% distinct(STATUS_YEAR)
  
TerRes <- as.data.frame.matrix(e_attrition %>%
                                   group_by(STATUS_YEAR)  %>%
                                   select(termreason_desc) %>%
                                   table())
                                    

View(TerRes)

ggplot() + 
  geom_bar(aes(y = ..count.. , x = as.factor(STATUS_YEAR), fill = as.factor(termreason_desc)),
           data = e_attrition, position = position_dodge()) + 
  labs(y = "Termination Reason", x = "Year", title = "Termination Reasons over The Years")

#3-3

AgeTerm <- as.data.frame.matrix(e_attrition %>%
                                  group_by(age)  %>%
                                  select(termreason_desc) %>%
                                  table()  )
View(AgeTerm)

ggplot() + 
  geom_bar(aes(y = ..count.. , x = as.factor(age), fill = as.factor(termreason_desc)),
           data = e_attrition, position = position_dodge()) + 
  labs(y = "Termination Reasons", x = "Age", title = "Termination Reasons in Age")

#3-4

DepTerm <- as.data.frame.matrix(e_attrition %>%
                                  group_by(department_name)  %>%
                                  select(termreason_desc) %>%
                                  table()  )
View(DepTerm)

ggplot() + 
  geom_bar(aes(y = ..count.. , x = as.factor(department_name), fill = as.factor(termreason_desc)),
           data = e_attrition, position = position_stack()) + coord_flip() +
  labs(y = "Termination Reasons", x = "Department", title = "Termination Reasons in Departments")


#4-1

AgeTit <- as.data.frame.matrix(e_attrition %>%
                                  group_by(age)  %>%
                                  select(job_title) %>%
                                  table()  )
View(AgeTit)

ggplot(e_attrition, aes(x = age, y = job_title)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  labs(x = "Age", y = "Job TItles")


#4-2

GenTit <- as.data.frame.matrix(e_attrition %>%
                                 group_by(gender_full) %>%
                                 select(job_title) %>%
                                 table()  )
View(GenTit)

ggplot(e_attrition, aes(after_stat(count), as.factor(gender_full), fill = as.factor(job_title))) + 
  geom_density(position = 'stack') +
  labs(title = "Effects of Gender on Job Title", x = 'Gender', y = 'Job Titles')
