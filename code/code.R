library(readr)
library(dplyr)
library(tidyr)
library(outliers)
library(lubridate)

at <- read.csv('at.csv')
at <- select(at,-2)
str(at)
at <- at%>% separate(start,into = c("date","time"), sep = " ")

class(at$date)
at$date <- ymd(at$date)

at_train <- at %>% filter (.,at$date%>%year()=='2015'|at$date%>%year()=='2016'|at$date%>%year()=='2017'|at$date%>%year()=='2018')
at_test <- at %>% filter (.,at$date%>%year()=='2019')
at_validate <- at %>% filter (.,at$date%>%year()=='2020')

at_test <- at_test%>%group_by(date)%>%summarise(dailyload = sum(load))
at_train <- at_train%>%group_by(date)%>%summarise(dailyload = sum(load))
at_validate <- at_validate%>%group_by(date)%>%summarise(dailyload = sum(load))


at_test %>% group_by(season)%>%group_by(date%>%year())%>%summarise(mean = mean(dailyload))

write.csv(at_test,"at_test.csv", row.names = FALSE)
write.csv(at_train,"at_train.csv", row.names = TRUE)
write.csv(at_validate,"at_validate.csv", row.names = TRUE)

at_test$dailyload <- as.character(at_test$dailyload)

str(at_test)


plot(at_test$date,at_test$dailyload)
ggplot(at_train, aes(x=date, y=dailyload)) +  geom_line()
ggplot(at_test, aes(x=date, y=dailyload)) +  geom_line()
ggplot(at_test, aes(x=date, y=dailyload_pre)) +  geom_line()
ggplot(at_validate, aes(x=date, y=dailyload)) +  geom_line()


library(readr)
at_test2 <- read_csv("at_test.csv")
p = ggplot() + 
  geom_line(data = at_test2, aes(x = date, y = dailyload ,group = 1), color = "blue") +
  geom_line(data = at_test2, aes(x = date, y = dailyload_predicted ,group = 1), color = "red") +
  xlab('Dates') +
  ylab('Daily load')

p

at_test2%>%group_by(season)%>%summarise(Mean = mean(dailyload, na.rm = TRUE),
                                            SD = sd(dailyload, na.rm = TRUE))
at_test2%>%group_by(season)%>%summarise(Mean = mean(dailyload_predicted, na.rm = TRUE),
                                            SD = sd(dailyload_predicted, na.rm = TRUE))


at_validate2 <- read_csv("at_validate.csv")
p1 = ggplot() + 
  geom_line(data = at_validate2, aes(x = date, y = dailyload ,group = 1), color = "blue") +
  geom_hline(yintercept = mean(at_validate2$dailyload),color = "black",size= 1)+
  geom_line(data = at_validate2, aes(x = date, y = dailyload_pre ,group = 1), color = "red") +
  geom_hline(yintercept = mean(at_validate2$dailyload_pre),linetype ="dashed" ,color = "black", size= 1)+
  xlab('Dates') +
  ylab('Daily load')

p1


at_validate2_jan_mar <- at_validate2%>% filter (.,at_validate2$date%>%month()=='1'|at_validate2$date%>%month()=='2'|at_validate2$date%>%month()=='3')

p1 = ggplot() + 
  geom_line(data = at_validate2_jan_mar, aes(x = date, y = dailyload ,group = 1), color = "blue") +
  geom_hline(yintercept = mean(at_validate2_jan_mar$dailyload),color = "black",size= 1)+
  geom_line(data = at_validate2_jan_mar, aes(x = date, y = dailyload_pre ,group = 1), color = "red") +
  geom_hline(yintercept = mean(at_validate2_jan_mar$dailyload_pre),linetype ="dashed" ,color = "black", size= 1)+
  xlab('Dates Jan to March') +
  ylab('Daily load')

p1

at_validate2_april <- at_validate2%>% filter (.,at_validate2$date%>%month()=='4'|at_validate2$date%>%month()=='5'|at_validate2$date%>%month()=='6'|at_validate2$date%>%month()=='7')
p1 = ggplot() + 
  geom_line(data = at_validate2_april, aes(x = date, y = dailyload ,group = 1), color = "blue") +
  geom_hline(yintercept = mean(at_validate2_april$dailyload),color = "black",size= 1)+
  geom_line(data = at_validate2_april, aes(x = date, y = dailyload_pre ,group = 1), color = "red") +
  geom_hline(yintercept = mean(at_validate2_april$dailyload_pre),linetype ="dashed" ,color = "black", size= 1)+
  xlab('Dates April to July') +
  ylab('Daily load')

p1
