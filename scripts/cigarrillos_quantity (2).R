library(tidyverse)

# make data set with only press releases that have "[number unit] de cigarrillos" in TITLE
cigarrillos$quantity<-as.numeric(gsub("\\D","",cigarrillos$seized)) # quantity column
cigarrillos$unit<-str_trim(gsub("[0-9]+","", 
                                gsub(" de cigarrillos","",cigarrillos$seized))) # unit column
write_csv(cigarrillos,"data/cigarrillos.csv")
cigarrillos1<-filter(cigarrillos,is.na(seized)==FALSE)  

# make data set with press releases that have "[number unit] de cigarrillos" in TEXT but not TITLE
cigarrillosNA<-filter(cigarrillos,is.na(seized)) 
cigarrillosNA$seized<-str_extract(cigarrillosNA$text,"[0-9.]+ [a-z]+ de cigarrillos")
cigarrillosNA$quantity<-as.numeric(gsub("\\D","",cigarrillosNA$seized))
cigarrillosNA$unit<-str_trim(gsub("[0-9]+","",
                                  gsub(" de cigarrillos","",cigarrillosNA$seized)))
cigarrillos2<-filter(cigarrillosNA,is.na(seized)==FALSE)
cigarrillos2$unit<-str_trim(gsub("[.]","",cigarrillos2$unit))

# manually tally the remaining rows and combine 
write_csv(filter(cigarrillosNA,is.na(seized)),"data/cigarrillos_remainder.csv")
cigarrillos3<-read_csv("data/cigarrillos_manual.csv")
cigarrillos_quantity<-rbind(cigarrillos1,cigarrillos2,cigarrillos3)
write_csv(cigarrillos_quantity,"data/cigarrillos_quantity.csv")

#visualization
cigarrillos_quantity <- read_csv("data/cigarrillos_quantity (1).csv")
cigarrillos_quantity$date<-ymd(gsub("[0-9]+[:]+[0-9]+[:]+[0-9]+","",cigarrillos_quantity$date))
ggplot(cigarrillos_quantity_ym, aes(x=year_month, y=total_cig))+
  geom_col() +
  theme_ic() + 
  hline

library(zoo)
library(ICplots)
cigarrillos_quantity_ym <- cigarrillos_quantity %>%
  mutate(year_month = as.yearmon(as.character(year_month), format = "%Y.%m"),
         total_cig = ifelse(cigarrillos_quantity$unit == "paquetes" | 
                              cigarrillos_quantity$unit == "atagos",
                            cigarrillos_quantity$quantity * 20, NA)) %>%
  group_by(year_month) %>%
  summarise(quantity = sum(quantity, na.rm = TRUE),
            total_cig = sum(total_cig, na.rm = TRUE))
cigarrillos_quantity_ym$admin <-ifelse(cigarrillos_quantity_ym$year_month <  as.yearmon("2023-05"),
                 "Abdo", ifelse(cigarrillos_quantity_ym$year_month >  as.yearmon("2023-08"),
                                "Peña", "Peña-elected"))

ggplot(data = subset(cigarrillos_quantity_ym,
                     year_month >= as.yearmon("2023-01")),
       mapping = aes(x = year_month, y = total_cig, color = admin)) +
  geom_point() +
  geom_segment(aes(x = year_month, xend = year_month, y = 0, yend = total_cig)) +
  theme_ic() +
  hline
       