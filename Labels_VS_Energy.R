library(tidyverse)
yrs <- c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

raw_EL <- read_csv("municipality_data.csv") 

raw_EL %>% group_by(year) %>%
  summarise(total_number = sum(number_of_labels))%>%
  ggplot() + 
  aes(x = year, y = total_number/10000) + geom_line(color = "black")+ 
  labs(title = "Total number of labeled houses", color = NULL) + 
  xlab("Year") + 
  ylab("Total number of houses (times 10^4)") +
  scale_y_continuous(limits = c(NA, 450), breaks = scales::breaks_extended(15))+
  scale_x_continuous(breaks = yrs)

ggsave("Tot number of labeled houses.png")

Label_H <- raw_EL %>%
  pivot_longer(c(3,4,5,6,7,8,9,10,11),names_to = "Labels", values_to = "Number_of_Houses") %>%
  group_by(year, Labels) %>%
  summarise(total_number = sum(Number_of_Houses))
  
ggplot(data = Label_H) +
  aes(x= year, y = total_number/10000, color = Labels) +
  geom_line(size = 1) +
  labs(title = "Total number of houses for each label, per year", color = NULL) + 
  xlab("Year") + 
  ylab("Total number of houses (times 10^4)") +
  scale_y_continuous(limits = c(NA, 110), breaks = scales::breaks_extended(15))+
  scale_x_continuous(breaks = yrs)

ggsave("Tot no. of houses for each label, per year.png")

Closeup <- Label_H  %>% 
  filter(Labels %in% c("Aplusplus" ,"Aplus"))

ggplot(data= Closeup) + 
  aes(x= year, y = total_number, color = Labels) +
  geom_line(size = 1)+ 
  labs(title = "Total number of houses A+ and A++, per year", color = NULL) + 
  xlab("Year") + 
  ylab("Total number of houses") +
  scale_y_continuous(limits = c(NA, 5600), breaks = scales::breaks_extended(15))+
  scale_x_continuous(breaks = yrs)

ggsave("A closeup view for A+ and A++.png")


Energy_H <- raw_EL %>%
  mutate(avg_gas_used_per_H = total_natural_gas_used/number_of_houses) %>%
  mutate(avg_electricity_per_H = total_electricity_used/number_of_houses) %>%
  group_by(year)%>%
  summarise(Electricity = sum(avg_electricity_per_H), 
            NaturalGas = sum(avg_gas_used_per_H)) %>%
  pivot_longer(c(2,3), names_to = "types_of_energy", values_to = "Units")

ggplot(data = Energy_H) + 
  aes(x = year, y = Units/1000000, color = types_of_energy) + 
  geom_line()+
  labs(title = "Different energy used per house", color = "Types of energy") + 
  xlab("Year") + 
  ylab("Energy units in MWh") +
  scale_y_continuous(breaks = scales::breaks_extended(10))+
  scale_x_continuous(breaks = yrs)

ggsave("Different energy used per house.png")