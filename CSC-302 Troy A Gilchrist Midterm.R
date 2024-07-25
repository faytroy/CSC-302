library(ggplot2)
library(dplyr)
library(tidyr)
data <- read.csv("C:/Users/RAC_P/OneDrive/Documents/UMICH/CSC-302/Midterm/table_01_17_052623_r1.csv")

data_summary <- data %>%
  group_by(Year, Sale_Lease_Type) %>%
  summarise(Number_Vehicles_Thousands = sum(Number_Vehicles_Thousands)) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(Percentage = Number_Vehicles_Thousands / sum(Number_Vehicles_Thousands) * 100)

ggplot(data_summary, aes(x = Year, y = Number_Vehicles_Thousands, fill = Sale_Lease_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black") +
  scale_fill_manual(values = scales::brewer_pal(palette = "Blues")(n_distinct(data_summary$Sale_Lease_Type))) +
  labs(y = "Sales (in Millions)", x = "Year", fill = "Sale or Lease") +
  theme_minimal()



       