## Messy Data and Machine Learning Homework 3
## Paul Sergent, Assel Dmitriyeva, Frankie Tam

# import data
sqf_08_16.data <- read_csv('sqf_08_16.csv')

# restrict to CPW stops
sqf_08_16.data <- sqf_08_16.data %>% filter(suspected.crime=='cpw')


