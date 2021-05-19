# Author:           Trevor Strobel
# Date:             5/19/2021

import csv
from salarStrings import country_names, city_names, male_names, female_names
   
# All Data
with open('StringData/1.csv', mode='w') as csvFile:
   data = country_names + city_names +  male_names + female_names
   for x in data:
       csvFile.write(x + "\n")

# Countries
with open('StringData/2.csv', mode='w') as csvFile:
    for x in country_names:
        csvFile.write(x +"\n")

# Cities
with open('StringData/3.csv', mode='w') as csvFile:
    for x in city_names:
        csvFile.write(x +"\n")        

# Male Names
with open('StringData/4.csv', mode='w') as csvFile:
    for x in male_names:
        csvFile.write(x +"\n")

# Female Names
with open('StringData/5.csv', mode='w') as csvFile:
    for x in female_names:
        csvFile.write(x +"\n")

# All Names
with open('StringData/6.csv', mode='w') as csvFile:
   names = male_names + female_names
   for x in names:
       csvFile.write(x + "\n")
   