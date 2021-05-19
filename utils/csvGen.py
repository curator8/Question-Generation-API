# Author:           Trevor Strobel
# Date:             5/19/2021

import csv
from salarStrings import country_names, city_names, male_names, female_names

with open('StringData/countries.csv', mode='w') as csvFile:
    for x in country_names:
        csvFile.write(x +"\n")


with open('StringData/cities.csv', mode='w') as csvFile:
    for x in city_names:
        csvFile.write(x +"\n")        


with open('StringData/male_names.csv', mode='w') as csvFile:
    for x in male_names:
        csvFile.write(x +"\n")


with open('StringData/female_names.csv', mode='w') as csvFile:
    for x in female_names:
        csvFile.write(x +"\n")


with open('StringData/names.csv', mode='w') as csvFile:
   names = male_names + female_names
   for x in names:
       csvFile.write(x + "\n")
   
    
