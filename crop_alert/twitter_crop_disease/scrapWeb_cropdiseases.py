# -*- coding: utf-8 -*-
"""
Created on Mon May 21 17:10:18 2018

@author: swaggyp1985
"""

from simple_get import simple_get
from bs4 import BeautifulSoup

url = "https://www.apsnet.org/publications/commonnames/Pages/Corn.aspx"
url = "https://www.apsnet.org/publications/commonnames/Pages/Soybean.aspx"


raw_html = simple_get(url)


html = BeautifulSoup(raw_html, 'html.parser')


category = []
common_name = []
spp = []

for each in html.find_all('dt'):
    print(each)
    name = each.text.strip()
    common_name.append(name)
    

    
    
common_name


fh = open('Corn_diseases.lst', 'w')
for i in common_name:
    i = i.lower()
    print(i, sep='', end='\n', file=fh)
    
fh = open('Soy_diseases.lst', 'w')
for i in common_name:
    i = i.lower()
    print(i, sep='', end='\n', file=fh)