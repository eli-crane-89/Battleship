#Code to Run in Python
"""
import os
os.chdir(r'C:\Users\eli.crane\Documents\School\Thesis')
execfile(r'Scripts\DataFlow\InitialParsing\TerrorismData.py')
"""

#Perform Imports
import datetime
from datetime import datetime
import pandas as pd
from pandas import DataFrame

def update_progress(progress, noLevs):
    print datetime.now()
    percCompl = float(progress)/float(noLevs) * 10
    print '\r[{0}] {1}%'.format('#'*(int(percCompl)), percCompl*10)
    return progress + 1
	
print datetime.now()
prog = 1
noLevs = 3

#Define File Paths
origPath = 'Data\\OriginalData\\'
workingPath = 'Data\\WorkingData\\'
transformedPath = 'Data\\TransformedData\\'

#Import Data
df_ter = pd.read_csv(origPath + 'globalterrorismdb_0615dist.csv')

#Identify Relevant Columns
colList = ['iyear','country_txt','country_code','region_txt','nkill','nwound','gname']
df_ter = df_ter[colList]

#Add Key Column
keyList = df_ter['country_code'] + df_ter['iyear'].astype(str)
df_ter['key'] = keyList


#Create working copy of data
df_ter = df_ter.sort(['key'])
df_ter.to_csv(path_or_buf= workingPath + "MiddleData.csv")


#Calculate Number Killed Per year
nrows = len(df_ter.index)
dic_nkil = {}
curKey = df_ter.ix[0,7]	   
sum = 0

#Progress
prog = 0
noLevs = nrows
for i in range(0,nrows):
	prog = update_progress(prog,noLevs)
	
	curKey = df_ter.ix[i,7]
	if (pd.notnull(df_ter.ix[i,4])):
		if (curKey in dic_nkil.keys()):
			sum = dic_nkil[curKey] + df_ter.ix[i,4]
		else:
			sum = df_ter.ix[i,4]
		dic_nkil[curKey] = sum
    



keyList = []
uniqueCountry = []

#Progress
prog = 0
noLevs = len(df_ter)

for i in range(0,len(df_ter)):
	prog = update_progress(prog,noLevs)
	country = df_ter.ix[i,2]
	key = country + str(df_ter.ix[i,0])
	if (key not in keyList):
		keyList.append(key)
	if (country not in uniqueCountry):
		uniqueCountry.append(country)

		
df_ter = df_ter.set_index(['key'])
		
#Progress
prog = update_progress(prog,noLevs)

keyList.sort()
r = -1
while (r < len(keyList) - 1):
	r = r + 1
	if (keyList[r] in dic_nkil.keys()):
		if (pd.isnull(dic_nkil[keyList[r]])):
			keyList.remove(keyList[r])
			r = r - 1
	else:
		keyList.remove(keyList[r])

#Progress
prog = 0
noLevs = len(df_ter)

#Fill in gaps for zero death years
for j in range(0,len(uniqueCountry)):
	prog = update_progress(prog,noLevs)
	for i in range(1970,2015):
		key = uniqueCountry[j] + str(i)
		if (key not in keyList):
			keyList.append(key)
			dic_nkil[key] = 0


colList = ['nkill','country_code']
df_ter_final = pd.DataFrame(columns=colList,index=keyList)
for key in dic_nkil:
	keyRow = df_ter_final.index.get_loc(key)
	df_ter_final.iloc[keyRow,0] = dic_nkil[key]
	
df_ter_final = df_ter_final.sort_index()
keyList.sort()

df_ter_final.to_csv(path_or_buf= workingPath + "MiddleData.csv")
df_ter.to_csv(path_or_buf= workingPath + "Middle2Data.csv")

for i in range(0, len(keyList)):
	df_ter_final.iloc[i,1] = keyList[i][:3]

#Progress
prog = update_progress(prog,noLevs)

df_ter_final = df_ter_final.sort_index()
df_ter_final.to_csv(path_or_buf= transformedPath + "TerrorismData.csv")



