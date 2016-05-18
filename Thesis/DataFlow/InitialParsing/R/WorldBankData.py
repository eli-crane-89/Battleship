#Code to Run in Python
#Perform Imports
import datetime
from datetime import datetime
import pandas as pd
from pandas import DataFrame
#from copy import deepcopy
import numpy as np

print(datetime.today())

def update_progress(progress, noLevs):
    print(datetime.now())
    percCompl = float(progress)/float(noLevs) * 10
    print('\r[{0}] {1}%'.format('#'*(int(percCompl)), percCompl*10))
    return progress + 1

#Set working directory
#os.chdir('home/ubuntu/Thesis/Scripts/DataFlow/InitialParsing')

#Define File Paths
rootDir = '/home/ubuntu/Thesis/'
origPath = rootDir + 'Data/OriginalData/'
workingPath = rootDir + 'Data/WorkingData/'
transformedPath = rootDir + 'Data/TransformedData/'

#Get FileName
fileString = 'WDI_MiddleEastSouthAsiaSouthEastAsia_1966_2014'
uScore = fileString.index('_')
uniqueString = fileString[uScore+1:]

#Import Data
df_wb = pd.read_csv( origPath + '%s.csv'%(fileString))


#Create Important Variables
numCol = len(df_wb.columns)
numRow = len(df_wb.index)

#Create list of variables
varList = []
for i in range(0,numRow):
	if (df_wb.ix[i,3] not in varList):
		varList.append(df_wb.ix[i,3])

cols = ['isPop','total', 'percent']
df_vars = pd.DataFrame(index=varList,columns=cols)


#Get Unique Countries
uniqueCountryList = []
for i in range(0,len(df_wb)):
	if (df_wb.ix[i,1] not in uniqueCountryList):
		uniqueCountryList.append(df_wb.ix[i,1])
numCountries = len(uniqueCountryList)

#Create Series Dictionary
dictSeries = {}
for i in range(0,len(df_wb)):
	if (df_wb.ix[i,3] not in dictSeries):
		dictSeries[df_wb.ix[i,3]] = df_wb.ix[i,2]


###################Tabulate totals####################
#Progress Numbers
prog = 1
noLevs = len(varList)-1320
		
#Get Variable Counts
isPopCount = 0
total = 0
percent = .00

#loop var
curYear = 0

for h in range(0, noLevs):
	if (h > -1 and h < noLevs):
		prog = update_progress(prog, noLevs)
		for i in range(0,numRow):
			#print df_wb.ix[i,3]
			#print varList[h]
			#print h
			if (df_wb.ix[i,3] == varList[h]):
				for j in range(4,numCol):
					curYear = int(df_wb.columns[j][2:])
					if (df_wb.ix[i,j] != ".."):
						isPopCount = isPopCount + 1
					total = total + 1

		keyRow = df_vars.index.get_loc(varList[h])
		df_vars.iloc[keyRow,0] = isPopCount
		df_vars.iloc[keyRow,1] = total
		percent = isPopCount * 1.00 / total * 1.00
		df_vars.iloc[keyRow,2] = percent
		
		#Reset Vars
		isPopCount = 0
		total = 0
		percent = .00


#print df_vars
		
df_vars.to_csv(path_or_buf= transformedPath + "WorldBankValueCounts_%s.csv"%(uniqueString))

print(datetime.today())

###################End Tabulate totals####################