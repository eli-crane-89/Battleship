#Code to Run in Python
"""
import os
os.chdir(r'C:\Users\eli.crane\Documents\School\Thesis')
execfile(r'Scripts\DataFlow\InitialParsing\WorldBankData.py')
"""

#Perform Imports
import datetime
from datetime import datetime
import pandas as pd
from pandas import DataFrame
from copy import deepcopy
import numpy as np

print datetime.today()

def update_progress(progress, noLevs):
    print datetime.now()
    percCompl = float(progress)/float(noLevs) * 10
    print '\r[{0}] {1}%'.format('#'*(int(percCompl)), percCompl*10)
    return progress + 1
	
#Define File Paths
origPath = 'Data\\OriginalData\\'
workingPath = 'Data\\WorkingData\\'
transformedPath = 'Data\\TransformedData\\'

"""
########Start Program#########
#Get FileName
fileString = 'WDI_MiddleEastSouthAsiaSouthEastAsia_1966_2013'
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
"""
"""
###################Tabulate totals####################
#Progress Numbers
prog = 1
noLevs = len(varList)
		
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

print datetime.today()

###################End Tabulate totals####################
"""

"""
df_vars = pd.read_csv(transformedPath + "WorldBankValueCounts_%s.csv"%(uniqueString), index_col=0)
df_vars = df_vars[df_vars.percent >= .80 ] #& (df_vars.delta < .03)) | (df_vars.percent90 >= .9)]

############Transform daa into panel dataframe###########
#Fill columns list with placeholder variables
keyList = list(df_vars.index.values)
colList = []
for i in range(0,len(keyList) + 1):
	colList.append("i")

colList[0] = 'Key'
for i in range(0,len(keyList)):
	colList[i+1] = keyList[i]
	
	
#print df_wb
wbColList = list(df_wb.columns.values)
df_final = pd.merge(df_wb, df_vars,left_on='SeriesCode',right_index=True,how='inner')
df_final = df_final[wbColList]
print len(colList)

df_trans = pd.DataFrame(index=range(0,50*numCountries),columns=colList)

dic_colindx={}
for i in range(0,len(colList)):
	dic_colindx[str(colList[i])] = i

k = 0
curConKey = df_wb.ix[0,1]


#Reset progress
progress = 1
noLevs = len(df_wb.index)

#Run Loop to transform data
for i in range(0,len(df_wb.index)):
	progress = update_progress(progress, noLevs)
	key = str(df_wb.ix[i,3])
	if (curConKey != df_wb.ix[i,1]):
		k = k + len(wbColList) - 4
		curConKey = df_wb.ix[i,1]
	if (key in dic_colindx.keys()):
		for j in range(4,len(df_wb.columns)):
			conKey = str(df_wb.ix[i,1]) + wbColList[j][2:]
			#print j - 4
			if (df_wb.ix[i,j] != '..'):
				df_trans.iloc[j+k-4,dic_colindx[key]] = df_wb.ix[i,j]
			df_trans.iloc[j+k-4,0] = conKey

listNoTotalPop = pd.isnull(df_trans['SP.POP.TOTL'])
df_trans = df_trans.drop(df_trans.index[listNoTotalPop])
df_trans = df_trans.reset_index(drop=True)

#Create Middle data
df_trans.to_csv(path_or_buf= workingPath + "TransWorldBankData_MiddleStage_%s.csv"%(uniqueString))
"""


#Get Middle Data
df_trans = pd.read_csv(workingPath + "TransWorldBankData_MiddleStage_%s.csv"%(uniqueString),index_col=0)

#######Remove Perfectly Correlated Variables########
corrMatrix = df_trans.corr(method='pearson',min_periods=700)

lenMatrix = len(corrMatrix.index)
axisCorr = 0
dropList = []
protectList = []
protectList.append('Key')
for i in range(0,lenMatrix):
	curProtectCol = corrMatrix.columns.values[i]
	for j in range(0, lenMatrix):
		curDropCol = corrMatrix.columns.values[j]
		if (i != axisCorr or j != axisCorr):
			if (abs(abs(corrMatrix.ix[i,j]) - 1) < .00000000001):
				if (curProtectCol not in protectList and curProtectCol not in dropList):
					protectList.append(curProtectCol)
				if (curDropCol not in protectList and curDropCol not in dropList):
					dropList.append(curDropCol)
	if (curProtectCol not in protectList and curProtectCol not in dropList):
		protectList.append(curProtectCol)					
	axisCorr = axisCorr + 1

df_trans = df_trans[protectList]

#Reset progress
progress = 1
noLevs = len(df_trans.index)

###########Remove Rows That Are Less Than 80% Complete###########
noCols = len(df_trans.columns)
dropIndex = []

for i in range(0, len(df_trans.index)):
	progress = update_progress(progress, noLevs)
	missingCount = 0
	for j in range(0, noCols):
		curVal = df_trans.ix[i,j]
		if (pd.isnull(curVal)):
			missingCount = missingCount + 1
	missingPerc =  missingCount * 1.00 /noCols * 1.00
	if (missingPerc > .2):
		dropIndex.append(i)
			
df_trans = df_trans.drop(df_trans.index[[dropIndex]])
df_trans = df_trans.reset_index()
df_trans = df_trans.drop('index',axis=1)

#Create Middle data
df_trans.to_csv(path_or_buf= workingPath + "TransWorldBankData_MiddleStage_AfterRowDrop_%s.csv"%(uniqueString))

"""
##########Transform variables to per capita#########
#Reset progress
progress = 1
noLevs = len(df_trans.columns)

popCol = len(df_trans.columns) + 1
i = -1
while (popCol == len(df_trans.columns) + 1):
	i = i + 1
	if (df_trans.columns.values[i] == 'SP.POP.TOTL'):
		popCol = i


dictPopByKey = {}
for i in range(0,len(df_trans.index)):
	dictPopByKey[df_trans.ix[i,0]] = df_trans.ix[i,popCol]

listBan = [' per ','%','population, total']	
for j in range(0, len(df_trans.columns)):
	progress = update_progress(progress, noLevs)
	boolTransform = 1
	curCol = df_trans.columns.values[j]
	if (curCol == 'Key'):
		boolTransform = 0
		indexKey = j
	else:
		for h in range(0, len(listBan)):
			if (listBan[h] in dictSeries[curCol].lower()):
				boolTransform = 0
				break
		
	if (boolTransform == 1):
		for i in range(0,len(df_trans.index)):
			key = df_trans.ix[i,0]
			if (key in dictPopByKey):
				popValBy100 = float(dictPopByKey[key]) / 100000.00
				df_trans.iloc[i,j] = str(float(df_trans.iloc[i,j]) / float(popValBy100))


#Create Middle data
df_trans.to_csv(path_or_buf= workingPath + "TransWorldBankData_MiddleStage2_%s.csv"%(uniqueString))
"""

"""
#Get Middle Data
df_trans = pd.read_csv(workingPath + "TransWorldBankData_MiddleStage2_%s.csv"%(uniqueString),index_col=0)




#Create Middle data
df_trans.to_csv(path_or_buf= transformedPath + "WorldBankData_ReadyForImputation_%s.csv"%(uniqueString))
"""

"""
# Imputation Ready Data
df_trans = pd.read_csv(transformedPath + "WorldBankData_ReadyForImputation_%s.csv"%(uniqueString),index_col=0)


################Create lags######################
#Find SP.POP.TOTL column
for i in range(0, len(df_trans.columns)):
	if (df_trans.columns.values[i] == 'SP.POP.TOTL'):
		indexPopTotl = i

indexKey = 0
lagList = [3,5,10]
		
df_trans_final = deepcopy(df_trans)
for i in range(1,11):
	if (i in lagList):
		print str(i)
		df_lag = deepcopy(df_trans)
		df_lag = df_lag.drop(df_lag.columns[indexPopTotl], axis=1)
		colName = 'Key_plus' + str(i) + 'yr'
		df_lag[colName] = ''
		colIndex = len(df_lag.columns) - 1
		for j in range(0, len(df_trans.index)):
			df_lag.iloc[j,colIndex] = df_lag.ix[j,indexKey][:3] + str(int(df_lag.ix[j,indexKey][3:]) + i)
		
		df_lag = df_lag.drop(df_lag.columns[indexKey],axis=1)
		
		for h in range(0,len(df_lag.columns)):
			curCol = str(df_lag.columns.values[h])
			if ('key' not in curCol.lower()):
				df_lag.columns.values[h] = curCol + '_lag_' + str(i) + 'yr'
		
		df_trans_final = pd.merge(df_trans_final, df_lag, left_on='Key',right_on = colName,how='inner')
		colIndex = len(df_trans_final.columns) - 1
		print df_trans_final.columns.values[colIndex]
		df_trans_final = df_trans_final.drop(df_trans_final.columns[colIndex], axis=1)


#Create Middle data
#df_trans_final.to_csv(path_or_buf=r"C:\Users\eli.crane\Documents\School\Thesis\Data\TransWorldBankData_MiddleStage4_%s.csv"%(uniqueString))


df_trans_final.to_csv(path_or_buf= transformedPath + "WorldBankData_EDA_%s.csv"%(uniqueString))
print datetime.today()
"""

