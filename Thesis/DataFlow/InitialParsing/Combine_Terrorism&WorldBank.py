#Code to Run in Python
"""
import os
os.chdir(r'C:\Users\eli.crane\Documents\School\Thesis')
execfile(r'Scripts\DataFlow\InitialParsing\Combine_Terrorism&WorldBank.py')
"""

#Perform Imports
import datetime
from datetime import datetime
import pandas as pd
from pandas import DataFrame

#Define File Paths
origPath = 'Data\\OriginalData\\'
workingPath = 'Data\\WorkingData\\'
transformedPath = 'Data\\TransformedData\\'

#Get World Bank Data
df_wb = pd.read_csv(transformedPath + 'WorldBankData_EDA_MiddleEastSouthAsiaSouthEastAsia_1966_2013.csv', index_col=0)

df_wb_lastyear = df_wb.copy(deep=True)

#Add Next Year Pop Total
df_wb_lastyear['KeyDecreaseYear'] = ''
indexKey_wb = 0
indKeyDecreaseYear = len(df_wb_lastyear.columns) - 1
for i in range(0, len(df_wb_lastyear.index)):
	df_wb_lastyear.iloc[i,indKeyDecreaseYear] = df_wb_lastyear.ix[i,indexKey_wb][:3] + str(int(df_wb_lastyear.ix[i,indexKey_wb][3:]) - 1)
	
df_wb_lastyear = df_wb_lastyear[['KeyDecreaseYear','SP.POP.TOTL']]
df_wb_lastyear = df_wb_lastyear.set_index(['KeyDecreaseYear'])
df_wb_lastyear.columns.values[0] = 'SP.POP.TOTL_nextyr'

#Get Terrorism Data
df_ter = pd.read_csv(transformedPath + 'TerrorismData.csv',index_col=0)

#Add Next Year Pop Total
df_ter['KeyDecreaseYear'] = ''
ter_index = df_ter.index.tolist()
indKeyDecreaseYear = len(df_ter.columns) - 1
for i in range(0, len(df_ter.index)):
	df_ter.iloc[i,indKeyDecreaseYear] = ter_index[i][:3] + str(int(ter_index[i][3:]) - 1)

df_ter_lastyear = df_ter.copy(deep=True)
df_ter_lastyear = df_ter_lastyear[['KeyDecreaseYear','nkill']]
df_ter_lastyear = df_ter_lastyear.set_index(['KeyDecreaseYear'])
df_ter_lastyear.columns.values[0] = 'nkill_nextyear'

#MergeData
df_final = pd.merge(df_wb,df_ter, left_on='Key',right_index=True,how='inner')
df_final = pd.merge(df_final, df_wb_lastyear, left_on='Key',right_index=True,how='inner')
df_final = df_final.drop('KeyDecreaseYear', axis=1)
df_final = pd.merge(df_final, df_ter_lastyear, left_on='Key',right_index=True,how='inner')

#Create csv
df_final.to_csv(workingPath + 'InitialMerge.csv')

#Add Columns
df_final['nkillpercap_nextyr'] = df_final['nkill_nextyear']/df_final['SP.POP.TOTL_nextyr'] * 100000

df_final = df_final.set_index(['Key'])



#Drop columsn 
dropList = ['country_code','nkill','nkill_nextyear','sp.pop.totl_nextyr']
dropIndex = []
for i in range(0,len(df_final.columns)):
	curCol = str(df_final.columns.values[i])
	curCol = curCol.lower()
	for h in range(0,len(dropList)):
		if (dropList[h] == curCol):
			if (i not in dropIndex):
				print df_final.columns[i]
				dropIndex.append(i)	

print dropIndex
df_final = df_final.drop(df_final.columns[dropIndex],axis=1)


#Create csv
df_final.to_csv(transformedPath + 'Combined_EDA_Complete.csv')
