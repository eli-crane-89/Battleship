using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Data;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace ThesisDataTransposer
{
    class CSVParser
    {
        private string grouping;
        private string dataName;
        private string fileName;
        private string filePath;
        private int minItemCount;

        public CSVParser(string dataName,string grouping,string fileName,string filePath, int minItemCount)
        {
            this.grouping = grouping;
            this.dataName = dataName;
            this.fileName = fileName;
            this.filePath = filePath;
            this.minItemCount = minItemCount;
        }

        public void createCSV()
        {
            string fullFilePath = filePath + fileName;
            StreamReader sr = new StreamReader(fullFilePath);
            List<string[]> lines = new List<string[]>();
            List<string> strLine = new List<string>();
            string key;
            string sortKey;
            int indexSpace = 0;

            sr.ReadLine();

            while (!sr.EndOfStream)
            {              
                string[] Line = sr.ReadLine().Split(',');
                if (Line[3] != "")
                {

                    int lenLine = Line.GetLength(0);
                    foreach (string s in Line)
                    {
                        strLine.Add(s);
                    }

                    indexSpace = strLine[3].IndexOf(" ");
                    if (indexSpace == -1)
                    {
                        indexSpace = strLine[3].Length;
                    }

                    key = strLine[2].Substring(0, 2) + strLine[3].Substring(0, indexSpace);
                    sortKey = key + " " + strLine[0];
                    strLine.Add(key.ToLower());
                    strLine.Add(sortKey.ToLower());

                    string[] finalLine = strLine.ToArray();
                    lines.Add(finalLine);
                    strLine.Clear();
                }
               
            }
            string[][] data = lines.ToArray();
            //int l = data.GetLength(1);

            writeStringBuilder(filePath, "AssociationData.arff", buildFile(data, grouping, extractHeaders(data, filePath), dataName));
        }

        private List<string[]> extractHeaders(string[][] csv, string filePath)
        {
            int lenCSV = csv[0].Length;
            int lRow = csv.GetLength(0);
            Sort<string>(csv, lenCSV-1);

            List<string> headers = new List<string>();
            List<string[]> headerRow = new List<string[]>();
            string item = "";
            string curGroup = "";
            string key = csv[0][lenCSV - 2];
            bool newKey;
            int keyCount = 0;
            string[] headerArray = new string[4];

            int i = 0;
            while (i < lRow)
            {
                if (curGroup == csv[i][0])
                {
                    keyCount--;
                }

                if (csv[i][lenCSV - 2] != key && keyCount >= minItemCount)
                {
                    headerArray[2] = item;
                    headerArray[3] = keyCount.ToString();
                    headerRow.Add(headerArray);
                    headerArray = new string[4];
                    keyCount = 0;
                }

                item = csv[i][3];
                key = csv[i][lenCSV - 2];
                newKey = true;
                curGroup = csv[i][0];
                //string lastItem = csv[i][3].ToString();

                if (i == 0)
                {
                    headers.Add(key);
                    headerArray[0] = key;
                    headerArray[1] = item;
                }
                else
                {
                    foreach (string s in headers)
                    {
                        if (s == key)
                        {
                            newKey = false;
                            break;
                        }
                    }
                    if (newKey)
                    {
                        headers.Add(key);
                        headerArray[0] = key;
                        headerArray[1] = item;
                        keyCount = 0;
                    }
                }

                keyCount++;
                i++;
                if (i == lRow - 1)
                {
                    headerArray[2] = item;
                    headerArray[3] = keyCount.ToString();
                    headerRow.Add(headerArray);
                }
                
            }

            StringBuilder sbHeadFile = new StringBuilder();
            string newLine;
            sbHeadFile.AppendLine("Key,FirstHeader,FinalHeader,Count");

            foreach (string[] s in headerRow)
            {
                newLine = "";
                for(int c = 0; c < s.GetLength(0);c++)
                {
                     newLine = newLine + s[c] + ",";
                }
                
                sbHeadFile.AppendLine(newLine);
            }
            writeStringBuilder(filePath, "HeaderFile.csv", sbHeadFile);

            return headerRow;
        }

        private StringBuilder buildFile(string[][] csv, string grouping, List<string[]> headers, string dataName)
        {
            
            StringBuilder sbFinal = new StringBuilder();
            sbFinal.AppendLine(@"@relation " + dataName);
            sbFinal.AppendLine("");

            foreach (string[] s in headers)
            {
                sbFinal.AppendLine(@"@attribute '" + s[0] + "' {1,0}");
            }

            sbFinal.AppendLine("");
            sbFinal.AppendLine(@"@data");

            string curGroup = "";
            int intGroupCol;
            if (grouping == "ByAccount")
            {
                intGroupCol = 0;
            }
            else
            {
                intGroupCol = 1;
            }

            Sort<string>(csv, intGroupCol);
            string[] line = new string[headers.Count];
            List<int> itemPlaces = new List<int>();
            List<string> listKeys = new List<string>();

            int i = 0;
            while (i < csv.GetLength(0))
            {
                int k = 0;
                while (k < headers.Count)
                {
                    line[k] = "0";
                    k++;
                }

                curGroup = csv[i][intGroupCol];
                while (i < csv.GetLength(0) && csv[i][intGroupCol] == curGroup)
                {
                    listKeys.Add(csv[i][csv[0].Length - 2]);
                    curGroup = csv[i][intGroupCol];
                    i++;

                }

                foreach (string it in listKeys)
                {
                    for (int j = 0; j < headers.Count; j++)
                    {
                        if (it == headers[j][0])
                        {
                            if (!itemPlaces.Contains(j))
                            {
                                itemPlaces.Add(j);
                            }
                        }
                    }
                }

                foreach (int ip in itemPlaces)
                {
                    line[ip] = "1";
                }

                sbFinal.AppendLine(convertStringArrayToString(line));
                itemPlaces.Clear();
                listKeys.Clear();
            }

            return sbFinal;
        }

        private static T[][] Sort<T>(T[][] data, int col)
        {
            Comparer<T> comparer = Comparer<T>.Default;
            Array.Sort<T[]>(data, (x, y) => comparer.Compare(x[col], y[col]));

            return data;
        }

        private void writeStringBuilder(string filePath, string fileName, StringBuilder sb)
        {
            File.WriteAllText(filePath + fileName, sb.ToString());
        }

        private string convertStringArrayToString(string[] array)
        {
            StringBuilder sb = new StringBuilder();
            foreach (string s in array)
            {
                sb.Append(s + ",");
            }

            return sb.ToString();
        }

        
    }
}
