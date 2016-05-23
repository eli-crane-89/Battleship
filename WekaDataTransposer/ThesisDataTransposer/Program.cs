using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ThesisDataTransposer
{
    class Program
    {  
        private static string wekaFileName = "PurchasingData";
        private static string grouping = "ByAccount";
        private static string dataFile = "FirstPull_Test.csv";
        private static string wd = @"C:\Users\eli.crane\Documents\School\Thesis\Data\";
        private static int minItemCount = 100;

        static void Main(string[] args)
        {
            CSVParser csvParser = new CSVParser(wekaFileName,grouping,dataFile,wd,minItemCount);
            csvParser.createCSV();
        }
    }
}
