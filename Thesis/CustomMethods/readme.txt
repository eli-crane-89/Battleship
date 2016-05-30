These two methods serve as replacements for the md.pattern method in the mice imputation package.

MDPatternFix
-Use row by row comparison to determine whether different rows are missing the same variables. This is very slow, but should work on smaller datasets.
(For comparison it took about two minutes to run on a dataset of 1175 rows and 95 variables).
-Completely accurate in differentiating rows
-Could possibly improve by implementing parallel processing

MDPatternFix2
-Uses matrix multiplication and splits the data set by column groups of 50
-Very fast
-Small chance of similar rows that are not completely the same in terms of missing variables grouping together.