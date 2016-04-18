## Code Book

*This document explains the use of the **run_analysis.R** script.*

# Variables
These only consist of the relative location for the files used by the script. Note that the script asumes as default values as if you have put the dataset's original folder into your work folder.

The function must be initialized by using the following call:

```
runanalysis<-function(dataA, dataB, dataC, dataD, dataE, dataF)
```

# Results
The function will return a list with the following data:

1. The merged train and test data sets.
2. A summary table by activity, showing columns for mean and standard deviation.
3. A summary table by subject, showing columns for mean and standard deviation.
4. A tidy (hopefully!) table as required by step 5 of the project.

# Process
The process goes like this:

- All files are read.
- Data is cleaned and separated into vectors.
- Vectors are merged together with corresponding data regarding activities and subjects.
- I used dplyr to play with the data and transform it as needed.
- Finally, the required answers are put into a list and returned.