## Code Book 

*This document explains the use of the **run_analysis.R** script.*

# Variables
As long as the data is set within the work directory, there is no need for adding anything to the function, as it will perform automatically.

The function must be initialized by using the following call:

```
data<-runanalysis()
```

# Results
The function will return the tidy data set as required. 

# Process
The process goes like this:

- All files are read and the data is stored in variables.
- Data for features is cleaned.
- Data from the Xtest and Xtrain files is split and put into vectors.
- Vectors are merged together with corresponding data regarding activities and subjects.
- I used dplyr to play with the data and transform it as needed.

You can notice that two vectors are used to determine the functions that contain mean and standard deviations in order to filter the required data.