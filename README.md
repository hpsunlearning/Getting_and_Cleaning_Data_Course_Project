# Getting_and_Cleaning_Data_Course_ProjectThis is for assignment of the Course Getting and Cleaning Data.    The purpose is to make a Clean and understandable  dataset of the smart phone records of human activities.  the original dataset is in folder "UCI HAR Dataset", detail can  be found in this paper: Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012  And the original dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.  this repository contain:1. original dataset folder "UCI HAR Dataset"2. "run_analysis.R" showing how to process the original data3. "CodeBook.md" showing explaination of each variables of the tidy data4. "mean_data.txt" containing the tidy data5. "README.md" just this fileThe dataset is processed by run_analysis.R script, new dataset mean_data.txt is generalized by this script, extracted only includes the mean and standard deviation for each measurement from both trainning and testing data, calculated average of each feature by each subject and each activity.The detailed variable names are in codebook.md