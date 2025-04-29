#Run_analysis function

This function takes two data.tables that should have the same format (same column names), calculates the mean and sd for each measurement based on the row labels (which could be an animal, a species, an individual, etc) and variables and returns a final data.table with these values. 

I opted for calculating both sd and mean of the instead of only the mean for the final dataset, so anyone can use this function to calculate that for any dataset they want in the future. 

The function was created as project assignment for the Getting and Cleaning Datasets course in the Data Science Specialization. 
