#loading csv file
getwd()
file_path ='./R/Energy Production Dataset.csv' #relative path
energy_data = read.csv(file= file_path)

#summary of the data
summary_data = summary(energy_data)
summary_data

# reads tge first five rows
head(x= energy_data, n=5)

#reads the last five rows
tail(x= energy_data, n=5)

#mean of every column
sapply(X= energy_data, FUN=mean)



