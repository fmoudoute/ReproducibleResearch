## Week 2 Reproducible Research Project

library(varhandle)


# Loading Data 
df <- read.csv("C:/Users/Frantz/CloudStation/Documents/Coursera/ReproducibleResearch/activity.csv", header = TRUE)
df_Safe <- df
       
#---------------------------------------WITHOUT NAs
#Cleaning Missing points
df <- na.omit(df)

#Verifying Data Type
str(df)

#Converting data in incorrect format 
df$date <- unfactor(df$date)
df$date <- as.Date(as.character(df$date, format = "%d/%m/%y"))
df$steps <- as.numeric(df$steps)
df$interval <- as.numeric(df$interval)


#Caculate the total number of step per day
Data_Table_Step_per_Day <- aggregate(df$steps, list(df$date), sum)
colnames(Data_Table_Step_per_Day) <- c("Date", "TotalSteps")
print(head(Data_Table_Step_per_Day))

#Calculate the mean & Median of the number of total step per day
Mean_Total_Nber_Step_Per_Day <- mean(Data_Table_Step_per_Day$TotalSteps)
print(paste("Mean Total Number of Steps Taken Per day : ", round(Mean_Total_Nber_Step_Per_Day, digits = 2), sep =""))
Median_Total_Nber_Step_Per_Day <- median(Data_Table_Step_per_Day$TotalSteps)
print(paste("Median Total Number of Steps Taken Per day : ", round(Median_Total_Nber_Step_Per_Day, digits = 2), sep =""))


#Histogram of total number of step taken each day 
qplot(Data_Table_Step_per_Day$Date, Data_Table_Step_per_Day$TotalSteps, data= Data_Table_Step_per_Day)

ggplot(Data_Table_Step_per_Day, aes(x =Date, y = TotalSteps)) + 
  geom_bar(stat = "identity") +
  ggtitle("Total Steps Per Day") +
  labs(x="Date", y="Total Steps Per Day")


#Average daily activity Pattern
#Calculating the Steps per interval's ID
Data_Table_Step_per_Interval <- aggregate(df$steps, list(df$interval), mean)
colnames(Data_Table_Step_per_Interval) <- c("IntervalID", "TotalSteps")

#Plotting the Steps per Time Iterval
ggplot(Data_Table_Step_per_Interval, aes(x =IntervalID, y = TotalSteps)) + 
  geom_bar(stat = "identity") +
  ggtitle("Total Steps Per Time Interval") +
  labs(x="Time interval", y="Total Steps Per Day")

#Find the max average interval
MaxStep <- max(Data_Table_Step_per_Interval$TotalSteps)
IDMAxstep <- which(Data_Table_Step_per_Interval[,2] == MaxStep)
Interval_IDMAxstep <- Data_Table_Step_per_Interval$IntervalID[IDMAxstep]
print(paste("In average the 5-minute interval with the maximum quantity of steps is Interval Number: ", round(Interval_IDMAxstep, digits = 0), sep =""))

#---------------------------------------WITH NAs

#Find how many NAs
Total_NA_Days <- sum(is.na(df_Safe$date))
Total_NA_Steps <- sum(is.na(df_Safe$steps))
Total_NA_interval <- sum(is.na(df_Safe$interval))
ifelse(Total_NA_Days, paste(Total_NA_Days, " rows in the Day column have NA", sep = ""), paste("There are no NAs in the Day Column", sep = ""))
ifelse(Total_NA_Steps, paste(Total_NA_Steps, " rows in the steps column have NA", sep = ""), paste("There are no NAs in the steps Column", sep = ""))
ifelse(Total_NA_interval, paste(Total_NA_interval, " rows in the interval column have NA", sep = ""), paste("There are no NAs in the interval Column", sep = ""))

df_Safe$date <- unfactor(df_Safe$date)
df_Safe$date <- as.Date(as.character(df_Safe$date, format = "%d/%m/%y"))
df_Safe$steps <- as.numeric(df_Safe$steps)
df_Safe$interval <- as.numeric(df_Safe$interval)


df_Safe[is.na(df_Safe)]<-"Missing"


for(i in 1:nrow(df_Safe)){
  if(df_Safe$steps[i] == "Missing"){
    
    #Find at which interval of the day the missing value happened
    WhichINterval <- df_Safe$interval[i]
    
    #Find the average number of steps at that interval
    Average_at_IntervalID <- which(Data_Table_Step_per_Interval[,1] == WhichINterval)
    Average_at_interval <- Data_Table_Step_per_Interval$TotalSteps[Average_at_IntervalID]
    
    #Replacing NAs by the average at that interval
    df_Safe$steps[i] <- round(Average_at_interval, digits = 2)
      }
  }
#New Data set equal to the original dataset but with the missing data filled 
New_df <- df_Safe
New_df$steps <- as.numeric(New_df$steps)


#Caculate the total number of step per day
Data_Table_Step_per_Day_Modified <- aggregate(New_df$steps, list(New_df$date), sum)
colnames(Data_Table_Step_per_Day_Modified) <- c("Date", "TotalSteps")
print(head(Data_Table_Step_per_Day_Modified))

#Calculate the mean & Median of the number of total step per day
Mean_Total_Nber_Step_Per_Day_Modified <- round(mean(Data_Table_Step_per_Day_Modified$TotalSteps), digits = 2)
print(paste("Mean Total Number of Steps Taken Per day After replacing Missing Data : ", round(Mean_Total_Nber_Step_Per_Day_Modified, digits = 0), sep =""))
Median_Total_Nber_Step_Per_Day_Modified <- round(median(Data_Table_Step_per_Day_Modified$TotalSteps), digits = 2)
print(paste("Median Total Number of Steps Taken Per day After replacing Missing Data : ", round(Median_Total_Nber_Step_Per_Day_Modified, digits = 0), sep =""))


#Histogram of total number of step taken each day 
qplot(Data_Table_Step_per_Day_Modified$Date, Data_Table_Step_per_Day_Modified$TotalSteps, data= Data_Table_Step_per_Day_Modified)

ggplot(Data_Table_Step_per_Day_Modified, aes(x =Date, y = TotalSteps)) + 
  geom_bar(stat = "identity") +
  ggtitle("Total Steps Per Day (Replaced Missing Data)") +
  labs(x="Date", y="Total Steps Per Day")


#Produce Table With comparaison
Col1 <- c("Variable","Mean","Median")
Col2 <- c("Without Replacing NA", Mean_Total_Nber_Step_Per_Day, Median_Total_Nber_Step_Per_Day)
Col3 <- c("Replacing NA", Mean_Total_Nber_Step_Per_Day_Modified, Median_Total_Nber_Step_Per_Day_Modified)
Col4 <- c("Impact of Replacing NA", round(Mean_Total_Nber_Step_Per_Day_Modified-Mean_Total_Nber_Step_Per_Day, digits = 2) , round(Median_Total_Nber_Step_Per_Day_Modified-Median_Total_Nber_Step_Per_Day,digits = 2))
TableResultat <- data.frame(Col1, Col2, Col3, Col4)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
TableResultat <- header.true(TableResultat)

print(TableResultat)

# Finding differences in activity Patterns between weekdays and weekends
# Creating a column for week day and week ends
New_df$WeekDay <- 0

for(i in 1:nrow(New_df)){
  
  if(weekdays(New_df$date[i]) != "Saturday" || weekdays(New_df$date[i]) != "Sunday"){
    New_df$WeekDay[i]<- "WeekDays"
  }
    if(weekdays(New_df$date[i]) == "Saturday" || weekdays(New_df$date[i]) == "Sunday"){
      New_df$WeekDay[i]<- "Weekend"
  }
 
}


#Calculating the Steps per interval's ID
Data_Table_Step_per_Interval_new <- aggregate(New_df$steps, list(df$interval), mean)
colnames(Data_Table_Step_per_Interval_new) <- c("IntervalID", "TotalSteps")

#Plotting the Steps per Time Iterval
ggplot(Data_Table_Step_per_Interval, aes(x =IntervalID, y = TotalSteps)) + 
  geom_bar(stat = "identity") +
  ggtitle("Total Steps Per Time Interval") +
  labs(x="Time interval", y="Total Steps Per Day")













