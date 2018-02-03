library(plyr)
options(digits=10)  #Precision upto 10 digits
df1=read.csv("Physician_Compare_2015_Individual_EP_Public_Reporting___Performance_Scores.csv")
a1=count(df1$NPI) #Create table with occurance of each label
a2=count(df1$PAC.ID)
str(c(a1,a2)) #Compare between two length
df2=read.csv("Physician_Compare_National_Downloadable_File.csv")
rt=count(df2$Gender)
rt$freq[2]/rt$freq[1] #Ratio of each label (male to female)
crd=table(df2$Gender,df2$Credential) #Table with two factor column
crd[1,]/crd[2,] #Ratio of female to male in each credential
df4=read.csv("Physician_Compare_2015_Group_Public_Reporting___Performance_Scores.csv")
st2=table(df4$State)
lst1=list(df1,df3,df4)
df5=merge_recurse(lst1)
pr1=df5[which(df5$Measure.Performance.Rate>=10),] #Subset the data with a condition
sd(by(pr1$Measure.Performance.Rate, pr1$NPI, mean)) #SD of mean peroformance across all NPI
df6=merge(df2,df5,by=c("NPI","PAC.ID"))
#Subset data with multiple condition
mpr=df6[which(df6$Graduation.year>=1973 & df6$Graduation.year<=2003 & df6$Measure.Performance.Rate>=10 & df6$Credential=="MD"),]
yr_mn_prfrnce=by(mpr$Measure.Performance.Rate, mpr$Graduation.year, mean) #Mean performance per graduation year
m1=lm(mpr$Measure.Performance.Rate ~ mpr$Graduation.year) #Linear regression of performance vs year
summary(m1)
dctrs=df6[which(df6$Measure.Performance.Rate>=10 & df6$Credential == "MD"),]
nrse=df6[which(df6$Measure.Performance.Rate>=10 & df6$Credential == "NP"),]
mean(dctrs$Measure.Performance.Rate)-mean(nrse$Measure.Performance.Rate) #Difference of mean performance of doctors and nurse
t.test(dctrs$Measure.Performance.Rate, nrse$Measure.Performance.Rate) #Welch Two Sample t-test

