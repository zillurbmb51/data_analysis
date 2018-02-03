library(plyr)
options(digits=10)  #Precision upto 10 digits
df1=read.csv("Physician_Compare_2015_Individual_EP_Public_Reporting___Performance_Scores.csv")
a1=count(df1$NPI) #Count a factor
a2=count(df1$PAC.ID)
str(c(a1,a2)) #Compare the length of two types ids
df2=read.csv("Physician_Compare_National_Downloadable_File.csv")
rt=count(df2$Gender)
rt$freq[2]/rt$freq[1] #Ratio of two factors
crd=table(df2$Gender,df2$Credential) #Create table with two columns both are factor
crd[1,]/crd[2,] #Ratio of Genders across Credential
df4=read.csv("Physician_Compare_2015_Group_Public_Reporting___Performance_Scores.csv")
st2=table(df4$State)
prfnce=all1[which(all1$Measure.Performance.Rate>=10),]
sd(prfnce$Measure.Performance.Rate)
p1=merge(df1,df2,by=c("NPI","PAC.ID"))
p3=p1[which(p1$Graduation.year>=1973 & p1$Graduation.year<=2003 & p1$Measure.Performance.Rate>=10 $p1$Credential=="MD"),]
yr_mn_prfrnce=by(p3$Measure.Performance.Rate, p3$Graduation.year, mean)
m1=lm(p3$Measure.Performance.Rate ~ p3$Graduation.year)
summary(m1)
dctrs=p1[which(p1$Measure.Performance.Rate>=10 & p1$Credential == "MD"),]
nrse=p1[which(p1$Measure.Performance.Rate>=10 & p1$Credential == "NP"),]
mean(dctrs$Measure.Performance.Rate)-mean(nrse$Measure.Performance.Rate)
t.test(dctrs$Measure.Performance.Rate, nrse$Measure.Performance.Rate)
