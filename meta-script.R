install.packages(c("robumeta","metafor","dplyr"))
library("robumeta")
library("metafor")
library("dplyr")
library("readxl")
test<-read_excel("C:/Users/elune/Desktop/test.xlsx")
View(test)
test<-mutate(test, study_id=1:28)
View(test)
test<-test%>%select(study_id, "Author":"ES-Delay") 
View(test)

smd_test<-data.frame(
  id=c(1:28),
  author=c("Arthur&Dawn, 2016", n
          "Baker et al., 2013",
          "Coyne et al., 2007",
          "Coyne et al., 2007",
          "Coyne et al., 2010",
          "Crevecoeur et al.,2014",
          "Fien et al., 2011",
          "Fricke et al., 2011",
          "Gonzalez et al., 2011",
          "Loftus et al., 2010",
          "Maynard et al., 2010",
          "Messier et al., 2015",
          "Mirza et al., 2010",
          "Nelson et al., 2011",
          "Neuman et al., 2011",
          "Neuman et al., 2013",
          "Neuman et al., 2015",
          "Pollard-Durodola et al., 2011",
          "Puhalla et al., 2011",
          "Pullen et al., 2011",
          "Silverman et al., 2013",
          "Silverman & Hines, 2009",
          "Spycher, 2009",
          "Tuckwiller, 2010",
          "Vadacy et al., 2011",
          "Vadacy et al., 2015",
          "Zipoli et al., 2011",
          "Zucker et al., 2013"),
  n2=c(154, 103, 15, 16,43, 42, 50, 90, 56, 43, 112, 18, 13, 92, 89, 108, 70, 56, 22,43, 88, 50, 20, 48, 66, 163, 40, 268),
  n1=c(92, 122, 16, 16, 79, 78, 52, 90, 92, 43, 112, 18, 9, 93, 89, 108, 73, 69, 44, 44, 85, 35, 19, 54, 74, 161, 40, 316),
  m2=c(5.73, 20.45, 3.32, 3.26, 92.51, 92.74, 12.73, 21.87, 92.29, 1.8, 4.35, 10.22, 61.38, 104.54,18.65, 0.89, 99.96, 84.3, 35.15, 5.43, 96.11, 90.7, 10.9, 12.31, 103.8, 80, 10.99, 19),
  m1=c(12.39, 12.8, 4.81, 4.84, 98.99, 98.99, 18.71, 29.13, 95.04, 3.45, 5.31, 11.11, 69.33, 106.65, 20.29,0.89, 101.47, 85.61, 44.68, 6.09, 85.69, 89.6, 15, 14.31, 108, 81, 21.47, 21.5),
  sd2=c(3.64, 10.22, 1.25, 1.09, 10.43, 10.45, 7.48, 7.46, 11.16, 1.36, 1.58, 1.43, 12.23, 9.31, 10.05, 0.11, 13.33, 14, 15.68, 1.55, 16.09, 24.2, 3, 0.55, 12.2, 13.5, 9.75, 6.41),
  sd1=c(4.91, 29.11, 1.11, 1.1, 13.68, 13.68, 10.26, 9.13, 8.75, 0.84, 1.02, 0.96, 14.34, 9.27,10.09, 0.12, 14.18, 12.51, 18.87, 1.96, 15.68, 18.1, 3.37, 1.08, 11.9, 13.67, 13.98, 10.8),
  duration=c(21, 19,1,1,18, 18, 8, 30, 18, 2, 1, 10, 2, 20, 16, 8, 12, 12, 12, 2, 12, 12,5, 8, 20, 12, 18, 4),
  measurement=c(0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0),
  grade=c(1,3,2,2,2,2,3,1,1,2,3,3,2,2,1,1,1,1,3,3,1,1,2,2,2,3,2,1),
  status=c(3,1,1,3,1,2,3,3,1,3,1,2,2,2,1,3,3,3,2,1,2,2,3,2,2,1,3,2),
  Theme=c(0,0,1,1,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,1,1,0,1,0))
  
View(smd_test)
#control-2 

smd_test<-escalc(measure="SMD", m1i=m1, m2i=m2, sd1i=sd1, sd2i=sd2, n1i=n1, n2i=n2, data=smd_test)
View(smd_test)

res<-rma(yi, vi, data=smd_test, slab=author)
res
predict(res, digits=3, trans=transf.ztor)
confint(res)

b_res<-rma(yi, vi, data=smd_test, slab=id)
baujat(b_res)

inf<-influence(res)
print(inf)
plot(inf)

forest(res)

forest(res,xlim=c(-1.6, 1.6), atransf=transf.ztor, 
       at=transf.rtoz(c(- .6, - .3, 0, 0.3, .6)), 
       digits=c(2,2), cex=1,pch=15)
text(-1.6, 30, "Author(s), Year", pos=4, cex=1)
text(2.3, 30, "95% Confidence Interval",pos=2, cex= 1)

## Funnel Plot

funnel(res, xlab="95%Confidence Interval")

# Bias
regtest(res)
rantest(res)

#Moderators
res.modduration<-rma(yi, vi, mods= ~ duration, data=smd_test)
res.modduration

res.modmeasurement<-rma(yi,vi, mods=~measurement, data=smd_test)
res.modmeasurement

res.modgrade<-rma(yi,vi, mods=~grade, data=smd_test)
res.modgrade


res.modstatus<-rma(yi,vi, mods=~status, data=smd_test)
res.modstatus

res.modTheme<-rma(yi,vi, mods=~Theme, data=smd_test)
res.modTheme


## Delayed smd_test<-data.frame(
delay<-data.frame(read_excel("C:/Users/elune/Desktop/111.xlsx"))
View(delay)
delay<-escalc(measure="SMD", m1i=m1, m2i=m2, sd1i=sd1, sd2i=sd2, n1i=n1, n2i=n2, data=delay)
View(delay)

res<-rma(yi, vi, data=delay, slab=Author)
forest(res)



 




         




