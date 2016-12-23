library(nlme); library(mgcv);
d = read.csv("//192.168.1.7/On-Site Home/alan.mcknight/Quote Estimates/Template2/FINALDATA.csv")

#d$NCB.Group = ifelse(d$No.Claim.Bonus < 2, 1, ifelse(d$No.Claim.Bonus <9, 2, 3))

#k=1
#Mean = data.frame(matrix(NA, nrow = 234, ncol = 3))
#colnames(Mean) <- c("Age", "NCB", "Average")
#for(j in 1:3){
#for(i in 1:78){
#Mean[k, 1] = i+16
#Mean[k, 2] = j
#Mean[k, 3] = mean(subset(d$No.Claim.Bonus, d$NCB.Group == j & d$Age ==i+16))
#k=k+1
#}
#}

#MeanD <- mean(subset(d$Excess, d$Excess < 200))
#MeanE <- mean(subset(d$Excess, d$Excess >= 200 | d$Excess <600))
#MeanF <- mean(subset(d$Excess, d$Excess > 600))

Data = data.frame(matrix(NA, nrow = 3900, ncol = 8))
colnames(Data) <- c("Vehicle.Group", "Postcode.Group", "Age", "NCB", "Total.Excess", "Price", "Total.Excess.Group", "NCB.Group")

Data[, 1] = ceiling((1:3900)/78)
Data[, 2] = 0
Data[, 3] = ceiling(1:78)+16
Data[, 4] = 0
Data[, 5] = 0

#Data$NCB.Group <- Mean[(78*(Data$NCB-1))+Data$Age-16, 3]


for(z in 1:7){

Data[, 2] = z

a <- subset(d, Postcode.Area == z)
A <- gam(Policy.Price ~ s(Vehicle.Group)+s(Age), data = a, family=Gamma(link=log))
Data$Price = predict(A, newdata = data.frame(Age=Data$Age, Vehicle.Group=Data$Vehicle.Group), type="response")

write.table(Data[,1:6], file=paste(z, "NoNCBNoExcess.csv", sep=""), row.names=F, sep=",")
Data$Price <- 0
}

Data[, 2] = 99
A <- gam(Policy.Price ~ s(Vehicle.Group)+s(Age), data = d, family=Gamma(link=log))
Data$Price = predict(A, newdata = data.frame(Age=Data$Age, Vehicle.Group=Data$Vehicle.Group), type="response")


write.table(Data[,1:6], "NoPCNoNCBNoExcess.csv", row.names=F, sep=",")