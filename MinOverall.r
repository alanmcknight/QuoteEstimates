library(plyr)
d = read.csv("//192.168.1.7/On-Site Home/alan.mcknight/Quote Estimates/Template2/FINALDATA.csv")

DataMinOverall = data.frame(matrix(NA, nrow = 62400, ncol = 6))
colnames(DataMinOverall) <- c("VehicleGroup", "PostcodeGroup", "Age", "NCB", "TotalExcess", "Price")


d$No.Claim.Bonus <- ifelse(d$No.Claim.Bonus < 2, 1, ifelse(d$No.Claim.Bonus <9, 2, 3))
d$Excess <- ifelse(d$Excess < 200, 1, ifelse(d$Excess <=600, 2, 3))

x=1
for(k in 1:4){
for(l in 1:4){
for(i in 1:78){
for(j in 1:50){

e= d[d$Vehicle.Group %in% (j-1):(j+1) & d$Age %in% (i+15):(i+17) & d$No.Claim.Bonus<=l & d$Excess<=k, 7]
e2 = round_any(min(e), 5, f = ceiling)
if(l == 4){a=0}else{a=l}
if(k == 4){b=0}else{b=k} 
e2[e2==Inf] <- 0
DataMinOverall[x, ] <- c(j, 99, i+16, a, b, e2)

x <- x+1

}
}
}
}
write.table(DataMinOverall, file = "MinOverall.csv", row.names=F, sep=",")