df <- read.csv("C:/GitHub/JewelKnotweedDecomp/data/BUNA_StreamTemp_LoggerData.csv")


#every 15 minutes = 4*24 = 96/day
# 96*7 = 672 per week.

#wk 1
# 1:320 are in air.
# 560:577 also in air.

wk1 <- df[-560:-577,]
wk1 <- wk1[320:590,]
mean(wk1$Temp...C..LGR.S.N..21750055..SEN.S.N..21750055.)
# 14.20879

#wk 2
608+672
mean(df$Temp...C..LGR.S.N..21750055..SEN.S.N..21750055.[608:1280])
# 12.079

#Wk3 
1280+672
mean(df$Temp...C..LGR.S.N..21750055..SEN.S.N..21750055.[1280:1952])
# 10.12756 

#Wk4
1952+672
mean(df$Temp...C..LGR.S.N..21750055..SEN.S.N..21750055.[1952:2624])
# 9.7146

#Wk5
2624+672
mean(df$Temp...C..LGR.S.N..21750055..SEN.S.N..21750055.[2624:3296])
# 9.283051

#Wk6
3296+672
mean(df$Temp...C..LGR.S.N..21750055..SEN.S.N..21750055.[3296:3968])
# 7.082152

#Wk7
3968+672
mean(df$Temp...C..LGR.S.N..21750055..SEN.S.N..21750055.[3968:4640])
# 7.135838

#Wk8
4640+672
mean(df$Temp...C..LGR.S.N..21750055..SEN.S.N..21750055.[4640:nrow(df)])
# 5.222826



