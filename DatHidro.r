# Datos hidrologicos ejercicio explorativo 

inp <- read.csv("FDC.csv",na.strings = "")

head(inp)
dim(inp)

inp[!complete.cases(inp),]

# newinp <- na.omit(inp)

plot(inp[,2], type = "l",col="blue")
lines(inp[,3], col="green")

summary(inp[,2:3])
hist(inp[,2])
hist(inp[,3])

names(inp)<- c("fecha", "Estrella", "Banano")
attach(inp)
plot(Estrella)

Tempdate <- strptime(inp[,1],format = "%d/%m/%Y")

MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN = sum) 
MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%Y"), FUN = sum) 
write.csv(rbind(MAQ_Estrella, MAQ_Banano), file="MAQ.csv")

plot(MAQ_Banano, ylim = c(100,3000))

lines(MAQ_Estrella, col=2)

MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN = sum) 

MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%m"), FUN = sum) 


#Analisis de correlción

corinp <-cor(inp[,2:3], method= "spearman")

plot(Estrella, Banano)

inp.lm <- lm(inp[,2]~ inp[,3], data = inp) 
summary(inp.lm)

plot(inp.lm)

