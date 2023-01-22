setwd("D:\\Users\\T-Gamer\\Documents\\Profissional_D\\DataScience\\Portfolio\\DataScience-R\\Acessibilidade_Lazer_SaoPaulo")

inteiro <-  read.csv("PT_Time_SP_v3.csv", header=FALSE, sep = ";", dec = ",")
dim(inteiro)

pt1 <- inteiro[,c(1:315)]
pt2 <- inteiro[,c(316:631)]
pt3 <- inteiro[,c(632:947)]
pt4 <- inteiro[,c(948:1263)]
pt5 <- inteiro[,c(1264:1579)]
pt6 <- inteiro[,c(1580:1895)]

write.table(pt1,"PT_Time_SP_v3_pt1.csv",  
          sep = ";", 
          dec = ",", 
          row.names = F,
          col.names = F)

write.table(pt2,"PT_Time_SP_v3_pt2.csv",  
            sep = ";", 
            dec = ",", 
            row.names = F,
            col.names = F)

write.table(pt3,"PT_Time_SP_v3_pt3.csv",  
            sep = ";", 
            dec = ",", 
            row.names = F,
            col.names = F)

write.table(pt4,"PT_Time_SP_v3_pt4.csv",  
            sep = ";", 
            dec = ",", 
            row.names = F,
            col.names = F)

write.table(pt5,"PT_Time_SP_v3_pt5.csv",  
            sep = ";", 
            dec = ",", 
            row.names = F,
            col.names = F)

write.table(pt6,"PT_Time_SP_v3_pt6.csv",  
            sep = ";", 
            dec = ",", 
            row.names = F,
            col.names = F)


