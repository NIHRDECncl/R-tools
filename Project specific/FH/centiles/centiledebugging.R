wd <- "H:/Jallens_homearea_DEC/Calculators/R/R-tools/Project specific/FH/centiles"
#wd <- "~/Documents/DEC WORK/Shiny/R tools/R-tools/Project specific/FH/centiles"

setwd(wd)

cm <- read.csv("malecentiles.csv")
cw <- read.csv("femalecentiles.csv")

gcm <- read.csv("gamlassmalecentiles.csv")
gcw <-read.csv("gamlassfemalecentiles.csv")

gamlass_centiles <- TRUE

centile_script <- function(age, sex, nonhdl){ 
  
  
  if(!gamlass_centiles){
    
    if(sex == "Male") centiles <- cm
    if(sex == "Female") centiles <- cw
    
    age_group <- 0
    age_group <- ifelse(age <= 16, 1, age_group)
    age_group <- ifelse(age >= 16 & age <= 24, 2, age_group)
    age_group <- ifelse(age >= 25 & age <= 34, 3, age_group)
    age_group <- ifelse(age >= 35 & age <= 44, 4, age_group)
    age_group <- ifelse(age >= 45 & age <= 54, 5, age_group)
    age_group <- ifelse(age >= 55 & age <= 64, 6, age_group)
    age_group <- ifelse(age >= 65 & age <= 74, 7, age_group)
    age_group <- ifelse(age >= 75, 8, age_group)
    
    
    centile <- "NULL"
    
    for (i in 1:8)
    {
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,9], ">100", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,8] & nonhdl < centiles[i,9], "99-100", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,7]& nonhdl < centiles[i,8], "95-99", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,6]& nonhdl < centiles[i,7], "90-95", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,5]& nonhdl < centiles[i,6], "80-90", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,4]& nonhdl < centiles[i,5], "75-80", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,3]& nonhdl < centiles[i,4], "50-75", centile)
      centile <- ifelse(age_group == i & nonhdl >= centiles[i,2]& nonhdl < centiles[i,3], "25-50", centile)
      centile <- ifelse(age_group == i & nonhdl < centiles[i,2] , "<25", centile)
    }
    
  }
  
  
  if (gamlass_centiles){
    if(sex == "Male") centiles <- gcm
    if(sex == "Female") centiles <- gcw
    
    
    centiles$X <- NULL
    rownames(centiles) <- centiles$age - 15
    
    ageindex <- age - 15
    ageindex[ageindex < 0] <- 0
    
    centile <- "NULL"
    
    
    
    centile <- ifelse(ageindex >0 & nonhdl >= centiles[ageindex,8], ">99.5", centile)
    centile <- ifelse(ageindex >0 & nonhdl >= centiles[ageindex,7]& nonhdl < centiles[ageindex,8], "99-99.5", centile)
    centile <- ifelse(ageindex >0 & nonhdl >= centiles[ageindex ,6]& nonhdl < centiles[ageindex,7], "97.5-99", centile)
    centile <- ifelse(ageindex >0 & nonhdl >= centiles[ageindex,5]& nonhdl < centiles[ageindex,6], "95-97.5", centile)
    centile <- ifelse(ageindex >0 & nonhdl >= centiles[ageindex,4] & nonhdl < centiles[ageindex,5], "90-95", centile)
    centile <- ifelse(ageindex >0 & nonhdl >= centiles[ageindex,3]& nonhdl < centiles[ageindex,4], "80-90", centile)
    centile <- ifelse(ageindex >0 & nonhdl >= centiles[ageindex,2]& nonhdl < centiles[ageindex,3], "75-80", centile)
    centile <- ifelse(ageindex >0 & nonhdl < centiles[ageindex,2], "<75", centile)
    
    
    
  }
  
  # plots
  centiles <- as.data.frame(centiles)
  centiles <- centiles[with(centiles,order(age)), ]
  
  dist <- ggplot(centiles, aes(x = age, y = X75., colour = "red"))
  dist <- dist + geom_line(na.rm = TRUE)   + geom_line(aes(x = age, y = X80., colour = "blue")) +
    geom_line(aes(x = age, y = X90., colour = "green")) + geom_line(aes(x = age, y = X95., colour = "orange"))+ 
    geom_line(aes(x = age, y = X97.5., colour = "purple"))+
    geom_line(aes(x = age, y = X99., colour = "cyan")) + geom_line(aes(x = age, y = X99.5., colour = "brown")) + 
    xlim(0,120)+ ylab("nonHDL (mmol/L)") +     xlab("Age")  
  dist
  
  
  
  # centile = 
  #   data.frame(
  #     Name = c("Age", "Sex", "nonHDL","Centile band"),
  #     Value = c(age, sex, nonhdl,centile))
  
  
  
  return(centile)
  
}

df <- as.data.frame(read.csv("data/mypatientsdatawsnps.csv"))
df_male <- subset(df, df$Sex == "MALE")

df_male$centile <- "NULL"

centiles <- cm

df_male$centiles2 <- 0
df_male$centiles2 <- as.numeric(df_male$centiles)
df_male$centiles[df_male$centile == "<99.5"] <- 100

df_male$centile[df_male$centile == "99-99.5"] <- 99.5
df_male$centile[df_male$centile == "97.5-99"] <- 99
df_male$centile[df_male$centile == "95-97.5"] <- 97.5
df_male$centile[df_male$centile == "90-95"] <- 95
df_male$centile[df_male$centile == "80-90"] <- 90
df_male$centile[df_male$centile == "75-80"] <- 80
df_male$centile[df_male$centile == "<75"] <- 75


df_male$centile <- apply(df_male,1, function(x,y,z) centile_script(df_male$age,  "Male", df_male$nonhdl))
p <- ggplot(data=subset(df_male, !is.na(SNPscore)), aes(x= df_male$SNPscore, y = df_male$centile))
p <- p + geom_point(data=subset(df_male, !is.na(SNPscore)),aes(x= df_male$SNPscore, y = df_male$centile)) + ylab("nonHDL (mmol/L)") +  
  xlab("SNP score")  +   ggtitle(paste("Male SNP plots"))
p
