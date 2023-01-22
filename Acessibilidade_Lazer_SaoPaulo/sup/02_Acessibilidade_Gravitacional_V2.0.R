####### Accessibility computation and visualisation #######
########### Gravity accessibility computation #############
####################### CASA UCL ##########################
############# Adaptado por: Gabriel Valverde  #############
######################## JAN 2023  ########################

  ###   Carrega as bibliotecas instaladas anteriormente
  library(classInt)
  library(stringr)
  library(raster)
  library(rgdal)
  library(reshape)
  library(ggplot2)
  library(RColorBrewer)
  
### define o diretório de trabalho. << ATENCAO!!! IMPORTANTE ALTERAR PARA DIRETORIO CORRETO ANTES DE RODAR O SCRIPT!!!>>
  setwd("D:\\Users\\T-Gamer\\Documents\\Profissional_D\\DataScience\\Portfolio\\DataScience-R\\Acessibilidade_Lazer_SaoPaulo")

  ### Leitura do arquivo de empregos (Possui cabeçalho, separador de colunas ";" e decimal ",")
  Dj0 <- read.csv("Jobs_Dj_2017_05_xy_SP_v4.csv", header = TRUE, sep = ",", dec = ",")
  
  ### Remove a linha 1317 do arquivo de empregos, pois não existe essa zona no arquivo shapefile
  Dj0 <- Dj0[-1317,] 
  
  ### Define e tempo PuT
  PuT <- 30
  
  ### Cria uma variável vazia com os campos abaixo 
  Dj1 <- c("OBJECTID","code","X","Y", "workplaceP")
  Dj2 <- c("workplaceP")
  
  ### Atribui a nova variável Dj o nome do campo "workplaceP" e os valores de empregos 
  Dj <- Dj0[Dj2]
  
  ### Leitura da matriz OD de tempos de viagem por transporte público (não tem cabeçalho, separador de colunas ";" e decimal ",")
  dij2 <- read.csv("PT_Time_SP_v3.csv", header=FALSE, sep = ";", dec = ",")
  
  ### Remove a linha e a coluna 1317 da matriz de tempos, pois não existe essa zona no arquivo shapefile
  dij2 <- dij2[-1317, -c(1317)]
  
  ### Trata os tempos de viagem da Matriz OD e aplica o exponencial de beta na busca de empregos acessiveis
  dij2[dij2==-1] <- 400
  work <- Dj$workplaceP
  expij <- exp(dij2*(-0.025))
  
  ### Cria matriz de empregos com o mesmo número de linhas e colunas que a matriz de tempos
  work2 <- matrix(work,NROW(dij2),NCOL(dij2))
  work2 <- as.matrix(work2)
  work2 <- t(work2)
  
  ### Calcula os empregos acessiveis considerando o beta da deterrance function
  result2 <- expij*work2
  result <- apply(result2,1,sum)
  
  ### valores finais da acessibilidade gravitacional
  cumacc <- result
  Dj<-Dj0[Dj1]
  result <- cbind (Dj,cumacc)

  ### Criação dos intervalos de classes do mapa
  natural.interval <- classIntervals(result$cumacc,10, style = "jenks")$brks
  std.interval <- classIntervals(result$cumacc,10, style = "sd")$brks
  quantile.interval <- classIntervals(result$cumacc,10, style = "quantile")$brks
  equal.interval<- classIntervals(result$cumacc,10, style = "equal")$brks
  kmeans.interval <- classIntervals(result$cumacc,10, style = "kmeans")$brks
  lable.natural <- natural.interval[1:length(natural.interval)]
  lable.std <- std.interval[1:length(std.interval)]
  lable.quantile <- quantile.interval[1:length(quantile.interval)]
  lable.equal <- equal.interval[1:length(equal.interval)]
  lable.kmeans <- kmeans.interval[1:length(kmeans.interval)]
  result$interval0 <- findInterval(result$cumacc,natural.interval)
  result$natural <- round(natural.interval[result$interval0],0)
  result$interval1 <- findInterval(result$cumacc,std.interval)
  result$std <- round(std.interval[result$interval1],0)
  result$interval2 <- findInterval(result$cumacc,quantile.interval)
  result$quantile <- round(quantile.interval[result$interval2],0)
  result$interval3 <- findInterval(result$cumacc,equal.interval)
  result$equal <- round(equal.interval[result$interval3],0)
  result$interval4 <- findInterval(result$cumacc,kmeans.interval)
  result$kmeans <- round(kmeans.interval[result$interval4],0)
  result$natural <- as.character(result$natural)
  result$std <- as.character(result$std)
  result$quantile <- as.character(result$quantile)
  result$equal <- as.character(result$equal)
  result$kmeans <- as.character(result$kmeans)
  result$equal <- str_pad(result$equal,7,"left",pad="0")
  result$natural <- str_pad(result$natural,7,"left",pad="0")
  result$std <- str_pad(result$std,7,"left",pad="0")
  result$quantile <- str_pad(result$quantile,7,"left",pad="0")
  result$kmeans <- str_pad(result$kmeans,7,"left",pad="0")
  
  ### Leitura do arquivo shapefile da area construida da RMSP e criação de novo arquivo shapefile com acessibilidades
  shp <- shapefile("shapefiles/CPTM_RMSP.shp")
  result <- rename(result, c(code="code"))
  combine <- merge (shp, result, by.x='CODE', by.y="code")
  
  # Selecionando apenas e região do Município de São Paulo
  vaux <- grep("Paulo" ,combine@data[["MUNICIPI0"]], ignore.case = TRUE)
  combine <- combine[vaux,]
  
  shapefile(combine, "shapefiles/Acessibilidade_Gravitacional.shp", overwrite=TRUE)
  map <- readOGR(dsn="shapefiles",layer= "Acessibilidade_Gravitacional")
  map <- spTransform(map,CRS=CRS("+init=epsg:32723"))
  map.data <- data.frame(id=rownames(map@data), map@data)
  map.df <- fortify(map)
  map.df <- merge(map.df,map.data,by="id")
 
  # Reiniciando o gerador de plots para evitar bugs
  graphics.off()
  
  ### Salvando o plot como imagem
  filename <- paste("plots/GravAcc.pdf",sep = "")
  pdf(file = paste("plots/GravAcc.pdf",sep = ""))
  # png(filename = paste("plots/GravAcc.pdf",sep = ""), width = 1000, height = 1000, units = "px")
  
   
  ### Se escolher classificar o mapa em quebras naturais
  colourCount = length(unique(result$natural))
  col <- rev(brewer.pal(10, "Spectral"))
  pal.natural <- colorRampPalette(col)(colourCount)
  ncolour.natural <- sort(unique(result$interval0))
  pal.natural2 <- pal.natural[ncolour.natural]
  plot.natural = ggplot(map.df, aes(x=long, y=lat, group=group))+
    geom_polygon(aes(fill=natural))+
    geom_path(colour=NA)+
    scale_fill_manual(values =pal.natural2)+
    theme(axis.text=element_blank())+
    labs(title="Acessibilidade Gravitacional aos Empregos", x="", y="")+
    coord_fixed()
  plot(plot.natural)
  
  dev.off()
  