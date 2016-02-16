#!/usr/bin/Rscript
#Metodo para obtener Te, que es la temperatura del primer metro de profundidad
getTe <- function(data){

    media = data$x[1];
    numElem = 1;
    
    for(i in 1:length(data$x)) 
    {
        if (data$y[i] > -1)
        {
            media = media + data$x[i]
            numElem = numElem + 1
        }
    }

    return (media/numElem)
}

#Funcion de ajusto, que es el minimo de la suma de la formula que crea la curva
min.RSS <- function(data, par) {
    r <- with(data, sum((x-(Th+((Te-Th)/((1+((par[1]*(-y))^par[2]))^(1-1/par[2])))))^2))
    return (r)
}

#Funcion para hallar la temperatura en la curva ajustada, a partir de alfa y n
mod.RSS <- function(data, par) {
    with(data, (Th+((Te-Th)/((1+((par[1]*(-y))^par[2]))^(1-1/par[2])))))
}
library("RMySQL")
#Consulto las fechas de inicio y fin de los perfiles
db='processed'
username='webuser'
pass='webuserpass'
#query='SELECT startDate, endDate from profile WHERE startDate BETWEEN date_sub(now(),INTERVAL 1 WEEK) and NOW()'
query='SELECT startDate, endDate from profile WHERE startDate BETWEEN date_sub(\'2015-05-05 00:00:00\',INTERVAL 1 DAY) and \'2015-05-07 00:00:00\''
#query='SELECT startDate, endDate from profile WHERE DATE(startDate) = \'2010-08-06\''
con <- dbConnect(MySQL(),user=username,password=pass,dbname=db,host="doriiie02.ifca.es")
profileList <- dbGetQuery(con, query)
profileList
dbDisconnect(con)

#Para cada uno de los perfiles
for(i in 1:length(profileList[,1]))
{
    #Cojo la temperatura y profundidad del perfil
    db='processed'
    query=paste('SELECT temp, depth from water_sensors where date BETWEEN \'',profileList$startDate[i],'\' and \'',profileList$endDate[i],'\'  and temp is not null ORDER BY depth ')
    con <- dbConnect(MySQL(),user=username,password=pass,dbname=db,host="doriiie02.ifca.es")
    profile <- dbGetQuery(con, query)
    dbDisconnect(con)
    dat=data.frame(x=profile$temp, y=(-1)*profile$depth)
    if(length(dat$x) > 9)
    {
        #Hallo los distintos parametros
        Te <- getTe(dat)
        Th <- min(dat$x)
        #Optim es el solver que ajusta la curva a los valores de temp
        result <- optim(par = c(0, 10), min.RSS, data = dat)
        alfa <- result$par[1]
        n <- result$par[2]
        m <- 1-1/n
        Zt <- (1/alfa)*((1-(1/n))^(1-(1-(1/n))))
        TZt <- Th+((Te-Th)/((1+((alfa*(Zt))^n))^(1-1/n)))
        TZtprima <- (-1)*(((Te-Th)*(n-1)*((alfa*Zt)^n)*((((alfa*Zt)^n)+1)^((1/n)-2)))/(Zt))
        Zu <- Zt-((TZt-Te)/(TZtprima))
        Zb <- Zt-((TZt-Th)/(TZtprima))
        S <- (Te-Th)/(Zb-Zu)
        mod <- data.frame(x=mod.RSS(dat, c(alfa,n)),y=dat$y)
        plot(y ~ x, data = dat)
        title(paste("Perfil: ",profileList$startDate[i]))
        lines(mod$x,mod$y)
        if (!is.nan(Te) && !is.nan(Th) && !is.nan(alfa) && !is.nan(n) && !is.nan(Zt) && !is.nan(TZt) && !is.nan(TZtprima) && !is.nan(Zu) && !is.nan(Zb) && !is.nan(S))
        {
            insert = paste('UPDATE profile SET Te =', round(Te,2),', Th =', round(Th,2), ', alfa =', round(alfa,2), ', n =', round(n,2), ', Zt =', round(Zt,2), ', TZt =', round(TZt,2), ', TZtprima =', round(TZtprima,2), ', Zu =', round(Zu,2), ', Zb =', round(Zb,2), ', S =', round(S,2), ' WHERE startDate = \'', profileList$startDate[i], '\' and Te is null')
            print(insert)
#            con <- dbConnect(MySQL(),user="ecoCdP",password="ecoplatform",dbname=db,host="87.111.28.83")
 #           dbGetQuery(con, insert)
 #           dbDisconnect(con)
        }
    }
}
#jpeg(paste("perfil_",profileList$startDate[i]),"jpg")
 #   plot(y ~ x, data = dat)
  # title(paste("Perfil: ",profileList$startDate[i]))
   #lines(mod$x,mod$y)
   #dev.off()


# Methods:
#"Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"

# Leer csv
#mysurface <- read.csv('termoclina08.csv', header=FALSE, sep = ";")
#dat <- data.frame(x = mysurface[,1], y = mysurface[,2])
