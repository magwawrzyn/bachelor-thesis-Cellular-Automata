#install.packages('mvtnorm')

library(plotly)

set.seed(5)

#ZASADA 254

M1 = matrix( nrow = 29, ncol = 49 )
M1[ , ] = 0
M1[1, 25] = 1
for ( i in 2:29 ){
  
  for ( j in 2:48 ){
    
    if ( M1[i-1, j-1] + M1[i-1, j] + M1[i-1, j+1] >= 1 )
    M1[i, j] = 1 
    else
    M1[i, j] = 0 
    
  }
  
#warunki brzegowe
  
  if ( M1[i-1, 2] == 1 )
    M1[i, 1] = 1
  else 
    M1[i, 1] = 0 
  
  if ( M1[i-1, 48] == 1 )
    M1[i, 49] = 1
  else
    M1[i, 49] = 0 

}

rule254 <- plot_ly(
        x = c(1:49), y = c(1:49),
        z = M1, type = "heatmap" )

rule254

#ZASADA 254 2.0

M1 = matrix( nrow=29, ncol=49 )
M1[ , ] = 0
M1[1, c(13, 25, 30) ] = 1
for ( i in 2:29 ){
  for ( j in 2:48 ){
    
    if ( M1[i-1, j-1] + M1[i-1, j] + M1[i-1, j+1] >= 1 )
      M1[i, j] = 1 
    else
      M1[i, j] = 0 
    
  }
  
  #warunki brzegowe
  
  if ( M1[i-1, 2] == 1 )
    M1[i, 1] = 1 
  else
    M1[i, 1] = 0 
  
  if ( M1[i-1, 48] == 1 )
    M1[i, 49] = 1
  else
    M1[i, 49] = 0
  
}


rule254z <- plot_ly(
  x = c(1:49), y = c(1:49),
  z = M1, type = "heatmap" )

rule254z


#ZASADA 50

M2 = matrix( nrow=29, ncol=49 )
M2[ , ] = 0
M2[1, 25] = 1
for (i in 2:29){
  
  for (j in 2:48){
    
    if ( (M2[i-1, j-1] + M2[i-1, j] + M2[i-1, j+1] >= 1) & (M2[i-1, j] == 0) )
      M2[i, j]= 1
    else
      M2[i, j]=0
  }
  #warunki brzegowe
  
  if ( (M2[i-1, 2] == 1) & (M2[i-1, 1] == 0) )
    M2[i, 1] = 1
  else 
    M2[i, 1] = 0
  
  if ( (M2[i-1, 48] == 1) & (M2[i-1, 49] == 0) )
    M2[i, 49] = 1
  else
    M2[i, 49] = 0
  
}


rule50 <- plot_ly(
  x = c(1:49), y = c(1:49),
  z = M2, type = "heatmap" )

rule50

#ZASADA 50 2.0

M2 = matrix( nrow=29, ncol=49 )
M2[ , ]=0
M2[ 1, c(2, 27, 37, 6, 28, 13, 5, 49, 7, 34, 20, 16, 47, 43, 2, 17, 21, 7, 1, 2, 41) ] = 1 # wylosowane za pomoc strony internetwej 
for ( i in 2:29 ){
  
  for ( j in 2:48 ){
    
    if ( (M2[i-1, j-1] + M2[i-1, j] + M2[i-1, j+1] >= 1) & (M2[i-1, j] == 0) )
      M2[i, j] = 1
    else
      M2[i, j] = 0
    
  }
  #warunki brzegowe
  
  if ( (M2[i-1,2]==1) & (M2[i-1,1]==0) )
    M2[i, 1] = 1
  else 
    M2[i, 1] = 0
  
  if ( (M2[i-1, 48] == 1) & (M2[i-1, 49] == 0) )
    M2[i, 49] = 1
  else
    M2[i, 49] = 0
  
}


rule50z <- plot_ly(
  x = c(1:49), y = c(1:49),
  z = M2, type = "heatmap" )

rule50z

#ZASADA 90

M3 = matrix( nrow=29, ncol=49 )
M3[ , ] = 0
M3[1, 25] = 1
for ( i in 2:29 ){
  
  for ( j in 2:48 ){
    if ( (( M3[i-1, j-1] + M3[i-1, j] + M3[i-1, j+1] == 1 ) & ( M3[i-1, j] == 0 ))
        || (( M3[i-1, j-1] + M3[i-1, j] + M3[i-1, j+1] == 2 ) & ( M3[i-1, j] == 1 )) )
      M3[i, j] = 1
    else
      M3[i, j] = 0
    
  }
  #warunki brzegowe
  
  if ( M3[i-1, 2] == 1 )
    M3[i, 1] = 1
  else 
    M3[i, 1] = 0
  
  if ( M3[i-1, 48] == 1 )
    M3[i, 49] = 1
  else
    M3[i, 49] = 0
  
}


rule90 <- plot_ly(
  x = c(1:49), y = c(1:49),
  z = M3, type = "heatmap" )

rule90

#ZASADA 90 2.0

M3l = matrix( nrow=50, ncol=99 )
M3l[ , ] = 0
M3l[1, c( 15, 27, 69, 37, 43, 90, 58, 54, 73, 72, 63, 92, 20, 82, 86, 45, 73,
        45, 5, 82, 84, 49, 90, 68, 70, 29, 82, 23, 44, 62, 93, 55, 32, 38, 6, 27 ) ] = 1

for ( i in 2:50 ){
  
  for ( j in 2:98 ){
    if ( (( M3l[i-1, j-1] + M3l[i-1,j] + M3l[i-1,j+1] == 1 ) & ( M3l[i-1,j] == 0 ))
        || (( M3l[i-1, j-1] + M3l[i-1,j] + M3l[i-1,j+1] == 2 ) & ( M3l[i-1,j] == 1 )) )
      M3l[i, j] = 1
    else
      M3l[i, j] =0
    
  }
  #warunki brzegowe
  
  if ( M3l[i-1, 2] == 1 )
    M3l[i, 1] = 1
  else 
    M3l[i, 1] = 0
  
  if ( M3l[i-1, 98] == 1 )
    M3l[i, 99] = 1
  else
    M3l[i, 99] = 0
  
}


rule90z <- plot_ly(
  x = c(1:99), y = c(1:49),
  z = M3l, type = "heatmap" )

rule90z

#----------------------------------------------------------------------------------------------
#Mr?wka
#----------------------------------------------------------------------------------------------

#stworzy? macierz nxn

#ti = 7
#x_ax = length( seq(0,10,1) )
#y_ax = length( seq(0,10,1) )

ti = 15000
x_ax = length( seq(0,49,1) )
y_ax = length( seq(0,49,1) )

time = length( seq(0,ti,1) )

Mr = array(NA, dim = c(x_ax, y_ax, time) )
Mr[ , , ] = 0

#zada? warunek poczatkowy

Mr[25, 25, 1] = 2
Mr[25, 24, 2] = 2
Mr[25, 25, 2] = 1

#Mr[6, 6, 1] = 2
#Mr[6, 5, 2] = 2
#Mr[6, 6, 2] = 1

#przechozenie do nowej kom?rki i zamiana kolor?w

for( t in seq(2, time-1 , 1) ){
  
  for( x in seq(2, x_ax-1, 1) ){
    
    for( y in seq(2, y_ax-1, 1) ){
      
      if ( (Mr[y, x, t] == 1) & (Mr[y, x-1, t] != 2) & (Mr[y, x+1, t] != 2) & (Mr[y-1, x, t] != 2) & (Mr[y+1, x, t] != 2) ){
       Mr[y, x, t+1] = 1 }
      
      if ( (Mr[y, x, t] == 0) & (Mr[y, x-1, t] != 2) & (Mr[y, x+1, t] != 2) & (Mr[y-1, x, t] != 2) & (Mr[y+1, x, t] != 2) ){
       Mr[y, x, t+1] = 0 }
    
      #1. Mrowka przszla z lewej strony na komorke w stanie 0, przechodzi do gory, komorka zmienia stan na 1.
      
      if( (Mr[y, x, t] == 2 ) & (Mr[y, x-1, t-1] == 2) & (Mr[y, x, t-1] == 0) ){
        Mr[y+1, x, t+1] = 2
        Mr[y, x, t+1] = 1 
        Mr[y-1, x, t+1] = Mr[y-1, x, t] 
        Mr[y, x+1, t+1] = Mr[y, x+1, t] 
        Mr[y, x-1, t+1] = Mr[y, x-1, t] }
        
      #2. Mrowka przszla z lewej strony na komorke w stanie 1, przechodzi w dol, komorka zmienia stan na 0.
      
      if( (Mr[y, x, t] == 2) & (Mr[y, x-1, t-1] == 2) & (Mr[y, x, t-1] == 1) ){
        Mr[y-1, x, t+1] = 2
        Mr[y, x, t+1] = 0
        Mr[y+1, x, t+1] = Mr[y+1, x, t] 
        Mr[y, x+1, t+1] = Mr[y, x+1, t] 
        Mr[y, x-1, t+1] = Mr[y, x-1, t] }
        
      #3. Mrowka przszla z prawej strony na komorke w stanie 0, przechodzi w dol, komorka zmienia stan na 1.
        
      if( (Mr[y, x, t] == 2) & (Mr[y, x+1, t- 1] == 2) & (Mr[y, x, t-1] == 0) ){
          Mr[y-1, x, t+1] = 2
          Mr[y, x, t+1] = 1 
          Mr[y+1, x, t+1] = Mr[y+1, x, t] 
          Mr[y, x+1, t+1] = Mr[y, x+1, t] 
          Mr[y, x-1, t+1] = Mr[y, x-1, t] }
          
      #4. Mrowka przszla z prawej strony na komorke w stanie 1, przechodzi do gory, komorka zmienia stan na 0.
      
      if( (Mr[y, x, t] == 2) & (Mr[y, x+1, t-1] == 2) & (Mr[y, x, t-1] == 1) ){
          Mr[y+1, x, t+1] = 2
          Mr[y, x, t+1] = 0
          Mr[y-1, x, t+1] = Mr[y-1, x, t] 
          Mr[y, x+1, t+1] = Mr[y, x+1, t] 
          Mr[y, x-1, t+1] = Mr[y, x-1, t] }    
          
      #5. Mrowka przszla z dolu na komorke w stanie 0, przechodzi w lewo, komorka zmienia stan na 1.
      
      if( (Mr[y, x, t] == 2) & (Mr[y-1, x, t-1] == 2) & (Mr[y, x, t-1] == 0) ){
          Mr[y, x-1, t+1] = 2
          Mr[y, x, t+1] = 1 
          Mr[y-1, x, t+1] = Mr[y-1, x, t] 
          Mr[y, x+1, t+1] = Mr[y, x+1, t] 
          Mr[y+1, x, t+1] = Mr[y+1, x, t] }
          
      #6. Mrowka przszla z dolu na komorke w stanie 1, przechodzi w prawo, komorka zmienia stan na 0.
      
      if( (Mr[y, x, t] == 2) & (Mr[y-1, x, t-1] == 2) & (Mr[y, x, t-1] == 1 ) ){
          Mr[y, x+1, t+1] = 2
          Mr[y, x, t+1] = 0 
          Mr[y-1, x, t+1] = Mr[y-1, x, t] 
          Mr[y+1, x, t+1] = Mr[y+1, x, t] 
          Mr[y, x-1, t+1] = Mr[y, x-1, t] } 
          
      #7. Mrowka przszla z gory na komorke w stanie 0, przechodzi w prawo, komorka zmienia stan na 1.
      
      if( (Mr[y, x, t] == 2) & (Mr[y+1, x, t-1] == 2) & (Mr[y, x, t-1] == 0) ){
          Mr[y, x+1, t+1] = 2
          Mr[y, x, t+1] = 1 
          Mr[y-1, x, t+1] = Mr[y-1, x, t] 
          Mr[y+1, x, t+1] = Mr[y+1, x, t] 
          Mr[y, x-1, t+1] = Mr[y, x-1, t] }         
            
      #8. Mrowka przszla z gory na komorke w stanie 1, przechodzi do gory, komorka zmienia stan na 0.
      
      if( (Mr[y, x, t] == 2) & (Mr[y+1, x, t-1] == 2) & (Mr[y, x, t-1] == 1) ){
          Mr[y, x-1, t+1] = 2
          Mr[y, x, t+1] = 0
          Mr[y-1, x, t+1] = Mr[y-1, x, t] 
          Mr[y, x+1, t+1] = Mr[y, x+1, t] 
          Mr[y+1, x, t+1] = Mr[y+1, x, t] }
      
    }
  }
}

#symulacje-------------------------------------------------------------------

plot_ly(
  x = c(1:10), y = c(1:10),
  z = Mr[,,1], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:10), y = c(1:10),
  z = Mr[,,2], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:10), y = c(1:10),
  z = Mr[,,3], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:10), y = c(1:10),
  z = Mr[,,4], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:10), y = c(1:10),
  z = Mr[,,5], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:10), y = c(1:10),
  z = Mr[,,6], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:10), y = c(1:10),
  z = Mr[,,7], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )
#----------------------------------------------------------------------------------
plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,10], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,100], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,200], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,500], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,1000], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,5000], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,9000], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,10000], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )
plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,10200], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )
plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,10300], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,10500], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )


plot_ly(
  x = c(1:49), y = c(1:49),
  z = Mr[,,11500], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )
#---------------------------------------------------------------------------------------------------------------
#Gra w ?ycie
#===================================================================

#stworzy? macierz nxn

ti = 200
xc = length( seq(0, 25, 1) )
yc = length( seq(0, 25, 1) )

ti = 2000
xc = length( seq(0, 75, 1) )
yc = length( seq(0, 75, 1) )

time = length( seq(0, ti, 1) )

G = array(NA, dim = c(xc, yc, time))
G[, , ] = 0

#zada? warunek poczatkowy 

#losowe wartosci dla du?ej tablicy


v = c(1:100)

for(y in seq(1, 100 , 1)){
  
  G[ y, sample( v, sample(v[15:45], 1) ), 1 ] = 1
}

gm = G[, , 1] #losowane dla 5-30 ?ywych kom?rek w wierszu
gs = G[, , 1] #losowane dla 15-45 ?ywych kom?rek w wierszu
gd = G[, , 1] #losowane dla 40-75 ?ywych kom?rek w wierszu

G[, , 1] = gm
G[, , 1] = gs
G[, , 1] = gd

#pojedyncze wzory 

#pasy 75x75

G[35, c(15:54),1] = 1
G[35, c(24, 30:32, 36:42, 59), 1] = 0

#pulsar 25x25
 
G[c(7, 12, 14, 19), c(9:11, 15:17), 1] = 1
G[c(9:11, 15:17), c(7, 12, 14, 19), 1] = 1

#statki + stale wzory

G[8, c(20:23), 1] = 1
G[9, c(19:23), 1] = 1
G[9, 21, 1] = 0
G[10, c(20, 21), 1] = 1
G[7, c(21, 22), 1] = 1

G[c(18, 19), 19, 1] = 1
G[20, c(18, 20), 1] = 1
G[19, 18, 1] = 1

G[c(18, 19), 4, 1] = 1
G[c(20, 19), 5, 1] = 1
G[20, 3, 1] = 1

#przechozenie do nowej kom?rki i zamiana kolor?w

for( t in seq(1, time-1 , 1) ){
  
  sr1 = G[1,2,t] + G[2, 2, t] + G[2, 1, t] + G[1, 1, t]
  sr2 = G[1, xc-1,t] + G[2, xc-1 , t] + G[2, xc, t] + G[1, xc, t]
  sr3 = G[yc, 1,t] + G[yc-1, 2, t] + G[yc-1, 1, t] + G[yc, 1, t]
  sr4 = G[yc, xc-1,t] + G[yc-1, xc, t] + G[yc-1, xc-1, t] + G[yc, xc, t]
  
  if(sr1 == 1) G[1, 1, t+1] = 1 else G[1, 1, t+1] = 0
  if(sr2 == 1) G[1, xc, t+1] = 1 else G[1, xc, t+1] = 0
  if(sr3 == 1) G[yc, 1, t+1] = 1 else G[yc, 1, t+1] = 0
  if(sr4 == 1) G[yc, xc, t+1] = 1 else G[yc, xc, t+1] = 0
  
  for( x in seq(2, xc-1, 1) ){
    
    S1 = G[1, x+1, t] + G[1, x-1, t] + G[2, x, t] + G[2, x-1, t] + G[2, x+1, t] + G[1, x, t]
    S2 = G[x+1, 1, t] + G[x-1, 1, t] + G[x, 2, t] + G[x-1, 2, t] + G[x+1, 2, t] + G[x, 1, t]
    S3 = G[yc, x+1, t] + G[yc, x-1, t] + G[yc-1, x, t] + G[yc-1, x-1, t] + G[yc-1, x+1, t] + G[yc, x, t]
    S4 = G[x+1, xc, t] + G[x-1, xc, t] + G[x, xc-1, t] + G[x-1, xc-1, t] + G[x+1, xc-1, t] + G[x, xc, t]
    
    if(S1 == 2)  G[1, x, t+1] = 1 else  G[1, x, t+1] = 0
    if(S2 == 2)  G[x, 1, t+1] = 1 else  G[x, 1, t+1] = 0
    if(S3 == 2)  G[yc, x, t+1] = 1 else  G[1, x, t+1] = 0
    if(S4 == 2)  G[x, xc, t+1] = 1 else  G[x, xc, t+1] = 0
    
    for( y in seq(2, yc-1, 1) ){
      
      S = G[y-1, x, t] + G[y+1, x, t] + G[y, x-1, t] + G[y, x+1, t] + G[y-1, x-1 ,t] + G[y-1, x+1, t] + G[y+1, x-1, t] + G[y+1, x+1, t] 

      if((S == 3)||((S == 2) & (G[y, x, t] == 1))) G[y, x, t+1] = 1  else   G[y, x, t+1] = 0
      
         
        
        
    }
  }
}

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,1], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,2], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,3], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,4], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,5], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,6], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )


plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,11], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,20], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,45], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,65], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,80], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,100], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,120], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,250], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,400], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )

plot_ly(
  x = c(1:100), y = c(1:100),
  z = G[, ,1100], colors = colorRamp(c("khaki", "darkgreen")), type = "heatmap" )
#---------------------------------------------------------------------------------------------

#Deklaracje zmiennych i ustalenie warunk?w pocz?tkowych:

dt <- 0.001
dx <- 0.1  
t <- 1
rows <- length(seq(0,t,dt))
columns <- length(seq(0,1,dx))
pi_value <- 3.14
M <- matrix(nrow=rows,ncol=columns)

#warunek pocz?tkowy
M[1,] <- sin(pi_value*seq(0,1,dx))

#warunki brzegowe Dirichleta
M[,1] <- 0
M[,columns] <- 1

#Liczenie:

for(i in seq(2,rows,1)){
  for(j in seq(2, columns-1, 1)){
    M[i,j] <- (M[i-1, j+1] + M[i-1, j-1])/2
  }
}

#symulacje
plot_ly(
  x = c(1:100), y = c(1:100),
  z = M,  type = "heatmap" )
#-----------------------------------------------------------------
#Deklaracje zmiennych i ustalenie warunk?w pocz?tkowych:

dt <- 0.01
dx <- 0.1  
t <- 1
rows <- length(seq(0,t,dt))
columns <- length(seq(0,1,dx))
pi_value <- 3.14
M <- matrix(nrow=rows,ncol=columns)

#warunek pocz?tkowy
M[1,] <- cos(pi_value*seq(0,1,dx))

#Liczenie:

for(i in seq(2,rows,1)){
  for(j in seq(2, columns-1, 1)){
    M[i,j] <- (M[i-1, j+1] + M[i-1, j-1])/2
  }
#Warunki brzegowe Neumanna - bark przep?ywu przez brzeg
  M[i,1] <- M[i,2]
  M[i, columns] <- M[i, columns - 1]
}

#symulacje
plot_ly(
  x = c(1:100), y = c(1:100),
  z = M,  type = "heatmap" )

