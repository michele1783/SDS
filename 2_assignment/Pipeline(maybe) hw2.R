library(DescTools)
library(stringr)

value <- function(team_member, eps, alpha){
  #input:
  #team_member: an integer that represent the player that we want to study
  #eps: tolerance
  #alpha: confidence
  
  #output:
  #Shapley payoff of the player
  
  #string with all the team members
  member <- "1-2-3-4-5-6-7-8-9-10-11-12"
  
  #this line is useful to delete the given team member from the "member" string
  team_member_1 <- paste("-", as.character(team_member), sep = "")
  
  #compute the max value that the given team member can assume 
  b <- char_fun[[12]] - char_fun[[11]][str_remove(member, team_member_1)]
  
  #compute the min value that the given team member can assume
  a <- char_fun[[1]][team_member]
  
  #Through Hoeffding compute the number of iteration 
  M <- ceiling(((b-a)^2/(2*eps^2)) * log(2/alpha))

  x <- seq(1,12)

  value_of_person <- c(length(M))
  for (i in 1:M){
    
    #sample
    campione <- sample(x)
    
    #discover the position of my person
    position <- which(campione == team_member) 
    
    #sort element until my person
    element <- sort(campione[1:position])
    
    #write in the same format of char_fun
    element_1<- paste(element, collapse = "-")
    
    #compute the value with my person
    v_1 <- char_fun[[position]][element_1] 
    
    #consider separately if the member is in the first position
    if(v_1 == 0){
      
      #so its values is zero
      value_of_person[i] <- 0
      
    }else{
      
      #delete my person from the sequence
      element_2 <- sort(element[element != team_member])
      
      #write in the same format of char_fun
      element_3 <- paste(element_2, collapse = "-")
      
      #compute the value without my person
      v_2 <- char_fun[[position-1]][element_3]
      
      #compute the value of my person 
      value_of_person[i] <- v_1 - v_2
    }
  }
  
  #take the mean of my person 
  return(mean(value_of_person))
  
}


player_2 <- value(2, 0.02, 0.01)     #M = 324522
player_12 <- value(12, 0.009, 0.005) #M = 332860
player_6 <-value(6, 0.015, 0.006)    #M = 332860

player_2
player_12
player_6

# Bonus -------------------------------------------------------------------


char_function_1 <- function(matrice, lista){
  if(length(lista) == 1){
    return(0)
  }else{
    member <- matrice[lista,]
    vettore <- colSums(member)
    return(sum(vettore[vettore > 1] - 1))
  }
  
}

uno <- c(0,0,0,1,1,1,1,1)
due <- c(1,1,1,1,1,1,1,0)
tre <- c(0,1,1,1,1,1,1,0)
quattro <- c(0,0,0,0,0,1,1,1)
cinque <- c(0,0,0,0,1,1,1,0)
sei <- c(0,0,1,1,1,1,1,0)
sette <- c(1,1,1,1,0,0,0,0)
otto <- c(1,1,1,0,0,0,1,0)
nove <- c(0,1,1,1,1,0,0,0)
dieci <- c(0,0,1,1,1,1,0,1)
undici <- c(1,1,0,0,0,1,1,0)
dodici <- c(0,0,0,1,0,0,1,1)

matrice_prova <- rbind(uno,due,tre,quattro,cinque,sei,sette,otto,nove,dieci,undici,dodici)

char_function_1(matrice_prova, c(12,11,10,9,8,7,6,5,4,3,2,1))
char_function_1(matrice_prova, c(2,1))
char_function_1(matrice_prova, c(2))


x <- seq(1,8)
n <- 200
matrice <- sapply(x, function(x) sample(c(0,1), size = team_member, replace = T, prob = c(0.25,0.75)))
char_function_1(matrice, c(3,14,21))



char_function <- function(matrice, n){
  repet <- list()
  for (i in 1:n){
    a <- CombSet(n, i, repl=FALSE, ord=FALSE)
    for(j in 1:nrow(a)){
      temp <- list(a[j,])
      repet <- append(repet, temp)
    }
  }
  
  risultato <- c(length(repet))
  ris <- list()
  for(i in 1:length(repet)){
    if(length(repet[[i]]) == 1){
      risultato[i] <- 0
    }else{
      member <- matrice[repet[[i]],]
      vettore <- colSums(member)
      risultato[i] <- sum(vettore[vettore > 1] - 1)
    }
   temp <- list(list(risultato[i], repet[[i]]))
   ris <- append(ris, temp)
  }

  return(ris)
}

r <- char_function(matrice_prova, n = 12)

query <- seq(1,12)
for(i in 1:(2^12-1)){
  if(all(r[[i]][[2]] == query)==TRUE){
    print(r[[i]][[1]])
  }
}

query <- c(1,2)
for(i in 1:(2^12-1)){
  if(all(r[[i]][[2]] == query)==TRUE){
    print(r[[i]][[1]])
  }
}


# Part 3 ------------------------------------------------------------------


suppressMessages(require(tseries, quietly = TRUE)) # Load the package
options("getSymbols.warning4.0" = FALSE) # Stop info-messages to show up

# Get Apple Inc. from NYSE
abbv <- get.hist.quote(instrument="ADBE", start="2020-01-01",
                       quote= "Close", provider="yahoo", drop=TRUE)

amzn <- get.hist.quote(instrument="OGN", start="2020-01-01",
                       quote= "Close", provider="yahoo", drop=TRUE)

#OGN

company <- list(abbv,amzn)

n <- length(company[[1]])

MATRICE <- matrix(, nrow = length(company), ncol = n-1,  byrow = T)

x <- c(length(company[[1]])-1)


for(j in 1:length(company)){
  
  for(i in 1:(n-1)){
    
    x[i] <- log(as.numeric(company[[j]][i+1])/as.numeric(company[[j]][i]))
  }
  MATRICE[j,] <- x
}
MATRICE


corrr <-cor.test(MATRICE[1,],MATRICE[2,], conf.level = 0.1)$conf.int

if((corrr[1] < 0) & (corrr[2] >0)){
  ...
}else{
  ...
}



#BOOTSTRAP
rho_hat <- cor(MATRICE[1,],MATRICE[2,])
rho_hat
couples <- CombSet(nrow(MATRICE),2)
n <-50
B <- 1000
brep <- rep(NA, B)
for (b in 1:B){
  idx <- sample (1:n, replace = T)
  bsamp_1 = MATRICE[1,idx]
  bsamp_2 = MATRICE[2,idx]
  btheta_media <- mean(bsamp_1)
  btheta = cor(bsamp_1,bsamp_2)
  brep[b] = btheta
}
se_boot <- sqrt(var(brep))
alpha <- 0.001
z <- qnorm(1 - 0.001/2)
z
c(rho_hat - z * se_boot, rho_hat + z * se_boot)

