#pipeline
x <- seq(1:12)
#M is the unknown variable
#M <- ((b-a)^2/2*eps^2) * log(2/alfa)

#what is b???


M <- 10^4 #is a random value just to try, not correct
#pick a person
person = "1"
person_int = as.numeric(person)
value_of_person <- c(length(M))
for (i in 1:M){
  
  #sample
  campione <- sample(x)
  
  #discover the position of my person
  position <- which(campione == person_int) 
  
  #sort element until my person
  element <- sort(campione[1:position])
  
  #write in the same format of char_fun
  element_1<- paste(element, collapse = "-")
  
  #compute the value with my person
  v_1 <- char_fun[[position]][element_1] 
  
  if(v_1 == 0){
    value_of_person[i] <- 0
  }else{
    
    #delete my person from the sequence
    element_2 <- sort(element[element != person_int])
    
    #write in the same format of char_fun
    element_3 <- paste(element_2, collapse = "-")
    
    #compute the value without my person
    v_2 <- char_fun[[position-1]][element_3]
    
    #compute the value of my person 
    value_of_person[i] <- v_1 - v_2
  }
}

#take the mean of my person 
mean(value_of_person)