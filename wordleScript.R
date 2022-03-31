if(!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

##Read in data
data <- read_csv("https://raw.githubusercontent.com/garyongguanjie/entrie/main/unigram_freq.csv") %>% 
  filter(nchar(word) == 5) 

##Construct vectors for output
gray <- c()
yellow1 <- c()
yellow2 <- c()
yellow3 <- c()
yellow4 <- c()
yellow5 <- c()
green <- vector("character", 5)
included <- c()
vec_prompt <- c("first", "second", "third", "fourth", "fifth", "sixth")
end <- FALSE

##Main loop
for (i in 1:6) {
  word <- readline(prompt= paste("What was your ", vec_prompt[i], " word?:"))
  word <- tolower(word)
  while (TRUE) {
    correct <- readline(prompt = "Was this correct? (Y or N)")
    correct <- toupper(correct)
    if (correct=="Y") {
      cat("Congratulations!")
      end <- TRUE
      break
    } else if (correct == "N") {
      break
    }
  }
  if (end) {
    break
  }
  #First letter
  while (TRUE) {
    letter1 <-str_sub(word, start=1, end=1)
    letterResult1 <- readline(prompt= "Was your first letter gray (1), green (2), or yellow (3)? (Input the number 1, 2, or 3):")
    if (letterResult1==1) {
      gray <- append(gray, letter1)
      break
    } else if (letterResult1==2) {
      green[1] <- letter1
      break
    } else if (letterResult1==3) {
      yellow1 <- append(yellow1, letter1)
      included <- append(included, letter1)
      break
    } else {
      print("Please enter a number 1 for gray, 2 for green, or 3 for yellow.")
    }
  }
  #Second letter
  while (TRUE) {
    letter2 <-str_sub(word, start=2, end=2)
    letterResult2 <- readline(prompt= "Was your second letter gray (1), green (2), or yellow (3)? (Input the number 1, 2, or 3):")
    if (letterResult2==1) {
      gray <- append(gray, letter2)
      break
    } else if (letterResult2==2) {
      green[2] <- letter2
      break
    } else if (letterResult2==3) {
      yellow2 <- append(yellow2, letter2)
      included <- append(included, letter2)
      break
    } else {
      print("Please enter a number 1 for gray, 2 for green, or 3 for yellow.")
    }
  }
  
  while (TRUE) {
    letter3 <-str_sub(word, start=3, end=3)
    letterResult3 <- readline(prompt= "Was your third letter gray (1), green (2), or yellow (3)? (Input the number 1, 2, or 3):")
    if (letterResult3==1) {
      gray <- append(gray, letter3)
      break
    } else if (letterResult3==2) {
      green[3] <- letter3
      break
    } else if (letterResult3==3) {
      yellow3 <- append(yellow3, letter3)
      included <- append(included, letter3)
      break
    } else {
      print("Please enter a number 1 for gray, 2 for green, or 3 for yellow.")
    }
  }
  
  while (TRUE) {
    letter4 <-str_sub(word, start=4, end=4)
    letterResult4 <- readline(prompt= "Was your fourth letter gray (1), green (2), or yellow (3)? (Input the number 1, 2, or 3):")
    if (letterResult4==1) {
      gray <- append(gray, letter4)
      break
    } else if (letterResult4==2) {
      green[4] <- letter4
      break
    } else if (letterResult4==3) {
      yellow4 <- append(yellow4, letter4)
      included <- append(included, letter4)
      break
    } else {
      print("Please enter a number 1 for gray, 2 for green, or 3 for yellow.")
    }
  }
  
  while (TRUE) {
    letter5 <-str_sub(word, start=5, end=5)
    letterResult5 <- readline(prompt= "Was your fifth letter gray (1), green (2), or yellow (3)? (Input the number 1, 2, or 3):")
    if (letterResult5==1) {
      gray <- append(gray, letter5)
      break
    } else if (letterResult5==2) {
      green[5] <- letter5
      break
    } else if (letterResult5==3) {
      yellow5 <- append(yellow5, letter5)
      included <- append(included, letter5)
      break
    } else {
      print("Please enter a number 1 for gray, 2 for green, or 3 for yellow.")
    }
  }
  
  
  
  if (length(gray)>0) {
    for (i in 1:length(gray)) {
      if (!(gray[i] %in% green)) {
        data <- filter(data, str_detect(word, gray[i], negate = T))
      }
    }
    
  }
  
  if (length(yellow1 > 0)) {
    for (i in 1:length(yellow1)) {
      data <- filter(data, str_sub(word, 1,1)!=yellow1[i])
    }
  }
  
  if (length(yellow2 > 0)) {
    for (i in 1:length(yellow2)) {
      data <- filter(data, str_sub(word, 2,2)!=yellow2[i])
    }
  }
  
  if (length(yellow3 > 0)) {
    for (i in 1:length(yellow3)) {
      data <- filter(data, str_sub(word, 3,3)!=yellow3[i])
    }
  }
  
  if (length(yellow4 > 0)) {
    for (i in 1:length(yellow4)) {
      data <- filter(data, str_sub(word, 4,4)!=yellow4[i])
    }
  }
  
  if (length(yellow5 > 0)) {
    for (i in 1:length(yellow5)) {
      data <- filter(data, str_sub(word, 5,5)!=yellow5[i])
    }
  }
  
  if (length(included >0)) {
    for (i in 1:length(included)) {
      data <- filter(data, str_detect(word, included[i], negate= F))
    }
  }
  
  for (i in 1:5) {
    if (green[i] != "") {
      data <- filter(data, str_sub(word, i,i)==green[i])
    }
  }
  cat("Below are the most common words that are still possible:\n")
  print(head(data, 20))
}