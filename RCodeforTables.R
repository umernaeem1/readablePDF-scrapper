# install these libraries first using install.packages("Package name")
library(tabulizer)
library(janitor)
library(tidyverse)

##################### Part 1: Preparations ##########################

######### Setting Directories
setwd('C:/Users/unaeem/Dropbox (OPML)/Gustavo/Simplicita/pdf scan/')

######## Getting names of all files
nameOnly <- list.files('./mmary')

##################### Part 2: Defining functions ##########################

######## Function 1: To get names in proper format
nameFun <- function(x){
  l <- lapply(x, tools::file_path_sans_ext)
  do.call(c, l)
}

######## Function 2: To clean each table within a document
tabClean <- function(x) {
  
  # First get all tables as R data.frame
  df <- as.data.frame(x)
  
  # This loop is for cases where the table has only 1 column
  if ((length(df) == 1) & (sum(str_detect(df$V1, 'Descripción Breve')) > 0)) {
    colnames(df) <- c('Descripción Breve')
    df <- tail(df, -1)
    return(df)
} else {
  message('Not valid, only has 1 column')
}
  
  # This is the main loop for tables with more than one column
  # The first 'if' argument is just filtering the Annex tables and getting a normal dataframe
  if ((sum(str_detect(df[-1,], 'Anexos')) == 0) & (length(df) > 1)) {
    print("Annex loop is active")
    
    # This is to get that table column which has word "description" in it. The rest will be discarded
    if (sum(str_detect(df$V2, 'Descripción')) > 0) {
      print('Breve loop is active')
      df <- df[, colSums(df != "") != 0] # removing empty columns
      print('frist if working')
      
      # If there are multiple events of word "description" in same column, then choose the last one
      if (sum(str_detect(df$V2, 'Descripción Breve')) >= 1) {
        y <- max(which(str_detect(df$V2, 'Descripción Breve')))
        print('max loop is workig')
      } else {
        y <- which(str_detect(df$V2, 'Descripción'))
      }
      
      # This is for a normal table which has word "description in it"
      if (length(y) > 0) {
        df <- df[y[1]:nrow(df),]
        df <- subset(df, V2 != "")
        
        # Most of the time, descriptions are in multiple rows. This is to overcome that
        if (length(df) >= 3) {
          m <- sum(!str_detect(df[,3], ""))
        } else {
          m <- 0
        }
        
        n <- sum(!str_detect(df$V2, ""))
        df <- df[-1,] ## removing first column
        print("this is wokring")
        
        # Data cleaning
        df <- subset(df, V2 != "Sí")
        df <- subset(df, V2 != "Breve")
        
        # This is related to above. When descriptions are in multiple rows
        if ((m > n) & (dim(df)[1] > 0)) {
          df <- df %>%
            na_if("") %>%
            mutate(uniq = 1:n()) %>%
            mutate(uniq = ifelse(df[,3] == "", NA, uniq)) %>%
            fill(everything(),.direction = "updown") %>%
            group_by(uniq) %>% 
            summarise_all(funs(trimws(paste(., collapse = ' '))))
        }
        
        # If not in multiple rows then this will work. It will work in any case though
        df <- select(df, c('V2'))
        colnames(df) <- c('Descripción Breve')
        print('final check before data save is working')
        return(df)
      } else {
        message(paste(pdf, 'is not valid for this'))
      }
      
    } else{ # When we have data in multiple tables, this will work
      print("The append loop is active")
      #message(paste(pdf, 'is not valid for this'))
      df <- df["V2"]
      colnames(df) <- c('Descripción Breve')
      return(df)
    }
  } else {
    message(paste(pdf, 'is not valid for this'))
  }
}

######## Function 3: This function is for pdf files
pdftab <- function(x){
  
  # Here directory is required
  pdf = paste0('./mmary/',x, '.pdf')
  
  # I will first use 'stream' method as it has proven to be most successful
  tab <- extract_tables(pdf, method = 'stream', encoding = 'UTF-8')
  last = length(tab)
  
  if (dim(tab[[last]])[2] < 5) {
    tab <- extract_tables(pdf, method = 'lattice', encoding = 'UTF-8')
    last = length(tab)
    print('Lattice working')
    
    if (dim(tab[[last]])[2] < 4) {
      tab <- extract_tables(pdf, method = 'decide', encoding = 'UTF-8')
      tab <- tab[length(tab)] ## Only picking last table
      print('Decide Working')
    }
  }
  
  # This is to filter all tables that are not useful. Usually with less than 4 columns
  v <- c()
  j <- 0
  
  for (i in seq(length(tab))) {
    j <- j + 1
    if (dim(tab[[i]])[2] < 5) {
      v[j] <- i
    }
  }
  
  v <- v[!is.na(v)]
  if (length(v) > 0) {
    newTab <- tab[-v]
  } else {
    newTab <- tab
  }
  
  # This is to get individual tables as data
  # This first if is about when we have a normal document with nice tables
  if (length(newTab) > 0) {
    DL = list()
    for (i in seq(newTab)) {
      df <- tabClean(newTab[[i]])
      DL[[i]] <- df
    }
    tabData <- bind_rows(DL)
    
    # However, sometimes mmary docs do not have a table. This is for that
    if (dim(tabData)[1] > 0) {
      tabData['DocID'] <- substr(x,1,7)
      return(tabData)
    }
  } else { # This is when there is no table extracted from a document
    DL = list()
    for (i in seq(tab)) {
      df <- tabClean(tab[[i]])
      DL[[i]] <- df
    }
    tabData <- bind_rows(DL)
    if (dim(tabData)[1] > 0) {
      tabData['DocID'] <- substr(x,1,7)
      return(tabData)
    }
  }
}


### Function 3: This function is for all pdf files
bigD <- function(x) {
  datalist = list()
  for (i in x) {
    print(i)
    df <- pdftab(i)
    datalist[[i]] <- df
  }
  big_data <- bind_rows(datalist)
  
  ## Data Cleaning steps
  big_data <- subset(big_data, `Descripción Breve` != "")
  big_data <- subset(big_data, `Descripción Breve` != "Breve")
  big_data <- subset(big_data, `Descripción Breve` != "Sí")
  big_data <- subset(big_data, `Descripción Breve` != "Descargable")
  big_data <- subset(big_data, `Descripción Breve` != "ClaveCUCOP")
  big_data <- subset(big_data, `Descripción Breve` != "Clave CUCOP")
  
  big_data <- unique(big_data)
  #return(big_data)
  big_data <- big_data %>% filter(is.na(as.numeric(big_data$`Descripción Breve`)))
  
  #df1 <- df %>%
  #  group_by(DocID) %>% 
  #  summarise_all(funs(trimws(paste(., collapse = '\n'))))
  
  # Change the directory accordingly
  write.csv(big_data, './Output/mmary.csv', row.names = FALSE)
}

names <- nameFun(nameOnly)
test <- names[525:535]

# Test
bigD(test)

# Full
bigD(names)







###################### Testing for type 2: that cannot be read by "decide"
pdf1 <- names[20]
pdf <- paste0("./mmary/", "2218797_mmary.1044544.pdf")
"2239583_mmary.1046316.pdf"
pdf1 <- "2218797_mmary.1044544"
df <- pdftab(pdf1)

tab <- extract_tables(pdf, method = 'lattice', encoding = 'UTF-8')
df <- as.data.frame(tab[[2]])

df = df[, colSums(df != "") != 0] # removing empty columns
x <- which(str_detect(df$V2, 'Descripción'))
df <- df[x[1]:nrow(df),]
df <- subset(df, V2 != "")
m <- sum(!str_detect(df[,3], ""))
n <- sum(!str_detect(df$V2, ""))
df <- df[-1,] ## removing first column
df <- subset(df, V2 != "Breve")

df <- df %>%
  na_if("") %>%
  mutate(uniq = 1:n()) %>%
  mutate(uniq = ifelse(df[,3] == "", NA, uniq)) %>%
  fill(everything(),.direction = "updown") %>%
  group_by(uniq) %>% 
  summarise_all(funs(trimws(paste(., collapse = ' '))))

tab <- tab[7]
v <- c()
j <- 0

for (i in seq(length(tab))) {
  j <- j + 1
  if (dim(tab[[i]])[2] < 5) {
    v[j] <- i
  }
}
colnames(df) <- c('Descripción Breve')
df <- tail(df, -1)
df <- df[-1,]
df <- tabClean(tab[[6]])

df1 <- df %>%
  filter(Descripción.Breve != 'ClaveCUCOP') %>%
  filter(Descripción.Breve != 'Clave CUCOP') %>%
  group_by(DocID) %>% 
  summarise_all(funs(trimws(paste(., collapse = '\n'))))

