## ---- preambule
#
# last modified by Sebastien Chaurin 12/11/2019
#

# camelCase for functions
# CAPITAL_CASE for column headers
# lower_case for variables

#****************************************************
#    
#****************************************************

# clear global environment
# rm(list = ls())

savePics <- FALSE

#   ____________________________________________________________________________
#   User specific assignments                                               ####

# don't load libraries for frequent users
load_libraries <- FALSE
# set working directory from user info
if (tolower(Sys.info()[names(Sys.info()) == "user"]) == tolower("SC68989")) {
  setwd("C:/R/WorkingTemp/Blockchain")
  .libPaths('C:/R/WorkingTemp/R')
  #.libPaths()
} else if (tolower(Sys.info()[names(Sys.info()) == "user"]) == tolower("sebastien.chaurin")) {
  setwd("C:/R/WorkingTemp/Blockchain")
  .libPaths('C:/R/WorkingTemp/R')
  #.libPaths()	
} else if (tolower(Sys.info()[names(Sys.info()) == "user"]) == tolower("sebastien chaurin")) {
  setwd("C:/R/WorkingTemp/Blockchain")
  .libPaths('C:/R/WorkingTemp/R')
  #.libPaths()
} else {
  print("Ensure you have set your working directory")
  load_libraries <- TRUE
}


if (load_libraries) {
  install.packages(c("digest", "dplyr", "purrr", "ggplot2"))
}
library(digest)
library(dplyr)
library(purrr)
library(ggplot2)

#   ____________________________________________________________________________
#   A block example                                                         ####

block_example <- list(index = 1,
                      timestamp = "2018-01-05 17.00 MST",
                      data = "some data",
                      previous_hash = 0,
                      proof = 9,
                      new_hash = NULL)

#   ____________________________________________________________________________
#   Introducing the hash                                                    ####

digest("Stata" ,"sha256") # first try
digest("R", "sha256") # second try

#   ____________________________________________________________________________
#   Function that creates a hashed "block"                                  ####

hash_block <- function(block){
  block$new_hash <- digest(c(block$index,
                             block$timestamp,
                             block$data,
                             block$previous_hash), "sha256")
  return(block)
}

#   ____________________________________________________________________________
#   Simple Proof of Work Alogrithm                                          ####

proof_of_work <- function(last_proof){
  proof <- last_proof + 1
  
  # Increment the proof number until a number is found that is divisable by 99 and by the proof of the previous block
  while (!(proof %% 99 == 0 & proof %% last_proof == 0 )) {
    proof <- proof + 1
  }
  
  return(proof)
}

#   ____________________________________________________________________________
#   Adding new blocks                                                       ####

gen_new_block <- function(previous_block){
  
  #Proof-of-Work
  new_proof <- proof_of_work(previous_block$proof)
  
  #Create new Block
  new_block <- list(index = previous_block$index + 1,
                    timestamp = Sys.time(),
                    data = paste0("this is block ", previous_block$index + 1),
                    previous_hash = previous_block$new_hash,
                    proof = new_proof)
  
  #Hash the new Block
  new_block_hashed <- hash_block(new_block)
  
  return(new_block_hashed)
}

#   ____________________________________________________________________________
#   Genesis                                                                 ####

block_genesis <-  list(index = 1,
                       timestamp = Sys.time(),
                       data = "Genesis Block",
                       previous_hash = "0",
                       proof = 1)

#   ____________________________________________________________________________
#   Building the blockchain                                                 ####

blockchain <- list(block_genesis)
previous_block <- blockchain[[1]]

# How many blocks should we add to the chain after the genesis block
num_of_blocks_to_add <- 20

# Add blocks to the chain
for (i in 1:num_of_blocks_to_add) {
  block_to_add <- gen_new_block(previous_block) 
  blockchain[i + 1] <- list(block_to_add)
  previous_block <- block_to_add
  
  print(cat(paste0("Block ", block_to_add$index, " has been added", "\n",
                   "\t", "Proof: ", block_to_add$proof, "\n",
                   "\t", "Hash: ", block_to_add$new_hash)))
}

#   ____________________________________________________________________________
#   Time to mine a new block                                                ####

df <- data.frame(BLOCK_INDEX = map(blockchain, 1) %>%
                   unlist(),
                 TIMESTAMP = map(blockchain, 2) %>%
                   unlist() %>%
                   as.POSIXct(origin = '1970-01-01')) %>% 
  mutate(ELAPSED = as.numeric(difftime(TIMESTAMP, blockchain[[2]]$timestamp), units = "secs", origin = "CEST"),
         ELAPSED = ifelse(ELAPSED < 0,
                          0,
                          ELAPSED))

df %>% 
  filter(BLOCK_INDEX > 1) %>% # Discard genesis block
  ggplot(aes(x = BLOCK_INDEX, y = ELAPSED)) +
  geom_line() +
  labs(title = "Time it takes to mine blocks",
       x = "Number of blocks",
       y = "Elapsed time (Sec)")
