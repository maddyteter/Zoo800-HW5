#########Zoo800 Homework 5 - Maddy Teter - 10/2/2025

#Problem One - read data into R

fishcsv <- read.csv("fish.csv")
fishrds <- readRDS("fish.rds")

library(readxl) #package allows readidng in excel files
fishxl <- read_excel("fish.xlsx")

#Print first 5 rows of each df
head(fishcsv, n = 5)
head(fishrds, n = 5)
head(fishxl, n = 5)

#Problem Two - Saving data 

#Choose a dataset (fishcsv) and save it in 3 different formats

library(writexl)

saveRDS(fishcsv, file = "output/fish.rds") #save fishcsv as rds file in output folder
write.csv(fishcsv, file = "output/fish.csv") #save fishcsv as csv file in output folder
write_xlsx(fishcsv, path = "output/fish.xlsx") #save fishcsv as xlsx file in output folder

file.info("fish.rds")
file.info("fish.csv")
file.info("fish.xlsx")

#Problem 3 - wrangling pipeline with dplyr

library(dplyr)
#Filter fish.csv for given species, lakes, and select columns
filtered_fish <-fishcsv %>%
  filter(Species %in% c("Walleye", "Yellow Perch", "Smallmouth Bass"), 
         Lake %in% c("Erie", "Michigan")) %>%
  select(Species, Lake, Year, Length_cm, Weight_g)

#Create column for length in mm
filtered_fish <- filtered_fish %>%
  mutate(Length_mm = Length_cm*10) %>%
  
#create bins column for length_mm 
filtered_fish <- filtered_fish %>%
  mutate(Length_group = cut(Length_mm,
                            breaks = c(-Inf, 200, 400, 600, Inf),
                            labels = c("<200", "200–400", "400–600", ">600")))

#Count the number of fish species in each bin

filtered_fish %>%
  count(Species, Length_group)

#Species × Year, calculate mean weight, median weight and sample size

filtered_fish <- filtered_fish %>%
  group_by(Species, Year) %>%
  mutate(Mean_wt_g = mean(Weight_g), Median_wt_g = median(Weight_g),
         Sample_size = n())

#Create quick plot of sample size by year x species
library(ggplot2)
  ggplot(filtered_fish, aes(Year, Sample_size,
         fill = Species)) +
           geom_bar(stat = "identity", position = "dodge") +
           labs(
             title = "Fish Sample Size by Year and Species",
             x = "Year",
             y = "Sample Size") +
           theme_classic()
           
#Save new code into Output folder 
  write.csv(filtered_fish, file = "output/filtered_fish.csv")


#Problem 4 -Read in all files from "Multiple Files Folder" 
#at once and combine into single df
  
#create path to pull files from
  multiple_files_path <- "C:/Users/maddy/OneDrive/Grad Classes/Zoology 800/Zoo800-HW5/Multiple_files"
#create list of files in folder
  multiple_files_list <- list.files(path = multiple_files_path, 
                                    pattern = "\\.csv$", full.names = TRUE)
#combine all csv files (keeping columns the same)
  multile_files_df <- multiple_files_list %>%
    lapply(read.csv) %>%
    bind_rows()
  
#Problem Five - Parallel computing for bootstap
  library(parallel) 
  fish_parallel_bootstrap <- read.csv("fish_bootstrap_parallel_computing.csv")
  
  species <- unique(fish_parallel_bootstrap$Species) #creates list of species
#Create bootstrap function
  boot_mean <- function(species_name, n_boot = 10000, sample_size = 200) {
    # Pull the weight_g vector for just this species
  x <- fish_parallel_bootstrap$Weight_g[fish_parallel_bootstrap$Species == species_name]
    
    means <- replicate(n_boot, mean(sample(x, size = sample_size, replace = TRUE)))
    
    # Return the average of those bootstrap means (a stable estimate)
    mean(means)
  }
  
  
#Run simulation in serial mode
  
    fish_serial <- system.time({                   # time the whole serial run
      res_serial <- lapply(                     # loop over species in the main R process
        species,                                # input: vector of species names
        boot_mean,                              # function to apply
        n_boot = 10000,                         # number of bootstrap resamples per species
        sample_size = 200                       # bootstrap sample size
      )
    })
  head(res_serial)
  
  
#Run simulation in parallel mode 
  n_cores <- max(1, detectCores() - 1)        # use all but one core (be nice to your laptop)
  cl <- makeCluster(n_cores)                  # start worker processes
  
  clusterSetRNGStream(cl, iseed = 123)        # make random numbers reproducible across workers
  
  # Send needed objects to workers (function + data + species vector)
  clusterExport(cl, varlist = c("fish_parallel_bootstrap", "boot_mean", "species"), envir = environment())
  
  fish_parallel <- system.time({                 # time the parallel run
    res_parallel <- parLapply(                # same API as lapply(), but across workers
      cl,                                     # the cluster
      species,                                # each worker gets one species (or more)
      boot_mean,                              # function to run
      n_boot = 10000,                           # same bootstrap settings as serial
      sample_size = 200
    )
  })
  stopCluster(cl)       

  
#Compare run time between serial and parallel mode 

  # Extract elapsed (wall) time and compute speedup = serial / parallel
  elapsed_serial   <- unname(fish_serial["elapsed"])
  elapsed_parallel <- unname(fish_parallel["elapsed"])
  speedup <- elapsed_serial / elapsed_parallel
  
  cat("Serial elapsed (s):   ", round(elapsed_serial, 3), "\n")
  cat("Parallel elapsed (s): ", round(elapsed_parallel, 3), " using ", n_cores, " cores\n", sep = "")
  cat("Speedup:               ", round(speedup, 2), "x\n", sep = "")
  
  #Serial run time = 1.27 sec, Parallel run time = 0.33 sec using 15 cores