load("~/560-Project/clean-data/data/landUse_clean.rds")

test = landUse_clean
save(test, file = "test.rds")

load("~/560-Project/clean-data/data/test.rds")

test = test |> 
  select(-county)

save(test, file = "test.rds")
