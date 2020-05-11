

# Toy example

model <- c('Marina', 'David', 'Carl', 'Matt')
scenario <- c('A', 'B')
cntry <- c('C1', 'C2')
sector <- c('X', 'Y', 'Z')
x <- c(1:200)

toy.ex <- expand.grid(model = model, cntry = cntry, scenario = scenario, sector = sector) %>% 
  mutate(invest = sample(x, size = 48))
                  
# Method 1: Calculate increamentals for each sector, for each model, and average across models

eg1 <- toy.ex %>% 
  spread(scenario, invest) %>% 
  mutate(diff = A - B) %>% 
  group_by(cntry, sector) %>% 
  mutate(mod.mean = mean(diff)) %>% 
  ungroup()

# Method 2: Calculate average investment by sector, then increamentals of averages

eg2 <- toy.ex %>% 
  group_by(cntry, sector, scenario) %>% 
  mutate(mod.mean = mean(invest)) %>% 
  ungroup() %>% 
  select(-invest) %>% 
  spread(scenario, mod.mean) %>% 
  mutate(diff = A - B)
  
