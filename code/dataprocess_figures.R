
library(ggplot2)
library(tidyverse)
library(broom)
library(countrycode)
library(readstata13)
library(zoo)
library(plm)
library(broom)
library(readxl)
library(tibble)
library(RColorBrewer)
library(cowplot)


# Comparing energy sector investments from McCollum et al. (2018) and COVID-19-related stimulus packages

# IAM regions for aggregation of energy investments and stimulus packages

iam.regs <- read_excel('data/iam_regions.xlsx') %>% 
  mutate(country.code = countrycode(country, 'country.name', 'iso3c'),
         region = recode(region, 'OECD90+EU' = 'OECD+')) %>% 
  filter(!country == 'Netherlands Antilles') %>% 
  mutate(country.code = country.code %>% paste %>% str_replace('NA', 'EU'),
         country.code = recode(country.code, 'EUM' = 'NAM')) #change iso3 code for Namibia

# Region/country groupings for aggregations later on

reg1 <- c('ASIA', 'LAM', 'MAF', 'OECD+','REF', 'World') 
reg2 <- c('CHN', 'EU', 'USA', 'IND')

# Stocktake of stimulus packages for G20 (IMF, 2020)

# Country level

stim.cntry <- read.csv('data/global_stimulus.csv', header = T, na.strings = '') %>% 
  select(country.code,region, total.stim.usd, individuals.households, health, business, deferrals, loans.guarantees, uncategorized.other) %>%
  gather(component, stimulus, -country.code, -region, -total.stim.usd) %>% 
  mutate(stimulus = ifelse(is.na(stimulus), 0, stimulus), # Replace empty cells with zeros
         region = recode(region, 'OECD90+EU' = 'OECD+')) %>% 
  spread(component, stimulus) %>% 
  mutate('Individuals' = individuals.households, # Aggregate into four categories
         'Health' = health,
         'Economy' = business+deferrals+loans.guarantees,
         'General' = uncategorized.other) %>% 
  select(country.code, region, 'Individuals', 'Health', 'Economy', 'General') %>% 
  gather(component, stimulus, -country.code, -region) %>% 
  drop_na(country.code) 

# EU member states for aggregation of stimulus packages on the EU level

eu.ms <- c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 
           'IRL', 'ITA', 'LVA', 'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROM', 'SVK', 'SVN', 'ESP', 'SWE')

# Aggegregation of stimulus pacakges for four big economies: China, European Union (supranational + member states), India, United States

big.cntry <- stim.cntry %>% 
  mutate(country.code = ifelse(country.code %in% eu.ms, 'EU', as.character(country.code))) %>% 
  group_by(country.code, component) %>% 
  mutate(Stimulus = sum(stimulus, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(id = paste0(country.code, region, component)) %>%
  filter(!duplicated(id) & !region == 'EU') %>% 
  filter(country.code %in% reg2)

# Stimulus packages aggregated by IAM regions

stim.reg <- stim.cntry %>% 
  group_by(component) %>% 
  mutate(World = sum(stimulus)) %>% 
  ungroup() %>% 
  group_by(region, component) %>% 
  mutate(Stimulus = sum(stimulus, na.rm = T)) %>% 
  ungroup() %>% 
  select(region, component, Stimulus, World) %>% 
  drop_na(region) %>% 
  mutate(id = paste0(region, component)) %>% 
  filter(!duplicated(id)) %>% 
  select(-id) %>% 
  spread(region, Stimulus) %>% 
  pivot_longer('World':'REF', names_to = 'region', values_to = 'Stimulus')

master.stim <- big.cntry %>% 
  select(-region, -id, -stimulus) %>% 
  rename(region = country.code) %>% 
  bind_rows(stim.reg) %>% 
  arrange(region)

# Figures

# Energy investments data from McCollum et al. 

e.type1  <- c('Energy Efficiency', 'CCS', 'Electricity - T&D and Storage', 'Extraction and Conversion - Nuclear', 
              'Extraction and Conversion - Bioenergy', 'Hydrogen - Non-fossil', 'Electricity - Non-bio Renewables', 
              'Hydrogen - Fossil', 'Electricity - Fossil Fuels w/o CCS', 'Extraction and Conversion - Fossil Fuels') # selected sectors to be aggreagted below

e.group2 <- c('Fossil fuels', 'Low carbon sources') # for aggregation to only two broad groups of energy investments

invest.prep <- read_excel('data/NE_paper_SM_data.xlsx', sheet = 'Investment_Annual') %>% 
  rename_all(tolower) %>% 
  mutate(region = recode(region, 'R5ASIA' = 'ASIA', 'R5LAM' = 'LAM', 'R5MAF' = 'MAF', 'R5OECD90+EU' = 'OECD+', 'R5REF' = 'REF')) %>%
  select(model, region, scenario, variable, '2020':'2025') %>% 
  filter(variable %in% e.type1) %>% 
  pivot_longer('2020':'2025', names_to = 'year', values_to = 'investment') %>% 
  mutate(year = as.integer(year), 
         investment = str_remove(investment, "%") %>% as.numeric(investment)) 

# Linear interpolation between 2020 and 2025

interp <- function(df) {
  with(df, approx(x=index, y=value, xout=min(index):max(index))) %>% 
    bind_cols()
}

invest.interp <- invest.prep %>% 
  group_by(scenario, model, region, variable) %>% 
  rename(index = year, value = investment) %>% 
  do(interp(.)) %>% 
  rename(year = x, investment = y) %>% 
  right_join(invest.prep %>% distinct(region), by = 'region')


# Master investment dataset

invest <- invest.interp %>% 
  spread(variable, investment) %>% 
  mutate('Fossil fuels' = `Electricity - Fossil Fuels w/o CCS` + `Hydrogen - Fossil` + `Extraction and Conversion - Fossil Fuels`,
         'Low carbon sources' = `Extraction and Conversion - Nuclear` + `CCS` + `Electricity - Non-bio Renewables` + `Hydrogen - Non-fossil` 
         + `Extraction and Conversion - Bioenergy` + `Electricity - T&D and Storage` + `Energy Efficiency`) %>% 
  gather(variable, investment, -c(model, region, scenario, year)) %>% 
  filter(variable %in% e.group2) %>% 
  mutate(investment = investment * 1.0728) #adjust for inflation

# Derive shifts in investments from current policies to 1.5C-compatible energy sector

# For 2020-2025
shift.invest <- invest %>% 
  filter(!year == 2025) %>% 
  group_by(region, variable, scenario, year) %>% 
  mutate(mod.avg = mean(investment)) %>% # model average
  ungroup() %>% 
  select(-investment) %>% 
  filter(model == 'MESSAGEix-GLOBIOM') %>% # not actual filtering but removing duplicates after taking model average above
  group_by(region, variable, scenario) %>% 
  mutate(total.invest = sum(mod.avg)) %>% # sum model averages for the time period
  ungroup() %>% 
  select(-mod.avg) %>% 
  spread(scenario, total.invest) %>% 
  filter(year == 2020) %>%  # since the values are aggregated over time, remove duplicates here 
  mutate(shift = `1.5C` - `CPol`) %>% # calculate the difference between scenarios per sector
  group_by(region) %>% 
  mutate(net = sum(shift)) %>% # show net investments for the 1.5C shift
  ungroup() %>% 
  select(-`NDC`, -`2C`, -model)


# # # # # # # # 
#   Figures   #
# # # # # # # #

# Energy investments

# Total 

# IAM regions
p1 <- ggplot(shift.invest %>% filter(region %in% reg1 & variable %in% e.group2)) +
  geom_bar(aes(region, `CPol`, fill = factor(variable, levels = e.group2)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  labs(x = 'Scenario', y = 'USD billion', fill = '', title = 'Total investments (2020-2025)\nCurrent policy') + #total investments 2020-2025
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 12), 
        legend.key.height=unit(1, "cm"), 
        legend.text = element_text(size = 7),
        axis.title.x = element_blank()) +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a')) +
  scale_y_continuous(limits = c(0, 12000), breaks = c(0, 2000, 4000, 6000, 8000, 10000, 12000))


p2 <- ggplot(shift.invest %>% filter(region %in% reg1 & variable %in% e.group2)) +
  geom_bar(aes(region, `1.5C`, fill = factor(variable, levels = e.group2)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  labs(x = 'Scenario', y = 'USD billion', fill = '', title = 'Total investments (2020-2025)\n1.5C pathway') + #total investments 2020-2025
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12), 
        legend.key.height=unit(1, "cm"), 
        legend.text = element_text(size = 7),
        axis.title.x = element_blank()) +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a')) +
  scale_y_continuous(limits = c(0,12000), breaks = c(0, 2000, 4000, 6000, 8000, 10000, 12000))

plot_grid(p1, p2, nrow = 1, rel_widths = c(2/5, 3/5))

# Four big economies

p3 <- ggplot(shift.invest %>% filter(region %in% reg2 & variable %in% e.group2)) +
  geom_bar(aes(region, `CPol`, fill = factor(variable, levels = e.group2)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  labs(x = 'Scenario', y = 'USD billion', fill = '', title = 'Total investments (2020-2025)\nCurrent policy') + #total investments 2020-2025
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 12), 
        legend.key.height=unit(1, "cm"), 
        legend.text = element_text(size = 7),
        axis.title.x = element_blank()) +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a')) +
  scale_y_continuous(limits = c(0, 2500), breaks = c(0, 1000, 2000))


p4 <- ggplot(shift.invest %>% filter(region %in% reg2 & variable %in% e.group2)) +
  geom_bar(aes(region, `1.5C`, fill = factor(variable, levels = e.group2)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  labs(x = 'Scenario', y = 'USD billion', fill = '', title = 'Total investments (2020-2025)\n1.5C pathway') + #total investments 2020-2025
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12), 
        legend.key.height=unit(1, "cm"), 
        legend.text = element_text(size = 7),
        axis.title.x = element_blank()) +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a')) +
  scale_y_continuous(limits = c(0, 2500), breaks = c(0, 1000, 2000))

plot_grid(p3, p4, nrow = 1, rel_widths = c(2/5, 3/5))


# Stimulus packages

# Total

stim.col <- brewer.pal(9, "Blues")[3:9] #colors for the stiumulus pacakges

ggplot(stim.reg %>% filter(region %in% reg1)) + 
  geom_bar(aes(region, Stimulus/1e9, fill = component), width = 0.6, stat = 'identity') +
  labs(x = 'Country', y = 'USD billion', title = 'Stimulus packages\n ') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12),
        axis.title.x = element_blank()) +
  scale_fill_manual(values = stim.col) 

ggplot(big.cntry %>% filter(country.code %in% reg2)) + 
  geom_bar(aes(country.code, Stimulus/1e9, fill = component), width = 0.6, stat = 'identity') +
  labs(x = 'Country', y = 'USD billion', title = 'Stimulus packages\n ') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12),
        axis.title.x = element_blank()) +
  scale_fill_manual(values = stim.col)

# Shifts in energy investments vs stimulus packages

p5 <- ggplot(shift.invest %>% filter(region %in% reg1 & variable %in% e.group2)) +
     geom_bar(aes(region, shift, fill = str_wrap(factor(variable, levels = e.group2), 10)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
     labs(x = 'Scenario', y = 'USD billion', fill = '', title = 'Relative change in energy investments:\n1.5C vs. current policy (2020-2025)') + #total investments 2020-2025
     theme_bw() +
     theme(axis.text.x = element_text(angle = 90, size = 12), 
            #legend.text = element_text(size = 7),
            axis.title.x = element_blank()) +
     scale_fill_manual(values = c('Fossil\nfuels' = '#f5a788', 
                                  'Low carbon\nsources' = '#acb96a')) + 
     geom_point(aes(region, net), shape=95, size=15, alpha = 0.40, color = 'black') + 
     scale_y_continuous(limits = c(-2000, 9000), breaks = c(seq(-2000, 8000, 2000)))

p6 <- ggplot(stim.reg %>% filter(region %in% reg1)) + 
     geom_bar(aes(region, Stimulus/1e9, fill = component), width = 0.6, stat = 'identity') +
     labs(x = 'Country', y = 'USD billion', fill = '', title = 'Stimulus packages\n ') +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 90, size = 12),
           axis.title.x = element_blank()) +
     scale_fill_manual(values = stim.col) +
     scale_y_continuous(limits = c(-2000, 9000), breaks = c(seq(-2000, 8000, 2000)))

plot_grid(p5, p6, nrow = 1)


p7 <- ggplot(shift.invest %>% filter(region %in% reg2 & variable %in% e.group2)) +
     geom_bar(aes(region, shift, fill = str_wrap(factor(variable, levels = e.group2), 10)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
     labs(x = 'Scenario', y = 'USD billion', fill = '', title = 'Relative change in energy investments:\n1.5C vs. current policy (2020-2025)') + #total investments 2020-2025
     theme_bw() +
     theme(axis.text.x = element_text(angle = 90, size = 12), 
           #legend.text = element_text(size = 7),
            axis.title.x = element_blank()) +
     scale_fill_manual(values = c('Fossil\nfuels' = '#f5a788', 
                                   +                               'Low carbon\nsources' = '#acb96a')) + 
     geom_point(aes(region, net), shape=95, size=15, alpha = 0.40, color = 'black') + 
     scale_y_continuous(limits = c(-1000, 4000), breaks = c(seq(-1000, 4000, 1000)))

p8 <- ggplot(big.cntry %>% filter(country.code %in% reg2)) + 
     geom_bar(aes(country.code, Stimulus/1e9, fill = component), width = 0.6, stat = 'identity') +
     labs(x = 'Country', y = 'USD billion', title = 'Stimulus packages\n ') +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 90, size = 12),
           axis.title.x = element_blank()) +
    scale_fill_manual(values = stim.col) +
    scale_y_continuous(limits = c(-1000, 4000), breaks = c(seq(-1000, 4000, 1000)))

plot_grid(p7, p8, nrow = 1)



# Energy investments and stimulus packages relative to GDP

# GDP data (World Bank) aggregated regionally 

gdp.reg <- read.csv("data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_988718.csv", skip = 3) %>% 
  rename_all(tolower) %>% 
  left_join(iam.regs, by = 'country.code') %>% 
  filter(!region == 'EU' & !duplicated(country.code)) %>% 
  pivot_longer(x1960:x2019, names_to = "year", values_to = "gdp") %>% 
  mutate(year = year %>% str_replace("x", "")) %>% 
  filter(year == 2018) %>% 
  select(region, gdp) %>% 
  group_by(region) %>% 
  mutate(gdp = sum(gdp, na.rm = T)) %>% 
  ungroup() %>% 
  filter(!duplicated(region)) %>% 
  mutate(World = sum(gdp, na.rm = T),
         EU = 1.8e13) %>% # Add the EU manually
  spread(region, gdp) %>% 
  gather(region, gdp) %>% 
  as.data.frame()

gdp.cntry <- read.csv("data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_988718.csv", skip = 3) %>% 
  rename_all(tolower) %>% 
  pivot_longer(x1960:x2019, names_to = "year", values_to = "gdp") %>% 
  mutate(year = year %>% str_replace("x", "")) %>% 
  filter(year == 2018) %>%
  mutate(EU = 1.8e13) %>% 
  filter(country.code %in% c('USA', 'CHN', 'IND')) %>% 
  select(country.code, gdp, EU) %>% 
  spread(country.code, gdp) %>% 
  gather(region, gdp)




# Calculate average yearly investment per sector for the 1.5C pathway (2020-2025)

scenario <- data.frame(scenario = c('1.5C', 'CPol'))

stim.prep <- big.cntry %>% 
  select(-region, -id, -stimulus) %>% 
  rename(region = country.code) %>% 
  bind_rows(stim.reg) %>% 
  rename(variable = component,
         avg.invest = Stimulus) %>% 
  merge(scenario)

gdp.shares <- invest %>% 
  group_by(region, variable, scenario, year) %>% 
  mutate(avg.invest = mean(investment)) %>% # mean across models
  ungroup() %>% 
  filter(model == 'MESSAGEix-GLOBIOM') %>% # removing duplicates after averaging across models 
  select(-model) %>% 
  group_by(region, variable, scenario) %>% 
  mutate(avg.invest = mean(avg.invest)*1e9) %>% # mean across years 
  ungroup() %>% 
  filter(year == 2020 & scenario %in% c('1.5C', 'CPol')) %>% 
  select(-year, -investment) %>% 
  bind_rows(master.stim) %>% 
  left_join(gdp.master, by = 'region')  %>% 
  mutate(pct.gdp = avg.invest/gdp*100) %>% 
  mutate(invest.type = ifelse(variable %in% c('Fossil fuels', 'Low carbon sources'), 'Energy', 'Stimulus'))

e.group3 <- c('Fossil fuels', 'Low carbon sources', 'Health', 'Economy', 'General', 'Individuals')

ggplot(gdp.shares %>% filter(region %in% reg2 & variable %in% e.group3 & scenario == '1.5C')) +
  geom_bar(aes(invest.type, pct.gdp, fill = factor(variable, levels = e.group3)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  facet_grid(~region) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12), 
        legend.key.height=unit(1, "cm"), 
        legend.text = element_text(size = 7),
        axis.title.x = element_blank()) +
  labs(y = '% GDP', fill = 'Category', title = 'Energy investments for 1.5 (avg/yr) vs stimulus packages') +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a',
                               'Individuals' = '#9abdd3',
                               'Economy' = '#52a4da',
                               'General' = '#4e8eb4',
                               'Health' = '#dce9f4'))



# Data for Joeri: 

# Stimulus
stim.gdp <- master.stim %>% 
  left_join(gdp.master, by = 'region') %>% 
  mutate(pct.gdp = Stimulus/gdp*100)

write.csv(stim.gdp, 'Joeri_data/stimulus_gdp.csv')

# Energy investment 

total.energy <-invest %>% 
  filter(!year == 2025) %>% 
  group_by(region, variable, scenario, year) %>% 
  mutate(mod.avg = mean(investment)) %>% # model average
  ungroup() %>% 
  select(-investment) %>% 
  filter(model == 'MESSAGEix-GLOBIOM') %>% # not actual filtering but removing duplicates after taking model average above
  group_by(region, variable, scenario) %>% 
  mutate(total.invest = sum(mod.avg)) %>% # sum model averages for the time period
  ungroup() %>% 
  select(-mod.avg) %>%
  filter(year == 2020) %>% 
  select(-model, -year) %>% 
  left_join(gdp.master, by = 'region')  %>% 
  mutate(pct.gdp = total.invest/gdp*100)

write.csv(total.energy, 'Joeri_data/energy_invest.csv')


# Shift in energy invest

write(shift.invest, 'Joeri_data/rel_change_energy.')
