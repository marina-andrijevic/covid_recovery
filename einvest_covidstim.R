
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


# Comparing energy investments from McCollum et al. (2018) and COVID-19-related stimulus packages

# IAM regions from McCollum et al. 
iam.regs <- read_excel('iam_regions.xlsx') %>% 
  mutate(country.code = countrycode(country, 'country.name', 'iso3c')) %>% 
  filter(!country == 'Netherlands Antilles') %>% 
  mutate(country.code = country.code %>% paste %>% str_replace('NA', 'EU'),
         country.code = recode(country.code, 'EUM' = 'NAM')) #change iso3 code for Namibia

# Stimulus packages from Oxford University policy tracker:

eu <- data.frame(CountryName = 'European Union', CountryCode = 'EU', E3_Fiscal.measures = as.numeric(583*10^9)) # add the EU estimates separately (not in the Oxford tracker, but in the IMF one)

stim.reg <- read.csv('/Users/marinaandrijevic/PhD/covid_recovery/covid-policy-tracker/data/OxCGRT_latest.csv') %>% 
  select(CountryName, CountryCode, E3_Fiscal.measures) %>%
  #bind_rows(eu) %>% 
  left_join(iam.regs, by = c('CountryCode' = 'country.code')) %>% 
  group_by(CountryCode) %>% 
  mutate(Stimulus = sum(E3_Fiscal.measures, na.rm = T)) %>% 
  ungroup()%>% 
  filter(!duplicated(CountryCode)) %>% 
  mutate(World = sum(Stimulus, na.rm = T)) %>% 
  group_by(region) %>% 
  mutate(Stimulus = sum(Stimulus, na.rm = T)) %>% 
  ungroup() %>% 
  filter(!duplicated(region)) %>% 
  select(region, Stimulus, World) %>% 
  drop_na(region) %>% 
  spread(region, Stimulus) %>% 
  pivot_longer('World':'REF', names_to = 'region', values_to = 'Stimulus')


# Energy investments data from McCollum et al. 


e.type1  <- c('Energy Efficiency', 'CCS', 'Electricity - T&D and Storage', 'Extraction and Conversion - Nuclear', 
              'Extraction and Conversion - Bioenergy', 'Hydrogen - Non-fossil', 'Electricity - Non-bio Renewables', 
              'Hydrogen - Fossil', 'Electricity - Fossil Fuels w/o CCS', 'Extraction and Conversion - Fossil Fuels') # selected sectors to be aggreagted below

full.invest <- read_excel('NE_paper_SM_data.xlsx', sheet = 'Investment_Annual') %>% 
  rename_all(tolower) %>% 
  mutate(region = recode(region, 'R5ASIA' = 'ASIA', 'R5LAM' = 'LAM', 'R5MAF' = 'MAF', 'R5OECD90+EU' = 'OECD90+EU', 'R5REF' = 'REF')) %>%
  select(model, region, scenario, variable, '2020':'2025') %>% 
  filter(variable %in% e.type1) %>% 
  pivot_longer('2020':'2025', names_to = 'year', values_to = 'investment') %>% 
  mutate(year = as.integer(year), 
         investment = str_remove(investment, "%") %>% as.numeric(investment)) %>% 
  spread(variable, investment) %>% 
  mutate('Fossil electricity &\nhydrogen w/o CCS' = `Electricity - Fossil Fuels w/o CCS` + `Hydrogen - Fossil`,
         'Fossil fuels\nextraction &\nconversion' = `Extraction and Conversion - Fossil Fuels`,
         'Nuclear & CCS' = `Extraction and Conversion - Nuclear` + `CCS`,
         'Renewables' = `Electricity - Non-bio Renewables` + `Hydrogen - Non-fossil` + `Extraction and Conversion - Bioenergy`) %>%
  rename('Electricity - T&D\nand Storage' = 'Electricity - T&D and Storage') %>% 
  gather(variable, investment, -c(model, region, scenario, year)) 

# Derive shifts in investments from current policies to 1.5C-compatible energy sector

shift.invest <- full.invest %>% 
  group_by(model, region, variable, scenario) %>% 
  mutate(total.invest = sum(investment)) %>%  #aggregate investments
  ungroup() %>% 
  select(-investment) %>% 
  spread(scenario, total.invest) %>% 
  filter(year == 2020) %>%  # since the values are aggregated over time, remove duplicates here (not actual filtering)
  mutate(shift = `1.5C` - `CPol`) %>% 
  group_by(region, year, variable) %>% 
  mutate(mod.mean = mean(shift)) %>% #average shift across models
  ungroup() %>% 
  filter(model == 'MESSAGEix-GLOBIOM') # since the values are averaged over time, remove duplicates here (not actual filtering)


# Figure 1: p1) Shifts in energy investments (total for 2020-2025) from current policy to 1.5C; p2) absolute stimulus packages aggregated by IAM regions

e.group2 <- str_wrap(c('Fossil electricity & hydrogen w/o CCS', 'Fossil fuels extraction & conversion', 'Electricity - T&D and Storage', 'Nuclear & CCS', 'Renewables', 'Energy Efficiency' ), 20)

reg1 <- c('ASIA', 'LAM', 'MAF', 'OECD90+EU', 'REF', 'World')

p1 <- ggplot(shift.invest %>% filter(region %in% reg1 & variable %in% e.group2)) +
  geom_bar(aes(region, mod.mean, fill = factor(variable, levels = e.group2)), width = 0.5, stat = 'identity', color = 'black', size = 0.1) +
  labs(x = 'Scenario', y = 'USD billion', fill = '', title = 'Relative change in energy investments:\n1.5C vs. current policy (2020-2025)') + #total investments 2020-2025
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12), 
        legend.key.height=unit(1, "cm"), 
        legend.text = element_text(size = 7),
        axis.title.x = element_blank()) +
  scale_fill_manual(values = c('Fossil electricity &\nhydrogen w/o CCS' = '#a0202f', 
                               'Fossil fuels\nextraction &\nconversion' = '#cf4d3d', 
                               'Electricity - T&D\nand Storage' = '#f4c785', 
                               'Nuclear & CCS' = '#faeeb2', 
                               'Renewables' = '#3c509e',
                               'Energy Efficiency' = '#5095cf'))


stim.col <- brewer.pal(9, "Blues")[3:9] #colors for the stiumulus pacakges

p2 <- ggplot(stim.reg %>% filter(region %in% reg1)) + 
  geom_bar(aes(region, Stimulus/1e9, fill = region), width = 0.6, stat = 'identity') +
  labs(x = 'Country', y = 'USD billion', title = 'Stimulus pacakges\n ') +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, size = 12),
        axis.title.x = element_blank()) +
  #axis.title.y = element_text(size = 12)) +
  scale_y_continuous(breaks = c(0, 2000, 4000, 6000, 8000, 10000)) +
  scale_fill_manual(values = stim.col) 

plot_grid(p1, p2, nrow = 1, rel_widths = c(2/3, 1/3))



# Energy investments and stimuli as shares of GDP

# GDP data (World Bank) aggregated regionally 

gdp.reg <- read.csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2_988718.csv", skip = 3) %>% 
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

# Calculate average yearly investment per sector for the 1.5C pathway (2020-2025)

avg.yr.invest <- full.invest %>% 
  group_by(region, variable, scenario, year) %>% 
  mutate(avg.invest = mean(investment)) %>% # mean across models
  ungroup() %>% 
  filter(model == 'MESSAGEix-GLOBIOM') %>% # removing duplicates after averaging across models 
  select(-model) %>% 
  group_by(region, variable, scenario) %>% 
  mutate(avg.invest = mean(avg.invest)*1e9) %>% # mean across years 
  ungroup() %>% 
  filter(year == 2020) %>% 
  select(-year, -investment) %>% 
  bind_rows(stim.reg %>% mutate(variable = 'COVID-19 Stimulus', scenario = '1.5C') %>% rename(avg.invest = Stimulus)) %>% 
  left_join(gdp.reg, by = 'region') %>% 
  mutate(gdp = gdp) %>% 
  filter(scenario == '1.5C') %>% 
  mutate(pct.gdp = avg.invest/gdp*100) %>% 
  mutate(invest.type = ifelse(variable == 'COVID-19 Stimulus', 'Stimulus', 'Energy'))

e.group3 <- str_wrap(c('Fossil electricity & hydrogen w/o CCS', 
                       'Fossil fuels extraction & conversion', 
                       'Electricity - T&D and Storage', 
                       'Nuclear & CCS', 
                       'Renewables', 
                       'Energy Efficiency',
                       'COVID-19 Stimulus'), 20)

ggplot(avg.yr.invest %>% filter(region %in% reg1 & variable %in% e.group3)) +
  geom_bar(aes(invest.type, pct.gdp, fill = factor(variable, levels = e.group3)), width = 0.5, stat = 'identity', color = 'black', size = 0.1) +
  facet_grid(~region) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12), 
        legend.key.height=unit(1, "cm"), 
        legend.text = element_text(size = 7),
        axis.title.x = element_blank()) +
  labs(y = '% of 2018 GDP', fill = 'Category', title = 'Energy investments for 1.5 (avg/yr) vs stimulus packages') +
  scale_fill_manual(values = c('Fossil electricity &\nhydrogen w/o CCS' = '#a0202f', 
                               'Fossil fuels\nextraction &\nconversion' = '#cf4d3d', 
                               'Electricity - T&D\nand Storage' = '#f4c785', 
                               'Nuclear & CCS' = '#faeeb2', 
                               'Renewables' = '#3c509e',
                               'Energy Efficiency' = '#5095cf',
                               'COVID-19 Stimulus' = '#091263'))


  
