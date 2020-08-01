
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

stim.cntry <- read.csv('data/global_stimulus_july_update.csv', na.strings = c('-', 'NA', ''), header = T, nrows = 177) %>%
  select(country.code,region, total.stim.usd, individuals.households, health, business, deferrals, loans.guarantees, uncategorized.other) %>%
  gather(component, stimulus, -country.code, -region, -total.stim.usd) %>% 
  mutate(stimulus = as.numeric(stimulus),
         stimulus = ifelse(is.na(stimulus), 0, stimulus), # Replace empty cells with zeros
         region = recode(region, 'OECD90+EU' = 'OECD+')) %>% 
  spread(component, stimulus) %>% 
  mutate('Health sector spending' = health, # Aggregate into three categories following the IMF
         'General spending' = individuals.households + business + deferrals + uncategorized.other,
         'Liquidity support' = loans.guarantees) %>% 
  select(country.code, region, 'Health sector spending', 'General spending', 'Liquidity support') %>% 
  gather(component, stimulus, -country.code, -region) %>% 
  drop_na(country.code) 


# Supplementary material table

stim.cntry.long <- read.csv('data/global_stimulus_july_update.csv', na.strings = '-', header = T, nrows = 177) %>%
  select(country.code,region, total.stim.usd, individuals.households, health, business, deferrals, loans.guarantees, uncategorized.other) %>%
  gather(component, stimulus, -country.code, -region, -total.stim.usd) %>% 
  mutate(stimulus = ifelse(is.na(stimulus), 0, stimulus), # Replace empty cells with zeros
         region = recode(region, 'OECD90+EU' = 'OECD+')) %>% 
  spread(component, stimulus) %>% 
  mutate('Health sector spending' = health, # Aggregate into three categories following the IMF
         'General spending' = individuals.households + business + deferrals + uncategorized.other,
         'Liquidity support' = loans.guarantees) %>% 
  select(country.code, region, 'Health sector spending', 'General spending', 'Liquidity support') %>% 
  gather(component, stimulus, -country.code, -region) %>% 
  drop_na(country.code) %>% 
  mutate(stimulus = stimulus/10^9) %>% 
  spread(component, stimulus) %>% 
  mutate(country = countrycode(country.code, 'iso3c', 'country.name')) %>% 
  select(-country.code)

#write.csv(stim.cntry.long, 'data/global_stimulus_long.csv')

# EU member states for aggregation of stimulus packages on the EU level

eu.ms <- c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 
           'IRL', 'ITA', 'LVA', 'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROM', 'SVK', 'SVN', 'ESP', 'SWE', 'EU')

# Aggegregation of stimulus pacakges for four big economies: China, European Union (supranational + member states), India, United States

stim.big.cntry <- stim.cntry %>% 
  mutate(country.code = ifelse(country.code %in% eu.ms, 'EU', as.character(country.code))) %>% 
  group_by(country.code, component) %>% 
  mutate(stimulus = sum(stimulus, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(id = paste0(country.code, region, component)) %>%
  filter(!duplicated(id) & !region == 'EU') %>% 
  filter(country.code %in% reg2)


# Stimulus packages aggregated by IAM regions

stim.reg <- stim.cntry %>% 
  group_by(component) %>% 
  mutate(World = sum(stimulus)) %>% 
  ungroup() %>% 
  mutate(region = recode(region, 'EU' = 'OECD+')) %>% 
  group_by(region, component) %>% 
  mutate(stimulus = sum(stimulus, na.rm = T)) %>% 
  ungroup() %>% 
  select(region, component, stimulus, World) %>% 
  drop_na(region) %>% 
  mutate(id = paste0(region, component)) %>% 
  filter(!duplicated(id)) %>% 
  select(-id) %>% 
  spread(region, stimulus) %>% 
  pivot_longer('World':'REF', names_to = 'region', values_to = 'stimulus')


# Big countries and regions together

master.stim <- stim.big.cntry %>% 
  select(-region, -id) %>% 
  rename(region = country.code) %>% 
  bind_rows(stim.reg) %>% 
  arrange(region)

g20 <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA", "EU", "ARG", "AUS", 
         "BRA", "CHN", "IND", "IDN", "MEX", "RUS", "SAU", "ZAF", "KOR", "TUR")

total.g20 <- stim.big.cntry %>% filter(country.code %in% g20) %>% mutate(total = sum(stimulus))

total.eu <- stim.big.cntry %>% filter(country.code == 'EU') %>% mutate(total = sum(stimulus))
total.oecd <- stim.reg %>% filter(region == 'OECD+') %>% mutate(total = sum(stimulus))

oecd <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "DEU", "GRC", "HUN", "ISL",
            "IRL", "ISR", "ITA", "JPN", "KOR", "LUX", "MEX", "NLD", "NOR", "POL", "PRT", "SVK", "SVN",
            "ESP", "SWE", "CHE", "TRK", "GBR", "USA")


check <- stim.reg %>% filter(region %in% c('LAM', 'MAF', 'REF', 'ASIA'))
sum(check$stimulus)

# Total stimulus for comparison later (not disaggregated)

total.oecd <- stim.cntry %>% filter(country.code %in% oecd) %>% mutate(total = sum(stimulus))

total.1 <- master.stim %>% 
  group_by(region) %>% 
  mutate(total.stim = sum(stimulus)/10^9) %>% 
  select(region, total.stim) %>% 
  filter(!duplicated(region))

# FIGURES: Stimulus packages

# Disaggregated in four categories: health, individuals and households, General spending and general 

stim.col <- brewer.pal(9, "Blues")[3:9] #colors for the stiumulus pacakges

# Fig S1
ggplot(stim.reg %>% filter(region %in% reg1)) + 
  geom_bar(aes(region, stimulus/1e9, fill = component), width = 0.6, stat = 'identity') +
  labs(x = 'Country', y = 'USD billion', fill = '', title = 'Stimulus packages') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(values = stim.col) 

# Fig S2
ggplot(stim.big.cntry %>% filter(country.code %in% reg2)) + 
  geom_bar(aes(country.code, stimulus/1e9, fill = component), width = 0.6, stat = 'identity') +
  labs(x = 'Country', y = 'USD billion', fill = '', title = 'Stimulus packages') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(values = stim.col)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # #  Energy system investments
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Energy investments data from McCollum et al. 

e.type1  <- c('Energy Efficiency', 'CCS', 'Electricity - T&D and Storage', 'Extraction and Conversion - Nuclear', 
              'Extraction and Conversion - Bioenergy', 'Hydrogen - Non-fossil', 'Electricity - Non-bio Renewables', 
              'Hydrogen - Fossil', 'Electricity - Fossil Fuels w/o CCS', 'Extraction and Conversion - Fossil Fuels') # selected sectors to be aggreagted below

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

e.group2 <- c('Fossil fuels', 'Low carbon sources') # for aggregation to only two broad groups of energy investments

invest <- invest.interp %>% 
  spread(variable, investment) %>% 
  mutate('Fossil fuels' = `Electricity - Fossil Fuels w/o CCS` + `Hydrogen - Fossil` + `Extraction and Conversion - Fossil Fuels`,
         'Low carbon sources' = `Extraction and Conversion - Nuclear` + `CCS` + `Electricity - Non-bio Renewables` + `Hydrogen - Non-fossil` 
         + `Extraction and Conversion - Bioenergy` + `Electricity - T&D and Storage` + `Energy Efficiency`) %>% 
  gather(variable, investment, -c(model, region, scenario, year)) %>% 
  filter(variable %in% e.group2) %>% 
  mutate(investment = investment * 1.0728) #adjust for inflation to current dollars

image <- invest %>% filter(model == 'IMAGE')

# Derive shifts in investments from current policies (CPol) to 1.5C-compatible energy sector

# For 2020-2025
shift.invest <- invest %>% 
  filter(!year == 2025) %>% 
  group_by(region, variable, scenario, year) %>% 
  mutate(mod.avg = mean(investment)) %>% # model average
  ungroup() %>% 
  select(-investment) %>% 
  filter(model == 'MESSAGEix-GLOBIOM') %>% # not actual filtering but removing duplicates after taking model average above
  group_by(region, variable, scenario) %>% 
  mutate(yr.invest = mean(mod.avg)) %>% # yearly average
  ungroup() %>% 
  select(-mod.avg) %>% 
  spread(scenario, yr.invest) %>% 
  filter(year == 2020) %>%  # since the values are aggregated over time, remove duplicates here 
  mutate(shift = `1.5C` - `CPol`) %>% # calculate the difference between scenarios per sector
  group_by(region) %>% 
  mutate(net = sum(shift)) %>% # show net investments for the 1.5C shift
  ungroup() %>% 
  select(-`NDC`, -`2C`, -model)

# Average annual 

annual <- invest %>%
  filter(!year == 2025) %>%
  group_by(region, variable, scenario, year) %>%
  mutate(mod.avg = mean(investment),
         mod.max = max(investment), 
         mod.min = min(investment)) %>% # model average
  ungroup() %>%
  filter(model == 'MESSAGEix-GLOBIOM') %>% # not actual filtering but removing duplicates after taking model average above
  filter(scenario %in% c('1.5C', 'CPol')) %>%
  group_by(region, variable, scenario) %>%
  mutate(annual = mean(mod.avg),
         annual.max = mean(mod.max),
         annual.min = mean(mod.min)) %>%
  ungroup() %>%
  filter(year == '2020') %>%
  select(scenario, region, variable, annual, annual.max, annual.min)

# Total investment over five years

total.invest <- invest %>%
  filter(!year == 2025) %>%
  group_by(region, variable, scenario, year) %>%
  mutate(mod.avg = mean(investment)) %>% # model average
  ungroup() %>%
  filter(model == 'MESSAGEix-GLOBIOM') %>% # not actual filtering but removing duplicates after taking model average above
  filter(scenario %in% c('1.5C', 'CPol')) %>%
  group_by(region, variable, scenario) %>%
  mutate(total = sum(mod.avg)) %>%
  ungroup() %>%
  filter(year == '2020') %>%
  select(scenario, region, variable, total)

# For exporting a table comparing investments and stimulus 

invest.comp <- annual %>%
  left_join(total.invest, by = c('region', 'scenario', 'variable')) %>%
  left_join(total.1, by = c('region')) %>% 
  left_join(gdp.master, by = c('region')) %>%
  rename(five.yr = total, 
         stimulus = total.stim) %>% 
  gather(tempo, energy.invest, -scenario, -region, -variable, -stimulus, -gdp) %>% 
  spread(scenario, energy.invest) %>% 
  mutate(shift = `1.5C` - `CPol`) %>% 
  group_by(region, tempo) %>% 
  mutate(net = sum(shift)) %>% 
  ungroup()

# write.csv(invest.comp, 'stim_invest_shift_comp.csv')

# # # # # # # # # # # # # # # # # # # #
#   Supplementary Material Figures    #
# # # # # # # # # # # # # # # # # # # #

# Energy investments

# Total energy investments in current policy baseline and 1.5C scenario

# For IAM regions (Fig S3)

p1 <- ggplot(shift.invest %>% filter(region %in% reg1 & variable %in% e.group2)) +
  geom_bar(aes(region, `CPol`, fill = str_wrap(factor(variable, levels = e.group2), 10)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  labs(x = 'Scenario', y = 'USD billion per year', fill = '', title = 'Annual investment (2020-2024)\nCurrent policy') + #total investments 2020-2025
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 12), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 13)) +
  scale_fill_manual(values = c('Fossil\nfuels' = '#f5a788', 
                               'Low carbon\nsources' = '#acb96a')) +
  scale_y_continuous(limits = c(0, 2500), breaks = c(seq(0, 2000, 1000)))


p2 <- ggplot(shift.invest %>% filter(region %in% reg1 & variable %in% e.group2)) +
  geom_bar(aes(region, `1.5C`, fill = str_wrap(factor(variable, levels = e.group2), 10)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  labs(x = 'Scenario', y = 'USD billion per year', fill = '', title = '\n1.5°C pathway') + #total investments 2020-2025
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12), 
        legend.text = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.key = element_rect(size = 2, fill = "white", colour = "white"),
        legend.key.size = unit(0.5, "cm"),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 13)) +
  scale_fill_manual(values = c('Fossil\nfuels' = '#f5a788', 
                               'Low carbon\nsources' = '#acb96a')) +
  scale_y_continuous(limits = c(0, 2500), breaks = c(seq(0, 2000, 1000)))

plot_grid(p1, p2, nrow = 1, rel_widths = c(2/5, 3/5))

# For four big economies (Fig S4)

p3 <- ggplot(shift.invest %>% filter(region %in% reg2 & variable %in% e.group2)) +
  geom_bar(aes(region, `CPol`, fill = str_wrap(factor(variable, levels = e.group2), 10)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  labs(x = 'Scenario', y = 'USD billion per year', fill = '', title = 'Annual investments (2020-2024)\nCurrent policy') + #total investments 2020-2025
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 12), 
        axis.text.y = element_text(size = 12),
        legend.key.height=unit(1, "cm"), 
        legend.text = element_text(size = 12),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 13)) +
  scale_fill_manual(values = c('Fossil\nfuels' = '#f5a788', 
                               'Low carbon\nsources' = '#acb96a')) +
  scale_y_continuous(limits = c(0, 500), breaks = c(0, 500, 250))


p4 <- ggplot(shift.invest %>% filter(region %in% reg2 & variable %in% e.group2)) +
  geom_bar(aes(region, `1.5C`, fill = str_wrap(factor(variable, levels = e.group2), 10)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  labs(x = 'Scenario', y = 'USD billion per year', fill = '', title = '\n1.5°C pathway') + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12),
        axis.text.y = element_text(size = 12),
        legend.key.height=unit(1, "cm"), 
        legend.text = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.key = element_rect(size = 2, fill = "white", colour = "white"),
        legend.key.size = unit(0.5, "cm")) +
  scale_fill_manual(values = c('Fossil\nfuels' = '#f5a788', 
                               'Low carbon\nsources' = '#acb96a')) +
  scale_y_continuous(limits = c(0, 500), breaks = c(0, 500, 250))

plot_grid(p3, p4, nrow = 1, rel_widths = c(2/5, 3/5))


# Shifts in energy investments vs stimulus packages

p5 <- ggplot(shift.invest %>% filter(region %in% reg1 & variable %in% e.group2)) +
     geom_bar(aes(region, shift, fill = str_wrap(factor(variable, levels = e.group2), 10)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
     labs(x = 'Scenario', y = 'USD billion per year', fill = '', title = 'Annual shift in energy investments:\n1.5°C vs. current policy (2020-2024)') + #total investments 2020-2025
     theme_bw() +
     theme(axis.text.x = element_text(angle = 90, size = 12), 
          legend.text = element_text(size = 7),
           legend.position = 'none',
           axis.title.x = element_blank(),
           axis.text.y = element_text(size = 12)) +
     scale_fill_manual(values = c('Fossil\nfuels' = '#f5a788', 
                                  'Low carbon\nsources' = '#acb96a')) + 
     geom_point(aes(region, net), shape=95, size=15, alpha = 0.40, color = 'black') 


p6 <- ggplot(shift.invest %>% filter(region %in% reg2 & variable %in% e.group2)) +
     geom_bar(aes(region, shift, fill = str_wrap(factor(variable, levels = e.group2), 10)), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
     labs(x = 'Scenario', y = 'USD billion per year', fill = '', title = '\n') + #total investments 2020-2025
     theme_bw() +
     theme(axis.text.x = element_text(angle = 90, size = 12), 
           #legend.text = element_text(size = 7),
            axis.title.x = element_blank()) +
     scale_fill_manual(values = c('Fossil\nfuels' = '#f5a788', 
                                   'Low carbon\nsources' = '#acb96a')) + 
     geom_point(aes(region, net), shape=95, size=15, alpha = 0.40, color = 'black') 

plot_grid(p5, p6, nrow = 1)


# Energy investments and stimulus packages relative to GDP

# GDP data (World Bank) aggregated regionally 

gdp.reg <- read.csv("data/gdp_2019_wb.csv", skip = 3) %>% 
  rename_all(tolower) %>% 
  left_join(iam.regs, by = 'country.code') %>% 
  filter(!region == 'EU' & !duplicated(country.code)) %>% 
  pivot_longer(x1960:x2019, names_to = "year", values_to = "gdp") %>% 
  mutate(year = year %>% str_replace("x", "")) %>% 
  filter(year == 2019) %>% 
  select(region, gdp) %>% 
  group_by(region) %>% 
  mutate(gdp = sum(gdp, na.rm = T)) %>% 
  ungroup() %>% 
  filter(!duplicated(region)) %>% 
  mutate(World = sum(gdp, na.rm = T)) %>% 
  spread(region, gdp) %>% 
  gather(region, gdp) %>% 
  as.data.frame()

gdp.cntry <- read.csv("data/gdp_2019_wb.csv", skip = 3) %>%
  rename_all(tolower) %>% 
  pivot_longer(x1960:x2019, names_to = "year", values_to = "gdp") %>% 
  mutate(year = year %>% str_replace("x", "")) %>% 
  filter(year == 2019) %>%
  mutate(EU = 15.5e12) %>% 
  filter(country.code %in% c('USA', 'CHN', 'IND')) %>% 
  select(country.code, gdp, EU) %>% 
  spread(country.code, gdp) %>% 
  gather(region, gdp)

gdp.master <- gdp.reg %>% 
  bind_rows(gdp.cntry)

# Calculate average yearly investment per sector for the 1.5C pathway (2020-2025)

scenario <- data.frame(scenario = c('1.5C', 'CPol'))

stim.prep <- master.stim %>% 
  left_join(gdp.master, by = 'region') %>% 
  mutate(pct.gdp = stimulus/gdp*100, 
         invest.type = 'Stimulus') %>% 
  rename(variable = component, 
         avg.invest = stimulus) %>% 
  merge(scenario)

# shift.prep <- shift.invest %>% 
#   select(region, variable, shift) %>% 
#   rename(avg.invest = shift) %>% 
#   left_join(gdp.master, by = 'region') %>% 
#   mutate(pct.gdp = (avg.invest*10^9)/gdp*100) %>% 
#   mutate(invest.type = 'Investment shift') %>% 
#   merge(scenario)


e.group3 <- c('Low carbon sources', 'Fossil fuels', 'General spending', 'Health sector spending', 'Liquidity support')

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
  left_join(gdp.master, by = 'region')  %>% 
  mutate(pct.gdp = avg.invest/gdp*100, 
         invest.type = 'Energy investment') %>% 
  bind_rows(stim.prep) %>% 
  bind_rows(shift.prep) %>% 
  mutate(variable = factor(variable, levels = e.group3)) %>% 
  filter(scenario == '1.5C')

p10 <- ggplot(gdp.shares %>% filter(region %in% reg2 & scenario == '1.5C')) +
  geom_bar(aes(invest.type, pct.gdp, fill = variable), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  facet_wrap(~region, scales = 'free') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title.x = element_blank(), 
        legend.position = "bottom") +
  labs(y = '% GDP', fill = '') +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a',
                               'General spending' = '#c6dbef',
                               'Health sector spending' = '#9ecae1',
                               'Liquidity support' = '#6baed6')) +
  guides(fill = guide_legend(nrow = 1))

legend <- cowplot::get_legend(p10)

p11 <- ggplot(gdp.shares %>% filter(region %in% reg1 & scenario == '1.5C')) +
  geom_bar(aes(invest.type, pct.gdp, fill = variable), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  facet_grid(~region) +
  theme_bw() +
  theme(legend.text = element_text(size = 7),
        axis.title.x = element_blank(),
        legend.position="none",
        axis.text.x = element_text(size = 9, angle = 90)) +
  labs(y = '% GDP') +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a',
                               'General spending' = '#c6dbef',
                               'Health sector spending' = '#9ecae1',
                               'Liquidity support' = '#6baed6'))

p12 <- ggplot(gdp.shares %>% filter(region %in% reg2 & scenario == '1.5C')) +
  geom_bar(aes(invest.type, pct.gdp, fill = variable), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  facet_grid(~region) +
  theme_bw() +
  theme(legend.text = element_text(size = 7),
        axis.title.x = element_blank(),
        legend.position="none",
        axis.text.x = element_text(size = 9, angle = 90)) +
  labs(y = '% GDP') +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a',
                               'General spending' = '#c6dbef',
                               'Health sector spending' = '#9ecae1',
                               'Liquidity support' = '#6baed6'))

p13 <- ggplot(gdp.shares %>% filter(region %in% reg1 & scenario == '1.5C')) +
  geom_bar(aes(invest.type, avg.invest/10^9, fill = variable), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  facet_grid(~region) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, angle = 90),
        legend.text = element_text(size = 7),
        axis.title.x = element_blank(),
        legend.position="none") +
  labs(y = 'USD billion', fill = 'Category') +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a',
                               'General spending' = '#c6dbef',
                               'Health sector spending' = '#9ecae1',
                               'Liquidity support' = '#6baed6'))

p14 <- ggplot(gdp.shares %>% filter(region %in% reg2 & scenario == '1.5C')) +
  geom_bar(aes(invest.type, avg.invest/10^9, fill = variable), width = 0.5, stat = 'identity', color = 'white', size = 0.1) +
  facet_grid(~region) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, angle = 90),
        legend.text = element_text(size = 7),
        axis.title.x = element_blank(),
        legend.position="none") +
  labs(y = 'USD billion', fill = 'Category') +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a',
                               'General spending' = '#c6dbef',
                               'Health sector spending' = '#9ecae1',
                               'Liquidity support' = '#6baed6'))

panels1 <- plot_grid(p11, p12)
panels2 <- plot_grid(p13, p14)

title <- ggdraw() + 
  draw_label("      Energy investments compatible with 1.5°C (avg/yr) vs stimulus packages",
    fontface = 'bold',
    x = 0,
    hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

plot_grid(title, panels1, legend, ncol = 1, rel_heights = c(0.1, 1, 0.12))
plot_grid(title, panels2, legend, ncol = 1, rel_heights = c(0.1, 1, 0.12))


# Figures for Joeri

shift.prep <- shift.invest %>% 
  select(regin, variable, shift) %>% 



# Data for Joeri: 

# Stimulus
stim.gdp <- master.stim %>% 
  left_join(gdp.master, by = 'region') %>% 
  mutate(pct.gdp = stimulus/gdp*100)

View(stim.gdp)
write.csv(stim.gdp, 'Joeri_data/stimulus_gdp.csv')

# Energy investment 

total.energy <- invest %>% 
  filter(!year == 2025) %>% 
  group_by(region, variable, scenario, year) %>% 
  mutate(mod.avg = mean(investment)) %>% # model average
  ungroup() %>% 
  select(-investment) %>% 
  filter(model == 'MESSAGEix-GLOBIOM') %>% # not actual filtering but removing duplicates after taking model average above
  group_by(region, variable, scenario) %>% 
  mutate(total.invest = mean(mod.avg)*10^9) %>% # sum model averages for the time period
  ungroup() %>% 
  select(-mod.avg) %>%
  filter(year == 2020) %>% 
  select(-model, -year) %>% 
  left_join(gdp.master, by = 'region')  %>% 
  mutate(pct.gdp = total.invest/gdp*100)

View(total.energy)
write.csv(total.energy, 'Joeri_data/energy_invest.csv')


# Shift in energy invest

write.csv(shift.invest, 'Joeri_data/rel_change_energy_annual.csv')


# IMF Regions

imf.reg <- read_excel('data/imf_regions.xlsx') %>% 
  mutate(country.code = countrycode(country, 'country.name', 'iso3c'))

gdp.imf.reg <- read.csv("data/gdp_2018_wb.csv", skip = 3) %>%
  rename_all(tolower) %>% 
  pivot_longer(x1960:x2019, names_to = "year", values_to = "gdp") %>% 
  mutate(year = year %>% str_replace("x", "")) %>% 
  filter(year == 2018)

stim.imf <- stim.cntry %>% 
  group_by(country.code) %>% 
  mutate(stimulus = sum(stimulus, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(id = paste0(country.code)) %>%
  filter(!duplicated(id)) %>% 
  left_join(gdp.imf.reg, by = 'country.code')  %>% 
  mutate(pct.gdp = stimulus/gdp*100) %>% 
  left_join(imf.reg, by = 'country.code') %>%
  group_by(region.y) %>% 
  mutate(avg.stinm.gdp = mean(pct.gdp, na.rm = T)) %>% 
  ungroup() %>% 
  filter(!duplicated(region.y))




# Model spread for energy investments

# For total energy investments
model.spread.total <- invest %>% 
  filter(!year == 2025) %>% 
  group_by(region, variable, model, scenario) %>% 
  mutate(annual.avg = mean(investment)) %>% # yearly average
  ungroup()  %>% 
  filter(year == 2020)


p91 <- ggplot(model.spread.total %>% filter(region %in% reg1 & scenario == '1.5C'), aes(region, annual.avg, fill = variable)) +
  stat_summary(geom = 'bar', fun = mean, position = 'dodge') +
  stat_summary(geom = 'errorbar', fun.min = min, fun.max = max, position = 'dodge') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, size = 12), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 13)) +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a')) + 
  labs(title = 'Energy investments: 1.5°C pathway')



p92 <- ggplot(model.spread.total %>% filter(region %in% reg1 & scenario == 'CPol'), aes(region, annual.avg, fill = variable)) +
  stat_summary(geom = 'bar', fun = mean, position = 'dodge') +
  stat_summary(geom = 'errorbar', fun.min = min, fun.max = max, position = 'dodge') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 13)) +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a')) +
  labs(title = 'Current policy')


gridExtra::grid.arrange(p1, p2, nrow = 1, widths = c(3/7, 4/7))

# For shifts in investments between current policy and 1.5 pathway

model.spread.shift <- invest %>% 
  filter(!year == 2050) %>% 
  group_by(region, variable, model, scenario) %>% 
  mutate(annual.avg = mean(investment)) %>% # yearly average
  ungroup()  %>% 
  select(-investment) %>% 
  filter(year == 2020) %>% 
  spread(scenario, annual.avg) %>% 
  mutate(shift = `1.5C` - `CPol`) %>% 
  group_by(region, variable) %>% 
  mutate(mean.shift = mean(shift),
         max.shift = max(shift), 
         min.shift = min(shift)) %>% 
  ungroup()

ggplot(model.spread.shift %>% filter(region %in% reg1), aes(region, shift, fill = variable)) +
  stat_summary(geom = 'bar', fun = mean, position = 'dodge') +
  stat_summary(geom = 'errorbar', fun.min = min, fun.max = max, position = 'dodge') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 12), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 13)) +
  scale_fill_manual(values = c('Fossil fuels' = '#f5a788', 
                               'Low carbon sources' = '#acb96a')) +
  labs(title = 'Shifts in annual energy investments between current policy and 1.5°C pathway')





