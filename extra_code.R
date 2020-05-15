# Energy investments data from McCollum et al. 


e.type1  <- c('Energy Efficiency', 'CCS', 'Electricity - T&D and Storage', 'Extraction and Conversion - Nuclear', 
              'Extraction and Conversion - Bioenergy', 'Hydrogen - Non-fossil', 'Electricity - Non-bio Renewables', 
              'Hydrogen - Fossil', 'Electricity - Fossil Fuels w/o CCS', 'Extraction and Conversion - Fossil Fuels') # selected sectors to be aggreagted below

e.group2 <- str_wrap(c('Fossil electricity & hydrogen w/o CCS', 'Fossil fuels extraction & conversion', 'Electricity - T&D and Storage', 'Nuclear & CCS', 'Renewables', 'Energy Efficiency' ), 20)


interp <- function(df) {
  with(df, approx(x=index, y=value, xout=min(index):max(index))) %>% 
    bind_cols()
}

invest.prep2 <- read_excel('NE_paper_SM_data.xlsx', sheet = 'Investment_Annual') %>% 
  rename_all(tolower) %>% 
  mutate(region = recode(region, 'R5ASIA' = 'ASIA', 'R5LAM' = 'LAM', 'R5MAF' = 'MAF', 'R5OECD90+EU' = 'OECD+', 'R5REF' = 'REF')) %>%
  select(model, region, scenario, variable, '2015':'2030') %>% 
  filter(variable %in% e.type1) %>% 
  pivot_longer('2015':'2030', names_to = 'year', values_to = 'investment') %>% 
  mutate(year = as.integer(year), 
         investment = str_remove(investment, "%") %>% as.numeric(investment)) 

invest.interp2 <- invest.prep2 %>% 
  group_by(scenario, model, region, variable) %>% 
  rename(index = year, value = investment) %>% 
  do(interp(.)) %>% 
  rename(year = x, investment = y) %>% 
  right_join(invest.prep %>% distinct(region), by = 'region')

invest2 <- invest.interp2%>% 
  spread(variable, investment) %>% 
  mutate('Fossil electricity &\nhydrogen w/o CCS' = `Electricity - Fossil Fuels w/o CCS` + `Hydrogen - Fossil`,
         'Fossil fuels\nextraction &\nconversion' = `Extraction and Conversion - Fossil Fuels`,
         'Nuclear & CCS' = `Extraction and Conversion - Nuclear` + `CCS`,
         'Renewables' = `Electricity - Non-bio Renewables` + `Hydrogen - Non-fossil` + `Extraction and Conversion - Bioenergy`) %>%
  rename('Electricity - T&D\nand Storage' = 'Electricity - T&D and Storage') %>% 
  gather(variable, investment, -c(model, region, scenario, year)) %>% 
  filter(variable %in% e.group2)

shift.invest.yr.2 <- invest2 %>% 
  filter(!year == 2015) %>% 
  group_by(model, region, variable, scenario) %>% 
  mutate(total.invest = mean(investment, na.rm = T)) %>%  #average investments per year
  ungroup()  %>% 
  select(-investment) %>% 
  spread(scenario, total.invest) %>% 
  filter(year == 2020) %>%  # since the values are aggregated over time, remove duplicates here (not actual filtering)
  mutate(shift = `1.5C` - `CPol`) %>% 
  group_by(region, year, variable) %>% 
  mutate(shift.mean.yr = mean(shift),
         cpol.mean.yr = mean(`CPol`)) %>% #average shift across models
  ungroup() %>% 
  filter(model == 'MESSAGEix-GLOBIOM') %>% 
  group_by(region) %>% 
  mutate(net = sum(shift.mean.yr)) %>% # show net increment in the 1.5 shift
  ungroup()

ggplot(shift.invest.yr.2 %>% filter(region == 'World')) + 
  geom_bar(aes(variable, shift.mean.yr, fill = factor(variable, levels = e.group2)), stat = 'identity') + 
  scale_fill_manual(values = c('Fossil electricity &\nhydrogen w/o CCS' = '#a0202f', 
                               'Fossil fuels\nextraction &\nconversion' = '#cf4d3d', 
                               'Electricity - T&D\nand Storage' = '#f4c785', 
                               'Nuclear & CCS' = '#faeeb2', 
                               'Renewables' = '#3c509e',
                               'Energy Efficiency' = '#5095cf'))
