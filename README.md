# Code and data for reproducing the analysis of "COVID-19 recovery stimulus dwarfs near-term climate change investment needs" (Andrijevic et al., 2020)

## Code
All figures generated for the Supplementary Information and the underlying data processing can be found in the R script "dataprocess_figures.R".

## Data
All data required to replicate this analysis and run the R script above are placed in the folder "data".

## Materials and Methods
### Energy system investments

Energy system investment portfolios (1) were derived from six global energy-economy, or ‘integrated assessment’, models (IAMs): AIM/CGE (2), IMAGE (3), MESSAGEix-GLOBIOM (4), POLES (5),REMIND-MAgPIE (6), and WITCH-GLOBIOM (7). In this analysis we focus on the near-term (2020-2024) investment needs under a current policies scenario (i.e., each model’s baseline) and on the upscaling requirements for moving toward an energy system compatible with the 1.5C target of the Paris Agreement. The models cover different types of energy technologies, including resource extraction, power generation, fuel conversion, transmission, energy storage and end-use demand services. We group these technologies into two broad sectors for our investments analysis:
•	fossil fuels: extraction and conversion of fossil fuels, electricity from fossil fuels without Carbon Capture and Storage (CCS) technologies and hydrogen from fossil fuels
•	low carbon sources: extraction and conversion of nuclear energy, CCS, electricity from non-bio renewables, hydrogen from non-fossil fuels, extraction and conversion of bioenergy, electricity transmission and distribution and storage, and energy efficiency

In this analysis, for purposes of quantifying highly uncertain ‘demand-side’, or ‘energy efficiency-increasing’ investments, we inherit the methodology of McCollum et al. (2018), the original source for all of our energy investment numbers. That study calculated demand-side energy efficiency investments across the end-use sectors (buildings, transport, industry) in a harmonized way for each of the global models. The calculations take into account two separate, additive components: (1) base-year energy efficiency and (2) supply-side offset. The base-year component is calculated by taking the level of energy efficiency investments estimated globally by the International Energy Agency in 2015 and then scaling those efficiency investments with total final energy demand in the models’ scenarios going forward. The supply-side offset component compares total final energy demand for a given model in a policy scenario (e.g., 1.5C) to that model’s demand in the reference case, and then assumes that, in equilibrium, the investments made to reduce energy demand can be equated to investments that are simultaneously being offset on the supply side. Such an approximation, which is a first-order approximation to be sure, implicitly includes both technological (e.g., air conditioner efficiency) and systems (e.g., urban land use and transit) changes that could allow for less energy-intensive lifestyles than today. Based on this methodology, however, investments into individual demand-side measures cannot be tracked explicitly.

All investment estimates from McCollum et al. (2018) were corrected for inflation (from USD 2015 to USD 2019) and are reported here as model averages.


### Strengths and limitations of energy investment modelling estimates

Estimating current and future energy investment flows is not an exact science. Private and publicly-traded companies, governments, stated-owned enterprises, and households are not required to report such information to statistical gathering organizations in all cases. For this reason, the numbers must be back-calculated based on physical quantities, such as gigawatts of installed power plant capacity. Energy efficiency investments are the most uncertain, largely owing to definitional issues (what exactly is the energy-related part of a demand-side device, such as a consumer appliance?). Organizations like the International Energy Agency have adopted a certain set methodologies; global integrated assessment models do similar. Importantly, base-year uncertainties can contribute to differences in future year projections, especially further out in time (e.g., 2050). Fortunately, for this analysis, which focuses on the near-term period of 2020-2024 (i.e., a single model time-step for all but one of the models relied upon here), the differences across models are relatively small. Moreover, despite the lack of temporal granularity of these models (i.e., no annual or sub-annual timesteps), one of their acknowledged strengths is that they permit the analysis of near-term system responses in the context of long-term goals. This is as much true for fuel-technology mixes and greenhouse gas emissions as it is for investments (in dollar terms), even if the latter is less often the focus of IAM research.


## Stimulus packages

The International Monetary Fund (IMF) has been tracking the policy measures announced by governments in response to the COVID-19 pandemic (8). For this analysis, we focus on the fiscal policy responses, which span a wide range of instruments such as spending and revenue measures, equity injections, asset purchases, extra-budgetary funds, guarantees on loans, etc. We extracted data for 176 countries and the European Union, announced until July 23, 2020.

Countries announced their stabilization packages in different levels of detail and scope. Here we group the fiscal measures broadly in line with the IMF. The packages are split into:
•	“Above-the-line” measures which contain mostly spending measures, and which are further divided into those aimed at supporting the health sector (in figures labeled as Health sector spending) and those intended for all other sectors of the economy (in figures labeled General spending), which include supporting individuals, households and businesses, as well as forgone and deferred revenue.
•	Liquidity measures, which including loans, guarantees and quasi-fiscal operations (labeled Liquidity support).

We do not account for governments' announcements to channel funds into international assistance, nor the recovery funds agreed between governments and the international finance institutions. See Table S1 for country-level detail.


### References:

1. 	D. L. McCollum, W. Zhou, C. Bertram, H. S. De Boer, V. Bosetti, S. Busch, J. Després, L. Drouet, J. Emmerling, M. Fay, O. Fricko, S. Fujimori, M. Gidden, M. Harmsen, D. Huppmann, G. Iyer, V. Krey, E. Kriegler, C. Nicolas, S. Pachauri, S. Parkinson, M. Poblete-Cazenave, P. Rafaj, N. Rao, J. Rozenberg, A. Schmitz, W. Schoepp, D. Van Vuuren, K. Riahi, Energy investment needs for fulfilling the Paris Agreement and achieving the Sustainable Development Goals. Nat. Energy. 3, 589–599 (2018).
2. 	S. Fujimori, T. Hasegawa, T. Masui, K. Takahashi, Land use representation in a global CGE model for long-term simulation: CET vs. logit functions. Food Secur. 6, 685–699 (2014).
3. 	E. Stehfest, D. van Vuuren, T. Kram, L. Bouwman, R. Alkemade, M. Bakkenes, H. Biemans, A. Bouwman, M. den Elzen, J. Janse, P. Lucas, J. van Minnen, C. Müller, A. G. Prins, “Integrated Assessmnet of Global Environmental Change with IMAGE 3.0. Model Description and Policy Applications” (2014).
4. 	V. Krey, P. Havlik, O. Fricko, J. Zilliacus, M. Gidden, M. Strubegger, G. Kartasasmita, T. Ermolieva, N. Forsell, M. Gusti, N. Johnson, G. Kindermann, P. Kolp, D. McCollum, S. Pachauri, S. Rao, J. Rogelj, H. Valin, M. Obersteiner, K. Riahi, “MESSAGE-GLOBIOM 1.0 Documentation” (2016).
5. 	P. Criqui, S. Mima, P. Menanteau, A. Kitous, Mitigation strategies and energy technology learning: An assessment with the POLES model. Technol. Forecast. Soc. Change. 90, 119–136 (2015).
6. 	E. Kriegler, N. Bauer, A. Popp, F. Humpenöder, M. Leimbach, J. Strefler, L. Baumstark, B. L. Bodirsky, J. Hilaire, D. Klein, I. Mouratiadou, I. Weindl, C. Bertram, J. P. Dietrich, G. Luderer, M. Pehl, R. Pietzcker, F. Piontek, H. Lotze-Campen, A. Biewald, M. Bonsch, A. Giannousakis, U. Kreidenweis, C. Müller, S. Rolinski, A. Schultes, J. Schwanitz, M. Stevanovic, K. Calvin, J. Emmerling, S. Fujimori, O. Edenhofer, Fossil-fueled development (SSP5): An energy and resource intensive scenario for the 21st century. Glob. Environ. Chang. 42, 297–315 (2017).
7. 	V. Bosetti, C. Carraro, M. Galeotti, E. Massetti, M. Tavoni, “WITCH. A World Induced Technical Change Hybrid Model” (2006), (available at https://econpapers.repec.org/RePEc:ven:wpaper:2006_46).
8. 	International Monetary Fund, Policy Responses to COVID-19 (2020).


### Energy system investments

Energy system investment portfolios [REF McCollum et al. 2018] were derived from six global energy-economy models (IAMs): AIM/CGE [REF Fujimori et al. 2014, Fujimori et al. 2012], IMAGE [REF Stehfest et al. 2014], MESSAGEix-GLOBIOM [REF Fricko et al. 2017, Krey et al. 2016], POLES [REF Criqui et al. 2015], REMIND-MAgPIE [REF Kriegler et al. 2017, Luderer et al. 2013] and WITCH-GLOBIOM [REF Bosetti et al. 2006, Emmerling et al. 2016]. We focus on investments under the current policies (used as each model’s baseline) and on upscaling requirements necessary for the energy system compatible with the 1.5°C target of the Paris Agreement. The models cover different types of energy technologies, including resource extraction, power generation, fuel conversion, transmission, energy storage and end-use demand services. We group these technologies into two broad sectors: fossil fuels (extraction and conversion of fossil fuels, electricity from fossil fuels without Carbon Capture and Storage (CCS) technologies and hydrogen from fossil fuels) and low carbon sources (extraction and conversion of nuclear energy, CCS, electricity from non-bio renewables, hydrogen from non-fossil fuels, extraction and conversion of bioenergy, electricity transmission and distribution and storage, and energy efficiency). The investment figures are corrected for inflation (from USD 2015 to current USD) [REF World Bank].

Our analysis focuses on the near term investment needs from 2020 to 2024, taking the model average for the underlying yearly data.

Figures xxx show the total investment needs over the 5-year time period.

Figures xxx show the change in the investment needs over the 5-year time period, from the current policy baseline to the 1.5°C-compatible pathway.

Figures xxx show the average yearly investment relative to the 2018 Gross Domestic Product (GDP) [REF World Bank]. 


### Stimulus packages

International Monetary Fund (IMF) has been tracking the policy measures announced by governments in response to the COVID-19 pandemic [REF]. For this analysis, we focus on the fiscal policy responses, which span a wide range of instruments such as spending and revenue measures (primarily in the health sector, and different types of liquidity support for affected businesses and workers), equity injections, asset purchases, extra-budgetary funds, guarantees on loans, etc. We extracted data for 175 countries and the European Union, announced up to May 15, 2020. 

Countries announced their stabilization packages in different levels of detail and scope. Here we group the fiscal measures broadly into those targeted at the health sector, those aimed at supporting individuals and households, those that aim at the economy at large (including loans and guarantees), and the remainder we pool into a category of general measures. Portions of fiscal measures allocated to the *general* category are either unspecified or do not belong to one of the categories above.
We do not account for governments' announcements to channel funds into international assistance, nor the recovery funds agreed between governments and the international finance institutions.
 


### Regional definitions

**OECD+:** Includes the OECD 1990 countries as well as EU members and candidates
Albania, Australia, Austria, Belgium, Bosnia and Herzegovina, Bulgaria, Canada, Croatia, Cyprus, Czech Republic, Denmark, Estonia, Fiji, Finland, France, French Polynesia,
Germany, Greece, Guam, Hungary, Iceland, Ireland, Italy, Japan, Latvia, Lithuania,
Luxembourg, Malta, Macedonia, Montenegro, Netherlands, New Caledonia, New Zealand,
Norway, Poland, Portugal, Romania, Samoa, Serbia, Slovakia, Slovenia, Solomon Islands,
Spain, Sweden, Switzerland, Turkey, United Kingdom, United States of America, Vanuatu

**REF:** Countries from the Reforming Ecomonies of the Former Soviet Union.
Armenia, Azerbaijan, Belarus, Georgia, Kazakhstan, Kyrgyzstan, Republic of Moldova,
Russian Federation, Tajikistan, Turkmenistan, Ukraine, Uzbekistan

**ASIA:** The region includes most Asian countries with the exception of the Middle East,
Japan and Former Soviet Union states.
Afghanistan, Bangladesh, Bhutan, Brunei Darussalam, Cambodia, China, China Hong Kong
SAR, China Macao SAR, Democratic People's Republic of Korea, East Timor, India,
Indonesia, Lao People's Democratic Republic, Malaysia, Maldives, Mongolia, Myanmar,
Nepal, Pakistan, Papua New Guinea, Philippines, Republic of Korea, Singapore, Sri Lanka,
Taiwan, Thailand, Viet Nam

**MAF:** This region includes the countries of the Middle East and Africa.
Algeria, Angola, Bahrain, Benin, Botswana, Burkina Faso, Burundi, Cameroon, Cape Verde,
Central African Republic, Chad, Comoros, Congo, Cote d'Ivoire, Democratic Republic of
the Congo, Djibouti, Egypt, Equatorial Guinea, Eritrea, Ethiopia, Eswatini, Gabon, Gambia, Ghana, Guinea, Guinea-Bissau, Iran (Islamic Republic of), Iraq, Israel, Jordan, Kenya, Kuwait,
Lebanon, Lesotho, Liberia, Libyan Arab Jamahiriya, Madagascar, Malawi, Mali, Mauritania,
Mauritius, Morocco, Mozambique, Namibia, Niger, Nigeria, Oman, Qatar, Reunion,
Rwanda, Saudi Arabia, Senegal, Sierra Leone, Somalia, South Africa, Sudan, Syrian Arab Republic, Togo, Tunisia, Uganda, United Arab Emirates, United Republic of
Tanzania, Western Sahara, Yemen, Zambia, Zimbabwe

**LAM:**  This region includes the countries of Latin America and the Caribbean.
Argentina, Bahamas, Barbados, Belize, Bolivia, Brazil, Chile, Colombia, Costa Rica, Cuba,
Dominican Republic, Ecuador, El Salvador, Guadeloupe, Guatemala, Guyana, Haiti, Honduras, Jamaica, Martinique, Mexico, Netherlands Antilles, Nicaragua, Panama, Paraguay,
Peru, Puerto Rico, Suriname, Trinidad and Tobago, Uruguay, Venezuela

**Four individual economies:** China, European Union, India, United States

