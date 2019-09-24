


library(tidyverse)
library(stringr)


# Process Carbon Stock Data
stocksEco = read_csv("data/ecoregion_stocks_by_scenario_timestep_95ci.csv") %>% 
  filter(Ecosystem=="Yes") %>% 
  mutate(Mean=Mean/1000, Lower=Lower/1000, Upper=Upper/1000) %>% 
  select(-EcoregionID)

stocksEcoTEC = stocksEco %>% 
  group_by(LUC,GCM,RCP,Timestep, EcoregionName, Ecosystem) %>% 
  summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>%
  mutate(StockGroup="TEC")

stocksState = read_csv("data/state_stocks_by_scenario_timestep_95ci.csv") %>% 
  filter(Ecosystem=="Yes") %>% 
  mutate(Mean=Mean/1000, Lower=Lower/1000, Upper=Upper/1000) %>%
  mutate(EcoregionName="State")

stocksStateTEC = stocksState %>% 
  group_by(LUC,GCM,RCP,Timestep, EcoregionName, Ecosystem) %>% 
  summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>%
  mutate(StockGroup="TEC")

stocks = bind_rows(stocksEco, stocksState, stocksEcoTEC, stocksStateTEC) %>%
  mutate(Mean=round(Mean,3), Lower=round(Lower,3), Upper=round(Upper,3)) %>% write_csv("data/stocks.csv")


# Process Net Flux List
fluxEco = read_csv("data/ecoregion_netflux_by_scenario_timestep_95ci.csv") %>% select(-EcoregionID)
fluxState = read_csv("data/state_netflux_by_scenario_timestep_95ci.csv")

netFluxEco = fluxEco %>% filter(Flux %in% c("NPP","Rh","NEP","NECB"))
netFluxState = fluxState %>% filter(Flux %in% c("NPP","Rh","NEP","NECB")) %>% mutate(EcoregionName="State")

netFlux = bind_rows(netFluxEco, netFluxState) %>% write_csv("data/net_flux.csv")
netFlux$Flux = factor(netFlux$Flux, levels=c("NECB","NEP","Rh","NPP"))


# Land Use Emissions
df = read_csv("E:/california-carbon-futures/Data/eco_flows_by_scn_iter_ts.csv")
unique(df$TransitionGroup)
df1 = data.frame(TransitionGroup=c("AgExpand", "Fire", "Insects", "Clearcut", "Thinning", "OrchardRemoval", "Urban"),
                 newName = c("Ag Expansion", "Fire", "Drought", "Forest Clearcut", "Forest Selection", "Orchard Removal", "Urbanization"))

df2 = df %>% left_join(df1, by="TransitionGroup") %>% select(-TransitionGroup) %>% rename("TransitionGroup"="newName") %>%
  group_by(LUC,GCM,RCP,Timestep,EcoregionName, TransitionGroup, Flow) %>%
  summarise(Mean=mean(Amount), Lower=quantile(Amount, 0.025), Upper=quantile(Amount, 0.975))



# Process Transition Data

transEco = read_csv("data/ecoregion_transitions_by_scenario_timestep_95ci.csv") 

# Disturbance Transitions (Fire and Drought)
fire = transEco %>% filter(str_detect(TransitionGroup, "FIRE"), GroupType=="Type") %>%
  separate(TransitionGroup, into=c("TransitionGroup", "StateClass", "Severity", NA, NA), sep=" ") %>%
  mutate(TransitionGroup = str_replace(TransitionGroup, pattern=":", replace="")) %>%
  mutate(TransitionGroup = str_to_title(TransitionGroup))
fire

drought = transEco %>% filter(str_detect(TransitionGroup, "INSECTS"), GroupType=="Type") %>%
  separate(TransitionGroup, into=c("TransitionGroup", "Severity", NA, NA), sep=" ") %>%
  mutate(StateClass = "Forest") %>%
  mutate(TransitionGroup = str_replace(TransitionGroup, pattern=":", replace="")) %>%
  mutate(TransitionGroup = str_to_title(TransitionGroup)) %>% mutate(TransitionGroup = "Drought")
drought

disturbances = bind_rows(fire, drought) %>% select(-GroupType, -EcoregionID)
disturbanceState = disturbances %>% group_by(LUC,GCM,RCP,Timestep,TransitionGroup,StateClass,Severity) %>%
  summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>% mutate(EcoregionName="State")

disturbancesFinal = bind_rows(disturbances, disturbanceState) %>% write_csv("data/disturbances.csv")

# Land Use Transitions
unique(transEco$TransitionGroup)

landuse = transEco %>% filter(str_detect(TransitionGroup, "URBAN") | str_detect(TransitionGroup, "AG"), GroupType=="Group") %>%
  mutate(TransitionGroup = str_to_title(TransitionGroup))


  

transState = read_csv("data/state_transitions_by_scenario_timestep_95ci.csv") %>% filter(TransitionGroup %in% transList) %>% mutate(EcoregionName="State")
transitions = bind_rows(transEco, transState) %>% select(-EcoregionID, -GroupType)
transitionsFire = transitions %>% filter(TransitionGroup %in% transFire)
transitionsFire$TransitionGroup = factor(transitionsFire$TransitionGroup, levels=c("FIRE: High Severity", "FIRE: Medium Severity", "FIRE: Low Severity"))

transitionsDist = transitions %>% filter(TransitionGroup %in% transDist)
unique(transitionsDist$TransitionGroup)