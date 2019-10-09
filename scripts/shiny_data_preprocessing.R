


library(tidyverse)
library(stringr)


# Process Carbon Stock Data #####
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


# Process Net Flux List #####
fluxEco = read_csv("data/ecoregion_netflux_by_scenario_timestep_95ci.csv") %>% select(-EcoregionID)
fluxState = read_csv("data/state_netflux_by_scenario_timestep_95ci.csv")

netFluxEco = fluxEco %>% filter(Flux %in% c("NPP","Rh","NEP","NECB"))
netFluxState = fluxState %>% filter(Flux %in% c("NPP","Rh","NEP","NECB")) %>% mutate(EcoregionName="State")

netFlux = bind_rows(netFluxEco, netFluxState) %>% write_csv("data/net_flux.csv")
netFlux$Flux = factor(netFlux$Flux, levels=c("NECB","NEP","Rh","NPP"))


# Land Use Emissions #####
df = read_csv("E:/california-carbon-futures/Data/eco_flows_by_scn_iter_ts.csv")
unique(df$TransitionGroup)
df1 = tibble(TransitionGroup=as.character(c("AgExpand", "Fire", "Insects", "Clearcut", "Thinning", "OrchardRemoval", "Urban")),
                 newName = as.character(c("Ag Expansion", "Fire", "Drought", "Forest Clearcut", "Forest Selection", "Orchard Removal", "Urbanization")))

df2 = df %>% left_join(df1, by="TransitionGroup") %>% select(-TransitionGroup) %>% rename("TransitionGroup"="newName") %>%
  group_by(LUC,GCM,RCP,Timestep,EcoregionName, TransitionGroup, Flow) %>%
  summarise(Mean=mean(Amount), Lower=quantile(Amount, 0.025), Upper=quantile(Amount, 0.975))
df2$TransitionGroup = as.character(df2$TransitionGroup)

df3 = df2 %>% filter(str_detect(Flow, "insects")) %>%
  group_by(LUC,GCM,RCP,Timestep,EcoregionName,TransitionGroup) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>% mutate(Flow="Mortality")
df4 = df2 %>% filter(TransitionGroup != "Drought") %>% bind_rows(df3)
df5 = df4 %>% group_by(LUC,GCM,RCP,Timestep,TransitionGroup,Flow) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>% mutate(EcoregionName="State") %>%
  bind_rows(df4)
write_csv(df5, "data/transitionFlows.csv")
unique(df5$TransitionGroup)





# Process Transition Data #####
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


  
# State Class Data #####
df = tibble(EcoregionName=c("Coast Range","Cascades","Sierra Nevada","California Chaparral and Oak Woodlands","Central California Valley",
                            "Southern California Mountains","Eastern Cascades Slopes and Foothills","Central Basin and Range",
                            "Mojave Basin and Range","Klamath Mountains","Northern Basin and Range","Sonoran Basin and Range","State"),
            ShortName=c("Coast Range","Cascades","Sierra Nevada","Oak Woodlands","Central Valley","SoCal Mtns.","East Cascades","Central B&R","Mojave B&R","Klamath Mtns.", 
                        "Northern B&R","Sonoran B&R","State"))
df1 = read_csv("data/state_lulc_by_scenario_timestep_95ci.csv") %>% mutate(EcoregionName="State")
df2 = read_csv("data/ecoregion_lulc_by_scenario_timestep_95ci.csv") %>% select(-EcoregionID) %>% bind_rows(df1) %>% left_join(df) %>%
  select(-EcoregionName) %>% rename("EcoregionName"="ShortName") %>%
  write_csv("data/lulc.csv")

unique(df2$EcoregionName)
unique(transEco$EcoregionName)















































