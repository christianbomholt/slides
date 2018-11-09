library(readxl)

optioner <- read_excel("R/couse_oct/data/xl/SPXoptions_changed.xlsx")


optioner %>% mutate(bid_ask = best_bid - best_offer) %>% 
  mutate(moneyness = abs(nonbundle/2085-1)) %>% 
  filter(bid_ask<5) %>% 
  filter(moneyness<0.4) %>% 
  filter(best_bid!=0) %>% 
  filter(nonbundle>1800) %>% 
  filter(cp_flag=="C") %>% 
  select(mid,cp_flag,best_bid,best_offer,nonbundle)
