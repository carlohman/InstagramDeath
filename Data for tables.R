# Move to thousands

library(tidyverse)

data.frame <- readRDS('./results/Models/global_in_models2.rds')

shrinking.data.frame <- data.frame %>% filter(
  Status == "Dead",
  Assumption == "Shrinking")    

shrinking.data.frame <- setDT(shrinking.data.frame)[, .(Profiles=sum(Profiles)),
                        by=list(Country=sub('\\d+$','', Country))]

saveRDS(shrinking.data.frame, './Results/Tables/Shrinking_table.rds')

growing.data.frame <- data.frame %>% filter(
  Status == "Dead",
  Assumption == "Growing")    

growing.data.frame <- setDT(growing.data.frame)[, .(Profiles=sum(Profiles)),
                                by=list(Country=sub('\\d+$','', Country))]

saveRDS(growing.data.frame, './Results/Tables/Growing_table.rds')
                                         