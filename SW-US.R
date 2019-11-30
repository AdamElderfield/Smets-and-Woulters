#-----------------------------------------------------------------------
# Smets and Woulters model of the US Economy - recreating MATLAB example
#-----------------------------------------------------------------------

# In this example, we will recreate the linear version VECM version of the Smetts and Woulters model of the US Economy. 

#-----------------------------------------------------------------------
# librarys needed
#-----------------------------------------------------------------------

library(Quandl)                           # For data
library(tidyverse)                        # For data manipulation / visualsation
library(vars)                             # For modelling
library(urca)                             # For modelling
source("~/GitHub/TST themes/Chart themes.R")

#-----------------------------------------------------------------------
# Using QUANDL FRED data to extract data
#-----------------------------------------------------------------------

Quandl.api_key("x7vhPMRdCbhdzYrvZZHu")

Data.FF <- Quandl("FRED/FEDFUNDS", type = "ts", collapse ="quarterly" )
Data.GDP <- 100*log(Quandl("FRED/GDP",type = "ts"))        # GDP (output)
Data.GDPDEF <- 100*log(Quandl("FRED/GDPDEF",type = "ts"))  # GDP deflator
Data.COE <- 100*log(Quandl("FRED/COE",type = "ts"))        # COE
Data.HOANBS <-  100*log(Quandl("FRED/HOANBS",type = "ts")) # Hours of all persons
Data.PCEC <-  100*log(Quandl("FRED/PCEC",type ="ts"))      # Consumption
Data.GPDI <- 100*log(Quandl("FRED/GPDI", type = "ts"))     # Investment


# Make a chart
Data.SW <- tibble(GDP = Data.GDP,
                  `GDP Deflator` = Data.GDPDEF,
                  COE = Data.COE,
                  `Hours` = Data.HOANBS,
                  `Consumption` = Data.PCEC,
                  `Investment` = Data.GPDI,
                  Date = seq(as.Date("1947-03-01"),as.Date("2018-12-04"), by = "quarter")) %>% 
  filter(Date >= "1954-09-01" &  Date <= "2018-12-01" ) %>% 
  bind_cols(tibble(`Federal Funds` = Data.FF[-259]))


chart1 <- Data.SW %>% 
  dplyr::select(Date, GDP, `GDP Deflator`) %>% 
  gather(Var, Val, -Date) %>% 
  ggplot()+
  geom_line(aes(Date, Val, colour = Var), size =1)+
  tst_theme()+
  scale_colour_tst()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  xlab("")+
  ylab("100x natural log")
  

chart2 <- Data.SW %>% 
  dplyr::select(Date, `Consumption`, `Investment`) %>% 
  gather(Var, Val, -Date) %>% 
  ggplot()+
  geom_line(aes(Date, Val, colour = Var), size =1)+
  tst_theme()+
  scale_colour_tst()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  xlab("")+
  ylab("100x natural log")

chart3 <- Data.SW %>% 
  dplyr::select(Date, COE, `Hours`) %>% 
  gather(Var, Val, -Date) %>% 
  ggplot()+
  geom_line(aes(Date, Val, colour = Var), size =1)+
  tst_theme()+
  scale_colour_tst()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  xlab("")+
  ylab("100x natural log")

chart4 <- Data.SW %>% 
  dplyr::select(Date, `Federal Funds`) %>% 
  gather(Var, Val, -Date) %>% 
  ggplot()+
  geom_line(aes(Date, Val, colour = Var), size =1)+
  tst_theme()+
  scale_colour_tst()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  xlab("")+
  ylab("Percent")

gridExtra::grid.arrange(chart1,chart2,chart3,chart4)

#-----------------------------------------------------------------------
# Determine the cointegration rank 
#-----------------------------------------------------------------------

Data.mod <- Data.SW %>% 
  dplyr::select(-Date, GDP, `GDP Deflator`, COE, `Hours`, `Federal Funds`, `Consumption`,`Investment`) 

ModH1 <- ca.jo(Data.mod, type = "trace", ecdet = "const" , K = 2)

summary(ModH1)

# H1 model suggest r =4
# Note: Add further analysis following matlab example

#-----------------------------------------------------------------------
# Restrict any coefficients to zero if p-val < 2 
#-----------------------------------------------------------------------
# To do this, estimate VAR in differences with error correction term as a exogenous regressor - this is the MATLAB process.


ectMod1 <- cbind(ModH1@x,1)%*%ModH1@V

# plot cointegrating relations
tibble(EC1 =  ectMod1[,1],
       EC2 =  ectMod1[,2],
       EC3 =  ectMod1[,3],
       EC4 =  ectMod1[,4],
       Date = Data.SW$Date) %>% 
  gather(Var, Val,-Date) %>% 
  ggplot()+
  geom_line(aes(Date, Val, colour = Var))+
  tst_theme()+
  scale_colour_tst()+
  theme(legend.title = element_blank())+
  xlab("")+
  ylab("")
    

ectMod1 <-  tibble(EC1 =  ectMod1[,1],
       EC2 =  ectMod1[,2],
       EC3 =  ectMod1[,3],
       EC4 =  ectMod1[,4])

# create difference data
diff.Data.mod <- Data.mod %>% 
  sapply(function(x) diff(as.numeric(x)))


VECM.H1 <-  VAR(diff.Data.mod, p =2 ,type = "none", exogen = ectMod1[-1,1:4])

VECM.H1.restrict <- restrict(VECM.H1)

#-----------------------------------------------------------------------
# Estimate the VAR model from the VEC 
#-----------------------------------------------------------------------

ModH1 <- vec2var(ModH1, r = 4)

ModH2 <- VAR(Data.mod, p = 3, type = "const")

# Resrict any coefficients to zero if p-val is less than 0

ResModh2 <-  restrict(ModH2, method = "ser", thresh = 2.0)

# Extract cointegration relations and plot

#-----------------------------------------------------------------------
# IRFs 
#-----------------------------------------------------------------------

ResponsesGDP <- ModH1 %>% 
  irf(response = "GDP", n.ahead = 40)


responses <- list()
for(i in names(ResponsesGDP$irf)){
  
  responses[[paste(i)]] <- as.numeric(ResponsesGDP$irf[[i]])
  responses[[paste0(i,"_l")]] <- as.numeric(ResponsesGDP$Lower[[i]])
  responses[[paste0(i,"_h")]] <- as.numeric(ResponsesGDP$Upper[[i]])
  
}

responses <- bind_cols(responses) %>% 
  mutate(H = 1:41)

charts <- list()
for( i in 1:length(names(ResponsesGDP$irf) ) ){

 charts[[names(ResponsesGDP$irf)[i] ]] <-  responses %>% 
    gather(Var, Val, -H) %>% 
    filter(Var == (names(ResponsesGDP$irf)[i])) %>%    
    filter(!grepl("_h|_l",.$Var)) %>%
    left_join(responses %>% 
                gather(Var, Val, -H) %>% 
                filter(grepl(paste0(names(ResponsesGDP$irf)[i],"_h"),.$Var)) %>%
                mutate(Var = gsub("_h","",.$Var)) %>%
                rename(Plus95 = Val)
    ) %>%
    left_join(responses %>% 
                gather(Var, Val, -H) %>% 
                filter(grepl(paste0(names(ResponsesGDP$irf)[i],"_l"),.$Var)) %>%
                mutate(Var = gsub("_l","",.$Var)) %>%
                rename(Less95 = Val )) %>%           
    ggplot(aes(x=H))+
    geom_line(aes(y = Val), colour = tst_colors[1])+
    geom_ribbon(aes(ymin = Less95, ymax = Plus95, group = Var), alpha = 0.2, fill = tst_colors[2] ) +
    tst_theme()+
    scale_colour_tst()+
    ggtitle(label = paste("GDP impulse response to",names(ResponsesGDP$irf)[i]))
  
    
}

gridExtra::grid.arrange(charts$GDP,
                        charts$GDP.Deflator,
                        charts$COE,
                        charts$Hours,
                        charts$Consumption,
                        charts$Investment,
                        charts$Federal.Funds, ncol =2)
                          

#-----------------------------------------------------------------------
# Out of sample forecasting 
#-----------------------------------------------------------------------

ResModh2 %>% predict(h = 20) %>% plot()
