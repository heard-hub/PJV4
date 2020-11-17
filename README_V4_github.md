Heard and Zimmerman
================
2020-11-17

# SUPPLEMENTARY FEEDING CARIBOU

## Number of pictures per year

| year | number\_of\_pics |
| ---: | ---------------: |
| 2014 |           215000 |
| 2015 |           624000 |
| 2016 |           700000 |
| 2017 |           906000 |
| 2018 |          1159000 |
| 2019 |          1366000 |

### Total pictures all years = 4.9710^{6}

## Arrival at Kennedy Siding of collared caribou

``` r
collars<- readxl::read_excel("KS_for_r.xlsx",   sheet="collar.arrival")
kable(collars, digits=2)
```

| year    | WLHID     | ID             | detection\_date | GPS\_arrival\_date | day\_diff |
| :------ | :-------- | :------------- | --------------: | -----------------: | --------: |
| 2014-15 | NA        | iridiMOM       |           10.23 |              10.11 |        12 |
| 2014-15 | NA        | iridiMOM2      |           10.20 |              10.14 |         6 |
| 2014-15 | NA        | flux net       |           11.02 |              10.21 |        12 |
| 2015-16 | 18-13052H | iridiMOM       |            9.27 |               9.25 |         2 |
| 2015-16 | 18-13082H | peg’s mom      |           10.20 |              10.07 |        13 |
| 2015-16 | 18-13081H | gray neck girl |           10.20 |              10.13 |         7 |
| 2015-16 | 18-13083H | hook           |           12.29 |              10.26 |        63 |
| 2016-17 | 18-13081H | gray neck girl |            9.30 |               9.22 |         8 |
| 2016-17 | 18-13052H | iridiMOM       |            9.28 |               9.27 |         1 |
| 2016-17 | 18-13093H | TWIGGY         |            9.28 |               9.27 |         1 |
| 2016-17 | 18-13082H | peg’s mom      |           10.07 |              10.07 |         0 |
| 2016-17 | 18-13094H | TEMPLE         |           10.13 |              10.11 |         2 |
| 2016-17 | 18-13095H | PIN            |           10.24 |              10.25 |       \-1 |
| 2017-18 | 15-7318   | Velveteen      |            9.10 |               9.11 |       \-1 |
| 2017-18 | 18-13082H | peg’s mom      |            9.14 |              10.01 |      \-17 |
| 2017-18 | 15-7320   | Funky top      |           10.01 |              10.02 |       \-1 |
| 2017-18 | 18-13094H | TEMPLE         |           10.04 |              10.05 |       \-1 |
| 2017-18 | 15-7321   | White Ear      |           10.04 |              10.06 |       \-2 |
| 2017-18 | 18-13052H | iridiMOM       |           10.12 |              10.08 |         4 |
| 2017-18 | 18-13081H | gray neck girl |           10.10 |              10.09 |         1 |
| 2017-18 | 15-7319   | Jester         |           10.27 |              10.10 |        17 |
| 2017-18 | 18-13095H | PIN            |           10.17 |              10.14 |         3 |
| 2018-19 | 15-7318   | Velveteen      |            9.13 |               9.13 |         0 |
| 2018-19 | 17-10518  | TWEEK          |            9.17 |               9.16 |         1 |
| 2018-19 | 17-10516  | BLADE          |            9.13 |               9.17 |       \-4 |
| 2018-19 | 17-10519  | STARBOARD      |            9.20 |               9.21 |       \-1 |
| 2018-19 | 15-7320   | Funky top      |            9.24 |               9.24 |         0 |
| 2018-19 | 15-7321   | White Ear      |            9.29 |               9.24 |         5 |
| 2018-19 | 18-13094H | TEMPLE         |           10.04 |              10.04 |         0 |
| 2018-19 | 18-13095H | PIN            |           10.09 |              10.09 |         0 |
| 2018-19 | 17-10517  | BLUR           |           10.16 |              10.17 |       \-1 |
| 2018-19 | 15-7319   | Jester         |           10.30 |              11.01 |       \-2 |
| 2019-20 | 15-7321   | White Ear      |           10.11 |               9.17 |        24 |
| 2019-20 | 17-10518  | TWEEK          |            9.25 |               9.20 |         5 |
| 2019-20 | 18-13988  | SWELL          |            9.24 |               9.24 |         0 |
| 2019-20 | 18-13992  | ALERT          |            9.24 |               9.24 |         0 |
| 2019-20 | 15-7320   | Funky top      |           10.04 |              10.01 |         3 |
| 2019-20 | 18-13987  | LIPS           |           10.07 |              10.06 |         1 |
| 2019-20 | 17-10517  | BLUR           |           10.09 |              10.07 |         2 |
| 2019-20 | 17-10516  | BLADE          |           10.19 |              10.09 |        10 |
| 2019-20 | 18-13993  | BAG LADY       |           10.10 |              10.09 |         1 |

## Arrival rate of caribou by year

``` r
#  arrivals  
figarr <- readxl::read_excel("KS_for_r.xlsx",   sheet="arrivals")

 figarr$graphdate<-as.Date(figarr$rrdate, origin="1899-12-30") 
  figarr$yr<-as.factor(figarr$yr)
  
  figarr$YEAR<-factor(figarr$yr, levels=c("2020","2019","2018","2017","2016","2015"))
  x<-data.frame(date=figarr$graphdate, value=figarr$count)
  x$year<-year(x$date)
  x$day_of_year<-as.Date(paste("2014", month(x$date), mday(x$date), sep="-"))
  x$year<-x$year+1
  x$yr<-as.factor(x$year)
  x$YEAR<-factor(x$yr, levels=c("2020","2019","2018","2017","2016","2015"))

g<-ggplot(x, aes(x=day_of_year, y=value, color=YEAR))+
  geom_line() +scale_x_date(labels=date_format("%m/%d"))+
  scale_colour_aaas()+
  scale_fill_discrete(guide=guide_legend(reverse = TRUE))+
  xlab("DATE (month/day)") + ylab("CUMMULATIVE CARIBOU COUNT")+
  theme(legend.justification=c(0, 1.2), legend.position=c(.8,.6))+
  geom_line(aes(color=as.factor(year)), size=1.0)
#g

#  ggsave(g, file='6_yr_arrival_graph.png',h=4, w=6, units='in', dpi=300) 

#   PCT   
figarr <- readxl::read_excel("KS_for_r.xlsx",   sheet="arrivals")
figarr$graphdate<-as.Date(figarr$rrdate, origin="1899-12-30") 
figarr$year<-as.factor(figarr$year)
figarr$YEAR<-factor(figarr$year, levels=c("2019","2018","2017","2016","2015","2014"))
x<-data.frame(date=figarr$graphdate, value=figarr$countPCT)
x$year<-year(x$date)
x$day_of_year<-as.Date(paste("2014", month(x$date), mday(x$date), sep="-"))
x$year<-as.factor(x$year)
x$YEAR<-factor(x$year, levels=c("2019","2018","2017","2016","2015","2014"))

gPCT<-ggplot(x, aes(x=day_of_year, y=value, color=YEAR))+
  geom_line() +scale_x_date(labels=date_format("%m/%d"))+
  scale_colour_aaas()+
  theme(legend.position="none", axis.title.x=element_blank())+
#  scale_fill_discrete(guide=guide_legend(reverse = TRUE))+
  xlab("") + ylab("%")+
#  theme_bw()+
  geom_line(aes(color=as.factor(year)), size=1.0)
#gPCT 


g22<-g+annotation_custom(ggplotGrob(gPCT), xmin=as.Date("2014-08-18"), 
                    xmax=as.Date("2014-10-16"), ymin=60, ymax=90)
g22
```

![](c:/Users/DCH/Dropbox/!!r/KS_caribou/README_V4_github_files/figure-gfm/arrivals-1.png)<!-- -->

``` r
# ggsave(g22, file='6_yr_arrival_graph22.png',h=4, w=6, units='in', dpi=300) 
```

## Age and Sex of all caribou by year

### 2014-2015

``` r
ind14<- readxl::read_excel("KS_for_r.xlsx",   sheet="2014")
ind15<- readxl::read_excel("KS_for_r.xlsx",   sheet="2015")
ind16<- readxl::read_excel("KS_for_r.xlsx",   sheet="2016")
ind17<- readxl::read_excel("KS_for_r.xlsx",   sheet="2017")
ind18<- readxl::read_excel("KS_for_r.xlsx",   sheet="2018")
ind19<- readxl::read_excel("KS_for_r.xlsx",   sheet="2019")
```

### 2014-15

``` r
#cat("individuals in 2014")
  kable(ind14)
```

| number | NAME            | age | sex |
| -----: | :-------------- | :-- | :-- |
|  14.01 | ADRIAN          | ad  | m   |
|  14.02 | DOS EQUIS       | ad  | f   |
|  14.03 | KAHUNA          | ad  | m   |
|  14.04 | TINA            | ad  | f   |
|  14.05 | DROPTINE        | ad  | f   |
|  14.06 | CALLIE          | ad  | f   |
|  14.07 | SHE             | ad  | f   |
|  14.08 | FUZZ BUTT       | ad  | f   |
|  14.09 | SPINDLY TOPS    | ad  | m   |
|  14.10 | BLACK BELLY     | ad  | f   |
|  14.11 | JUNIOR          | ad  | m   |
|  14.12 | JAMES DEAN      | ad  | m   |
|  14.13 | SLENDER ANTLERS | ad  | f   |
|  14.14 | TWIN PEAKS      | ad  | m   |
|  14.15 | HOOK            | ad  | f   |
|  14.16 | GRAY NECK       | ad  | m   |
|  14.17 | FLUXNET         | ad  | f   |
|  14.19 | STUB            | ad  | f   |
|  14.20 | SPIKE           | ad  | f   |
|  14.21 | CROOKED         | ad  | m   |
|  14.22 | BIGBROW         | ad  | m   |
|  14.23 | PEG             | c   | f   |
|  14.25 | GREY NECK GIRL  | ad  | f   |
|  14.26 | PEG’S MOM       | ad  | f   |
|  14.27 | DOUBLE-DOUBLE   | ad  | f   |
|  14.28 | RIBS            | ad  | f   |
|  14.29 | RIB’S BOY       | c   | m   |
|  14.30 | CRACKED         | ad  | m   |
|  14.31 | COMBO           | ad  | m   |
|  14.32 | IRIDIUM 2       | ad  | f   |
|  14.33 | IRIDI-MOM       | ad  | f   |
|  14.34 | iBABY           | c   | f   |
|  14.36 | REAR POINTS     | ad  | m   |
|  14.37 | ORPHAN          | c   | f   |
|  14.38 | LEFT CURVE      | ad  | f   |
|  14.39 | LCs KID         | c   | m   |
|  14.40 | MEGA STUB       | ad  | f   |
|  14.41 | BLACK EARS      | ad  | f   |
|  14.42 | SPATULA         | ad  | m   |
|  14.43 | LEFT HOOK       | ad  | f   |
|  14.44 | LEFT HOOK’S BOY | c   | m   |
|  14.45 | STUB 2          | ad  | f   |
|  14.46 | STUB 2’S CALF   | c   | m   |
|  14.47 | SHED            | ad  | m   |
|  14.48 | FRONT POINTS    | ad  | m   |
|  14.49 | BULLET          | ad  | m   |
|  14.50 | SNOW NOSE       | ad  | f   |
|  14.51 | JESTER          | ad  | f   |
|  14.52 | WHITE EYE       | ad  | f   |

### 2015-16

``` r
#banner("individuals in 2015")
  kable(ind15)
```

| number | name            | age | sex |
| -----: | :-------------- | :-- | :-- |
|  15.01 | VELVET          | ad  | f   |
|  15.02 | STRETCH         | ad  | f   |
|  15.03 | STR.CALF        | c   | m   |
|  15.04 | SHRED           | ad  | m   |
|  15.05 | PEBBLES         | ad  | m   |
|  15.06 | TOP KNOT        | ad  | f   |
|  15.07 | TK              | ad  | m   |
|  15.08 | DT2             | ad  | f   |
|  15.09 | dos equis       | ad  | f   |
|  15.10 | LUCKY           | ad  | m   |
|  15.11 | PEACE           | ad  | f   |
|  15.12 | iridi-mom       | ad  | f   |
|  15.13 | SHINY           | ad  | f   |
|  15.14 | BULBOUS         | ad  | m   |
|  15.15 | BORIS           | ad  | m   |
|  15.16 | MIDNIGHT        | ad  | m   |
|  15.17 | REAR PIPES      | ad  | m   |
|  15.18 | BROTHER BULBOUS | ad  | m   |
|  15.19 | TWIGGY          | ad  | f   |
|  15.20 | ROBOT           | ad  | m   |
|  15.21 | QUADRA SPIKE    | ad  | f   |
|  15.22 | SWEEP           | ad  | m   |
|  15.23 | DREAMER         | ad  | f   |
|  15.24 | TWIST           | ad  | f   |
|  15.25 | FORK LIFT       | ad  | m   |
|  15.26 | SHRED-LIKE      | ad  | m   |
|  15.27 | MARCH           | ad  | m   |
|  15.28 | RIGHTY          | ad  | f   |
|  15.29 | gray neck girl  | ad  | f   |
|  15.30 | JEST2           | ad  | f   |
|  15.31 | PALMETTE        | ad  | m   |
|  15.32 | SCYTHE          | ad  | m   |
|  15.33 | TEMPLE          | ad  | f   |
|  15.34 | TEM.CALF        | c   | f   |
|  15.35 | hook            | ad  | f   |
|  15.36 | HOOK’S CALF     | c   | m   |
|  15.37 | ISABELLE        | ad  | f   |
|  15.38 | ISABELLE’S BOY  | c   | m   |
|  15.39 | KK              | ad  | f   |
|  15.40 | PIN             | ad  | f   |
|  15.41 | CAST            | ad  | m   |
|  15.42 | peg’s mom       | ad  | f   |
|  15.43 | NOT11           | ad  | f   |
|  15.44 | MICRON          | ad  | f   |
|  15.45 | FLAME           | ad  | f   |
|  15.46 | TRICERATOPS     | ad  | f   |
|  15.47 | GRAEME          | ad  | m   |
|  15.48 | EUREKA          | ad  | f   |
|  15.49 | ERIK            | ad  | m   |
|  15.50 | NOT 11’s GIRL   | c   | f   |

### 2016-17

``` r
#banner("individuals in 2016")
   kable(ind16)
```

| number | name                            | age  | sex |
| -----: | :------------------------------ | :--- | :-- |
|  16.01 | KEENER                          | ad   | m   |
|  16.02 | VELVETEEN                       | ad   | f   |
|  16.03 | ROBOT 2016                      | ad   | m   |
|  16.04 | PADDLE                          | ad   | m   |
|  16.05 | MINI-BEZES                      | ad   | m   |
|  16.06 | FUNKY TOP                       | ad   | f   |
|  16.07 | SWEEP 2016                      | ad   | m   |
|  16.08 | HENRIETTA LACKS                 | ad   | f   |
|  16.09 | IRIDI-MOM 2016                  | ad   | f   |
|  16.10 | IRIDI-MOM’S BOY                 | calf | m   |
|  16.11 | TWIGGY                          | ad   | f   |
|  16.12 | TIPLESS                         | ad   | m   |
|  16.13 | GRAY NECK GIRL                  | ad   | f   |
|  16.14 | GRAY NECK’S BOY                 | calf | m   |
|  16.15 | GOAT                            | ad   | f   |
|  16.16 | GOAT’S GIRL                     | calf | f   |
|  16.17 | RED EYE                         | ad   | f   |
|  16.18 | RED EYE’S GIRL                  | calf | f   |
|  16.19 | THINKER                         | ad   | f   |
|  16.20 | BEZOOKA                         | ad   | m   |
|  16.21 | CROWN                           | ad   | f   |
|  16.22 | FUNKY TOP’S BOY                 | calf | m   |
|  16.23 | ESTRAGON                        | ad   | m   |
|  16.24 | GOGO                            | ad   | m   |
|  16.25 | GODOT                           | ad   | f   |
|  16.26 | PEG’S MOM 2016                  | ad   | f   |
|  16.27 | LIMPY                           | ad   | m   |
|  16.28 | DROP TINE 2016                  | ad   | f   |
|  16.29 | SHINY 2016                      | ad   | f   |
|  16.30 | CROWN’S BOY                     | calf | m   |
|  16.31 | GODOT’S GIRL                    | calf | f   |
|  16.32 | GOGO’S TWIN                     | ad   | f   |
|  16.33 | RIGHTY                          | ad   | f   |
|  16.34 | WALRUS                          | ad   | m   |
|  16.35 | PEG’S MOM’S BOY                 | calf | m   |
|  16.36 | JESTER 2016                     | ad   | f   |
|  16.37 | R2’s GIRL also called BEAM      | calf | f   |
|  16.38 | R2                              | ad   | f   |
|  16.39 | HARPOON                         | ad   | f   |
|  16.40 | JESTER’S BOY                    | calf | m   |
|  16.41 | HARPOONS’S BOY                  | calf | m   |
|  16.42 | REAR POINTS 2017                | ad   | m   |
|  16.43 | VLADIMIR                        | ad   | m   |
|  16.44 | TEMPLE 2016                     | ad   | f   |
|  16.45 | TEMPLE’S GIRL                   | calf | f   |
|  16.46 | WHITE EAR                       | ad   | f   |
|  16.47 | WHITE EAR’S BOY                 | calf | m   |
|  16.48 | PIPPA                           | ad   | f   |
|  16.49 | ELKY                            | ad   | m   |
|  16.50 | BLUNT TOP                       | ad   | m   |
|  16.51 | SIMPLE                          | ad   | f   |
|  16.52 | PIN 2016                        | ad   | f   |
|  16.53 | CEE                             | ad   | m   |
|  16.54 | c1none                          | calf | f   |
|  16.55 | c2bumps                         | calf | f   |
|  16.56 | c3stubs not PIN 2016’S BOY      | calf | m   |
|  16.57 | WHIPPET                         | ad   | m   |
|  16.58 | DOS EQUIS 2016                  | ad   | f   |
|  16.59 | ANNIE                           | ad   | f   |
|  16.60 | ANNIE’S BOY also called PENGUIN | calf | m   |
|  16.61 | HOLLOW                          | ad   | m   |
|  16.62 | LYRE                            | ad   | f   |
|  16.63 | PIN’s boy                       | calf | m   |

### 2017-18

``` r
#banner("individuals in 2017")
   kable(ind17)
```

| number | name                                     | age  | sex |
| -----: | :--------------------------------------- | :--- | :-- |
|  17.01 | VELVETEEN (2016) purple L car201         | ad   | F   |
|  17.02 | HENRIETTA LACKS (2016)                   | ad   | F   |
|  17.03 | WALRUS (2016)                            | ad   | M   |
|  17.04 | PEG’S MOM (2016) orange L car186         | ad   | F   |
|  17.05 | POST                                     | ad   | F   |
|  17.06 | POST’s GIRL                              | calf | F   |
|  17.07 | BAZOOKA (2016)                           | ad   | M   |
|  17.08 | SWEEP (2016)                             | ad   | M   |
|  17.09 | KEENER (2016)                            | ad   | M   |
|  17.10 | BLUR                                     | ad   | F   |
|  17.11 | BLADE                                    | ad   | F   |
|  17.12 | BLADE"S GIRL                             | calf | F   |
|  17.13 | SHY                                      | ad   | M   |
|  17.14 | FUNKY TOP (2016) orange L car203         | ad   | F   |
|  17.15 | TWEAK                                    | ad   | F   |
|  17.16 | SHY’S TWIN                               | ad   | M   |
|  17.17 | TWO-SPIKE                                | ad   | F   |
|  17.18 | TWO-SPIKES GIRL                          | calf | F   |
|  17.19 | DIMPLE                                   | ad   | F   |
|  17.20 | DIMPLE’S GIRL                            | calf | F   |
|  17.21 | DELICATE                                 | ad   | F   |
|  17.22 | MOOSE                                    | ad   | F   |
|  17.23 | GNG grey neck girl (2014) green L car185 | ad   | F   |
|  17.24 | DINO                                     | ad   | M   |
|  17.25 | UPRIGHT                                  | ad   | F   |
|  17.26 | MONSTER                                  | ad   | M   |
|  17.27 | REAR PIPES (2015)                        | ad   | M   |
|  17.28 | BEZ BLADE                                | ad   | M   |
|  17.29 | CHAMPION                                 | ad   | F   |
|  17.30 | CHAMPION’S GIRL                          | calf | F   |
|  17.31 | FORK                                     | ad   | F   |
|  17.32 | FORK’S BOY                               | calf | M   |
|  17.33 | ROBOT (2014)                             | ad   | M   |
|  17.34 | ROBOT’S TWIN                             | ad   | M   |
|  17.35 | PIN (2015) yellow R car200               | ad   | F   |
|  17.36 | FRONTBACK                                | ad   | M   |
|  17.37 | JANIE                                    | ad   | F   |
|  17.38 | FORKLIFT                                 | ad   | M   |
|  17.39 | EAR SPIKES                               | ad   | F   |
|  17.40 | EAR SPIKES’ BOY                          | calf | M   |
|  17.41 | TWEAK’S TWIN                             | ad   | F   |
|  17.42 | WHITE EAR (2016) orange R car204         | ad   | F   |
|  17.43 | TEMPLE (2016) blue R car199              | ad   | F   |
|  17.44 | MINI BEZETTE                             | ad   | F   |
|  17.45 | SONY                                     | ad   | F   |
|  17.46 | FLAME                                    | ad   | M   |
|  17.47 | FORK’S BROTHER                           | ad   | M   |
|  17.48 | FORKLIFT’S TWIN                          | ad   | M   |
|  17.49 | GLINT                                    | ad   | M   |
|  17.50 | FUNKY TOP’S TWIN                         | ad   | M   |
|  17.51 | ELEGANT                                  | ad   | F   |
|  17.52 | PORT                                     | ad   | F   |
|  17.53 | JESTER (2014) green L car202             | ad   | F   |
|  17.54 | FUTURE                                   | ad   | M   |
|  17.55 | STARBOARD                                | ad   | F   |
|  17.56 | BLACK FACE                               | ad   | F   |
|  17.57 | RIGHT WING                               | ad   | M   |
|  17.58 | IRIDI MOM (2014) orange L car156b        | ad   | F   |
|  17.59 | SENIOR                                   | ad   | M   |
|  17.60 | Q                                        | ad   | M   |
|  17.61 | DOS EQUIS (2014) car039                  | ad   | F   |
|  17.62 | DOS EQUIS’ BOY                           | calf | M   |
|  17.63 | FLEET                                    | ad   | M   |
|  17.65 | BALDY                                    | ad   | F   |
|  17.66 | BALDY’S GIRL                             | calf | F   |

### 2018-19

``` r
#banner("individuals in 2018")
   kable(ind18)
```

| number | name                              | age  | sex |
| -----: | :-------------------------------- | :--- | :-- |
|  18.01 | SCOUT                             | ad   | F   |
|  18.02 | EARLY BIRD                        | ad   | F   |
|  18.03 | EARLY BIRD’S Boy                  | calf | M   |
|  18.04 | VELVETEEN (2016) car201 LEFT      | ad   | F   |
|  18.05 | VELVETEEN’s Girl                  | calf | F   |
|  18.06 | BLADE(2017) 150.410 car212 RIGHT  | ad   | F   |
|  18.07 | BLADE’S Girl                      | calf | F   |
|  18.08 | SPROUT                            | ad   | F   |
|  18.09 | TWEAK(2017) 150.420 car211 LEFT   | ad   | F   |
|  18.10 | ROBOT’S TWIN (2017)               | ad   | M   |
|  18.11 | GREY NECK GIRL (2014) car185 LEFT | ad   | F   |
|  18.12 | Q (2017)                          | ad   | M   |
|  18.13 | PEG’S MOM (2016) car186 LEFT      | ad   | F   |
|  18.14 | PEG’S MOM’S Boy                   | calf | M   |
|  18.15 | CINDER                            | ad   | F   |
|  18.16 | SONY                              | ad   | M   |
|  18.17 | SWELL                             | ad   | F   |
|  18.18 | SHEDDING                          | ad   | F   |
|  18.19 | SLURPY                            | ad   | F   |
|  18.20 | BEAR                              | ad   | M   |
|  18.21 | ALERT                             | ad   | F   |
|  18.22 | ALERTS Girl                       | calf | F   |
|  18.23 | KNOB                              | ad   | F   |
|  18.24 | HENRIETTA LACKS (2017)            | ad   | F   |
|  18.25 | HR’s CALF                         | calf | F   |
|  18.26 | 6 PACK                            | ad   | M   |
|  18.27 | BUFF                              | ad   | M   |
|  18.28 | BW                                | ad   | F   |
|  18.29 | KNOB’S GIRL                       | calf | F   |
|  18.30 | B2D2                              | ad   | M   |
|  18.31 | ROMAN                             | ad   | M   |
|  18.32 | RIGHTBLACK                        | ad   | F   |
|  18.33 | WALRUS (2016)                     | ad   | M   |
|  18.34 | DARKY                             | ad   | M   |
|  18.35 | LIPS                              | ad   | F   |
|  18.36 | NECK                              | ad   | M   |
|  18.37 | SPEAR                             | ad   | F   |
|  18.38 | SNAP                              | ad   | M   |
|  18.39 | TROOPER                           | ad   | M   |
|  18.40 | BRUTUS                            | ad   | M   |
|  18.41 | STARBOARD (2016) car209 RIGHT     | ad   | F   |
|  18.42 | STARBOARD’S CALF                  | calf | F   |
|  18.43 | RAIN                              | ad   | M   |
|  18.45 | ICE PICK                          | ad   | M   |
|  18.46 | WHITE EAR (2016) car204 RIGHT     | ad   | F   |
|  18.47 | WHITE EAR’S BOY                   | calf | M   |
|  18.48 | BLIP                              | ad   | F   |
|  18.49 | SQUARE                            | ad   | F   |
|  18.50 | FUNKY TOP (2016) car203 LEFT      | ad   | F   |
|  18.51 | FUNKY TOP (2016) ’ BOY            | calf | M   |
|  18.52 | WIDEBROW                          | ad   | F   |
|  18.53 | WIDEBROW’S GIRL                   | calf | F   |
|  18.54 | PIN (2015) car200 RIGHT           | ad   | F   |
|  18.55 | PIN (2015) Girl                   | calf | F   |
|  18.56 | JESTER (2014) car202 LEFT         | ad   | F   |
|  18.57 | JESTER (2014)’s GIRL              | calf | F   |
|  18.58 | IRIDI-MOM (2014) car156 LEFT      | ad   | F   |
|  18.59 | 2-DROP                            | ad   | F   |
|  18.60 | 2-DROP’S BOY                      | calf | M   |
|  18.61 | BUMPS                             | ad   | M   |
|  18.62 | SHOULDER                          | ad   | M   |
|  18.63 | BBQ                               | ad   | F   |
|  18.64 | BLACKFACE(2017)                   | ad   | F   |
|  18.65 | TEMPLE (2016) car199 RIGHT        | ad   | F   |
|  18.66 | BLUR car210 LEFT                  | ad   | F   |
|  18.67 | BLUR’S Girl                       | calf | F   |
|  18.68 | GUIDE                             | ad   | M   |
|  18.69 | SNAKE                             | ad   | M   |
|  18.70 | BAG LADY                          | ad   | F   |
|  18.71 | BAG LADY’s GIRL                   | calf | F   |
|  18.72 | LITTLE EAR                        | ad   | M   |
|  18.73 | BASKET                            | ad   | M   |
|  18.74 | BBQ’S BOY white legs              | calf | M   |
|  18.75 | SEE-SHEDDING                      | ad   | F   |
|  18.76 | IRIDI-MOM (2014)’s BOY            | calf | M   |
|  18.77 | GARDEN                            | ad   | F   |
|  18.78 | FLAT BEZ                          | ad   | M   |

### 2019-20

``` r
#banner("individuals in 2019")
   kable(ind19)
```

| number | name                                         | age | sex |
| -----: | :------------------------------------------- | :-- | :-- |
|  19.01 | PALM lady                                    | ad  | F   |
|  19.02 | LEFT CROSS                                   | ad  | F   |
|  19.03 | ROPER / R antler like a loose rope           | ad  | F   |
|  19.04 | ROPER’S BOY                                  | c   | M   |
|  19.05 | LEFT 4BEZ also Keizer                        | ad  | M   |
|  19.06 | BURR BUMP top brow                           | ad  | M   |
|  19.07 | TRUCKER                                      | ad  | M   |
|  19.08 | WHITE SPOT                                   | ad  | M   |
|  19.09 | INDY                                         | ad  | F   |
|  19.10 | WHITE EAR (2016) car204 RIGHT                | ad  | F   |
|  19.11 | INLET 1st had counts 4 L & R backward r brow | ad  | M   |
|  19.12 | TWEEK (2017) car 211 LEFT                    | ad  | F   |
|  19.13 | TWEEK’S GIRL                                 | c   | F   |
|  19.14 | SIMPLE                                       | ad  | M   |
|  19.15 | SWELL (2018) WLHID 18-13988 right            | ad  | F   |
|  19.16 | ALERT (2018) WLHID 18-13992 LEFT             | ad  | F   |
|  19.17 | PROUD                                        | ad  | F   |
|  19.18 | WALRUS (2016)                                | ad  | M   |
|  19.19 | ALERT’S BOY                                  | c   | M   |
|  19.20 | BEZINGA                                      | ad  | M   |
|  19.21 | PORCUPINE                                    | ad  | M   |
|  19.22 | MOON                                         | ad  | F   |
|  19.23 | SYMMETRY                                     | ad  | F   |
|  19.24 | SYMMETRY’S BOY                               | c   | M   |
|  19.25 | ZERO                                         | ad  | F   |
|  19.26 | FUNKY TOP (2016) LEFT butch                  | ad  | F   |
|  19.27 | SPEAR (2018) LEFT impala                     | ad  | F   |
|  19.28 | ROOSTER                                      | ad  | M   |
|  19.29 | STARBOARD (2017) RIGHT hook                  | ad  | F   |
|  19.30 | IRIDI-MOM (2014) LEFT spoons                 | ad  | F   |
|  19.31 | HENRIETTA LACKS (2017) see BLONDY            | ad  | F   |
|  19.32 | PIN (2015) RIGHT small brow                  | ad  | F   |
|  19.33 | PIN’S BOY                                    | c   | M   |
|  19.34 | TEMPLE (2016) car199 RIGHT                   | ad  | F   |
|  19.35 | YOUNG BOY                                    | ad  | M   |
|  19.36 | BATMAN                                       | ad  | M   |
|  19.37 | LIPS (2018) WLHID 18-13987 RIGHT             | ad  | F   |
|  19.38 | BAG LADY (2018) 18-13993 RIGHT               | ad  | F   |
|  19.39 | BAG LADY’S BOY SPIKES BEZZIE’s BOY           | c   | M   |
|  19.40 | MS.ELBOWS                                    | ad  | F   |
|  19.41 | STRETCH                                      | ad  | M   |
|  19.42 | EXTRA - BURR BUMPS mr big                    | ad  | M   |
|  19.43 | BLUR (2017) car 210 LEFT stubby              | ad  | F   |
|  19.44 | BLONDY see 19.31 HR                          | ad  | F   |
|  19.45 | SLINGSHOT                                    | ad  | M   |
|  19.46 | TINY BEZ                                     | ad  | M   |
|  19.47 | EASY                                         | ad  | F   |
|  19.48 | PEG’S MOM (2016) LEFT                        | ad  | F   |
|  19.49 | NIGHT OWL                                    | ad  | F   |
|  19.50 | SPIKY BROW                                   | ad  | M   |
|  19.51 | GREY NECK GIRL (2016) car185 LEFT            | ad  | F   |
|  19.52 | VICTORY                                      | ad  | F   |
|  19.53 | VICTORY’S GIRL needle                        | c   | F   |
|  19.54 | BEAST                                        | ad  | M   |
|  19.55 | GRANDPA top brow                             | ad  | M   |
|  19.56 | B\&C                                         | ad  | M   |
|  19.57 | BLADE (2017) car212 missing ear tag          | ad  | F   |
|  19.58 | AMOUR hang top                               | ad  | M   |
|  19.59 | TOP-3                                        | ad  | F   |
|  19.60 | NEW GIRL prev thought BOY small top          | ad  | F   |
|  19.61 | LEAST                                        | ad  | F   |
|  19.62 | GREY NECK GIRLS’S BOY \[ANTENNA\]            | c   | M   |
|  19.63 | JERSEY two brow                              | ad  | M   |
|  19.64 | CLAWS                                        | ad  | F   |
|  19.65 | MINIMAL                                      | ad  | F   |
|  19.66 | SHARKFIN                                     | ad  | M   |
|  19.67 | SICKLE                                       | ad  | F   |
|  19.68 | IMPOSTER                                     | ad  | F   |
|  19.69 | SMALL RIGHT                                  | ad  | F   |
|  19.70 | FORK TOP                                     | ad  | M   |
|  19.71 | RIGHT FORK                                   | ad  | F   |
|  19.72 | BEZ FORKS                                    | ad  | F   |
|  19.73 | PINCHER                                      | ad  | M   |
|  19.74 | HEART                                        | ad  | F   |
|  19.75 | CREEP                                        | ad  | M   |
|  19.76 | GRASPING                                     | ad  | M   |
|  19.77 | POTENTIAL limper from BB                     | ad  | M   |
|  19.78 | LONG BROW                                    | ad  | M   |
|  19.79 | LEAST’S BOY ET                               | c   | M   |
|  19.80 | SEMI                                         | ad  | M   |
|  19.81 | LATECOMER                                    | ad  | M   |
|  19.82 | HIGHWAY                                      | ad  | F   |
|  19.83 | JESTER (2014) missing ear tag                | ad  | F   |
|  19.85 | AMAZON                                       | ad  | F   |
|  19.86 | AMAZONs CALF                                 | c   | F   |
|  19.87 | FUZZBROW                                     | ad  | M   |
|  19.88 | MRS ELBOWS BOY                               | c   | M   |

# Number of caribou by age sex & year

``` r
# Table 1
## 6 year counts
table1 <- readxl::read_excel("KS_for_r.xlsx",   sheet="6_summary_counts")
t1<-hux(table1)
t1<-t1 %>%
 set_bold(1, everywhere, TRUE) %>%
 set_font_size(12)  %>%
 set_bottom_border(3,everywhere, TRUE)   
```

``` r
t1
```

``` 
 YEAR       2014-15   2015-16   2016-17   2017-18   2018-19   2019-20  
 Cows            26        26        27        32        36        44  
 Female           3         2         7         6        11         3  
 calves                                                                
```

────────────────────────────────────────────────────────────────────────
Total 29 28 34 38 47 47  
females  
Bulls 16 19 18 24 23 32  
Male 4 3 11 3 7 8  
calves  
Total 20 22 29 27 30 40  
males  
Total 49 50 63 65 77 87  
adults+c  
alves  
Adjustme  
nts  
for  
human  
caused  
mortalit  
y  
Cows -1 -1 0 -1 0 0  
Female 0 0 0 0 0 0  
calves  
Bulls -1 0 0 -1 0 0  
Male 0 0 0 0 0 0  
calves  
Adjusted 25 25 27 31 36 44  
cows  
Adjusted 3 2 7 6 11 3  
female  
calves  
Adjusted 28 27 34 37 47 47  
females  
Adjusted 15 19 18 23 23 32  
bulls  
Adjusted 4 3 11 3 7 8  
male  
calves  
Adjusted 19 22 29 26 30 40  
males  
Adjusted 47 49 63 63 77 87  
total  
adults+c  
alves  
cows (%) 62 58 60 57 61 58  
Bulls:10 62 73 67 75 64 73  
0  
cows  
calves 14 10 29 14 23 13  
(%)

Column names: YEAR, 2014-15, 2015-16, 2016-17, 2017-18, 2018-19, 2019-20

``` r
# 5 intervals survival & lambda 
table2 <- readxl::read_excel("KS_for_r.xlsx",   sheet="5_lambdas")
t2<-hux(table2)
t2<-t2 %>%
  set_bold(1, everywhere, TRUE) %>%
  set_font_size(12)  %>%
  set_bottom_border(1,everywhere, TRUE)   
#start/stop sending output to txt and redirects back to console
#  sink("Table 2.txt")
  t2
```

YEAR 2015 to 2016 to 2017 to 2018 to 2019 to  
2016 2017 2018 2019 2020  
───────────────────────────────────────────────────────────────────────────────
Female 92.9  100    94.1  97.3  93.6   
survival  
Male 100    81.8  82.8  88.5  107     
survival  
Adult 95.7  91.8  88.9  93.7  98.7   
survival  
Lambda 1.06 1.29 1.03 1.22 1.13

Column names: YEAR, 2015 to 2016, 2016 to 2017, 2017 to 2018, 2018 to
2019, 2019 to 2020

## Lambdas

``` r
 delta <- readxl::read_excel("KS_for_r.xlsx",  sheet="popdata")
 d1<-c("n","year","herd","method")
  d2<-delta[d1]
   d2$logn<-log(d2$n)
   
#KS mean lambda census 
     d3<- subset(d2, herd=="KS" & method=="aerial")
#       ggplot(d3, aes(x=year, y=logn))+
#        geom_point(size = 3) +
#         geom_smooth(method=lm)
  l1<-lm(data=d3, logn~year)
    s1<-coef(l1)         #extract coefficient
     s2<-s1[2]           #assign it to s2
       slks1<-exp(s2)      #antilog
#cat("\n Kennedy Siding 2007-2014 census lambda mean slope",slks1,"\n")

#KS mean lambda RM pretreatment 
# slope of  log(n) vs yr
# alternatively mean  of        
       d3<- subset(d2, herd=="KS" & method=="RM")
#ggplot(d3, aes(x=year, y=logn))+
#geom_point(size = 3) +
#geom_smooth(method=lm)
l1<-lm(data=d3, logn~year)
s1<-coef(l1)         #extract coefficient
s2<-s1[2]           #assign it to s2
slks2<-exp(s2)      #antilog
slks2
```

    ##     year 
    ## 0.912073

``` r
#cat("\n Kennedy Siding 2007-2016 RM lambda mean slope",slks2,"\n")

#Q mean lambda census 
    d3<- subset(d2, herd=="Q" & method=="aerial" & year<2017)
#      ggplot(d3, aes(x=year, y=logn))+
#       geom_point(size = 3)  +
#        geom_smooth(method=lm)
  l1<-lm(data=d3, logn~year)
    s1<-coef(l1)         #extract coefficient
     s2<-s1[2]           #assign it to s2
       slq1<-exp(s2)      #antilog
#cat("\n Quintette 2008-2016 census lambda mean slope",slq1,"\n")

#Q mean lambda  pretreatment 
d3<- subset(d2, herd=="Q" & method=="RM" & year<2017)
#ggplot(d3, aes(x=year, y=logn))+
#  geom_point(size = 3)  +
#  geom_smooth(method=lm)
l1<-lm(data=d3, logn~year)
s1<-coef(l1)         #extract coefficient
s2<-s1[2]           #assign it to s2
slq2<-exp(s2)      #antilog
#cat("\n Quintette 2008-2016 RM lambda mean slope",slq2,"\n")

cat("\n Kennedy Siding 2007-2014 census lambda mean slope",slks1,"\n") 
```

    ## 
    ##  Kennedy Siding 2007-2014 census lambda mean slope 0.8057751

``` r
cat("\n Kennedy Siding 2007-2016 RM lambda mean slope",slks2,"\n")
```

    ## 
    ##  Kennedy Siding 2007-2016 RM lambda mean slope 0.912073

``` r
cat("\n Quintette 2008-2016 census lambda mean slope",slq1,"\n")
```

    ## 
    ##  Quintette 2008-2016 census lambda mean slope 0.8868551

``` r
cat("\n Quintette 2008-2016 RM lambda mean slope",slq2,"\n")
```

    ## 
    ##  Quintette 2008-2016 RM lambda mean slope 0.9496056

``` r
#meanlambda 4 years each KS Q 
ks4 <- subset(delta, herd=="KS" & lambda_year>2016)
ks4$ksflambda<-as.numeric(ks4$lambda)
logf<-log(ks4$ksflambda)
# calculate geomean, sd, SE, CI
gmf<-geoMean(ks4$ksflambda, na.rm = FALSE)
sdlogf<-sd(logf)
#n=4, length is count/ sample size
n1<-length(logf)
selogf<-(sdlogf/(n1^.5))

q4 <- subset(delta, herd=="Q" & lambda_year > 2016 & method=="RM")
q4$qnflambda<-as.numeric(q4$lambda)
lognf=log(q4$qnflambda)
gmnf<-geoMean(q4$qnflambda, na.rm = FALSE)
sdlognf<-sd(lognf)
n2<-length(lognf)
selognf<-(sdlognf/(n2^.5))

 
cat("\n Kennedy Siding 2015-2019 lambda  \n mean     SE         n \n",gmf,selogf,n1,"\n")
```

    ## 
    ##  Kennedy Siding 2015-2019 lambda  
    ##  mean     SE         n 
    ##  1.163366 0.04882571 4

``` r
cat("\n Quintette 2016-2020 lambda  \n mean     SE        n \n",gmnf,selognf,n2,"\n")
```

    ## 
    ##  Quintette 2016-2020 lambda  
    ##  mean     SE        n 
    ##  1.080775 0.0405885 4

``` r
# compare lambdas one-sample t-test where mu=kennedy siding lambda
res<-t.test(q4$qnflambda, mu=1.163366, alternative="less")
res
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  q4$qnflambda
    ## t = -1.7856, df = 3, p-value = 0.08607
    ## alternative hypothesis: true mean is less than 1.163366
    ## 95 percent confidence interval:
    ##      -Inf 1.188767
    ## sample estimates:
    ## mean of x 
    ##   1.08348

``` r
cat("\n the probablility that the mean Quintette herd growth rate\
       is as high as the Kennedy Siding growth rate is ", res$p.value,"\n")
```

    ## 
    ##  the probablility that the mean Quintette herd growth rate
    ##        is as high as the Kennedy Siding growth rate is  0.08607313

## Population trends

``` r
# date from  excel file // worksheet
fig4 <- readxl::read_excel("KS_for_r.xlsx", 
                            sheet="popdata")
#KS and Q subsets
KSfig4 <- subset(fig4, herd=="KS")
#        fig4<- subset(fig, year>2008)
Qfig4 <- subset(fig4, herd=="Q")
          

# KS plot
plot4m<-ggplot(KSfig4, aes(x = year, y = n, fill = treatment)) +  
        geom_point(size = 4, shape = 21) +    xlim(2006, 2021)+ ylim(40, 200)+
        xlab("YEAR") + ylab("NUMBER OF CARIBOU") + 
        scale_fill_manual(breaks=c("no","feed","feedreduce","model"),
        name="TREATMENTS", values = c('green','#7570B3', "#7570B3", "white"),
        labels=c("NO MANAGEMENT","FEED","FEED + WOLF REDUCTION", "model"))+
#        theme(panel.grid.major=element_blank())+
  theme(panel.grid.major = element_line(colour="#f0f0f0"))+
        theme(panel.grid.minor=element_blank())+
        theme(panel.background=element_rect(color='black', fill="white"))+
        theme(legend.position="none")+
        theme(axis.title = element_text(face = "bold",size = rel(1)))+
        guides(fill=guide_legend(nrow=2))+
        annotate(geom='text', x=2012.61, y=140, 
                 label='KENNEDY SIDING HERD', colour='black')
# plot4m
# ggsave(plot4m, file='trend_graphmodel_KS.png',h=4, w=6, units='in', dpi=300) 
  
# Q plot
plot4mQ<-ggplot(Qfig4, aes(x = year, y = n, fill = treatment)) +  
  geom_point(size = 4, shape = 21) +    xlim(2006, 2021)+ ylim(40,200)+
  xlab("YEAR") + ylab("NUMBER OF CARIBOU") + 
  scale_fill_manual(breaks=c("no","reduce","model"),
      name="TREATMENTS", values = c('green', "green", "white"),
      labels=c("NO MANAGEMENT","WOLF REDUCTION", "model"))+
#  theme(panel.grid.major=element_blank())+
 theme(panel.grid.major = element_line(colour="#f0f0f0"))+
   theme(panel.grid.minor=element_blank())+
  theme(panel.background=element_rect(color='black', fill="white"))+
  theme(legend.position="none")+
  theme(axis.title = element_text(face = "bold",size = rel(1)))+
  guides(fill=guide_legend(nrow=2))+
  annotate(geom='text', x=2012.6, y=180, 
         label='QUINTETTE HERD', colour='black')
# plot4mQ
# ggsave(plot4mQ, file='trend_graphmodel_Q.png',h=4, w=6, units='in', dpi=300) 
 

#grid.arrange(plot4m,plot4mQ)
library(gtable)
library(grid)
g2<-ggplotGrob(plot4m)
g3<-ggplotGrob(plot4mQ)
g<-rbind(g2,g3, size="first")
g$widths<-unit.pmax(g2$widths, g3$widths)
grid.newpage()
grid.draw(g)
```

![](c:/Users/DCH/Dropbox/!!r/KS_caribou/README_V4_github_files/figure-gfm/trend_plots-1.png)<!-- -->

``` r
#ggsave(g, file='trend_grid_arrange.png',h=4, w=6, units='in', dpi=300) 
```

## lambda means by treament violin plot

``` r
delta <- readxl::read_excel("KS_for_r.xlsx",  sheet="popdata")
d1<-c("treatment2","lambda_year","herd","method","lambda")
d2<-delta[d1] 
d3<-subset(d2, treatment2=="KS_model"| treatment2=="Q_model"|
               treatment2=="KS_feedreduce"|treatment2=="Q_reduce"|
               treatment2=="KS_feed")  

d3$treatment2<-factor(d3$treatment2, levels=c("Q_model",
                                    "KS_model","KS_feed","KS_feedreduce","Q_reduce"))
violin2m<-ggplot(d3, aes(factor(treatment2), lambda))+
  geom_violin(adjust=.8, aes(fill=herd))+ geom_dotplot(binaxis='y',binwidth=.01,
                                                       dotsize=1.5)+
  annotate("segment", x=.7,xend=1.3, y=0.95,yend=0.95, colour="red", size=2)+
  annotate("segment", x=1.7,xend=2.3, y=0.91,yend=0.91, colour="red", size=2)+
  annotate("segment", x=3.7,xend=4.3, y=1.16,yend=1.16, colour="red", size=2)+
  annotate("segment", x=4.7,xend=5.3, y=1.08,yend=1.08, colour="red", size=2)+
  scale_x_discrete(labels=c("NONE", "NONE","FEED", "FEED + WR","WR"))+
scale_fill_manual(values=c("#7570B3", "#E69F00"),labels=c("KENNEDY SIDING", 
                            "QUINTETTE"))+
xlab("TREATMENT") +  ylab("LAMBDA") +
theme(legend.position = 'bottom',
      panel.border=element_blank(),
      panel.grid.major = element_line(size=0.5),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(colour = NA))
violin2m   
```

![](c:/Users/DCH/Dropbox/!!r/KS_caribou/README_V4_github_files/figure-gfm/lambda_means-1.png)<!-- -->

## Lambda plot against wolf density and feeding

``` r
#lambdas vs wolf densties by year

fig6<- readxl::read_excel("KS_for_r.xlsx",   sheet="popdata")

fig6$treatment3
```

    ##  [1] NA     NA     NA     NA     NA     NA     NA     NA     NA     NA    
    ## [11] NA     NA     NA     "FEED" "FEED" "FEED" "FEED" "FEED" NA     NA    
    ## [21] NA     NA     NA     NA     NA     NA     NA     NA     NA     NA    
    ## [31] "NO"   "NO"   NA     "NO"   NA     "NO"   NA     "NO"   "NO"   "NO"  
    ## [41] NA     NA

``` r
fig6$wolfden
```

    ##  [1]   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA 10.8  0.5
    ## [16]  2.0  0.3  1.0   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## [31]  6.5  0.3   NA  1.1   NA  1.1   NA  1.3 12.6 12.6   NA   NA

``` r
plot2<-ggplot(fig6, aes(x = wolfden, y = lambda, fill = treatment3, colour = treatment3)) +  
        geom_point(size = 3, shape = 21) + 
        scale_color_manual(values = c('#7570B3', '#E69F00')) + 
        scale_fill_manual(values = c('#7570B3', '#E69F00')) +
     #   xlab(expression(WOLF DENSITY (wolves/1000 km^2))) +
        xlab(expression(WOLF~DENSITY~(wolves1000km^-2)))+
        ylab("LAMBDA") +
        geom_smooth(se=FALSE, method=lm, formula=y~log(x))+
        annotate(geom='text', x=6, y=1.09, 
                 label='WITH FEEDING', colour='black')+
        annotate(geom='text', x=7.5, y=.97, 
                 label='WITHOUT FEEDING', colour='black')+
        theme(panel.grid.major=element_line(size=0.5))+
        theme(panel.grid.minor=element_blank())+
        theme(legend.position='none')
 
 
 plot2
```

![](c:/Users/DCH/Dropbox/!!r/KS_caribou/README_V4_github_files/figure-gfm/wolf%20density%20effect-1.png)<!-- -->

``` r
#plot2
# ggsave(plot2, file='wolf_density_lambda.png',h=4, w=6, units='in', dpi=300) 
```

## survival and recruitment

``` r
surv<- readxl::read_excel("KS_for_r.xlsx",   sheet="popdata")
# compare survivals 4 years KS Q
#  k<-subset(surv, treatment2=="KS_feedreduce" | treatment2=="Q_reduce")

k1<-subset(surv, treatment2=="KS_feedreduce")
kn1<-length(k1$survival)
km1<-mean(k1$survival)
ksd1<-sd(k1$survival)
kse1<-ksd1/(kn1^.5)
cat("\n mean cow survival Kennedy Siding 2015-16 to 2019-20 \n mean    SE        n \n", km1,kse1,kn1)
```

    ## 
    ##  mean cow survival Kennedy Siding 2015-16 to 2019-20 
    ##  mean    SE        n 
    ##  0.9625 0.01494713 4

``` r
#pretreatment surv recruitment
k2<-subset(surv, treatment2=="KS_model")
k2$s<-(1-k2$M)
kn2<-length(k2$s)
km2<-mean(k2$s)
ksd2<-sd(k2$s)
kse2<-ksd2/(kn2^.5)
cat("\n mean cow survival Kennedy Siding 2007-08 to 2013-14 \n mean    SE        n \n", km2,kse2,kn2)
```

    ## 
    ##  mean cow survival Kennedy Siding 2007-08 to 2013-14 
    ##  mean    SE        n 
    ##  0.8242857 0.04258645 7

``` r
#post treatment  female recruitment
k3<-subset(surv, treatment2=="KS_feedreduce")
k3$R
```

    ## [1] 0.20588235 0.15789474 0.23404255 0.06382979

``` r
kn3<-length(k3$R)
km3<-mean(k3$R)
ksd3<-sd(k3$R)
kse3<-ksd3/(kn3^.5)
cat("\n mean female recruitemnt Kennedy Siding 2016-17 to 2019-20 \n mean    SE        n \n", km3,kse3,kn3)
```

    ## 
    ##  mean female recruitemnt Kennedy Siding 2016-17 to 2019-20 
    ##  mean    SE        n 
    ##  0.1654124 0.03733124 4

``` r
#pretreatment  female recruitment
k4<-subset(surv, treatment2=="KS_model")
kn4<-length(k4$R)
km4<-mean(k4$R)
ksd4<-sd(k4$R)
kse4<-ksd4/(kn4^.5)
cat("\n mean female recruitemnt Kennedy Siding 2007-08 to 2013-14 \n mean    SE        n \n", km4,kse4,kn4)
```

    ## 
    ##  mean female recruitemnt Kennedy Siding 2007-08 to 2013-14 
    ##  mean    SE        n 
    ##  0.08713714 0.01313538 7

``` r
q<-subset(surv, treatment2=="Q_reduce")
n2<-length(q$survival)
m2<-mean(q$survival)
sd2<-sd(q$survival)
se2<-sd2/(n2^.5)
cat("\n mean cow survival Quintette 2015-16 to 2019-20 \n mean    SE        n \n", m2,se2,n2)
```

    ## 
    ##  mean cow survival Quintette 2015-16 to 2019-20 
    ##  mean    SE        n 
    ##  0.88925 0.04671434 4

``` r
t.test(k1$survival, q$survival, paired=TRUE) 
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  k1$survival and q$survival
    ## t = 1.4205, df = 3, p-value = 0.2506
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.09085634  0.23735634
    ## sample estimates:
    ## mean of the differences 
    ##                 0.07325

## Between year calf presence

``` r
alt<- readxl::read_excel("KS_for_r.xlsx",   sheet="btwyr_calf_status")
t1<-colSums(alt)
cat("\n frequency of yr-to-yr calf presence 
    NN no calf to no calf
    NC no calf to calf
    CC calf to calf
    CN calf to no calf
    NN NC CC CN
    \n", t1)
```

    ## 
    ##  frequency of yr-to-yr calf presence 
    ##     NN no calf to no calf
    ##     NC no calf to calf
    ##     CC calf to calf
    ##     CN calf to no calf
    ##     NN NC CC CN
    ##     
    ##  13 17 6 18

``` r
round<-prop.table(t1) 
cat("\n proportion of yr-to-yr calf presence")
```

    ## 
    ##  proportion of yr-to-yr calf presence

``` r
signif(round, digits=3)
```

    ##    NN    NC    CC    CN 
    ## 0.241 0.315 0.111 0.333

## observed vs expected frequency of calf presence

## in alternate years

``` r
# 35 cows with a calf present in only one of 2 years 
# 11 cows with a calf present in 2 of 2 years
# 8 cows with no calf present in 2 years
# 36% accompanied by a calf on average

observed<-c(35, 19)
expected<-c(.48, .52)
chisq.test(x = observed,
           p = expected)
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  observed
    ## X-squared = 6.1169, df = 1, p-value = 0.01339

## Caribou weight plot & analysis

``` r
wt1 <- readxl::read_excel("KS_for_r.xlsx",   sheet="wts")
wt1$yr<-as.factor(wt1$year)
agesex.labs<-c("FEMALES","MALES","FEMALE CALVES","MALE CALVES")
names(agesex.labs)<-c("af","am","cf","cm")
wt<-subset(wt1, agesex=="af"|agesex=="am")
blank_data<- data.frame(yr=c("2015","2015","2016","2016","2017","2017","2018","2018"),
                        agesex=c("af","af","am","am"), 
                        wt=c(90,260,90,260))
wtplot1<-ggplot(wt, aes(x=yr, y=wt))+
  geom_boxplot(outlier.shape=NA, coef=0, aes(fill=yr), show.legend=FALSE)+ 
        facet_grid(.~agesex, labeller=labeller(agesex=agesex.labs))+
        geom_jitter(shape = 19, size=1,
              color = "black",
              position = position_jitter(width = 0.21)) +
        ylab(label="WEIGHT (kg)")+
        xlab(label="")
#wtplot1
# ggsave(wtplot1, file="weight_by_adultsex.png", h=4, w=6, units='in', dpi=300)
 
#now calves

wt2 <- readxl::read_excel("KS_for_r.xlsx",   sheet="wts")
wt2$yr<-as.factor(wt2$year)
agesex.labs<-c("FEMALES","MALES","FEMALE CALVES","MALE CALVES")
names(agesex.labs)<-c("af","am","cf","cm")
wt<-subset(wt2, agesex=="cf"|agesex=="cm")
blank_data<- data.frame(yr=c("2015","2015","2016","2016","2017","2017","2018","2018"),
                        agesex=c("af","af","am","am"), 
                        wt=c(50,90,50,90))
wtplot2<-ggplot(wt, aes(x=yr, y=wt))+
  geom_boxplot(outlier.shape=NA, coef=0, aes(fill=yr), show.legend=FALSE)+ 
  facet_grid(.~agesex, labeller=labeller(agesex=agesex.labs))+
  geom_jitter(shape = 19, size=1,
              color = "black",
              position = position_jitter(width = 0.21)) +
  ylab(label="WEIGHT (kg)")+
  xlab(label="YEAR")
#wtplot2
 # ggsave(wtplot2, file="weight_by_calvessex.png", h=4, w=6, units='in', dpi=300)


#grid.arrange(wtplot1, wtplot2)
library(gtable)
library(grid)
wts1<-ggplotGrob(wtplot1)
wts2<-ggplotGrob(wtplot2) 
wts<-rbind(wts1,wts2, size="first")
wts$widths<-unit.pmax(wts1$widths, wts2$widths)
grid.newpage()
grid.draw(wts)
```

![](c:/Users/DCH/Dropbox/!!r/KS_caribou/README_V4_github_files/figure-gfm/weights-1.png)<!-- -->

``` r
# ggsave(wts, file="weight_by_agesexyear.png", h=4, w=6, units='in', dpi=300)

 
 # means and SEs
# sink("weight_analyses2.txt") 
 sumdata1<-ddply(wt, c("agesex"), summarise,
                mean=mean(wt, na.rm=TRUE),
                N=sum(!is.na(wt)),
                sd=sd(wt, na.rm=TRUE),
                SE=sd/sqrt(N) )
 sumdata1
```

``` 
                 ┌───────────────────────────────────┐
                 │ agesex   mean     N     sd     SE │
                 ├───────────────────────────────────┤
                 │ cf       70.5    14   7.2    1.92 │
                 │ cm       81.1    18   6.87   1.62 │
                 └───────────────────────────────────┘
```

Column names: agesex, mean, N, sd, SE

``` r
 wt2<-subset(wt, agesex=="af" & calf!="NA") 
 sumdata2<-ddply(wt2, c("agesex","VCI"), summarise,
                 mean=mean(wt, na.rm=TRUE),
                 N=sum(!is.na(wt)),
                 sd=sd(wt, na.rm=TRUE),
                 SE=sd/sqrt(N) )
 sumdata2
```

``` 
              ┌────────────────────────────────────────┐
              │ year    wt   agesex   VCI   calf   yr  │
              └────────────────────────────────────────┘
```

Column names: year, wt, agesex, VCI, calf, yr

``` r
 sumdata3<-ddply(wt2, c("calf"), summarise,
                 mean=mean(wt, na.rm=TRUE),
                 N=sum(!is.na(wt)),
                 sd=sd(wt, na.rm=TRUE),
                 SE=sd/sqrt(N) )
 sumdata3
```

``` 
              ┌────────────────────────────────────────┐
              │ year    wt   agesex   VCI   calf   yr  │
              └────────────────────────────────────────┘
```

Column names: year, wt, agesex, VCI, calf, yr

``` r
#sink() 


#sink("weight_analyses2n.txt") 
#calculate means and sd

 cat("\n","AGESEX N MEAN WEIGHT sd","\n")
```

    ## 
    ##  AGESEX N MEAN WEIGHT sd

``` r
 group_by(wt, agesex) %>% 
 dplyr::summarise(count=n(), mean=mean(wt, na.rm=TRUE),sd=sd(wt, na.rm=TRUE))
```

``` 
                   ┌──────────────────────────────┐
                   │ agesex   count   mean     sd │
                   ├──────────────────────────────┤
                   │ cf          14   70.5   7.2  │
                   │ cm          18   81.1   6.87 │
                   └──────────────────────────────┘
```

Column names: agesex, count, mean, sd

``` r
 #cow wts by calf
cat("\n","COW'S CALF STATUS N MEAN WEIGHT sd","\n")
```

    ## 
    ##  COW'S CALF STATUS N MEAN WEIGHT sd

``` r
cowwts<-subset(wt, agesex=="af")
#with calf
group_by(cowwts, calf) %>% 
  dplyr::summarise(count=n(), mean=mean(wt, na.rm=TRUE),sd=sd(wt, na.rm=TRUE))
```

``` 
                    ┌────────────────────────────┐
                    │ calf   count   mean     sd │
                    └────────────────────────────┘
```

Column names: calf, count, mean, sd

``` r
#compare calves wts by sex
cat("\n","CALF SEX N MEAN WEIGHT sd","\n")
```

    ## 
    ##  CALF SEX N MEAN WEIGHT sd

``` r
calves<-subset(wt, agesex=="cm"|agesex== "cf")
group_by(calves, agesex) %>% 
  dplyr::summarise(count=n(), mean=mean(wt, na.rm=TRUE))
```

``` 
                      ┌────────────────────────┐
                      │ agesex   count    mean │
                      ├────────────────────────┤
                      │ cf          14    70.5 │
                      │ cm          18    81.1 │
                      └────────────────────────┘
```

Column names: agesex, count, mean

``` r
res.aov<-aov(wt~agesex, data=calves)
summary(res.aov)   
```

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## agesex       1  879.9   879.9   17.89 0.000203 ***
    ## Residuals   30 1475.9    49.2                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
cv<-ggplot(calves, aes(x=agesex, y=wt))+  
  geom_jitter(width=.05) +
  stat_summary(fun=mean, fun.min=mean, fun.max=mean, geom="crossbar", width=.5, colour="red")
#  cv
# ggsave(cv, file="weight_of_calves.png", h=4, w=6, units='in', dpi=300)
# compare cow rib wts - use cowwts from above
cat("\n","COW'S RIBS STATUS N MEAN WEIGHT sd","\n")
```

    ## 
    ##  COW'S RIBS STATUS N MEAN WEIGHT sd

``` r
group_by(cowwts, VCI) %>% 
  dplyr::summarise(count=n(), mean=mean(wt, na.rm=TRUE),sd=sd(wt, na.rm=TRUE))
```

``` 
                    ┌────────────────────────────┐
                    │ VCI    count   mean     sd │
                    └────────────────────────────┘
```

Column names: VCI, count, mean, sd

``` r
#rib.aov<-aov(wt~VCI, data=cowwts)
#summary(rib.aov)
#ribplot<-ggplot(cowwts, aes(x=VCI, y=wt))+  
#  geom_jitter(width=.05) +
#  stat_summary(fun=mean, fun.min=mean, fun.max=mean, geom="crossbar", width=.5, #colour="red")+
  stat_summary(fun.data = mean_se, geom ="errorbar")
```

    ## geom_errorbar: na.rm = FALSE, orientation = NA
    ## stat_summary: fun.data = function (x, mult = 1) 
    ## {
    ##     x <- stats::na.omit(x)
    ##     se <- mult * sqrt(stats::var(x)/length(x))
    ##     mean <- mean(x)
    ##     new_data_frame(list(y = mean, ymin = mean - se, ymax = mean + se), n = 1)
    ## }, fun = NULL, fun.max = NULL, fun.min = NULL, fun.args = list(), na.rm = FALSE, orientation = NA
    ## position_identity

``` r
#cat("\n","COW'S RIBS STATUS N MEAN WEIGHT sd","\n")
#group_by(cowwts, VCI) %>% 
#  dplyr::summarise(count=n(), mean=mean(wt, na.rm=TRUE),sd=sd(wt, na.rm=TRUE))
#rib.aov<-aov(wt~VCI, data=cowwts)
#cat("\n","COW'S RIBS STATUS N MEAN WEIGHT sd","\n")
#group_by(cowwts, VCI) %>% 
#  dplyr::summarise(count=n(), mean=mean(wt, na.rm=TRUE),sd=sd(wt, na.rm=TRUE))
#res.aov<-aov(wt~VCI, data=cowwts)
#summary(rib.aov)
#ribplot<-ggplot(cowwts, aes(x=VCI, y=wt))+  
#  geom_jitter(width=.05) +
#  stat_summary(fun=mean, fun.min=mean, fun.max=mean, geom="crossbar", width=.5, #colour="red")+
#  stat_summary(fun.data = mean_se, geom ="errorbar")
#
#
#  ribplot
#  ggsave(ribplot, file="weight_by_ribs.png", h=4, w=6, units='in', dpi=300)
#sink() 
```

## weight by ribs violin plot

``` r
wt <- readxl::read_excel("KS_for_r.xlsx",   sheet="wts") 
wt$yr<-as.factor(wt$year)
agesex.labs<-c("COWS","BULLS","FEMALE CALVES","MALE CALVES")
names(agesex.labs)<-c("af","am","cf","cm")
blank_data<- data.frame(yr=c("2015","2015","2016","2016","2017","2017","2018","2018"),
                        agesex=c("af","af","am","am","cf","cf","cm","cm"), 
   wt=c(90,150,90,260,50,95,50,95))
 
cowwts<-subset(wt, agesex=="af")
 
cat("\n","COW'S RIBS STATUS N MEAN WEIGHT sd","\n")
```

    ## 
    ##  COW'S RIBS STATUS N MEAN WEIGHT sd

``` r
group_by(cowwts, VCI) %>% 
  dplyr::summarise(count=n(), mean=mean(wt, na.rm=TRUE),sd=sd(wt, na.rm=TRUE))
```

``` 
                    ┌────────────────────────────┐
                    │ VCI    count   mean     sd │
                    ├────────────────────────────┤
                    │ no        45    123   12.9 │
                    │ ribs      21    115   12.5 │
                    └────────────────────────────┘
```

Column names: VCI, count, mean, sd

``` r
rib.aov<-aov(wt~VCI, data=cowwts)
 

 
# rib violin plot
theme_Publication <- function(base_size=14, base_family="helvetica")
{
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold",
                                     size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(colour = NA),
 #      axis.title = element_text(face = "bold",size = rel(1)),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(), 
           axis.line = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_line(colour="#f0f0f0"),
           panel.grid.minor = element_blank(),
 #          legend.key = element_rect(colour = NA),
           legend.position = "none",
 #          legend.direction = "horizontal",
 #          legend.key.size= unit(0.2, "cm"),
 #          legend.margin = unit(0, "cm"),
 #          legend.title = element_text(face="italic"),
           plot.margin=unit(c(10,5,5,5),"mm"),
           strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
           strip.text = element_text(face="bold")
   ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

violin<-ggplot(cowwts, aes(factor(VCI), wt))+
  geom_violin(adjust=.8, aes(fill=VCI))+ geom_jitter(width=.1)+
  stat_summary(fun=mean, fun.min=mean, fun.max=mean, geom="crossbar", width=.5, colour="red")+
  xlab("FEMALE CONDITION INDEX") +  ylab("WEIGHT (Kg)") +
  scale_x_discrete(labels=c("NO VISIBLE RIBS", "WITH VISIBLE RIBS"))

violin+scale_colour_Publication()+ theme_Publication()
```

![](c:/Users/DCH/Dropbox/!!r/KS_caribou/README_V4_github_files/figure-gfm/weight_by_ribs-1.png)<!-- -->

``` r
#violin2<-violin+theme(legend.position="none")
#violin2
```

## Food provided

``` r
avail<- readxl::read_excel("KS_for_r.xlsx",   sheet="consumption")
hux(avail)
```

``` 
year       2014       2015       2016       2017       2018       2019  
food       NA     106        106        106        76         76        
days                                                                    
caribou    NA     4.45e+03   6.06e+03   6.21e+03   7.95e+03   8.38e+03  
days                                                                    
weight     NA     6e+03      5.36e+03   7.06e+03   7.06e+03   9.8e+03   
                                                                        
bags       NA     300        268        353        353        490       
                                                                        
kg/c/d     NA     1.35       0.884      1.14       0.888      1.17      
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
```

Column names: year, 2014, 2015, 2016, 2017, 2018, 2019

## Activity by hour of the day plot

``` r
   activity2014 <- readxl::read_excel("activity_6yr.xlsx", sheet="2014")
   activity2015 <- readxl::read_excel("activity_6yr.xlsx", sheet="2015")
   activity2016 <- readxl::read_excel("activity_6yr.xlsx", sheet="2016")
   activity2017 <- readxl::read_excel("activity_6yr.xlsx", sheet="2017")
   activity2018 <- readxl::read_excel("activity_6yr.xlsx", sheet="2018")
   activity2019 <- readxl::read_excel("activity_6yr.xlsx", sheet="2019")

      activity<-rbind(activity2014, activity2015, activity2016, activity2017, activity2018, activity2019)

# take out any data and missing and create new newdata2 
# newdata2 <- subset(activity, monthly1<5, select=date:monthly1)
newdata2<-activity  

#geom_density_ridges(mapping = NULL, data = NULL,
#                    stat = "density_ridges", position = "points_sina",
#                    panel_scaling = TRUE, na.rm = FALSE, show.legend = NA,
#                    inherit.aes = TRUE)

newdata2$year <- factor(newdata2$year)
# levels(newdata2$year)

# finally...  change scale and bandwidth to get best image 
ggplot(newdata2, aes(x=time, y=year, fill=year)) + 
  geom_density_ridges(scale=4, bandwidth=.5, from = 0, to =23, 
    show.legend = FALSE) +
  labs(x ='Hour of the Day' , y ='Activity Index') + 
  theme_ridges(grid=FALSE, center_axis_labels = TRUE) +
  scale_x_continuous(expand = c(0.01,0))
```

![](c:/Users/DCH/Dropbox/!!r/KS_caribou/README_V4_github_files/figure-gfm/activity-1.png)<!-- -->

``` r
 #a
 #  ggsave(a, file='activity_6yr.png',h=4, w=6, units='in', dpi=300) 
```

## frequencies by hour

``` r
#frequency by hour
 df<-subset(newdata2, select=-year) 
  df$cc<-as.integer(df$time) 
  ccc<-count(df, var="df$cc")
  names(ccc)[1]<-"Hour"
 x<-hux(ccc)
x
```

``` 
                           Hour            n  
                           df$cc      871621  
```

Column names: Hour, n

## SI Snow effect on early arrival 2018

``` r
snow<- readxl::read_excel("KS_for_r.xlsx",   sheet="pt_345")
snow$snowtotal<-snow$swe3+snow$swe4+snow$swe5 
SWE1<-subset(snow, yday>244 & yday<285 & year>2014) 

a<-ggplot(SWE1, aes(x=yday, y=pctarr, color=year))+
  geom_line(aes(color=factor(year)), size=1.5) +
  xlab("")+ ylab("Proportion Arrived")
#a

s<-ggplot(SWE1, aes(x=yday, y=snowtotal, color=year))+
    geom_line(aes(color=factor(year)), size=1.5)+ 
    xlab("Julian Day")+ ylab("SWE")
#s


# ggsave(snowgrid, file='snow_arrival_graph.png',h=4, w=6, units='in', dpi=300) 
 
 
snowgrid<-grid.arrange(a,s) 
```

![](c:/Users/DCH/Dropbox/!!r/KS_caribou/README_V4_github_files/figure-gfm/snow_effect-1.png)<!-- -->

We indexed the presence of snow on the ground from the snow water
equivalent (SWE) measures obtained from ORNL DAAC (Daymet Software
Version 3.0; Daymet Data Version 3.0 Thornton; P.E.; M.M. Thornton; B.W.
Mayer; Y. Wei; R. Devarakonda; R.S. Vose; and R.B. Cook. 2016. Daymet:
Daily Surface Weather Data on a 1-km Grid for North America; Version 3.
ORNL DAAC; Oak Ridge; Tennessee; USA.
<http://dx.doi.org/10.3334/ORNLDAAC/1328>). We extracted the daily SWE
for 3 mountain locations where there were radioed cows in mid-September
for 2015 through 2019. When we plotted summed SWE for those sites from 1
September (day 244) through 12 October (day 285) by year (excluding 2014
when caribou was arrival was not recorded before mid-October), the 2018
snow on day 255 \[even though it disappeared by day 269\] correlated
with increase in percent arriving caribou over the next 10 days relative
to the other 4 years. On day 255 the percent of caribou that had arrived
was about 5% in all years, but in the 10 days following the 2018
snowfall, the percent of caribou that had arrived in 2018 increased to
44% but to only 13% in the other years. That 31% difference persisted
through mid-October.

Top graph: Proportion arriving by year mid-September to mid-October; day
285

Bottom graph: early 2018 snow in blue correlates with subsequent
increase in 2018 arrivals over the next 10 days relative to arrivals in
the other 4 years. The difference between the blue line and the mean of
other lines shows that in 2018, relatively more caribou started to
arrive at Kennedy Siding after snow on day 256 until day 267 when
caribou started to arrive at the same rare in other years.
