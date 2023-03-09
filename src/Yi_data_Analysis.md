Yi Lui Data Analysis: Transfer Efficiency
================
MFK
25/08/2021

# Import data

This imports the data and skim the columns using skimr

## Fix date

Some of the dates are in American format, some are in British format.
Given that the betteries were changed on day 23, this has an effect on
the level of raw intensity

# Plot the individual intensity/powder area vs contact number for all the surfaces

We think the date has an important contribution to the levels of
intensity because the batteries were changed at some point in the UV
light.

## Plot of powder Area

## Plot of Loading Weight vs contact number

We fit a quadratic regression to the loading weight vs contact number
for each date. Visually it appears that the loading weight increases
over time but it seems that this is not true.

We now look at the regression lines quantitatively

``` r
fit_lm <- lm(data=dY,loading_weight~contact_number+surface)

broom::tidy(fit_lm)
```

    ## # A tibble: 4 × 5
    ##   term           estimate std.error statistic  p.value
    ##   <chr>             <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)       67.2       4.33    15.5   5.82e-52
    ## 2 contact_number     9.98      1.12     8.93  8.25e-19
    ## 3 surfaceGlass       3.62      3.87     0.935 3.50e- 1
    ## 4 surfacePlastic    12.3       3.87     3.18  1.52e- 3

``` r
# fit_lmer <- lmer(data=dY,loading_weight~contact_number+surface+(1|id))
# summary(fit_lmer)
```

## Remove last four dates

``` r
dY <- dY %>% 
  filter(date<"23/07/2019")
```

# Plot contacts of first 20

``` r
# Individual powder area vs contact number and loading weight 
dY %>% 
  filter(surface=="Plastic" ) %>% 
  filter(id<21) %>% 
  ggplot()+
  geom_point(aes(x=contact_number,y=raw_int_d/(powder_area),colour=gloves),size=5)+
  #geom_smooth(aes(x=contact_number,y=raw_int_d/(powder_area),colour=gloves),method="lm",se=FALSE)+
  scale_y_continuous(trans="log10",limits=c(3e4,4e5 ))+
  ylab("Intensity/Powder Area")+
  xlab("Contact number")+
  facet_wrap(~id)+
  scale_colour_brewer(palette = "Set1")+
  theme_bw()+
  labs(color = "Gloves")+
  #font_size(14)
  theme(text = element_text(size=14),
  axis.text.x=element_text(size=14),
  axis.text.y=element_text(size=14))->a
a
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](Yi_data_Analysis_files/figure-gfm/individual%20plots-1.png)<!-- -->

``` r
  ggsave(a,filename = "images/individual_relative_int_vs_contact_number.png",dpi=300, device="png",width = 15, height = 10, units = "in",bg="white")
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

``` r
# Raw int violin plots faceted by surface
dY %>% 
  # filter(surface=="Plastic" ) %>% 
  # group_by(surface,contact_number,gloves) %>% 
  # summarise(Mean=mean(powder_area),SD=sd(powder_area))
  ggplot()+
  geom_violin(aes(x=as.factor(contact_number),y=raw_int_d,fill=gloves),draw_quantiles = c(0.25,0.5,0.95))+
  scale_y_continuous(trans = "log10")+
  scale_fill_brewer(palette = "Set1")+
  facet_wrap(~surface,scales="free",nrow=3)+
  ylab("Raw intensity")+
  xlab("Contact number")+
  labs(fill = "Gloves")+
  hrbrthemes::theme_ipsum()
```

![](Yi_data_Analysis_files/figure-gfm/individual%20plots-2.png)<!-- -->

### Plots of plastic per person

``` r
#Plot log10(raw_int_d) vs contact_number for each ID and colour by gloves
dY%>%
filter(surface=="Plastic") %>%
filter(id>20 & id<=40) %>%
ggplot()+
geom_point(aes(x=contact_number,y=raw_int_d,colour=gloves),size=4)+
scale_colour_brewer(palette="Set1")+
scale_y_continuous(trans="log10")+
facet_wrap(~id)+
xlab("Contact number")+
ylab("Intensity units")+
theme_bw()+
labs(color='Gloves')+
theme(text = element_text(size=14),axis.text.x=element_text(size=14))  ->a

#save ggplot figure
# ggsave(a,"images/intensity_for_each_ID.png",device="png",units=c("cm"),width=30)
ggsave(a,filename = "images/intensity_for_each_ID_plastic_21_40.png",dpi=300, device="png",width = 10, height = 10, units = "in")


# Create a scatter plot of the intensity for each ID and colour by gloves, facet by surface

dY%>%
ggplot()+
geom_jitter(aes(x=contact_number,y=raw_int_d/powder_area,colour=date),size=4,alpha=0.4)+
scale_colour_brewer(palette="Set1")+
# geom_smooth(method="lm",se=TRUE,aes(x=contact_number,y=raw_int_d/powder_area,colour=gloves))+
facet_grid(gloves~surface)+
xlab("Contact number")+
ylab("Intensity units")+
scale_y_continuous(trans = "log10")+
theme_bw()+
labs(color='Date')+
theme(text = element_text(size=14),axis.text.x=element_text(size=14))  ->a

ggsave(a,filename = "images/aggregated_intensity.png",dpi=300, device="png",width = 12, height = 10, units = "in")
```

## Relative Intensity units / Loading Weight

``` r
dY%>%
  mutate(experimentID=paste0(id,surface,gloves)) %>% 
  group_by(experimentID) %>% 
  mutate(rel_int_d=raw_int_d/first(raw_int_d)) %>% 
  mutate(gloves=case_when(gloves=="Y"~glove_size_group,
                          gloves=="N"~"None")) %>% 
  filter(!is.na(gloves)) %>% 
  # drop_na() %>% 
  mutate(gloves=factor(gloves,levels=c("None","Small","Medium","Large"))) %>% 
  ggplot()+
  geom_violin(aes(x=as.factor(contact_number),y=rel_int_d/loading_weight,fill=surface),draw_quantiles = c(0.25,0.5,.75))+
  geom_smooth(aes(x=contact_number,y=rel_int_d/loading_weight),color="black",method="lm")+
  scale_fill_brewer(palette="Set1")+
  # geom_smooth(method="lm",se=TRUE,aes(x=contact_number,y=raw_int_d/powder_area,colour=gloves))+
  facet_grid(surface~gloves)+
  xlab("Contact number")+
  ylab("Relative Intensity units / Loading Weight")+
  scale_y_continuous(trans = "log10",labels = scales::label_number())+
  theme_bw()+
  labs(color='Surface')+
  theme(text = element_text(size=14),axis.text.x=element_text(size=14)) 
```

    ## Warning: Removed 6 rows containing non-finite values (stat_ydensity).

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

![](Yi_data_Analysis_files/figure-gfm/Relative%20Intensity%20units%20/%20Loading%20Weight-1.png)<!-- -->

# LMER4 model fit

Here we fit a Linear Mixed Effects Model using LMER4 to the data.

## Violin plots of glove effect Y/N by surface

``` r
dY%>%
  filter(date<"23/07/2019") %>% 
  mutate(experimentID=paste0(id,surface,gloves)) %>% 
  group_by(experimentID) %>% 
  mutate(rel_int_d=raw_int_d/first(raw_int_d)) %>% 
  # mutate(gloves=case_when(gloves=="Y"~glove_size_group,
  #                         gloves=="N"~"None")) %>% 
  filter(!is.na(gloves)) %>% 
  filter(contact_number=="1") %>% 
  # drop_na() %>% 
  # mutate(gloves=factor(gloves,levels=c("None","Small","Medium","Large"))) %>% 
  ggplot()+
  geom_violin(aes(x=as.factor(contact_number),y=raw_int_d/powder_area/loading_weight,fill=gloves),draw_quantiles = c(0.25,0.5,.75))+
  # geom_smooth(aes(x=contact_number,y=raw_int_d/loading_weight),color="black",method="lm")+
  scale_fill_brewer(palette="Set1")+
  # geom_smooth(method="lm",se=TRUE,aes(x=contact_number,y=raw_int_d/powder_area,colour=gloves))+
  facet_grid(~surface)+
  xlab("Contact number")+
  ylab("Raw Intensity units / Loading Weight")+
  scale_y_continuous(trans = "log10",labels = scales::label_number())+
  labs(fill="Gloves worn")+
  theme_bw()+
  labs(color='Surface')+
  theme(text = element_text(size=14),axis.text.x=element_text(size=14)) ->p

#add_pval(ggplot_obj = p,pairs = list(c(1,2)))
```

## Gradient of concentration

Let’s look at the gradient for each participant. We can normalise every
raw intensity value by first contact.

``` r
dY%>%
  mutate(experimentID=paste0(id,surface,gloves)) %>% 
  group_by(experimentID) %>% 
  mutate(rel_int_d=raw_int_d/first(raw_int_d)) %>% 
  ggplot()+
  geom_jitter(aes(x=contact_number,y=rel_int_d),size=1.2,alpha=0.1,width=0.05,height=0)+
  geom_smooth(aes(x=contact_number,y=rel_int_d,colour=surface),method="lm")+
  scale_colour_brewer(palette="Set1")+
  # geom_smooth(method="lm",se=TRUE,aes(x=contact_number,y=raw_int_d/powder_area,colour=gloves))+
  facet_grid(~gloves)+
  xlab("Contact number")+
  ylab("Intensity units")+
  scale_y_continuous(trans = "log10")+
  theme_bw()+
  labs(color='Surface')+
  theme(text = element_text(size=14),axis.text.x=element_text(size=14)) 
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Yi_data_Analysis_files/figure-gfm/gradient%20of%20concentration:%20raw_int_d/first(raw_int_d)-1.png)<!-- -->

## Relative intensity By glove size

Same as above but by glove size (Small, medium and large, no gloves)

``` r
dY%>%
  mutate(experimentID=paste0(id,surface,gloves)) %>% 
  group_by(experimentID) %>% 
  mutate(rel_int_d=raw_int_d/first(raw_int_d)) %>% 
  mutate(gloves=case_when(gloves=="Y"~glove_size_group,
                          gloves=="N"~"None")) %>% 
  filter(!is.na(gloves)) %>% 
  # drop_na() %>% 
  mutate(gloves=factor(gloves,levels=c("None","Small","Medium","Large"))) %>% 
  ggplot()+
  geom_jitter(aes(x=contact_number,y=rel_int_d/loading_weight),size=1.2,alpha=0.1,width=0.05,height=0)+
  geom_smooth(aes(x=contact_number,y=rel_int_d/loading_weight,colour=surface),method="lm")+
  scale_colour_brewer(palette="Set1")+
  # geom_smooth(method="lm",se=TRUE,aes(x=contact_number,y=raw_int_d/powder_area,colour=gloves))+
  facet_grid(~gloves)+
  xlab("Contact number")+
  ylab("Relative Intensity units / Loading Weight")+
  scale_y_continuous(trans = "log10")+
  theme_bw()+
  labs(color='Surface')+
  theme(text = element_text(size=14),axis.text.x=element_text(size=14)) 
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](Yi_data_Analysis_files/figure-gfm/rel%20intensity%20vs%20glove%20size-1.png)<!-- -->

# LMER4 model fit

Here we fit a Linear Mixed Effects Model using LMER4 to the data.

``` r
#Here we fit a LMER4 model from LMER4 to the data, with raw_ind_d as the dependent variable
#We
model_fit_g<-lme4::lmer(log10(raw_int_d)~powder_area + contact_number + gloves + surface + (powder_area+contact_number+gloves+surface | id),
                  data = dY,
                  weights=log10(raw_int_d))
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(model_fit_g)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: log10(raw_int_d) ~ powder_area + contact_number + gloves + surface +  
    ##     (powder_area + contact_number + gloves + surface | id)
    ##    Data: dY
    ## Weights: log10(raw_int_d)
    ## 
    ## REML criterion at convergence: -2585.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4969 -0.3552  0.1088  0.4437  3.9393 
    ## 
    ## Random effects:
    ##  Groups   Name           Variance  Std.Dev. Corr                         
    ##  id       (Intercept)    5.959e-02 0.244116                              
    ##           powder_area    1.586e-05 0.003982 -0.97                        
    ##           contact_number 6.645e-04 0.025778 -0.84  0.82                  
    ##           glovesY        7.439e-03 0.086250 -0.50  0.44  0.69            
    ##           surfaceGlass   3.974e-03 0.063039 -0.61  0.49  0.72  0.16      
    ##           surfacePlastic 5.277e-03 0.072644 -0.02 -0.14 -0.05 -0.07  0.28
    ##  Residual                6.253e-02 0.250058                              
    ## Number of obs: 1740, groups:  id, 58
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)     6.341818   0.034452 184.074
    ## powder_area     0.008315   0.000547  15.201
    ## contact_number -0.027268   0.003842  -7.098
    ## glovesY        -0.046738   0.012622  -3.703
    ## surfaceGlass   -0.025567   0.010200  -2.506
    ## surfacePlastic -0.074637   0.011243  -6.639
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) pwdr_r cntct_ glovsY srfcGl
    ## powder_area -0.950                            
    ## contct_nmbr -0.808  0.737                     
    ## glovesY     -0.474  0.405  0.560              
    ## surfaceGlss -0.510  0.390  0.517  0.109       
    ## surfacPlstc -0.080 -0.094 -0.028 -0.047  0.341
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## unable to evaluate scaled gradient
    ## Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

``` r
plot(model_fit_g)
```

![](Yi_data_Analysis_files/figure-gfm/LMER4%20raw_int_d-1.png)<!-- -->

# BRMS model fit

Here we fit a Bayesian Regression Model using BRMS to the data.
