
![](img/pltsesim_logo_annimation.gif)

Simulate **P**robabilistic **L**ong-**t**erm **E**ffects in Models with Temporal
Dependence

Christopher Gandrud and Laron K. Williams

[![Build Status](https://travis-ci.org/christophergandrud/pltesim.svg?branch=master)](https://travis-ci.org/christophergandrud/pltesim)

# Examples 

These examples replicate Figure 1 in [Williams (2016)](http://pan.oxfordjournals.org/content/24/2/243).


```r
library(pltesim)
library(ggplot2)

data('negative')

# BTSCS set the data
neg_set <- btscs(df = negative, event = 'y', t_var = 'tim',
                 cs_unit = 'group', pad_ts = FALSE)


# Create temporal dependence variables
neg_set$t <- neg_set$spell + 1

m1 <- glm(y ~ x + t + I(t^2) + I(t^3),
          family = binomial(link = 'logit'),
          data = neg_set)

# Fit counterfactuals
counterfactual <- data.frame(x = 0.5)

# Permanent
sim1 <- plte_builder(obj = m1, obj_tvar = 't',
                     cf = counterfactual, t_points = c(13, 25))

plte_plot(sim1) + ggtitle('Permanent')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
# One-time
sim2 <- plte_builder(obj = m1, obj_tvar = 't', cf_duration = 'one-time',
                     cf = counterfactual, t_points = c(13, 25))

plte_plot(sim2) + ggtitle('One-time')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
# Temporary
sim3 <- plte_builder(obj = m1, obj_tvar = 't', cf_duration = 4,
                     cf = counterfactual, t_points = c(13, 25))

plte_plot(sim3) + ggtitle('Temporary')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```r
# Multiple events, permanent counter factual
sim4 <- plte_builder(obj = m1, obj_tvar = 't',
                     cf = counterfactual, t_points = c(13, 20, 25))

plte_plot(sim4) + ggtitle('Permanent, Multiple Events')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)
