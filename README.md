# scanpatterns
R package for analysis of scan patterns

## Installation

```{r}
install.packages("devtools")
devtools::install_github("fidadoma/scanpatterns")
```

## Usage
Let's use example dataset from the package. It is a portion of dataset from Multiple Object Tracking task. First, we need to set global values

```{r}
library(scanpatterns)
scanpatterns.set.parameters(list("arenamin"=-15,"arenamax"=15,"tmax"=6.0,"spaces-dir"="cache/spaces","eye-dir"="cache/eye"))
data(motData) 
eye1 <- df.eye %>% filter(id == 1, trial == 7) %>% as.eye()
plot(eye1)
```

## Scan pattern comparison

There are several metrics for scan pattern comparison in the package, namely
* Levenshtein distance
* Fréchet distance
* Correlation distance
* Mean distance
* Median distance

We could compute distances between scan patterns. Here we select two scan patterns that were measured in repeated presentation of the same trial.

```{r}
eye2 <- df.eye %>% filter(id == 1, trial == 13) %>% as.eye()
eye3 <- df.eye %>% filter(id == 1, trial == 34) %>% as.eye()
correlation_distance(eye2, eye3)
```

## Use with your data
Here we show an example how to load data with [eyelinker](https://github.com/dahtah/eyelinker) package and convert to objects in this package. See the package description for installation instructions

```{r}
library(eyelinker)
library(scanpatterns)

ppd <- 60 # we are guessing in this case

fpath <- system.file("extdata/mono500.asc.gz",package="eyelinker")
dat <- read.asc(fpath)
df <- dat$raw %>% 
  group_by(block) %>% 
  mutate(id = 1, trial = block, time = time - min(time) + 1, xp = xp / ppd, yp = yp / ppd) %>% 
  ungroup() %>% 
  select(id, trial, xp, yp, time)

arena <- abs(round(c(range(df$xp), range(df$yp))/ppd*4)/4)

scanpatterns.set.parameters(list("arenamin"=-max(arena),"arenamax"=max(arena),"tmax"=max(df$time)/1000,"max-time"=-1))

df <- df %>% group_by(id, trial) %>% do(eye = as.eye(., xname = "xp", yname = "yp", timename = "time"))
plot(df$eye[[1]])

correlation_distance(df$eye[[1]], df$eye[[2]])
```


