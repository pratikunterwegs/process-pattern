#### code to make simulation batch files ####

library(glue)
library(stringr)
library(tidyr)
library(dplyr)

runs <- str_pad(seq(2, 20), width = 3, pad = "0")
strategy <- c(
  "exploit",
  "interf",
  "random"
)

regrowth <- c(0.001, 0.0033, 0.005, 0.01, 0.02, 0.03)

mask <- "{0,0,0}"

# make data
data_param <- crossing(replicate = runs, strategy, regrowth) %>% 
  mutate(
    outdir = glue('{strategy}_{replicate}'),
    agents.forage = as.numeric(strategy == "exploit"),
    agents.obligate = as.numeric(strategy == "obligate")
  )


# prepare lines
lines <- glue_data(
  data_param, 
  'kleptomove config=../settings/config.ini \\
   landscape.item_growth={regrowth} \\
   agents.forage={agents.forage} \\
   agents.obligate={agents.obligate} \\
   agents.handling_time=5 outdir=\\
   ../../data/sim_{strategy}_rep_{replicate}_gro_{regrowth}'
)

# mask all env cues for random movers
lines[data_param$strategy == "random"] <- 
  glue('{lines[data_param$strategy == "random"]} \\
       agents.input_mask={{0,0,0}')

# write to file
library(readr)
date = Sys.time() %>% str_replace_all(" |:", "_")
write_lines(lines,
            file = glue("kleptomove-sim/bin/Release/runs_{date}.bat"))
