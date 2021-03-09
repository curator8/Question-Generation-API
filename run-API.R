#! /usr/bin/env Rscript

library(plumber)
pr("set-relation.R") %>%
  pr_run(port = 3157, host = "127.0.0.1")

#The above code runs the plubmer api defined in the "set-relation.R" file as a service. 