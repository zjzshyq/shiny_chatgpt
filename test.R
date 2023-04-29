library(Rcpp)

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
sourceCpp("input.cpp")

cpp_sys_prompt()
cpp_api_key()
cpp_user_prompt()

