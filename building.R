dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# 生成文件夹
# usethis::create_package(paste(dir,"/chatgpt", sep=''))

devtools::build(paste(dir,"/chatgpt", sep=''),
                path=paste(dir,"/pkg", sep=''))

system(sprintf("tar -C %s -xf %s",
               shQuote(paste(dir,"/pkg", sep='')),
               shQuote(paste(dir,"/pkg/chatgpt_0.1.0.tar.gz", sep=''))
               )
       )
devtools::install(paste(dir,"/pkg/chatgpt", sep=''))
library("chatgpt")
