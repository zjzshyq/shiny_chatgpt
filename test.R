library(httr)
library(R6)
library(shiny)

# engines:
# text-davinci-002: This is the most capable GPT-based model available from OpenAI, and it can perform a wide variety of tasks, including chat, translation, summarization, and more.
# text-curie-001: This is a smaller version of the Davinci model that is optimized for generating human-like responses in conversational settings.
# text-babbage-001: This model is similar in size to the Curie model, but it is optimized for generating informative responses to questions.
# text-ada-001: This is another small model that is designed for generating natural-sounding text in a conversational setting.

ChatGPT <- R6Class("ChatGPT",
                   public = list(
                     api_key = "",
                     model_name = 'gpt-3.5-turbo',
                     temperature = 0.5,
                     max_length = 512,
                     sysprompt = '',
                     prompt = '',
                     
                     # Initialize
                     initialize = function(api_key, model_name, temperature, max_length, sysprompt, user_message) {
                       self$api_key <- api_key
                       self$model_name <- model_name
                       self$temperature <- temperature
                       self$max_length <- max_length
                       self$sysprompt <- sysprompt
                       self$prompt <- user_message
                     },
                     
                     # Call OpenAI API
                     call_gpt_api = function() {
                       response <- httr::POST(
                         url = "https://api.openai.com/v1/chat/completions",
                         add_headers(Authorization = paste("Bearer", self$api_key)),
                         content_type("application/json"),
                         encode = "json",
                         body = list(
                           model = self$model_name,
                           messages = list(
                             list(role = "user", content = self$prompt),
                             list(role = "system", content = self$sysprompt)
                           ),
                           temperature = self$temperature,
                           max_tokens = self$max_length
                         )
                       )
                       content
                       return(str_trim(content(response)$choices[[1]]$message$content))
                     },
                     
                     call_gpt_api2 = function() {
                       print(self$model_name)
                       response <- POST(
                         url = "https://api.openai.com/v1/engines/chat/generate",
                         add_headers(Authorization = paste("Bearer", self$api_key)),
                         body = list(
                           model = self$model_name,
                           #engine = 'text-davinci-003',
                            prompt = 'hello',
                           temperature = 0.7,
                           max_tokens = 100
                         ),
                         encode = "json",
                         content_type("application/json")
                       )
                       
                       return(content(response, "text"))
                     }
                     
                     
                   )
)

api_key<-'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'
model_name<-'gpt-3.5-turbo'
temperature<-0.5
max_length<-512
sysprompt<-''
user_message<-'hello'
gpt <- ChatGPT$new(api_key, model_name, temperature, max_length, sysprompt, user_message)
print(gpt$call_gpt_api2())



