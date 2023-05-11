library(httr)
library(R6)

#' @title ChatGPT
#' @export
ChatGPT <- R6Class("ChatGPT",
                   private = list(api_key = ""),
                   public = list(
                     model_name = 'gpt-3.5-turbo',
                     temperature = 0.7,
                     max_length = 512,
                     sysprompt = '',
                     prompt = '',

                     # Initialize
                     initialize = function(data) {
                       self$model_name <- data$model_name
                       self$temperature <- data$temperature
                       self$max_length <- data$max_length
                       self$sysprompt <- data$sysprompt
                       self$prompt <- data$user_message

                       private$api_key <- data$api_key
                       invisible(self)
                     },

                     set_api_key = function(k){
                       private$api_key = k
                     },
                     get_api_key = function(){
                       return(private$api_key)
                     },

                     # Call OpenAI API
                     call_gpt_api = function() {
                     tryCatch({
                       response <- httr::POST(
                         url = "https://api.openai.com/v1/chat/completions",
                         add_headers(Authorization = paste("Bearer", private$api_key)),
                         encode = "json",
                         body = list(
                           model = self$model_name,
                           messages = list(
                             list(role = "user", content = self$prompt),
                             list(role = "system", content = self$sysprompt)
                           ),
                           stop = '\n',
                           temperature = self$temperature,
                           max_tokens = self$max_length
                         )
                       )
                       return(str_trim(content(response)$choices[[1]]$message$content))
                      },error = function(e) {
                          cat("Error:", e$message, "\n")
                          return(NULL)
                      }
                     }
                   )
)
