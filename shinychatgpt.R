library(shiny)
library(shinyalert)

library(httr)
library(stringr)
library(R6)

# R6 style Class
# 可以再把几个参数如api_key从public变为private属性，具体见R6的课件
# 将这个类单独做成一个R包
ChatGPT <- R6Class("ChatGPT",
                   public = list(
                     api_key = "",
                     model_name = 'gpt-3.5-turbo',
                     temperature = 0.7,
                     max_length = 512,
                     sysprompt = '',
                     prompt = '',
                     
                     # Initialize
                     initialize = function(data) {
                       self$api_key <- data$api_key
                       self$model_name <- data$model_name
                       self$temperature <- data$temperature
                       self$max_length <- data$max_length
                       self$sysprompt <- data$sysprompt
                       self$prompt <- data$user_message
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
                           stop = '\n',
                           temperature = self$temperature,
                           max_tokens = self$max_length
                         )
                       )
                       return(str_trim(content(response)$choices[[1]]$message$content))
                     }
                   )
)
# 以上做成一个R包

# to set the UI
ui <- fluidPage(
  div(
    titlePanel("ChatGPT Clone with Shiny"),
    style = "color: white; background-color: #3d3f4e"
  ),
  sidebarLayout(
    sidebarPanel(
      h3("Welcome to the OpenAI Playground - ChatGPT Clone with Shiny!"),
      p("This application allows you to chat with an OpenAI GPT model and explore its capabilities. Simply use your own API keys with adding below."),
      textInput("api_key", "API Key", "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"),
      tags$p("Find your own OpenAI API:",
             tags$a(href = "https://platform.openai.com/account/api-keys",
                    target="_blank", "https://platform.openai.com/account/api-keys")
      ),
      tags$hr(),
      selectInput("model_name", "Model Name",
                  choices = c("gpt-4", "gpt-4-0314", "gpt-3.5-turbo-0301", "gpt-3.5-turbo"),
                  selected = "gpt-3.5-turbo"),
      tags$hr(),
      sliderInput("temperature", "Temperature", min = 0.1, max = 1.0, value = 0.7, step = 0.1),
      sliderInput("max_length", "Maximum Length", min = 1, max = 2048, value = 512, step = 1),
      tags$hr(),
      textAreaInput(inputId = "sysprompt", label = "SYSTEM PROMPT",height = "200px", placeholder = "You are a helpful assistant."),
      tags$hr(),
      tags$div(
        style="text-align:center; margin-top: 15px; color: white; background-color: #FFFFFF",
        a(href="https://github.com/zjzshyq",
          target="_blank",
          img(src="https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png",
              height="30px"),
          "View my Repo on Github"
        )
      ),
      style = "background-color: #1a1b1f; color: white"
    ),

    mainPanel(
      tags$style(type = "text/css", ".shiny-output-error {visibility: hidden;}"),
      tags$style(type = "text/css", ".shiny-output-error:before {content: ' Check your inputs or API key';}"),
      tags$style(type = "text/css", "label {font-weight: bold;}"),
      fluidRow(# 回答列表
        column(12,
               tags$h3("Chat History"),
               tags$hr(),
               uiOutput("chat_history"),
               tags$hr())
      ),
      fluidRow(# 提问
        column(11,textAreaInput(inputId = "user_message",
                                placeholder = "Enter your message:",
                                label="USER PROMPT",
                                width = "100%")),
      ),
      fluidRow(#send 按钮
        column(1,
               actionButton("send_message",
                            "Send",
                            icon = icon("play")
                            )
               )
      ),
      style = "background-color: #00A67E")
  ),
  style = "background-color: #3d3f4e"
)


server <- function(input, output, session) { # advanced function
  chat_data <- reactiveVal(data.frame())

  observeEvent(input$send_message, {
    gpt <- ChatGPT$new(data = input)

    # 获取输入
    if (input$user_message != "") {
      new_data <- data.frame(source = 'User',
                             message = gpt$prompt,
                             stringsAsFactors = FALSE)
      chat_data(rbind(chat_data(), new_data))

      # 获取gpt的返回
      gpt_res = gpt$call_gpt_api()
      print(gpt$prompt)
      print(gpt_res)

      # 在这里可以增加一个tryCatch，当检查到gpt_res返回异常时，执行下面这段
      if (length(gpt_res) == 0) {
        # 彈出提示框
        shinyalert(
          title = "ERROR!",
          text = "Please set right API Key or model.\nReload the page and try again.",
          type = "error"
        )
      }
      
      if (!is.null(gpt_res)) {
        gpt_data <- data.frame(source = "ChatGPT",
                               message = gpt_res,
                               stringsAsFactors = FALSE)
        chat_data(rbind(chat_data(), gpt_data))
      }
      updateTextInput(session, "user_message", value = "")
    }
  })

  # 输出列表
  output$chat_history <- renderUI({
    chatBox <- vector("list", nrow(chat_data())) # vectorization
    
    for(i in seq_len(nrow(chat_data()))) {
      source <- chat_data()[i, "source"]
      message <- chat_data()[i, "message"]
      class_name <- ifelse(chat_data()[i, "source"] == "User",
                           "alert alert-secondar",
                           "alert alert-success")
      chatBox[[i]] <- tags$div(class = class_name,
                               HTML(paste0("<b>", source, ":</b> ", message)))
    }
    do.call(tagList, chatBox)
  })
}

shinyApp(ui = ui, server = server)
