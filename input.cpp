#include <Rcpp.h>

// [[Rcpp::export]]
std::string cpp_sys_prompt() {
    std::string message = "You are a helpful assistant.";
    return message;
}

// [[Rcpp::export]]
std::string cpp_api_key(){
    std::string message = "Please enter the API-Key";
    return message;
}

// [[Rcpp::export]]
std::string cpp_user_prompt(){
    std::string message = "Hello!";
    return message;
}
