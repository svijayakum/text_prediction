library(shiny); library(tm); library(plyr); library(stringr); library(ngram); 
#library(qdap); library(ngram);library(rJava); library(openNLP); ; library(RWeka)
#load ngram data:
two_gram_df = read.csv("two_gram_df.csv")
three_gram_df = read.csv("three_gram_df.csv")
four_gram_df = read.csv("four_gram_df.csv")

ui = fluidPage(
  titlePanel("Predict the next word!"),
  sidebarLayout(
    sidebarPanel (
      textInput("Input_Phrase", "Enter word of phrase here"),
      submitButton(text = "Predict", icon = NULL)
    ),
    mainPanel(
      textOutput("Entered_phrase"),
      h4("Predicted word:"),
      verbatimTextOutput("first"),
      h4('Alternative choices:'),
      verbatimTextOutput("second"),
      verbatimTextOutput("third")
    )
  )
)

word_pred = function(input,n){
  
  input_text = input %>% tolower() %>% removePunctuation() %>% removeNumbers() %>% stripWhitespace()
  
  pred_freq = function(input_text,ngram){
    word_loc = grep(input_text,ngram[,1]) # extracts location of input_text
    w_1 = gsub(paste0("^",input_text,"[s ]"),"",ngram[word_loc,1]) # extracts ngram that contains input_text
    w_1 = gsub(paste0(input_text,"(.*)"),"NA",w_1) # replaces input_text with NA if it comes after another word.
    w_1 = cbind(ngram[word_loc,],w_1) # adds the matching n_grams and frequencies to w_1
    w_1 = w_1[grep("[^NA]$",w_1[,3]),] # removes all the n_gram rows where the input_text occurs at the end
  }
  
  
  if (wordcount(input_text) == 1){
    
    one_word = word(input_text,-1)
    w_1 = pred_freq(one_word,two_gram_df)
    
    
    first = paste("1st:", w_1[1,3])
    second = paste("2nd:", w_1[2,3])
    third = paste("3rd:", w_1[3,3])
    
    ifelse(missing(n),return(c(first,second,third)),
           ifelse(n=="1",return(first),
                  ifelse(n=="2",return(second),return(third))))
  }
  else if (wordcount(input_text) == 2){
    one_word = word(input_text,-1)
    w_1 = pred_freq(one_word,two_gram_df)
    w_1$score = w_1[,2]*0.4
    colnames(w_1) <- c("tok","Freq","wordpred","score")
    
    two_words = paste(word(input_text,-2:-1),collapse = " ")
    w_2 = pred_freq(two_words,three_gram_df)
    w_2$score = w_2[,2]
    colnames(w_2) <- c("tok","Freq","wordpred","score")
    
    final = rbind(w_1, w_2)
    
    first = paste("1st:", final[1,3])
    second = paste("2nd:", final[2,3])
    third = paste("3rd:", final[3,3])
    
    ifelse(missing(n),return(c(first,second,third)),
           ifelse(n=="1",return(first),
                  ifelse(n=="2",return(second),return(third))))
    
  }
  else if (wordcount(input_text) >= 3){
    one_word = word(input_text,-1)
    w_1 = pred_freq(one_word,two_gram_df)
    w_1$score = w_1[,2]*0.16
    colnames(w_1) = c("tok","Freq","wordpred","score")
    
    two_words = paste(word(input_text,-2:-1),collapse = " ")
    w_2 = pred_freq(two_words,three_gram_df)
    w_2$score = w_2[,2]*0.4
    colnames(w_2) = c("tok","Freq","wordpred","score")
    
    three_words = paste(word(input_text,-3:-1),collapse = " ")
    w_3 = pred_freq(three_words,four_gram_df)
    w_3$score = w_3[,2]
    colnames(w_3) = c("tok","Freq","wordpred","score")
    
    final = rbind(w_1, w_2, w_3)
    
    first = paste("1st:", final[1,3])
    second = paste("2nd:", final[2,3])
    third = paste("3rd:", final[3,3])
    
    ifelse(missing(n),return(c(first,second,third)),
           ifelse(n=="1",return(first),
                  ifelse(n=="2",return(second),return(third))))
    
  }
  
}

First <- function(Input_Phrase) { word_pred(Input_Phrase,1) }
Second <- function(Input_Phrase) { word_pred(Input_Phrase,2) }
Third <- function(Input_Phrase) { word_pred(Input_Phrase,3) }

server = function(input, output){
  output$first <- renderText({First(input$Input_Phrase)})
  output$second <- renderText({Second(input$Input_Phrase)})
  output$third <- renderText({Third(input$Input_Phrase)})
  }
  


shinyApp(ui=ui,server = server)