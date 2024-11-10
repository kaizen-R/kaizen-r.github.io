## Simple exercise: Distinguish ZIP (or Ciphered) text from normal/plain text
## Trying to use that to explain "Shannon's information entropy"
library(ggplot2)
library(gridExtra)

library(WikipediR) # Get Wiki data
#library(text2vec) # NLP stuff
#library(udpipe) # NLP stuff
#library(word2vec) # NLP stuff (embeddings)
library(tm)

make_freq_df <- function(filename) {
    test1 <- file(filename, open="rb", raw = TRUE)
    t1_bytes <- t1_chars <- c()
    while(TRUE) {
        temp <- readBin(test1, what = "raw")
        if(length(temp) == 0) break;
        t1_bytes <- c(t1_bytes, temp)
        t1_chars <- c(t1_chars, rawToChar(temp))
    }
    
    t1_df <- data.frame(sort(table(as.character.hexmode(t1_bytes)), decreasing = TRUE))
    t1_df$char <- names(sort(table(t1_chars), decreasing = TRUE))
    names(t1_df) <- c("x", "probs", "char")
    # Instead of counts (table output), make it probability:
    t1_df$probs <- t1_df$probs/sum(t1_df$probs)
    # Alternative could have been:
    #t1_df$probs <- as.numeric(prop.table(sort(table(t1_chars), decreasing = TRUE)))
    
    t1_df
}

## A manual function, to explain "traditional ML" is not magical
clean_text_set <- function(t_input) {
    ## Input is Wikipedia "clean text":
    t_input <- tolower(t_input$text$`*`)
    
    ## Put it in vector:
    t_input <- strsplit(t_input, "\n")[[1]]
    
    ## HTML stuff needs cleaning...
    strings_to_replace <- c("\\[", "\\]", "\\{", "\\}", "'", "\\:", "\\&",
                            "\\#", ";", "http(s)?:[\\/a-z\\.]+", "<\\!--.*?>",
                            "<.*?>", "\\.[0-9a-z\\:\\-]+", " [a-z]{1,2}(\\,)? ",
                            " [0-9]+", " div ", " ifont", " dt", "  ")
    for(i in 1:length(strings_to_replace))
        t_input <- gsub(strings_to_replace[i], " ", t_input)
    
    ## Remove english "Stopwords":
    t_input <- sapply(t_input, function(x) {
        t_str <- strsplit(x, " ", fixed=TRUE)
        vapply(t_str, function(y) 
            paste(y[!tolower(y) %in% tm::stopwords("english")], 
                  collapse = " "), character(1))
    })
    
    ## Duplicates are useless
    t_input <- unique(t_input)
    t_input <- t_input[nchar(t_input) > 4] ## Too small a word? Out.
    
    t_input
}

## Simple wrapper
my_page_content <- function(keywords) {
    page_content(language = "en",
                 project = "wikipedia",
                 page_name = keywords,
                 as_wikitext = FALSE,
                 clean_response = TRUE) |>
        clean_text_set()
}

## Explicitly for explanation:
compare_clear_zip <- function(df_row_num, wiki_pages_df) {
    temp_text <- my_page_content(wiki_pages_df[df_row_num, "wiki_name"])
    temp_text <- temp_text[1:wiki_pages_df[df_row_num, "keep_paragraphs"]]
    
    writeLines(temp_text, paste0("./posts/2024-11-10_entropy_of_zip/", wiki_pages_df[df_row_num, "var_name"], ".txt"))
    zip(paste0("./posts/2024-11-10_entropy_of_zip/", wiki_pages_df[df_row_num, "var_name"], ".zip"), 
        paste0("./posts/2024-11-10_entropy_of_zip/", wiki_pages_df[df_row_num, "var_name"], ".txt"))
    
    
    t1_df <- make_freq_df(paste0("./posts/2024-11-10_entropy_of_zip/", wiki_pages_df[df_row_num, "var_name"], ".txt"))
    t2_df <- make_freq_df(paste0("./posts/2024-11-10_entropy_of_zip/", wiki_pages_df[df_row_num, "var_name"], ".zip"))
    print(head(t1_df))
    print(head(t2_df))
    
    print(paste("Entropy Plain text:", -sum(t1_df$probs*log2(t1_df$probs))))
    print(paste("Entropy Zip text:", -sum(t2_df$probs*log2(t2_df$probs))))
    
    g1 <- ggplot(t1_df, aes(x = x, y = probs)) +
        geom_point() + ylim(0, max(t1_df$probs)) + theme_bw() +
        labs(title=paste(wiki_pages_df[df_row_num, "var_name"], "- clear text. Mean:", round(mean(t1_df$probs), 2), "SD=", round(sd(t1_df$probs), 2)))
    g2 <- ggplot(t2_df, aes(x = x, y = probs)) + 
        geom_point() + ylim(0, max(t1_df$probs)) + theme_bw() +
        labs(title=paste("zip text. Mean:", round(mean(t2_df$probs), 2), "SD=", round(sd(t2_df$probs), 2)))
    
    list(g1, g2)
}

wiki_pages_df <- data.frame(
    var_name = c("firewall_wiki", "switch_wiki", "router_wiki",
                 "virus_wiki", "hacker_wiki", "computer_wiki",
                 "network_card_wiki", "cpu_wiki"),
    wiki_name = c("firewall (computing)", "network switch", "router (computing)",
                  "computer virus", "hacker", "computer",
                  "network interface controller", "central processing unit"),
    keep_paragraphs = c(82, 96, 65,
                        89, 116, 101,
                        56, 144)
    )

firewall_wiki <- compare_clear_zip(1, wiki_pages_df)
hacker_wiki <- compare_clear_zip(5, wiki_pages_df)
switch_wiki <- compare_clear_zip(2, wiki_pages_df)
grid.arrange(firewall_wiki[[1]], firewall_wiki[[2]],
             hacker_wiki[[1]], hacker_wiki[[2]],
             switch_wiki[[1]], switch_wiki[[2]],
             ncol=2)
print("Entropy is the measure of uncertainty of a variable. The more uncertain it is, the higher the entropy is.")
