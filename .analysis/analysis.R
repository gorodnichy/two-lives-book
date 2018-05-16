library(data.table)

library(ggplot2)
#library(ggthemes); #library(RColorBrewer)
theme_set(theme_minimal())
#   p_theme =  theme_bw(); #   # theme_minimal(); theme_economist(); theme_economist_white();# g+p_theme

library(tidyverse)
library(tidytext)


# 1.0 Read .Rmd file ####

if (F) { # Way #1
  library(readtext)
  dt <- readtext("../01-amor.Rmd") %>% data.table()
  strEntireText <- dt[1,1]
}



if (F){ # automated splitting in sections####

}

## A df where each line is a row in the rmd file.

raw <- data_frame(text = read_lines("../01-Two-Lives-book.Rmd"))

## We don't want to mark R comments as sections.
detect_codeblocks <- function(text) {
  blocks <- text %>%
    str_detect("```") %>%
    cumsum()

  blocks %% 2 != 0
}


#extract information, such as headers, using regex patterns.
df <-
  raw %>%
  mutate(
    code_block = detect_codeblocks(text),
    section = text %>%
      str_match("^# .*") %>%
      str_remove("^#+ +"),
    section = ifelse(code_block, NA, section),
    subsection = text %>%
      str_match("^## .*") %>%
      str_remove("^#+ +"),
    subsection = ifelse(code_block, NA, subsection)
  ) %>%
  fill(section, subsection)

#to glue the text together within sections/subsections,
## then just group by them and flatten the text.

dtSongs <- df %>%
  group_by(section, subsection) %>%
  slice(-1) %>%                           # remove the header
  summarize(
    text = text %>%
      str_flatten(" ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  data.table()


for(i in 1:nrow(dtSongs)){
  cat(i)
  print(dtSongs[i]$section)
  print(dtSongs[i]$subsection)
  print(dtSongs[i]$text)
  dd.getKey()
}

dtSongs[c(4,5,8,13, 15,17,19,21) ]$text %>% str_c() %>% str_wrap()

dtSongs[c(4,5,8,13, 15,17,19,21) ]$subsection

# tidytext approach #### #####################################


wordsTechnical <- c("blockquote", "pre", "cite", "жобим", "стихи",
                    "диньджи", "ча", "de", "beber",
                    "agua", "aqua", "camara", "camara",
                    "что", #"как",  "когда"
                    "и", "в", "с", "у","о", "o","б","к", "д",
                    "по", "под",
                     "на", "из", "за", "же", "что", "б")

filename <- "../01-Two-Lives-book.Rmd"


# 2.0 Extract words ####


getWords<- function(filename) {

  # text <- read_lines(filename)
  # text_df <- data_frame(line = 1:length(text), text = text)

  text_df <- data_frame(text = read_lines(filename))

  dt_tidy_books <- text_df %>%
    unnest_tokens(word, text) %>% data.table()



  #dt_tidy_books <- dt_tidy_books[str_length(word)>1]
  #  dt_tidy_books <- dt_tidy_books[str_length(word)<12]
  dt_tidy_books <- dt_tidy_books[!(word %in% wordsTechnical)]

  #  dt_tidy_books[, .N, word][N>1][order(str_length(word))][1:100]
  dt_tidy_books[, .N, word][N>1][order(N)] %>% print()



  # 2.2. Wordclouds ####


  library(wordcloud)

  dt_tidy_books %>%
    #  anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, min.freq=100,
                   random.order=F, random.color=F, ordered.colors=T,
                   rot.per=0.0
    ))


  # 2.1.  words stats ####

  dt_tidy_books %>%
    count(word, sort = TRUE) %>%
    filter(n > 200) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()




  return(dt_tidy_books)
}



tidy_books <- getWords("01-amor-a.Rmd")
tidy_books2 <- getWords("02-wave-a.Rmd")

# 2.0 Extract words ####



# 2.1.  comparative plots ####


frequency <- bind_rows(mutate(tidy_books, author = "amour"),
                       mutate(tidy_books2, author = "ocean")) %>%
#  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `amour`:`amour`)

library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `ocean`, color = abs(`ocean` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "«Ocean» (About Life)", x = "«Grand amor» (About Love)" )







# 4.0  Using API read from web ####

library(pageviews)
