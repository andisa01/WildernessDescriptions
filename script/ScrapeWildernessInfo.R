library(rvest) # For web scrapping
library(qdap) # For tagging parts of speech
library(tidytext) # Text mining 
library(hunspell) # For word stemming
library(quanteda) # Text analsysis
library(cowplot) # Composite figures
library(ggrepel) # For text labels in visualizations
library(wordcloud2) # Word cloud vizualizations
library(tidyverse) # For everything.

# I find it easier to install wordcloud2 from the repo:
# library(devtools)
# devtools::install_github("lchiffon/wordcloud2")

theme_set(
  theme_minimal() +
    theme(
      axis.line = element_line(
        size = 1,
        colour = "black",
        linetype = 1
      ),
      panel.grid.major = element_line(size = 0.1),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(hjust = 0),
      axis.title.y = element_text(hjust = 0),
      legend.position = "none"
    )
) # This alters the default ggplot design

### Web scraping ====
# I found the video tutorials by Dataslice [https://www.youtube.com/watch?v=v8Yh_4oE-Fs] very helpful. In those tutorials, the author employs a Chrome extension, SelectorGadget [https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb?hl=en] to isolate the page elements that you want to scrape. Unfortunately, Wilderness.net uses an extremely convoluted page structure without CSS. Thus, I could not find a way to isolate the Wilderness descriptions using CCS tags. Instead, I scrub all of the body text and they use [strsplit] to cleave out the portions of the text I am interested in keeping.
# Conveniently, each Wilderness Areas gets its own page on Wilderness.net and each of those pages are numbered consecutively in the page address. For example, South Baranof Wilderness is number 561: "wilderness.net/visit-wilderness/?ID=561". This means that we can simply loop through each page, scrub the text, and store it for further analysis.

# First, we need to set up an empty matrix that we will populate with the data we scrub from Wilderness.net
# From the text, we want to isolate and store the Wilderness name, the state that contains the most acerage, the year of designation, the federal agency (or agencies) charged with its management, and the description text.
Colnames <- c("Wild", "State", "Year", "Agency", "Descrip")
WildText <- matrix(nrow = 804, ncol = length(Colnames))
colnames(WildText) <- Colnames

for(i in 1:nrow(WildText)){
  link = paste0("https://wilderness.net/visit-wilderness/?ID=", i)
  page = read_html(link)
  
  content <- page %>%
    html_nodes("#maincontent , h1") %>%
    html_text()
  
  WildText[i, 1] <- content %>%
    strsplit("wilderness = '") %>%
    unlist() %>%
    `[`(2) %>%
    strsplit("';\nvar") %>%
    unlist() %>%
    `[`(1)
  
  WildText[i, 2] <- content %>%
    strsplit("stateAbbr = '") %>%
    unlist() %>%
    `[`(2) %>%
    strsplit("';\nvar") %>%
    unlist() %>%
    `[`(1)
  
  WildText[i, 3] <- content %>%
    strsplit("yearDesignated = '") %>%
    unlist() %>%
    `[`(2) %>%
    strsplit("';\nvar") %>%
    unlist() %>%
    `[`(1)
  
  WildText[i, 4] <- content %>%
    strsplit("agency = '") %>%
    unlist() %>%
    `[`(2) %>%
    strsplit(";\nvar") %>%
    unlist() %>%
    `[`(1) %>%
    gsub("[^a-zA-Z ]", "", .)
  
  WildText[i, 5] <- content %>%
    strsplit("<h2>Introduction</h2></div>") %>%
    unlist() %>%
    `[`(2) %>%
    strsplit("Leave No Trace\n\t\t\t") %>%
    unlist() %>%
    `[`(1) %>%
    strsplit(";\n\n\n") %>%
    unlist() %>%
    `[`(2)
}

WildText <- as_tibble(WildText) # Convert the matrix to a tibble.

# Check to see if our text scrubbing rules missed any Wildernesses.
MissingDescrip <- WildText %>%
  mutate(WID = row_number()) %>%
  filter(is.na(Descrip)) %>%
  .$WID  

MissingDescrip # We can see that 44 Areas are missing descriptions because the page structure is slightly different.

# We will alter the rules a bit and re-scrub those pages.
for(i in MissingDescrip){
  link = paste0("https://wilderness.net/visit-wilderness/?ID=", i)
  page = read_html(link)
  
  WildText[i, 5] <- page %>%
    html_nodes("#maincontent") %>%
    html_text() %>%
    strsplit(paste0(";\nvar WID = '", i, "';\n\n\n")) %>%
    unlist() %>%
    `[`(2) %>%
    strsplit("Leave No Trace\n\t\t\t") %>%
    unlist() %>%
    `[`(1)
}

# There are still a couple of Wildernesses with missing information: Wisconsin Islands Wilderness #654 and Okefenokee Wilderness #426. Each of these areas have idiosyncratic text elements, so we can write specific rules to pull the descriptions for each.
# Wisconsin Islands Wilderness #654
link = paste0("https://wilderness.net/visit-wilderness/?ID=", 654)
page = read_html(link)

WildText[654, 5] <- page %>%
  html_nodes("#maincontent") %>%
  html_text() %>%
  strsplit(";\nvar WID = '654';\n\n\n") %>%
  unlist() %>%
  `[`(2) %>%
  strsplit("Closed Wilderness Area") %>%
  unlist() %>%
  `[`(1)

# Okefenokee Wilderness #426
link = paste0("https://wilderness.net/visit-wilderness/?ID=", 426)
page = read_html(link)

WildText[426, 5] <- page %>%
  html_nodes("#maincontent") %>%
  html_text() %>%
  strsplit("WID = '426';\n\n\n") %>%
  unlist() %>%
  `[`(2) %>%
  strsplit("Leave No TracePlan Ahead and Prepare:") %>%
  unlist() %>%
  `[`(1)

# Gunnison Gorge Wilderness #227
link = paste0("https://wilderness.net/visit-wilderness/?ID=", 227)
page = read_html(link)

WildText[227, 5] <- page %>%
  html_nodes("#maincontent") %>%
  html_text() %>%
  strsplit("WID = '227';\n\n\n") %>%
  unlist() %>%
  `[`(2) %>%
  strsplit("Leave No Trace") %>%
  unlist() %>%
  `[`(1)

# King Range Wilderness #687
link = paste0("https://wilderness.net/visit-wilderness/?ID=", 687)
page = read_html(link)

WildText[687, 5] <- page %>%
  html_nodes("#maincontent") %>%
  html_text() %>%
  strsplit("WID = '687';\n\n\n") %>%
  unlist() %>%
  `[`(2) %>%
  strsplit("Leave No Trace") %>%
  unlist() %>%
  `[`(1)

# The management of many Wilderness Areas is mandated to two agencies. We need to parse those.
WildText <- WildText %>%
  mutate(
    Agency = case_when(
      Agency == "Bureau of Land ManagementForest Service" ~ "Bureau of Land Management; Forest Service",
      Agency == "Bureau of Land ManagementNational Park Service" ~ "Bureau of Land Management; National Park Service",
      Agency == "Forest ServiceNational Park Service" ~ "Forest Service; National Park Service",
      Agency == "Fish and Wildlife ServiceForest Service" ~ "Fish and Wildlife Service; Forest Service",
      TRUE ~ Agency
    ),
    WID = row_number()
  ) %>%
  filter(!is.na(Wild))

# It would be difficult to analyse a 804-way comparisons. Instead, I want to group the Wilderness Areas by broad regions. I'm defining the Eastern Region as the states that boarder the Mississippi and those states to the east. The Western Region is everything to the west and Alaska, which contains almost half of the nation's Wilderness acreage [link to post] is it's own Region. I've grouped Hawaii and Puerto Rico into an Island Region, but because there are only ___ Areas in those places, we won't have enough data to analyze.
WildText <- WildText %>%
  mutate(Region = case_when(State %in% c("MT", "CA", "NM", "WA", "NV", "AZ", "UT", "OR", "SD", "TX", "CO", "WY", "ND", "ID", "NE", "OK") ~ "West",
                            State %in% c("MN", "FL", "PA", "IL", "TN", "VA", "KY", "MO", "VT", "GA", "MI", "AR", "NJ", "MS", "WI", "LA", "NC", "SC", "ME", "IN", "AL", "WV", "NY", "NH", "MA", "OH") ~ "East",
                            State == "AK" ~ "Alaska",
                            State == "HI" | State == "PR" ~ "Island"))

# At this point, I like to save these data so we don't need to rescrub every time we run the analysis.
saveRDS(WildText, "./WildernessDescriptions.rds")
WildText <- readRDS("./WildernessDescriptions.rds")

# Now that we've successfully scrubbed our data, we can begin text mining. I found Silge & Robinson's book, "Text Mining with R" invaluable [https://www.tidytextmining.com/index.html]
### NLP ====

# First, we remove non-text characters.
WT <- WildText %>%
  mutate(Descrip = gsub("\\.", " ", Descrip),
         Descrip = gsub("  ", " ", Descrip),
         Descrip = gsub("[^\x01-\x7F]", "", Descrip))

# Next, we tag the parts of speech for each word in the essay.
WT$pos <- with(WT, qdap::pos_by(Descrip))$POStagged$POStagged

POS_list <- strsplit(WT$pos, " ") # Break string down into list of tagged words

# Next, we create a tidy dataframe with the parts of speech and retain only the informational words (nouns, verbs, adjectives, and adverbs)
WT2 <- data.frame(Wild = rep(WT$Wild, sapply(POS_list, length)),
                  words = unlist(POS_list)) %>% # Convert lists of tagged words into tidy format, one word per row
  separate(words, into = c("word", "POS"), sep = "/") %>% # Create matching columns for the word and the tag
  filter(POS %in% c("JJ", "JJR", "JJS", "NN", "NNS", "RB", "RBR", "RBS", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ")) %>% # Keep only the nouns, verbs, adjectives, and adverbs
  anti_join(stop_words) # Remove stop words

rm(POS_list)

# Next we lemmatize the words (i.e. extract the stem word, for instance, peaks = peak and starkly = stark).
WT2$stem <- NA
for(i in 1:nrow(WT2)){
  WT2$stem[i] <- hunspell_stem(WT2$word[i]) %>% unlist() %>% .[1]
  print(paste0(sprintf("%.2f", i/nrow(WT2)*100), "% completed")) # This just outputs a progress indicator. 
} # This clocks in at the same time as sapply, but this allows us to track progress as well. 

# Add the regions from the original dataset onto the tidy dataset.
WT2 <- WT2 %>%
  left_join(WildText %>% select(Wild, Region))

# At this point, I like to save these data so we don't need to reprocess every time we run the analysis.
# saveRDS(WT2, "./WildernessByWord.rds")
WT2 <- readRDS("./WildernessByWord.rds")

# One of the first questions we might ask is, which word is used most frequently?
# A follow-up question might be, which adjective is most frequent? However, instead of looking for the most common adjective across all descriptions, we can find the most common adjective in each description and then count the number of WIlderness Areas for which the same word is most common.
plot_grid(
  WT2 %>% 
    group_by(word) %>%
    tally() %>%
    arrange(desc(n)) %>%
    top_n(20) %>%
    ggplot(aes(x = fct_reorder(word, n), 
               y = n)) +
    geom_segment(aes(xend = word,
                     y = 0,
                     yend = n), 
                 col = "grey70",
                 size = 1) +
    geom_point(aes(size = n), col = "grey50", pch = 16) +
    coord_flip() +
    theme(axis.line.y = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(vjust = 4),
          plot.margin=unit(c(1,0.2,0.2,0.2), "cm")) +
    labs(x = "",
         y = "Word use (total frequency)",
         title = "What word occurs most frequently?",
         subtitle = "Across all Wilderness descriptions, \nwhich word is the most common?"),
  
  WT2 %>% 
    filter(POS %in% c("JJ", "JJS", "JJR")) %>%
    group_by(word, Wild) %>%
    tally() %>%
    group_by(Wild) %>%
    filter(n == max(n)) %>%
    group_by(word) %>%
    tally() %>%
    arrange(desc(n)) %>%
    top_n(20) %>%
    ggplot(aes(x = fct_reorder(word, n), 
               y = n)) +
    geom_segment(aes(xend = word,
                     y = 0,
                     yend = n), 
                 col = "grey70",
                 size = 1) +
    geom_point(aes(size = n), col = "grey50", pch = 16) +
    coord_flip() +
    theme(axis.line.y = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(vjust = 5),
          plot.margin=unit(c(1,0.2,0.2,0.2), "cm")) +
    labs(x = "",
         y = "Number of Wilderness Areas descriptions",
         title = "What adjectives describe Wilderness Areas?",
         subtitle = expression("Among the most frequent adjectives in a description, \nwhich is most common across Wildnerness Areas?")),
  nrow = 1
)
ggsave("./figs/TotalFrequency.png", height = 5, width = 10, dpi = 300)

# We also might want to consider differences across regions.
WT2 %>%
  group_by(Region) %>% 
  summarise(Areas = n_distinct(Wild),
            WordCount = length(word)) %>%
  mutate(Word.per.Area = WordCount/Areas)

WT2 %>%
  group_by(Region, Wild) %>%
  summarise(WordCount = length(word)) %>%
  ggplot(aes(x = fct_reorder(Region, WordCount, median), col = Region, y = WordCount)) +
  geom_jitter(size = 3, alpha = 0.4, pch = 16, height = 0) +
  geom_boxplot(width = 0.3, alpha = 0.7) +
  scale_y_log10() +
  scale_color_manual(values = c("#008dc0", "#8ba506", "grey", "#bb5a00")) +
  labs(x = "", y = "", 
       title = "How many words to describe a Wilderness?",
       subtitle = "Description wordcount per Area")
ggsave("./figs/Boxplot.png", width = 5, height = 5, dpi = 600)

WT2 %>%
  group_by(Region, Wild) %>%
  summarise(WordCount = length(word)) %>%
  filter(WordCount > 1000)

WildText %>% filter(Wild == "Gunnison Gorge Wilderness") %>% .$Descrip
WildText %>% filter(Wild == "King Range Wilderness") %>% .$Descrip
# There are far more Wilderness Areas in the Western Region, and therefore more words. However the wordcount per Area is about the same for West and East. For whatever reason, Alaska and the Islands get more written about them!
  

# Visualize ====
# To make the main figure for this project, I really wanted to use word clounds. Word clounds aren't a great visualization for data interpretation, but they are a fun gimmick! Unfortunately, R doesn't have the best support for generating fancy wordclouds. The package wordcloud is solidly reliable, but makes can only make very basic images. The package wordcloud2 allows for far more customization, but it is extremely buggy and requires some work-arounds on most systems.
# I want the regional word clouds to conform to the Region's shape. I made some simple shape images (here: East, West, Alaska) that we can pass to wordcloud2.

WT2 %>%
  filter(Region == "West") %>%
  filter(word != "wilderness") %>%
  count(word) %>%
  filter(n > 10) %>%
  wordcloud2(figPath = "./West.png",
             maxRotation = 0,
             minRotation = 0,
             color = "#bb5a00",
             fontFamily = 'serif')
# Depending on your system, the image may not population in RStudio's plotting window. Click the button to "show in new window" which will open a browser tab. Try refreshind the browser tab a few times until the wordcloud generates. Unfortunately, there is no way to constrain the aspect ratio of the cloud, so you will need to resize your browser window to a square. Then you can right-click and save the image. ...like I said, wordcloud2 requires far too many work-arounds, but it is the only option for word clouds with custom shapes in R.

WT2 %>%
  filter(Region == "East") %>%
  filter(word != "wilderness") %>%
  count(word) %>%
  filter(n > 10) %>%
  wordcloud2(figPath = "./East.png",
             maxRotation = 0,
             minRotation = 0,
             color = "#8ba506",
             fontFamily = 'serif')

WT2 %>%
  filter(Region == "Alaska") %>%
  filter(word != "wilderness") %>%
  filter(word != "riv") %>%
  count(word) %>%
  filter(n > 2) %>%
  wordcloud2(figPath = "./AKblk.png",
             maxRotation = 0,
             minRotation = 0,
             color = "#008dc0",
             fontFamily = 'serif')
# I pulled the wordclound images into Illustrator to make the final adjustments. (You could certainly do all of this in R, but I'm much faster at Illustrator and prefer to use a visual program for graphic design decisions.)


# Because wordclouds are not useful for quantitative interpretation, I also want to make some histograms of the most common words associated with each region.
HistAK <- WT2 %>%
  filter(Region == "Alaska") %>%
  filter(stem != "wilderness") %>%
  filter(stem != "riv") %>%
  count(stem) %>%
  arrange(desc(n)) %>%
  top_n(40) %>%
  ggplot(aes(x = fct_reorder(stem, n), y = n)) +
  geom_bar(stat = "identity", fill = "#008dc0", width = 0.7) +
  coord_flip() +
  labs(y = "", x = "") +
  theme(axis.text.y = element_text(color = "#008dc0"),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "serif"))
HistAK
# ggsave("./figs/Bars_Alaska.png", HistAK, width = 3, height = 10, dpi = 600)

HistWest <- WT2 %>%
  filter(Region == "West") %>%
  filter(stem != "wilderness") %>%
  count(stem) %>%
  arrange(desc(n)) %>%
  top_n(40) %>%
  ggplot(aes(x = fct_reorder(stem, n), y = n)) +
  geom_bar(stat = "identity", fill = "#bb5a00", width = 0.7) +
  coord_flip() +
  labs(y = "", x = "") +
  theme(axis.text.y = element_text(color = "#bb5a00"),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "serif"))
HistWest
ggsave("./figs/Bars_West.png", HistWest, width = 3, height = 10, dpi = 600)

HistEast <- WT2 %>%
  filter(Region == "East") %>%
  filter(stem != "wilderness") %>%
  count(stem) %>%
  arrange(desc(n)) %>%
  top_n(40) %>%
  ggplot(aes(x = fct_reorder(stem, n), y = n)) +
  geom_bar(stat = "identity", fill = "#8ba506", width = 0.7) +
  coord_flip() +
  labs(y = "", x = "") +
  theme(axis.text.y = element_text(color = "#8ba506"),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "serif"))
HistEast
ggsave("./figs/Bars_East.png", HistEast, width = 3, height = 10, dpi = 600)

plot_grid(HistAK, HistWest, HistEast, ncol = 3)
ggsave("./figs/Histograms2.pdf", height = 9, width = 6, dpi = 600)

# We can also determine which words most distinguish regions from other regions. For example, "mountain" and "trail" are high frequency words in the Western region, but they also occur at high frequency in the Eastern region as well. So, these terms don't help us distinguish between regions. Instead we can estimate logratios of word occurence between regions. Log ratios conveniently scale symmetrically around 0. Greater absolute values indicate words particularly relevant to one region and smaller values indicate words that are equally relevant to both regions.
wordratios <- WT2 %>%
  filter(word != "wilderness") %>%
  filter(Region == "East" | Region == "West") %>%
  count(stem, Region) %>%
  filter(sum(n) > 10) %>%
  ungroup() %>%
  spread(Region, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1)/sum(. + 1))) %>%
  mutate(logratio = log(East/West)) %>%
  arrange(desc(logratio))
wordratios

wordratios %>%
  arrange(abs(logratio)) # Equally likely to be from East or West

WT2 %>%
  count(stem, Region) %>%
  group_by(stem) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = Region, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio.EW = log(East / West)) %>%
  arrange(abs(logratio.EW)) %>%
  slice(1:20) %>%
  mutate(class = "mid") %>%
  bind_rows(
    WT2 %>%
      count(stem, Region) %>%
      group_by(stem) %>%
      filter(sum(n) >= 10) %>%
      ungroup() %>%
      pivot_wider(names_from = Region, values_from = n, values_fill = 0) %>%
      mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
      mutate(logratio.EW = log(East / West)) %>%
      arrange((logratio.EW)) %>%
      slice(1:20) %>%
      mutate(class = "west")
  ) %>%
  bind_rows(
    WT2 %>%
      count(stem, Region) %>%
      group_by(stem) %>%
      filter(stem != "roger") %>%
      filter(sum(n) >= 10) %>%
      ungroup() %>%
      pivot_wider(names_from = Region, values_from = n, values_fill = 0) %>%
      mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
      mutate(logratio.EW = log(East / West)) %>%
      arrange(desc(logratio.EW)) %>%
      slice(1:20) %>%
      mutate(class = "east")
  ) %>%
  ggplot(aes(x = fct_reorder(stem, logratio.EW), 
             y = logratio.EW, 
             col = class)) +
  geom_segment(aes(xend = fct_reorder(stem, logratio.EW), 
                   y = case_when(class == "west" ~ -0.1,
                                 class == "mid" ~ 0,
                                 class == "east" ~ 0.1),
                   yend = logratio.EW)) +
  geom_point(data = . %>% filter(class == "west"), aes(size = exp(abs(logratio.EW))), pch = 16) +
  geom_point(data = . %>% filter(class == "east"), aes(size = exp(abs(logratio.EW))), pch = 16) +
  geom_text(data = . %>% filter(class == "west"), aes(label = stem, y = 0), hjust = 0) +
  geom_text(data = . %>% filter(class == "east"), aes(label = stem, y = 0), hjust = 1) +
  geom_text(data = . %>% filter(class == "mid"), aes(label = stem, y = 0)) +
  coord_flip() +
  scale_color_manual(values = c("#8ba506", "grey70", "#bb5a00")) +
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank()) +
  labs(x = "",
       y = "Log ratio ('Uniqueness' of words for a region)",
       title = "Which words are most unique to a Wilderness?")
ggsave("./figs/logratio.pdf", height = 8, width = 4)
ggsave("./figs/logratio.png", height = 8, width = 5, dpi = 300)

# Log ratios aren't as useful when we have more than two groups to compare. In our case, where we want to compare multiple groups, we can find the words that most distinguish one region from the others by computing the term frequency-inverse document frequency (tf-idf). In this case, we treat all descriptions from a region as a single "document".
WT2 %>%
  count(Wild, stem, sort = TRUE) %>%
  bind_tf_idf(stem, Wild, n) %>%
  group_by(Wild) %>%
  top_n(10) %>%
  ungroup() %>%
  arrange(desc(tf_idf)) %>%
  print(n = 20)

WT2 %>%
  filter(Region != "Island") %>%
  filter(!is.na(stem)) %>%
  filter(stem != "roger") %>%
  count(Region, stem, sort = TRUE) %>%
  bind_tf_idf(stem, Region, n) %>%
  group_by(Region) %>%
  top_n(10, tf_idf) %>%
  arrange(desc(Region), desc(tf_idf)) %>%
  print(n = 40)
# Words like "desert", "wash", "bighorn", and "mesa" are highly indicative of the West. The East is dexcribed most distinctly by it's plant species: "laurel", "oak", "maple" and by terms like "key" which refers to the islands in the southeast. Alaska is dominated by intuitive terms like "fjord", "tundra" and "alpine" and sea animals like "whale" and sea "lion". Place names also rise to high relevance for Alaska with terms like "Anchorage", "Prince of Wales Island", and "Cook Inlet".
# Let's remove the place name words from Alaska.

# I could simply display the tf-idf values as a bar chart like I did with term frequency, but it doesn't feel right. First, it would require that folks understand what a tf-idf value means. Second, I think of tf-idf as a comparison metric, not a stand-alone value. Unlike term frequency, we cannot estimate the tf-idf for any region with out the others. So, I think a visual representation should make it clear that high tf-idf indicate high dissimilarity from the words associated with other regions.
# The best way I can think of showing this for any number of comparisons is to plot them radially.
WT2 %>%
  filter(Region != "Island") %>%
  filter(!is.na(stem)) %>%
  filter(!stem %in% c("roger", "anchorage", "prince", "wale", "cook", "warren", "admiralty", "coronation")) %>%
  count(Region, stem, sort = TRUE) %>%
  bind_tf_idf(stem, Region, n) %>%
  group_by(Region) %>%
  top_n(30, tf_idf) %>%
  ungroup() %>%
  mutate(ordering = as.numeric(as.factor(Region)) + (tf_idf*100),
         stem = fct_reorder(stem, ordering, .desc = FALSE)) %>%
  mutate(tf_idf = tf_idf * 100) %>%
  ggplot(aes(x = fct_relevel(Region, c("East", "West", "Alaska")), label = stem, y = tf_idf, col = Region)) +
  geom_point() +
  coord_flip() +
  scale_color_manual(values = c("#008dc0", "#8ba506", "#bb5a00")) +
  scale_y_log10(limits = c(.025, 0.35)) +
  coord_polar() +
  geom_text_repel(aes(cex = tf_idf), max.overlaps = 100, family = "serif", segment.linetype = 0) +
  theme(panel.grid.major.x = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank()) +
  labs(x = "", y = "")
  NULL
ggsave("./figs/tf_idf_radial2.pdf", height = 8, width = 8)
ggsave("./figs/tf_idf_radial2.png", height = 8, width = 8, dpi = 300)

WildText %>% filter(Wild == 'Okefenokee Wilderness') %>% .$Descrip

WildText <- readRDS("./WildernessByWord.rds")

WT2 %>%
  filter(Region == "Alaska") %>%
  filter(stem != "wilderness") %>%
  count(stem) %>%
  arrange(desc(n)) %>%
  top_n(50) %>%
  write.csv("./Alaska.csv")


WT2 %>%
  filter(Region == "West") %>%
  filter(stem != "wilderness") %>%
  count(stem) %>%
  arrange(desc(n)) %>%
  top_n(50) %>%
  write.csv("./West.csv")


WT2 %>%
  filter(Region == "East") %>%
  filter(stem != "wilderness") %>%
  count(stem) %>%
  arrange(desc(n)) %>%
  top_n(50) %>%
  write.csv("./East.csv")

WT2_dfm <- cast_dfm(Region, word)


# Frequency difference ====
WT2 %>% head()

WT2 %>% 
  group_by(Region, stem) %>%
  summarise(n = n()) %>%
  group_by(Region) %>%
  mutate(perc = n/sum(n)*100) %>%
  mutate(odds =(n + 1)/sum(n + 1)) %>%
  filter(Region %in% c("West", "East")) %>%
  pivot_wider(id_cols = stem, names_from = Region, values_from = perc, values_fill = 0) %>%
  filter(stem != "wilderness") %>%
  mutate(dif = East-West) %>%
  ggplot(aes(x = West, y = East, label = stem, col = dif)) +
    # geom_point(pch = 16, alpha = 0.5, col = "grey70", size = 2) +
    geom_abline(col = "grey80", lty = 2) +
    geom_text(check_overlap = TRUE, position = "jitter") +
    scale_color_gradientn(colors = c("#8ba506", "#8ba506", "grey70", "#bb5a00", "#bb5a00"), values = c(1,0.62,0.566,0.516,0)) +
    # scale_x_log10() +
    # scale_y_log10() +
  theme(legend.position = "right")

WT2 %>% 
  group_by(Region, stem) %>%
  summarise(n = n()) %>%
  group_by(Region) %>%
  mutate(perc = n/sum(n)*100) %>%
  mutate(logodds = (n + 1)/sum(n + 1)) %>%
  filter(Region %in% c("West", "East")) %>%
  pivot_wider(id_cols = stem, names_from = Region, values_from = logodds, values_fill = NA) %>%
  filter(stem != "wilderness") %>%
  mutate(oddsratio = East/West) %>%
  filter(!is.na(stem)) %>%
  filter(oddsratio > 0) %>%
  arrange(desc(abs(oddsratio)))

0.566 + 0.05
0.566 - 0.05



