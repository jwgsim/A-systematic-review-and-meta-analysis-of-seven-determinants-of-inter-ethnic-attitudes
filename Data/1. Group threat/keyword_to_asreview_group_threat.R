############################################
##### Literature Search - Group Threat #####
############################################

####################
##### Packages #####
####################

## Package names
packages <- c("remotes", "devtools", "revtools", "dplyr", "readr", "synthesisr")
## Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
  }
## Load packages.
invisible(lapply(packages, library, character.only = TRUE))
## Check if "litsearchr" and "ggVennDiagram" packages can be required. If not, install it. 
if(!require(litsearchr)) install_github("elizagrames/litsearchr", ref = "main")
if (!require(ggVennDiagram)) install_github("gaospecial/ggVennDiagram")
## Require "litsearchr" and "ggVennDiagram" packages. 
library(litsearchr)
library(ggVennDiagram)
## Clear environment.
rm(list = ls())

###################################
##### Naive Literature Search #####
###################################

set.seed(3791) # One ring to rule them all. In principle, there is no RNG in this script, but I set a seed
# anyway. 

### Start by generating a set of naive keywords. This set will be used to perform an initial, naive search of 
### the group threat literature. The naive keywords are extracted from two sources, the review question, and 
### a set of gold-standard articles. The review question is formulated as: are group threat, group contact, 
### media, socialization, and the demographics age, gender, and education determinants of inter-ethnic 
### attitudes in the 2010-2022 period? Note that we identify four research paradigms in this research 
### question: group threat, group contact, media, and socialization. Effects of the various demographic 
### variables are assumed to be present in articles on the four main paradigms. In this specific R-file we 
### limit the scope to the group threat determinant. We split the constituent elements in the research 
### question as being either a determinant or an outcome of interest, a distinction that we will follow 
### throughout the rest of this document:
naive_keywords <- c("group threat",  "inter-ethnic attitudes")

### The second and most important source of naive keywords are a set of six gold standard articles. These 
### articles have been selected by Frank van Tubergen. Note that this set of articles is not assumed to be 
### representative of the literature that we seek to retrieve. We use the information within these articles
### to try and maximize coverage of this conceptually heterogeneous literature. We argue that due to this 
### heterogeneity, it is difficult to predict which keywords will and will not be relevant. As such, we start 
### by selecting a set of typical articles to include in a review, and use the keywords in these articles to 
### frame our initial search. The idea is that by combining the information in these articles with expert 
### judgement, we will be better able to construct search strings that cover most of the relevant 
### literature.
gs_group_threat <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/1. Validat", 
                     "ion/1. Gold standard"), 
  verbose = TRUE)
## Request the length of the title object to show that there are 6 gold standard articles. Also request the
## object itself to show the titles of the articles themselves. 
length(gs_group_threat$title)
gs_group_threat$title

### The keywords embedded in these gold standard articles are the keywords as they have been listed by the 
### authors themselves, and those keywords as identified by the Rapid Automatic Keyword Extraction Algorithm 
### (RAKE). The RAKE is a keyword extraction algorithm which tries to determine key phrases in a body of text 
### by analyzing the frequency of word appearance and its co-occurrence with other words in the text. Please 
### refer to Rose, Engel, Cramer, and Cowley (2010) for an elaboration on the RAKE. 
## Start by obtaining the keywords listed in the articles. We extract ngrams larger or equal to one that are 
## listed at least once. 
sum(!is.na(gs_group_threat$keywords)) # Note that all 6 gold standard articles list keywords. 
gs_tagged_keyword_candidates <- litsearchr::extract_terms(
  keywords = gs_group_threat$keywords, # This is a list with the keywords from the gold standard articles. 
  method = "tagged", # Indicate that we want to obtain the keywords from the provided list.
  min_freq = 1, # The keyword has to occur a minimum of one time to be included.  
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 1, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
gs_tagged_keyword_candidates # Resulting keywords. 
## Filter the "gs_tagged_keywords" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship. The keywords "americans",
## "anti-immigrant", "attitudes", "commitment", "contact", "cross-national comparison", "education", 
## "europe", "european social  survey", "european social survey ess", "exclusionism", "factorial invariance",
## "foreigners", "inequality", "measurement invariance", "measurement scalar equivalence", "misperception",                  
## "multilevel", "nationalism", "numbers", "opinion", "opposition", "percent black", "politics", 
## "populations", "race", "racism", "segregation", "social-context", "societies", 
## "structural equation modelling", "threat", "tolerance", "views", "visibility", "western-europe", are 
## excluded.

## Among these exclusions, some arguably arbitrary choices are, "anti-immigrant", "attitudes", 
## "exclusionism", "foreigners", "inequality", "nationalism", "opinion", "opposition", "percent black", 
## "politics", "populations", "race", "racism", "segregation", "threat", "tolerance", "visibility". Although
## these keywords are arguably related in content to either the dependent or independent variable of 
## interest, I decide to exclude them because they are either redundant due to the inclusion of other words
## ("anti-immigrant") or are not specific enough and will greatly decrease the precision of the search 
## ("attitudes" or "exclusionism"). 
gs_tagged_keywords <- gs_tagged_keyword_candidates[
  -c(1, 3, 4, 6, 8, 9, 11, 13, 14, 15, 16, 17, 18, 21, 23, 24, 26, 27, 28, 29, 30, 31, 35, 36, 37, 39, 43, 45, 
     48, 49, 50, 52, 53, 54, 55, 56)
] 
gs_tagged_keywords[3] <- "group competition" # Change "competition" to "group competition" to make it more 
# specific to the search at hand, i.e., competition at the group level, not in general.
gs_tagged_keywords[5] <- "ethnic exclusionism" # Remove spacing.
gs_tagged_keywords[19] <- "size of immigrant population" # Remove spacing.
gs_tagged_keywords

## Use the RAKE to obtain keywords from the titles and abstracts of the gold standard articles. We extract 
## all non-single keywords that occur at least two times in the titles and abstracts. 
gs_raked_keyword_candidates <- litsearchr::extract_terms(
  text = paste(gs_group_threat$title, gs_group_threat$abstract), # This is a list of the gold standard 
  # articles' titles and abstracts.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 2, # The keyword has to occur a minimum of two times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
gs_raked_keyword_candidates
## Filter "gs_raked_keywords" object based on keyword being either dependent or independent variable of 
## interest. The following terms are removed: "european countries", "european social", "european social 
## survey", "social survey". 
gs_raked_keywords <- gs_raked_keyword_candidates[-c(3, 4, 5, 9)]
gs_raked_keywords
## Combine the tagged and raked keywords from the gold standard articles.
gs_all_keywords <- c()
gs_all_keywords <- append(gs_all_keywords, c(naive_keywords, gs_tagged_keywords, gs_raked_keywords))
gs_all_keywords <- remove_redundancies(gs_all_keywords, closure = "full") # Remove duplicate search terms.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive search") # Set working
# directory.
## Save the selected keywords to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate.
sink("naive_search_selected_terms.txt")
print(gs_all_keywords)
sink()
## Group resulting keywords based on being either a determinant or outcome of interest. 
gs_grouped_terms <- list(
  determinant = gs_all_keywords[c(1, 5, 6, 8, 10, 12, 13, 18, 19, 20, 21, 25, 26, 27)],
  outcome = gs_all_keywords[c(2, 3, 4, 7, 9, 11, 14, 15, 16, 17, 22, 23, 24)])
gs_grouped_terms # Final set of naive keywords. 

### Given the naive keywords, we can proceed with a naive search of the academic literature. Gusenbauer & 
### Haddaway (2020) advise the use of what they term principal academic search systems when performing a
### systematic review, where each of these principal systems meet a set of quality requirements. Please 
### refer to Gusenbauer & Haddaway (2020) for a complete overview of this requirement set. They identify 
### fourteen principal search engines, six of which are relevant in the context of this systematic review. 
### These are, with the database being searched in parentheses behind the academic search system: (1) the 
### Bielefeld Academic Search Engine (BASE) (Full index), (2) OVID (Selection: PsycINFO), (3) ProQuest 
### (Selection: Sociological Abstracts), (4) ScienceDirect (Full index), (5) Scopus (Full index), (6) Web of 
### Science (Selection: Web of Science Core Collection), and (7) Wiley Online Library (Full index). Although 
### appropriate in principle, the BASE (Full index), ScienceDirect (Full index), and Wiley Online Library 
### (Full index) are excluded from this list. Our reason for doing so is that trial and error showed that the 
### BASE does not return enough documents which are relevant to our search, that ScienceDirect only allows 
### for searches with a string length of 25, which is not suitable for our purposes, and finally, that the 
### Wiley Online Library only allows for users to extract twenty documents per batch, which is not enough to 
### facilitate a time-efficient search. Gusenbauer & Haddaway (2020) note that arXiv (Full index; settings: 
### All fields), Directory of Open Access Journals (DOAJ) (Full index), Google Scholar (Full index), JSTOR 
### (Full index), Microsoft Academic (Full index), Springer Link (Full index), WorldCat (Selection: 
### Thesis/Dissertation), and WorldWideScience (Full index) can be used as supplementary search systems, to 
### obtain grey literature for example. Although these supplementary search engines would ideally also be 
### considered, we do not consider them, again due to time and resource constraints.

### An initial, manual naive search is performed with the set of naive keywords in OVID (Selection: 
### PsycINFO), ProQuest (Selection: Sociological Abstracts), Scopus (Selection: Full index), and Web of 
### Science (Selection: Web of Science Core Collection). Given the grouped gold standard article terms, write 
### the the Boolean search to the directory and print it to the console.  
write_search(
  gs_grouped_terms, # The list of determinant and outcome keywords identified in the gold standard articles.
  languages = "English", # Language to write the search in, here set to English.
  exactphrase = TRUE, # Whether terms that consist of more than one word should be matched exactly rather 
  # than as two separate words. Set to TRUE, to limit both the scope and the number of redundant documents.
  stemming = TRUE, # Whether words are stripped down to the smallest meaningful part of the word to catch all 
  # variants of the word. Set to TRUE.
  closure = "none", # Whether partial matches are matched at the left end of a word ("left"), at the right 
  # ("right"), only as exact matches ("full"), or as any word containing a term ("none"). Set to "none" to
  # obtain as many documents as possible.
  writesearch = TRUE) # Whether we would like to write the search text to a file in the current directory. 
# Set to TRUE.
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- gsub('\\', "", bool, fixed = TRUE)
## Write the Boolean search for Ovid. 
bool <- gsub("((", "(", bool, fixed = T)
bool <- gsub("))", ")", bool, fixed = T)
cat(bool)
writeLines(bool, "naive_bool_ovid.txt")
## Write the Boolean search for ProQuest. 
writeLines(bool, "naive_bool_proquest.txt")
## Write the Boolean search for Scopus. 
writeLines(bool, "naive_bool_scopus.txt")
## Write the Boolean search for Web of Science. 
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- paste0('ALL = ', bool, 'AND PY = (2010-2022)')
writeLines(bool, "naive_bool_wos.txt")
file.remove("search-inEnglish.txt") # Remove source file. 
## Finally save the grouped terms to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate.
sink("naive_search_terms.txt")
print(gs_grouped_terms)
sink()

### For each search, we restrict the time period to the 2010-2022 period and search for English documents
### peer-reviewed research articles. Note that our date endpoint is determined by the date that we searched 
### the respective databases. This date will be indicated for each search.

### In Ovid, we start by clicking on "Advanced Search", followed by "Change" where we select the APA 
### PsycInfo database. We then click on "Edit Limits", deselect the "Abstracts" box, and select the "Peer 
### Reviewed Journal" box, leaving the remaining boxes as is. We then click on "Customize Limits, check the 
### "Peer Reviewed Journal" and "English Language" boxes, and set the "Publication Year" tabs to "2010" and 
### "Current", respectively, We then enter the Boolean search in the search box and click "Search". Note 
### that the "Keyword" and "Map Term to Subject Heading" boxes are checked by default and are left as is. 
### This search was conducted on 05-07-2022. The documents are subsequently exported with "Format:" set to 
### "RIS" and "Fields:" set to "Complete Reference". If the total number of documents exceed 1500, this 
### exporting process is executed in batches of size 1500.

### In Proquest, we click on "Advanced Search", enter the Boolean search in the first line, set the value of 
### the "in" bar to "Anywhere except full text - NOFT", check the "Peer reviewed" box, select 
### "After this date..." in the "Publication date:" drop-down menu, and set the respective boxes to 
### "January", "1" and "2010", respectively. Under "Source type:" we select "Scholarly Journals" and under 
### "Language" we tick the "English" box. We subsequently click on "Search". On the next page, under 
### "Document type" we subsequently select "Article", where under "Language" we select "English". We set 
### "Items per page" to 100 and select the "EndNote" category under "All save & export options" for 
### exporting a RIS file for each page. This search was conducted on 05-07-2022.

### On the Scopus start page, we set the "Search within" tab to the default "Article title, Abstract, 
### Keywords" tab. We do not set it to the "All fields" tab, because doing so 1) retrieves a large number of 
### irrelevant documents, and 2) makes it difficult to export documents from Scopus due to sheer volumen. We 
### subsequently enter the Boolean search in the "Search documents" tab. We click on the "Add date range" 
### button and set the "Published from" tab to "2010", leaving the "To" tab to "Present", and the "Added to 
### Scopus" tab to "Anytime". We click "Search" and after having searched, scroll down to "Document type" and 
### "Language" under "Refine results", check the "Article" and "English" boxes, respectively, and click 
### "Limit to". This search was conducted on 05-07-2022. We subsequently select "All" and click on "Export" 
### and select "RIS Format". Note that I only export "Citation information", "Bibliographical information" 
### and "Abstracts & keywords" in Scopus because exporting the additional two categories "Funding details" 
### and "Other information" leads to merging issues later on. Note that we stratify the returned document set 
### by years when this number exceeds 2000. 

### In Web of Science, we click on "Advanced search", enter the Boolean search in the "Query Preview" search 
### box, and click "Search". We once again scroll down to "Document Types" and "Languages" under "Refine 
### results" and check the "Articles" and "English" and "Unspecified" boxes, respectively, if applicable. 
### This search was conducted on 05-07-2022. We subsequently click on "Select all records", "Export" and 
### then "RIS (other reference software):". We subsequently click on "Records from:" and export either the 
### total returned document set if this set is less than 1000, or export them in a per 1000 batch-wise 
### fashion if this number exceeds 1000, with "Record Content:" set to "Full Record". 

### The naive search resulted in 219 documents from Ovid (Selection: PsycINFO), 272 documents from ProQuest 
### (Selection: Sociological Abstracts), 308 documents from Scopus (Selection: Full index), and 821 
### documents from Web of Science (Selection: Web of Science Core Collection) for a total of 1,620 documents. 
### Please note that this document set is unfiltered, i.e., duplicates, retracted documents, unidentified 
### non-English documents, etc., have not yet been removed. The documents were manually extracted in a single 
### batch in Ovid, Scopus, and Web of Science, and in 100 sized batches in ProQuest.
  
### Data import and cleaning.
## Import results of initial, manual naive search.
naive_import_ovid <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive ", 
                     "search/1. Unmerged/Ovid"),
  verbose = TRUE)
naive_import_proquest <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive ", 
                     "search/1. Unmerged/ProQuest"),
  verbose = TRUE)
naive_import_scopus <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive ", 
                     "search/1. Unmerged/Scopus"),
  verbose = TRUE)
naive_import_wos <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive ", 
                     "search/1. Unmerged/Web of Science"),
  verbose = TRUE)
## Checking whether the length of the imported .ris files are equal to the lengths of the raw .ris files.
length(naive_import_ovid$title) # 219, which is correct.  
length(naive_import_proquest$title) # 272, which is correct.  
length(naive_import_scopus$title) # 308, which is correct.  
length(naive_import_wos$title) # 821, which is correct.  

### We subsequently identify and remove identifiable, non-English documents, and save the merged result
### to the working directory.
## Ovid.
table(naive_import_ovid$language) # All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive search/2. ", 
             "Merged/Ovid/1. Raw"))
write_refs(naive_import_ovid, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
## ProQuest.
table(naive_import_proquest$language) # ProQuest. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive search/2. ", 
             "Merged/ProQuest/1. Raw"))
write_refs(naive_import_proquest, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
## Scopus.
table(naive_import_scopus$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive search/2. ", 
             "Merged/Scopus/1. Raw"))
write_refs(naive_import_scopus, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
## Web of Science.
table(naive_import_wos$language) # Web of Science. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive search/2. ", 
             "Merged/Web of Science/1. Raw"))
write_refs(naive_import_wos, format = "ris", tag_naming = "synthesisr", file = "web_of_science_r")

### De-duplication. We remove duplicates within each corpus with the suite of functions in the "synthesisr"
### package. We start with exact matching, and afterwards use optimal string alignment (OSA) to identify 
### potential duplicates. More specifically, we use approximate string matching, which is prone to errors in 
### that either duplicates are identified which are not duplicates (false positive), or duplicates are not 
### identified which are (false negative). We address the first error by manually inspecting duplicate 
### candidates and only removing them if they are identified to be duplicates in that process. I execute two 
### OSA runs over a corpus, one which is more and one which is less strict in its matching criteria. Note  
### that the "extract_unique_references()" function merges information over the various duplicates, more 
### specifically, uses that column with the most characters, which generally translates to the column with 
### the most information on the document. We additionally import the de-duplicated output of the merged 
### corpora into two reference managers: EndNote and Zotero. I do this first to inspect if any duplicates 
### remain after the de-duplication in R. Note that we do not inspect duplicates in the separate corpora in 
### the two reference managers because this takes too much time and is ultimately redundant since we merge 
### and de-deduplicate them afterwards. In principle, it is not necessary to de-duplicate the separate 
### corpora before merging and de-deduplicating them. I still do this because it is interesting to gauge the 
### degree to which the separate corpora overlap and hold up to the validity criteria. The principal reason 
### that we use two these reference managers is because they have different sensitivity and specificity 
### rates. As such, with manual assessment their combined use will enable us to more thoroughly identify 
### duplicates. Zotero is additionally generally superior over EndNote in its detecting of retracted items 
### and in exporting the bibliography as a .ris file. EndNote is however superior to Zotero in that it 
### enables more detailed screening of duplicates on the basis of titles, abstracts, and keywords. As such, 
### after the main de-duplication procedure in R, I start by screening duplicates in EndNote and removing 
### these in Zotero, whether these are flagged by Zotero or not. In general, those documents in the duplicate 
### set are retained which list the most information on the title, abstract, and keywords. In EndNote I use 
### the "Author", "Volume", and "Pages" categories to compare references and identify duplicates. I 
### subsequently screen and flag any remaining duplicates in Zotero that were not identified in EndNote. 
### Zotero uses the the title, DOI, and ISBN fields to identify duplicates. If these fields match (or are 
### absent), Zotero also compares the years of publication (if they are within a year of each other) and 
### author/creator lists (if at least one author last name plus first initial matches). After this screening 
### procedure I remove retracted items as flagged by Zotero and EndNote, and export the result to R. The 
### duplication choices are not explicitly noted in the R-script but the retraction choices are. Note that I 
### will store the corpus file that is de-duplicated in the reference managers along with the final 
### de-deduplicated file for an iteration, so that the reader can cross-validate the de-duplication process. 
### Note that it is possible for some duplicates to remain unidentified with this procedure, but not for 
### non-duplicates to be accidentally removed, which is the most important to my judgment. Remaining 
### duplicates will be identified and removed during the review process. Note that I finally do not delete 
### errata or responses, because in principle, these contain non-redundant information. 

### Ovid.
## Exact matching.
length(naive_import_ovid$title) # Input is 219 documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  naive_import_ovid$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_ovid$title, exact_duplicates_ovid)) # Perform a 
# manual check. No duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # Zero documents should be removed.
naive_dedup_ovid <- synthesisr::extract_unique_references(naive_import_ovid, exact_duplicates_ovid) 
length(naive_dedup_ovid$title) # Output after exact matching is 219. Zero documents removed.
## Fuzzy matching five.
fuzzy_duplicates_ovid <- find_duplicates( 
  naive_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. No duplicate combinations identified.
sum(table(fuzzy_duplicates_ovid) - 1) # Zero documents should be removed.
naive_dedup_ovid <- extract_unique_references(naive_dedup_ovid, fuzzy_duplicates_ovid) # Extract unique 
# references. 
length(naive_dedup_ovid$title) # De-duplicated output is 219. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive search/2. ", 
             "Merged/Ovid/2. Deduplicated"))
write_refs(naive_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_ovid")

## ProQuest.
## Exact matching.
length(naive_import_proquest$title) # Input is 272 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  naive_import_proquest$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_proquest$title, exact_duplicates_proquest)) # 
# Perform a manual check. All candidate combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 20 documents should be removed.
naive_dedup_proquest <- extract_unique_references(naive_import_proquest, exact_duplicates_proquest) 
length(naive_dedup_proquest$title) # Output after exact matching is 252. 20 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  naive_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. All candidate combinations are duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # Four documents should be removed.
naive_dedup_proquest <- extract_unique_references(naive_dedup_proquest, fuzzy_duplicates_proquest) # Extract
# unique references. 
length(naive_dedup_proquest$title) # De-duplicated output is 248. Four documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  naive_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. All candidate combinations are duplicates. 
sum(table(fuzzy_duplicates_proquest) - 1) # Two documents should be removed.
naive_dedup_proquest <- extract_unique_references(naive_dedup_proquest, fuzzy_duplicates_proquest) # 
# Extract unique references. 
length(naive_dedup_proquest$title) # De-duplicated output is 246. Two documents removed.
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive search/2. ", 
             "Merged/ProQuest/2. Deduplicated"))
write_refs(naive_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_proquest")

## Scopus.
## Exact matching.
length(naive_import_scopus$title) # Input is 308 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  naive_import_scopus$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_scopus$title, exact_duplicates_scopus)) # Perform a 
# manual check. All candidate combinations are duplicates.
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # One document should be removed.
naive_dedup_scopus <- synthesisr::extract_unique_references(naive_import_scopus, exact_duplicates_scopus) 
length(naive_dedup_scopus$title) # Output after exact matching is 307. One document removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  naive_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. No duplicate combinations identified.
sum(table(fuzzy_duplicates_scopus) - 1) # Zero documents should be removed.
naive_dedup_scopus <- extract_unique_references(naive_dedup_scopus, fuzzy_duplicates_scopus) # Extract unique 
# references. 
length(naive_dedup_scopus$title) # De-duplicated output is 307. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive search/2. ", 
             "Merged/Scopus/2. Deduplicated"))
write_refs(naive_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_scopus")

## Web of Science.
## Exact matching.
length(naive_import_wos$title) # Input is 821 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  naive_import_wos$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_wos$title, exact_duplicates_wos)) # Perform a 
# manual check. No duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Zero documents should be removed.
naive_dedup_wos <- synthesisr::extract_unique_references(naive_import_wos, exact_duplicates_wos) 
length(naive_dedup_wos$title) # Output after exact matching is 821. Zero documents removed.
## Fuzzy matching five.
fuzzy_duplicates_wos <- find_duplicates( 
  naive_dedup_wos$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_wos$title, fuzzy_duplicates_wos)) # Perform a 
# manual check. No duplicate combinations identified.
sum(table(fuzzy_duplicates_wos) - 1) # Zero documents should be removed.
naive_dedup_wos <- extract_unique_references(naive_dedup_wos, fuzzy_duplicates_wos) # Extract unique 
# references. 
length(naive_dedup_wos$title) # De-duplicated output is 821. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive search/2. ", 
             "Merged/Web of Science/2. Deduplicated"))
write_refs(naive_dedup_wos, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_wos")

### Investigate the distribution of the overlap in the naive search. I am not completely sure how this 
### function identifies and matches duplicates.  
ggVennDiagram(list(naive_dedup_ovid$title, naive_dedup_proquest$title, naive_dedup_scopus$title, 
                   naive_dedup_wos$title), 
              category.names = c("Ovid", "ProQuest", "Scopus", "Web of Science"), label_alpha = 0.75, 
              label = c("count"))
## The Venn diagram seems to indicate that although there exists quite some overlap between the corpora, 
## especially between Scopus and Web of Science, that they each add a substantial number of 
## non-overlapping documents to the sum total. It might however be the case that this positively affects 
## recall and therefore negatively affects precision, in that many non-relevant documents are added by a 
## search engine like Ovid, as shown by the fact that Ovid did not as successfully retrieve the gold 
## standard articles where Scopus, ProQuest, and Web of Science did. 

### Corpus precision diagnostics. The precision of the search is quantified in two ways. We first evaluate 
### the degree to which the set of gold standard articles are in the retrieved corpora. Note that only 2 of 
### the 6 gold standard articles on group threat were published after 2010. Also note that the "check_recall" 
### function uses osa for approximate matching. Note that in the context of the naive search, whether we 
### start iterating on the search is conditional on the gold standard articles being in the sum over the 
### document sets retrieved over the four search systems. We secondly validate each corpus over a set of 
### articles that were obtained by taking a snowball sample over the gold standard articles. More 
### specifically,we enter the title of each gold standard article on the "connectedpapers.com" website, and
### select all papers that are suggested in the resulting network, and all papers under the "Derivative 
### works" tab, that were published between the 2010-2022 period and are relevant to the query of interest as 
### judged by the corresponding author, here the effect of group threat on inter-ethnic attitudes. 44 such
### external articles were identified. The following information is listed on the "connectedpapers.com"
### website on their methodology: "To create each graph, we analyze an order of ~50,000 papers and select the 
### few dozen with the strongest connections to the origin paper. In the graph, papers are arranged 
### according to their similarity. That means that even papers that do not directly cite each other can be 
### strongly connected and very closely positioned. Connected Papers is not a citation tree. Our similarity 
### metric is based on the concepts of Co-citation and Bibliographic Coupling. According to this measure, two 
### papers that have highly overlapping citations and references are presumed to have a higher chance of 
### treating a related subject matter. Our algorithm then builds a Force Directed Graph to distribute the 
### papers in a way that visually clusters similar papers together and pushes less similar papers away from 
### each other. Upon node selection we highlight the shortest path from each node to the origin paper in 
### similarity space. Our database is connected to the Semantic Scholar Paper Corpus (licensed under 
### ODC-BY)." 

## Gold standard precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
cbind(gs_group_threat$title, gs_group_threat$year) # 2 documents >= 2010.
## Ovid.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], naive_dedup_ovid$title)[, 3]), 0)) # 0 of 2 
# gold standard articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], naive_dedup_proquest$title)[, 3]), 0)) # 2 
# of 2 gold standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], naive_dedup_scopus$title)[, 3]), 0)) # 2 of 
# 2 gold standard articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], naive_dedup_wos$title)[, 3]), 0)) # 2 of 2 
# gold standard articles are retrieved.
## The gold standard precision check tentatively indicates that ProQuest, Scopus, and Web of Science have 
## superior precision to Ovid.

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
ex_group_threat <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/1. Validat", 
                     "ion sets/2. External"), 
  verbose = TRUE)
cbind(ex_group_threat$title, ex_group_threat$year) # Data frame of title and year of publication of 44 
# external articles.
## Ovid.
sum(round(as.numeric(check_recall(ex_group_threat$title, naive_dedup_ovid$title)[, 3]), 0)) # 5 of 44 
# external articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_group_threat$title, naive_dedup_proquest$title)[, 3]), 0)) # 12 of 44 
# external articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_group_threat$title, naive_dedup_scopus$title)[, 3]), 0)) # 14 of 44 
# external articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_group_threat$title, naive_dedup_wos$title)[, 3]), 0)) # 26 of 44 
# external articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus, ProQuest, and Ovid, respectively. 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
## Set working directory. 
naive_dedup <- bind_rows(naive_dedup_ovid, naive_dedup_proquest, naive_dedup_scopus, naive_dedup_wos) # Bind 
# the corpora into a single corpus. Should be equal to 219 + 246 + 307 + 821 = 1,593.
## Exact matching.
length(naive_dedup$title) # Input is 1,593 documents.
exact_duplicates <- synthesisr::find_duplicates(
  naive_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(naive_dedup$title, exact_duplicates) # Perform a manual check. 
length(exact_manual$title) # Sum of 808 duplicate combinations identified.
exact_manual$title[1:400] # All are duplicates.
exact_manual$title[400:802] # All are duplicates.
sum(as.numeric(table(exact_duplicates) - 1)) # 497 articles should be removed.
naive_dedup <- extract_unique_references(naive_dedup, exact_duplicates) 
length(naive_dedup$title) # Output after exact matching is 1,096. 497 documents removed.
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates( 
  naive_dedup$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup$title, fuzzy_duplicates)) # Perform a manual
# check. All combinations are duplicates.
sum(table(fuzzy_duplicates) - 1) # 38 documents should be removed.
naive_dedup <- extract_unique_references(naive_dedup, fuzzy_duplicates) # Extract unique references. 
length(naive_dedup$title) # De-duplicated output is 1,058. 38 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates( 
  naive_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup$title, fuzzy_duplicates)) # Perform a manual 
# check. All combinations are duplicates.
sum(table(fuzzy_duplicates) - 1) # Nine documents should be removed.
naive_dedup <- extract_unique_references(naive_dedup, fuzzy_duplicates) # Extract unique references. 
length(naive_dedup$title) # De-duplicated output is 1,049. Nine documents removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/2. Naive search/2. Merged")
write_refs(naive_dedup, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_refman")
# EndNote and Zotero both indicate that five duplicates remain in the .ris file. We furthermore remove two 
# retracted papers: "The Social Context of Latino Threat and Punitive Latino Sentiment", and "Structural 
# stigma and all-cause mortality in sexual minority populations". The result is exported in the file 
# "naive_dedup.ris" which is subsequently imported.  
naive_dedup <- read_bibliography("naive_dedup.ris")
length(naive_dedup$title) # 1,042 documents. Six documents removed.

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], naive_dedup$title)[, 3]), 0)) # 2 of 2 
# gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_group_threat$title, naive_dedup$title)[, 3]), 0)) # 26 of 44  external 
# articles are retrieved.

#########################################
##### Iteration 1 Literature Search #####
#########################################

### Now repeat the previous procedure, but with keywords extracted from the naive search corpus. This follows
### the same logic as earlier, due to the assumed high degree of heterogeneity of the literature, we combine 
### keyword information from the retrieved document set with author judgment to maximize coverage.

### Clear the environment except for the grouped terms, validation sets, and "naive_dedup" object. 
rm(list = setdiff(ls(), c("gs_grouped_terms", "gs_group_threat", "ex_group_threat", "naive_dedup")))

### Start by checking the naive search corpus for provided keywords, and keywords in titles and abstracts.
## Keywords.
length(naive_dedup$keywords) # Total number of documents is 1,042. 
length(naive_dedup$keywords) - sum(is.na(naive_dedup$keywords)) # 1,039 of the articles list keywords.  
## Titles and abstracts.
length(naive_dedup$title) - sum(is.na(naive_dedup$title)) # All of the articles list a title.  
length(naive_dedup$abstract) - sum(is.na(naive_dedup$abstract)) # 973 of the articles list an abstract.  

### The first step is to obtain the keywords listed in the articles. We extract all non-single keywords that 
### are listed a minimum of ten times. Note that the ten number is arbitrary insofar that we need to 
### strike a balance between retrieving enough but not too many keyword candidates. More specifically, 
### Gusenbauer & Haddaway (2020) note that search terms of length 25 should allow reviewers to specify their 
### search scope to a reasonable extent. Put differently, our Boolean search should contain at least 25 
### search terms. On the other hand, the maximum search string length that Ovid, Proquest, Scopus, and Web of 
### Science allow is around 1000 according to Gusenbauer & Haddaway (2020). Web of Science for example 
### states that the maximum number of terms allowed in one field is 50 terms when using "All Fields". As 
### such, this number of keywords is a natural cap to the number of keywords that we can seek to obtain from 
### a corpus set. To reiterate, the number of keywords that will be incorporated in the final searches will 
### have a lower limit of 25 search terms, and an upper limit of 1000 characters in the search string length, 
### which translates to around 50 to 60 search terms (depending on the length of the search terms).
tagged_keywords <- litsearchr::extract_terms(
  keywords = naive_dedup$keywords, # This is a list with the keywords from the articles. 
  method = "tagged", # Indicate that we want to obtain the keywords from the provided list.
  min_freq = 10, # The keyword has to occur a minimum of ten times to be included.   
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(tagged_keywords) # 96 tagged keywords.
### Now use the RAKE to obtain keywords from the titles and abstracts of naive search corpus. We extract all 
### non-single keywords that occur at least ten times in these titles and abstracts. 
raked_keywords <- litsearchr::extract_terms(
  text = paste(naive_dedup$title, naive_dedup$abstract), # This is a list of titles and abstracts 
  # per gold standard article.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 10, # The keyword has to occur a minimum of ten times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(raked_keywords) # 210 raked keywords.
## Sum total of tagged and raked keywords. 
keyword_candidates <- remove_redundancies(c(raked_keywords, tagged_keywords), closure = "full") # Remove 
# duplicates.
length(keyword_candidates) # Total of 252 keyword candidates. 
## The subsequent task is to select all keywords that are relevant to our query from the candidate pool. I 
## select terms that relate in content to the relevant literature, i.e., are a independent or dependent 
## variable of interest in the group threat on inter-ethnic attitudes relationship. Note that I interpret 
## relevance quite broadly, since these terms will be will be ranked and filtered further based on their 
## "connectedness" in a co-occurrence network. In that sense, it is better too be too liberal than too 
## selective in our choices here, since it might be hard to anticipate which relevant terms are important 
## from this "connectedness" perspective. I furthermore do not include keywords which relate directly to any 
## of the other paradigms, e.g., I do not include keywords on contact theory even though these might appear 
## often in the group threat literature. Finally note that I do this part manually, which is prone to errors. 
all_keywords <- keyword_candidates[
  c(1, 10, 11, 12, 13, 21, 23, 29, 31, 32, 34, 35, 36, 37, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 65, 66, 
    67, 68, 69, 70, 71, 76, 77, 78, 79, 80, 81, 84, 85, 86, 89, 90, 96, 98, 99, 100, 101, 107, 111, 113, 114,
    117, 118, 119, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 132, 133, 143, 145, 146, 147, 148, 149,
    150, 151, 152, 153, 154, 155, 156, 157, 159, 163, 166, 171, 177, 179, 180, 181, 182, 183, 188, 200, 201,
    203, 204, 210, 211, 212, 217, 219, 220, 221, 222, 223, 224, 225, 226, 230, 231, 232, 235, 237, 238, 242,
    243, 244, 245, 246, 248, 250)
  ]
## Manual cleaning.
(all_keywords <- sort(all_keywords))
all_keywords <- all_keywords[c(-1)] # Remove "0410 group interactions".
all_keywords[1] <- "group interactions" # Change "0410group interactions" to "group interactions".
(all_keywords <- sort(all_keywords))
length(all_keywords) # 119 keyword candidates.

### We further filter the keyword set by ranking the relative strength of each keyword in a so-called keyword 
### co-occurrence network (KCN). In a KCN, each keyword is "represented as a node and each co-occurrence of 
### a pair of words is represented as a link" (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). "The number 
### of times that a pair of words co-occurs in multiple articles constitutes the weight of the link 
### connecting the pair" (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). "The network constructed in this 
### manner represents cumulative knowledge of a domain and helps to uncover meaningful knowledge components 
### and insights based on the patterns and strength of links between keywords that appear in the literature" 
### (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). I furthermore combine the second iteration keywords 
## with the naive keywords. I add the naive keywords to the keyword candidate selection under the assumption 
## that these are important keywords because they were obtained from the gold standard articles, and should 
## therefore be considered explicitly in the KCN analysis, even though they are implicitly represented.
all_keywords_final <- c()
all_keywords_final <- append(all_keywords_final, c(as.vector(unlist(gs_grouped_terms)), all_keywords))
all_keywords_final <- remove_redundancies(all_keywords_final, closure = "full") # Remove duplicates. 
length(all_keywords_final) # 132 candidates.
## Build the keyword co-occurrence network. This chunk of code is a reworked version of the tutorial at: 
## https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html.
filter_dfm <- litsearchr::create_dfm(
  elements = paste(naive_dedup$title, naive_dedup$abstract), # The input in which the keywords can co-occur, 
  # titles and abstracts. 
  features = all_keywords) # The keyword candidates. 
kcn <- create_network(filter_dfm) # Create a KCN.
## Rank keywords based on strength in the co-occurrence network.
strengths <- strength(kcn) # Calculate strength values. 
data.frame(term = names(strengths), strength = strengths, row.names = NULL) %>%
  mutate(rank = rank(strength, ties.method = "min")) %>%
  arrange(desc(strength)) ->
  term_strengths # Create a data frame where the keywords are sorted by strength, in descending order. 
## I now apply a filter to select 57 and 42 terms for the search string in ovid, scopus, and web of 
## science, and ProQuest, respectively, where I remove terms which refer to an equivalent or near equivalent
## concept, i.e., "target group" and "target groups", where I select the term with the highest strength 
## value. Note that the 57 and 42 values were the result of trial-and-error in the respective search engines.
(term_strengths <- cbind(seq(length(term_strengths$term)), term_strengths[order(term_strengths$term), ]))
term_strengths <- term_strengths[-c(4, 10, 20, 22, 24, 30, 36, 44, 55, 57, 58, 60, 64, 68, 69, 72, 76, 89, 
                                    94, 96, 99, 102, 105, 112), ]

### Construct Boolean search OVID, Web of Science, and Scopus.
## Select first 57 terms from filtered term set. 
keywords_ovid_scopus_wos <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:57, ]$term)
(keywords_ovid_scopus_wos <- keywords_ovid_scopus_wos[order(keywords_ovid_scopus_wos)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1")
sink("first_iteration_selected_terms_ovid_scopus_wos.txt")
print(keywords_ovid_scopus_wos)
sink() 
## Categorize "keywords_ovid_scopus_wos" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_ovid_scopus_wos <- list(
  determinant = keywords_ovid_scopus_wos[c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 
                                           22, 23, 28, 30, 31, 32, 33, 35, 36, 37, 38, 41, 47, 50, 51, 52, 
                                           53, 54, 55, 56, 57)],
  outcome = keywords_ovid_scopus_wos[c(1, 2, 3, 24, 25, 26, 27, 29, 34, 39, 40, 42, 43, 44, 45, 46, 48, 49)])
grouped_terms_ovid_scopus_wos
### Given the grouped terms, write the Boolean search for OVID, Web of Science, and Scopus to the 
### directory and print it to the console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1") # Set directory.
write_search(
  grouped_terms_ovid_scopus_wos, # The list of determinant and outcome keywords identified in the naive 
  # document set.
  languages = "English", # Language to write the search in, here set to English.
  exactphrase = TRUE, # Whether terms that consist of more than one word should be matched exactly rather 
  # than as two separate words. Set to TRUE, to limit both the scope and the number of redundant documents.
  stemming = TRUE, # Whether words are stripped down to the smallest meaningful part of the word to catch all 
  # variants of the word. Set to TRUE.
  closure = "none", # Whether partial matches are matched at the left end of a word ("left"), at the right 
  # ("right"), only as exact matches ("full"), or as any word containing a term ("none").  Set to "none" to 
  # obtain as many documents as possible.  
  writesearch = TRUE) # Whether we would like to write the search text to a file in the current directory. 
# Set to TRUE.
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- gsub('\\', "", bool, fixed = TRUE)
## Write the Boolean search for Ovid. 
bool <- gsub("((", "(", bool, fixed = T)
bool <- gsub("))", ")", bool, fixed = T)
cat(bool)
writeLines(bool, "bool_ovid_it1.txt")
## Write the Boolean search for Scopus. 
writeLines(bool, "bool_scopus_it1.txt")
## Write the Boolean search for Web of Science. 
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- paste0('ALL = ', bool, 'AND PY = (2010-2022)')
writeLines(bool, "bool_wos_it1.txt")
file.remove("search-inEnglish.txt") # Remove source file. 
## Finally save the grouped terms to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate for any particular reason.
sink("first_iteration_search_terms_ovid_scopus_wos.txt")
print(grouped_terms_ovid_scopus_wos)
sink() 

### Construct Boolean search ProQuest.
## Select first 42 terms from filtered term set.
keywords_proquest <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:42, ]$term)
(keywords_proquest <- keywords_proquest[order(keywords_proquest)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1")
sink("first_iteration_selected_terms_proquest.txt")
print(keywords_proquest)
sink() 
## Categorize "keywords_proquest" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_proquest <- list(
  determinant = keywords_proquest[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 26,
                                    27, 34, 37, 38, 39, 40, 41, 42)],
  outcome = keywords_proquest[c(1, 2, 18, 19, 20, 25, 28, 29, 30, 31, 32, 33, 35, 36)])
grouped_terms_proquest
### Given the grouped terms, write the the Boolean search for ProQuest to the directory and print it to the 
### console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1") # Set directory.
write_search(
  grouped_terms_proquest, # The list of determinant and outcome keywords identified in the naive document 
  # set.
  languages = "English", # Language to write the search in, here set to English.
  exactphrase = TRUE, # Whether terms that consist of more than one word should be matched exactly rather 
  # than as two separate words. Set to TRUE, to limit both the scope and the number of redundant documents.
  stemming = TRUE, # Whether words are stripped down to the smallest meaningful part of the word to catch all 
  # variants of the word. Set to TRUE.
  closure = "none", # Whether partial matches are matched at the left end of a word ("left"), at the right 
  # ("right"), only as exact matches ("full"), or as any word containing a term ("none").  Set to "none" to 
  # obtain as many documents as possible.  
  writesearch = TRUE) # Whether we would like to write the search text to a file in the current directory. 
# Set to TRUE.
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- gsub('\\', "", bool, fixed = TRUE)
## Write the Boolean search for ProQuest. 
bool <- gsub("((", "(", bool, fixed = T)
bool <- gsub("))", ")", bool, fixed = T)
cat(bool)
writeLines(bool, "bool_proquest_it1.txt")
file.remove("search-inEnglish.txt") # Remove source file. 
## Finally save the grouped terms to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate for any particular reason.
sink("first_iteration_search_terms_proquest.txt")
print(grouped_terms_proquest)
sink() 

### Please refer to the naive iteration for an overview of the exact steps of the search procedure. 

### All searches were conducted on 09-07-2022. These resulted in 2,466 documents from Ovid (Selection: 
### PsycINFO), 4,096 from ProQuest (Selection: Sociological Abstracts), 4,347 documents from Scopus 
### (Selection: Full index), and 4,749 documents from Web of Science (Selection: Web of Science Core 
### Collection), for a total of 15,658 documents. Please note that this document set is unfiltered, i.e., 
### duplicates, retracted documents, unidentified non-English documents, etc., have not yet been removed. In
### Ovid, the documents were manually extracted in three batches. In ProQuest the documents were manually 
### extracted in 100-sized batches. In Scopus, the documents were manually extracted in 2000 document sized 
### batches, stratified by year. In Web of Science, the documents were manually extracted in 1000 document 
### sized batches. 

### Data import and cleaning.
## Import results of informed search. Note that the batches for each search system were merged in EndNote.
## Start by checking whether the imported length is equal to the length of the raw .ris files.
import_ovid_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iterat", 
                     "ion 1/1. Unmerged/Ovid"),
  verbose = TRUE)
import_proquest_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iterat", 
                     "ion 1/1. Unmerged/ProQuest"),
  verbose = TRUE)
import_scopus_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iterat", 
                     "ion 1/1. Unmerged/Scopus"), 
  verbose = TRUE)
import_wos_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iterat", 
                     "ion 1/1. Unmerged/Web of Science"),
  verbose = TRUE)
length(import_ovid_it1$title) # 2,466, which is correct.  
length(import_proquest_it1$title) # 4,096, which is correct.  
length(import_scopus_it1$title) # 4,347, which is correct.
length(import_wos_it1$title) # 4,749, which is correct.  

### We subsequently identify and remove identifiable, non-English documents, and merge and save the result
### to the appropriate working directory.
## Ovid.
table(import_ovid_it1$language) # Ovid. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1/2. Me",
             "rged/Ovid/1. Raw"))
write_refs(import_ovid_it1, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
## ProQuest.
table(import_proquest_it1$language) # ProQuest. Not all documents in the .ris file are in English.
import_proquest_it1 <- import_proquest_it1[import_proquest_it1$language == "English" ,] # Select English 
# documents and store them.
table(import_proquest_it1$language) # ProQuest. All documents in the .ris file are now in English. Note 
# the ProQuest corpus now consists of 4,095 articles.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1/2. Me", 
             "rged/ProQuest/1. Raw"))
write_refs(import_proquest_it1, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
## Scopus.
table(import_scopus_it1$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1/2. Me", 
             "rged/Scopus/1. Raw"))
write_refs(import_scopus_it1, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
## Web of Science.
table(import_wos_it1$language) # Web of Science. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1/2. Me", 
             "rged/Web of Science/1. Raw"))
write_refs(import_wos_it1, format = "ris", tag_naming = "synthesisr", file = "web_of_science_r")

### De-duplication. Please refer to the naive iteration for an overview of the exact steps of the 
### de-duplication procedure.

### Ovid.
## Exact matching.
length(import_ovid_it1$title) # Input is 2,466 documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  import_ovid_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_ovid_it1$title, exact_duplicates_ovid)) # Perform a 
# manual check. One duplicate combination identified.
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # One document should be removed.
it1_dedup_ovid <- synthesisr::extract_unique_references(import_ovid_it1, exact_duplicates_ovid) 
length(it1_dedup_ovid$title) # Output after exact matching is 2,465. One document removed.
## Fuzzy matching five.
fuzzy_duplicates_ovid <- find_duplicates( 
  it1_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. One duplicate combination identified.
sum(table(fuzzy_duplicates_ovid) - 1) # One document should be removed.
it1_dedup_ovid <- extract_unique_references(it1_dedup_ovid, fuzzy_duplicates_ovid) # Extract unique 
# references. 
length(it1_dedup_ovid$title) # De-duplicated output is 2,464. One document removed. 
## Fuzzy matching ten.
fuzzy_duplicates_ovid <- find_duplicates( 
  it1_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. No duplicate combinations identified.
length(it1_dedup_ovid$title) # De-duplicated output is 2,464. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1/2. Mer", 
             "ged/Ovid/2. Deduplicated"))
write_refs(it1_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_ovid")

### ProQuest. 
## Exact matching.
length(import_proquest_it1$title) # Input is 4,095 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  import_proquest_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_proquest_it1$title, exact_duplicates_proquest)) # All
# combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 110 articles should be removed.
it1_dedup_proquest <- extract_unique_references(import_proquest_it1, exact_duplicates_proquest) 
length(it1_dedup_proquest$title) # Output after exact matching is 3,985. 110 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  it1_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. All but one combination are duplicates.
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, c(174)) # Override
# non-duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # 18 documents should be removed.
it1_dedup_proquest <- extract_unique_references(it1_dedup_proquest, fuzzy_duplicates_proquest) # 
# Extract unique references. 
length(it1_dedup_proquest$title) # De-duplicated output is 3,967. 18 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  it1_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. Two duplicate combinations identified.
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, c(174, 174, 174, 174,
                                                                                          174, 2067)) 
# Override non-duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # Two documents should be removed.
it1_dedup_proquest <- extract_unique_references(it1_dedup_proquest, fuzzy_duplicates_proquest) # Extract 
# unique references. 
length(it1_dedup_proquest$title) # De-duplicated output is 3,965. Three documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1/2. Mer", 
             "ged/ProQuest/2. Deduplicated"))
write_refs(it1_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_proquest")

### Scopus.
## Exact matching.
length(import_scopus_it1$title) # Input is 4,347 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  import_scopus_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_scopus_it1$title, exact_duplicates_scopus)) # Perform a 
# manual check. Four duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # Four documents should be removed.
it1_dedup_scopus <- synthesisr::extract_unique_references(import_scopus_it1, exact_duplicates_scopus) 
length(it1_dedup_scopus$title) # Output after exact matching is 4,343 . Seven documents removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  it1_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. Two duplicate combinations identified.
sum(table(fuzzy_duplicates_scopus) - 1) # Two documents should be removed.
it1_dedup_scopus <- extract_unique_references(it1_dedup_scopus, fuzzy_duplicates_scopus) # Extract unique 
# references. 
length(it1_dedup_scopus$title) # De-duplicated output is 4,341. One document removed. 
## Fuzzy matching ten.
fuzzy_duplicates_scopus <- find_duplicates( 
  it1_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. No duplicate combinations identified.
length(it1_dedup_scopus$title) # De-duplicated output is 4,341. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1/2. Mer", 
             "ged/Scopus/2. Deduplicated"))
write_refs(it1_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_scopus")

### Web of Science.
length(import_wos_it1$title) # Input is 4,749 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  import_wos_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_wos_it1$title, exact_duplicates_wos)) # Perform a 
# manual check. Five duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Five documents should be removed.
it1_dedup_wos <- synthesisr::extract_unique_references(import_wos_it1, exact_duplicates_wos) 
length(it1_dedup_wos$title) # Output after exact matching is 4,744. Four documents removed.
## Fuzzy matching five.
fuzzy_duplicates_wos <- find_duplicates( 
  it1_dedup_wos$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_wos$title, fuzzy_duplicates_wos)) # Perform a 
# manual check. No duplicate combinations identified.
sum(table(fuzzy_duplicates_wos) - 1) # Zero documents should be removed.
it1_dedup_wos <- extract_unique_references(it1_dedup_wos, fuzzy_duplicates_wos) # Extract unique 
# references. 
length(it1_dedup_wos$title) # De-duplicated output is 4,744. One document removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1/2. Mer", 
             "ged/Web of Science/2. Deduplicated"))
write_refs(it1_dedup_wos, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_wos")

### Investigate the distribution of the overlap in the first iteration. Please note that this is very 
### approximate, in that I am not sure how this function matches duplicates (I assume its exact).  
ggVennDiagram(list(it1_dedup_ovid$title, it1_dedup_proquest$title, it1_dedup_scopus$title, 
                   it1_dedup_wos$title), 
              category.names = c("Ovid", "ProQuest", "Scopus", "Web of Science"), label_alpha = 0.75, 
              label = c("count"))
## The Venn diagram seems to indicate that although there exists quite some overlap between the corpora, 
## especially between Scopus and Web of Science, that they each add a substantial number of 
## non-overlapping documents to the sum total. It might however be the case that this positively affects 
## recall and therefore negatively affects precision, in that many non-relevant documents are added by a 
## search engine like Ovid, as shown by the fact that Ovid did not as successfully retrieve the gold 
## standard articles as Scopus, ProQuest, and Web of Science did. 

### Corpus precision diagnostics. Please refer to the naive iteration for an overview of the exact steps of 
### the precision diagnostics.

## Gold standard precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
cbind(gs_group_threat$title, gs_group_threat$year) # 2 documents >= 2010.
## Ovid.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it1_dedup_ovid$title)[, 3]), 0)) # 0 of 2 
# gold standard articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it1_dedup_proquest$title)[, 3]), 0)) # 2 
# of 2 gold standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it1_dedup_scopus$title)[, 3]), 0)) # 1 of 
# 2 gold standard articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it1_dedup_wos$title)[, 3]), 0)) # 2 of 2 
# gold standard articles are retrieved.
## The gold standard precision check tentatively indicates that ProQuest, Scopus, and Web of Science have 
## superior precision to Ovid. Relative to the naive search, it furthermore has remained constant for 
## ProQuest, Scopus, Web of Science, and has decreased for Scopus (-1).

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
cbind(ex_group_threat$title, ex_group_threat$year) # Data frame of title and year of publication of 44 
# external articles.
## Ovid.
sum(round(as.numeric(check_recall(ex_group_threat$title, it1_dedup_ovid$title)[, 3]), 0)) # 6 of 44 
# external articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_group_threat$title, it1_dedup_proquest$title)[, 3]), 0)) # 16 of 44 
# external articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_group_threat$title, it1_dedup_scopus$title)[, 3]), 0)) # 19 of 44 
# external articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_group_threat$title, it1_dedup_wos$title)[, 3]), 0)) # 25 of 44
# external articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus, ProQuest, and Ovid, respectively. Relative to the naive search, the precision with 
## respect to the external articles has increased in Ovid (+1), ProQuest (+4), and Scopus (+5), but has 
## decreased in Web of Science (-1). 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
it1_dedup <- bind_rows(it1_dedup_ovid, it1_dedup_proquest, it1_dedup_scopus, it1_dedup_wos) # Merge corpora. 
## Exact matching.
length(it1_dedup$title) # Input is 15,514 documents.
exact_duplicates <- synthesisr::find_duplicates(
  it1_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(it1_dedup$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 8,707 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 5,317 documents should be removed.
it1_dedup <- synthesisr::extract_unique_references(it1_dedup, exact_duplicates) 
length(it1_dedup$title) # Output after exact matching is 10,197. 5,317 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
fuzzy_manual <- review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check.
length(fuzzy_manual$title) # 1,004 potential duplicate combinations. I check these candidates manually in 
# batches. Note that this procedure is prone to error.  
fuzzy_manual$title[1:500] # 2580. Remaining combinations are duplicates.
fuzzy_manual$title[501:1004] # All combinations are duplicates. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(2580)) 
sum(table(fuzzy_duplicates) - 1) # 508 document should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references.
length(it1_dedup$title) # De-duplicated output is 9,689. 508 documents were removed.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 304 duplicate combinations.
fuzzy_manual$title # 55, 87, 803, 828, 1984, 1987, 2086, 2343, 2406, 2569, 2569, 2569, 2569, 2569, 4141, 
# 4141, 4141, 4146, 5585, are not duplicate combinations. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(55, 87, 803, 828, 1984, 1987, 2086, 
                                                                        2343, 2406, 2569, 2569, 2569, 2569, 
                                                                        2569, 4141, 4141, 4141, 4146, 5585)) 
sum(table(fuzzy_duplicates) - 1) # 136 documents should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references. 
length(it1_dedup$title) # De-duplicated output is 9,553. 136 document removed. 
## Fuzzy matching fifteen.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Fuzzy matching ten output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 15) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 149 duplicate combinations.
fuzzy_manual$title # 5, 7, 53, 85, 95, 216, 340, 408, 535, 592, 735, 796, 808, 821, 909, 909, 909, 909, 909, 
# 909, 909, 909, 909, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 1009,
# 1209, 1523, 1738, 1890, 1890, 1969, 1972, 2071, 2327, 2390, 2954, 3063, 3147, 3631, 4528, 4528, 4668, 5060,
# 5187, 5552.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(5, 7, 53, 85, 95, 216, 340, 408, 535, 592, 735, 796, 
                                                      808, 821, 909, 909, 909, 909, 909, 909, 909, 909, 909, 
                                                      915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 915, 
                                                      915, 915, 915, 915, 915, 1009, 1209, 1523, 1738, 1890, 
                                                      1890, 1969, 1972, 2071, 2327, 2390, 2954, 3063, 3147, 
                                                      3631, 4528, 4528, 4668, 5060, 5187, 5552)) 
sum(table(fuzzy_duplicates) - 1) # 27 documents should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references. 
length(it1_dedup$title) # De-duplicated output is 9,526. 27 document removed. 
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1/2. Merged")
write_refs(it1_dedup, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_refman")
# EndNote and Zotero indicate that 44 duplicates remain in the .ris file, which are removed. We furthermore 
# remove five retracted papers that are flagged in EndNote: "Interprofessional learning in acute care: 
# Developing a theoretical framework", "The Evolution of Intergroup Bias: Perceptions and Attitudes in 
# Rhesus Macaques", "The Social Context of Latino Threat and Punitive Latino Sentiment", "When contact 
# changes minds: An experiment on transmission of support for gay equality", and "Ethnic threat and social 
# control: Examining public support for judicial use of ethnicity in punishment". The final paper has two 
# records, one for the original paper, one for the retraction. The result is exported from Zotero in the file 
# "it1_dedup.ris" which is subsequently imported.  
it1_dedup <- read_bibliography("it1_dedup.ris")
length(it1_dedup$title) # 9,476 documents. 50 documents removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it1_dedup$title)[, 3]), 0)) # 2 of 2 
# gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_group_threat$title, it1_dedup$title)[, 3]), 0)) # 29 of 44 external 
# articles are retrieved.

### Evaluate first iteration search relative to naive search. 

## Here I do not use the "check_recall" function because it takes a long time, and it is not exact / 
## conservative enough, for the purpose of merging the naive search documents that are not in the first 
## iteration search. I do this to try and maximize coverage. 
length(naive_dedup$title) # 1,042 documents.
length(it1_dedup$title) # 9,476 documents. 
overlap_naive_it1 <- bind_rows(naive_dedup, it1_dedup) # Merge corpora. Should be 1,042 + 9,476 = 10,518. 
## Exact matching.
length(overlap_naive_it1$title) # Input is 10,508 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_naive_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_naive_it1$title, exact_duplicates) # Perform a manual
# check.
length(exact_manual$title) # Sum of 1,326 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 663 documents should be removed.
it1_dedup_out <- synthesisr::extract_unique_references(overlap_naive_it1, exact_duplicates) 
length(it1_dedup_out$title) # Output after exact matching is 9,855. 663 documents removed. 379 documents 
# from the naive search not in the first iterations search remain.
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it1_dedup_out$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it1_dedup_out$title, fuzzy_duplicates)) # Perform a manual check. All but
# one combinations are duplicates.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(9818))
sum(table(fuzzy_duplicates) - 1) # 11 documents should be removed.
it1_dedup_out <- extract_unique_references(it1_dedup_out, fuzzy_duplicates) # Extract unique references.
length(it1_dedup_out$title) # De-duplicated output is 9,844. 11 documents were removed. 368 documents remain.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it1_dedup_out$title, # Fuzzy matching five as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # default cutoff. 
(fuzzy_manual <- review_duplicates(it1_dedup_out$title, fuzzy_duplicates)) # Perform a manual check. One 
# duplicate combination identified. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(836, 1092, 1122, 1780, 1802, 2850,
                                                                        2853, 2945, 3184, 3245, 6209, 9799,
                                                                        9799, 9799, 9799, 9800, 9804, 9806,
                                                                        9806))
sum(table(fuzzy_duplicates) - 1) # One document should be removed.
it1_dedup_out <- extract_unique_references(it1_dedup_out, fuzzy_duplicates) # Extract unique references.
length(it1_dedup_out$title) # De-duplicated output is 9,843. 1 document was removed. 367 documents remain.
## Save .ris file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/3. Iteration 1")
write_refs(it1_dedup_out, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_out_refman") # Export
# as .ris. 
write_refs(it1_dedup_out, format = "bib", tag_naming = "synthesisr", file = "it1_dedup_out_refman") # Export
# as .bib for Zotero. 
# EndNote and Zotero indicate that two duplicates remain in the .ris file, which are removed. The result is 
# exported from Zotero in the file "it1_dedup_out.ris" which is subsequently imported.  
it1_dedup_out <- read_bibliography("it1_dedup_out.ris")
length(it1_dedup_out$title) # 9,841 documents. Two documents removed. This is the final corpus file of the 
# first iteration search.

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it1_dedup_out$title)[, 3]), 0)) # 2 of 2 
# gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_group_threat$title, it1_dedup_out$title)[, 3]), 0)) # 29 of 44  external 
# articles are retrieved.

## Inspect coverage gain relative to the naive search. I define coverage as the sum of the additional 
## documents that were found relative to the previous search iteration, and the documents from the previous 
## search iteration that were not identified in the current one. 
length(naive_dedup$title) # 1,042 articles.
length(it1_dedup$title) # 9,476 articles.
(length(it1_dedup$title) - length(naive_dedup$title)) # 9,476 - 1,042 = 8,434 more documents identified. 
(length(it1_dedup_out$title) - length(it1_dedup$title)) # ~365 documents from the naive search not in the 
# first iteration search. 
# So total coverage increase is: 8,434 + 365 = 8,799 documents which is a factor increase of 8,799 / 1,042 = 
# 8.44. 
(1 - (length(it1_dedup_out$title) - length(it1_dedup$title)) / length(naive_dedup$title)) * 100 # 64.97% of 
# the naive search was in the first iteration search. 

## Since the overlap with the previous iteration is only moderate and the coverage increase differential is 
## very large, we continue with a second iteration.

#########################################
##### Iteration 2 Literature Search #####
#########################################

### Now repeat the previous procedure, but with keywords extracted from the first iteration search corpus.  

### Clear the environment except for the gold standard search terms, the validation article sets, and 
### the "naive_dedup", and "it1_dedup_out" objects. 
rm(list = setdiff(ls(), c("gs_grouped_terms", "gs_group_threat", "ex_group_threat", "naive_dedup", 
                          "it1_dedup_out")))

### Start by checking how many documents in the first iteration search corpus provide keywords, titles, and 
### abstracts.
## Keywords.
length(it1_dedup_out$keywords) # Total number of documents is 9,841. 
length(it1_dedup_out$keywords) - sum(is.na(it1_dedup_out$keywords)) # All of the articles list keywords.  
## Titles and abstracts.
length(it1_dedup_out$title) - sum(is.na(it1_dedup_out$title)) # All of the articles list a title.  
length(it1_dedup_out$abstract) - sum(is.na(it1_dedup_out$abstract)) # All of the articles list an abstract.  

### The first step is to obtain the keywords listed in the articles. We extract all non-single keywords that 
### are listed a minimum of forty times. Note that the forty number is arbitrary insofar that we need to 
### strike a balance between retrieving enough but not too many keyword candidates. More specifically,
### Gusenbauer & Haddaway (2020) note that search terms of length twenty-five should allow reviewers to 
### specify theirsearch scope to a reasonable extent. Put differently, our Boolean search should contain at 
### least twenty-five search terms. On the other hand, the maximum search string length that Ovid, Scopus, 
### and Web of Science allow according to Gusenbauer & Haddaway (2020) is around 1000. Web of Science for 
### example states that the maximum number of terms allowed in one field is fifty terms when using 
### "All Fields". As such, this number of keywords is a natural cap to the number of keywords that we can 
### seek to obtain from a corpora set. To reiterate, the number of keywords that will be incorporated in the 
### final searches will have a lower limit of 25 search terms, and an upper limit of 1000 characters in the 
### search string length, which translates to around fifty to sixty search terms (depending on the length of 
### the search terms).
tagged_keywords <- litsearchr::extract_terms(
  keywords = it1_dedup_out$keywords, # This is a list with the keywords from the articles. 
  method = "tagged", # Indicate that we want to obtain the keywords from the provided list.
  min_freq = 40, # The keyword has to occur a minimum of forty times to be included.   
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(tagged_keywords) # 300 tagged keywords.
### Now use the RAKE to obtain keywords from the titles and abstracts of the search corpus. We extract all 
### non-single keywords that occur at least forty times in these titles and abstracts. 
raked_keywords <- litsearchr::extract_terms(
  text = paste(it1_dedup_out$title, it1_dedup_out$abstract), # This is a list of titles and abstracts.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 40, # The keyword has to occur a minimum of forty times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(raked_keywords) # 485 raked keywords.
## Sum total of tagged and raked keywords is 300 + 458 = 758. 
keyword_candidates <- remove_redundancies(c(tagged_keywords, raked_keywords), closure = "full") # Remove
# duplicate terms.
length(keyword_candidates) # Total of 640 keyword candidates. 
## The subsequent task is to select all keywords that are relevant to our query from the candidate pool. I 
## select terms that relate in content to the relevant literature, i.e., are a independent or dependent 
## variable of interest in the group threat on inter-ethnic attitudes relationship. Note that I interpret 
## relevance quite broadly, since these terms will be will be ranked and filtered further based on their 
## "connectedness" in a co-occurrence network. In that sense, it is better too be too liberal than too 
## selective in our choices here, since it might be hard to anticipate which relevant terms are important 
## from this "connectedness" perspective. I furthermore do not include keywords which relate directly to any 
## of the other paradigms, e.g., I do not include keywords on contact theory even though these might appear 
## often in the group threat literature. Finally note that I do this part manually, which is prone to errors. 
all_keywords <- keyword_candidates[
  c(5, 6, 16, 22, 32, 36, 44, 49, 50, 52, 54, 58, 59, 62, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 88,
    89, 90, 91, 92, 93, 94, 95, 96, 116, 117, 118, 120, 125, 126, 127, 128, 130, 131, 132, 135, 145, 153,
    154, 155, 156, 161, 165, 170, 171, 172, 175, 194, 196, 197, 198, 202, 203, 204, 205, 206, 207, 209, 210, 
    213, 214, 215, 230, 231, 234, 236, 237, 239, 240, 242, 243, 244, 245, 247, 248, 249, 250, 251, 252, 253,
    259, 260, 264, 265, 270, 271, 272, 280, 283, 313, 314, 315, 352, 356, 357, 358, 360, 362, 363, 369, 370, 
    371, 372, 373, 374, 375, 387, 408, 409, 410, 411, 412, 413, 418, 419, 420, 421, 422, 423, 424, 425, 427,
    428, 430, 435, 436, 437, 438, 441, 448, 449, 450, 454, 456, 457, 458, 459, 460, 461, 471, 478, 480, 487, 
    488, 489, 490, 502, 503, 504, 507, 510, 511, 516, 517, 526, 527, 540, 541, 544, 545, 546, 547, 548, 551,
    555, 584, 585, 586, 587, 588, 595, 618, 624, 625, 626) 
]
## Manual cleaning. 
all_keywords <- all_keywords[-c(1)] # Remove "0410 group interactions".
all_keywords[1] <- "group interactions" # Re-format.
all_keywords[157] <- "perceived group size" # Re-format.
all_keywords <- append(all_keywords, c("group size")) # Add "group size". 
(all_keywords <- sort(all_keywords))
length(all_keywords) # 186 keyword candidates. 

### We further filter the keyword set by ranking the relative strength of each keyword in a so-called keyword 
### co-occurrence network (KCN). In a KCN, each keyword is "represented as a node and each co-occurrence of 
### a pair of words is represented as a link" (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). "The number 
### of times that a pair of words co-occurs in multiple articles constitutes the weight of the link 
### connecting the pair" (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). "The network constructed in this 
### manner represents cumulative knowledge of a domain and helps to uncover meaningful knowledge components 
### and insights based on the patterns and strength of links between keywords that appear in the literature" 
### (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). I furthermore combine the second iteration keywords 
## with the naive keywords. I add the naive keywords to the keyword candidate selection under the assumption 
## that these are important keywords because they were obtained from the gold standard articles, and should 
## therefore be considered explicitly in the KCN analysis, even though they are implicitly represented.
all_keywords_final <- c()
all_keywords_final <- append(all_keywords_final, c(as.vector(unlist(gs_grouped_terms)), all_keywords))
all_keywords_final <- remove_redundancies(all_keywords_final, closure = "full") # Remove duplicates. 
length(all_keywords_final) # 204 candidates.
## Build the keyword co-occurrence network. This chunk of code is a reworked version of the tutorial at: 
## https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html.
filter_dfm <- litsearchr::create_dfm(
  elements = paste(it1_dedup_out$title, it1_dedup_out$abstract), # The input in which the keywords can 
  # co-occur, titles and abstracts. 
  features = all_keywords_final) # The keyword candidates. 
kcn <- create_network(filter_dfm) # Create a KCN.
## Rank keywords based on strength in the co-occurrence network.
strengths <- strength(kcn) # Calculate strength values. 
data.frame(term = names(strengths), strength = strengths, row.names = NULL) %>%
  mutate(rank = rank(strength, ties.method = "min")) %>%
  arrange(desc(strength)) ->
  term_strengths # Create a data frame where the keywords are sorted by strength, in descending order. 
## I now apply a filter to select 57 and 42 terms for the search string in OVID, Scopus, and Web of Science 
## and ProQuest, respectively, where I remove terms which refer to an equivalent or near equivalent concept, 
## i.e., "target group" and "target groups", where I select the term with the highest strength value. 
(term_strengths <- cbind(seq(length(term_strengths$term)), term_strengths[order(term_strengths$term), ]))
term_strengths <- term_strengths[-c(5, 6, 20, 34, 35, 36, 39, 41, 48, 53, 56, 60, 64, 68, 71, 74, 75, 76, 89, 
                                    93, 98, 100, 101, 104, 108, 109, 113, 116, 118, 120, 122, 128, 133, 160, 
                                    145, 148, 163, 165, 166, 167, 174, 175, 176, 178, 179, 184, 185, 187, 
                                    188, 193), ]

### Construct Boolean search OVID, Web of Science, and Scopus.
## Select first 55 terms from filtered term set. 
keywords_ovid_scopus_wos <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:55, ]$term)
(keywords_ovid_scopus_wos <- keywords_ovid_scopus_wos[order(keywords_ovid_scopus_wos)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2")
sink("second_iteration_selected_terms_ovid_scopus_wos.txt")
print(keywords_ovid_scopus_wos)
sink() 
## Categorize "keywords_ovid_scopus_wos" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_ovid_scopus_wos <- list(
  determinant = keywords_ovid_scopus_wos[c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 21, 24, 26, 
                                           27, 28, 29, 32, 33, 34, 43, 45, 46, 47, 48, 49, 50, 51, 52, 53, 
                                           54, 55)],
  outcome = keywords_ovid_scopus_wos[c(1, 2, 3, 19, 20, 22, 23, 25, 30, 31, 35, 36, 37, 38, 39, 40, 41, 42, 
                                       44)])
grouped_terms_ovid_scopus_wos
### Given the grouped terms, write the Boolean search for OVID, Web of Science, and Scopus to the 
### directory and print it to the console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2") # Set directory.
write_search(
  grouped_terms_ovid_scopus_wos, # The list of determinant and outcome keywords identified in the naive 
  # document set.
  languages = "English", # Language to write the search in, here set to English.
  exactphrase = TRUE, # Whether terms that consist of more than one word should be matched exactly rather 
  # than as two separate words. Set to TRUE, to limit both the scope and the number of redundant documents.
  stemming = TRUE, # Whether words are stripped down to the smallest meaningful part of the word to catch all 
  # variants of the word. Set to TRUE.
  closure = "none", # Whether partial matches are matched at the left end of a word ("left"), at the right 
  # ("right"), only as exact matches ("full"), or as any word containing a term ("none").  Set to "none" to 
  # obtain as many documents as possible.  
  writesearch = TRUE) # Whether we would like to write the search text to a file in the current directory. 
# Set to TRUE.
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- gsub('\\', "", bool, fixed = TRUE)
## Write the Boolean search for Ovid. 
bool <- gsub("((", "(", bool, fixed = T)
bool <- gsub("))", ")", bool, fixed = T)
cat(bool)
writeLines(bool, "bool_ovid_it2.txt")
## Write the Boolean search for Scopus. 
writeLines(bool, "bool_scopus_it2.txt")
## Write the Boolean search for Web of Science. 
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- paste0('ALL = ', bool, 'AND PY = (2010-2022)')
writeLines(bool, "bool_wos_it2.txt")
file.remove("search-inEnglish.txt") # Remove source file. 
## Finally save the grouped terms to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate for any particular reason.
sink("second_iteration_search_terms_ovid_scopus_wos.txt")
print(grouped_terms_ovid_scopus_wos)
sink() 

### Construct Boolean search ProQuest.
## Select first 34 terms from filtered term set.
keywords_proquest <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:34, ]$term)
(keywords_proquest <- keywords_proquest[order(keywords_proquest)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2")
sink("second_iteration_selected_terms_proquest.txt")
print(keywords_proquest)
sink() 
## Categorize "keywords_proquest" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_proquest <- list(
  determinant = keywords_proquest[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 15, 16, 18, 19, 26, 28, 29, 30, 31, 32, 
                                    33, 34)],
  outcome = keywords_proquest[c(10, 11, 12, 14, 17, 20, 21, 22, 23, 24, 25, 27)])
grouped_terms_proquest$determinant <- grouped_terms_proquest$determinant[c(-19)] # Remove "social group" 
# because it blows ups the search in ProQuest.
### Given the grouped terms, write the the Boolean search for ProQuest to the directory and print it to the 
### console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2") # Set directory.
write_search(
  grouped_terms_proquest, # The list of determinant and outcome keywords identified in the naive document 
  # set.
  languages = "English", # Language to write the search in, here set to English.
  exactphrase = TRUE, # Whether terms that consist of more than one word should be matched exactly rather 
  # than as two separate words. Set to TRUE, to limit both the scope and the number of redundant documents.
  stemming = TRUE, # Whether words are stripped down to the smallest meaningful part of the word to catch all 
  # variants of the word. Set to TRUE.
  closure = "none", # Whether partial matches are matched at the left end of a word ("left"), at the right 
  # ("right"), only as exact matches ("full"), or as any word containing a term ("none").  Set to "none" to 
  # obtain as many documents as possible.  
  writesearch = TRUE) # Whether we would like to write the search text to a file in the current directory. 
# Set to TRUE.
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- gsub('\\', "", bool, fixed = TRUE)
## Write the Boolean search for ProQuest. 
bool <- gsub("((", "(", bool, fixed = T)
bool <- gsub("))", ")", bool, fixed = T)
cat(bool)
writeLines(bool, "bool_proquest_it2.txt")
file.remove("search-inEnglish.txt") # Remove source file. 
## Finally save the grouped terms to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate for any particular reason.
sink("second_iteration_search_terms_proquest.txt")
print(grouped_terms_proquest)
sink() 

### Please refer to the naive iteration for an overview of the exact steps of the search procedure.

### All searches were conducted on 10-07-2022. These resulted in 7,302 documents from Ovid (Selection: 
### PsycINFO), 4,649 documents from Proquest (Selection: Sociological Abstracts), 8,358 documents from Scopus 
### (Selection: Full index), and 9,067 documents from Web of Science (Selection: Web of Science Core 
### Collection), for a total of 29,367 documents. Please note that this document set is unfiltered, i.e., 
### duplicates, retracted documents, unidentified non-English documents, etc., have not yet been removed. In 
### Ovid, the documents were manually extracted in three batches. In ProQuest the documents were manually 
### extracted in 100-sized batches. In Scopus, the documents were manually extracted in 2000 document sized 
### batches, stratified by publication year. In Web of Science, the documents were manually extracted in 1000 
### document sized batches. In retrospect I finally made a mistake in the Ovid search, were I selected the
### "Abstracts" box instead of the "Peer reviewed journals" box, which resulted in ~1500 more documents than 
### would otherwise have been retrieved.

### Data import and cleaning.
## Import results of informed search. Note that the batches for each search system were merged in EndNote.
## Start by checking whether the imported length is equal to the length of the raw .ris files.
import_ovid_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iterat", 
                     "ion 2/1. Unmerged/Ovid"),
  verbose = TRUE)
import_proquest_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iterat", 
                     "ion 2/1. Unmerged/ProQuest"),
  verbose = TRUE)
import_scopus_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iterat", 
                     "ion 2/1. Unmerged/Scopus"),
  verbose = TRUE)
import_wos_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iterat", 
                     "ion 2/1. Unmerged/Web of Science"),
  verbose = TRUE)
length(import_ovid_it2$title) # 7,302, which is correct.  
length(import_proquest_it2$title) # 4,649, which is correct. 
length(import_scopus_it2$title) # 8,358, which is correct.  
length(import_wos_it2$title) # 9,067, which is correct.  

## We subsequently identify and remove identifiable, non-English documents, if necessary. We then save the 
## resulting file.
# Ovid.
table(import_ovid_it2$language) # Ovid. All documents in the .ris file are in English
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2/2. Me", 
             "rged/Ovid/1. Raw"))
write_refs(import_ovid_it2, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
# ProQuest.
table(import_proquest_it2$language) # ProQuest. All documents in the .ris file are in English. 
import_proquest_it2 <- import_proquest_it2[import_proquest_it2$language == "English" ,] # Select English 
# documents and store them. 4,644 documents in ProQuest document set.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2/2. Me", 
             "rged/ProQuest/1. Raw"))
write_refs(import_proquest_it2, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
# Scopus.
table(import_scopus_it2$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2/2. Me", 
             "rged/Scopus/1. Raw"))
write_refs(import_scopus_it2, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
# Web of Science.
table(import_wos_it2$language) # Web of Science. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2/2. Me", 
             "rged/Web of Science/1. Raw"))
write_refs(import_wos_it2, format = "ris", tag_naming = "synthesisr", file = "web_of_science_r")

### De-duplication. Please refer to the naive iteration for an overview of the exact steps of the 
### de-deplication procedure.

### Ovid.
## Exact matching.
length(import_ovid_it2$title) # Input is 7,302 documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  import_ovid_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(import_ovid_it2$title, exact_duplicates_ovid) # Perform a 
# manual check. 
length(exact_manual$title) # 43 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # 24 documents should be removed.
it2_dedup_ovid <- synthesisr::extract_unique_references(import_ovid_it2, exact_duplicates_ovid) 
length(it2_dedup_ovid$title) # Output after exact matching is 7,278. One document removed.
## Fuzzy matching five.
fuzzy_duplicates_ovid <- find_duplicates( 
  it2_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. Three duplicate combination identified.
fuzzy_duplicates_ovid <- synthesisr::override_duplicates(fuzzy_duplicates_ovid, c(126, 2581, 2581, 2581, 
                                                                                  2819, 4348, 6080)) # 
# Override non-duplicates.
sum(table(fuzzy_duplicates_ovid) - 1) # Three documents should be removed.
it2_dedup_ovid <- extract_unique_references(it2_dedup_ovid, fuzzy_duplicates_ovid) # Extract unique 
# references. 
length(it2_dedup_ovid$title) # De-duplicated output is 7,275. One document removed. 
## Fuzzy matching ten.
fuzzy_duplicates_ovid <- find_duplicates( 
  it2_dedup_ovid$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. One duplicate combination identified.
fuzzy_duplicates_ovid <- synthesisr::override_duplicates(fuzzy_duplicates_ovid, 
                                                         c(79, 126, 126, 176, 271, 448, 448, 448, 1236, 1236,
                                                          1236, 2117, 2165, 2267, 2344, 2344, 2407, 2407, 
                                                          2574, 2574, 2574, 2574, 2574, 2632, 2811, 2811,
                                                          2811, 3048, 3279, 3285, 4236, 4236, 4335, 4335,
                                                          4335, 4718, 5230, 5318, 5457, 5788, 5894, 6204, 
                                                          6805, 6971, 7045)) # 
# Override non-duplicates.
sum(table(fuzzy_duplicates_ovid) - 1) # One document should be removed.
it2_dedup_ovid <- extract_unique_references(it2_dedup_ovid, fuzzy_duplicates_ovid) # Extract unique 
# references. 
length(it2_dedup_ovid$title) # De-duplicated output is 7,274. One document removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2/2. Mer", 
             "ged/Ovid/2. Deduplicated"))
write_refs(it2_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_ovid")

### ProQuest. 
## Exact matching.
length(import_proquest_it2$title) # Input is 4,644 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  import_proquest_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_proquest_it2$title, exact_duplicates_proquest)) # All
# combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 134 articles should be removed.
it2_dedup_proquest <- extract_unique_references(import_proquest_it2, exact_duplicates_proquest) 
length(it2_dedup_proquest$title) # Output after exact matching is 4,510. 134 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  it2_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. All combinations are duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # 19 documents should be removed.
it2_dedup_proquest <- extract_unique_references(it2_dedup_proquest, fuzzy_duplicates_proquest) # 
# Extract unique references. 
length(it2_dedup_proquest$title) # De-duplicated output is 4,491. 19 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  it2_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. Four duplicate combinations identified.
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, c(717, 717, 717)) 
# Override non-duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # Four documents should be removed.
it2_dedup_proquest <- extract_unique_references(it2_dedup_proquest, fuzzy_duplicates_proquest) # Extract 
# unique references. 
length(it2_dedup_proquest$title) # De-duplicated output is 4,487. Four documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2/2. Mer", 
             "ged/ProQuest/2. Deduplicated"))
write_refs(it2_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_proquest")

### Scopus.
## Exact matching.
length(import_scopus_it2$title) # Input is 8,358 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  import_scopus_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_scopus_it2$title, exact_duplicates_scopus)) # Perform a 
# manual check. All combinations are duplicates.
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # Eight documents should be removed.
it2_dedup_scopus <- synthesisr::extract_unique_references(import_scopus_it2, exact_duplicates_scopus) 
length(it2_dedup_scopus$title) # Output after exact matching is 8,350 . Seven documents removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  it2_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. Two duplicate combinations identified.
fuzzy_duplicates_scopus <- synthesisr::override_duplicates(fuzzy_duplicates_scopus, c(4488)) 
# Override non-duplicates.
sum(table(fuzzy_duplicates_scopus) - 1) # Two documents should be removed.
it2_dedup_scopus <- extract_unique_references(it2_dedup_scopus, fuzzy_duplicates_scopus) # Extract unique 
# references. 
length(it2_dedup_scopus$title) # De-duplicated output is 8,348. Two documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_scopus <- find_duplicates( 
  it2_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. No duplicate combinations identified.
length(it2_dedup_scopus$title) # De-duplicated output is 8,348. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2/2. Mer", 
             "ged/Scopus/2. Deduplicated"))
write_refs(it2_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_scopus")

### Web of Science.
length(import_wos_it2$title) # Input is 9,067 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  import_wos_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_wos_it2$title, exact_duplicates_wos)) # Perform a 
# manual check. Six duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Six documents should be removed.
it2_dedup_wos <- synthesisr::extract_unique_references(import_wos_it2, exact_duplicates_wos) 
length(it2_dedup_wos$title) # Output after exact matching is 9,061. Four documents removed.
## Fuzzy matching five.
fuzzy_duplicates_wos <- find_duplicates( 
  it2_dedup_wos$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_wos$title, fuzzy_duplicates_wos)) # Perform a 
# manual check. No duplicate combinations identified.
sum(table(fuzzy_duplicates_wos) - 1) # Zero documents should be removed.
it2_dedup_wos <- extract_unique_references(it2_dedup_wos, fuzzy_duplicates_wos) # Extract unique 
# references. 
length(it2_dedup_wos$title) # De-duplicated output is 9,061. One document removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2/2. Mer", 
             "ged/Web of Science/2. Deduplicated"))
write_refs(it2_dedup_wos, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_wos")

### Investigate the distribution of the overlap in the first iteration. Please note that this is very 
### approximate, in that I am not sure how this function matches duplicates (I assume its exact).  
ggVennDiagram(list(it2_dedup_ovid$title, it2_dedup_proquest$title, it2_dedup_scopus$title, 
                   it2_dedup_wos$title), 
              category.names = c("Ovid", "ProQuest", "Scopus", "Web of Science"), label_alpha = 0.75, 
              label = c("count"))
## The Venn diagram seems to indicate that although there exists quite some overlap between the corpora, 
## especially between Scopus and Web of Science, that they each add a substantial number of 
## non-overlapping documents to the sum total. It might however be the case that this positively affects 
## recall and therefore negatively affects precision, in that many non-relevant documents are added by a 
## search engine like Ovid, as shown by the fact that Ovid did not as successfully retrieve the gold 
## standard articles as Scopus, ProQuest, and Web of Science did. 

### Corpus precision diagnostics. Please refer to the naive iteration for an overview of the exact steps of 
### the precision diagnostics.

## Gold standard precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
cbind(gs_group_threat$title, gs_group_threat$year) # 2 documents >= 2010.
## Ovid.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it2_dedup_ovid$title)[, 3]), 0)) # 0 of 2 
# gold standard articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it2_dedup_proquest$title)[, 3]), 0)) # 1 
# of 2 gold standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it2_dedup_scopus$title)[, 3]), 0)) # 2 of 
# 2 gold standard articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it2_dedup_wos$title)[, 3]), 0)) # 2 of 2 
# gold standard articles are retrieved.
## The gold standard precision check tentatively indicates that ProQuest, Scopus, and Web of Science have 
## superior precision to Ovid. It furthermore has decreased relative to the first iteration search for 
## ProQuest (-1), and increased for Scopus (+1).

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
cbind(ex_group_threat$title, ex_group_threat$year) # Data frame of title and year of publication of 44 
# external articles.
## Ovid.
sum(round(as.numeric(check_recall(ex_group_threat$title, it2_dedup_ovid$title)[, 3]), 0)) # 8 of 44 
# external articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_group_threat$title, it2_dedup_proquest$title)[, 3]), 0)) # 14 of 44 
# external articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_group_threat$title, it2_dedup_scopus$title)[, 3]), 0)) # 24 of 44 
# external articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_group_threat$title, it2_dedup_wos$title)[, 3]), 0)) # 29 of 44 
# external articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus, ProQuest, and Ovid, respectively. Relative to the first iteration search, the 
## precision with respect to the external articles has increased in Ovid (+2), Scopus (+5), and Web of 
## Science (+4), but decreased in ProQuest (-2). 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
it2_dedup <- bind_rows(it2_dedup_ovid, it2_dedup_proquest, it2_dedup_scopus, it2_dedup_wos) # Merge corpora. 
## Exact matching.
length(it2_dedup$title) # Input is 29,170 documents.
exact_duplicates <- synthesisr::find_duplicates(
  it2_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(it2_dedup$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 16,959 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 10,313 documents should be removed.
it2_dedup <- synthesisr::extract_unique_references(it2_dedup, exact_duplicates) 
length(it2_dedup$title) # Output after exact matching is 18,857.  documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it2_dedup$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
fuzzy_manual <- review_duplicates(it2_dedup$title, fuzzy_duplicates) # Perform a manual check.
length(fuzzy_manual$title) #1,589 potential duplicate combinations. I check these candidates manually in 
# batches. Note that this procedure is prone to error.  
fuzzy_manual$title[1:500] # 126, 2574, 2574, 2574, 2574. Remaining combinations are duplicates.
fuzzy_manual$title[501:1000] # 2811, 7255. Remaining combinations are duplicates. 
fuzzy_manual$title[1001:1589] # 12515. Remaining combinations are duplicates. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(126, 2574, 2574, 2574, 2574, 2811, 
                                                                        7255, 12515)) 
sum(table(fuzzy_duplicates) - 1) # 798 document should be removed.
it2_dedup <- extract_unique_references(it2_dedup, fuzzy_duplicates) # Extract unique references.
length(it2_dedup$title) # De-duplicated output is 18,059. 798 documents were removed.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it2_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it2_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 580 duplicate combinations.
fuzzy_manual$title # 79, 126, 126, 176, 221, 271, 319, 448, 448, 448, 1233, 1236, 1236, 1236, 2117, 2165, 
# 2267, 2344, 2344, 2344, 2344, 2407, 2407, 2574, 2574, 2574, 2574, 2574, 2574, 2574, 2632, 2811, 2811, 2811,
# 3279, 3285, 4236, 4236, 4335, 4335, 4718, 4720, 5230, 5318, 5457, 5474, 5680, 5711, 5788, 5894, 6204, 6805, 
# 6971, 7045, 8639, 8964, 8964, 11607, 12393, are not duplicate combinations. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(79, 126, 126, 176, 221, 271, 319, 448, 448, 448, 1233, 
                                                      1236, 1236, 1236, 2117, 2165, 2267, 2344, 2344, 2344, 
                                                      2344, 2407, 2407, 2574, 2574, 2574, 2574, 2574, 2574, 
                                                      2574, 2632, 2811, 2811, 2811, 3279, 3285, 4236, 4236, 
                                                      4335, 4335, 4718, 4720, 5230, 5318, 5457, 5474, 5680, 
                                                      5711, 5788, 5894, 6204, 6805, 6971, 7045, 8639, 8964, 
                                                      8964, 11607, 12393)) 
sum(table(fuzzy_duplicates) - 1) # 241 documents should be removed.
it2_dedup <- extract_unique_references(it2_dedup, fuzzy_duplicates) # Extract unique references. 
length(it2_dedup$title) # De-duplicated output is 17,818. 241 document removed. 
## Fuzzy matching fifteen.
fuzzy_duplicates <- find_duplicates(
  it2_dedup$title, # Fuzzy matching ten output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 15) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it2_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 413 duplicate combinations.
fuzzy_manual$title # 10, 13, 44, 44, 77, 85, 124, 124, 174, 204, 219, 219,  219, 219, 219, 219, 219, 219, 
# 219, 219, 219, 219, 219, 219, 219, 219, 219, 219, 219, 269, 289, 290, 316, 316, 316, 316, 316, 316, 316, 
# 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 438, 438, 438, 445, 445, 445, 445, 445, 445, 
# 445, 445, 593, 601, 647, 703, 724, 787, 959, 1066, 1127, 1165, 1167, 1227, 1227, 1227, 1227, 1227, 1327, 
# 1406, 1426, 1455, 1587, 1808, 1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938, 1947, 2088, 2088, 2088, 2088, 
# 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 
# 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2101, 2149, 2153, 2169, 2251, 2266, 2292, 2667, 2739, 2739, 2739, 
# 2739, 2739, 2739, 2739, 2763, 3019, 3054, 3169, 3204, 3223, 3223, 3244, 3250, 3386, 3386, 3386, 3594, 3862, 
# 4004, 4075, 4224, 4240, 4304, 4331, 4399, 4473, 4665, 4667, 4667, 4667, 4687, 4956, 4956, 4963, 5172, 5177, 
# 5177, 5177, 5260, 5301, 5354, 5395, 5412, 5534, 5618, 5648, 5754, 5829, 6075, 6157, 6186, 6262, 6387, 6531, 
# 6531, 6575, 6607, 6676, 6725, 6882, 6916, 6959, 7088, 7088, 7129, 7727, 7916, 7916, 9304, 9492, 9933, 
# 10839, 11472, 12250, 15576, are not duplicate combinations.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(10, 13, 44, 44, 77, 85, 124, 124, 174, 204, 219, 219, 
                                                      219, 219, 219, 219, 219, 219, 219, 219, 219, 219, 219, 
                                                      219, 219, 219, 219, 219, 219, 269, 289, 290, 316, 316, 
                                                      316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 316, 
                                                      316, 316, 316, 316, 316, 316, 438, 438, 438, 445, 445, 
                                                      445, 445, 445, 445, 445, 445, 593, 601, 647, 703, 724, 
                                                      787, 959, 1066, 1127, 1165, 1167, 1227, 1227, 1227, 
                                                      1227, 1227, 1327, 1406, 1426, 1455, 1587, 1808, 1938, 
                                                      1938, 1938, 1938, 1938, 1938, 1938, 1938, 1947, 2088, 
                                                      2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 
                                                      2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 
                                                      2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 
                                                      2088, 2101, 2149, 2153, 2169, 2251, 2266, 2292, 2667, 
                                                      2739, 2739, 2739, 2739, 2739, 2739, 2739, 2763, 3019, 
                                                      3054, 3169, 3204, 3223, 3223, 3244, 3250, 3386, 3386, 
                                                      3386, 3594, 3862, 4004, 4075, 4224, 4240, 4304, 4331, 
                                                      4399, 4473, 4665, 4667, 4667, 4667, 4687, 4956, 4956, 
                                                      4963, 5172, 5177, 5177, 5177, 5260, 5301, 5354, 5395, 
                                                      5412, 5534, 5618, 5648, 5754, 5829, 6075, 6157, 6186, 
                                                      6262, 6387, 6531, 6531, 6575, 6607, 6676, 6725, 6882, 
                                                      6916, 6959, 7088, 7088, 7129, 7727, 7916, 7916, 9304,
                                                      9492, 9933, 10839, 11472, 12250, 15576)) 
sum(table(fuzzy_duplicates) - 1) # 50 documents should be removed.
it2_dedup <- extract_unique_references(it2_dedup, fuzzy_duplicates) # Extract unique references. 
length(it2_dedup$title) # De-duplicated output is 17,768. 50 document removed. 
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2/2. Merged")
write_refs(it2_dedup, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_refman")
# EndNote and Zotero indicate that 53 duplicates remain in the .ris file, which are removed. We furthermore 
# remove seven retracted papers that are flagged in EndNote: "Bridging the Gap on Facebook: Assessing 
# Intergroup Contact and Its Effects for Intergroup Relations", "The Evolution of Intergroup Bias: 
# Perceptions and Attitudes in Rhesus Macaques", "The Evolution of Intergroup Bias: Perceptions and Attitudes 
# in Rhesus Macaques", "The Social Context of Latino Threat and Punitive Latino Sentiment", "When contact 
# changes minds: An experiment on transmission of support for gay equality", "Ethnic threat and social 
# control: Examining public support for judicial use of ethnicity in punishment", "Structural stigma and 
# all-cause mortality in sexual minority populations", and "When contact changes minds: An experiment on 
# transmission of support for gay equality". Three of these papers have two records, one for the original 
# paper, one for the retraction, for a total of ten removed papers. The result is exported from Zotero in the 
# file "it2_dedup.ris" which is subsequently imported.  
it2_dedup <- read_bibliography("it2_dedup.ris")
length(it2_dedup$title) # 17,705 documents. 53 documents removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it2_dedup$title)[, 3]), 0)) # 2 of 2 
# gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_group_threat$title, it2_dedup$title)[, 3]), 0)) # 29 of 66 external 
# articles are retrieved.

#### Check second iteration search against the naive and first iteration searches. 

### Relative to naive search.
## Overlap with naive search. Here I do not use the "check_recall" function because it takes a long time, 
## and it is not exact / conservative enough, for the purpose of merging the naive search documents that are 
## not in the second iteration search. I do this to try and maximize coverage. 
overlap_naive_it2 <- bind_rows(naive_dedup, it2_dedup) # Merge corpora. Should be 1,042 + 17,705 = 18,747. 
## Exact matching.
length(overlap_naive_it2$title) # Input is 18,747 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_naive_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_naive_it2$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 1,582 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 791 documents should be removed.
it2_dedup_out_naive <- synthesisr::extract_unique_references(overlap_naive_it2, exact_duplicates) 
length(it2_dedup_out_naive$title) # Output after exact matching is 17,956. 791 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it2_dedup_out_naive$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it2_dedup_out_naive$title, fuzzy_duplicates)) # Perform a manual check.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(1162, 12785, 17762, 17763, 17763, 
                                                                        17821, 17821)) 
sum(table(fuzzy_duplicates) - 1) # 10 documents should be removed.
it2_dedup_out_naive <- extract_unique_references(it2_dedup_out_naive, fuzzy_duplicates) # Extract unique 
# references.
length(it2_dedup_out_naive$title) # De-duplicated output is 17,946. 10 documents were removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2")
write_refs(it2_dedup_out_naive, format = "ris", tag_naming = "synthesisr", 
           file = "it2_dedup_out_naive_refman")
write_refs(it2_dedup_out_naive, format = "bib", tag_naming = "synthesisr", 
           file = "it2_dedup_out_naive_refman") # Also save as .bib for importing into Zotero.
# EndNote and Zotero indicate that three duplicates remain in the .ris file after the de-duplication 
# procedure, which are removed. The result is exported from Zotero in the file "it2_dedup_out_naive.ris" 
# which is subsequently imported.  
it2_dedup_out_naive <- read_bibliography("it2_dedup_out_naive.ris")
length(it2_dedup_out_naive$title) #  documents. 17,943 documents removed. 

## Coverage gain relative to the naive iteration search. I define coverage as the sum of the additional 
## documents that were found relative to the previous search iteration, and the documents from the previous 
## search iteration that were not identified in the current one.  
length(naive_dedup$title) # 1,042 articles.
length(it2_dedup$title) # 17,705 articles.
((length(it2_dedup$title) - length(naive_dedup$title))) # 16,663 document increase.
(length(it2_dedup_out_naive$title) - length(it2_dedup$title)) # 238 document from the naive search not in the
# first iteration search. Coverage increase of 16,663 + 238 = 16,901 or (16,901 / 1,042) = 16,22 as a factor
# increase.   
1 - (length(it2_dedup_out_naive$title) - length(it2_dedup$title)) / length(naive_dedup$title) # 77.16% of 
# the naive search was in the second iteration search. 

## Relative to first iteration. Note that the naive search articles are also in the first iteration 
## output object. 
## Overlap with first iteration search. Here I do not use the "check_recall" function because it takes a 
## long time, and it is not exact / conservative enough, for the purpose of merging the first iteration 
## search documents that are not in the second iteration search. I do this to try and maximize coverage. 
overlap_it1_it2 <- bind_rows(it1_dedup_out, it2_dedup_out_naive) # Merge corpora. Should be 9,841 + 17,943 = 
# 27,784. 
## Exact matching.
length(overlap_it1_it2$title) # Input is 27,784 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_it1_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_it1_it2$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 18,562 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 9,281 documents should be removed.
overlap_it1_it2 <- synthesisr::extract_unique_references(overlap_it1_it2, exact_duplicates) 
length(overlap_it1_it2$title) # Output after exact matching is 18,503. 9,281 documents removed. 560 documents
# remain.
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  overlap_it1_it2$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(overlap_it1_it2$title, fuzzy_duplicates)) # Perform a manual check.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(9834, 9834, 9834, 9920, 15504, 18343, 
                                                                        18386, 18386)) 
sum(table(fuzzy_duplicates) - 1) # 30 documents should be removed.
it2_dedup_out <- extract_unique_references(overlap_it1_it2, fuzzy_duplicates) # Extract unique references.
length(it2_dedup_out$title) # De-duplicated output is 18,473. 30 documents were removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/4. Iteration 2")
write_refs(it2_dedup_out, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_out_refman")
write_refs(it2_dedup_out, format = "bib", tag_naming = "synthesisr", file = "it2_dedup_out_refman")
# EndNote and Zotero indicate that 15 duplicates remain in the .ris file after the de-duplication 
# procedure, which are removed. The result is exported from Zotero in the file "it2_dedup_out_naive.ris" 
# which is subsequently imported.  
it2_dedup_out <- read_bibliography("it2_dedup_out.ris")
length(it2_dedup_out$title) # 18,458 documents. 15 documents removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it2_dedup_out$title)[, 3]), 0)) # 2 of 2 
# gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_group_threat$title, it2_dedup_out$title)[, 3]), 0)) # 30 of 44 external 
# articles are retrieved.

## Coverage gain relative to the first iteration search. I define coverage as the sum of the additional 
## documents that were found relative to the previous search iteration, and the documents from the previous 
## search iteration that were not identified in the current one. 
length(it1_dedup_out$title) # 9,841 articles.
length(it2_dedup_out$title) # 18,458 articles.
(length(it2_dedup_out$title) - length(it1_dedup_out$title)) # 8,617 article increase. 
(length(it2_dedup_out$title) - length(it2_dedup_out_naive$title)) # 515 documents from the first iteration
# search were not in the second. Coverage increase of 515 + 8,617 = 9,132 or (9,132 / 9,841) = 0.92 as a 
# factor increase. 
1 - (length(it2_dedup_out$title) - length(it2_dedup_out_naive$title)) / length(it1_dedup_out$title) # 94.77%
# of the first iteration search was in the second iteration search. 

### It is subsequently the question whether we should continue with a third iteration search or not. Note 
### that approximately 77% of the naive and 95% of the first iteration search is contained in the second 
### iteration search, and that the second iteration search added approximately 92% in terms of documents to 
### the first iteration search. The question is at which percentages for these various indicators we should 
### stop iterating. In theory, we could continue until the coverage increase is 0%, meaning that 100% of the 
### previous iteration is contained in the current iteration and that nothing is added in terms of additional
### documents. Due to decreasing returns and increasing computational burden for each additional iteration, 
### this is not feasible. We can alternatively impose limits on these indicators, i.e., ~90% of the previous 
### iterations are contained in the current iteration and the coverage increase is <5% for the previous 
### iteration, respectively, for example. The question is however whether this additional iteration with more 
### keywords will mostly result in increases in recall or precision. My guess is that it will mostly add to 
### recall. The ~90% and <5% values are in that sense also arbitrary in that these values might be reached 
### only long after almost all relevant documents have been found. It might nonetheless be fruitful to keep 
### iterating as long as some relevant documents are likely to still be identified, and the computational 
### cost is acceptable, which I judge to still be the case for a third iteration. Finally, because we use 
### ASReview for screening, we can afford to add proportionally more irrelevant as opposed to relevant 
### documents. As such, I execute a third iteration. 

#########################################
##### Iteration 3 Literature Search #####
#########################################

### Now repeat the previous procedure, but with keywords extracted from the second iteration search corpus.  

### Clear the environment except for the gold standard articles, "naive_dedup", "it1_dedup_out", and 
### "it2_dedup_out" objects. 
rm(list = setdiff(ls(), c("gs_grouped_terms", "gs_group_threat", "ex_group_threat", "naive_dedup", 
                          "it1_dedup_out", "it2_dedup_out")))

### Start by checking the second iteration search corpus for provided keywords, and keywords in titles and 
### abstracts.
## Keywords.
length(it2_dedup_out$keywords) # Total number of documents is 18,458. 
length(it2_dedup_out$keywords) - sum(is.na(it2_dedup_out$keywords)) # All of the articles list keywords.  
## Titles and abstracts.
length(it2_dedup_out$title) - sum(is.na(it2_dedup_out$title)) # All of the articles list a title.  
length(it2_dedup_out$abstract) - sum(is.na(it2_dedup_out$abstract)) # All of the articles list an abstract.  

### The first step is to obtain the keywords listed in the articles. We extract all non-single keywords that 
### are listed a minimum of sixty times. Note that the sixty number is arbitrary insofar that we need to 
### strike a balance between retrieving enough but not too many keyword candidates. More specifically,
### Gusenbauer & Haddaway (2020) note that search terms of length twenty-five should allow reviewers to 
### specify theirsearch scope to a reasonable extent. Put differently, our Boolean search should contain at 
### least twenty-five search terms. On the other hand, the maximum search string length that Ovid, Scopus, 
### and Web of Science allow according to Gusenbauer & Haddaway (2020) is around 1000. Web of Science for 
### example states that the maximum number of terms allowed in one field is fifty terms when using 
### "All Fields". As such, this number of keywords is a natural cap to the number of keywords that we can 
### seek to obtain from a corpora set. To reiterate, the number of keywords that will be incorporated in the 
### final searches will have a lower limit of 25 search terms, and an upper limit of 1000 characters in the 
### search string length, which translates to around fifty to sixty search terms (depending on the length of 
### the search terms).
tagged_keywords <- litsearchr::extract_terms(
  keywords = it2_dedup_out$keywords, # This is a list with the keywords from the articles. 
  method = "tagged", # Indicate that we want to obtain the keywords from the provided list.
  min_freq = 60, # The keyword has to occur a minimum of sixty times to be included.   
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(tagged_keywords) # 360 tagged keywords.
### Now use the RAKE to obtain keywords from the titles and abstracts of the search corpus. We extract all 
### non-single keywords that occur at least sixty times in these titles and abstracts. 
raked_keywords <- litsearchr::extract_terms(
  text = paste(it2_dedup_out$title, it1_dedup_out$abstract), # This is a list of titles and abstracts.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 60, # The keyword has to occur a minimum of sixty times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(raked_keywords) # 656 raked keywords.
## Sum total of tagged and raked keywords is 360 + 656 = 1016. 
keyword_candidates <- remove_redundancies(c(tagged_keywords, raked_keywords), closure = "full") # Remove
# duplicate terms.
length(keyword_candidates) # Total of 869 keyword candidates. 
## The subsequent task is to select all keywords that are relevant to our query from the candidate pool. I 
## select terms that relate in content to the relevant literature, i.e., are a independent or dependent 
## variable of interest in the group threat on inter-ethnic attitudes relationship. Note that I interpret 
## relevance quite broadly, since these terms will be will be ranked and filtered further based on their 
## "connectedness" in a co-occurrence network. In that sense, it is better too be too liberal than too 
## selective in our choices here, since it might be hard to anticipate which relevant terms are important 
## from this "connectedness" perspective. I furthermore do not include keywords which relate directly to any 
## of the other paradigms, e.g., I do not include keywords on contact theory even though these might appear 
## often in the group threat literature. Finally note that I do this part manually, which is prone to errors. 
all_keywords <- keyword_candidates[
  c(4, 5, 15, 33, 34, 42, 43, 54, 57, 61, 62, 64, 65, 70, 71, 73, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 
    87, 88, 89, 104, 105, 106, 107, 108, 109, 110, 111, 112, 150, 151, 153, 154, 155, 161, 162, 163, 164, 
    167, 168, 171, 182, 196, 197, 198, 199, 205, 210, 214, 215, 216, 220, 234, 243, 245, 246, 247, 251, 252, 
    253, 254, 255, 256, 257, 258, 259, 267, 283, 285, 288, 290, 291, 297, 298, 299, 300, 301, 303, 304, 305,
    306, 307, 309, 318, 320, 324, 325, 330, 331, 332, 341, 342, 345, 363, 380, 381, 382, 383, 397, 408, 411, 
    430, 431, 441, 442, 445, 446, 450, 453, 456, 466, 467, 468, 469, 470, 471, 472, 473, 474, 493, 516, 517, 
    518, 519, 520, 521, 522, 523, 524, 525, 534, 535, 536, 538, 539, 541, 542, 543, 544, 545, 546, 552, 554,
    556, 557, 562, 563, 564, 565, 566, 567, 568, 570, 571, 573, 586, 587, 588, 589, 595, 597, 598, 599, 600, 
    601, 602, 611, 612, 624, 633, 644, 645, 646, 647, 666, 673, 674, 676, 677, 684, 685, 691, 697, 716, 719,
    720, 725, 726, 727, 728, 729, 730, 731, 732, 733, 737, 742, 743, 760, 761, 785, 788, 789, 790, 791, 792,
    793, 795, 797, 802, 803, 815, 816, 838, 841, 842, 848, 849, 850) 
]
## Manual cleaning. 
all_keywords <- all_keywords[-c(1)] # Remove "0410 group interactions".
all_keywords[1] <- "group interactions" # Re-format.
all_keywords <- append(all_keywords, c("group size", "perceived group size")) # Add "group size". 
(all_keywords <- sort(all_keywords))
length(all_keywords) # 230 keyword candidates. 

### We further filter the keyword set by ranking the relative strength of each keyword in a so-called keyword 
### co-occurrence network (KCN). In a KCN, each keyword is "represented as a node and each co-occurrence of 
### a pair of words is represented as a link" (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). "The number 
### of times that a pair of words co-occurs in multiple articles constitutes the weight of the link 
### connecting the pair" (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). "The network constructed in this 
### manner represents cumulative knowledge of a domain and helps to uncover meaningful knowledge components 
### and insights based on the patterns and strength of links between keywords that appear in the literature" 
### (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). I furthermore combine the second iteration keywords 
## with the naive keywords. I add the naive keywords to the keyword candidate selection under the assumption 
## that these are important keywords because they were obtained from the gold standard articles, and should 
## therefore be considered explicitly in the KCN analysis, even though they are implicitly represented.
all_keywords_final <- c()
all_keywords_final <- append(all_keywords_final, c(as.vector(unlist(gs_grouped_terms)), all_keywords))
all_keywords_final <- remove_redundancies(all_keywords_final, closure = "full") # Remove duplicates. 
length(all_keywords_final) # 248 candidates.
## Build the keyword co-occurrence network. This chunk of code is a reworked version of the tutorial at: 
## https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html.
filter_dfm <- litsearchr::create_dfm(
  elements = paste(it2_dedup_out$title, it2_dedup_out$abstract), # The input in which the keywords can 
  # co-occur, titles and abstracts. 
  features = all_keywords_final) # The keyword candidates. 
kcn <- create_network(filter_dfm) # Create a KCN.
## Rank keywords based on strength in the co-occurrence network.
strengths <- strength(kcn) # Calculate strength values. 
data.frame(term = names(strengths), strength = strengths, row.names = NULL) %>%
  mutate(rank = rank(strength, ties.method = "min")) %>%
  arrange(desc(strength)) ->
  term_strengths # Create a data frame where the keywords are sorted by strength, in descending order. 
## I now apply a filter to select 57 and 42 terms for the search string in OVID, Scopus, and Web of Science 
## and ProQuest, respectively, where I remove terms which refer to an equivalent or near equivalent concept, 
## i.e., "target group" and "target groups", where I select the term with the highest strength value. 
(term_strengths <- cbind(seq(length(term_strengths$term)), term_strengths[order(term_strengths$term), ]))
term_strengths <- term_strengths[-c(6, 7, 23, 27, 33, 40, 44, 45, 46, 49, 51, 52, 59, 64, 65, 68, 69, 70, 
                                    74, 78, 79, 80, 81, 85, 88, 91, 93, 94, 96, 101, 102, 105, 112, 114, 116,
                                    118, 119, 120, 123, 125, 126, 127, 129, 136, 139, 140, 143, 145, 148, 
                                    155, 161, 164, 167, 172, 177, 181, 188, 190, 195, 198, 200, 203, 208, 
                                    209, 210, 212, 217, 219, 224, 225, 226, 227, 235, 237), ]

### Construct Boolean search OVID, Web of Science, and Scopus.
## Select first 55 terms from filtered term set. 
keywords_ovid_scopus_wos <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:58, ]$term)
(keywords_ovid_scopus_wos <- keywords_ovid_scopus_wos[order(keywords_ovid_scopus_wos)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3")
sink("third_iteration_selected_terms_ovid_scopus_wos.txt")
print(keywords_ovid_scopus_wos)
sink() 
## Categorize "keywords_ovid_scopus_wos" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_ovid_scopus_wos <- list(
  determinant = keywords_ovid_scopus_wos[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 22, 
                                           23, 24, 26, 27, 28, 29, 32, 33, 34, 42, 44, 46, 47, 48, 49, 50, 
                                           51, 52, 53, 54, 55, 56, 57, 58)],
  outcome = keywords_ovid_scopus_wos[c(18, 19, 20, 21, 25, 30, 31, 35, 36, 37, 38, 39, 40, 41, 43, 45)])
grouped_terms_ovid_scopus_wos
### Given the grouped terms, write the Boolean search for OVID, Web of Science, and Scopus to the 
### directory and print it to the console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3") # Set directory.
write_search(
  grouped_terms_ovid_scopus_wos, # The list of determinant and outcome keywords identified in the naive 
  # document set.
  languages = "English", # Language to write the search in, here set to English.
  exactphrase = TRUE, # Whether terms that consist of more than one word should be matched exactly rather 
  # than as two separate words. Set to TRUE, to limit both the scope and the number of redundant documents.
  stemming = TRUE, # Whether words are stripped down to the smallest meaningful part of the word to catch all 
  # variants of the word. Set to TRUE.
  closure = "none", # Whether partial matches are matched at the left end of a word ("left"), at the right 
  # ("right"), only as exact matches ("full"), or as any word containing a term ("none").  Set to "none" to 
  # obtain as many documents as possible.  
  writesearch = TRUE) # Whether we would like to write the search text to a file in the current directory. 
# Set to TRUE.
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- gsub('\\', "", bool, fixed = TRUE)
## Write the Boolean search for Ovid. 
bool <- gsub("((", "(", bool, fixed = T)
bool <- gsub("))", ")", bool, fixed = T)
cat(bool)
writeLines(bool, "bool_ovid_it3.txt")
## Write the Boolean search for Scopus. 
writeLines(bool, "bool_scopus_it3.txt")
## Write the Boolean search for Web of Science. 
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- paste0('ALL = ', bool, 'AND PY = (2010-2022)')
writeLines(bool, "bool_wos_it3.txt")
file.remove("search-inEnglish.txt") # Remove source file. 
## Finally save the grouped terms to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate for any particular reason.
sink("third_iteration_search_terms_ovid_scopus_wos.txt")
print(grouped_terms_ovid_scopus_wos)
sink() 

### Construct Boolean search ProQuest.
## Select first 34 terms from filtered term set.
keywords_proquest <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:34, ]$term)
(keywords_proquest <- keywords_proquest[order(keywords_proquest)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3")
sink("third_iteration_selected_terms_proquest.txt")
print(keywords_proquest)
sink() 
## Categorize "keywords_proquest" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_proquest <- list(
  determinant = keywords_proquest[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 19, 20, 21, 26, 27, 
                                    28, 29, 30, 31, 32, 33, 34)],
  outcome = keywords_proquest[c(13, 15, 18, 22, 23, 24, 25)])
grouped_terms_proquest$determinant <- grouped_terms_proquest$determinant[c(-24)] # Remove "social group" 

# because it blows ups the search in ProQuest.
### Given the grouped terms, write the the Boolean search for ProQuest to the directory and print it to the 
### console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3") # Set directory.
write_search(
  grouped_terms_proquest, # The list of determinant and outcome keywords identified in the naive document 
  # set.
  languages = "English", # Language to write the search in, here set to English.
  exactphrase = TRUE, # Whether terms that consist of more than one word should be matched exactly rather 
  # than as two separate words. Set to TRUE, to limit both the scope and the number of redundant documents.
  stemming = TRUE, # Whether words are stripped down to the smallest meaningful part of the word to catch all 
  # variants of the word. Set to TRUE.
  closure = "none", # Whether partial matches are matched at the left end of a word ("left"), at the right 
  # ("right"), only as exact matches ("full"), or as any word containing a term ("none").  Set to "none" to 
  # obtain as many documents as possible.  
  writesearch = TRUE) # Whether we would like to write the search text to a file in the current directory. 
# Set to TRUE.
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- gsub('\\', "", bool, fixed = TRUE)
## Write the Boolean search for ProQuest. 
bool <- gsub("((", "(", bool, fixed = T)
bool <- gsub("))", ")", bool, fixed = T)
cat(bool)
writeLines(bool, "bool_proquest_it3.txt")
file.remove("search-inEnglish.txt") # Remove source file. 
## Finally save the grouped terms to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate for any particular reason.
sink("third_iteration_search_terms_proquest.txt")
print(grouped_terms_proquest)
sink() 

### Please refer to the naive iteration for an overview of the exact steps of the search procedure.

### All searches were conducted on 11-07-2022. These resulted in 5,995 documents from Ovid (Selection: 
### PsycINFO), 5,184 documents from Proquest (Selection: Sociological Abstracts), 8,907 documents from Scopus 
### (Selection: Full index), and 9,430 documents from Web of Science (Selection: Web of Science Core 
### Collection), for a total of 29,997 documents. Please note that this document set is unfiltered, i.e., 
### duplicates, retracted documents, unidentified non-English documents, etc., have not yet been removed. In 
### Ovid, the documents were manually extracted in three batches. In ProQuest the documents were manually 
### extracted in 100-sized batches. In Scopus, the documents were manually extracted in 2000 document sized 
### batches, stratified by publication year. In Web of Science, the documents were manually extracted in 1000 
### document sized batches. 

### Data import and cleaning.
## Import results of informed search. Note that the batches for each search system were merged in EndNote.
## Start by checking whether the imported length is equal to the length of the raw .ris files.
import_ovid_it3 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iterat", 
                     "ion 3/1. Unmerged/Ovid"),
  verbose = TRUE)
import_proquest_it3 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iterat", 
                     "ion 3/1. Unmerged/ProQuest"),
  verbose = TRUE)
import_scopus_it3 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iterat", 
                     "ion 3/1. Unmerged/Scopus"),
  verbose = TRUE)
import_wos_it3 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iterat", 
                     "ion 3/1. Unmerged/Web of Science"),
  verbose = TRUE)
length(import_ovid_it3$title) # 5,995, which is correct.  
length(import_proquest_it3$title) # 5,184 which is correct. 
length(import_scopus_it3$title) # 8,907, which is correct.  
length(import_wos_it3$title) # 9,430, which is correct.  

## We subsequently identify and remove identifiable, non-English documents, if necessary. We then save the 
## resulting file.
# Ovid.
table(import_ovid_it3$language) # Ovid. All documents in the .ris file are in English
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3/2. Me", 
             "rged/Ovid/1. Raw"))
write_refs(import_ovid_it3, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
# ProQuest.
table(import_proquest_it3$language) # ProQuest. All documents in the .ris file are in English. 
import_proquest_it3 <- import_proquest_it3[import_proquest_it3$language == "English" ,] # Select English 
# documents and store them. 5,180 documents in ProQuest document set.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3/2. Me", 
             "rged/ProQuest/1. Raw"))
write_refs(import_proquest_it3, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
# Scopus.
table(import_scopus_it3$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3/2. Me", 
             "rged/Scopus/1. Raw"))
write_refs(import_scopus_it3, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
# Web of Science.
table(import_wos_it3$language) # Web of Science. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3/2. Me", 
             "rged/Web of Science/1. Raw"))
write_refs(import_wos_it3, format = "ris", tag_naming = "synthesisr", file = "web_of_science_r")

### De-duplication. Please refer to the naive iteration for an overview of the exact steps of the 
### de-duplication procedure.

### Ovid.
## Exact matching.
length(import_ovid_it3$title) # Input is 5,995 documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  import_ovid_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(import_ovid_it3$title, exact_duplicates_ovid) # Perform a 
# manual check. 
length(exact_manual$title) # 10 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # Five documents should be removed.
it3_dedup_ovid <- synthesisr::extract_unique_references(import_ovid_it3, exact_duplicates_ovid) 
length(it3_dedup_ovid$title) # Output after exact matching is 5,990. Five documents removed.
## Fuzzy matching five.
fuzzy_duplicates_ovid <- find_duplicates( 
  it3_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. One duplicate combination identified.
sum(table(fuzzy_duplicates_ovid) - 1) # One document should be removed.
it3_dedup_ovid <- extract_unique_references(it3_dedup_ovid, fuzzy_duplicates_ovid) # Extract unique 
# references. 
length(it3_dedup_ovid$title) # De-duplicated output is 5,989. One document removed. 
## Fuzzy matching ten.
fuzzy_duplicates_ovid <- find_duplicates( 
  it3_dedup_ovid$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. No duplicate combinations identified.
length(it3_dedup_ovid$title) # De-duplicated output is 5,989.  
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3/2. Mer", 
             "ged/Ovid/2. Deduplicated"))
write_refs(it3_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "it3_dedup_ovid")

### ProQuest. 
## Exact matching.
length(import_proquest_it3$title) # Input is 5,180 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  import_proquest_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(import_proquest_it3$title, exact_duplicates_proquest) # All
# combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 135 articles should be removed.
it3_dedup_proquest <- extract_unique_references(import_proquest_it3, exact_duplicates_proquest) 
length(it3_dedup_proquest$title) # Output after exact matching is 5,045. 135 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  it3_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. All combinations are duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # 19 documents should be removed.
it3_dedup_proquest <- extract_unique_references(it3_dedup_proquest, fuzzy_duplicates_proquest) # 
# Extract unique references. 
length(it3_dedup_proquest$title) # De-duplicated output is 5,026. 19 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  it3_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. Four duplicate combinations identified.
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, c(1557, 1960, 1960,
                                                                                          1960, 2546, 3340)) 
# Override non-duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # Four documents should be removed.
it3_dedup_proquest <- extract_unique_references(it3_dedup_proquest, fuzzy_duplicates_proquest) # Extract 
# unique references. 
length(it3_dedup_proquest$title) # De-duplicated output is 5,022. Four documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3/2. Mer", 
             "ged/ProQuest/2. Deduplicated"))
write_refs(it3_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "it3_dedup_proquest")

### Scopus.
## Exact matching.
length(import_scopus_it3$title) # Input is 8,907 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  import_scopus_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_scopus_it3$title, exact_duplicates_scopus)) # Perform a 
# manual check. All combinations are duplicates.
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # Six documents should be removed.
it3_dedup_scopus <- synthesisr::extract_unique_references(import_scopus_it3, exact_duplicates_scopus) 
length(it3_dedup_scopus$title) # Output after exact matching is 8,901. Seven documents removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  it3_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. Three duplicate combinations identified.
fuzzy_duplicates_scopus <- synthesisr::override_duplicates(fuzzy_duplicates_scopus, c(4760)) 
# Override non-duplicates.
sum(table(fuzzy_duplicates_scopus) - 1) # Three documents should be removed.
it3_dedup_scopus <- extract_unique_references(it3_dedup_scopus, fuzzy_duplicates_scopus) # Extract unique 
# references. 
length(it3_dedup_scopus$title) # De-duplicated output is 8,898. Two documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_scopus <- find_duplicates( 
  it3_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. No duplicate combinations identified.
length(it3_dedup_scopus$title) # De-duplicated output is 8,898. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3/2. Mer", 
             "ged/Scopus/2. Deduplicated"))
write_refs(it3_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "it3_dedup_scopus")

### Web of Science.
length(import_wos_it3$title) # Input is 9,430 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  import_wos_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_wos_it3$title, exact_duplicates_wos)) # Perform a 
# manual check. Six duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Six documents should be removed.
it3_dedup_wos <- synthesisr::extract_unique_references(import_wos_it3, exact_duplicates_wos) 
length(it3_dedup_wos$title) # Output after exact matching is 9,424. Four documents removed.
## Fuzzy matching five.
fuzzy_duplicates_wos <- find_duplicates( 
  it3_dedup_wos$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_wos$title, fuzzy_duplicates_wos)) # Perform a 
# manual check. No duplicate combinations identified.
sum(table(fuzzy_duplicates_wos) - 1) # Zero documents should be removed.
length(it3_dedup_wos$title) # De-duplicated output is 9,424. One document removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3/2. Mer", 
             "ged/Web of Science/2. Deduplicated"))
write_refs(it3_dedup_wos, format = "ris", tag_naming = "synthesisr", file = "it3_dedup_wos")

### Investigate the distribution of the overlap in the first iteration. Please note that this is very 
### approximate, in that I am not sure how this function matches duplicates (I assume its exact).  
ggVennDiagram(list(it3_dedup_ovid$title, it3_dedup_proquest$title, it3_dedup_scopus$title, 
                   it3_dedup_wos$title), 
              category.names = c("Ovid", "ProQuest", "Scopus", "Web of Science"), label_alpha = 0.75, 
              label = c("count"))
## The Venn diagram seems to indicate that although there exists quite some overlap between the corpora, 
## especially between Scopus and Web of Science, that they each add a substantial number of 
## non-overlapping documents to the sum total. It might however be the case that this positively affects 
## recall and therefore negatively affects precision, in that many non-relevant documents are added by a 
## search engine like Ovid, as shown by the fact that Ovid did not as successfully retrieve the gold 
## standard articles as Scopus, ProQuest, and Web of Science did. 

### Corpus precision diagnostics. Please refer to the naive iteration for an overview of the exact steps of 
### the precision diagnostics.

## Gold standard precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
cbind(gs_group_threat$title, gs_group_threat$year) # 2 documents >= 2010.
## Ovid.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it3_dedup_ovid$title)[, 3]), 0)) # 0 of 2 
# gold standard articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it3_dedup_proquest$title)[, 3]), 0)) # 1 
# of 2 gold standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it3_dedup_scopus$title)[, 3]), 0)) # 1 of 
# 2 gold standard articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it3_dedup_wos$title)[, 3]), 0)) # 2 of 2 
# gold standard articles are retrieved.
## The gold standard precision check tentatively indicates that ProQuest, Scopus, and Web of Science have 
## superior precision to Ovid. Relative to the second iteratoin, it has remained constant in Ovid, Proquest, 
## and Web of Science, but decreased in Scopus (-1). 

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
cbind(ex_group_threat$title, ex_group_threat$year) # Data frame of title and year of publication of 66 
# external articles.
## Ovid.
sum(round(as.numeric(check_recall(ex_group_threat$title, it3_dedup_ovid$title)[, 3]), 0)) # 7 of 44 
# external articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_group_threat$title, it3_dedup_proquest$title)[, 3]), 0)) # 14 of 44 
# external articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_group_threat$title, it3_dedup_scopus$title)[, 3]), 0)) # 21 of 44 
# external articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_group_threat$title, it3_dedup_wos$title)[, 3]), 0)) # 29 of 44 
# external articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus, ProQuest, and Ovid, respectively. Relative to the second iteration search, the 
## precision with respect to the external articles has decreased in Ovid (-1) and Scopus (-3), and remained
## constant in Proquest and Web of Science. 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
it3_dedup <- bind_rows(it3_dedup_ovid, it3_dedup_proquest, it3_dedup_scopus, it3_dedup_wos) # Merge corpora. 
## Exact matching.
length(it3_dedup$title) # Input is 29,333 documents.
exact_duplicates <- synthesisr::find_duplicates(
  it3_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(it3_dedup$title, exact_duplicates) # Perform a manual check.
length(exact_manual$title) # Sum of 17,657 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 10,750 documents should be removed.
it3_dedup <- synthesisr::extract_unique_references(it3_dedup, exact_duplicates) 
length(it3_dedup$title) # Output after exact matching is 18,583.  documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it3_dedup$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
fuzzy_manual <- review_duplicates(it3_dedup$title, fuzzy_duplicates) # Perform a manual check.
length(fuzzy_manual$title) # 1,623 potential duplicate combinations. I check these candidates manually in 
# batches. Note that this procedure is prone to error.  
fuzzy_manual$title[1:500] # All combinations are duplicates.
fuzzy_manual$title[501:1000] # All combinations are duplicates. 
fuzzy_manual$title[1001:1623] # 11898. Remaining combinations are duplicates.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(11898)) 
sum(table(fuzzy_duplicates) - 1) # 820 document should be removed.
it3_dedup <- extract_unique_references(it3_dedup, fuzzy_duplicates) # Extract unique references.
length(it3_dedup$title) # De-duplicated output is 17,763. 798 documents were removed.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it3_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it3_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 318 duplicate combinations.
fuzzy_manual$title # 9, 13, 64, 76, 135, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 
# 169, 169, 169, 169, 209, 220, 221, 467, 473, 565, 852, 888, 924, 1144, 1164, 1192, 1308, 1425, 1476, 1476, 
# 1476, 1476, 1476, 1476, 1476, 1519, 1641, 1695, 1695, 1695, 1695, 1695, 1695, 1695, 1695, 1695, 1778, 1778, 
# 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778,
# 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1787, 1834, 1838, 1851, 1927, 1954, 2226, 2259, 2347, 2556, 2666, 
# 2667, 2689, 2710, 2710, 2734, 3178, 3322, 3389, 3626, 3877, 3885, 3885, 3901, 3943, 4090, 4296, 4296, 4296, 
# 4296, 4489, 4500, 4545, 4827, 5086, 5499, 5547, 5697, 5765, 5874, 5874, 5874, 5874, 5874, 5890, 5890, 6174, 
# 6529, 6750, 6750, 7264, 7442, 7442, 7442, 10164, 10861, 10861, 11691, 13142, 15640, 15647, are not 
# duplicate combinations. Remaining combinations are.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(9, 13, 64, 76, 135, 169, 169, 169, 169, 169, 169, 169, 
                                                      169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 209, 
                                                      220, 221, 467, 473, 565, 852, 888, 924, 1144, 1164,
                                                      1192, 1308, 1425, 1476, 1476, 1476, 1476, 1476, 1476, 
                                                      1476, 1519, 1641, 1695, 1695, 1695, 1695, 1695, 1695, 
                                                      1695, 1695, 1695, 1778, 1778, 1778, 1778, 1778, 1778, 
                                                      1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 
                                                      1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 1778, 
                                                      1778, 1778, 1778, 1787, 1834, 1838, 1851, 1927, 1954, 
                                                      2226, 2259, 2347, 2556, 2666, 2667, 2689, 2710, 2710, 
                                                      2734, 3178, 3322, 3389, 3626, 3877, 3885, 3885, 3901, 
                                                      3943, 4090, 4296, 4296, 4296, 4296, 4489, 4500, 4545, 
                                                      4827, 5086, 5499, 5547, 5697, 5765, 5874, 5874, 5874, 
                                                      5874, 5874, 5890, 5890, 6174, 6529, 6750, 6750, 7264, 
                                                      7442, 7442, 7442, 10164, 10861, 10861, 11691, 13142, 
                                                      15640, 15647)) 
sum(table(fuzzy_duplicates) - 1) # 52 documents should be removed.
it3_dedup <- extract_unique_references(it3_dedup, fuzzy_duplicates) # Extract unique references. 
length(it3_dedup$title) # De-duplicated output is 17,471. 52 documents removed. 
## Fuzzy matching fifteen.
fuzzy_duplicates <- find_duplicates(
  it3_dedup$title, # Fuzzy matching ten output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 15) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it3_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 214 duplicate combination candidates..
fuzzy_manual$title # None of the combinations are duplicates. 
length(it3_dedup$title) # De-duplicated output is 17,471.  
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3/2. Merged")
write_refs(it3_dedup, format = "ris", tag_naming = "synthesisr", file = "it3_dedup_refman")
write_refs(it3_dedup, format = "bib", tag_naming = "synthesisr", file = "it3_dedup_refman")
# EndNote and Zotero indicate that 29 duplicates remain in the .ris file, which are removed. We furthermore 
# remove seven retracted papers that are flagged in EndNote: "The Evolution of Intergroup Bias: 
# Perceptions and Attitudes in Rhesus Macaques", "The Social Context of Latino Threat and Punitive Latino 
# Sentiment", "When contact changes minds: An experiment on transmission of support for gay equality". One of 
# these papers has three records, one for the original paper, two for the retraction, for a total of five 
# removed papers. The result is exported from Zotero in the file "it3_dedup.ris" which is subsequently 
# imported.  
it3_dedup <- read_bibliography("it3_dedup.ris")
length(it3_dedup$title)  # 17,437 documents. 34 documents removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it3_dedup$title)[, 3]), 0)) # 2 of 2 
# gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_group_threat$title, it3_dedup$title)[, 3]), 0)) # 29 of 44 external 
# articles are retrieved.

#### Check third iteration search against the naive, first, and second iteration searches. 

### Against the naive search.
## Overlap with naive search. Here I do not use the "check_recall" function because it takes a long time, 
## and it is not exact / conservative enough, for the purpose of merging the naive search documents that are 
## not in the second iteration search. I do this to try and maximize coverage. 
overlap_naive_it3 <- bind_rows(naive_dedup, it3_dedup) # Merge corpora. Should be 1,042 + 17,437 = 18,479. 
## Exact matching.
length(overlap_naive_it3$title) # Input is 18,479 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_naive_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_naive_it3$title, exact_duplicates) # Perform a manual 
# check.
length(exact_manual$title) # Sum of 1,582 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 791 documents should be removed.
it3_naive_dedup_out <- synthesisr::extract_unique_references(overlap_naive_it3, exact_duplicates) 
length(it3_naive_dedup_out$title) # Output after exact matching is 17,688.  documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it3_naive_dedup_out$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it3_naive_dedup_out$title, fuzzy_duplicates)) # Perform a manual check.
# All combinations are duplicates.
sum(table(fuzzy_duplicates) - 1) # 13 documents should be removed.
it3_naive_dedup_out <- extract_unique_references(it3_naive_dedup_out, fuzzy_duplicates) # Extract unique 
# references.
length(it3_naive_dedup_out$title) # De-duplicated output is 17,675. 13 documents were removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3")
write_refs(it3_naive_dedup_out, format = "ris", tag_naming = "synthesisr", 
           file = "it3_naive_dedup_out_refman")
write_refs(it3_naive_dedup_out, format = "bib", tag_naming = "synthesisr", 
           file = "it3_naive_dedup_out_refman")
# EndNote and Zotero indicate that 8 duplicates remain in the .ris file, which are removed. The result is 
# exported from Zotero in the file "it3_dedup.ris" which is subsequently imported. 
it3_naive_dedup_out <- read_bibliography("it3_naive_dedup_out.ris")
length(it3_naive_dedup_out$title) # 17,667 documents. 8 documents removed. 

## Coverage gain relative to the naive search.  
length(naive_dedup$title) # 1,042 articles.
length(it3_dedup$title) # 17,437 articles.
(length(it3_dedup$title) - length(naive_dedup$title)) # 16,395 additional articles.
(length(it3_naive_dedup_out$title) - length(it3_dedup$title)) # 230 articles from  naive not in third 
# iteration. This amounts to 16,395 + 230 = 16,625 or (16,625 / 1,042) = 15.95 as a factor increase. 
1 - (length(it3_naive_dedup_out$title) - length(it3_dedup$title)) / length(naive_dedup$title) # 77.93% of the 
# naive search was in the third iteration search. 

### Against the first iteration. Note that the naive search articles are also in the first iteration 
### output object. 
## Overlap with first iteration search. Here I do not use the "check_recall" function because it takes a 
## long time, and it is not exact / conservative enough, for the purpose of merging the first iteration 
## search documents that are not in the second iteration search. I do this to try and maximize coverage. 
overlap_it1_it3 <- bind_rows(it1_dedup_out, it3_naive_dedup_out) # Merge corpora. Should be 9,841 + 17,667 = 
# 27,508.
## Exact matching.
length(overlap_it1_it3$title) # Input is 27,508 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_it1_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_it1_it3$title, exact_duplicates) # Perform a manual 
# check.
length(exact_manual$title) # Sum of 17,940 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 8,970 documents should be removed.
it3_it1_dedup_out <- synthesisr::extract_unique_references(overlap_it1_it3, exact_duplicates) 
length(it3_it1_dedup_out$title) # Output after exact matching is 18,538. 8,970 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it3_it1_dedup_out$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it3_it1_dedup_out$title, fuzzy_duplicates)) # Perform a manual check.
length(fuzzy_manual$title) # 110 potential duplicate combinations. All combinations are duplicates.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(9834)) 
sum(table(fuzzy_duplicates) - 1) # 54 document should be removed.
it3_it1_dedup_out <- extract_unique_references(it3_it1_dedup_out, fuzzy_duplicates) # Extract unique 
# references.
length(it3_it1_dedup_out$title) # De-duplicated output is 18,484. 54 documents were removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3")
write_refs(it3_it1_dedup_out, format = "ris", tag_naming = "synthesisr", file = "it3_it1_dedup_out_refman")
write_refs(it3_it1_dedup_out, format = "bib", tag_naming = "synthesisr", file = "it3_it1_dedup_out_refman")
# EndNote indicates that 7 duplicates remain in the .ris file after the de-duplication procedure. These 
# documents were removed manually in EndNote, for a final sum total of  documents. After manually 
# removing the duplicate documents from the "it3_it1_dedup_out.ris" in EndNote, I overwrite the original 
# "it3_it1_dedup_out.ris" file with it, which I then import. 
it3_it1_dedup_out <- read_bibliography("it3_it1_dedup_out.ris")
length(it3_it1_dedup_out$title) # 18,478 documents. 7 documents removed.

## Coverage gain relative to the first iteration search.  
length(it1_dedup_out$title) # 9,841 articles.
length(it3_naive_dedup_out$title) # 17,667 articles.
(length(it3_naive_dedup_out$title) - length(it1_dedup_out$title)) # 7,826 additional articles.
(length(it3_it1_dedup_out$title) - length(it3_naive_dedup_out$title)) # 811 articles from first not in third 
# iteration. This amounts to 7,826 + 811 = 8,637 or (8,637 / 9,841) = 0.88 as a factor increase.
1 - (length(it3_it1_dedup_out$title) - length(it3_naive_dedup_out$title)) / length(it1_dedup_out$title) # 
# 91.59% of the first iteration search was in the third iteration search. 

### Against the second iteration. Note that the first iteration search articles are also in the second 
### iteration output object. 
## Overlap with second iteration search. Here I do not use the "check_recall" function because it takes a 
## long time, and it is not exact / conservative enough, for the purpose of merging the second iteration 
## search documents that are not in the third iteration search. I do this to try and maximize coverage. 
overlap_it2_it3 <- bind_rows(it2_dedup_out, it3_it1_dedup_out) # Merge corpora. Should be 18,458 +  18,478 = 
# 36,936.
## Exact matching.
length(overlap_it2_it3$title) # Input is 36,936 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_it2_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_it2_it3$title, exact_duplicates) # Perform a manual 
# check.
length(exact_manual$title) # Sum of 33,112 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 16,556 documents should be removed.
it3_dedup_out <- synthesisr::extract_unique_references(overlap_it2_it3, exact_duplicates) 
length(it3_dedup_out$title) # Output after exact matching is 20,380. 16,556 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it3_dedup_out$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it3_dedup_out$title, fuzzy_duplicates)) # Perform a manual check.
length(fuzzy_manual$title) # 127 potential duplicate combinations. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(9828, 9828, 9828, 9914, 15494, 18328,
                                                                        18371, 18371)) 
sum(table(fuzzy_duplicates) - 1) # 57 documents should be removed.
it3_dedup_out <- extract_unique_references(it3_dedup_out, fuzzy_duplicates) # Extract unique references.
length(it3_dedup_out$title) # De-duplicated output is 20,323. 57 documents were removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/1. Group threat/5. Iteration 3")
write_refs(it3_dedup_out, format = "ris", tag_naming = "synthesisr", file = "it3_dedup_out_refman")
write_refs(it3_dedup_out, format = "bib", tag_naming = "synthesisr", file = "it3_dedup_out_refman")
# EndNote indicates that 5 duplicates remain in the .ris file after the de-duplication procedure. These 
# documents were removed manually in EndNote, for a final sum total of  documents. After manually removing 
# the duplicate documents from the "it3_dedup_out.ris" in EndNote, I overwrite the original 
# "it3_dedup_out.ris" file with it, which I then import. 
it3_dedup_out <- read_bibliography("it3_dedup_out.ris")
length(it3_dedup_out$title) # 20,318 documents. 5 documents removed.

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
sum(round(as.numeric(check_recall(gs_group_threat$title[c(2, 4)], it3_dedup_out$title)[, 3]), 0)) # 2 of 2 
# gold standard articles are retrieved.
## External precision check.
check_recall(ex_group_threat$title, it3_dedup_out$title) # 32 of 44 or 72.73% of external articles are 
# retrieved. 

## Coverage gain relative to the second iteration search.  
length(it2_dedup_out$title) # 18,458 articles.
length(it3_dedup_out$title) # 20,318 articles.
(length(it3_dedup_out$title) - length(it2_dedup_out$title)) # 1,860 additional articles.
(length(it3_dedup_out$title) - length(it3_it1_dedup_out$title)) # 1,840 articles from second not in third 
# iteration. This amounts to 1,860 + 1,840 = 3,700 or (3,700 / 18,458) = 0.20 as a factor increase.
1 - (length(it3_dedup_out$title) - length(it3_it1_dedup_out$title)) / length(it2_dedup_out$title) # 90.03% of 
# the second iteration search was in the third iteration search. 

### Approximately 78% of the naive, 92% of the first, and 90% of the second iteration search is contained in 
### the third iteration search. The coverage increase between the second and third iteration is 20%, but it 
### should be noted that due to a mistake in the second iteration, approximately 1500 articles that were in 
### the second iteration are not included in the third iteration. Without these 1500 articles, the coverage 
### increase would be ((3,700 - 1,500) / 18,458) = 0.12. The external precision has furthermore also barely
### increased between the second and third iterations. Based on this observation, the respective overlap 
### values, and the coverage value increase of around 12%, I do not continue with a fourth iteration.

###########################
##### ASReview export #####
###########################

### Export the final document set as input for screening in ASReview.
## Data frame of the title, abstract, author, and year of publication. 
asreview_group_threat <- as.data.frame(cbind(it3_dedup_out$title, it3_dedup_out$abstract, 
                                             it3_dedup_out$author, it3_dedup_out$year))
## Assign column names.
colnames(asreview_group_threat) <- c("Title", "Abstract", "Author", "Year")
## Adding those gold standard articles that were published before 2010. These need to be added so that they 
## can be used as prior information in ASReview. Start with data frame of the title, abstract, author, and 
## year of publication. 
gs_to_asreview <- as.data.frame(cbind(gs_group_threat$title, gs_group_threat$abstract, 
                                      gs_group_threat$author, gs_group_threat$year))
## Select articles published before 2010.
gs_to_asreview <- gs_to_asreview[c(1, 3, 5, 6), ]
## Assign column names.
colnames(gs_to_asreview) <- c("Title", "Abstract", "Author", "Year")

## Row bind the two article document sets.
asreview_group_threat <- rbind(asreview_group_threat, gs_to_asreview)
## Final number of articles.
length(asreview_group_threat$Title) # 20,322 candidate articles for the group threat determinant. 
## Write result as a .csv file.
write.csv(asreview_group_threat, paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/1. G", 
                                        "roup threat/6. ASReview/ASReview_input_group_threat.csv"))

## Note that the .csv file is cleaned one more time before entering it into ASReview. We furthermore do not 
## address missing values.

#####################################################################
##### Quantifying inter-rater reliability and calculating power #####
#####################################################################

### Quantify reliability of initial screening in ASReview on the basis of inter-rater reliability. 
### Subsequently execute a power calculation to estimate how many articles should be included. 

### Quantifying inter-rater reliability in ASReview.

## TO BE ADDED.

### A power calculation to think about how many research articles need to minimally be included in the 
### meta-analysis of group threat on inter-ethnic attitudes. 
es <- 0.20 # Effect size. 0.20 is typically taken as a small effect size. 
avg <- 40  # Average number of respondents per study group. 40 seems quite low for sociological observational
# studies. 
het <- 3   # Heterogeneity. 3 is typically taken for large heterogeneity, which seems appropriate for the 
# field of study. 
power <- 0.80 # Power of 80 is the convention.   
lambda <- qnorm(p = 0.05 / 2, lower.tail = FALSE) - (- qnorm(power)) # Two-tailed test.

foo1 <- ((avg + avg) / ((avg) * (avg))) + ((es ^ 2) / (2 * (avg + avg)))
foo2 <- het * (foo1)
v_star <- foo1 + foo2
(k <- (v_star * (lambda ** 2)) / (es ** 2)) # ~40 studies at a minimum for group threat paradigm. Thus: 
# simple random sample over the literature retrieved in ASReview should at least have a size 40.  

######################
##### References #####
######################

## Session information and citation of packages used outside of base R.
sessionInfo()
out <- sessionInfo()
out$otherPkgs

# Gusenbauer, M., & Haddaway, N. R. (2020). Which academic search systems are suitable for systematic 
# reviews or meta-analyses? Evaluating retrieval qualities of Google Scholar, PubMed, and 26 other 
# resources. Research synthesis methods, 11(2), 181-217.

# Radhakrishnan, S., Erbis, S., Isaacs, J. A., & Kamarthi, S. (2017). Novel keyword co-occurrence 
# network-based methods to foster systematic reviews of scientific literature. PloS one, 12(3), e0172778.

# Rose, S., Engel, D., Cramer, N., & Cowley, W. (2010). Automatic keyword extraction from individual 
# documents. Text mining: applications and theory, 1, 1-20.
