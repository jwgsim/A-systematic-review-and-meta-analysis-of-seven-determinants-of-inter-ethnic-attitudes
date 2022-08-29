#####################################
##### Literature Search - Media #####
#####################################

####################
##### Packages #####
####################

## Package names
packages <- c("devtools", "dplyr", "igraph",  "readr", "remotes", "revtools", "synthesisr")
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
### the media literature. The naive keywords are extracted from two sources, the review question, and a set 
### of gold-standard articles. The review question is formulated as: are group threat, group contact, media, 
### socialization, and the demographics age, gender, and education determinants of inter-ethnic attitudes in 
### the 2010-2022 period? Note that we identify four research paradigms in this research question: group 
### threat, group contact, media, and socialization. Effects of the various demographic variables are assumed 
### to be present in articles on the four main paradigms. In this specific R-file we limit the scope to the 
### media determinant. We split the constituent elements in the research question as being either a 
### determinant or an outcome of interest, a distinction that we will follow throughout the rest of this 
### document:
naive_keywords <- c("media",  "inter-ethnic attitudes")

### The second and most important source of naive keywords are a set of six gold standard articles. These 
### articles have been selected by Eva Jaspers. Note that this set of articles is not assumed to be 
### representative of the literature that we seek to retrieve. We use the information within these articles
### to try and maximize coverage of this conceptually heterogeneous literature. We argue that due to this 
### heterogeneity, it is difficult to predict which keywords will and will not be relevant. As such, we start 
### by selecting a set of typical articles to include in a review, and use the keywords in these articles to 
### frame our initial search. The idea is that by combining the information in these articles with expert 
### judgement, we will be better able to construct search strings that cover most of the relevant 
### literature. 
## Importing the set of gold standard articles from the corresponding directory.
gs_media <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/1. Valida", 
                     "tion sets/1. Gold standard"), 
  verbose = TRUE)
## Request the length of the title object to show that there are 6 gold standard articles. Also request the
## object itself to show the titles of the articles themselves. 
length(gs_media$title)
gs_media$title

### The keywords embedded in these gold standard articles are the keywords as they have been listed by the 
### authors themselves, and those keywords as identified by the Rapid Automatic Keyword Extraction Algorithm 
### (RAKE). The RAKE is a keyword extraction algorithm which tries to determine key phrases in a body of text 
### by analyzing the frequency of word appearance and its co-occurrence with other words in the text. Please 
### refer to Rose, Engel, Cramer, and Cowley (2010) for an overview of the RAKE. 
## Start by obtaining the keywords listed in the articles. We extract all non-single keywords that are listed
## at least once. Single word keywords were inspected for viability but excluded. 
sum(!is.na(gs_media$title)) # Note that all of the 6 gold standard articles list keywords. 
gs_tagged_keywords <- litsearchr::extract_terms(
  keywords = gs_media$keywords, # This is a list with the keywords from the gold standard articles. 
  method = "tagged", # Indicate that we want to obtain the keywords from the provided list.
  min_freq = 1, # The keyword has to occur a minimum of one time to be included.  
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 1, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
gs_tagged_keywords # Resulting keywords. 
## I subsequently make a selection based on the degree to which each of the keywords is relevant to the 
## effect of media on inter ethnic attitudes. Note that I interpret this broadly. I exclude "arabic people", 
# "census", "covariance", "immigration", "islam", "new zealand", "panel data", "political parties", 
# "populism", "religion", "social discrimination". 
gs_tagged_keywords <- gs_tagged_keywords[c(2, 5, 8, 10, 12)]

## Use the RAKE to obtain keywords from the titles and abstracts of the gold standard articles. We extract 
## keywords that occur at least once in the titles and abstracts. 
gs_raked_keywords <- litsearchr::extract_terms(
  text = paste(gs_media$title, gs_media$abstract), # This is a list of the gold standard 
  # articles' titles and abstracts.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 1, # The keyword has to occur a minimum of two times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 1, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
gs_raked_keywords
## I subsequently make a selection based on the degree to which each of the keywords is relevant to the 
## media on inter-ethnic attitudes relationship. Note that I interpret this broadly. 
gs_raked_keywords <- gs_raked_keywords[c(23, 33, 34, 36, 37, 38, 40, 52, 66, 74, 169, 197, 232, 243, 249, 
                                         289, 290, 291, 292, 293, 294, 295, 297, 298, 299, 300, 301, 303, 
                                         306, 308, 322, 334, 348, 349, 362, 372, 387, 389, 393, 398, 399,
                                         409, 410, 415, 419, 420, 422, 425, 480, 531, 532)]
gs_raked_keywords # The resulting keywords.
## Remove redundantcharacters.
gs_raked_keywords[36] <- "people's immigration attitudes"

## Combine the tagged and raked keywords from the gold standard articles.
gs_all_keywords <- c()
gs_all_keywords <- append(gs_all_keywords, c(gs_tagged_keywords, gs_raked_keywords))
gs_all_keywords <- sort(gs_all_keywords)
gs_all_keywords <- remove_redundancies(gs_all_keywords, closure = "full") # Remove duplicate search terms.
gs_all_keywords <- gs_all_keywords[-c(8, 13, 15, 16, 23, 26, 30, 42, 43, 46)]

## Making a selection on whether a keyword is either a dependent or independent variable of interest, or is 
## prominent in the title or abstract of the gold standard articles.
gs_grouped_terms <- list(
  determinant = gs_all_keywords[c(1, 9, 10, 11, 14, 15, 18, 19, 20, 21, 22, 23, 24, 25, 28, 20, 30, 31, 
                                  35, 36, 41, 42, 43, 44, 45)],
  outcome = c(gs_all_keywords[c(2, 3, 4, 5, 6, 7, 8, 12, 13, 16, 17, 26, 27, 32, 33, 34, 37, 38, 39, 40)]))
gs_grouped_terms # Final set of naive keywords. 

### Given the naive keywords, we can proceed with a naive search of the academic literature. Gusenbauer & 
### Haddaway (2020) advise the use of what they call principal academic search systems when performing a
### systematic review, where each of these principal systems meet a set of quality requirements. Please 
### refer to Gusenbauer & Haddaway (2020) for a complete overview of this requirement set. They identify 
### fourteen principal search engines, seven of which are relevant in the context of this systematic review. 
### These are, with the database being searched in parentheses behind the academic search system: (1) the 
### Bielefeld Academic Search Engine (BASE) (Full index), (2) OVID (Selection: PsycINFO), (3) ScienceDirect 
### (Full index), (4) Proquest (Sociological Abstracts), (5) Scopus (Full index), (6) Web of Science 
### (Selection: Web of Science Core Collection), and (6) Wiley Online Library (Full index). Although 
### appropriate in principle, the BASE (Full index), ScienceDirect (Full index), and Wiley Online Library 
### (Full index) are excluded from this list. Our reasonfor doing so is that trial and error showed that the 
### BASE does not return enough documents which are relevant to our search, that ScienceDirect only allows 
### for searches with a string length of 25, which is not suitable for our purposes, and finally, that the 
### Wiley Online Library only allows for users to extract twenty documents per batch, which is not enough to 
### facilitate a search given the time and resources available to the project. Gusenbauer & Haddaway (2020) 
### note that arXiv (Full index; settings: All fields), Directory of Open Access Journals (DOAJ) (Full 
### index), Google Scholar (Full index), JSTOR (Full index), Microsoft Academic (Full index), Springer Link 
### (Full index), WorldCat (Selection: Thesis/Dissertation), and WorldWideScience (Full index) can be used as 
### supplementary search systems, to obtain grey literature for example. Although these supplementary search 
### engines would ideally also be considered, we do not consider them, again due to time and resource 
### constraints.

### An initial, manual naive search is performed with the set of naive keywords in OVID (Selection: 
### PsycINFO), Scopus (Selection: Full index), Proquest (Selection: Sociological Abstracts), and Web of 
### Science (Selection: Web of Science Core Collection). Given the grouped gold standard article terms, write 
### the Boolean search to the directory and print it to the console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive search") # Set 
# directory.
write_search(
  gs_grouped_terms, # The list of determinant and outcome keywords identified in the gold standard articles.
  languages = "English", # Language to write the search in, here set to English.
  exactphrase = TRUE, # Whether terms that consist of more than one word should be matched exactly rather 
  # than as two separate words. Set to TRUE, to limit both the scope and the number of redundant documents.
  stemming = FALSE, # Whether words are stripped down to the smallest meaningful part of the word to catch all 
  # variants of the word. Set to FALSE, to limit both the scope and the number of redundant documents.
  closure = "full", # Whether partial matches are matched at the left end of a word ("left"), at the right 
  # ("right"), only as exact matches ("full"), or as any word containing a term ("none"). Set to "full" to
  # to limit both the scope and the number of redundant documents.
  writesearch = TRUE) # Whether we would like to write the search text to a file in the current directory. 
# Set to TRUE.
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- gsub('\\', "", bool, fixed = TRUE)
## Write the Boolean search for Ovid. 
bool <- gsub("((", "(", bool, fixed = T)
bool <- gsub("))", ")", bool, fixed = T)
cat(bool)
writeLines(bool, "naive_bool_ovid.txt")
## Write the Boolean search for Scopus. 
writeLines(bool, "naive_bool_scopus.txt")
## Write the Boolean search for Proquest. 
writeLines(bool, "naive_bool_proquest.txt")
## Write the Boolean search for Web of Science. 
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- paste0('ALL = ', bool, 'AND PY = (2010-2022)')
writeLines(bool, "naive_bool_wos.txt")
file.remove("search-inEnglish.txt") # Remove source file. 
## Finally save the grouped terms to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate for some reason.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive search") # Set 
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
### This search was conducted on 08-08-2022. The documents are subsequently exported with "Format:" set to 
### "RIS" and "Fields:" set to "Complete Reference". If the total number of documents exceed 1500, this 
### exporting process is executed in batches of size 1500.

### In Proquest, we click on "Advanced Search", enter the Boolean search in the first line, set the value of 
### the "in" bar to "Anywhere except full text - NOFT", check the "Peer reviewed" box, select 
### "After this date..." in the "Publication date:" drop-down menu, and set the respective boxes to 
### "January", "1" and "2010", respectively. Under "Source type:" we select "Scholarly Journals" and under 
### "Language" we tick the "English" box. We subsequently click on "Search". On the next page, under 
### "Document type" we subsequently select "Article", where under "Language" we select "English". We set 
### "Items per page" to 100 and select the "EndNote" category under "All save & export options" for 
### exporting a RIS file for each page. This search was conducted on 08-08-2022.

### On the Scopus start page, we set the "Search within" tab to the default "Article title, Abstract, 
### Keywords" tab. We do not set it to the "All fields" tab, because doing so 1) retrieves a large number of 
### irrelevant documents, and 2) makes it difficult to export documents from Scopus. We subsequently enter 
### the Boolean search in the "Search documents" tab. We click on the "Add date range" button and set the 
### "Published from" tab to "2010", leaving the "To" tab to "Present", and the "Added to Scopus" tab to 
### "Anytime". We click "Search" and after having searched, scroll down to "Document type" and "Language" 
### under "Refine results", check the "Article" and "English" boxes, respectively, and click "Limit to". 
### This search was conducted on 08-08-2022. We subsequently select "All" and click on "Export" and select 
### "RIS Format". Note that I only export "Citation information", "Bibliographical information" and 
### "Abstracts & keywords" in Scopus because exporting the additional two categories "Funding details" and 
### "Other information" leads to merging issues later on. Note that we stratify the returned document set 
### by years when this number exceeds 2000. 

### In Web of Science, we click on "Advanced search", enter the Boolean search in the "Query Preview" search 
### box, and click "Search". We once again scroll down to "Document Types" and "Languages" under "Refine 
### results" and check the "Articles" and "English" and "Unspecified" boxes, respectively, if applicable. 
### This search was conducted on 08-08-2022. We subsequently click on "Select all records", "Export" and 
### then "RIS (other reference software):". We subsequently click on "Records from:" and export either the 
### total returned document set if this set is less than 1000, or export them in a per 1000 batch-wise 
### fashion if this number exceeds 1000, with "Record Content:" set to "Full Record". 

### The naive search resulted in 2,102 documents from Ovid (Selection: PsycINFO), 1,599 documents from ProQuest 
### (Selection: Sociological Abstracts), 7,413 documents from Scopus (Selection: Full index), and 6,390
### documents from Web of Science (Selection: Web of Science Core Collection) for a total of 17,504 documents. 
### Please note that this document set is unfiltered, i.e., duplicates, retracted documents, unidentified 
### non-English documents, etc., have not yet been removed. The documents were manually extracted in a single 
### batch in Ovid, Scopus, and Web of Science, and in 100 sized batches in ProQuest.

### Data import and cleaning.
## Import results of initial, manual naive search. Note that the batches for each search system were merged
## into a single .ris file in EndNote before being imported. 
naive_import_ovid <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive ", 
                     "search/1. Unmerged/Ovid"),
  verbose = TRUE)
naive_import_proquest <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive ", 
                     "search/1. Unmerged/ProQuest"),
  verbose = TRUE)
naive_import_scopus <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive ", 
                     "search/1. Unmerged/Scopus"),
  verbose = TRUE)
naive_import_wos <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive ", 
                     "search/1. Unmerged/Web of Science"),
  verbose = TRUE)
## Checking whether the length of the imported .ris files are equal to the lengths of the raw .ris files.
length(naive_import_ovid$title) # 2,102, which is correct.  
length(naive_import_proquest$title) # 1,599, which is correct.  
length(naive_import_scopus$title) # 7,413, which is correct.  
length(naive_import_wos$title) # 6,390, which is correct.  

any(is.na(naive_import_ovid$abstract))
any(is.na(naive_import_proquest$abstract))
which(is.na(naive_import_scopus$abstract))
which(is.na(naive_import_wos$abstract))

naive_import_scopus$title[66]

### We subsequently identify and remove identifiable, non-English documents.
## Ovid.
table(naive_import_ovid$language) # All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive search/2. ",
             "Merged/Ovid/1. Raw"))
write_refs(naive_import_ovid, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
## ProQuest.
table(naive_import_proquest$language) # ProQuest. All documents in the .ris file are in English.
naive_import_proquest <- naive_import_proquest[naive_import_proquest$language == "English" ,] # Select 
# English documents and store them. 1,593 documents in ProQuest document set.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive search/2. ",
             "Merged/ProQuest/1. Raw"))
write_refs(naive_import_proquest, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
## Scopus.
table(naive_import_scopus$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive search/2. ",
             "Merged/Scopus/1. Raw"))
write_refs(naive_import_scopus, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
## Web of Science.
table(naive_import_wos$language) # Web of Science. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive search/2. ",
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
### after the mainde-duplication procedure in R, I start by screening duplicates in EndNote and removing 
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
length(naive_import_ovid$title) # Input is 2,102 documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  naive_import_ovid$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_ovid$title, exact_duplicates_ovid)) # Perform a 
# manual check. No duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # Eight documents should be removed.
naive_dedup_ovid <- synthesisr::extract_unique_references(naive_import_ovid, exact_duplicates_ovid) 
length(naive_dedup_ovid$title) # Output after exact matching is 2,094. Eight documents removed.
## Fuzzy matching five.
fuzzy_duplicates_ovid <- find_duplicates( 
  naive_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. No duplicate combinations identified.
length(naive_dedup_ovid$title) # De-duplicated output is 2,094.  
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive search/2. M",
             "erged/Ovid/2. Deduplicated"))
write_refs(naive_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_ovid")

## ProQuest.
## Exact matching.
length(naive_import_proquest$title) # Input is 1,593 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  naive_import_proquest$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_proquest$title, exact_duplicates_proquest)) # 
# Perform a manual check. All candidate combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 61 documents should be removed.
naive_dedup_proquest <- extract_unique_references(naive_import_proquest, exact_duplicates_proquest) 
length(naive_dedup_proquest$title) # Output after exact matching is 1,532. 61 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  naive_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. 628, 628, 628, 628, 629, 629, are not duplicates, remaining ones are.
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, c(628, 628, 628, 628, 
                                                                                          629, 629)) 
sum(table(fuzzy_duplicates_proquest) - 1) # Eight documents should be removed.
naive_dedup_proquest <- extract_unique_references(naive_dedup_proquest, fuzzy_duplicates_proquest) # Extract
# unique references. 
length(naive_dedup_proquest$title) # De-duplicated output is 1,524. Eight documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  naive_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. None of the candidate combinations are duplicates. 
length(naive_dedup_proquest$title) # De-duplicated output is 1,524. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive search/2. M",
             "erged/ProQuest/2. Deduplicated"))
write_refs(naive_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_proquest")

## Scopus.
## Exact matching.
length(naive_import_scopus$title) # Input is 7,413 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  naive_import_scopus$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_scopus$title, exact_duplicates_scopus)) # Perform 
# a manual check. All candidate combinations are duplicates.
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # Nine documents should be removed.
naive_dedup_scopus <- synthesisr::extract_unique_references(naive_import_scopus, exact_duplicates_scopus) 
length(naive_dedup_scopus$title) # Output after exact matching is 7,404. Nine documents removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  naive_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform 
# a manual check. No duplicate combinations identified.
length(naive_dedup_scopus$title) # De-duplicated output is 7,404.  
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive search/2. M",
             "erged/Scopus/2. Deduplicated"))
write_refs(naive_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_scopus")

## Web of Science.
## Exact matching.
length(naive_import_wos$title) # Input is 6,390 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  naive_import_wos$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_wos$title, exact_duplicates_wos)) # Perform a 
# manual check. No duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Six documents should be removed.
naive_dedup_wos <- synthesisr::extract_unique_references(naive_import_wos, exact_duplicates_wos) 
length(naive_dedup_wos$title) # Output after exact matching is 6,384. Six documents removed.
## Fuzzy matching five.
fuzzy_duplicates_wos <- find_duplicates( 
  naive_dedup_wos$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_wos$title, fuzzy_duplicates_wos)) # Perform a 
# manual check. No duplicate combinations identified.
length(naive_dedup_wos$title) # De-duplicated output is 6,384.  
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/2. Naive search/2. M",
             "erged/Web of Science/2. Deduplicated"))
write_refs(naive_dedup_wos, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_wos")

### Investigate the distribution of the overlap in the naive search. I am not completely sure how this 
### function identifies and matches duplicates.  
ggVennDiagram(list(naive_dedup_ovid$title, naive_dedup_proquest$title, naive_dedup_scopus$title, 
                   naive_dedup_wos$title), 
              category.names = c("Ovid", "ProQuest", "Scopus", "Web of Science"), label_alpha = 0.75, 
              label = c("count"))
## The Venn diagram seems to indicate that although there exists quite some overlap between the corpora, 
## especially between Scopus and Web of Science, that they each add a substantial number of 
## non-overlapping documents to the sum total. 

### Corpus precision diagnostics. The precision of the search is quantified in two ways. We first evaluate 
### the degree to which the set of gold standard articles are in the retrieved corpora. Note that the 
### "check_recall" function uses osa for approximate matching. We secondly validate each corpus over a set of 
### articles that were obtained by taking a snowball sample over the gold standard articles. More 
### specifically, we enter the title of each gold standard article on the "connectedpapers.com" website, and 
### select all papers that are suggested in the resulting network, and all papers under the "Derivative 
### works" tab, that were published between the 2010-2022 period and are relevant to the query of interest
### as judged by the corresponding author, here the effect of media on inter-ethnic attitudes. 45 
### such external articles were identified. The following information is listed on the "connectedpapers.com"
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
cbind(gs_media$title, gs_media$year) 
## Ovid.
sum(round(as.numeric(check_recall(gs_media$title, naive_dedup_ovid$title)[, 3]), 0)) # 0 of 6 gold standard 
# articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_media$title, naive_dedup_proquest$title)[, 3]), 0)) # 1 of 6 gold 
# standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_media$title, naive_dedup_scopus$title)[, 3]), 0)) # 5 of 6 gold 
# standard articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_media$title, naive_dedup_wos$title)[, 3]), 0)) # 6 of 6 gold standard 
# articles are retrieved.
## The gold standard precision check tentatively indicates that Web of Science has superior precision, 
## followed by Scopus, ProQuest, and Ovid. Web of Science and Scopus seem superior to Ovid and Proquest 
## for the media determinant. Note that all gold standard articles are retrieved over the sum of the document 
## sets. We as such continue with the rest of the search strategy.

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and those
## =< 0.5 are not.
ex_media <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/1. Validat", 
                     "ion sets/2. External"), 
  verbose = TRUE)
cbind(ex_media$title, ex_media$year) # Data frame of title and year of publication of 98 
# external articles.
## Ovid.
sum(round(as.numeric(check_recall(ex_media$title, naive_dedup_ovid$title)[, 3]), 0)) # 10 of 45 external 
# articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_media$title, naive_dedup_proquest$title)[, 3]), 0)) # 8 of 45 external 
# articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_media$title, naive_dedup_scopus$title)[, 3]), 0)) # 22 of 45 external 
# articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_media$title, naive_dedup_wos$title)[, 3]), 0)) # 32 of 45 external 
# articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision,
## followed by Scopus, Ovid, and Proquest, respectively. Again, Scopus and Web of Science are superior to 
## Ovid and ProQuest. 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
## Set working directory. 
naive_dedup <- bind_rows(naive_dedup_ovid, naive_dedup_proquest, naive_dedup_scopus, naive_dedup_wos) # Bind 
# the corpora into a single corpus. Should be equal to 2,094 + 1,524 + 7,404 + 6,384 = 17,406.
## Exact matching.
length(naive_dedup$title) # Input is 17,406 documents.
exact_duplicates <- synthesisr::find_duplicates(
  naive_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(naive_dedup$title, exact_duplicates) # Perform a manual check. 
length(exact_manual$title) # Sum of 9,138 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 5,205 articles should be removed.
naive_dedup <- extract_unique_references(naive_dedup, exact_duplicates) 
length(naive_dedup$title) # Output after exact matching is 12,201. 5,205 documents removed.
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates( 
  naive_dedup$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup$title, fuzzy_duplicates)) # Perform a manual
# check. 
fuzzy_manual$title[1:500] # 2593, 2593, 2593, 2593, 2594, 2594, are not duplicates. Remaining combinations 
# are. 
fuzzy_manual$title[501:879] # All combinations are duplicates.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(2593, 2593, 2593, 2593, 2594, 2594)) 
sum(table(fuzzy_duplicates) - 1) # 439 documents should be removed.
naive_dedup <- extract_unique_references(naive_dedup, fuzzy_duplicates) # Extract unique references. 
length(naive_dedup$title) # De-duplicated output is 11,762. 439 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates( 
  naive_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
fuzzy_manual <- synthesisr::review_duplicates(naive_dedup$title, fuzzy_duplicates) # Perform a manual 
# check. 
fuzzy_manual$title # 32, 39, 132, 619, 674, 692, 721, 721, 742, 742, 742, 1537, 1697, 2508, 2508, 2572, 2576, 
# 2576, 2576, 2576, 2576, 2576, 2576, 2576, 3493, 3806, 4949, are not duplicate combinations, remaining ones 
# are.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(32, 39, 132, 619, 674, 692, 721, 721, 
                                                                        742, 742, 742, 1537, 1697, 2508, 
                                                                        2508, 2572, 2576, 2576, 2576, 2576, 
                                                                        2576, 2576, 2576, 2576, 3493, 3806, 
                                                                        4949)) 
sum(table(fuzzy_duplicates) - 1) # 224 documents should be removed.
naive_dedup <- extract_unique_references(naive_dedup, fuzzy_duplicates) # Extract unique references. 
length(naive_dedup$title) # De-duplicated output is 11,538. 224 documents removed.
## Fuzzy matching fifteen.
fuzzy_duplicates <- find_duplicates( 
  naive_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 15) # increased threshold of 15. 
fuzzy_manual <- synthesisr::review_duplicates(naive_dedup$title, fuzzy_duplicates) # Perform a manual 
# check. 
fuzzy_manual$title # 3, 31, 31, 31, 38, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 
# 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 131, 145, 206, 224, 321, 328, 328, 
# 427, 461, 581, 609, 611, 665, 683, 967, 1059, 1112, 1200, 1200, 1200, 1200, 1269, 1368, 1378, 1473, 1473, 
# 1473, 1519, 1654, 1679, 1780, 1954, 1954, 2132, 2197, 2404, 2451, 2549, 2549, 2549, 2549, 2549, 2549, 2549, 
# 2549, 2549, 2613, 3062, 4090, 4090, 4335, 4592, 4804, 4992, 5454, 10428, 10915, are not duplicate 
# combinations, remaining ones are.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(3, 31, 31, 31, 38, 130, 130, 130, 130, 130, 130, 130, 
                                                      130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 
                                                      130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 131, 
                                                      145, 206, 224, 321, 328, 328, 427, 461, 581, 609, 611, 
                                                      665, 683, 967, 1059, 1112, 1200, 1200, 1200, 1200, 
                                                      1269, 1368, 1378, 1473, 1473, 1473, 1519, 1654, 1679, 
                                                      1780, 1954, 1954, 2132, 2197, 2404, 2451, 2549, 2549, 
                                                      2549, 2549, 2549, 2549, 2549, 2549, 2549, 2613, 3062, 
                                                      4090, 4090, 4335, 4592, 4804, 4992, 5454, 10428, 
                                                      10915)) 
sum(table(fuzzy_duplicates) - 1) # 31 documents should be removed.
naive_dedup <- extract_unique_references(naive_dedup, fuzzy_duplicates) # Extract unique references. 
length(naive_dedup$title) # De-duplicated output is 11,507. 31 documents removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/3. media/2. Naive search/2. Merged")
write_refs(naive_dedup, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_refman")
# EndNote and Zotero both indicate that 68 duplicates remain in the .ris file. We furthermore remove four 
# retracted papers: "Bridging the Gap on Facebook: Assessing Intergroup Contact and Its Effects for 
# Intergroup Relations" (2), "Countering fake news in the COVID-19 era: The public's opinion on the role of 
# an honest and reliable website" (1), "Integrated CNN- and LSTM-DNN-based sentiment analysis over big 
# social data for opinion mining" (1), "Internet Blogs, Polar Bears, and Climate-Change Denial by Proxy" (1).
# The result is exported in the file "naive_dedup.ris" which is subsequently imported.  
naive_dedup <- read_bibliography("naive_dedup.ris")
length(naive_dedup$title) # 11,434 documents. 73 documents removed.

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_media$title, naive_dedup$title) # 6 of 6 gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_media$title, naive_dedup$title)[, 3]), 0)) # 35 of 45 external articles 
# are retrieved.

#########################################
##### Iteration 1 Literature Search #####
#########################################

### Now repeat the previous procedure, but with keywords extracted from the naive search corpus. This follows
### the same logic as earlier, due to the assumed high degree of heterogeneity of the literature, we combine 
### keyword information from the retrieved document set with author judgment to maximize coverage.

### Clear the environment except for the gold standard articles and "naive_dedup" objects. 
rm(list=setdiff(ls(), c("gs_grouped_terms", "gs_media", "ex_media", "naive_dedup")))

### Start by checking the naive search corpus for provided keywords, and keywords in titles and abstracts.
## Keywords.l
length(naive_dedup$keywords) # Total number of documents is 11,434. 
length(naive_dedup$keywords) - sum(is.na(naive_dedup$keywords)) # 11,228 of the articles list keywords.  
## Titles and abstracts.
length(naive_dedup$title) - sum(is.na(naive_dedup$title)) # All of the articles list a title.  
length(naive_dedup$abstract) - sum(is.na(naive_dedup$abstract)) # 10,439 of the articles list an abstract.  

### The first step is to obtain the keywords listed in the articles. We extract all non-single keywords that 
### are listed a minimum of fifty times. Note that the fifty number is arbitrary insofar that we need to 
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
  min_freq = 50, # The keyword has to occur a minimum of fifty times to be included.   
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(tagged_keywords) # 185 tagged keywords.
### Now use the RAKE to obtain keywords from the titles and abstracts of naive search corpus. We extract all 
### non-single keywords that occur at least fifty times in these titles and abstracts. 
raked_keywords <- litsearchr::extract_terms(
  text = paste(naive_dedup$title, naive_dedup$abstract), # This is a list of titles and abstracts 
  # per gold standard article.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 50, # The keyword has to occur a minimum of fifty times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(raked_keywords) # 351 raked keywords.
## Sum total of tagged and raked keywords. 
keyword_candidates <- remove_redundancies(c(raked_keywords, tagged_keywords), closure = "full") # Remove 
# duplicates.
length(keyword_candidates) # Total of 475 keyword candidates. 
## The subsequent task is to select all keywords that are relevant to our query from the candidate pool. I 
## select terms that relate in content to the relevant literature, i.e., are a independent or dependent 
## variable of interest in the media on inter-ethnic attitudes relationship. Note that I interpret 
## relevance quite broadly, since these terms will be will be ranked and filtered further based on their 
## "connectedness" in a co-occurrence network. In that sense, it is better too be too liberal than too 
## selective in our choices here, since it might be hard to anticipate which relevant terms are important 
## from this "connectedness" perspective. I furthermore do not include keywords which relate directly to any 
## of the other paradigms, e.g., I do not include keywords on group threat theory even though these might 
## appear often in the media literature. Finally note that I do this part manually, which is prone to errors.
all_keywords <- keyword_candidates[
  c(56, 111, 115, 119, 123, 124, 125, 126, 128, 129, 130, 132, 133, 134, 135, 136, 139, 140, 141, 142, 143, 
    145, 146, 150, 155, 156, 160, 166, 168, 169, 171, 174, 178, 196, 198, 215, 216, 223, 226, 230, 232, 239, 
    246, 262, 289, 298, 299, 301, 302, 306, 342, 343, 406, 412, 413, 414, 415, 416, 417, 418, 426, 427, 429, 
    440, 459)
]
## Manual cleaning.
(all_keywords <- sort(all_keywords))
length(all_keywords) # 65 keyword candidates.

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
length(all_keywords_final) # 102 candidates.
## Build the keyword co-occurrence network. This chunk of code is a reworked version of the tutorial at: 
## https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html.
filter_dfm <- litsearchr::create_dfm(
  elements = paste(naive_dedup$title, naive_dedup$abstract), # The input in which the keywords can co-occur, 
  # titles and abstracts. 
  features = all_keywords_final) # The keyword candidates. 
kcn <- create_network(filter_dfm) # Create a KCN.
## Rank keywords based on strength in the co-occurrence network.
strengths <- strength(kcn) # Calculate strength values. 
data.frame(term = names(strengths), strength = strengths, row.names = NULL) %>%
  mutate(rank = rank(strength, ties.method = "min")) %>%
  arrange(desc(strength)) ->
  term_strengths # Create a data frame where the keywords are sorted by strength, in descending order. 
## I now apply a filter to select 60 and 42 terms for the search string in ovid, scopus, and web of 
## science, and ProQuest, respectively, where I remove terms which refer to an equivalent or near equivalent
## concept, i.e., "target group" and "target groups", where I select the term with the highest strength 
## value. Note that the 60 and 42 values were the result of trial-and-error in the respective search engines.
(term_strengths <- cbind(seq(length(term_strengths$term)), term_strengths[order(term_strengths$term), ]))
term_strengths <- term_strengths[-c(5, 29, 50, 72, 73, 74, 75), ]

### Construct Boolean search OVID, Web of Science, and Scopus.
keywords_ovid_proquest_scopus_wos <- as.character(term_strengths$term)
## Categorize "keywords_ovid_proquest_scopus_wos" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_ovid_proquest_scopus_wos <- list(
  determinant = keywords_ovid_proquest_scopus_wos[c(1, 8, 9, 10, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
                                           27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 
                                           43, 44, 46, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 62, 
                                           63, 66, 67, 69, 71, 74, 75, 76, 77, 78, 79, 80, 81, 82)],
  outcome = keywords_ovid_proquest_scopus_wos[c(2, 3, 4, 5, 6, 7, 11, 12, 14, 45, 47, 60, 61, 64, 65, 68, 70, 72,
                                       73)])
grouped_terms_ovid_proquest_scopus_wos
### Given the grouped terms, write the Boolean search for OVID, Web of Science, and Scopus to the 
### directory and print it to the console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iteration 1") # Set directory.
write_search(
  grouped_terms_ovid_proquest_scopus_wos, # The list of determinant and outcome keywords identified in the naive 
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
## Write the Boolean search for ProQuest. 
writeLines(bool, "bool_proquest_it1.txt")
## Write the Boolean search for Scopus. 
writeLines(bool, "bool_scopus_it1.txt")
## Write the Boolean search for Web of Science. 
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- paste0('ALL = ', bool, 'AND PY = (2010-2022)')
writeLines(bool, "bool_wos_it1.txt")
file.remove("search-inEnglish.txt") # Remove source file. 
## Finally save the grouped terms to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate for any particular reason.
sink("first_iteration_search_terms_ovid_proquest_scopus_wos.txt")
print(grouped_terms_ovid_scopus_wos)
sink() 

### Please refer to the naive iteration for an overview of the exact steps of the search procedure. 

### All searches were conducted on 09-08-2022. These resulted in 4,221 documents from Ovid (Selection: 
### PsycINFO), 3,086 from ProQuest (Selection: Sociological Abstracts), 10,586 documents from Scopus 
### (Selection: Full index), and 9,672 documents from Web of Science (Selection: Web of Science Core 
### Collection), for a total of 27,565 documents. Please note that this document set is unfiltered, i.e., 
### duplicates, retracted documents, unidentified non-English documents, etc., have not yet been removed. In
### Ovid, the documents were manually extracted in three batches. In ProQuest the documents were manually 
### extracted in 100-sized batches. In Scopus, the documents were manually extracted in 2000 document sized 
### batches, stratified by year. In Web of Science, the documents were manually extracted in 1000 document 
### sized batches. 

### Data import and cleaning.
## Import results of informed search. Note that the batches for each search system were merged in EndNote.
## Start by checking whether the imported length is equal to the length of the raw .ris files.
import_ovid_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iterat", 
                     "ion 1/1. Unmerged/Ovid"),
  verbose = TRUE)
import_proquest_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iterat", 
                     "ion 1/1. Unmerged/ProQuest"),
  verbose = TRUE)
import_scopus_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iterat", 
                     "ion 1/1. Unmerged/Scopus"), 
  verbose = TRUE)
import_wos_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iterat", 
                     "ion 1/1. Unmerged/Web of Science"),
  verbose = TRUE)
length(import_ovid_it1$title) # 4,221, which is correct.  
length(import_proquest_it1$title) # 3,086, which is correct.  
length(import_scopus_it1$title) # 10,586, which is correct.
length(import_wos_it1$title) # 9,672, which is correct.  

### We subsequently identify and remove identifiable, non-English documents. 
## Ovid.
table(import_ovid_it1$language) # Ovid. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iteration 1/2. Me",
             "rged/Ovid/1. Raw"))
write_refs(import_ovid_it1, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
## ProQuest.
table(import_proquest_it1$language) # ProQuest. All documents in the .ris file are in English.
import_proquest_it1 <- import_proquest_it1[import_proquest_it1$language == "English" ,] # Select English 
# documents and store them. 3,065 documents in ProQuest document set.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iteration 1/2. Me", 
             "rged/ProQuest/1. Raw"))
write_refs(import_proquest_it1, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
## Scopus.
table(import_scopus_it1$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iteration 1/2. Me", 
             "rged/Scopus/1. Raw"))
write_refs(import_scopus_it1, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
## Web of Science.
table(import_wos_it1$language) # Web of Science. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iteration 1/2. Me", 
             "rged/Web of Science/1. Raw"))
write_refs(import_wos_it1, format = "ris", tag_naming = "synthesisr", file = "web_of_science_r")

### De-duplication. Please refer to the naive iteration for an overview of the exact steps of the 
### de-duplication procedure.

### Ovid.
## Exact matching.
length(import_ovid_it1$title) # Input is 4,221 documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  import_ovid_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_ovid_it1$title, exact_duplicates_ovid)) # Perform a 
# manual check. One duplicate combination identified.
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # Seven documents should be removed.
it1_dedup_ovid <- synthesisr::extract_unique_references(import_ovid_it1, exact_duplicates_ovid) 
length(it1_dedup_ovid$title) # Output after exact matching is 4,214. Seven documents removed.
## Fuzzy matching five.
fuzzy_duplicates_ovid <- find_duplicates( 
  it1_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. Zero duplicate combination identified.
length(it1_dedup_ovid$title) # De-duplicated output is 4,214.  
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iteration 1/2. Mer", 
             "ged/Ovid/2. Deduplicated"))
write_refs(it1_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_ovid")

### ProQuest. 
## Exact matching.
length(import_proquest_it1$title) # Input is 3,065 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  import_proquest_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_proquest_it1$title, exact_duplicates_proquest)) # All
# combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 85 articles should be removed.
it1_dedup_proquest <- extract_unique_references(import_proquest_it1, exact_duplicates_proquest) 
length(it1_dedup_proquest$title) # Output after exact matching is 2,980. 85 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  it1_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. 2312, 2312, 2312, 2312, 2314, are not duplicate combinations, remaining ones are. 
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, c(2312, 2312, 2312, 
                                                                                          2312, 2314)) 
# Override non-duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # 16 documents should be removed.
it1_dedup_proquest <- extract_unique_references(it1_dedup_proquest, fuzzy_duplicates_proquest) # Extract 
# unique references. 
length(it1_dedup_proquest$title) # De-duplicated output is 2,964. 16 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  it1_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. No duplicate combinations identified.
length(it1_dedup_proquest$title) # De-duplicated output is 2,964. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iteration 1/2. Mer", 
             "ged/ProQuest/2. Deduplicated"))
write_refs(it1_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_proquest")

### Scopus.
## Exact matching.
length(import_scopus_it1$title) # Input is 10,586 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  import_scopus_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_scopus_it1$title, exact_duplicates_scopus)) # Perform a 
# manual check. 
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # 14 documents should be removed.
it1_dedup_scopus <- synthesisr::extract_unique_references(import_scopus_it1, exact_duplicates_scopus) 
length(it1_dedup_scopus$title) # Output after exact matching is 10,572. 14 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  it1_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. One duplicate combination identified.
sum(table(fuzzy_duplicates_scopus) - 1) # One document should be removed.
it1_dedup_scopus <- extract_unique_references(it1_dedup_scopus, fuzzy_duplicates_scopus) # Extract unique 
# references. 
length(it1_dedup_scopus$title) # De-duplicated output is 10,571. One document removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iteration 1/2. Mer", 
             "ged/Scopus/2. Deduplicated"))
write_refs(it1_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_scopus")

### Web of Science.
length(import_wos_it1$title) # Input is 9,672 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  import_wos_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_wos_it1$title, exact_duplicates_wos)) # Perform a 
# manual check.
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Nine documents should be removed.
it1_dedup_wos <- synthesisr::extract_unique_references(import_wos_it1, exact_duplicates_wos) 
length(it1_dedup_wos$title) # Output after exact matching is 9,663. Nine documents removed.
## Fuzzy matching five.
fuzzy_duplicates_wos <- find_duplicates( 
  it1_dedup_wos$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_wos$title, fuzzy_duplicates_wos)) # Perform a 
# manual check. No duplicate combinations identified.
length(it1_dedup_wos$title) # De-duplicated output is 3,963. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iteration 1/2. Mer", 
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
## non-overlapping documents to the sum total.

### Corpus precision diagnostics. Please refer to the naive iteration for an overview of the exact steps of 
### the precision diagnostics.

## Gold standard precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
cbind(gs_media$title, gs_media$year)
## Ovid.
sum(round(as.numeric(check_recall(gs_media$title, it1_dedup_ovid$title)[, 3]), 0)) # 0 of 6 gold standard 
# articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_media$title, it1_dedup_proquest$title)[, 3]), 0)) # 1 of 6 gold 
# standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_media$title, it1_dedup_scopus$title)[, 3]), 0)) # 5 of 6 gold standard 
# articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_media$title, it1_dedup_wos$title)[, 3]), 0)) # 6 of 6 gold standard 
# articles are retrieved.
## The gold standard precision check tentatively indicates that Web of Science has superior precision, 
## followed by Scopus, ProQuest, and Ovid. Web of Science and Scopus seem superior to Ovid and Proquest 
## for the media determinant. The number of retrieved gold standard documents has remained constant over all 
## search engines. 

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
cbind(ex_media$title, ex_media$year) # Data frame of title and year of publication of 45 external articles.
## Ovid.
sum(round(as.numeric(check_recall(ex_media$title, it1_dedup_ovid$title)[, 3]), 0)) # 12 of 45 external 
# articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_media$title, it1_dedup_proquest$title)[, 3]), 0)) # 9 of 45 external 
# articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_media$title, it1_dedup_scopus$title)[, 3]), 0)) # 23 of 45 external 
# articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_media$title, it1_dedup_wos$title)[, 3]), 0)) # 31 of 45 external 
# articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus, Ovid, and ProQuest, respectively. Relative to the naive search, the precision with 
## respect to the external articles has increased slightly in Ovid (+2), ProQuest (+1), Scopus (+1). It has 
## decreased slightly in Web of Science (-2). 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
it1_dedup <- bind_rows(it1_dedup_ovid, it1_dedup_proquest, it1_dedup_scopus, it1_dedup_wos) # Merge corpora. 
## Exact matching.
length(it1_dedup$title) # Input is 27,412 documents.
exact_duplicates <- synthesisr::find_duplicates(
  it1_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(it1_dedup$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 16,946 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 10,079 documents should be removed.
it1_dedup <- synthesisr::extract_unique_references(it1_dedup, exact_duplicates) 
length(it1_dedup$title) # Output after exact matching is 17,333. 16,946 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
fuzzy_manual <- review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check.
length(fuzzy_manual$title) # 1,781 potential duplicate combinations. I check these candidates manually. Note 
# that this procedure is prone to error.
fuzzy_manual$title[1:500] # All combinations are duplicates.
fuzzy_manual$title[501:1000] # 5970, 5970, 5970, 5970, 5972. Remaining combinations are duplicates.
fuzzy_manual$title[1001:1500] # All combinations are duplicates.
fuzzy_manual$title[1500:1781] # All combinations are duplicates.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(5970, 5970, 5970, 5970, 5972)) 
sum(table(fuzzy_duplicates) - 1) # 897 document should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references.
length(it1_dedup$title) # De-duplicated output is 16,436. 897 documents were removed.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 640 duplicate combinations.
fuzzy_manual$title[1:500] # 96, 853, 853, 1690, 1690, 1690, 1909, 2380, 2452, 2580, 2627, 2665, 2728, 3174, 
# 3252, 3902, 4278, 5937, 5937, 5937, 5937, 5937, 5937, 5937, 5937, 6735, are not duplicate combinations. 
# Remaining combinations are.
fuzzy_manual$title[501:640] # All combinations are duplicates.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(96, 853, 853, 1690, 1690, 1690, 1909, 2380, 2452, 2580, 
                                                      2627, 2665, 2728, 3174, 3252, 3902, 4278, 5937, 5937, 
                                                      5937, 5937, 5937, 5937, 5937, 5937, 6735)) 
sum(table(fuzzy_duplicates) - 1) # 299 documents should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references. 
length(it1_dedup$title) # De-duplicated output is 16,137. 299 document removed. 
## Fuzzy matching fifteen.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Fuzzy matching ten output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 15) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 237 duplicate combinations.
fuzzy_manual$title # 4, 49, 54, 94, 216, 369, 473, 555, 592, 799, 813, 827, 827, 847, 847, 847, 847, 847, 
# 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847,
# 847, 895, 896, 1066, 1102, 1104, 1327, 1358, 1477, 1826, 1894, 2363, 2433, 2435, 2609, 2646, 2694, 2708,
# 2736, 2936, 2973, 3103, 3228, 3436, 3873, 3933, 4022, 4162, 4345, 4568, 4780, 4838, 5152, 5604, 5891, 5892,
# 5892, 5892, 5892, 5892, 5892, 5892, 5892, 5892, 8658, 10076, 10092, 13732, are not duplicate combinations,
# remaining ones are.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(4, 49, 54, 94, 216, 369, 473, 555, 592, 799, 813, 827, 
                                                      827, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 
                                                      847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 847, 
                                                      847, 847, 847, 847, 847, 847, 895, 896, 1066, 1102, 
                                                      1104, 1327, 1358, 1477, 1826, 1894, 2363, 2433, 2435, 
                                                      2609, 2646, 2694, 2708, 2736, 2936, 2973, 3103, 3228, 
                                                      3436, 3873, 3933, 4022, 4162, 4345, 4568, 4780, 4838, 
                                                      5152, 5604, 5891, 5892, 5892, 5892, 5892, 5892, 5892, 
                                                      5892, 5892, 5892, 8658, 10076, 10092, 13732)) 
sum(table(fuzzy_duplicates) - 1) # 49 documents should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references. 
length(it1_dedup$title) # De-duplicated output is 16,088. 49 documents removed. 
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iteration 1/2. Merged")
write_refs(it1_dedup, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_refman")
# EndNote and Zotero indicate that 61 duplicates remain in the .ris file, which are removed. We furthermore 
# remove eight retracted papers that are flagged in EndNote and Zotero: "Bridging the Gap on Facebook: 
# Assessing Intergroup Contact and Its Effects for Intergroup Relations" (2), "Helping the ingroup versus 
# harming the outgroup: Evidence from morality-based groups" (2), "The way in which COVID-19 changed 
# behaviour on social media in Malta" (1), "Use of personal protective equipment towards pesticide exposure: 
# Farmers' attitudes and determinants of behavior" (1), "Nurses attitudes toward psychiatric help for 
# depression: The serial mediation effect of self-stigma and depression on public stigma and attitudes 
# toward psychiatric help" (1), "Public environmental awareness of water pollution from urban growth: The 
# case of Zarjub and Goharrud rivers in Rasht, Iran" (1), "Sports and sportsmen as role models or otherwise 
# in the COVID-19 era" (1), "Double cropping in paddy fields of northern Iran: Current trends and 
# determinants of adoption" (1). The result is exported from Zotero in the file "it1_dedup.ris" which is 
# subsequently imported.  
it1_dedup <- read_bibliography("it1_dedup.ris")
length(it1_dedup$title) # 16,017 documents. 71 documents removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
sum(round(as.numeric(check_recall(gs_media$title, it1_dedup$title)[, 3]), 0)) # 6 of 6 gold standard 
# articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_media$title, it1_dedup$title)[, 3]), 0)) # 32 of 45 external 
# articles are retrieved.

### Evaluate first iteration search relative to naive search. 

## Here I do not use the "check_recall" function because it takes a long time, and it is not exact / 
## conservative enough, for the purpose of merging the naive search documents that are not in the first 
## iteration search. I do this to try and maximize coverage. 
length(naive_dedup$title) # 11,434 documents.
length(it1_dedup$title) # 16,017 documents. 
overlap_naive_it1 <- bind_rows(naive_dedup, it1_dedup) # Merge corpora. Should be 11,434 + 16,017 = 27,451 
## Exact matching.
length(overlap_naive_it1$title) # Input is 27,451 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_naive_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_naive_it1$title, exact_duplicates) # Perform a manual
# check.
length(exact_manual$title) # Sum of 10,628 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 5,314 documents should be removed.
it1_dedup_out <- synthesisr::extract_unique_references(overlap_naive_it1, exact_duplicates) 
length(it1_dedup_out$title) # Output after exact matching is 22,137. 5,314 documents removed. 6,120 documents 
# from the naive search not in the first iterations search remain.
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it1_dedup_out$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it1_dedup_out$title, fuzzy_duplicates)) # Perform a manual check. 
length(fuzzy_manual$title) # 26 duplicate combinations.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(2540, 2540, 2540, 2540, 11416, 
                                                                        11416)) 
sum(table(fuzzy_duplicates) - 1) # Nine documents should be removed.
it1_dedup_out <- extract_unique_references(it1_dedup_out, fuzzy_duplicates) # Extract unique references.
length(it1_dedup_out$title) # De-duplicated output is 22,128. Nine documents were removed. 6,111 documents 
# remain.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it1_dedup_out$title, # Fuzzy matching five as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # default cutoff. 
(fuzzy_manual <- review_duplicates(it1_dedup_out$title, fuzzy_duplicates)) # Perform a manual check. No 
# duplicate combinations identified. 
length(it1_dedup_out$title) # De-duplicated output is 22,128. 
## Save .ris file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/3. Iteration 1")
write_refs(it1_dedup_out, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_out_refman") # Export
# as .ris. 
write_refs(it1_dedup_out, format = "bib", tag_naming = "synthesisr", file = "it1_dedup_out_refman") # Export
# as .bib for importing into Zotero, which does not accept the .ris file.  
# EndNote and Zotero indicate that eight duplicates remain in the .ris file, which are removed. In addition, 
# two more retractions were identified: "Coping with chaos: How disordered contexts promote stereotyping and 
# discrimination" (1) and "Nurses attitudes toward psychiatric help for depression: The serial mediation 
# effect of self-stigma and depression on public stigma and attitudes toward psychiatric help" (1). The 
# result is exported from Zotero in the file "it1_dedup_out.ris" which is subsequently imported.  
it1_dedup_out <- read_bibliography("it1_dedup_out.ris")
length(it1_dedup_out$title) #  documents. Ten documents removed. This is the final corpus file of the 
# first iteration search.

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_media$title, it1_dedup_out$title) # 6 of 6 gold gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_media$title, it1_dedup_out$title)[, 3]), 0)) # 36 of 45 external 
# articles are retrieved.

## Inspect coverage gain relative to the naive search. I define coverage as the sum of the additional 
## documents that were found relative to the previous search iteration, and the documents from the previous 
## search iteration that were not identified in the current one. 
length(naive_dedup$title) # 11,434 articles.
length(it1_dedup$title) # 16,017 articles.
(length(it1_dedup$title) - length(naive_dedup$title)) # 16,017 - 11,434 = 4,583 more documents identified. 
(length(it1_dedup_out$title) - length(it1_dedup$title)) # ~6,101 documents from the naive search not in the 
# first iteration search. 
# So total coverage increase is: 4,583 + 6,101 = 10,684 documents or 10,684 / 11,434 = 0.93 as a factor 
# increase. 
(1 - (length(it1_dedup_out$title) - length(it1_dedup$title)) / length(naive_dedup$title)) * 100 # 46.64% of 
# the naive search was in the first iteration search. 

## Since the overlap with the previous iteration is quite low and the coverage increase differential is 
## moderate, we continue with a second iteration.

#########################################
##### Iteration 2 Literature Search #####
#########################################

### Now repeat the previous procedure, but with keywords extracted from the first iteration search corpus.  

### Clear the environment except for the gold standard search terms, the validation article sets, and 
### the "naive_dedup", and "it1_dedup_out" objects. 
rm(list = setdiff(ls(), c("gs_grouped_terms", "gs_media", "ex_media", "naive_dedup", "it1_dedup_out")))

### Start by checking how many documents in the first iteration search corpus provide keywords, titles, and 
### abstracts.
## Keywords.
length(it1_dedup_out$keywords) # Total number of documents is 22,118. 
length(it1_dedup_out$keywords) - sum(is.na(it1_dedup_out$keywords)) # All of the articles list keywords.  
## Titles and abstracts.
length(it1_dedup_out$title) - sum(is.na(it1_dedup_out$title)) # All of the articles list a title.  
length(it1_dedup_out$abstract) - sum(is.na(it1_dedup_out$abstract)) # All of the articles list an abstract.  

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
  keywords = it1_dedup_out$keywords, # This is a list with the keywords from the articles. 
  method = "tagged", # Indicate that we want to obtain the keywords from the provided list.
  min_freq = 60, # The keyword has to occur a minimum of sixty times to be included.   
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(tagged_keywords) # 425 tagged keywords.
### Now use the RAKE to obtain keywords from the titles and abstracts of the search corpus. We extract all 
### non-single keywords that occur at least sixty times in these titles and abstracts. 
raked_keywords <- litsearchr::extract_terms(
  text = paste(it1_dedup_out$title, it1_dedup_out$abstract), # This is a list of titles and abstracts.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 60, # The keyword has to occur a minimum of sixty times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(raked_keywords) # 687 raked keywords.
## Sum total of tagged and raked keywords is 425 + 687 = 1,112. 
keyword_candidates <- remove_redundancies(c(tagged_keywords, raked_keywords), closure = "full") # Remove
# duplicate terms.
length(keyword_candidates) # Total of 983 keyword candidates. 
## The subsequent task is to select all keywords that are relevant to our query from the candidate pool. I 
## select terms that relate in content to the relevant literature, i.e., are a independent or dependent 
## variable of interest in the media on inter-ethnic attitudes relationship. Note that I interpret 
## relevance quite broadly, since these terms will be will be ranked and filtered further based on their 
## "connectedness" in a co-occurrence network. In that sense, it is better too be too liberal than too 
## selective in our choices here, since it might be hard to anticipate which relevant terms are important 
## from this "connectedness" perspective. I furthermore do not include keywords which relate directly to any 
## of the other paradigms, e.g., I do not include keywords on group threat theory even though these might 
## appear often in the media literature. Finally note that I do this part manually, which is prone to errors.
all_keywords <- keyword_candidates[
  c(49, 51, 53, 86, 168, 173, 176, 183, 198, 199, 200, 201, 202, 203, 204, 205, 206, 227, 231, 232, 260, 275,
    290, 291, 305, 350, 355, 362, 363, 396, 454, 474, 622, 623, 628, 638, 643, 646, 647, 648, 649, 650, 652, 
    653, 654, 655, 656, 657, 658, 659, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 670, 685, 691, 697,
    702, 704, 705, 709, 712, 724, 752, 756, 768, 770, 788, 792, 826, 836, 908, 909, 910, 911, 912, 913, 973, 
    974) 
]
## Manual cleaning. 
(all_keywords <- sort(all_keywords))
length(all_keywords) # 86 keyword candidates. 

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
length(all_keywords_final) # 124 candidates.
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
## I now apply a filter to select  and  terms for the search string in OVID, Scopus, and Web of Science 
## and ProQuest, respectively, where I remove terms which refer to an equivalent or near equivalent concept, 
## i.e., "target group" and "target groups", where I select the term with the highest strength value. 
(term_strengths <- cbind(seq(length(term_strengths$term)), term_strengths[order(term_strengths$term), ]))
term_strengths <- term_strengths[-c(39, 49, 55, 66, 81, 84, 89, 90, 101), ]

### Construct Boolean search OVID, Web of Science, and Scopus.
## Select first 60 terms from filtered term set. 
keywords_ovid_proquest_scopus_wos <- as.character(term_strengths$term)
(keywords_ovid_proquest_scopus_wos <- keywords_ovid_scopus_wos[order(keywords_ovid_scopus_wos)])
## Categorize "keywords_ovid_proquest_scopus_wos" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_ovid_proquest_scopus_wos <- list(
  determinant = keywords_ovid_proquest_scopus_wos[c(1, 10, 11, 12, 13, 14, 15, 18, 19, 20, 22, 23, 25, 26, 
                                                    27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 
                                                    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 
                                                    55, 56, 57, 58, 59, 61, 63, 64, 65, 66, 67, 68, 69, 70, 
                                                    71, 73, 77, 80, 81, 83, 86, 90, 91, 92, 93, 94, 95, 96, 
                                                    97, 98, 99, 100, 101)],
  outcome = keywords_ovid_proquest_scopus_wos[c(2, 3, 4, 5, 6, 7, 8, 9, 16, 17, 21, 24, 60, 62, 72, 74, 75, 
                                                76, 78, 79, 82, 84, 85, 87, 88, 89)])
grouped_terms_ovid_proquest_scopus_wos
### Given the grouped terms, write the Boolean search for OVID, Proquest, Web of Science, and Scopus to the 
### directory and print it to the console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2") # Set directory.
write_search(
  grouped_terms_ovid_proquest_scopus_wos, # The list of determinant and outcome keywords identified in the 
  # naive document set.
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
## Write the Boolean search for ProQuest. 
writeLines(bool, "bool_proquest_it2.txt")
## Write the Boolean search for Scopus. 
writeLines(bool, "bool_scopus_it2.txt")
## Write the Boolean search for Web of Science. 
bool <- capture.output(cat(read_file("search-inEnglish.txt"))) 
bool <- paste0('ALL = ', bool, 'AND PY = (2010-2022)')
writeLines(bool, "bool_wos_it2.txt")
file.remove("search-inEnglish.txt") # Remove source file. 
## Finally save the grouped terms to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate for any particular reason.
sink("second_iteration_search_terms_ovid_proquest_scopus_wos.txt")
print(grouped_terms_ovid_proquest_scopus_wos)
sink() 

### Please refer to the naive iteration for an overview of the exact steps of the search procedure.

### All searches were conducted on 10-08-2022. This resulted in 4,787 documents from Ovid (Selection: 
### PsycINFO), 2,783 documents from Proquest (Selection: Sociological Abstracts), 13,458 documents from Scopus 
### (Selection: Full index), and 11,399 documents from Web of Science (Selection: Web of Science Core 
### Collection), for a total of 32,427 documents. Please note that this document set is unfiltered, i.e., 
### duplicates, retracted documents, unidentified non-English documents, etc., have not yet been removed. In 
### Ovid, the documents were manually extracted in three batches. In ProQuest the documents were manually 
### extracted in 100-sized batches. In Scopus, the documents were manually extracted in 2000 document sized 
### batches, stratified by publication year. In Web of Science, the documents were manually extracted in 1000 
### document sized batches. 

### Data import and cleaning.
## Import results of informed search. Note that the batches for each search system were merged in EndNote.
## Start by checking whether the imported length is equal to the length of the raw .ris files.
import_ovid_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iterat", 
                     "ion 2/1. Unmerged/Ovid"),
  verbose = TRUE)
import_proquest_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iterat", 
                     "ion 2/1. Unmerged/ProQuest"),
  verbose = TRUE)
import_scopus_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iterat", 
                     "ion 2/1. Unmerged/Scopus"),
  verbose = TRUE)
import_wos_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iterat", 
                     "ion 2/1. Unmerged/Web of Science"),
  verbose = TRUE)
length(import_ovid_it2$title) # 4,787, which is correct.  
length(import_proquest_it2$title) # 2,783, which is correct. 
length(import_scopus_it2$title) # 13,458, which is correct.  
length(import_wos_it2$title) # 11,399, which is correct.  

## We subsequently identify and remove identifiable, non-English documents, if necessary. We then save the 
## resulting file.
# Ovid.
table(import_ovid_it2$language) # Ovid. All documents in the .ris file are in English
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2/2. Me", 
             "rged/Ovid/1. Raw"))
write_refs(import_ovid_it2, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
# ProQuest.
table(import_proquest_it2$language) # ProQuest. All documents in the .ris file are in English. 
import_proquest_it2 <- import_proquest_it2[import_proquest_it2$language == "English" ,] # Select English 
# documents and store them. 2,772 documents in ProQuest document set.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2/2. Me", 
             "rged/ProQuest/1. Raw"))
write_refs(import_proquest_it2, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
# Scopus.
table(import_scopus_it2$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2/2. Me", 
             "rged/Scopus/1. Raw"))
write_refs(import_scopus_it2, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
# Web of Science.
table(import_wos_it2$language) # Web of Science. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2/2. Me", 
             "rged/Web of Science/1. Raw"))
write_refs(import_wos_it2, format = "ris", tag_naming = "synthesisr", file = "web_of_science_r")

### De-duplication. Please refer to the naive iteration for an overview of the exact steps of the 
### de-deplication procedure.

### Ovid.
## Exact matching.
length(import_ovid_it2$title) # Input is 4,787  documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  import_ovid_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(import_ovid_it2$title, exact_duplicates_ovid) # Perform a 
# manual check. 
length(exact_manual$title) # 19 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # Ten documents should be removed.
it2_dedup_ovid <- synthesisr::extract_unique_references(import_ovid_it2, exact_duplicates_ovid) 
length(it2_dedup_ovid$title) # Output after exact matching is 4,777. Ten documents removed.
## Fuzzy matching five.
fuzzy_duplicates_ovid <- find_duplicates( 
  it2_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. One duplicate combination identified.
sum(table(fuzzy_duplicates_ovid) - 1) # One document should be removed.
it2_dedup_ovid <- extract_unique_references(it2_dedup_ovid, fuzzy_duplicates_ovid) # Extract unique 
# references. 
length(it2_dedup_ovid$title) # De-duplicated output is 4,776. One document removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2/2. Mer", 
             "ged/Ovid/2. Deduplicated"))
write_refs(it2_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_ovid")

### ProQuest. 
## Exact matching.
length(import_proquest_it2$title) # Input is 2,772 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  import_proquest_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_proquest_it2$title, exact_duplicates_proquest)) # All
# combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 93 articles should be removed.
it2_dedup_proquest <- extract_unique_references(import_proquest_it2, exact_duplicates_proquest) 
length(it2_dedup_proquest$title) # Output after exact matching is 2,679. 93 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  it2_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. 
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, c(1892, 1892, 1892, 
                                                                                          1892, 1894)) 
sum(table(fuzzy_duplicates_proquest) - 1) # 12 documents should be removed.
it2_dedup_proquest <- extract_unique_references(it2_dedup_proquest, fuzzy_duplicates_proquest) # 
# Extract unique references. 
length(it2_dedup_proquest$title) # De-duplicated output is 2,667. 12 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  it2_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. No duplicate combinations identified.
length(it2_dedup_proquest$title) # De-duplicated output is 2,667. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2/2. Mer", 
             "ged/ProQuest/2. Deduplicated"))
write_refs(it2_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_proquest")

### Scopus.
## Exact matching.
length(import_scopus_it2$title) # Input is 13,458 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  import_scopus_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_scopus_it2$title, exact_duplicates_scopus)) # Perform a 
# manual check. All combinations are duplicates.
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # 16 documents should be removed.
it2_dedup_scopus <- synthesisr::extract_unique_references(import_scopus_it2, exact_duplicates_scopus) 
length(it2_dedup_scopus$title) # Output after exact matching is 13,442. 16 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  it2_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. Three duplicate combinations identified.
sum(table(fuzzy_duplicates_scopus) - 1) # Three documents should be removed.
it2_dedup_scopus <- extract_unique_references(it2_dedup_scopus, fuzzy_duplicates_scopus) # Extract unique 
# references. 
length(it2_dedup_scopus$title) # De-duplicated output is 13,439. Three documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_scopus <- find_duplicates( 
  it2_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. No duplicate combinations identified.
length(it2_dedup_scopus$title) # De-duplicated output is 13,439. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2/2. Mer", 
             "ged/Scopus/2. Deduplicated"))
write_refs(it2_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_scopus")

### Web of Science.
length(import_wos_it2$title) # Input is 11,399 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  import_wos_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_wos_it2$title, exact_duplicates_wos)) # Perform a 
# manual check. 
sum(as.numeric(table(exact_duplicates_wos) - 1)) # 11 documents should be removed.
it2_dedup_wos <- synthesisr::extract_unique_references(import_wos_it2, exact_duplicates_wos) 
length(it2_dedup_wos$title) # Output after exact matching is 11,388. 11 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_wos <- find_duplicates( 
  it2_dedup_wos$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_wos$title, fuzzy_duplicates_wos)) # Perform a 
# manual check. No duplicate combinations identified.
length(it2_dedup_wos$title) # De-duplicated output is 9,201. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2/2. Mer", 
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
## non-overlapping documents to the sum total.

### Corpus precision diagnostics. Please refer to the naive iteration for an overview of the exact steps of 
### the precision diagnostics.

## Gold standard precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
## Ovid.
sum(round(as.numeric(check_recall(gs_media$title, it2_dedup_ovid$title)[, 3]), 0)) # 0 of 6 gold standard 
# articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_media$title, it2_dedup_proquest$title)[, 3]), 0)) # 1 of 6 gold 
# standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_media$title, it2_dedup_scopus$title)[, 3]), 0)) # 5 of 6 gold standard 
# articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_media$title, it2_dedup_wos$title)[, 3]), 0)) # 6 of 6 gold standard 
# articles are retrieved.
## The gold standard precision check tentatively indicates that Web of Science has superior precision, 
## followed by Scopus, ProQuest, and Ovid. Web of Science and Scopus seem superior to Ovid and Proquest 
## for the media determinant. The number of retrieved gold standard documents has remained constant over all 
## search engines relative to the first iteration.

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
## Ovid.
sum(round(as.numeric(check_recall(ex_media$title, it2_dedup_ovid$title)[, 3]), 0)) # 11 of 45 external 
# articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_media$title, it2_dedup_proquest$title)[, 3]), 0)) # 9 of 45 external 
# articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_media$title, it2_dedup_scopus$title)[, 3]), 0)) # 22 of 45 external 
# articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_media$title, it2_dedup_wos$title)[, 3]), 0)) # 30 of 45 external
# articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus, Ovid, and ProQuest, respectively. Relative to the first iteration search, the 
## precision with respect to the external articles has stayed constant in ProQuest and decreased slightly in 
## Ovid (-1), Scopus (-1) and Web of Science (-1).  

### Remove duplicates between corpora and combine the various corpora into a .ris file.
it2_dedup <- bind_rows(it2_dedup_ovid, it2_dedup_proquest, it2_dedup_scopus, it2_dedup_wos) # Merge corpora. 
## Exact matching.
length(it2_dedup$title) # Input is 32,270 documents.
exact_duplicates <- synthesisr::find_duplicates(
  it2_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(it2_dedup$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 19,9566 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 11,641 documents should be removed.
it2_dedup <- synthesisr::extract_unique_references(it2_dedup, exact_duplicates) 
length(it2_dedup$title) # Output after exact matching is 20,629. 11,641 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it2_dedup$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
fuzzy_manual <- review_duplicates(it2_dedup$title, fuzzy_duplicates) # Perform a manual check.
length(fuzzy_manual$title) # 933 potential duplicate combinations. I check these candidates manually in 
# batches. Note that this procedure is prone to error.  
fuzzy_manual$title[1:500] # 1970, 3091, 3091, 3091, 3091, are not duplicate combinations. Remaining 
# combinations are. 
fuzzy_manual$title[501:933] # 3753, 7294, 9362, are not duplicate combinations. Remaining ones are. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(1970, 3091, 3091, 3091, 3091,
                                                                        3753, 7294, 9362)) 
sum(table(fuzzy_duplicates) - 1) # 464 documents should be removed.
it2_dedup <- extract_unique_references(it2_dedup, fuzzy_duplicates) # Extract unique references.
length(it2_dedup$title) # De-duplicated output is 19,689. 940 documents were removed.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it2_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it2_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 726 duplicate combinations.
fuzzy_manual$title[1:500] # 107, 834, 943, 1760, 1869, 1869, 1869, 1869, 1869, 1869, 2120, 2657, 2735, 2912, 
# 2957, 3030, 3495, 3524, 3612, 3622, 3803, 4406, 5004, 5974, 5974, 5974, 5974, 5974, 5974, 5974, 5974, 6681.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(107, 834, 943, 1760, 1869, 1869, 1869, 1869, 1869, 
                                                      1869, 2120, 2657, 2735, 2912, 2957, 3030, 3495, 3524, 
                                                      3612, 3622, 3803, 4406, 5004, 5974, 5974, 5974, 5974, 
                                                      5974, 5974, 5974, 5974, 6681)) 
sum(table(fuzzy_duplicates) - 1) # 337 documents should be removed.
it2_dedup <- extract_unique_references(it2_dedup, fuzzy_duplicates) # Extract unique references. 
length(it2_dedup$title) # De-duplicated output is 19,352. 337 documents removed. 
## Fuzzy matching fifteen.
fuzzy_duplicates <- find_duplicates(
  it2_dedup$title, # Fuzzy matching ten output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 15) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it2_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 261 duplicate combinations.
fuzzy_manual$title # 5, 54, 61, 105, 193, 247, 365, 417, 658, 829, 883, 898, 938, 938, 938, 938, 938, 938,
# 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938,  
# 958, 994, 995, 1049, 1190, 1198, 1226, 1471, 1471, 1506, 1508, 1578, 1635, 1744, 1857, 2103, 2157, 2637,
# 2713, 2715, 2892, 2937, 2992, 3009, 3040, 3257, 3300, 3450, 3587, 3597, 3778, 3850, 4375, 4448, 4448, 4552,
# 4552, 4715, 4852, 4793, 5497, 5708, 5922, 5929, 5930, 5930, 5930, 5930, 5930, 5930, 5930, 5930, 5930, 5985,
# 6047, 6159, 6159, 7316, 7638, 7695, 9156, 9156, 10840, 11837, 11856, 16742, are not duplicate combinations, 
# remaining ones are.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(5, 54, 61, 105, 193, 247, 365, 417, 658, 829, 883, 898, 
                                                      938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 
                                                      938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 938, 
                                                      938, 938, 938, 938, 938, 958, 994, 995, 1049, 1190, 
                                                      1198, 1226, 1471, 1471, 1506, 1508, 1578, 1635, 1744, 
                                                      1857, 2103, 2157, 2637, 2713, 2715, 2892, 2937, 2992, 
                                                      3009, 3040, 3257, 3300, 3450, 3587, 3597, 3778, 3850, 
                                                      4375, 4448, 4448, 4552, 4552, 4715, 4852, 4793, 5497, 
                                                      5708, 5922, 5929, 5930, 5930, 5930, 5930, 5930, 5930, 
                                                      5930, 5930, 5930, 5985, 6047, 6159, 6159, 7316, 7638, 
                                                      7695, 9156, 9156, 10840, 11837, 11856, 16742)) 
sum(table(fuzzy_duplicates) - 1) # 46 documents should be removed.
it2_dedup <- extract_unique_references(it2_dedup, fuzzy_duplicates) # Extract unique references. 
length(it2_dedup$title) # De-duplicated output is 19,306. 46 documents removed. 
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2/2. Merged")
write_refs(it2_dedup, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_refman") # Export as .ris
# file. 
write_refs(it2_dedup, format = "bib", tag_naming = "synthesisr", file = "it2_dedup_refman") # Export as .bib
# file for importing into Zotero.
# EndNote and Zotero indicate that 68 duplicates remain in the .ris file, which are removed. We furthermore 
# remove ten retracted papers that are flagged in EndNote and Zotero: "Bridging the Gap on Facebook: Assessing 
# Intergroup Contact and Its Effects for Intergroup Relations" (2), "Helping the ingroup versus harming the 
# outgroup: Evidence from morality-based groups" (2), "The way in which COVID-19 changed behaviour on social 
# media in Malta" (1), "Use of personal protective equipment towards pesticide exposure: Farmers' attitudes 
# and determinants of behavior" (1), "Pesticide use in cereal production in Moghan Plain, Iran: Risk 
# knowledge and farmers' attitudes" (1), "When contact changes minds: An experiment on transmission of 
# support for gay equality" (1), "Nurses attitudes toward psychiatric help for depression: The serial 
# mediation effect of self-stigma and depression on public stigma and attitudes toward psychiatric help" (1),
# "Public environmental awareness of water pollution from urban growth: The case of Zarjub and Goharrud 
# rivers in Rasht, Iran" (1), "Sports and sportsmen as role models - or otherwise - in the COVID-19 era" (1),
# "Double cropping in paddy fields of northern Iran: Current trends and determinants of adoption" (1). The 
# result is exported from Zotero in the file "it2_dedup.ris" which is subsequently imported.  
it2_dedup <- read_bibliography("it2_dedup.ris") 
length(it2_dedup$title) # 19,226 documents. 80 documents removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_media$title, it2_dedup$title) # 6 of 6 gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_media$title, it2_dedup$title)[, 3]), 0)) # 31 of 45 external articles 
# are retrieved.

#### Check second iteration search against the naive and first iteration searches. 

### Relative to naive search.
## Overlap with naive search. Here I do not use the "check_recall" function because it takes a long time, 
## and it is not exact / conservative enough, for the purpose of merging the naive search documents that are 
## not in the second iteration search. I do this to try and maximize coverage. 
overlap_naive_it2 <- bind_rows(naive_dedup, it2_dedup) # Merge corpora. Should be 11,434 + 19,226 = 30,660. 
## Exact matching.
length(overlap_naive_it2$title) # Input is 16,605 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_naive_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_naive_it2$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 10,568 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 5,284 documents should be removed.
it2_dedup_out_naive <- synthesisr::extract_unique_references(overlap_naive_it2, exact_duplicates) 
length(it2_dedup_out_naive$title) # Output after exact matching is 25,376. 5,284 documents remain. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it2_dedup_out_naive$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it2_dedup_out_naive$title, fuzzy_duplicates)) # Perform a manual check.
# 2540, 2540, 2540, 2540, 11416, 11416, are not duplicate combinations. Remaining ones are. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(2540, 2540, 2540, 2540, 11416, 
                                                                        11416)) 
sum(table(fuzzy_duplicates) - 1) # 11 documents should be removed.
it2_dedup_out_naive <- extract_unique_references(it2_dedup_out_naive, fuzzy_duplicates) # Extract unique 
# references.
length(it2_dedup_out_naive$title) # De-duplicated output is 25,365. 11 documents remain.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2")
write_refs(it2_dedup_out_naive, format = "ris", tag_naming = "synthesisr", 
           file = "it2_dedup_out_naive_refman") # Export as .ris file. 
write_refs(it2_dedup_out_naive, format = "bib", tag_naming = "synthesisr", 
           file = "it2_dedup_out_naive_refman") # Also save as .bib for importing into Zotero.
# EndNote and Zotero indicate that 5 duplicates remain in the .ris file after the de-duplication 
# procedure, which are removed. One more retraction is also identified in Zotero: "Coping with chaos: How 
# disordered contexts promote stereotyping and discrimination" (1). The result is exported from Zotero in the 
# file "it2_dedup_out_naive.ris" which is subsequently imported.  
it2_dedup_out_naive <- read_bibliography("it2_dedup_out_naive.ris")
length(it2_dedup_out_naive$title) # 25,359 documents. 6 documents removed. 

## Coverage gain relative to the naive iteration search. I define coverage as the sum of the additional 
## documents that were found relative to the previous search iteration, and the documents from the previous 
## search iteration that were not identified in the current one.  
length(naive_dedup$title) # 11,434 articles.
length(it2_dedup$title) # 19,226 articles.
((length(it2_dedup$title) - length(naive_dedup$title))) # 7,792 document increase.
(length(it2_dedup_out_naive$title) - length(it2_dedup$title)) # 6,133 documents from the naive search not in 
# thefirst iteration search. Coverage increase of 7,792 + 6,133 = 13,925 or (13,925 / 11,434) = 1.22 as a 
# factor increase.   
1 - (length(it2_dedup_out_naive$title) - length(it2_dedup$title)) / length(naive_dedup$title) # 46.36% of 
# the naive search was in the second iteration search. 

## Relative to first iteration. Note that the naive search articles are also in the first iteration 
## output object. 
## Overlap with first iteration search. Here I do not use the "check_recall" function because it takes a 
## long time, and it is not exact / conservative enough, for the purpose of merging the first iteration 
## search documents that are not in the second iteration search. I do this to try and maximize coverage. 
length(it1_dedup_out$title)
length(it2_dedup_out_naive$title)
overlap_it1_it2 <- bind_rows(it1_dedup_out, it2_dedup_out_naive) # Merge corpora. Should be 22,118 + 25,359 = 
# 47,477. 
## Exact matching.
length(overlap_it1_it2$title) # Input is 47,477 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_it1_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_it1_it2$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 41,386 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 20,693 documents should be removed.
overlap_it1_it2 <- synthesisr::extract_unique_references(overlap_it1_it2, exact_duplicates) 
length(overlap_it1_it2$title) # Output after exact matching is 26,784. 20,693 documents removed.
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  overlap_it1_it2$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(overlap_it1_it2$title, fuzzy_duplicates)) # Perform a manual check. 2540, 
# 2540, 2540, 2540, 11415, 11415, are not duplicate combinations. Remaining ones are.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(2540, 2540, 2540, 2540, 11415, 
                                                                        11415)) 
sum(table(fuzzy_duplicates) - 1) # 22 documents should be removed.
it2_dedup_out <- extract_unique_references(overlap_it1_it2, fuzzy_duplicates) # Extract unique references.
length(it2_dedup_out$title) # De-duplicated output is 26,762. 22 documents were removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media/4. Iteration 2")
write_refs(it2_dedup_out, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_out_refman") # Export 
# as .ris file.
write_refs(it2_dedup_out, format = "bib", tag_naming = "synthesisr", file = "it2_dedup_out_refman") # Export 
# as .bib file for importing into Zotero.
# EndNote and Zotero indicate that one duplicate remains in the .ris file after the de-duplication 
# procedure, which are removed. The result is exported from Zotero in the file "it2_dedup_out_naive.ris" 
# which is subsequently imported.  
it2_dedup_out <- read_bibliography("it2_dedup_out.ris")
length(it2_dedup_out$title) # 26,761 documents. One document removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_media$title, it2_dedup_out$title) # 6 of 6 gold standard articles are retrieved.
## External precision check.
check_recall(ex_media$title, it2_dedup_out$title) # 37 of 45 or 82.22% of the external articles are 
# retrieved.

## Coverage gain relative to the first iteration search. I define coverage as the sum of the additional 
## documents that were found relative to the previous search iteration, and the documents from the previous 
## search iteration that were not identified in the current one. 
length(it1_dedup_out$title) # 22,118 articles.
length(it2_dedup_out$title) # 26,761 articles.
(length(it2_dedup_out$title) - length(it1_dedup_out$title)) # 4,643  article increase. 
(length(it2_dedup_out$title) - length(it2_dedup_out_naive$title)) # 1,402 documents from the first iteration
# search were not in the second. Coverage increase of 4,643 + 1,402 = 6,045 or (6,045 / 22,118) = 0.27 as a 
# factor increase. 
1 - (length(it2_dedup_out$title) - length(it2_dedup_out_naive$title)) / length(it1_dedup_out$title) # 93.66%
# of the first iteration search was in the second iteration search. 

### It is subsequently the question whether we should continue with a third iteration search or not. Note 
### that approximately 46% of the naive and 94% of the first iteration search is contained in the second 
### iteration search, and that the second iteration search added approximately 27% in terms of documents to 
### the first iteration search. Given the high overlap values between the first and second iteration search, 
### and the fact that the overlap value with respect to the naive search has not improved between the first 
### and second iteration search, combined with the fact that the precision has not increased substantially
### between the first and second iteration search, and the fact that the computational burden of a third 
### iteration will be quite high, I do not continue with a third iteration.

###########################
##### ASReview export #####
###########################

### Export the final document set as input for screening in ASReview.
## Data frame of the title, abstract, author, and year of publication. 
asreview_media <- as.data.frame(cbind(it2_dedup_out$title, it2_dedup_out$abstract, it2_dedup_out$author, 
                                      it2_dedup_out$year))
## Assign column names.
colnames(asreview_media) <- c("Title", "Abstract", "Author", "Year")
## Final number of articles.
length(asreview_media$Title) # 26,761 candidate articles for the media determinant. 
## Write result as a .csv file.
write.csv(asreview_media, paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/3. Media", 
                                 "/5. ASReview/ASReview_input_media.csv"))

## The .csv file is cleaned one more time before entering it into ASReview. More specifically, some 
## conversion errors might have occurred in the .csv file which are addressed manually. 

#########################################################
##### Inter-rater reliability and power calculation #####
#########################################################

### Quantify reliability of initial screening in ASReview on the basis of inter-rater reliability. 
### Subsequently execute a power calculation to estimate how many articles should be included. 

### Quantifying inter-rater reliability in ASReview.

## TO BE ADDED.

### A power calculation to think about how many research articles need to minimally be included in the 
### meta-analysis of media on inter-ethnic attitudes. 
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
(k <- (v_star * (lambda ** 2)) / (es ** 2)) # ~40 studies at a minimum for media paradigm. Thus: simple 
# random sample over the literature retrieved in ASReview should have a size 40 at least. 

######################
##### References #####
######################

sessionInfo()

# R-packages.
packages <- c("devtools", "dplyr", "igraph",  "readr", "remotes", "revtools", "synthesisr", "litsearchr")
for (i in 1:length(packages)){
  print(citation(packages[i]))
}

# Gusenbauer, M., & Haddaway, N. R. (2020). Which academic search systems are suitable for systematic 
# reviews or meta-analyses? Evaluating retrieval qualities of Google Scholar, PubMed, and 26 other 
# resources. Research synthesis methods, 11(2), 181-217.

# Radhakrishnan, S., Erbis, S., Isaacs, J. A., & Kamarthi, S. (2017). Novel keyword co-occurrence 
# network-based methods to foster systematic reviews of scientific literature. PloS one, 12(3), e0172778.

# Rose, S., Engel, D., Cramer, N., & Cowley, W. (2010). Automatic keyword extraction from individual 
# documents. Text mining: applications and theory, 1, 1-20.
