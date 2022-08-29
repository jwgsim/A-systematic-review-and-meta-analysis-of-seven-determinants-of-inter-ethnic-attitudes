#############################################
##### Literature Search - Socialization #####
#############################################

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
### the socialization literature. More specifically, we focus on socialization by parents, peers, and the 
### ethnic in-group. The naive keywords are extracted from two sources, the review question and, a set of 
### gold-standard articles. The review question is formulated as: are group threat, group contact, media, 
### socialization, and the demographics age, gender, and education determinants of inter-ethnic attitudes in 
### the 2010-2022 period? Note that we identify four research paradigms in this research question: group 
### threat, group contact, media, and socialization. Effects of the various demographic variables are assumed 
### to be present in articles on the four main paradigms. In this specific R-file we limit the scope to the 
### socialization determinant. We split the constituent elements in the research question as being either a 
### determinant or an outcome of interest, a distinction that we will follow throughout the remainder of this 
### document:
naive_keywords <- c("socialization", "parent socialization", "parental socialization", "peer socialization", 
                    "ethnic in-group socialization", "inter-ethnic attitudes")

### The second and most important source of naive keywords are a set of six gold standard articles. These 
### articles have been selected by Eva Jaspers. Note that this set of articles is not assumed to be 
### representative of the literature that we aim to retrieve. We use the information within these articles
### to try and maximize coverage of this conceptually heterogeneous literature. We argue that due to this 
### heterogeneity, it is difficult to predict which keywords will and will not be relevant. As such, we start 
### by selecting a set of typical articles to include in a review, and use the keywords in these articles to 
### frame our initial search. The idea is that by combining the information in these articles with expert 
### judgement, we will be better able to construct search strings that cover most of the relevant 
### literature. 
## Importing the set of gold standard articles from the corresponding directory.
gs_socialization <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/1. Valida", 
                     "tion sets/1. Gold standard"), 
  verbose = TRUE)
## Request the length of the title object to show that there are 6 gold standard articles. Also request the
## object itself to show the titles of the articles themselves. 
length(gs_socialization$title)

### The keywords embedded in these gold standard articles are the keywords as they have been listed by the 
### authors themselves, and those keywords as identified by the Rapid Automatic Keyword Extraction Algorithm 
### (RAKE). The RAKE is a keyword extraction algorithm which tries to determine key phrases in a body of text 
### by analyzing the frequency of word appearance and its co-occurrence with other words in the text. Please 
### refer to Rose, Engel, Cramer, and Cowley (2010) for an overview of the RAKE. 
## Start by obtaining the keywords listed in the articles. We extract all non-single keywords that are listed
## at least once. Single word keywords were inspected for viability but excluded. 
sum(!is.na(gs_socialization$title)) # Note that all of the 6 gold standard articles list keywords. 
gs_tagged_keywords <- litsearchr::extract_terms(
  keywords = gs_socialization$keywords, # This is a list with the keywords from the gold standard articles. 
  method = "tagged", # Indicate that we want to obtain the keywords from the provided list.
  min_freq = 1, # The keyword has to occur a minimum of one time to be included.  
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 1, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
gs_tagged_keywords # Resulting keywords. 
## I subsequently make a selection based on the degree to which each of the keywords is relevant to the 
## effect of socialization on inter-ethnic attitudes. Note that I interpret this broadly. From the provided
## list, I include "anti-immigrant attitudes", "classroom", "ethnic harassment", "ethnic victimization",
## "friendship", "immigrant peers", "intergenerational transmission", "parents", "peers", "prejudice", 
## "prejudice development', "prejudice prevention", "prejudicial beliefs", "socialization". 
gs_tagged_keywords <- gs_tagged_keywords[c(2, 6, 12, 14, 15, 17, 19, 24, 25, 27, 28, 29, 30, 34)]
gs_tagged_keywords[5] <- "interethnic friendship" # Friendship to interethnic friendship.

## Use the RAKE to obtain keywords from the titles and abstracts of the gold standard articles. We extract 
## keywords that occur at least once in the titles and abstracts. 
gs_raked_keywords <- litsearchr::extract_terms(
  text = paste(gs_socialization$title, gs_socialization$abstract), # This is a list of the gold standard 
  # articles' titles and abstracts.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 1, # The keyword has to occur a minimum of two times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 1, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
gs_raked_keywords
## I subsequently make a selection based on the degree to which each of the keywords is relevant to the 
## direct and extended contact on inter-ethnic attitudes relationship. Note that I interpret this broadly. 
gs_raked_keywords <- gs_raked_keywords[c(20, 31, 48, 51, 52, 63, 64, 78, 81, 82, 88, 138, 140, 141, 143, 170, 
                                         174, 203, 208, 214, 215, 216, 235, 236, 260, 269, 271, 276, 321, 
                                         333, 334, 336, 345, 348, 349, 351, 354, 357, 358, 375, 377, 379, 
                                         380, 381, 401, 402, 425, 426, 427, 452, 453, 454, 499)]
gs_raked_keywords # The resulting keywords.
gs_raked_keywords[1] <- "adolescents anti-immigrant prejudice"
gs_raked_keywords[2] <- "adolescents intergroup friendships"
gs_raked_keywords[20] <- "friends attitudes"
gs_raked_keywords[33] <- "parent-offspring socialization"
gs_raked_keywords[36] <- "parents attitudes"
gs_raked_keywords[38] <- "peers anti-immigrant attitudes"
gs_raked_keywords[39] <- "peers prejudice"

## Combine the tagged and raked keywords from the gold standard articles.
gs_all_keywords <- c()
gs_all_keywords <- append(gs_all_keywords, c(gs_tagged_keywords, gs_raked_keywords, naive_keywords))
gs_all_keywords <- sort(gs_all_keywords)
gs_all_keywords <- remove_redundancies(gs_all_keywords, closure = "full") # Remove duplicate search terms.

gs_all_keywords[1] <- "adolescent anti-immigrant prejudice"
gs_all_keywords[2] <- "adolescent intergroup friendship"
gs_all_keywords[44] <- "peer anti-immigrant attitudes"
gs_all_keywords[45] <- "peer prejudice"

gs_all_keywords <- gs_all_keywords[-c(1, 2, 10, 16, 17, 18, 22, 30, 33, 34, 53)]

## Filter "gs_all_keywords" object. I do this on the basis of the keyword being either a dependent or 
## independent variable of interest, or being a prominent keywords in the title or abstract.

## Making a selection on whether a keyword is either a dependent or independent variable of interest, or is 
## prominent in the title or abstract of the gold standard articles.
gs_grouped_terms <- list(
  determinant = gs_all_keywords[c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 
                                  27, 28, 39, 30, 31, 32, 33, 34, 35, 39, 42, 45, 46, 47, 48, 49, 50)],
  outcome = c(gs_all_keywords[c(1, 2, 3, 4, 5, 20, 24, 36, 37, 38, 40, 41, 43, 44, 51)]))
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
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive search") # Set 
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
## Write the Boolean search for ProQuest. 
writeLines(bool, "naive_bool_proquest.txt")
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
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive search") # Set 
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
### This search was conducted on 12-08-2022. The documents are subsequently exported with "Format:" set to 
### "RIS" and "Fields:" set to "Complete Reference". If the total number of documents exceed 1500, this 
### exporting process is executed in batches of size 1500.

### In Proquest, we click on "Advanced Search", enter the Boolean search in the first line, set the value of 
### the "in" bar to "Anywhere except full text - NOFT", check the "Peer reviewed" box, select 
### "After this date..." in the "Publication date:" drop-down menu, and set the respective boxes to 
### "January", "1" and "2010", respectively. Under "Source type:" we select "Scholarly Journals" and under 
### "Language" we tick the "English" box. We subsequently click on "Search". On the next page, under 
### "Document type" we subsequently select "Article", where under "Language" we select "English". We set 
### "Items per page" to 100 and select the "EndNote" category under "All save & export options" for 
### exporting a RIS file for each page. This search was conducted on 12-08-2022.

### On the Scopus start page, we set the "Search within" tab to the default "Article title, Abstract, 
### Keywords" tab. We do not set it to the "All fields" tab, because doing so 1) retrieves a large number of 
### irrelevant documents, and 2) makes it difficult to export documents from Scopus. We subsequently enter 
### the Boolean search in the "Search documents" tab. We click on the "Add date range" button and set the 
### "Published from" tab to "2010", leaving the "To" tab to "Present", and the "Added to Scopus" tab to 
### "Anytime". We click "Search" and after having searched, scroll down to "Document type" and "Language" 
### under "Refine results", check the "Article" and "English" boxes, respectively, and click "Limit to". 
### This search was conducted on 12-08-2022. We subsequently select "All" and click on "Export" and select 
### "RIS Format". Note that I only export "Citation information", "Bibliographical information" and 
### "Abstracts & keywords" in Scopus because exporting the additional two categories "Funding details" and 
### "Other information" leads to merging issues later on. Note that we stratify the returned document set 
### by years when this number exceeds 2000. 

### In Web of Science, we click on "Advanced search", enter the Boolean search in the "Query Preview" search 
### box, and click "Search". We once again scroll down to "Document Types" and "Languages" under "Refine 
### results" and check the "Articles" and "English" and "Unspecified" boxes, respectively, if applicable. 
### This search was conducted on 12-08-2022. We subsequently click on "Select all records", "Export" and 
### then "RIS (other reference software):". We subsequently click on "Records from:" and export either the 
### total returned document set if this set is less than 1000, or export them in a per 1000 batch-wise 
### fashion if this number exceeds 1000, with "Record Content:" set to "Full Record". 

### The naive search resulted in 2,747 documents from Ovid (Selection: PsycINFO), 1,471 documents from 
### ProQuest (Selection: Sociological Abstracts), 5,267 documents from Scopus (Selection: Full index), and 
### 9,367 documents from Web of Science (Selection: Web of Science Core Collection) for a total of 18,852 
### documents. Please note that this document set is unfiltered, i.e., duplicates, retracted documents, 
### unidentified non-English documents, etc., have not yet been removed. The documents were manually 
### extracted in a single batch in Ovid, Scopus, and Web of Science, and in 100 sized batches in ProQuest.

### Data import and cleaning.
## Import results of initial, manual naive search. Note that the batches for each search system were merged
## into a single .ris file in EndNote before being imported. 
naive_import_ovid <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive ", 
                     "search/1. Unmerged/Ovid"),
  verbose = TRUE)
naive_import_proquest <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive ", 
                     "search/1. Unmerged/ProQuest"),
  verbose = TRUE)
naive_import_scopus <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive ", 
                     "search/1. Unmerged/Scopus"),
  verbose = TRUE)
naive_import_wos <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive ", 
                     "search/1. Unmerged/Web of Science"),
  verbose = TRUE)
## Checking whether the length of the imported .ris files are equal to the lengths of the raw .ris files.
length(naive_import_ovid$title) # 2,747, which is correct.  
length(naive_import_proquest$title) # 1,471, which is correct.  
length(naive_import_scopus$title) # 5,267, which is correct.  
length(naive_import_wos$title) # 9,367, which is correct.  

### We subsequently identify and remove identifiable, non-English documents.
## Ovid.
table(naive_import_ovid$language) # All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive search/2. ",
             "Merged/Ovid/1. Raw"))
write_refs(naive_import_ovid, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
## ProQuest.
table(naive_import_proquest$language) # ProQuest. All documents in the .ris file are in English.
naive_import_proquest <- naive_import_proquest[naive_import_proquest$language == "English" ,] # Select 
# English documents and store them. 1,468 documents in ProQuest document set.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive search/2. ",
             "Merged/ProQuest/1. Raw"))
write_refs(naive_import_proquest, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
## Scopus.
table(naive_import_scopus$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive search/2. ",
             "Merged/Scopus/1. Raw"))
write_refs(naive_import_scopus, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
## Web of Science.
table(naive_import_wos$language) # Web of Science. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive search/2. ",
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
length(naive_import_ovid$title) # Input is 2,747 documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  naive_import_ovid$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_ovid$title, exact_duplicates_ovid)) # Perform a 
# manual check. 
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # 13 documents should be removed.
naive_dedup_ovid <- synthesisr::extract_unique_references(naive_import_ovid, exact_duplicates_ovid) 
length(naive_dedup_ovid$title) # Output after exact matching is 2,734. 14 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_ovid <- find_duplicates( 
  naive_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. 1970 is not a duplicate combination. Remaining ones are. 
fuzzy_duplicates_ovid <- synthesisr::override_duplicates(fuzzy_duplicates_ovid, c(1970)) 
sum(table(fuzzy_duplicates_ovid) - 1) # Two documents should be removed.
naive_dedup_ovid <- extract_unique_references(naive_dedup_ovid, fuzzy_duplicates_ovid) # Extract unique 
# references. 
length(naive_dedup_ovid$title) # De-duplicated output is 2,732. Two documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive search/2. M",
             "erged/Ovid/2. Deduplicated"))
write_refs(naive_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_ovid")

## ProQuest.
## Exact matching.
length(naive_import_proquest$title) # Input is 1,468 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  naive_import_proquest$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_proquest$title, exact_duplicates_proquest)) # 
# Perform a manual check. All candidate combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 67 documents should be removed.
naive_dedup_proquest <- extract_unique_references(naive_import_proquest, exact_duplicates_proquest) 
length(naive_dedup_proquest$title) # Output after exact matching is 1,401. 67 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  naive_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. 437, 437, 437, 437, 438, 438, are not duplicate combinations. Remaining ones are. 
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, c(437, 437, 437, 
                                                                                          437, 438, 438)) 
sum(table(fuzzy_duplicates_proquest) - 1) # Five documents should be removed.
naive_dedup_proquest <- extract_unique_references(naive_dedup_proquest, fuzzy_duplicates_proquest) # Extract
# unique references. 
length(naive_dedup_proquest$title) # De-duplicated output is 1,396. Five documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  naive_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. 437, 437, 437, 437, 437, 437, 437, 437, are not duplicate combinations. Remaining
# ones are.  
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, c(437, 437, 437, 437, 
                                                                                          437, 437, 437, 
                                                                                          437)) 
sum(table(fuzzy_duplicates_proquest) - 1) # Two documents should be removed.
naive_dedup_proquest <- extract_unique_references(naive_dedup_proquest, fuzzy_duplicates_proquest) # 
# Extract unique references. 
length(naive_dedup_proquest$title) # De-duplicated output is 1,394. Two documents removed.
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive search/2. M",
             "erged/ProQuest/2. Deduplicated"))
write_refs(naive_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_proquest")

## Scopus.
## Exact matching.
length(naive_import_scopus$title) # Input is 5,267 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  naive_import_scopus$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_scopus$title, exact_duplicates_scopus)) # Perform 
# a manual check. All candidate combinations are duplicates.
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # 11 documents should be removed.
naive_dedup_scopus <- synthesisr::extract_unique_references(naive_import_scopus, exact_duplicates_scopus) 
length(naive_dedup_scopus$title) # Output after exact matching is 5,256. 11 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  naive_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform 
# a manual check. One duplicate combination identified.
sum(table(fuzzy_duplicates_scopus) - 1) # One document should be removed.
naive_dedup_scopus <- extract_unique_references(naive_dedup_scopus, fuzzy_duplicates_scopus) # Extract unique 
# references. 
length(naive_dedup_scopus$title) # De-duplicated output is 5,255. One document removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive search/2. M",
             "erged/Scopus/2. Deduplicated"))
write_refs(naive_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_scopus")

## Web of Science.
## Exact matching.
length(naive_import_wos$title) # Input is 9,367 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  naive_import_wos$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_wos$title, exact_duplicates_wos)) # Perform a 
# manual check. 
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Five documents should be removed.
naive_dedup_wos <- synthesisr::extract_unique_references(naive_import_wos, exact_duplicates_wos) 
length(naive_dedup_wos$title) # Output after exact matching is 9,362. Fivedocuments removed.
## Fuzzy matching five.
fuzzy_duplicates_wos <- find_duplicates( 
  naive_dedup_wos$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_wos$title, fuzzy_duplicates_wos)) # Perform a 
# manual check. 100, 4482 are not duplicate combinations. One duplicate combination identified.
fuzzy_duplicates_wos <- synthesisr::override_duplicates(fuzzy_duplicates_wos, c(100, 4482)) 
sum(table(fuzzy_duplicates_wos) - 1) # Zero documents should be removed.
naive_dedup_wos <- extract_unique_references(naive_dedup_wos, fuzzy_duplicates_wos) # Extract unique 
# references. 
length(naive_dedup_wos$title) # De-duplicated output is 9,361. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive search/2. M",
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
### as judged by the corresponding author, here the effect of group contact on inter-ethnic attitudes. 98 
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
cbind(gs_socialization$title, gs_socialization$year) 
## Ovid.
check_recall(gs_socialization$title[c(1, 2, 3, 5, 6)], naive_dedup_ovid$title) # 4 of 5 gold standard 
# articles are retrieved.
## ProQuest.
check_recall(gs_socialization$title[c(1, 2, 3, 5, 6)], naive_dedup_proquest$title) # 4 of 5 gold standard 
# articles are retrieved.
## Scopus.
check_recall(gs_socialization$title[c(1, 2, 3, 5, 6)], naive_dedup_scopus$title) # 5 of 5 gold standard 
# articles are retrieved.
## Web of Science.
check_recall(gs_socialization$title[c(1, 2, 3, 5, 6)], naive_dedup_wos$title) # 4 of 5 gold standard 
# articles are retrieved.
## The gold standard precision check tentatively indicates that Scopus has superior precision, followed 
## closely by Ovid, ProQuest, and Web of Science. Also note that all gold standard articles are retrieved
## over the sum of the documents sets. We as such continue with the rest of the search strategy.

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and those
## =< 0.5 are not.
ex_socialization <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/1. Validat", 
                     "ion sets/2. External"), 
  verbose = TRUE)
cbind(ex_socialization$title, ex_socialization$year) # Data frame of title and year of publication of 39 
# external articles.
## Ovid.
sum(round(as.numeric(check_recall(ex_socialization$title, naive_dedup_ovid$title)[, 3]), 0)) # 12 of 39
# external articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_socialization$title, naive_dedup_proquest$title)[, 3]), 0)) # 10 of 39 
# external articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_socialization$title, naive_dedup_scopus$title)[, 3]), 0)) # 19 of 39 
# external articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_socialization$title, naive_dedup_wos$title)[, 3]), 0)) # 23 of 39
# external articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision,
## followed by Scopus, Ovid, and Proquest, respectively. 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
## Set working directory. 
naive_dedup <- bind_rows(naive_dedup_ovid, naive_dedup_proquest, naive_dedup_scopus, naive_dedup_wos) # Bind 
# the corpora into a single corpus. 
## Exact matching.
length(naive_dedup$title) # Input is 18,742 documents.
exact_duplicates <- synthesisr::find_duplicates(
  naive_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(naive_dedup$title, exact_duplicates) # Perform a manual check. 
length(exact_manual$title) # Sum of 8,066 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 4,729 articles should be removed.
naive_dedup <- extract_unique_references(naive_dedup, exact_duplicates) 
length(naive_dedup$title) # Output after exact matching is 14,013. 4,729 documents removed.
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates( 
  naive_dedup$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
fuzzy_manual <- synthesisr::review_duplicates(naive_dedup$title, fuzzy_duplicates) # Perform a manual
# check. 
length(fuzzy_manual$title) # 933 duplicate combination candidates.
fuzzy_manual$title[1:500] # 1970, 3091, 3091, 3091, 3091, are not duplicate combinations. Remaining ones are.
fuzzy_manual$title[501:933] # 3753, 7294, 9362 are not duplicate combinations. Remaining ones are.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(1970, 3091, 3091, 3091, 3091, 3753, 7294, 9362)) 
sum(table(fuzzy_duplicates) - 1) # 464 documents should be removed.
naive_dedup <- extract_unique_references(naive_dedup, fuzzy_duplicates) # Extract unique references. 
length(naive_dedup$title) # De-duplicated output is 13,549. 464 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates( 
  naive_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
fuzzy_manual <- synthesisr::review_duplicates(naive_dedup$title, fuzzy_duplicates) # Perform a manual 
# check. All but one combination are duplicates.
length(fuzzy_manual$title) # 415 duplicate combinations.
fuzzy_manual$title # 40, 124, 124, 454, 1016, 1226, 1226, 1226, 1226, 1226, 1306, 1306, 1870, 1904, 1909, 
# 2155, 3077, 3077, 3077, 3077, 3077, 3077, 3077, 3077, 7203, 7823, 8683, 9192, are not duplicate 
# combinations. Remaining ones are.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(40, 124, 124, 454, 1016, 1226, 1226, 1226, 1226, 1226, 
                                                      1306, 1306, 1870, 1904, 1909, 2155, 3077, 3077, 3077, 
                                                      3077, 3077, 3077, 3077, 3077, 7203, 7823, 8683, 9192)) 
sum(table(fuzzy_duplicates) - 1) # 186 documents should be removed.
naive_dedup <- extract_unique_references(naive_dedup, fuzzy_duplicates) # Extract unique references. 
length(naive_dedup$title) # De-duplicated output is 13,363. 186 documents removed.
## Fuzzy matching fifteen.
fuzzy_duplicates <- find_duplicates( 
  naive_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 15) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup$title, fuzzy_duplicates)) # Perform a manual 
# check. All but one combination are duplicates.
fuzzy_manual$title # are not duplicate combinations. Remaining ones are.
fuzzy_manual$matches[185:196] # 40, 81, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 
# 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 154, 154, 237, 295, 295, 295, 295, 295, 295, 295, 
# 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 319, 405, 
# 441, 451, 451, 451, 451, 630, 827, 890, 944, 948, 997, 1007, 1093, 1183, 1201, 1379, 1467, 1486, 1501, 
# 1525, 1525, 1525, 1525, 1585, 1618, 1670, 1728, 1878, 1883, 1968, 1982, 1982, 2030, 2143, 2535, 2685, 2685, 
# 2704, 2891, 3042, 3042, 3042, 3042, 3042, 5670, 6100, 7890, 8464, 8863, 9114, 9642, are duplicate 
# combinations. Remaining ones are not.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(40, 81, 124, 124, 124, 124, 124, 124, 124, 124, 124, 
                                                      124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 
                                                      124, 124, 124, 124, 124, 154, 154, 237, 295, 295, 295, 
                                                      295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 
                                                      295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 295, 
                                                      295, 319, 405, 441, 451, 451, 451, 451, 630, 827, 890, 
                                                      944, 948, 997, 1007, 1093, 1183, 1201, 1379, 1467, 
                                                      1486, 1501, 1525, 1525, 1525, 1525, 1585, 1618, 1670, 
                                                      1728, 1878, 1883, 1968, 1982, 1982, 2030, 2143, 2535, 
                                                      2685, 2685, 2704, 2891, 3042, 3042, 3042, 3042, 3042, 
                                                      5670, 6100, 7890, 8464, 8863, 9114, 9642)) 
sum(table(fuzzy_duplicates) - 1) # 20 documents should be removed.
naive_dedup <- extract_unique_references(naive_dedup, fuzzy_duplicates) # Extract unique references. 
length(naive_dedup$title) # De-duplicated output is 13,343. 20 documents removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/2. Naive search/2. Merged")
write_refs(naive_dedup, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_refman")
# EndNote and Zotero both indicate that 35 duplicates remain in the .ris file. We furthermore remove three
# retracted papers: "Structural stigma and all-cause mortality in sexual minority populations" (1), 
# "Homeopathy combat against coronavirus disease (Covid-19)" (1), "Not-Sold-Here: How Attitudes Influence 
# External Knowledge Exploitation" (1). The result is exported in the file "naive_dedup.ris" which is 
# subsequently imported.  
naive_dedup <- read_bibliography("naive_dedup.ris")
length(naive_dedup$title) # 13,305 documents. 38 documents removed.

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_socialization$title, naive_dedup$title) # 6 of 6 gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_socialization$title, naive_dedup$title)[, 3]), 0)) # 24 of 39 external 
# articles are retrieved.

#########################################
##### Iteration 1 Literature Search #####
#########################################

### Now repeat the previous procedure, but with keywords extracted from the naive search corpus. This follows
### the same logic as earlier, due to the assumed high degree of heterogeneity of the literature, we combine 
### keyword information from the retrieved document set with author judgment to maximize coverage.

### Clear the environment except for the gold standard articles and "naive_dedup" objects. 
rm(list=setdiff(ls(), c("gs_grouped_terms", "gs_socialization", "ex_socialization", "naive_dedup")))

### Start by checking the naive search corpus for provided keywords, and keywords in titles and abstracts.
## Keywords.l
length(naive_dedup$keywords) # Total number of documents is 13,305. 
length(naive_dedup$keywords) - sum(is.na(naive_dedup$keywords)) # 13,083 of the articles list keywords.  
## Titles and abstracts.
length(naive_dedup$title) - sum(is.na(naive_dedup$title)) # All of the articles list a title.  
length(naive_dedup$abstract) - sum(is.na(naive_dedup$abstract)) # 12,113 of the articles list an abstract.  

### The first step is to obtain the keywords listed in the articles. We extract all non-single keywords that 
### are listed a minimum of forty times. Note that the forty number is arbitrary insofar that we need to 
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
  min_freq = 40, # The keyword has to occur a minimum of forty times to be included.   
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(tagged_keywords) # 333 tagged keywords.
### Now use the RAKE to obtain keywords from the titles and abstracts of naive search corpus. We extract all 
### non-single keywords that occur at least forty times in these titles and abstracts. 
raked_keywords <- litsearchr::extract_terms(
  text = paste(naive_dedup$title, naive_dedup$abstract), # This is a list of titles and abstracts 
  # per gold standard article.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 40, # The keyword has to occur a minimum of forty times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(raked_keywords) # 607 raked keywords.
## Sum total of tagged and raked keywords. 
keyword_candidates <- remove_redundancies(c(raked_keywords, tagged_keywords), closure = "full") # Remove 
# duplicates.
length(keyword_candidates) # Total of 808 keyword candidates. 
## The subsequent task is to select all keywords that are relevant to our query from the candidate pool. I 
## select terms that relate in content to the relevant literature, i.e., are a independent or dependent 
## variable of interest in the socialization on inter-ethnic attitudes relationship. Note that I interpret 
## relevance quite broadly, since these terms will be will be ranked and filtered further based on their 
## "connectedness" in a co-occurrence network. In that sense, it is better too be too liberal than too 
## selective in our choices here, since it might be hard to anticipate which relevant terms are important 
## from this "connectedness" perspective. I furthermore do not include keywords which relate directly to any 
## of the other paradigms. Finally note that I do this part manually, which is prone to errors. 
all_keywords <- keyword_candidates[
  c(15, 16, 27, 28, 39, 40, 99, 104, 119, 132, 144, 145, 204, 209, 226, 227, 231, 270, 287, 298, 312, 346, 
    360, 361, 362, 371, 382, 384, 385, 386, 401, 406, 407, 416, 418, 455, 456, 457, 458, 459, 460, 461, 496, 
    501, 506, 516, 517, 541, 546, 582, 583, 602, 603, 604, 607, 613, 614, 631, 634, 658, 659, 662, 670, 671, 
    672, 673, 690, 691, 692, 730, 731, 736, 750, 752, 760, 761, 791, 792, 796)
]
## Manual cleaning.
(all_keywords <- sort(all_keywords))
length(all_keywords) # 79 keyword candidates.

### We further filter the keyword set by ranking the relative strength of each keyword in a so-called keyword 
### co-occurrence network (KCN). In a KCN, each keyword is "represented as a node and each co-occurrence of 
### a pair of words is represented as a link" (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). "The number 
### of times that a pair of words co-occurs in multiple articles constitutes the weight of the link 
### connecting the pair" (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). "The network constructed in this 
### manner represents cumulative knowledge of a domain and helps to uncover meaningful knowledge components 
### and insights based on the patterns and strength of links between keywords that appear in the literature" 
### (Radhakrishnan, Erbis, Isaacs, & Kamarthi, 2017). 
all_keywords_final <- c()
all_keywords_final <- append(all_keywords_final, c(as.vector(unlist(gs_grouped_terms)), all_keywords))
all_keywords_final <- remove_redundancies(all_keywords_final, closure = "full") # Remove duplicates. 
length(all_keywords_final) # 122 candidates.
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
## I now apply a filter to select 58 terms for the search string in ovid, proquest, scopus, and web of 
## science, where I remove terms which refer to an equivalent or near equivalent concept, i.e., "target 
## group" and "target groups", where I select the term with the highest strength value. Note that the 58 
## number is arbitrary, and a result of a trial-and-error process which aimed to balance the number of 
## retrieved documents and the incorporating of as many relevant search terms as possible.
(term_strengths <- cbind(seq(length(term_strengths$term)), term_strengths[order(term_strengths$term), ]))
term_strengths <- term_strengths[-c(7, 15, 20, 23, 33, 37, 44, 47, 48, 60, 64, 68, 69, 70, 78, 80, 96, 98, 
                                    100, 101, 103, 104), ]

### Construct Boolean search OVID, ProQuest, Web of Science, and Scopus.
## Select first 57 terms from filtered term set. 
keywords_ovid_scopus_wos <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), ]
                                         [1:57, ]$term)
keywords_ovid_scopus_wos <- append(keywords_ovid_scopus_wos, 
                                   c("peer socialization", "parental socialization", 
                                     "ingroup socialization"))
(keywords_ovid_scopus_wos <- keywords_ovid_scopus_wos[order(keywords_ovid_scopus_wos)])
keywords_ovid_scopus_wos <- keywords_ovid_scopus_wos[-c(3, 48)]
keywords_ovid_scopus_wos
## Categorize "keywords_ovid_scopus_wos" object based on keyword being a potential dependent or 
## independent variable of interest in the socialization on inter-ethnic attitudes relationship.
grouped_terms_ovid_scopus_wos <- list(
  determinant = keywords_ovid_scopus_wos[c(3, 4, 5, 6, 7, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 24, 27, 
                                           28, 29, 30, 31, 32, 36, 37, 39, 42, 43, 44, 45, 46, 47, 48, 49,
                                           50, 51, 52, 54, 55, 56, 57, 58)],
  outcome = keywords_ovid_scopus_wos[c(1, 2, 8, 9, 10, 21, 22, 23, 25, 26, 33, 34, 35, 38, 40, 41, 53)])
grouped_terms_ovid_scopus_wos

### Given the grouped terms, write the Boolean search for OVID and Web of Science to the directory and print 
### it to the console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1") # Set directory.
write_search(
  grouped_terms_ovid_scopus_wos, # The list of determinant and outcome keywords identified in the naive 
  # document grouped_terms_ovid_scopus_wos
  languages = "English", # Language to write the search in, here set to English.
  exactphrase = TRUE, # Whether terms that consist of more than one word should be matched exactly rather 
  # than as two separate words. Set to TRUE, to limit both the scope and the number of redundant documents.
  stemming = TRUE, # Whether words are stripped down to the smallest meaningful part of the word to catch 
  # all variants of the word. Set to FALSE, because the number of retrieved documents is much too large if 
  # it is set to TRUE.
  closure = "none", # Whether partial matches are matched at the left end of a word ("left"), at the right 
  # ("right"), only as exact matches ("full"), or as any word containing a term ("none"). Set to "none" to
  # retrieve as many documents as possible.
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

### Given the grouped terms, write the Boolean search for ProQuest to the directory and print it to the 
### console.  
grouped_terms_proquest <- vector()
grouped_terms_proquest$determinant <- grouped_terms_ovid_scopus_wos$determinant
grouped_terms_proquest$outcome <- grouped_terms_ovid_scopus_wos$outcome[-c(8)] # I remove the "intergroup 
# relations" term because it severely blows up the search in ProQuest.
keywords_proquest <- keywords_ovid_scopus_wos[-c(23)]
## Finally save the grouped terms to a text file in the working directory. I do this to keep the search 
## reproducible in case some previous chunk of code does not replicate for any particular reason.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1") # Set directory.
sink("first_iteration_search_terms_proquest.txt")
print(grouped_terms_proquest)
sink() 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1") # Set directory.
write_search(
  grouped_terms_proquest, # The list of determinant and outcome keywords identified in the naive 
  # document grouped_terms_ovid_proquest_scopus_wos
  languages = "English", # Language to write the search in, here set to English.
  exactphrase = TRUE, # Whether terms that consist of more than one word should be matched exactly rather 
  # than as two separate words. Set to TRUE, to limit both the scope and the number of redundant documents.
  stemming = TRUE, # Whether words are stripped down to the smallest meaningful part of the word to catch 
  # all variants of the word. Set to FALSE, because the number of retrieved documents is much too large if 
  # it is set to TRUE in the ProQuest and Scopus databases.
  closure = "none", # Whether partial matches are matched at the left end of a word ("left"), at the right 
  # ("right"), only as exact matches ("full"), or as any word containing a term ("none"). Set to "none" to
  # retrieve as many documents as possible.
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

### Please refer to the naive iteration for an overview of the exact steps of the search procedure. 

### The searches in Ovid, Scopus, and Web of Science were conducted on 24-08-2022. The search in ProQuest
### was conducted on 25-08-2022. These searches resulted in 9,282 documents from Ovid (Selection: PsycINFO), 
### 5,886 from ProQuest (Selection: Sociological Abstracts), 13,442 documents from Scopus (Selection: Full
### index), and 14,935 documents from Web of Science (Selection: Web of Science Core Collection), for a 
### total of 43,545 documents. Please note that this document set is unfiltered, i.e., duplicates, retracted 
### documents, unidentified non-English documents, etc., have not yet been removed. In Ovid, the documents 
### were manually extracted in three batches. In ProQuest the documents were manually extracted in 100-sized 
### batches. In Scopus, the documents were manually extracted in 2000 document sized batches, stratified by 
### year. In Web of Science, the documents were manually extracted in 1000 document sized batches. 

### Data import and cleaning.
## Import results of informed search. Note that the batches for each search system were merged in EndNote.
## Start by checking whether the imported length is equal to the length of the raw .ris files.
import_ovid_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iterat", 
                     "ion 1/1. Unmerged/Ovid"),
  verbose = TRUE)
import_proquest_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iterat", 
                     "ion 1/1. Unmerged/ProQuest"),
  verbose = TRUE)
import_scopus_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iterat", 
                     "ion 1/1. Unmerged/Scopus"), 
  verbose = TRUE)
import_wos_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iterat", 
                     "ion 1/1. Unmerged/Web of Science"),
  verbose = TRUE)
length(import_ovid_it1$title) # 9,282, which is correct.  
length(import_proquest_it1$title) # 5,886, which is correct.  
length(import_scopus_it1$title) # 13,442, which is correct.
length(import_wos_it1$title) # 14,935, which is correct.  

### We subsequently identify and remove identifiable, non-English documents. 
## Ovid.
table(import_ovid_it1$language) # Ovid. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1/2. Me",
             "rged/Ovid/1. Raw"))
write_refs(import_ovid_it1, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
## ProQuest.
table(import_proquest_it1$language) # ProQuest. All documents in the .ris file are in English.
import_proquest_it1 <- import_proquest_it1[import_proquest_it1$language == "English" ,] # Select English 
# documents and store them. 5,871 documents in ProQuest document set.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1/2. Me", 
             "rged/ProQuest/1. Raw"))
write_refs(import_proquest_it1, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
## Scopus.
table(import_scopus_it1$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1/2. Me", 
             "rged/Scopus/1. Raw"))
write_refs(import_scopus_it1, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
## Web of Science.
table(import_wos_it1$language) # Web of Science. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1/2. Me", 
             "rged/Web of Science/1. Raw"))
write_refs(import_wos_it1, format = "ris", tag_naming = "synthesisr", file = "web_of_science_r")

### De-duplication. Please refer to the naive iteration for an overview of the exact steps of the 
### de-duplication procedure.

### Ovid.
## Exact matching.
length(import_ovid_it1$title) # Input is 9,282 documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  import_ovid_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_ovid_it1$title, exact_duplicates_ovid)) # Perform a 
# manual check. 
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # 27 documents should be removed.
it1_dedup_ovid <- synthesisr::extract_unique_references(import_ovid_it1, exact_duplicates_ovid) 
length(it1_dedup_ovid$title) # Output after exact matching is 9,255. 27 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_ovid <- find_duplicates( 
  it1_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. 2161, 6561, 7139, are not duplicate combinations. Remainings ones are.
fuzzy_duplicates_ovid <- synthesisr::override_duplicates(fuzzy_duplicates_ovid, 
                                                         c(2161, 6561, 7139)) 
sum(table(fuzzy_duplicates_ovid) - 1) # Six documents should be removed.
it1_dedup_ovid <- extract_unique_references(it1_dedup_ovid, fuzzy_duplicates_ovid) # Extract unique 
# references. 
length(it1_dedup_ovid$title) # De-duplicated output is 9,249. Six documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_ovid <- find_duplicates( 
  it1_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. No duplicate combinations identified.
length(it1_dedup_ovid$title) # De-duplicated output is 9,249. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1/2. Mer", 
             "ged/Ovid/2. Deduplicated"))
write_refs(it1_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_ovid")

### ProQuest. 
## Exact matching.
length(import_proquest_it1$title) # Input is 5,871 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  import_proquest_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_proquest_it1$title, exact_duplicates_proquest)) # All
# combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 285 articles should be removed.
it1_dedup_proquest <- extract_unique_references(import_proquest_it1, exact_duplicates_proquest) 
length(it1_dedup_proquest$title) # Output after exact matching is 5,586. 285 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  it1_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. 4828, 4828, 4828, 4828, 4829.
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, 
                                                         c(4828, 4828, 4828, 4828, 4829)) 
sum(table(fuzzy_duplicates_proquest) - 1) # 22 documents should be removed.
it1_dedup_proquest <- extract_unique_references(it1_dedup_proquest, fuzzy_duplicates_proquest) # 
# Extract unique references. 
length(it1_dedup_proquest$title) # De-duplicated output is 5,564. 22 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  it1_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. 384, 384, 384, 522, 533, 533, 684, 983, 2090, 2205, 4429, 4816, 4816, 4816, 4816, 
# 4816, 4816, 4816, 4816.
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, 
                                                             c(384, 384, 384, 522, 533, 533, 684, 983, 2090, 
                                                               2205, 4429, 4816, 4816, 4816, 4816, 4816, 
                                                               4816, 4816, 4816)) 
sum(table(fuzzy_duplicates_proquest) - 1) # Six documents should be removed.
it1_dedup_proquest <- extract_unique_references(it1_dedup_proquest, fuzzy_duplicates_proquest) # Extract 
# unique references. 
length(it1_dedup_proquest$title) # De-duplicated output is 5,558. Six documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1/2. Mer", 
             "ged/ProQuest/2. Deduplicated"))
write_refs(it1_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_proquest")

### Scopus.
## Exact matching.
length(import_scopus_it1$title) # Input is 13,442 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  import_scopus_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_scopus_it1$title, exact_duplicates_scopus)) # Perform a 
# manual check. 
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # 21 documents should be removed.
it1_dedup_scopus <- synthesisr::extract_unique_references(import_scopus_it1, exact_duplicates_scopus) 
length(it1_dedup_scopus$title) # Output after exact matching is 13,421. 21 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  it1_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. One duplicate combination identified.
fuzzy_duplicates_scopus <- synthesisr::override_duplicates(fuzzy_duplicates_scopus, c(7888, 9752)) 
sum(table(fuzzy_duplicates_scopus) - 1) # One documents should be removed.
it1_dedup_scopus <- extract_unique_references(it1_dedup_scopus, fuzzy_duplicates_scopus) # Extract unique 
# references. 
length(it1_dedup_scopus$title) # De-duplicated output is 13,420. One document removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1/2. Mer", 
             "ged/Scopus/2. Deduplicated"))
write_refs(it1_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_scopus")

### Web of Science.
length(import_wos_it1$title) # Input is 14,935 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  import_wos_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_wos_it1$title, exact_duplicates_wos)) # Perform a 
# manual check. 
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Five documents should be removed.
it1_dedup_wos <- synthesisr::extract_unique_references(import_wos_it1, exact_duplicates_wos) 
length(it1_dedup_wos$title) # Output after exact matching is 14,930. Five documents removed.
## Fuzzy matching five.
fuzzy_duplicates_wos <- find_duplicates( 
  it1_dedup_wos$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_wos$title, fuzzy_duplicates_wos)) # Perform a 
# manual check. One duplicate combination identified.
fuzzy_duplicates_wos <- synthesisr::override_duplicates(fuzzy_duplicates_wos, c(1634, 10275)) 
sum(table(fuzzy_duplicates_wos) - 1) # One document should be removed.
it1_dedup_wos <- extract_unique_references(it1_dedup_wos, fuzzy_duplicates_wos) # Extract unique 
# references. 
length(it1_dedup_wos$title) # De-duplicated output is 14,929. One document removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1/2. Mer", 
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
cbind(gs_socialization$title, gs_socialization$year)
## Ovid.
sum(round(as.numeric(check_recall(gs_socialization$title, it1_dedup_ovid$title)[, 3]), 0)) # 4 of 5 gold 
# standard articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_socialization$title, it1_dedup_proquest$title)[, 3]), 0)) # 4 of 5 gold 
# standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_socialization$title, it1_dedup_scopus$title)[, 3]), 0)) # 5 of 5 gold 
# standard articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_socialization$title, it1_dedup_wos$title)[, 3]), 0)) # 4 of 5 gold 
# standard articles are retrieved.
## The gold standard precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus and Ovid, and finally ProQuest. This precision has remained constant in Ovid (0), 
## ProQuest (0), Scopus (0), and Web of Science (0).

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
cbind(ex_socialization$title, ex_socialization$year) # Data frame of title and year of publication of 98 
# external articles.
## Ovid.
sum(round(as.numeric(check_recall(ex_socialization$title, it1_dedup_ovid$title)[, 3]), 0)) # 16 of 39 
# external articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_socialization$title, it1_dedup_proquest$title)[, 3]), 0)) # 14 of 39 
# external articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_socialization$title, it1_dedup_scopus$title)[, 3]), 0)) # 26 of 39 
# external articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_socialization$title, it1_dedup_wos$title)[, 3]), 0)) # 24 of 39 
# external articles are retrieved.  
## The external precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus, Ovid, and ProQuest, respectively. Relative to the naive search, the precision with 
## respect to the external articles has increased in Ovid (+4), ProQuest (+4), Scopus (+7), and Web of 
## Science (+1). 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
it1_dedup <- bind_rows(it1_dedup_ovid, it1_dedup_proquest, it1_dedup_scopus, it1_dedup_wos) # Merge corpora. 
## Exact matching.
length(it1_dedup$title) # Input is 43,156 documents.
exact_duplicates <- synthesisr::find_duplicates(
  it1_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(it1_dedup$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 26,796 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 16,259 documents should be removed.
it1_dedup <- synthesisr::extract_unique_references(it1_dedup, exact_duplicates) 
length(it1_dedup$title) # Output after exact matching is 26,897. 16,259 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
fuzzy_manual <- review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check.
length(fuzzy_manual$title) # 2233 potential duplicate combinations. I check these candidates manually. Note 
# that this procedure is prone to error.
fuzzy_manual$title[1:500] # 2161, 2161, 2161, are not duplicate combinations. Remaining ones are. 
fuzzy_manual$title[501:1000] # 6026 is not a duplicate combination. Remaining ones are. 
fuzzy_manual$title[1001:1500] # 6561, 7139, 9111, are not duplicate combinations. Remaining ones are. 
fuzzy_manual$title[1501:2223] # 12480, 12480, 12480, 12480, 13001, 13001, 24024, are not duplicate 
# combinations. Remaining ones are. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(2161, 2161, 2161, 6026, 6561, 7139, 
                                                                        9111, 12480, 12480, 12480, 12480, 
                                                                        13001, 13001, 24024)) 
sum(table(fuzzy_duplicates) - 1) # 1,115 document should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references.
length(it1_dedup$title) # De-duplicated output is 25,782. 1,115 documents were removed.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 1060 duplicate combinations.
fuzzy_manual$title[1:500] # 161, 239, 323, 1199, 1199, 1199, 1664, 1717, 2017, 2017, 2017, 2017, 2017, 2017,
# 2047, 2047, 2047, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2268, 2482, 2482, 2482, 2911, 2911, 3096, 3124,
# 3250, 3604, 3697, 3844, 3908, 4249, 4258, 4485, 4670, 4682, 4788, 4788, 4788, 4893, 5460, 5460, 5637, 5655,
# 5733, 5814, 5814, 5915, 5915, 5915, 5915, 5915, 5915, 6003, 6311, 6470, 6697, 6878, 6878, 7115, 7120, 7120, 
# 7205, 7351, 7526, 7620, 7620, 8340, 8345, 8345, 8345, 8345, 8345, 8345, 8349, 8569, 8804, 8910, 8912, are 
# not duplicate combinations. 
fuzzy_manual$title[501:1060] # 9554, 9554, 9554, 9554, 9647, 12084, 12390, 12390, 12390, 12390, 12390, 12390,
# 12390, 12390, 12894, 12894, 12894, 14700, 15003, 15801, 22660, 23539, are not duplicate combinations. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(161, 239, 323, 1199, 1199, 1199, 1664, 1717, 2017, 
                                                      2017, 2017, 2017, 2017, 2017, 2047, 2047, 2047, 2268, 
                                                      2268, 2268, 2268, 2268, 2268, 2268, 2268, 2482, 2482, 
                                                      2482, 2911, 2911, 3096, 3124, 3250, 3604, 3697, 3844, 
                                                      3908, 4249, 4258, 4485, 4670, 4682, 4788, 4788, 4788, 
                                                      4893, 5460, 5460, 5637, 5655, 5733, 5814, 5814, 5915, 
                                                      5915, 5915, 5915, 5915, 5915, 6003, 6311, 6470, 6697, 
                                                      6878, 6878, 7115, 7120, 7120, 7205, 7351, 7526, 7620, 
                                                      7620, 8340, 8345, 8345, 8345, 8345, 8345, 8345, 8349, 
                                                      8569, 8804, 8910, 8912, 9554, 9554, 9554, 9554, 9647, 
                                                      12084, 12390, 12390, 12390, 12390, 12390, 12390, 12390, 
                                                      12390, 12894, 12894, 12894, 14700, 15003, 15801, 22660, 
                                                      23539)) 
sum(table(fuzzy_duplicates) - 1) # 447 documents should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references. 
length(it1_dedup$title) # De-duplicated output is 25,335. 447 documents removed. 
## Fuzzy matching fifteen.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Fuzzy matching ten output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 15) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 626 duplicate combinations.
fuzzy_manual$title[1:626] # 86, 86, 86, 106, 108, 159, 171, 197, 197, 197, 197, 199, 237, 262, 273, 273, 273, 
# 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273,
# 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 312, 319, 361, 513, 
# 629, 629, 629, 629, 643, 665, 857, 874, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 
# 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 
# 970, 970, 970, 970, 970, 970, 1138, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 
# 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 
# 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 
# 1189, 1189, 1189, 1189, 1189, 1245, 1246, 1247, 1248, 1249, 1250, 1254, 1287, 1573, 1596, 1629, 1652, 1682, 
# 1705, 1752, 1752, 1752, 1800, 1813, 1813, 1813, 2002, 2002, 2002, 2002, 2002, 2002, 2066, 2068, 2081, 2640,
# 2640, 2640, 2753, 2762, 2781, 2802, 2883, 3064, 3064, 3092, 3092, 3092, 3092, 3135, 3189, 3217, 3248, 3285, 
# 3335, 3408, 3632, 3755, 3755, 3755, 3790, 3802, 3866, 3866, 3898, 3902, 3903, 3903, 3903, 3930, 3930, 4029, 
# 4089, 4213, 4244, 4269, 4439, 4546, 4623, 4635, 4720, 4725, 4771, 4816, 4829, 4844, 4915, 5042, 5165, 5180, 
# 5180, 5180, 5180, 5193, 5332, 5351, 5408, 5408, 5570, 5570, 5570, 5583, 5601, 5601, 5769, 5785, 5886, 5886, 
# 5936, 5936, 5944, 5944, 5944, 5944, 6093, 6114, 6327, 6495, 6601, 6607, 6607, 6633, 6692, 6994, 7044, 7044,
# 7044, 7115, 7275, 7581, 7772, 8001, 8256, 8264, 8331, 8430, 8480, 8581, 8818, 8820, 8891, 9283, 9283, 9283,
# 9456, 10049, 10108, 10271, 10433, 10433, 10876, 11027, 12253, 12253, 12254, 12254, 12254, 12254, 12254, 
# 12254, 12254, 12269, 13224, 13662, 13662, 14376, 14376, 14490, 14642, 14680, 14873, 14937, 17935, 17935, 
# 18784, 22394, 23257, are not duplicate combinations, remaining combinations are. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(86, 86, 86, 106, 108, 159, 171, 197, 197, 197, 197, 
                                                      199, 237, 262, 273, 273, 273, 273, 273, 273, 273, 273, 
                                                      273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 
                                                      273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 
                                                      273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 273, 
                                                      312, 319, 361, 513, 629, 629, 629, 629, 643, 665, 857, 
                                                      874, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 
                                                      970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 
                                                      970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 970, 
                                                      970, 970, 970, 970, 970, 970, 970, 970, 1138, 1189, 
                                                      1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 
                                                      1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 
                                                      1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 
                                                      1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 
                                                      1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 
                                                      1189, 1189, 1189, 1189, 1189, 1189, 1189, 1245, 1246, 
                                                      1247, 1248, 1249, 1250, 1254, 1287, 1573, 1596, 1629, 
                                                      1652, 1682, 1705, 1752, 1752, 1752, 1800, 1813, 1813, 
                                                      1813, 2002, 2002, 2002, 2002, 2002, 2002, 2066, 2068, 
                                                      2081, 2640, 2640, 2640, 2753, 2762, 2781, 2802, 2883, 
                                                      3064, 3064, 3092, 3092, 3092, 3092, 3135, 3189, 3217, 
                                                      3248, 3285, 3335, 3408, 3632, 3755, 3755, 3755, 3790, 
                                                      3802, 3866, 3866, 3898, 3902, 3903, 3903, 3903, 3930, 
                                                      3930, 4029, 4089, 4213, 4244, 4269, 4439, 4546, 4623, 
                                                      4635, 4720, 4725, 4771, 4816, 4829, 4844, 4915, 5042, 
                                                      5165, 5180, 5180, 5180, 5180, 5193, 5332, 5351, 5408, 
                                                      5408, 5570, 5570, 5570, 5583, 5601, 5601, 5769, 5785, 
                                                      5886, 5886, 5936, 5936, 5944, 5944, 5944, 5944, 6093, 
                                                      6114, 6327, 6495, 6601, 6607, 6607, 6633, 6692, 6994, 
                                                      7044, 7044, 7044, 7115, 7275, 7581, 7772, 8001, 8256, 
                                                      8264, 8331, 8430, 8480, 8581, 8818, 8820, 8891, 9283, 
                                                      9283, 9283, 9456, 10049, 10108, 10271, 10433, 10433, 
                                                      10876, 11027, 12253, 12253, 12254, 12254, 12254, 12254, 
                                                      12254, 12254, 12254, 12269, 13224, 13662, 13662, 14376, 
                                                      14376, 14490, 14642, 14680, 14873, 14937, 17935, 17935, 
                                                      18784, 22394, 23257)) 
sum(table(fuzzy_duplicates) - 1) # 74 documents should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references. 
length(it1_dedup$title) # De-duplicated output is 25,261. 74 documents removed. 
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1/2. Merged")
write_refs(it1_dedup, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_refman")
# EndNote and Zotero indicate that 91 duplicates remain in the .ris file, which are removed. We furthermore 
# remove three retracted papers that are flagged in EndNote and Zotero: "When contact changes minds: An 
# experiment on transmission of support for gay equality" (2), "Racist biases in legal decisions are reduced 
# by a justice focus" (1), "The Evolution of Intergroup Bias: Perceptions and Attitudes in Rhesus Macaques" 
# (3), "Stereotype Disconfirmation Affect: When Sweet Hooligans Make You Happy and Honest Salesmen Make You 
# Sad" (3), "Bridging the Gap on Facebook: Assessing Intergroup Contact and Its Effects for Intergroup 
# Relations" (3), and ""Tightening the shackles": The continued invisibility of Liverpool's British African 
# Caribbean teachers" (2).  
# The result is exported from Zotero in the file "it1_dedup.ris" which is subsequently imported.  
it1_dedup <- read_bibliography("it1_dedup.ris")
length(it1_dedup$title) # 25,156 documents. 105 documents removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_socialization$title, it1_dedup$title) # 4 of 5 gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_socialization$title, it1_dedup$title)[, 3]), 0)) # 27 of 39 or 69.23% 
# of external articles are retrieved. +3 increase relative to naive search.

### Evaluate first iteration search relative to naive search. 

## Here I do not use the "check_recall" function because it takes a long time, and it is not exact / 
## conservative enough, for the purpose of merging the naive search documents that are not in the first 
## iteration search. I do this to try and maximize coverage. 
length(naive_dedup$title) # 13,305 documents.
length(it1_dedup$title) # 25,156 documents. 
overlap_naive_it1 <- bind_rows(naive_dedup, it1_dedup) # Merge corpora. 
## Exact matching.
length(overlap_naive_it1$title) # Input is 38,461 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_naive_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_naive_it1$title, exact_duplicates) # Perform a manual
# check.
length(exact_manual$title) # Sum of 14,184 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 7,092 documents should be removed.
it1_dedup_out <- synthesisr::extract_unique_references(overlap_naive_it1, exact_duplicates) 
length(it1_dedup_out$title) # Output after exact matching is 31,369. 7,092 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it1_dedup_out$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it1_dedup_out$title, fuzzy_duplicates)) # Perform a manual check. 3036, 
# 3036, 3036, 3036, 13205, 13250, 13250, 13260, 13291, 18373, 29921, 31148, 31148, are not duplicate 
# combinations. Remaining ones are. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(3036, 3036, 3036, 3036, 13205, 13250, 13250, 13260, 
                                                      13291, 18373, 29921, 31148, 31148))
sum(table(fuzzy_duplicates) - 1) # 64 documents should be removed.
it1_dedup_out <- extract_unique_references(it1_dedup_out, fuzzy_duplicates) # Extract unique references.
length(it1_dedup_out$title) # De-duplicated output is 31,305. 64 documents were removed.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it1_dedup_out$title, # Fuzzy matching five as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # default cutoff. 
(fuzzy_manual <- review_duplicates(it1_dedup_out$title, fuzzy_duplicates)) # Perform a manual check. 40, 124,
# 124, 124, 124, 451, 451, 451, 451, 451, 451, 451, 451, 451, 453, 1007, 1525, 1618, 1844, 1878, 1883, 2125, 
# 2516, 2887, 2959, 3036, 3036, 3036, 3036, 3036, 3036, 3036, 3036, 3118, 5653, 5788, 7119, 7354, 12839, 
# 13202, 13202, 13203, 13203, 13203, 13203, 13203, 13203, 13204, 13204, 13204, 13204, 13204, 13204, 13204, 
# 13204, 13204, 13204, 13204, 13205, 13207, 13209, 13209, 13210, 13210, 13210, 13210, 13210, 13210, 13210, 
# 13210, 13211, 13213, 13214, 13214, 13215, 13222, 13222, 13226, 13226, 13226, 13226, 13226, 13226, 13226, 
# 13226, 13226, 13228, 13228, 13248, 13259, 13268, 13273, 13457, 13515, 14454, 15457, 15478, 15572, 16450, 
# 16579, 16589, 16745, 17159, 17159, 17279, 17293, 18508, 19205, 19608, 19610, 29220, 29888, 31017, 31018, 
# 31042, 31050, 31051, 31052, 31097, 31120, 31120, are not duplicate combinations, remaining ones are. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(40, 124, 124, 124, 124, 451, 451, 451, 451, 451, 451, 
                                                      451, 451, 451, 453, 1007, 1525, 1618, 1844, 1878, 1883, 
                                                      2125, 2516, 2887, 2959, 3036, 3036, 3036, 3036, 3036, 
                                                      3036, 3036, 3036, 3118, 5653, 5788, 7119, 7354, 12839, 
                                                      13202, 13202, 13203, 13203, 13203, 13203, 13203, 13203, 
                                                      13204, 13204, 13204, 13204, 13204, 13204, 13204, 13204, 
                                                      13204, 13204, 13204, 13205, 13207, 13209, 13209, 13210, 
                                                      13210, 13210, 13210, 13210, 13210, 13210, 13210, 13211, 
                                                      13213, 13214, 13214, 13215, 13222, 13222, 13226, 13226, 
                                                      13226, 13226, 13226, 13226, 13226, 13226, 13226, 13228, 
                                                      13228, 13248, 13259, 13268, 13273, 13457, 13515, 14454, 
                                                      15457, 15478, 15572, 16450, 16579, 16589, 16745, 17159, 
                                                      17159, 17279, 17293, 18508, 19205, 19608, 19610, 29220, 
                                                      29888, 31017, 31018, 31042, 31050, 31051, 31052, 31097, 
                                                      31120, 31120))
sum(table(fuzzy_duplicates) - 1) # Two documents should be removed.
it1_dedup_out <- extract_unique_references(it1_dedup_out, fuzzy_duplicates) # Extract unique references.
length(it1_dedup_out$title) # De-duplicated output is 31,303. TWo documents were removed. 
## Save .ris file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Socialization/3. Iteration 1")
write_refs(it1_dedup_out, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_out_refman") # Export
# as .ris. 
write_refs(it1_dedup_out, format = "bib", tag_naming = "synthesisr", file = "it1_dedup_out_refman") # Export
# as .bib for Zotero. 
# EndNote and Zotero indicate that six duplicates remain in the .ris file, which are removed. In addition, 
# four more retractions were identified: "Helping the ingroup versus harming the outgroup: Evidence from 
# morality-based groups" (2), "Structural stigma and all-cause mortality in sexual minority populations" (2), 
# "Ethnic threat and social control: Examining public support for judicial use of ethnicity in punishment" 
# (2), and "Coping with chaos: How disordered contexts promote stereotyping and discrimination" (1). The 
# result is exported from Zotero in the file "it1_dedup_out.ris" which is subsequently imported.  
it1_dedup_out <- read_bibliography("it1_dedup_out.ris")
length(it1_dedup_out$title) # 31,289 documents. 13 documents removed. This is the final corpus file of the 
# first iteration search.

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_socialization$title, it1_dedup_out$title) # 5 of 5 gold gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_socialization$title, it1_dedup_out$title)[, 3]), 0)) # 27 of 39 external 
# articles or 69,23% are retrieved.

## Inspect coverage gain relative to the naive search. I define coverage as the sum of the additional 
## documents that were found relative to the previous search iteration, and the documents from the previous 
## search iteration that were not identified in the current one. 
length(naive_dedup$title) # 13,305 articles.
length(it1_dedup$title) # 25,156 articles.
(length(it1_dedup$title) - length(naive_dedup$title)) # 11,851 more documents identified. 
(length(it1_dedup_out$title) - length(it1_dedup$title)) # 6,133 documents from the naive search not in the 
# first iteration search. 
# So total coverage increase is: 11,851 + 6,133 = 17,984 documents or 17,984 / 13,309 = 1,35 as a factor 
# increase. 
(1 - (length(it1_dedup_out$title) - length(it1_dedup$title)) / length(naive_dedup$title)) * 100 # 53.90% of 
# the naive search was in the first iteration search. 

### It is subsequently the question whether we should continue with a second iteration search or not. 53.90% 
### of the naive search was contained in the first iteration search. An increase of 11,851 documents however 
### only added 3 documents in the accuracy set. Another issue is that we have retrieved a total of 31,289 
### documents, meaning that any subsequent iteration will be laborious, both in a computational and time 
### sense. Put differently, given the strategy of choice, and the associated computational and time costs, 
### do we expect to retrieve many more relevant documents? I make the judgment that we will probably not 
### obtain many more relevant documents given what is required to obtained it. As such, I terminate the 
### search after this first iteration.

###########################
##### ASReview export #####
###########################

### Export the final document set as input for screening in ASReview.
## Data frame of the title, abstract, author, and year of publication. 
asreview_socialization <- as.data.frame(cbind(it1_dedup_out$title, it1_dedup_out$abstract, 
                                              it1_dedup_out$author, it1_dedup_out$year))
## Assign column names.
colnames(asreview_socialization) <- c("Title", "Abstract", "Author", "Year")

## Adding those gold standard articles that were published before 2010. These need to be added so that they 
## can be used as prior information in ASReview. Start with data frame of the title, abstract, author, and 
## year of publication. 
gs_to_asreview <- as.data.frame(cbind(gs_socialization$title, gs_socialization$abstract, 
                                      gs_socialization$author, gs_socialization$year))
## Select articles published before 2010.
gs_to_asreview <- gs_to_asreview[c(4), ]
## Assign column names.
colnames(gs_to_asreview) <- c("Title", "Abstract", "Author", "Year")

## Row bind the two article document sets.
asreview_socialization <- rbind(asreview_socialization, gs_to_asreview)
## Final number of articles.
length(asreview_socialization$Title) # 31,290 candidate articles for the socialization determinant. 
## Write result as a .csv file.
write.csv(asreview_socialization, paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/4. Sociali", 
                                         "zation/4. ASReview/ASReview_input_socialization.csv"))

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
### meta-analysis of group contact on inter-ethnic attitudes. 
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
(k <- (v_star * (lambda ** 2)) / (es ** 2)) # ~40 studies at a minimum for group contact paradigm. Thus: 
# simple random sample over the literature retrieved in ASReview should have a size 40 at least. 

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

