#############################################
##### Literature Search - Group Contact #####
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
### the group contact literature. More specifically, we limit the scope of the search to the direct and 
### extended group contact literature. The naive keywords are extracted from two sources, the review 
### question, and a set of gold-standard articles. The review question is formulated as: are group threat, 
## group contact, media, socialization, and the demographics age, gender, and education determinants of 
### inter-ethnic attitudes in the 2010-2022 period? Note that we identify four research paradigms in this 
### research question: group threat, group contact, media, and socialization. Effects of the various 
### demographic variables are assumed to be present in articles on the four main paradigms. In this specific 
### R-file we limit the scope to the group contact determinant. We split the constituent elements in the 
### research question as being either a determinant or an outcome of interest, a distinction that we will 
### follow throughout the rest of this document:
naive_keywords <- c("group contact",  "inter-ethnic attitudes")

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
gs_group_contact <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/1. Valida", 
                     "tion sets/1. Gold standard"), 
  verbose = TRUE)
## Request the length of the title object to show that there are 6 gold standard articles. Also request the
## object itself to show the titles of the articles themselves. 
length(gs_group_contact$title)
gs_group_contact$title

### The keywords embedded in these gold standard articles are the keywords as they have been listed by the 
### authors themselves, and those keywords as identified by the Rapid Automatic Keyword Extraction Algorithm 
### (RAKE). The RAKE is a keyword extraction algorithm which tries to determine key phrases in a body of text 
### by analyzing the frequency of word appearance and its co-occurrence with other words in the text. Please 
### refer to Rose, Engel, Cramer, and Cowley (2010) for an overview of the RAKE. 
## Start by obtaining the keywords listed in the articles. We extract all non-single keywords that are listed
## at least once. Single word keywords were inspected for viability but excluded. 
sum(!is.na(gs_group_contact$title)) # Note that all of the 6 gold standard articles list keywords. 
gs_tagged_keywords <- litsearchr::extract_terms(
  keywords = gs_group_contact$keywords, # This is a list with the keywords from the gold standard articles. 
  method = "tagged", # Indicate that we want to obtain the keywords from the provided list.
  min_freq = 1, # The keyword has to occur a minimum of one time to be included.  
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 1, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
gs_tagged_keywords # Resulting keywords. 
## I subsequently make a selection based on the degree to which each of the keywords is relevant to the 
## effect of contact on inter ethnic attitudes. Note that I interpret this broadly. I exclude "behavior", 
# "children", "ethnic  diversity", "europe", "homosexuals", "identity", "immigration", "in-group", 
# "judgments", "mediating role", "minority", "negative  contact", "netherlands", "out-group", "personality", 
# "predictors", "preferences", "reconciliation", "reduction", "school", "sentiments", "threat", "trust", and 
# "violence". I remove  "ethnic  diversity" and "negative  contact" because these same terms are listed 
# without redundant spacing. I exclude terms like "identity" and "immigration" because although these are 
# arguably somewhat relevant terms, they are much too broad for a contact theoretical specific search.
gs_tagged_keywords <- gs_tagged_keywords[c(1, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 16, 17, 24, 25, 26, 
                                           28, 32, 33, 36, 37, 39, 42, 43, 44, 47, 48, 50, 52, 53, 54, 
                                           56)]
## Remove redundant spacing and characters.
gs_tagged_keywords[7] <- "cultural embeddedness"
gs_tagged_keywords[8] <- "ethnic group"
gs_tagged_keywords[20] <- "outgroup size"
gs_tagged_keywords[21] <- "perceived diversity"
gs_tagged_keywords[24] <- "public attitudes"
gs_tagged_keywords[25] <- "racial attitudes"
gs_tagged_keywords[30] <- "social cohesion"
gs_tagged_keywords

## Use the RAKE to obtain keywords from the titles and abstracts of the gold standard articles. We extract 
## keywords that occur at least once in the titles and abstracts. 
gs_raked_keywords <- litsearchr::extract_terms(
  text = paste(gs_group_contact$title, gs_group_contact$abstract), # This is a list of the gold standard 
  # articles' titles and abstracts.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 1, # The keyword has to occur a minimum of two times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 1, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
gs_raked_keywords
## I subsequently make a selection based on the degree to which each of the keywords is relevant to the 
## direct and extended contact on inter-ethnic attitudes relationship. Note that I interpret this broadly. 
gs_raked_keywords <- gs_raked_keywords[c(13, 24, 40, 49, 66, 70, 81, 85, 93, 95, 112, 124, 126, 145, 147, 
                                         148, 150, 182, 184, 217, 238, 248, 249, 251, 277, 287, 289, 290, 
                                         292, 316, 317, 328, 351, 355, 358, 384, 385, 404, 407, 409, 413, 
                                         415, 417, 423, 424, 453, 494, 498, 568, 570)]
gs_raked_keywords # The resulting keywords.
## Remove redundant spacing and characters.
gs_raked_keywords[6] <- "contact valence"
gs_raked_keywords[45] <- "prejudice reduction"

## Combine the tagged and raked keywords from the gold standard articles.
gs_all_keywords <- c()
gs_all_keywords <- append(gs_all_keywords, c(gs_tagged_keywords, gs_raked_keywords))
gs_all_keywords <- sort(gs_all_keywords)
gs_all_keywords <- remove_redundancies(gs_all_keywords, closure = "full") # Remove duplicate search terms.

## Filter "gs_all_keywords" object. I do this on the basis of the keyword being either a dependent or 
## independent variable of interest, or being a prominent keywords in the title or abstract.

## Important title keywords. 
gs_group_contact$title[1] # "diverse societies", "cohesion", "contact theory", "mediated contact theory", are 
# prominent keywords. 
gs_group_contact$title[2] # "intergroup contact", "prejudice toward immigrants", "individual conservative 
# values", "cultural embeddedness", are prominent keywords. 
gs_group_contact$title[3] # "countervailing contact", "ethnic diversity", "anti immigrant attitudes", 
# "positive inter-ethnic contact, "negative inter-ethnic contact", are prominent keywords. 
gs_group_contact$title[4] # "xenophobia", "inter-ethnic contact", are prominent keywords. 
gs_group_contact$title[5] # "social contact", "prejudice", "discrimination", are prominent keywords. 
gs_group_contact$title[6] # "extended contact", "direct contact", "group norms", "positive ethnic intergroup 
# attitudes", are prominent keywords. 

## Important abstract keywords.
gs_group_contact$abstract[1] # "ethnic diversity", "social cohesion", "positive intergroup contact", are 
# prominent keywords. 
gs_group_contact$abstract[2] # "individual conservative values", "cultural embeddedness", "contact with 
# immigrants", "attitudes toward immigrants", "ethnic prejudice', "prejudice", are prominent keywords. 
gs_group_contact$abstract[3] # "inter-ethnic contact", "contact-valence", "attitudes towards immigrants", 
# "inter-group contact, "positive inter-group contact", "negative inter-group contact", "diverse 
# communities", are prominent keywords. 
gs_group_contact$abstract[4] # "xenophobic attitudes", "xenophobia", "inter-ethnic contact", "positive 
# inter-ethnic contact", "negative inter-ethnic contact", are prominent keywords. 
gs_group_contact$abstract[5] # "positive social contact", "social contact", "positive integroup social 
# contact", "prejudice", "discrimination", are prominent keywords. 
gs_group_contact$abstract[6] # "direct contact", "extended contact", "in-group norms", "out-group norms", 
# "cross-ethnic friendships", are prominent keywords. 

## Making a selection on whether a keyword is either a dependent or independent variable of interest, or is 
## prominent in the title or abstract of the gold standard articles.
gs_grouped_terms <- list(
  determinant = gs_all_keywords[c(8, 9, 11, 15, 25, 26, 33, 35, 37, 39, 40, 42, 43, 44, 50, 52, 53)],
  outcome = c(gs_all_keywords[c(18, 21, 23, 28, 34, 51, 56, 57, 58, 59, 70)]))
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
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive search") # Set 
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
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive search") # Set 
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
### This search was conducted on 18-07-2022. The documents are subsequently exported with "Format:" set to 
### "RIS" and "Fields:" set to "Complete Reference". If the total number of documents exceed 1500, this 
### exporting process is executed in batches of size 1500.

### In Proquest, we click on "Advanced Search", enter the Boolean search in the first line, set the value of 
### the "in" bar to "Anywhere except full text - NOFT", check the "Peer reviewed" box, select 
### "After this date..." in the "Publication date:" drop-down menu, and set the respective boxes to 
### "January", "1" and "2010", respectively. Under "Source type:" we select "Scholarly Journals" and under 
### "Language" we tick the "English" box. We subsequently click on "Search". On the next page, under 
### "Document type" we subsequently select "Article", where under "Language" we select "English". We set 
### "Items per page" to 100 and select the "EndNote" category under "All save & export options" for 
### exporting a RIS file for each page. This search was conducted on 18-07-2022.

### On the Scopus start page, we set the "Search within" tab to the default "Article title, Abstract, 
### Keywords" tab. We do not set it to the "All fields" tab, because doing so 1) retrieves a large number of 
### irrelevant documents, and 2) makes it difficult to export documents from Scopus. We subsequently enter 
### the Boolean search in the "Search documents" tab. We click on the "Add date range" button and set the 
### "Published from" tab to "2010", leaving the "To" tab to "Present", and the "Added to Scopus" tab to 
### "Anytime". We click "Search" and after having searched, scroll down to "Document type" and "Language" 
### under "Refine results", check the "Article" and "English" boxes, respectively, and click "Limit to". 
### This search was conducted on 18-07-2022. We subsequently select "All" and click on "Export" and select 
### "RIS Format". Note that I only export "Citation information", "Bibliographical information" and 
### "Abstracts & keywords" in Scopus because exporting the additional two categories "Funding details" and 
### "Other information" leads to merging issues later on. Note that we stratify the returned document set 
### by years when this number exceeds 2000. 

### In Web of Science, we click on "Advanced search", enter the Boolean search in the "Query Preview" search 
### box, and click "Search". We once again scroll down to "Document Types" and "Languages" under "Refine 
### results" and check the "Articles" and "English" and "Unspecified" boxes, respectively, if applicable. 
### This search was conducted on 18-07-2022. We subsequently click on "Select all records", "Export" and 
### then "RIS (other reference software):". We subsequently click on "Records from:" and export either the 
### total returned document set if this set is less than 1000, or export them in a per 1000 batch-wise 
### fashion if this number exceeds 1000, with "Record Content:" set to "Full Record". 

### The naive search resulted in 633 documents from Ovid (Selection: PsycINFO), 403 documents from ProQuest 
### (Selection: Sociological Abstracts), 844 documents from Scopus (Selection: Full index), and 1,852 
### documents from Web of Science (Selection: Web of Science Core Collection) for a total of 3,732 documents. 
### Please note that this document set is unfiltered, i.e., duplicates, retracted documents, unidentified 
### non-English documents, etc., have not yet been removed. The documents were manually extracted in a single 
### batch in Ovid, Scopus, and Web of Science, and in 100 sized batches in ProQuest.

### Data import and cleaning.
## Import results of initial, manual naive search. Note that the batches for each search system were merged
## into a single .ris file in EndNote before being imported. 
naive_import_ovid <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive ", 
                     "search/1. Unmerged/Ovid"),
  verbose = TRUE)
naive_import_proquest <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive ", 
                     "search/1. Unmerged/ProQuest"),
  verbose = TRUE)
naive_import_scopus <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive ", 
                     "search/1. Unmerged/Scopus"),
  verbose = TRUE)
naive_import_wos <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive ", 
                     "search/1. Unmerged/Web of Science"),
  verbose = TRUE)
## Checking whether the length of the imported .ris files are equal to the lengths of the raw .ris files.
length(naive_import_ovid$title) # 633, which is correct.  
length(naive_import_proquest$title) # 403, which is correct.  
length(naive_import_scopus$title) # 844, which is correct.  
length(naive_import_wos$title) # 1,852, which is correct.  

### We subsequently identify and remove identifiable, non-English documents.
## Ovid.
table(naive_import_ovid$language) # All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive search/2. ",
             "Merged/Ovid/1. Raw"))
write_refs(naive_import_ovid, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
## ProQuest.
table(naive_import_proquest$language) # ProQuest. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive search/2. ",
             "Merged/ProQuest/1. Raw"))
write_refs(naive_import_proquest, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
## Scopus.
table(naive_import_scopus$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive search/2. ",
             "Merged/Scopus/1. Raw"))
write_refs(naive_import_scopus, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
## Web of Science.
table(naive_import_wos$language) # Web of Science. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive search/2. ",
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
length(naive_import_ovid$title) # Input is 633 documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  naive_import_ovid$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_ovid$title, exact_duplicates_ovid)) # Perform a 
# manual check. No duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # Zero documents should be removed.
naive_dedup_ovid <- synthesisr::extract_unique_references(naive_import_ovid, exact_duplicates_ovid) 
length(naive_dedup_ovid$title) # Output after exact matching is 633. Zero documents removed.
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
length(naive_dedup_ovid$title) # De-duplicated output is 633. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive search/2. M",
             "erged/Ovid/2. Deduplicated"))
write_refs(naive_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_ovid")

## ProQuest.
## Exact matching.
length(naive_import_proquest$title) # Input is 403 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  naive_import_proquest$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_proquest$title, exact_duplicates_proquest)) # 
# Perform a manual check. All candidate combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 25 documents should be removed.
naive_dedup_proquest <- extract_unique_references(naive_import_proquest, exact_duplicates_proquest) 
length(naive_dedup_proquest$title) # Output after exact matching is 378. 25 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  naive_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. All candidate combinations are duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # Five documents should be removed.
naive_dedup_proquest <- extract_unique_references(naive_dedup_proquest, fuzzy_duplicates_proquest) # Extract
# unique references. 
length(naive_dedup_proquest$title) # De-duplicated output is 373. Five documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  naive_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. All candidate combinations are duplicates. 
sum(table(fuzzy_duplicates_proquest) - 1) # One document should be removed.
naive_dedup_proquest <- extract_unique_references(naive_dedup_proquest, fuzzy_duplicates_proquest) # 
# Extract unique references. 
length(naive_dedup_proquest$title) # De-duplicated output is 372. one document removed.
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive search/2. M",
             "erged/ProQuest/2. Deduplicated"))
write_refs(naive_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_proquest")

## Scopus.
## Exact matching.
length(naive_import_scopus$title) # Input is 844 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  naive_import_scopus$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_scopus$title, exact_duplicates_scopus)) # Perform a 
# manual check. All candidate combinations are duplicates.
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # One document should be removed.
naive_dedup_scopus <- synthesisr::extract_unique_references(naive_import_scopus, exact_duplicates_scopus) 
length(naive_dedup_scopus$title) # Output after exact matching is 843. One document removed.
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
length(naive_dedup_scopus$title) # De-duplicated output is 843. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive search/2. M",
             "erged/Scopus/2. Deduplicated"))
write_refs(naive_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_scopus")

## Web of Science.
## Exact matching.
length(naive_import_wos$title) # Input is 1,852 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  naive_import_wos$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(naive_import_wos$title, exact_duplicates_wos)) # Perform a 
# manual check. No duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Zero documents should be removed.
naive_dedup_wos <- synthesisr::extract_unique_references(naive_import_wos, exact_duplicates_wos) 
length(naive_dedup_wos$title) # Output after exact matching is 1,852. Zero documents removed.
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
length(naive_dedup_wos$title) # De-duplicated output is 1,852. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive search/2. M",
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
cbind(gs_group_contact$title, gs_group_contact$year) 
## Ovid.
sum(round(as.numeric(check_recall(gs_group_contact$title, naive_dedup_ovid$title)[, 3]), 0)) # 2 of 6 gold
# standard articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_group_contact$title, naive_dedup_proquest$title)[, 3]), 0)) # 3 of 6
# gold standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_group_contact$title, naive_dedup_scopus$title)[, 3]), 0)) # 3 of 6 gold
# standard articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_group_contact$title, naive_dedup_wos$title)[, 3]), 0)) # 6 of 6 gold 
# standard articles are retrieved.
## The gold standard precision check tentatively indicates that Web of Science has superior precision, 
## followed by Scopus and ProQuest, and finally Ovid. Also note that all gold standard articles are retrieved
## over the sum of the documents sets. We as such continue with the rest of the search strategy.

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and those
## =< 0.5 are not.
ex_group_contact <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/1. Validat", 
                     "ion sets/2. External"), 
  verbose = TRUE)
cbind(ex_group_contact$title, ex_group_contact$year) # Data frame of title and year of publication of 98 
# external articles.
## Ovid.
sum(round(as.numeric(check_recall(ex_group_contact$title, naive_dedup_ovid$title)[, 3]), 0)) # 30 of 98 
# external articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_group_contact$title, naive_dedup_proquest$title)[, 3]), 0)) # 19 of 98 
# external articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_group_contact$title, naive_dedup_scopus$title)[, 3]), 0)) # 35 of 98 
# external articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_group_contact$title, naive_dedup_wos$title)[, 3]), 0)) # 56 of 98
# external articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision,
## followed by Scopus, Ovid, and Proquest, respectively. 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
## Set working directory. 
naive_dedup <- bind_rows(naive_dedup_ovid, naive_dedup_proquest, naive_dedup_scopus, naive_dedup_wos) # Bind 
# the corpora into a single corpus. Should be equal to 633 + 372 + 843 + 1,852 = 3,700.
## Exact matching.
length(naive_dedup$title) # Input is 3,700 documents.
exact_duplicates <- synthesisr::find_duplicates(
  naive_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(naive_dedup$title, exact_duplicates) # Perform a manual check. 
length(exact_manual$title) # Sum of 2,338 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 1,532 articles should be removed.
naive_dedup <- extract_unique_references(naive_dedup, exact_duplicates) 
length(naive_dedup$title) # Output after exact matching is 2,168.  documents removed.
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
length(naive_dedup$title) # De-duplicated output is 2,076. 38 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates( 
  naive_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(naive_dedup$title, fuzzy_duplicates)) # Perform a manual 
# check. All but one combination are duplicates.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(462)) #
sum(table(fuzzy_duplicates) - 1) # 22 documents should be removed.
naive_dedup <- extract_unique_references(naive_dedup, fuzzy_duplicates) # Extract unique references. 
length(naive_dedup$title) # De-duplicated output is 2,054. Nine documents removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/2. Naive search/2. Merged")
write_refs(naive_dedup, format = "ris", tag_naming = "synthesisr", file = "naive_dedup_refman")
# EndNote and Zotero both indicate that seven duplicates remain in the .ris file. We furthermore remove three 
# retracted papers: "Interprofessional learning in acute care: Developing a theoretical framework", 
# "Bridging the Gap on Facebook: Assessing Intergroup Contact and Its Effects for Intergroup Relations", and
# "When contact changes minds: An experiment on transmission of support for gay equality". The second paper 
# has two documents in the set, one on the paper itself, and one on the retraction. The other two have just 
# one paper. The result is exported in the file "naive_dedup.ris" which is subsequently imported.  
naive_dedup <- read_bibliography("naive_dedup.ris")
length(naive_dedup$title) # 2,043 documents. 11 documents removed.

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_group_contact$title, naive_dedup$title) # 6 of 6 gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_group_contact$title, naive_dedup$title)[, 3]), 0)) # 57 of 98 external 
# articles are retrieved.

#########################################
##### Iteration 1 Literature Search #####
#########################################

### Now repeat the previous procedure, but with keywords extracted from the naive search corpus. This follows
### the same logic as earlier, due to the assumed high degree of heterogeneity of the literature, we combine 
### keyword information from the retrieved document set with author judgment to maximize coverage.

### Clear the environment except for the gold standard articles and "naive_dedup" objects. 
rm(list=setdiff(ls(), c("gs_grouped_terms", "gs_group_contact", "ex_group_contact", "naive_dedup")))

### Start by checking the naive search corpus for provided keywords, and keywords in titles and abstracts.
## Keywords.l
length(naive_dedup$keywords) # Total number of documents is 2,043. 
length(naive_dedup$keywords) - sum(is.na(naive_dedup$keywords)) # 2,041 of the articles list keywords.  
## Titles and abstracts.
length(naive_dedup$title) - sum(is.na(naive_dedup$title)) # All of the articles list a title.  
length(naive_dedup$abstract) - sum(is.na(naive_dedup$abstract)) # 1,982 of the articles list an abstract.  

### The first step is to obtain the keywords listed in the articles. We extract all non-single keywords that 
### are listed a minimum of twenty times. Note that the twenty number is arbitrary insofar that we need to 
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
  min_freq = 20, # The keyword has to occur a minimum of twenty times to be included.   
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(tagged_keywords) # 101 tagged keywords.
### Now use the RAKE to obtain keywords from the titles and abstracts of naive search corpus. We extract all 
### non-single keywords that occur at least twenty times in these titles and abstracts. 
raked_keywords <- litsearchr::extract_terms(
  text = paste(naive_dedup$title, naive_dedup$abstract), # This is a list of titles and abstracts 
  # per gold standard article.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 20, # The keyword has to occur a minimum of twenty times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(raked_keywords) # 235 raked keywords.
## Sum total of tagged and raked keywords. 
keyword_candidates <- remove_redundancies(c(raked_keywords, tagged_keywords), closure = "full") # Remove 
# duplicates.
length(keyword_candidates) # Total of 286 keyword candidates. 
## The subsequent task is to select all keywords that are relevant to our query from the candidate pool. I 
## select terms that relate in content to the relevant literature, i.e., are a independent or dependent 
## variable of interest in the group contact on inter-ethnic attitudes relationship. Note that I interpret 
## relevance quite broadly, since these terms will be will be ranked and filtered further based on their 
## "connectedness" in a co-occurrence network. In that sense, it is better too be too liberal than too 
## selective in our choices here, since it might be hard to anticipate which relevant terms are important 
## from this "connectedness" perspective. I furthermore do not include keywords which relate directly to any 
## of the other paradigms, e.g., I do not include keywords on group threat theory even though these might 
## appear often in the group contact literature. Finally note that I do this part manually, which is prone to 
## errors. 
all_keywords <- keyword_candidates[
  c(6, 18, 19, 20, 21, 22, 24, 26, 28, 32, 33, 35, 38, 41, 44, 45, 46, 47, 48, 49, 50, 55, 70, 72, 73, 74, 
    76, 77, 79, 80, 82, 83, 89, 90, 91, 93, 95, 96, 97, 98, 99, 100, 101, 102, 103, 105, 106, 107, 110, 111, 
    112, 115, 116, 117, 118, 120, 121, 123, 125, 127, 131, 132, 133, 134, 135, 136, 137, 140, 142, 144, 145, 
    149, 150, 151, 153, 157, 158, 168, 169, 170, 171, 172, 178, 180, 181, 182, 190, 193, 199, 200, 202, 204, 
    231, 237, 238, 239, 243, 244, 246, 247, 248, 255, 256, 257, 263, 267, 271, 272, 274, 276, 278, 279, 283, 
    285)
]
## Manual cleaning.
(all_keywords <- sort(all_keywords))
all_keywords[1] <- "group interactions" # Change "0410 group interactions" to "group interactions".
(all_keywords <- sort(all_keywords))
length(all_keywords) # 114 keyword candidates.

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
length(all_keywords_final) # 126 candidates.
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
## I now apply a filter to select 60 and 42 terms for the search string in ovid, scopus, and web of 
## science, and ProQuest, respectively, where I remove terms which refer to an equivalent or near equivalent
## concept, i.e., "target group" and "target groups", where I select the term with the highest strength 
## value. Note that the 60 and 42 values were the result of trial-and-error in the respective search engines.
(term_strengths <- cbind(seq(length(term_strengths$term)), term_strengths[order(term_strengths$term), ]))
term_strengths <- term_strengths[-c(2, 7, 9, 13, 20, 22, 32, 36, 37, 47, 52, 55, 62, 63, 66, 67, 73, 78, 89, 
                                    92, 99, 100, 101), ]

### Construct Boolean search OVID, Web of Science, and Scopus.
## Select first 60 terms from filtered term set. 
keywords_ovid_scopus_wos <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:60, ]$term)
(keywords_ovid_scopus_wos <- keywords_ovid_scopus_wos[order(keywords_ovid_scopus_wos)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1")
sink("first_iteration_selected_terms_ovid_scopus_wos.txt")
print(keywords_ovid_scopus_wos)
sink() 
## Categorize "keywords_ovid_scopus_wos" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_ovid_scopus_wos <- list(
  determinant = keywords_ovid_scopus_wos[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
                                           21, 24, 25, 26, 27, 29, 30, 31, 34, 35, 36, 37, 39, 40, 41, 43, 
                                           44, 45, 47, 49, 50, 54, 55, 56, 57, 58, 59, 60)],
  outcome = keywords_ovid_scopus_wos[c(11, 22, 23, 28, 32, 33, 38, 42, 46, 48, 51, 52, 53)])
grouped_terms_ovid_scopus_wos
### Given the grouped terms, write the Boolean search for OVID, Web of Science, and Scopus to the 
### directory and print it to the console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1") # Set directory.
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
## Select first 47 terms from filtered term set.
keywords_proquest <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:48, ]$term)
(keywords_proquest <- keywords_proquest[order(keywords_proquest)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1")
sink("first_iteration_selected_terms_proquest.txt")
print(keywords_proquest)
sink() 
## Categorize "keywords_proquest" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_proquest <- list(
  determinant = keywords_proquest[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 22, 
                                    23, 24, 26, 27, 28, 39, 31, 32, 34, 35, 37, 39, 43, 44, 45, 46)],
  outcome = keywords_proquest[c(18, 21, 25, 30, 33, 36, 38, 40, 41, 42)])
grouped_terms_proquest$outcome <- grouped_terms_proquest$outcome[-c(3)] # Remove "intergroup relations" 
# because it severely blows up the search.
grouped_terms_proquest
### Given the grouped terms, write the the Boolean search for ProQuest to the directory and print it to the 
### console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1") # Set directory.
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

### All searches were conducted on 20-07-2022. These resulted in 2,884 documents from Ovid (Selection: 
### PsycINFO), 956 from ProQuest (Selection: Sociological Abstracts), 3,092 documents from Scopus 
### (Selection: Full index), and 3,966 documents from Web of Science (Selection: Web of Science Core 
### Collection), for a total of 11,029 documents. Please note that this document set is unfiltered, i.e., 
### duplicates, retracted documents, unidentified non-English documents, etc., have not yet been removed. In
### Ovid, the documents were manually extracted in three batches. In ProQuest the documents were manually 
### extracted in 100-sized batches. In Scopus, the documents were manually extracted in 2000 document sized 
### batches, stratified by year. In Web of Science, the documents were manually extracted in 1000 document 
### sized batches. 

### Data import and cleaning.
## Import results of informed search. Note that the batches for each search system were merged in EndNote.
## Start by checking whether the imported length is equal to the length of the raw .ris files.
import_ovid_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iterat", 
                     "ion 1/1. Unmerged/Ovid"),
  verbose = TRUE)
import_proquest_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iterat", 
                     "ion 1/1. Unmerged/ProQuest"),
  verbose = TRUE)
import_scopus_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iterat", 
                     "ion 1/1. Unmerged/Scopus"), 
  verbose = TRUE)
import_wos_it1 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iterat", 
                     "ion 1/1. Unmerged/Web of Science"),
  verbose = TRUE)
length(import_ovid_it1$title) # 2,884, which is correct.  
length(import_proquest_it1$title) # 956, which is correct.  
length(import_scopus_it1$title) # 3,092, which is correct.
length(import_wos_it1$title) # 3,966, which is correct.  

### We subsequently identify and remove identifiable, non-English documents. 
## Ovid.
table(import_ovid_it1$language) # Ovid. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1/2. Me",
             "rged/Ovid/1. Raw"))
write_refs(import_ovid_it1, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
## ProQuest.
table(import_proquest_it1$language) # ProQuest. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1/2. Me", 
             "rged/ProQuest/1. Raw"))
write_refs(import_proquest_it1, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
## Scopus.
table(import_scopus_it1$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1/2. Me", 
             "rged/Scopus/1. Raw"))
write_refs(import_scopus_it1, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
## Web of Science.
table(import_wos_it1$language) # Web of Science. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1/2. Me", 
             "rged/Web of Science/1. Raw"))
write_refs(import_wos_it1, format = "ris", tag_naming = "synthesisr", file = "web_of_science_r")

### De-duplication. Please refer to the naive iteration for an overview of the exact steps of the 
### de-duplication procedure.

### Ovid.
## Exact matching.
length(import_ovid_it1$title) # Input is 2,884 documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  import_ovid_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_ovid_it1$title, exact_duplicates_ovid)) # Perform a 
# manual check. One duplicate combination identified.
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # One document should be removed.
it1_dedup_ovid <- synthesisr::extract_unique_references(import_ovid_it1, exact_duplicates_ovid) 
length(it1_dedup_ovid$title) # Output after exact matching is 2,883. One document removed.
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
length(it1_dedup_ovid$title) # De-duplicated output is 2,882. One document removed. 
## Fuzzy matching ten.
fuzzy_duplicates_ovid <- find_duplicates( 
  it1_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. No duplicate combinations identified.
length(it1_dedup_ovid$title) # De-duplicated output is 2,882. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1/2. Mer", 
             "ged/Ovid/2. Deduplicated"))
write_refs(it1_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_ovid")

### ProQuest. 
## Exact matching.
length(import_proquest_it1$title) # Input is 956 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  import_proquest_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_proquest_it1$title, exact_duplicates_proquest)) # All
# combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 38 articles should be removed.
it1_dedup_proquest <- extract_unique_references(import_proquest_it1, exact_duplicates_proquest) 
length(it1_dedup_proquest$title) # Output after exact matching is 918. 82 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  it1_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. All combinations are duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # Five documents should be removed.
it1_dedup_proquest <- extract_unique_references(it1_dedup_proquest, fuzzy_duplicates_proquest) # 
# Extract unique references. 
length(it1_dedup_proquest$title) # De-duplicated output is 913. Five documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  it1_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. No duplicate combinations identified.
sum(table(fuzzy_duplicates_proquest) - 1) # Zero documents should be removed.
it1_dedup_proquest <- extract_unique_references(it1_dedup_proquest, fuzzy_duplicates_proquest) # Extract 
# unique references. 
length(it1_dedup_proquest$title) # De-duplicated output is 913. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1/2. Mer", 
             "ged/ProQuest/2. Deduplicated"))
write_refs(it1_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_proquest")

### Scopus.
## Exact matching.
length(import_scopus_it1$title) # Input is 3,092 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  import_scopus_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_scopus_it1$title, exact_duplicates_scopus)) # Perform a 
# manual check. Four duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # Three documents should be removed.
it1_dedup_scopus <- synthesisr::extract_unique_references(import_scopus_it1, exact_duplicates_scopus) 
length(it1_dedup_scopus$title) # Output after exact matching is 3,089 . Seven documents removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  it1_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it1_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. Zero duplicate combinations identified.
sum(table(fuzzy_duplicates_scopus) - 1) # Zero documents should be removed.
it1_dedup_scopus <- extract_unique_references(it1_dedup_scopus, fuzzy_duplicates_scopus) # Extract unique 
# references. 
length(it1_dedup_scopus$title) # De-duplicated output is 3,089. One document removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1/2. Mer", 
             "ged/Scopus/2. Deduplicated"))
write_refs(it1_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_scopus")

### Web of Science.
length(import_wos_it1$title) # Input is 3,966 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  import_wos_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_wos_it1$title, exact_duplicates_wos)) # Perform a 
# manual check. Three duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Three documents should be removed.
it1_dedup_wos <- synthesisr::extract_unique_references(import_wos_it1, exact_duplicates_wos) 
length(it1_dedup_wos$title) # Output after exact matching is 3,963. Three documents removed.
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
length(it1_dedup_wos$title) # De-duplicated output is 3,963. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1/2. Mer", 
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
cbind(gs_group_contact$title, gs_group_contact$year)
## Ovid.
sum(round(as.numeric(check_recall(gs_group_contact$title, it1_dedup_ovid$title)[, 3]), 0)) # 2 of 6 gold 
# standard articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_group_contact$title, it1_dedup_proquest$title)[, 3]), 0)) # 1 of 6 gold 
# standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_group_contact$title, it1_dedup_scopus$title)[, 3]), 0)) # 2 of 6 gold 
# standard articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_group_contact$title, it1_dedup_wos$title)[, 3]), 0)) # 3 of 6 gold 
# standard articles are retrieved.
## The gold standard precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus and Ovid, and finally ProQuest. This precision has furthermore decreased 
## for ProQuest (-2), Scopus (-1), and Web of Science (-3), and stayed constant for Ovid (+0). 

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
cbind(ex_group_contact$title, ex_group_contact$year) # Data frame of title and year of publication of 98 
# external articles.
## Ovid.
sum(round(as.numeric(check_recall(ex_group_contact$title, it1_dedup_ovid$title)[, 3]), 0)) # 32 of 98 
# external articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_group_contact$title, it1_dedup_proquest$title)[, 3]), 0)) # 18 of 98 
# external articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_group_contact$title, it1_dedup_scopus$title)[, 3]), 0)) # 36 of 98 
# external articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_group_contact$title, it1_dedup_wos$title)[, 3]), 0)) # 45 of 98 
# external articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus, Ovid, and ProQuest, respectively. Relative to the naive search, the precision with 
## respect to the external articles has increased slightly in Ovid (+2) and Scopus (+1). It has decreased 
## slightly in ProQuest (-1), and quite significantly in Web of Science (-11). 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
it1_dedup <- bind_rows(it1_dedup_ovid, it1_dedup_proquest, it1_dedup_scopus, it1_dedup_wos) # Merge corpora. 
## Exact matching.
length(it1_dedup$title) # Input is 10,847 documents.
exact_duplicates <- synthesisr::find_duplicates(
  it1_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(it1_dedup$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 7,638 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 4,829 documents should be removed.
it1_dedup <- synthesisr::extract_unique_references(it1_dedup, exact_duplicates) 
length(it1_dedup$title) # Output after exact matching is 6,018. 4,829 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
fuzzy_manual <- review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check.
length(fuzzy_manual$title) # 623 potential duplicate combinations. I check these candidates manually. Note 
# that this procedure is prone to error.
fuzzy_manual$title[1:623] # All combinations are duplicates.
sum(table(fuzzy_duplicates) - 1) # 314 document should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references.
length(it1_dedup$title) # De-duplicated output is 5,704. 314 documents were removed.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 184 duplicate combinations.
fuzzy_manual # 67, 103, 1803, 2297, 2823, are not duplicate combinations. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(67, 103, 1803, 2297, 2823)) 
sum(table(fuzzy_duplicates) - 1) # 87 documents should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references. 
length(it1_dedup$title) # De-duplicated output is 5,617. 87 document removed. 
## Fuzzy matching fifteen.
fuzzy_duplicates <- find_duplicates(
  it1_dedup$title, # Fuzzy matching ten output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 15) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it1_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 149 duplicate combinations.
fuzzy_manual # 8, 66, 67, 102, 228, 253, 451, 453, 601, 656, 722, 741, 823, 921, 924, 948, 1047, 1047, 1055,
# 1171, 1463, 1465, 1540, 1786, 1791, 1868, 1919, 2282, 2304, 2553, 2788, 2803, 2911, 3007, 3760 are not 
# duplicate combinations, remaining combinations are. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c( 8, 66, 67, 102, 228, 253, 451, 453, 601, 656, 722, 
                                                       741, 823, 921, 924, 948, 1047, 1047, 1055, 1171, 1463, 
                                                       1465, 1540, 1786, 1791, 1868, 1919, 2282, 2304, 2553, 
                                                       2788, 2803, 2911, 3007, 3760)) 
sum(table(fuzzy_duplicates) - 1) # Seven documents should be removed.
it1_dedup <- extract_unique_references(it1_dedup, fuzzy_duplicates) # Extract unique references. 
length(it1_dedup$title) # De-duplicated output is 5,610. Seven documents removed. 
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group Contact/3. Iteration 1/2. Merged")
write_refs(it1_dedup, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_refman")
# EndNote and Zotero indicate that 24 duplicates remain in the .ris file, which are removed. We furthermore 
# remove three retracted papers that are flagged in EndNote and Zotero: "Interprofessional learning in acute 
# care: Developing a theoretical framework", "The Evolution of Intergroup Bias: Perceptions and Attitudes in 
# Rhesus Macaques", and "Bridging the Gap on Facebook: Assessing Intergroup Contact and Its Effects for 
# Intergroup Relations". The second paper had three records, because two retraction statements were included,
# where the final paper had four, because it was a duplicate, each of which had a retraction statement. The 
# result is exported from Zotero in the file "it1_dedup.ris" which is subsequently imported.  
it1_dedup <- read_bibliography("it1_dedup.ris")
length(it1_dedup$title) # 5,578 documents. Seven documents removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
sum(round(as.numeric(check_recall(gs_group_contact$title, it1_dedup$title)[, 3]), 0)) # 3 of 6 gold standard 
# articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_group_contact$title, it1_dedup$title)[, 3]), 0)) # 47 of 98 external 
# articles are retrieved.

### Evaluate first iteration search relative to naive search. 

## Here I do not use the "check_recall" function because it takes a long time, and it is not exact / 
## conservative enough, for the purpose of merging the naive search documents that are not in the first 
## iteration search. I do this to try and maximize coverage. 
length(naive_dedup$title) # 2,043 documents.
length(it1_dedup$title) # 5,578 documents. 
overlap_naive_it1 <- bind_rows(naive_dedup, it1_dedup) # Merge corpora. Should be 2,043 + 5,578 = 7,621 
## Exact matching.
length(overlap_naive_it1$title) # Input is 7,621 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_naive_it1$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_naive_it1$title, exact_duplicates) # Perform a manual
# check.
length(exact_manual$title) # Sum of 2,368 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 1,184 documents should be removed.
it1_dedup_out <- synthesisr::extract_unique_references(overlap_naive_it1, exact_duplicates) 
length(it1_dedup_out$title) # Output after exact matching is 6,437. 1,184 documents removed. 859 documents 
# from the naive search not in the first iterations search remain.
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it1_dedup_out$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it1_dedup_out$title, fuzzy_duplicates)) # Perform a manual check. All 
# combinations are duplicates.
sum(table(fuzzy_duplicates) - 1) # 22 documents should be removed.
it1_dedup_out <- extract_unique_references(it1_dedup_out, fuzzy_duplicates) # Extract unique references.
length(it1_dedup_out$title) # De-duplicated output is 6,415. 22 documents were removed. 837 documents remain.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it1_dedup_out$title, # Fuzzy matching five as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # default cutoff. 
(fuzzy_manual <- review_duplicates(it1_dedup_out$title, fuzzy_duplicates)) # Perform a manual check. One 
# duplicate combination identified. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(462, 2093, 2123, 3430, 3802, 4196))
sum(table(fuzzy_duplicates) - 1) # One document should be removed.
it1_dedup_out <- extract_unique_references(it1_dedup_out, fuzzy_duplicates) # Extract unique references.
length(it1_dedup_out$title) # De-duplicated output is 6,414. One document was removed. 836 documents remain.
## Save .ris file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/3. Iteration 1")
write_refs(it1_dedup_out, format = "ris", tag_naming = "synthesisr", file = "it1_dedup_out_refman") # Export
# as .ris. 
write_refs(it1_dedup_out, format = "bib", tag_naming = "synthesisr", file = "it1_dedup_out_refman") # Export
# as .bib for Zotero. 
# EndNote and Zotero indicate that two duplicates remain in the .ris file, which are removed. In addition, 
# one more retraction was identified: "Ethnic threat and social control: Examining public support for 
# judicial use of ethnicity in punishment", consisting of two documents, which was removed. The result is 
# exported from Zotero in the file "it1_dedup_out.ris" which is subsequently imported.  
it1_dedup_out <- read_bibliography("it1_dedup_out.ris")
length(it1_dedup_out$title) # 6,410 documents. Four documents removed. This is the final corpus file of the 
# first iteration search.

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_group_contact$title, it1_dedup_out$title) # 6 of 6 gold gold standard articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_group_contact$title, it1_dedup_out$title)[, 3]), 0)) # 61 of 98 external 
# articles are retrieved.

## Inspect coverage gain relative to the naive search. I define coverage as the sum of the additional 
## documents that were found relative to the previous search iteration, and the documents from the previous 
## search iteration that were not identified in the current one. 
length(naive_dedup$title) # 2,043 articles.
length(it1_dedup$title) # 5,578 articles.
(length(it1_dedup$title) - length(naive_dedup$title)) # 5,578 - 2,043 = 3,535 more documents identified. 
(length(it1_dedup_out$title) - length(it1_dedup$title)) # ~832 documents from the naive search not in the 
# first iteration search. 
# So total coverage increase is: 3,535 + 832 = 4,367 documents or 4,367 / 2,043 = 2.14 as a factor increase. 
(1 - (length(it1_dedup_out$title) - length(it1_dedup$title)) / length(naive_dedup$title)) * 100 # 59.28% of 
# the naive search was in the first iteration search. 

## Since the overlap with the previous iteration is only moderate and the coverage increase differential is 
## large, we continue with a second iteration.

#########################################
##### Iteration 2 Literature Search #####
#########################################

### Now repeat the previous procedure, but with keywords extracted from the first iteration search corpus.  

### Clear the environment except for the gold standard search terms, the validation article sets, and 
### the "naive_dedup", and "it1_dedup_out" objects. 
rm(list = setdiff(ls(), c("gs_grouped_terms", "gs_group_contact", "ex_group_contact", "naive_dedup", 
                          "it1_dedup_out")))

### Start by checking how many documents in the first iteration search corpus provide keywords, titles, and 
### abstracts.
## Keywords.
length(it1_dedup_out$keywords) # Total number of documents is 6,410. 
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
length(tagged_keywords) # 180 tagged keywords.
### Now use the RAKE to obtain keywords from the titles and abstracts of the search corpus. We extract all 
### non-single keywords that occur at least forty times in these titles and abstracts. 
raked_keywords <- litsearchr::extract_terms(
  text = paste(it1_dedup_out$title, it1_dedup_out$abstract), # This is a list of titles and abstracts.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 40, # The keyword has to occur a minimum of forty times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(raked_keywords) # 317 raked keywords.
## Sum total of tagged and raked keywords is 180 + 317 = 497. 
keyword_candidates <- remove_redundancies(c(tagged_keywords, raked_keywords), closure = "full") # Remove
# duplicate terms.
length(keyword_candidates) # Total of 421 keyword candidates. 
## The subsequent task is to select all keywords that are relevant to our query from the candidate pool. I 
## select terms that relate in content to the relevant literature, i.e., are a independent or dependent 
## variable of interest in the group contact on inter-ethnic attitudes relationship. Note that I interpret 
## relevance quite broadly, since these terms will be will be ranked and filtered further based on their 
## "connectedness" in a co-occurrence network. In that sense, it is better too be too liberal than too 
## selective in our choices here, since it might be hard to anticipate which relevant terms are important 
## from this "connectedness" perspective. I furthermore do not include keywords which relate directly to any 
## of the other paradigms, e.g., I do not include keywords on group threat theory even though these might 
## appear often in the group contact literature. Finally note that I do this part manually, which is prone to 
## errors. 
all_keywords <- keyword_candidates[
  c(2, 3, 13, 15, 17, 18, 22, 27, 29, 32, 35, 36, 37, 38, 39, 41, 44, 45, 46, 47, 48, 49, 50, 62, 64, 65, 69,
    70, 72, 73, 74, 75, 78, 79, 83, 94, 95, 96, 101, 102, 105, 109, 111, 116, 118, 120, 121, 122, 124, 126,
    127, 134, 135, 137, 142, 144, 147, 149, 150, 151, 152, 153, 154, 161, 164, 167, 168, 173, 188, 201, 202, 
    215, 216, 218, 222, 223, 224, 225, 226, 227, 228, 235, 250, 251, 256, 259, 260, 264, 266, 267, 269, 270,
    271, 275, 276, 277, 278, 281, 282, 283, 284, 285, 288, 289, 290, 292, 293, 297, 308, 309, 310, 311, 318,
    320, 323, 324, 325, 329, 330, 331, 336, 351, 353, 354, 359, 360, 361, 382, 387, 388) 
]
## Manual cleaning. 
all_keywords <- all_keywords[-c(1)] # Remove "0410 group interactions".
all_keywords[1] <- "group interactions" # Re-format.
(all_keywords <- sort(all_keywords))
length(all_keywords) # 129 keyword candidates. 

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
length(all_keywords_final) # 141 candidates.
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
term_strengths <- term_strengths[-c(2, 3, 13, 17, 18, 22, 24, 27, 29, 32, 36, 39, 43, 48, 49, 54, 61, 65, 68, 
                                    73, 74, 76, 78, 79, 82, 84, 85, 88, 93, 96, 99, 102, 110, 111, 112, 122, 
                                    123, 125), ]

### Construct Boolean search OVID, Web of Science, and Scopus.
## Select first 60 terms from filtered term set. 
keywords_ovid_scopus_wos <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:60, ]$term)
(keywords_ovid_scopus_wos <- keywords_ovid_scopus_wos[order(keywords_ovid_scopus_wos)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2")
sink("second_iteration_selected_terms_ovid_scopus_wos.txt")
print(keywords_ovid_scopus_wos)
sink() 
## Categorize "keywords_ovid_scopus_wos" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_ovid_scopus_wos <- list(
  determinant = keywords_ovid_scopus_wos[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
                                           20, 21, 22, 23, 25, 26, 28, 29, 30, 31, 34, 35, 36, 38, 40, 41, 
                                           43, 51, 54, 55, 57, 58, 59, 60)],
  outcome = keywords_ovid_scopus_wos[c(24, 28, 32, 33, 37, 39, 42, 44, 45, 46, 47, 48, 49, 50, 52, 53, 56)])
grouped_terms_ovid_scopus_wos
### Given the grouped terms, write the Boolean search for OVID, Web of Science, and Scopus to the directory
### and print it to the console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2") # Set directory.
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
## Select first 36 terms from filtered term set.
keywords_proquest <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:36, ]$term)
(keywords_proquest <- keywords_proquest[order(keywords_proquest)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2")
sink("second_iteration_selected_terms_proquest.txt")
print(keywords_proquest)
sink() 
## Categorize "keywords_proquest" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_proquest <- list(
  determinant = keywords_proquest[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 20, 21, 22, 
                                    24, 26, 28, 32, 34, 35, 36)],
  outcome = keywords_proquest[c(15, 19, 23, 25, 27, 29, 30, 31, 33)])
grouped_terms_proquest$outcome <- grouped_terms_proquest$outcome[c(-2)] # Remove "intergroup relations" 
# because it blows ups the search in ProQuest.
### Given the grouped terms, write the the Boolean search for ProQuest to the directory and print it to the 
### console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2") # Set directory.
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

### All searches were conducted on 27-07-2022, except for the search in Ovid, which was re-done on 01-08-2022
### due to the presence of later identified NA values. This resulted in 5,645 documents from Ovid (Selection: 
### PsycINFO), 2,635 documents from Proquest (Selection: Sociological Abstracts), 8,499 documents from Scopus 
### (Selection: Full index), and 9,206 documents from Web of Science (Selection: Web of Science Core 
### Collection), for a total of 25,985 documents. Please note that this document set is unfiltered, i.e., 
### duplicates, retracted documents, unidentified non-English documents, etc., have not yet been removed. In 
### Ovid, the documents were manually extracted in three batches. In ProQuest the documents were manually 
### extracted in 100-sized batches. In Scopus, the documents were manually extracted in 2000 document sized 
### batches, stratified by publication year. In Web of Science, the documents were manually extracted in 1000 
### document sized batches. 

### Data import and cleaning.
## Import results of informed search. Note that the batches for each search system were merged in EndNote.
## Start by checking whether the imported length is equal to the length of the raw .ris files.
import_ovid_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iterat", 
                     "ion 2/1. Unmerged/Ovid"),
  verbose = TRUE)
import_proquest_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iterat", 
                     "ion 2/1. Unmerged/ProQuest"),
  verbose = TRUE)
import_scopus_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iterat", 
                     "ion 2/1. Unmerged/Scopus"),
  verbose = TRUE)
import_wos_it2 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iterat", 
                     "ion 2/1. Unmerged/Web of Science"),
  verbose = TRUE)
length(import_ovid_it2$title) # 5,645, which is correct.  
length(import_proquest_it2$title) # 2,635, which is correct. 
length(import_scopus_it2$title) # 8,499, which is correct.  
length(import_wos_it2$title) # 9,206, which is correct.  

## We subsequently identify and remove identifiable, non-English documents, if necessary. We then save the 
## resulting file.
# Ovid.
table(import_ovid_it2$language) # Ovid. All documents in the .ris file are in English
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2/2. Me", 
             "rged/Ovid/1. Raw"))
write_refs(import_ovid_it2, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
# ProQuest.
table(import_proquest_it2$language) # ProQuest. All documents in the .ris file are in English. 
import_proquest_it2 <- import_proquest_it2[import_proquest_it2$language == "English" ,] # Select English 
# documents and store them. 3,632 documents in ProQuest document set.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2/2. Me", 
             "rged/ProQuest/1. Raw"))
write_refs(import_proquest_it2, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
# Scopus.
table(import_scopus_it2$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2/2. Me", 
             "rged/Scopus/1. Raw"))
write_refs(import_scopus_it2, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
# Web of Science.
table(import_wos_it2$language) # Web of Science. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2/2. Me", 
             "rged/Web of Science/1. Raw"))
write_refs(import_wos_it2, format = "ris", tag_naming = "synthesisr", file = "web_of_science_r")

### De-duplication. Please refer to the naive iteration for an overview of the exact steps of the 
### de-deplication procedure.

### Ovid.
## Exact matching.
length(import_ovid_it2$title) # Input is 5,645  documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  import_ovid_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(import_ovid_it2$title, exact_duplicates_ovid) # Perform a 
# manual check. 
length(exact_manual$title) # 14 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # 8 documents should be removed.
it2_dedup_ovid <- synthesisr::extract_unique_references(import_ovid_it2, exact_duplicates_ovid) 
length(it2_dedup_ovid$title) # Output after exact matching is 5,637. Eight documents removed.
## Fuzzy matching five.
fuzzy_duplicates_ovid <- find_duplicates( 
  it2_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. TWo duplicate combinations identified.
sum(table(fuzzy_duplicates_ovid) - 1) # Three documents should be removed.
it2_dedup_ovid <- extract_unique_references(it2_dedup_ovid, fuzzy_duplicates_ovid) # Extract unique 
# references. 
length(it2_dedup_ovid$title) # De-duplicated output is 5,634. Three documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_ovid <- find_duplicates( 
  it2_dedup_ovid$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. No duplicate combinations identified.
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2/2. Mer", 
             "ged/Ovid/2. Deduplicated"))
write_refs(it2_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_ovid")

### ProQuest. 
## Exact matching.
length(import_proquest_it2$title) # Input is 2,632 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  import_proquest_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_proquest_it2$title, exact_duplicates_proquest)) # All
# combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 83 articles should be removed.
it2_dedup_proquest <- extract_unique_references(import_proquest_it2, exact_duplicates_proquest) 
length(it2_dedup_proquest$title) # Output after exact matching is 2,549. 83 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  it2_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. All combinations are duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # 10 documents should be removed.
it2_dedup_proquest <- extract_unique_references(it2_dedup_proquest, fuzzy_duplicates_proquest) # 
# Extract unique references. 
length(it2_dedup_proquest$title) # De-duplicated output is 2,539. 10 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  it2_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. One duplicate combination identified.
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, c(135, 135, 709)) 
# Override non-duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # One document should be removed.
it2_dedup_proquest <- extract_unique_references(it2_dedup_proquest, fuzzy_duplicates_proquest) # Extract 
# unique references. 
length(it2_dedup_proquest$title) # De-duplicated output is 2,538. Four documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2/2. Mer", 
             "ged/ProQuest/2. Deduplicated"))
write_refs(it2_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_proquest")

### Scopus.
## Exact matching.
length(import_scopus_it2$title) # Input is 8,499 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  import_scopus_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_scopus_it2$title, exact_duplicates_scopus)) # Perform a 
# manual check. All combinations are duplicates.
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # Seven documents should be removed.
it2_dedup_scopus <- synthesisr::extract_unique_references(import_scopus_it2, exact_duplicates_scopus) 
length(it2_dedup_scopus$title) # Output after exact matching is 8,492 . Seven documents removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  it2_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. Two duplicate combinations identified.
sum(table(fuzzy_duplicates_scopus) - 1) # Two documents should be removed.
it2_dedup_scopus <- extract_unique_references(it2_dedup_scopus, fuzzy_duplicates_scopus) # Extract unique 
# references. 
length(it2_dedup_scopus$title) # De-duplicated output is 8,490. Two documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_scopus <- find_duplicates( 
  it2_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it2_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. No duplicate combinations identified.
length(it2_dedup_scopus$title) # De-duplicated output is 8,490. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2/2. Mer", 
             "ged/Scopus/2. Deduplicated"))
write_refs(it2_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_scopus")

### Web of Science.
length(import_wos_it2$title) # Input is 9,206 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  import_wos_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_wos_it2$title, exact_duplicates_wos)) # Perform a 
# manual check. Five duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Five documents should be removed.
it2_dedup_wos <- synthesisr::extract_unique_references(import_wos_it2, exact_duplicates_wos) 
length(it2_dedup_wos$title) # Output after exact matching is 9,201. Five documents removed.
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
fuzzy_duplicates_wos <- synthesisr::override_duplicates(fuzzy_duplicates_wos, c(3053)) 
it2_dedup_wos <- extract_unique_references(it2_dedup_wos, fuzzy_duplicates_wos) # Extract unique 
# references. 
length(it2_dedup_wos$title) # De-duplicated output is 9,201. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2/2. Mer", 
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
sum(round(as.numeric(check_recall(gs_group_contact$title, it2_dedup_ovid$title)[, 3]), 0)) # 2 of 6 gold 
# standard articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_group_contact$title, it2_dedup_proquest$title)[, 3]), 0)) # 3 of 6 gold 
## standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_group_contact$title, it2_dedup_scopus$title)[, 3]), 0)) # 3 of 6 gold 
# standard articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_group_contact$title, it2_dedup_wos$title)[, 3]), 0)) # 6 of 6 gold 
# standard articles are retrieved.
## The gold standard precision check tentatively indicates that Web of Science has the highest precision, 
## followed by ProQuest and Scopus, and finally by Ovid. Relative to the first iteration search, the 
## precision with respect to the external articles has increased in ProQuest (+2), Scopus (+1), and Web of 
## Science (+3), and stayed constant in Ovid (+0). 

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
## Ovid.
sum(round(as.numeric(check_recall(ex_group_contact$title, it2_dedup_ovid$title)[, 3]), 0)) # 37 of 98 
# external articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_group_contact$title, it2_dedup_proquest$title)[, 3]), 0)) # 24 of 98 
# external articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_group_contact$title, it2_dedup_scopus$title)[, 3]), 0)) # 46 of 98 
# external articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_group_contact$title, it2_dedup_wos$title)[, 3]), 0)) # 62 of 98 
# external articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus, Ovid, and ProQuest, respectively. Relative to the first iteration search, the 
## precision with respect to the external articles has increased in ProQuest (+6), Scopus (+8), Ovid (+7), 
## and Web of Science (+16). 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
it2_dedup <- bind_rows(it2_dedup_ovid, it2_dedup_proquest, it2_dedup_scopus, it2_dedup_wos) # Merge corpora. 
## Exact matching.
length(it2_dedup$title) # Input is 25,863 documents.
exact_duplicates <- synthesisr::find_duplicates(
  it2_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(it2_dedup$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 16,814 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 10,328 documents should be removed.
it2_dedup <- synthesisr::extract_unique_references(it2_dedup, exact_duplicates) 
length(it2_dedup$title) # Output after exact matching is 15,535. 10,328 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it2_dedup$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
fuzzy_manual <- review_duplicates(it2_dedup$title, fuzzy_duplicates) # Perform a manual check.
length(fuzzy_manual$title) #1,357 potential duplicate combinations. I check these candidates manually in 
# batches. Note that this procedure is prone to error.  
fuzzy_manual$title[1:500] # All combinations are duplicates.
fuzzy_manual$title[501:1000] # All combinations are duplicates. 
fuzzy_manual$title[1001:1357] # 12227 is not a duplicate combination. Remaining combinations are.  
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(12227)) 
sum(table(fuzzy_duplicates) - 1) # 686 documents should be removed.
it2_dedup <- extract_unique_references(it2_dedup, fuzzy_duplicates) # Extract unique references.
length(it2_dedup$title) # De-duplicated output is 14,849. 686 documents were removed.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it2_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it2_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 453 duplicate combinations.
fuzzy_manual$title # 93, 153, 221, 1396, 1436, 1721, 1731, 1777, 2570, 2648, 2879, 2879, 2879, 2879, 2879, 
# 2879, 3700, 4291, 4305, 4403, 4518, 4518, 4875, 5465, 5466, 6010, 6010, 6012, 6702, 11364, 12067, are not 
# duplicate combinations. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(93, 153, 221, 1396, 1436, 1721, 1731, 1777, 2570, 2648, 
                                                      2879, 2879, 2879, 2879, 2879, 2879, 3700, 4291, 4305, 
                                                      4403, 4518, 4518, 4875, 5465, 5466, 6010, 6010, 6012, 
                                                      6702, 11364, 12067)) 
sum(table(fuzzy_duplicates) - 1) # 199 documents should be removed.
it2_dedup <- extract_unique_references(it2_dedup, fuzzy_duplicates) # Extract unique references. 
length(it2_dedup$title) # De-duplicated output is 14,650. 199 documents removed. 
## Fuzzy matching fifteen.
fuzzy_duplicates <- find_duplicates(
  it2_dedup$title, # Fuzzy matching ten output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 15) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it2_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 266 duplicate combinations.
fuzzy_manual$title # 31, 47, 92, 103, 152, 156, 220, 318, 376, 474, 483, 483, 483, 483, 483, 483, 483, 483, 
# 483, 483, 483, 483, 483, 483, 483, 483, 483, 483, 483, 483, 483, 483, 514, 520, 590, 602, 713, 713, 897, 
# 933, 935, 1131, 1175, 1271, 1388, 1388, 1388, 1388, 1388, 1388, 1466, 1577, 1624, 1716, 1742, 1747, 1762, 
# 1851, 1934, 1934, 1934, 1934, 1934, 1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948, 2126, 2162, 2540, 
# 2549, 2550, 2550, 2571, 2588, 2612, 2628, 2742, 2821, 2828, 2851, 3012, 3145, 3202, 3344, 3430, 3671, 3671, 
# 3685, 3704, 4180, 4180, 4199, 4259, 4273, 4318, 4808, 4834, 5167, 5231, 5231, 5231, 5421, 5422, 5846, 5918, 
# 5918, 5918, 6128, 6932, 6932, 6934, 7308, 8168, 9043, 9043, 9884, 11219, 11267, 12001, 12491, are not 
# duplicate combinations.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(31, 47, 92, 103, 152, 156, 220, 318, 376, 474, 483, 
                                                      483, 483, 483, 483, 483, 483, 483, 483, 483, 483, 483, 
                                                      483, 483, 483, 483, 483, 483, 483, 483, 483, 483, 514, 
                                                      520, 590, 602, 713, 713, 897, 933, 935, 1131, 1175, 
                                                      1271, 1388, 1388, 1388, 1388, 1388, 1388, 1466, 1577, 
                                                      1624, 1716, 1742, 1747, 1762, 1851, 1934, 1934, 1934, 
                                                      1934, 1934, 1948, 1948, 1948, 1948, 1948, 1948, 1948, 
                                                      1948, 1948, 2126, 2162, 2540, 2549, 2550, 2550, 2571, 
                                                      2588, 2612, 2628, 2742, 2821, 2828, 2851, 3012, 3145, 
                                                      3202, 3344, 3430, 3671, 3671, 3685, 3704, 4180, 4180, 
                                                      4199, 4259, 4273, 4318, 4808, 4834, 5167, 5231, 5231, 
                                                      5231, 5421, 5422, 5846, 5918, 5918, 5918, 6128, 6932, 
                                                      6932, 6934, 7308, 8168, 9043, 9043, 9884, 11219, 11267, 
                                                      12001, 12491)) 
sum(table(fuzzy_duplicates) - 1) # 31 documents should be removed.
it2_dedup <- extract_unique_references(it2_dedup, fuzzy_duplicates) # Extract unique references. 
length(it2_dedup$title) # De-duplicated output is 14,619 31 documents removed. 
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2/2. Merged")
write_refs(it2_dedup, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_refman")
write_refs(it2_dedup, format = "bib", tag_naming = "synthesisr", file = "it2_dedup_refman")
# EndNote and Zotero indicate that 48 duplicates remain in the .ris file, which are removed. We furthermore 
# remove seven retracted papers that are flagged in EndNote and Zotero: "Interprofessional learning in acute 
# care: Developing a theoretical framework" (1 document), "Bridging the Gap on Facebook: Assessing Intergroup 
# Contact and Its Effects for Intergroup Relations" (3 documents), "The Evolution of Intergroup Bias: 
# Perceptions and Attitudes in Rhesus Macaques" (3 documents), "When contact changes minds: An experiment on 
# transmission of support for gay equality" (2 documents), The result is exported from Zotero in the 
# file "it2_dedup.ris" which is subsequently imported.  
it2_dedup <- read_bibliography("it2_dedup.ris")
length(it2_dedup$title) # 14,562 documents. 57 documents removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_group_contact$title, it2_dedup$title) # 6 of 6 gold standard 
# articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_group_contact$title, it2_dedup$title)[, 3]), 0)) # 67 of 98 external 
# articles are retrieved.

#### Check second iteration search against the naive and first iteration searches. 

### Relative to naive search.
## Overlap with naive search. Here I do not use the "check_recall" function because it takes a long time, 
## and it is not exact / conservative enough, for the purpose of merging the naive search documents that are 
## not in the second iteration search. I do this to try and maximize coverage. 
overlap_naive_it2 <- bind_rows(naive_dedup, it2_dedup) # Merge corpora. Should be 2,043 + 14,562 = 16,605. 
## Exact matching.
length(overlap_naive_it2$title) # Input is 16,605 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_naive_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_naive_it2$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 3,906 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 1,953 documents should be removed.
it2_dedup_out_naive <- synthesisr::extract_unique_references(overlap_naive_it2, exact_duplicates) 
length(it2_dedup_out_naive$title) # Output after exact matching is 14,652. 90 documents remain. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it2_dedup_out_naive$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it2_dedup_out_naive$title, fuzzy_duplicates)) # Perform a manual check.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(14628)) 
sum(table(fuzzy_duplicates) - 1) # 21 documents should be removed.
it2_dedup_out_naive <- extract_unique_references(it2_dedup_out_naive, fuzzy_duplicates) # Extract unique 
# references.
length(it2_dedup_out_naive$title) # De-duplicated output is 14,631. 69 documents remain.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2")
write_refs(it2_dedup_out_naive, format = "ris", tag_naming = "synthesisr", 
           file = "it2_dedup_out_naive_refman")
write_refs(it2_dedup_out_naive, format = "bib", tag_naming = "synthesisr", 
           file = "it2_dedup_out_naive_refman") # Also save as .bib for importing into Zotero.
# EndNote and Zotero indicate that three duplicates remain in the .ris file after the de-duplication 
# procedure, which are removed. Two more retractions are also identified in Zotero: "Ethnic threat and 
# social control: Examining public support for judicial use of ethnicity in punishment" (2 documents) and 
# "Structural stigma and all-cause mortality in sexual minority populations" (2 documents). The result is 
# exported from Zotero in the file "it2_dedup_out_naive.ris" which is subsequently imported.  
it2_dedup_out_naive <- read_bibliography("it2_dedup_out_naive.ris")
length(it2_dedup_out_naive$title) # 14,624 documents. 7 documents removed. 62 documents remain.

## Coverage gain relative to the naive iteration search. I define coverage as the sum of the additional 
## documents that were found relative to the previous search iteration, and the documents from the previous 
## search iteration that were not identified in the current one.  
length(naive_dedup$title) # 2,043 articles.
length(it2_dedup$title) # 14,562 articles.
((length(it2_dedup$title) - length(naive_dedup$title))) # 12,519 document increase.
(length(it2_dedup_out_naive$title) - length(it2_dedup$title)) # 62 documents from the naive search not in the
# first iteration search. Coverage increase of 12,519 + 62 = 12,581 or (12,581 / 2,043) = 6,158 as a factor
# increase.   
1 - (length(it2_dedup_out_naive$title) - length(it2_dedup$title)) / length(naive_dedup$title) # 96.97% of 
# the naive search was in the second iteration search. 

## Relative to first iteration. Note that the naive search articles are also in the first iteration 
## output object. 
## Overlap with first iteration search. Here I do not use the "check_recall" function because it takes a 
## long time, and it is not exact / conservative enough, for the purpose of merging the first iteration 
## search documents that are not in the second iteration search. I do this to try and maximize coverage. 
length(it1_dedup_out$title)
length(it2_dedup_out_naive$title)
overlap_it1_it2 <- bind_rows(it1_dedup_out, it2_dedup_out_naive) # Merge corpora. Should be 6,410 + 14,624 = 
# 21,034. 
## Exact matching.
length(overlap_it1_it2$title) # Input is 21,034 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_it1_it2$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_it1_it2$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 11,126 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 5,563 documents should be removed.
overlap_it1_it2 <- synthesisr::extract_unique_references(overlap_it1_it2, exact_duplicates) 
length(overlap_it1_it2$title) # Output after exact matching is 15,471. 5,563 documents removed. 847 documents
# remain.
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  overlap_it1_it2$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(overlap_it1_it2$title, fuzzy_duplicates)) # Perform a manual check.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(5701)) 
sum(table(fuzzy_duplicates) - 1) # 48 documents should be removed.
it2_dedup_out <- extract_unique_references(overlap_it1_it2, fuzzy_duplicates) # Extract unique references.
length(it2_dedup_out$title) # De-duplicated output is 15,423. 48 documents were removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/4. Iteration 2")
write_refs(it2_dedup_out, format = "ris", tag_naming = "synthesisr", file = "it2_dedup_out_refman")
write_refs(it2_dedup_out, format = "bib", tag_naming = "synthesisr", file = "it2_dedup_out_refman")
# EndNote and Zotero indicate that 5 duplicates remain in the .ris file after the de-duplication 
# procedure, which are removed. The result is exported from Zotero in the file "it2_dedup_out_naive.ris" 
# which is subsequently imported.  
it2_dedup_out <- read_bibliography("it2_dedup_out.ris")
length(it2_dedup_out$title) # 15,417 documents. 5 documents removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_group_contact$title, it2_dedup_out$title) # 6 of 6 gold standard articles are retrieved.
## External precision check.
check_recall(ex_group_contact$title, it2_dedup_out$title) # 69 of 98 or 70.41% of the external articles are 
# retrieved.

## Coverage gain relative to the first iteration search. I define coverage as the sum of the additional 
## documents that were found relative to the previous search iteration, and the documents from the previous 
## search iteration that were not identified in the current one. 
length(it1_dedup_out$title) # 6,410 articles.
length(it2_dedup_out$title) # 15,417 articles.
(length(it2_dedup_out$title) - length(it1_dedup_out$title)) # 9,007 article increase. 
(length(it2_dedup_out$title) - length(it2_dedup_out_naive$title)) # 793 documents from the first iteration
# search were not in the second. Coverage increase of 793 + 9,007 = 9,800 or (9,800 / 6,410) = 1.52 as a 
# factor increase. 
1 - (length(it2_dedup_out$title) - length(it2_dedup_out_naive$title)) / length(it1_dedup_out$title) # 87.63%
# of the first iteration search was in the second iteration search. 

### It is subsequently the question whether we should continue with a third iteration search or not. Note 
### that approximately 97% of the naive and 88% of the first iteration search is contained in the second 
### iteration search, and that the second iteration search added approximately 52% in terms of documents to 
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

### Now repeat the previous procedure, but with keywords extracted from the first iteration search corpus.  

### Clear the environment except for the gold standard search terms, the validation article sets, and 
### the "naive_dedup", and "it1_dedup_out" objects. 
rm(list = setdiff(ls(), c("gs_grouped_terms", "gs_group_contact", "ex_group_contact", "naive_dedup", 
                          "it1_dedup_out", "it2_dedup_out")))

### Start by checking how many documents in the first iteration search corpus provide keywords, titles, and 
### abstracts.
## Keywords.
length(it2_dedup_out$keywords) # Total number of documents is 15,417. 
length(it2_dedup_out$keywords) - sum(is.na(it2_dedup_out$keywords)) # All of the articles list keywords.  
## Titles and abstracts.
length(it2_dedup_out$title) - sum(is.na(it2_dedup_out$title)) # All of the articles list a title.  
length(it2_dedup_out$abstract) - sum(is.na(it2_dedup_out$abstract)) # All of the articles list an abstract.  

### The first step is to obtain the keywords listed in the articles. We extract all non-single keywords that 
### are listed a minimum of eighty times. Note that the eighty number is arbitrary insofar that we need to 
### strike a balance between retrieving enough but not too many keyword candidates. More specifically,
### Gusenbauer & Haddaway (2020) note that search terms of length twenty-five should allow reviewers to 
### specify their search scope to a reasonable extent. Put differently, our Boolean search should contain at 
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
  min_freq = 80, # The keyword has to occur a minimum of eighty times to be included.   
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(tagged_keywords) # 248 tagged keywords.
### Now use the RAKE to obtain keywords from the titles and abstracts of the search corpus. We extract all 
### non-single keywords that occur at least eighty times in these titles and abstracts. 
raked_keywords <- litsearchr::extract_terms(
  text = paste(it2_dedup_out$title, it2_dedup_out$abstract), # This is a list of titles and abstracts.
  method = "fakerake", # Indicate that 'litsearchr' should use the RAKE algorithm.
  min_freq = 80, # The keyword has to occur a minimum of eighty times to be included.
  ngrams = TRUE, # 'litsearchr' should only consider an ngram with a minimum of length n, 
  min_n = 2, # where n is equal to 2.
  language = "English") # 'litsearchr' should only consider English keywords.
length(raked_keywords) # 321 raked keywords.
## Sum total of tagged and raked keywords is 180 + 317 = 497. 
keyword_candidates <- remove_redundancies(c(tagged_keywords, raked_keywords), closure = "full") # Remove
# duplicate terms.
length(keyword_candidates) # Total of 421 keyword candidates. 
## The subsequent task is to select all keywords that are relevant to our query from the candidate pool. I 
## select terms that relate in content to the relevant literature, i.e., are a independent or dependent 
## variable of interest in the group contact on inter-ethnic attitudes relationship. Note that I interpret 
## relevance quite broadly, since these terms will be will be ranked and filtered further based on their 
## "connectedness" in a co-occurrence network. In that sense, it is better too be too liberal than too 
## selective in our choices here, since it might be hard to anticipate which relevant terms are important 
## from this "connectedness" perspective. I furthermore do not include keywords which relate directly to any 
## of the other paradigms, e.g., I do not include keywords on group threat theory even though these might 
## appear often in the group contact literature. Finally note that I do this part manually, which is prone to 
## errors. 
all_keywords <- keyword_candidates[
  c(2, 3, 27, 28, 36, 42, 48, 50, 51, 52, 53, 55, 56, 58, 65, 66, 67, 68, 69, 70, 71, 94, 96, 102, 103, 105, 
    106, 107, 108, 109, 110, 111, 112, 127, 128, 129, 130, 135, 139, 143, 147, 150, 157, 159, 160, 165, 166, 
    168, 169, 170, 188, 191, 193, 197, 198, 200, 201, 203, 204, 206, 207, 208, 209, 210, 211, 212, 213, 219, 
    220, 221, 225, 226, 230, 231, 234, 236, 237, 270, 282, 286, 287, 289, 293, 294, 295, 296, 297, 318, 319, 
    320,327, 328, 329, 333, 334, 336, 340, 341, 346, 347, 348, 351, 356, 358, 362, 363, 370, 371, 372, 383,
    386, 387, 389, 390, 391, 394, 398, 411, 412, 415, 416, 417, 421, 422, 445, 446, 447, 448, 450, 451, 452,
    455) 
]
## Manual cleaning. 
all_keywords <- all_keywords[-c(1)] # Remove "0410 group interactions".
all_keywords[1] <- "group interactions" # Re-format.
(all_keywords <- sort(all_keywords))
length(all_keywords) # 131 keyword candidates. 

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
length(all_keywords_final) # 146 candidates.
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
## I now apply a filter to select  and  terms for the search string in OVID, Scopus, and Web of Science 
## and ProQuest, respectively, where I remove terms which refer to an equivalent or near equivalent concept, 
## i.e., "target group" and "target groups", where I select the term with the highest strength value. 
(term_strengths <- cbind(seq(length(term_strengths$term)), term_strengths[order(term_strengths$term), ]))
term_strengths <- term_strengths[-c(6, 9, 14, 17, 19, 22, 24, 27, 31, 34, 43, 44, 51, 54, 58, 61, 63, 66, 69, 
                                    71, 73, 76, 83, 85, 91, 99, 104, 105, 107, 109, 112, 113, 116, 120, 121, 
                                    122, 128, 129, 135), ]

### Construct Boolean search OVID, Web of Science, and Scopus.
## Select first 60 terms from filtered term set. 
keywords_ovid_scopus_wos <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:58, ]$term)
(keywords_ovid_scopus_wos <- keywords_ovid_scopus_wos[order(keywords_ovid_scopus_wos)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3")
sink("third_iteration_selected_terms_ovid_scopus_wos.txt")
print(keywords_ovid_scopus_wos)
sink() 
## Categorize "keywords_ovid_scopus_wos" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_ovid_scopus_wos <- list(
  determinant = keywords_ovid_scopus_wos[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20,
                                           21, 22, 23, 27, 29, 30, 31, 32, 34, 36, 38, 41, 42, 49, 52, 54, 
                                           55, 56, 57, 58)],
  outcome = keywords_ovid_scopus_wos[c(12, 24, 25, 26, 28, 33, 35, 37, 39, 40, 43, 44, 45, 46, 47, 48, 50, 
                                       51, 53)])
grouped_terms_ovid_scopus_wos
### Given the grouped terms, write the Boolean search for OVID, Web of Science, and Scopus to the directory
### and print it to the console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3") # Set directory.
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
## Select first 36 terms from filtered term set.
keywords_proquest <- as.character(term_strengths[order(term_strengths$strength, decreasing = T), 
][1:40, ]$term)
(keywords_proquest <- keywords_proquest[order(keywords_proquest)])
## Save the grouped terms to a text file in the working directory. I do this to keep the search reproducible 
## in case some previous chunk of code does not replicate.
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3")
sink("third_iteration_selected_terms_proquest.txt")
print(keywords_proquest)
sink() 
## Categorize "keywords_proquest" object based on keyword being a potential dependent or independent 
## variable of interest in the group threat on inter-ethnic attitudes relationship.
grouped_terms_proquest <- list(
  determinant = keywords_proquest[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 14, 16, 17, 18, 19, 22, 25, 31, 34, 36, 
                                    37, 38, 39, 40)],
  outcome = keywords_proquest[c(11, 12, 13, 15, 20, 21, 23, 24, 26, 27, 28, 29, 30, 32, 33, 35)])
grouped_terms_proquest$outcome <- grouped_terms_proquest$outcome[c(-4)] # Remove "intergroup relations" 
# because it blows ups the search in ProQuest.
### Given the grouped terms, write the the Boolean search for ProQuest to the directory and print it to the 
### console.  
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3") # Set directory.
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

### All searches were conducted on 04-08-2022. This resulted in 7,576 documents from Ovid (Selection: 
### PsycINFO), 3,793 documents from Proquest (Selection: Sociological Abstracts), 11,674 documents from Scopus 
### (Selection: Full index), and 12,170 documents from Web of Science (Selection: Web of Science Core 
### Collection), for a total of 35,213 documents. Please note that this document set is unfiltered, i.e., 
### duplicates, retracted documents, unidentified non-English documents, etc., have not yet been removed. In 
### Ovid, the documents were manually extracted in three batches. In ProQuest the documents were manually 
### extracted in 100-sized batches. In Scopus, the documents were manually extracted in 2000 document sized 
### batches, stratified by publication year. In Web of Science, the documents were manually extracted in 1000 
### document sized batches. 

### Data import and cleaning.
## Import results of informed search. Note that the batches for each search system were merged in EndNote.
## Start by checking whether the imported length is equal to the length of the raw .ris files.
import_ovid_it3 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iterat", 
                     "ion 3/1. Unmerged/Ovid"),
  verbose = TRUE)
import_proquest_it3 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iterat", 
                     "ion 3/1. Unmerged/ProQuest"),
  verbose = TRUE)
import_scopus_it3 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iterat", 
                     "ion 3/1. Unmerged/Scopus"),
  verbose = TRUE)
import_wos_it3 <- import_results(
  directory = paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iterat", 
                     "ion 3/1. Unmerged/Web of Science"),
  verbose = TRUE)
length(import_ovid_it3$title) # 7,576, which is correct.  
length(import_proquest_it3$title) # 3,793, which is correct. 
length(import_scopus_it3$title) # 11,674, which is correct.  
length(import_wos_it3$title) # 12,170, which is correct.  

## We subsequently identify and remove identifiable, non-English documents, if necessary. We then save the 
## resulting file.
# Ovid.
table(import_ovid_it3$language) # Ovid. All documents in the .ris file are in English
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3/2. Me", 
             "rged/Ovid/1. Raw"))
write_refs(import_ovid_it3, format = "ris", tag_naming = "synthesisr", file = "ovid_r")
# ProQuest.
table(import_proquest_it3$language) # ProQuest. All documents in the .ris file are in English. 
import_proquest_it3 <- import_proquest_it3[import_proquest_it3$language == "English" ,] # Select English 
# documents and store them. 3,632 documents in ProQuest document set.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3/2. Me", 
             "rged/ProQuest/1. Raw"))
write_refs(import_proquest_it3, format = "ris", tag_naming = "synthesisr", file = "proquest_r")
# Scopus.
table(import_scopus_it3$language) # Scopus. All documents in the .ris file are in English.
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3/2. Me", 
             "rged/Scopus/1. Raw"))
write_refs(import_scopus_it3, format = "ris", tag_naming = "synthesisr", file = "scopus_r")
# Web of Science.
table(import_wos_it3$language) # Web of Science. All documents in the .ris file are in English. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3/2. Me", 
             "rged/Web of Science/1. Raw"))
write_refs(import_wos_it3, format = "ris", tag_naming = "synthesisr", file = "web_of_science_r")

### De-duplication. Please refer to the naive iteration for an overview of the exact steps of 
### the de-duplication procedure.

### Ovid.
## Exact matching.
length(import_ovid_it3$title) # Input is 7,576  documents.
exact_duplicates_ovid <- synthesisr::find_duplicates(
  import_ovid_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(import_ovid_it3$title, exact_duplicates_ovid) # Perform a 
# manual check. 
length(exact_manual$title) # 15 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_ovid) - 1)) # 8 documents should be removed.
it3_dedup_ovid <- synthesisr::extract_unique_references(import_ovid_it3, exact_duplicates_ovid) 
length(it3_dedup_ovid$title) # Output after exact matching is 7,568. Eight documents removed.
## Fuzzy matching five.
fuzzy_duplicates_ovid <- find_duplicates( 
  it3_dedup_ovid$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. TWo duplicate combinations identified.
sum(table(fuzzy_duplicates_ovid) - 1) # Three documents should be removed.
it3_dedup_ovid <- extract_unique_references(it3_dedup_ovid, fuzzy_duplicates_ovid) # Extract unique 
# references. 
length(it3_dedup_ovid$title) # De-duplicated output is 7,565. Three documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_ovid <- find_duplicates( 
  it3_dedup_ovid$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_ovid$title, fuzzy_duplicates_ovid)) # Perform a 
# manual check. No duplicate combinations identified.
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3/2. Mer", 
             "ged/Ovid/2. Deduplicated"))
write_refs(it3_dedup_ovid, format = "ris", tag_naming = "synthesisr", file = "it3_dedup_ovid")

### ProQuest. 
## Exact matching.
length(import_proquest_it3$title) # Input is 3,786 documents.
exact_duplicates_proquest <- synthesisr::find_duplicates(
  import_proquest_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_proquest_it3$title, exact_duplicates_proquest)) # All
# combinations are duplicates.
sum(as.numeric(table(exact_duplicates_proquest) - 1)) # 130 articles should be removed.
it3_dedup_proquest <- extract_unique_references(import_proquest_it3, exact_duplicates_proquest) 
length(it3_dedup_proquest$title) # Output after exact matching is 3,656. 130 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_proquest <- find_duplicates( 
  it3_dedup_proquest$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. All combinations are duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # 14 documents should be removed.
it3_dedup_proquest <- extract_unique_references(it3_dedup_proquest, fuzzy_duplicates_proquest) # 
# Extract unique references. 
length(it3_dedup_proquest$title) # De-duplicated output is 3,642. 14 documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_proquest <- find_duplicates( 
  it3_dedup_proquest$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_proquest$title, fuzzy_duplicates_proquest)) # 
# Perform a manual check. Three duplicate combinations identified.
fuzzy_duplicates_proquest <- synthesisr::override_duplicates(fuzzy_duplicates_proquest, c(486, 1213, 1213, 
                                                                                          1492)) 
# Override non-duplicates.
sum(table(fuzzy_duplicates_proquest) - 1) # Three documents should be removed.
it3_dedup_proquest <- extract_unique_references(it3_dedup_proquest, fuzzy_duplicates_proquest) # Extract 
# unique references. 
length(it3_dedup_proquest$title) # De-duplicated output is 3,639. Three documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3/2. Mer", 
             "ged/ProQuest/2. Deduplicated"))
write_refs(it3_dedup_proquest, format = "ris", tag_naming = "synthesisr", file = "it3_dedup_proquest")

### Scopus.
## Exact matching.
length(import_scopus_it3$title) # Input is 11,674 documents.
exact_duplicates_scopus <- synthesisr::find_duplicates(
  import_scopus_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_scopus_it3$title, exact_duplicates_scopus)) # Perform a 
# manual check. All combinations are duplicates.
sum(as.numeric(table(exact_duplicates_scopus) - 1)) # 11 documents should be removed.
it3_dedup_scopus <- synthesisr::extract_unique_references(import_scopus_it3, exact_duplicates_scopus) 
length(it3_dedup_scopus$title) # Output after exact matching is 11,663. 11 documents removed.
## Fuzzy matching five.
fuzzy_duplicates_scopus <- find_duplicates( 
  it3_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. Two duplicate combinations identified.
fuzzy_duplicates_scopus <- synthesisr::override_duplicates(fuzzy_duplicates_scopus, c(5166)) 
# Override non-duplicates.
sum(table(fuzzy_duplicates_scopus) - 1) # Two documents should be removed.
it3_dedup_scopus <- extract_unique_references(it3_dedup_scopus, fuzzy_duplicates_scopus) # Extract unique 
# references. 
length(it3_dedup_scopus$title) # De-duplicated output is 11,661. Two documents removed. 
## Fuzzy matching ten.
fuzzy_duplicates_scopus <- find_duplicates( 
  it3_dedup_scopus$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased threshold of ten. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_scopus$title, fuzzy_duplicates_scopus)) # Perform a 
# manual check. No duplicate combinations identified.
length(it3_dedup_scopus$title) # De-duplicated output is 11,661. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3/2. Mer", 
             "ged/Scopus/2. Deduplicated"))
write_refs(it3_dedup_scopus, format = "ris", tag_naming = "synthesisr", file = "it3_dedup_scopus")

### Web of Science.
length(import_wos_it3$title) # Input is 12,170 documents.
exact_duplicates_wos <- synthesisr::find_duplicates(
  import_wos_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
(exact_manual <- synthesisr::review_duplicates(import_wos_it3$title, exact_duplicates_wos)) # Perform a 
# manual check. Five duplicate combinations identified.
sum(as.numeric(table(exact_duplicates_wos) - 1)) # Seven documents should be removed.
it3_dedup_wos <- synthesisr::extract_unique_references(import_wos_it3, exact_duplicates_wos) 
length(it3_dedup_wos$title) # Output after exact matching is 12,163. Seven documents removed.
## Fuzzy matching five.
fuzzy_duplicates_wos <- find_duplicates( 
  it3_dedup_wos$title, # Exact matching output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default threshold of five. 
(fuzzy_manual <- synthesisr::review_duplicates(it3_dedup_wos$title, fuzzy_duplicates_wos)) # Perform a 
# manual check. No duplicate combinations identified.
length(it3_dedup_wos$title) # De-duplicated output is 12,163. Zero documents removed. 
## Save output as .ris file. 
setwd(paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3/2. Mer", 
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
## non-overlapping documents to the sum total.

### Corpus precision diagnostics. Please refer to the naive iteration for an overview of the exact steps of 
### the precision diagnostics.

## Gold standard precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
## Ovid.
sum(round(as.numeric(check_recall(gs_group_contact$title, it3_dedup_ovid$title)[, 3]), 0)) # 2 of 6 gold 
# standard articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(gs_group_contact$title, it3_dedup_proquest$title)[, 3]), 0)) # 3 of 6 gold 
## standard articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(gs_group_contact$title, it3_dedup_scopus$title)[, 3]), 0)) # 3 of 6 gold 
# standard articles are retrieved. 
## Web of Science.
sum(round(as.numeric(check_recall(gs_group_contact$title, it3_dedup_wos$title)[, 3]), 0)) # 6 of 6 gold 
# standard articles are retrieved.
## The gold standard precision check tentatively indicates that Web of Science has the highest precision, 
## followed by ProQuest and Scopus, and finally by Ovid. Relative to the second iteration search, the 
## precision with respect to the external articles has remained constant in all search systems. 

## External precision check. For convenience I assume that similarity scores > 0.5 are a match, and 
## those =< 0.5 are not.
## Ovid.
sum(round(as.numeric(check_recall(ex_group_contact$title, it3_dedup_ovid$title)[, 3]), 0)) # 37 of 98 
# external articles are retrieved.
## ProQuest.
sum(round(as.numeric(check_recall(ex_group_contact$title, it3_dedup_proquest$title)[, 3]), 0)) # 26 of 98 
# external articles are retrieved.
## Scopus.
sum(round(as.numeric(check_recall(ex_group_contact$title, it3_dedup_scopus$title)[, 3]), 0)) # 47 of 98 
# external articles are retrieved.
## Web of Science.
sum(round(as.numeric(check_recall(ex_group_contact$title, it3_dedup_wos$title)[, 3]), 0)) # 61 of 98 
# external articles are retrieved.
## The external precision check tentatively indicates that Web of Science has the highest precision, 
## followed by Scopus, Ovid, and ProQuest, respectively. Relative to the second iteration search, the 
## precision with respect to the external articles has remained constant in Ovid (+0), increased in ProQuest 
## (+2), Scopus (+1), and decreased in Web of Science (-1). 

### Remove duplicates between corpora and combine the various corpora into a .ris file.
it3_dedup <- bind_rows(it3_dedup_ovid, it3_dedup_proquest, it3_dedup_scopus, it3_dedup_wos) # Merge corpora. 
## Exact matching.
length(it3_dedup$title) # Input is 35,028 documents.
exact_duplicates <- synthesisr::find_duplicates(
  it3_dedup$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(it3_dedup$title, exact_duplicates) # Perform a 
# manual check.
length(exact_manual$title) # Sum of 23,390 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 14,354 documents should be removed.
it3_dedup <- synthesisr::extract_unique_references(it3_dedup, exact_duplicates) 
length(it3_dedup$title) # Output after exact matching is 20,674. 14,354 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it3_dedup$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
fuzzy_manual <- review_duplicates(it3_dedup$title, fuzzy_duplicates) # Perform a manual check.
length(fuzzy_manual$title) # 1,746 potential duplicate combinations. I check these candidates manually in 
# batches. Note that this procedure is prone to error.  
fuzzy_manual$title[1:500] # All combinations are duplicates.
fuzzy_manual$title[501:1000] # All combinations are duplicates. 
fuzzy_manual$title[1001:1500] # 11835, Remaining combinations are duplicates. 
fuzzy_manual$title[1501:1746] # 16660.  Remaining combinations are duplicates.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(11835, 16660)) 
sum(table(fuzzy_duplicates) - 1) # 883 documents should be removed.
it3_dedup <- extract_unique_references(it3_dedup, fuzzy_duplicates) # Extract unique references.
length(it3_dedup$title) # De-duplicated output is 19,791. 883 documents were removed.
## Fuzzy matching ten.
fuzzy_duplicates <- find_duplicates(
  it3_dedup$title, # Fuzzy matching five output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 10) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it3_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 553 duplicate combinations.
fuzzy_manual$title # 129, 217, 310, 1930, 1986, 2368, 2379, 2438, 2604, 2604, 2604, 2604, 2604, 2604, 2604, 
# 2604, 2716, 2716, 3112, 3161, 3590, 5036, 5074, 5254, 5847, 5857, 5874, 6001, 6149, 6149, 6575, 7338, 7341, 
# 8406, 9463, 11741, 15491, 16465, are not duplicate combinations. 
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c(129, 217, 310, 1930, 1986, 2368, 2379, 2438, 2604, 
                                                      2604, 2604, 2604, 2604, 2604, 2604, 2604, 2716, 2716, 
                                                      3112, 3161, 3590, 5036, 5074, 5254, 5847, 5857, 5874, 
                                                      6001, 6149, 6149, 6575, 7338, 7341, 8406, 9463, 11741, 
                                                      15491, 16465)) 
sum(table(fuzzy_duplicates) - 1) # 243 documents should be removed.
it3_dedup <- extract_unique_references(it3_dedup, fuzzy_duplicates) # Extract unique references. 
length(it3_dedup$title) # De-duplicated output is 19,548. 243 documents removed. 
## Fuzzy matching fifteen.
fuzzy_duplicates <- find_duplicates(
  it3_dedup$title, # Fuzzy matching ten output as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 15) # increased cutoff. 
fuzzy_manual <- synthesisr::review_duplicates(it3_dedup$title, fuzzy_duplicates) # Perform a manual check. 
length(fuzzy_manual$title) # 318, 215, 308, 418, 446, 515, 649, 654, 660, 660, 660, 660, 660, 660, 660, 660, 
# 660, 660, 660, 660, 660, 660, 660, 660, 660, 660, 660, 660, 660, 660, 660, 702, 706, 783, 799, 812, 894, 
# 980, 980, 980, 1233, 1292, 1294, 1578, 1587, 1587, 1601, 1636, 1768, 1919, 1919, 1919, 1919, 1919, 1919, 
# 2025, 2178, 2244, 2360, 2397, 2402, 2419, 2548, 2585, 2585, 2585, 2585, 2585, 2585, 2585, 2585, 2585, 2585, 
# 2585, 2585, 2585, 2585, 2676, 2676, 2676, 2676, 2961, 3071, 3091, 3140, 3463, 3474, 3498, 3519, 3547, 3549,
# 3549, 3566, 3715, 3827, 3838, 3869, 4284, 4310, 4365, 4397, 4425, 4557, 4678, 5003, 5003, 5018, 5041, 5044, 
# 5713, 5713, 5736, 5818, 5835, 5888, 6027, 6027, 6027, 6027, 6027, 6148, 6495, 6526, 6568, 6766, 6824, 6927,
# 6927, 6982, 7042, 7042, 7042, 7283, 7286, 7469, 7714, 7714, 7782, 7833, 8150, 8300, 8364, 8364, 9494, 
# 11644, 12491, 13138, 13138, 15048, 15376, 15988, 19031, are not duplicate combinations.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, 
                                                    c( 318, 215, 308, 418, 446, 515, 649, 654, 660, 660, 660, 
                                                       660, 660, 660, 660, 660, 660, 660, 660, 660, 660, 660, 
                                                       660, 660, 660, 660, 660, 660, 660, 660, 660, 702, 706, 
                                                       783, 799, 812, 894, 980, 980, 980, 1233, 1292, 1294, 
                                                       1578, 1587, 1587, 1601, 1636, 1768, 1919, 1919, 1919, 
                                                       1919, 1919, 1919, 2025, 2178, 2244, 2360, 2397, 2402, 
                                                       2419, 2548, 2585, 2585, 2585, 2585, 2585, 2585, 2585, 
                                                       2585, 2585, 2585, 2585, 2585, 2585, 2585, 2676, 2676, 
                                                       2676, 2676, 2961, 3071, 3091, 3140, 3463, 3474, 3498, 
                                                       3519, 3547, 3549, 3549, 3566, 3715, 3827, 3838, 3869, 
                                                       4284, 4310, 4365, 4397, 4425, 4557, 4678, 5003, 5003, 
                                                       5018, 5041, 5044, 5713, 5713, 5736, 5818, 5835, 5888, 
                                                       6027, 6027, 6027, 6027, 6027, 6148, 6495, 6526, 6568, 
                                                       6766, 6824, 6927, 6927, 6982, 7042, 7042, 7042, 7283, 
                                                       7286, 7469, 7714, 7714, 7782, 7833, 8150, 8300, 8364, 
                                                       8364, 9494, 11644, 12491, 13138, 13138, 15048, 15376, 
                                                       15988, 19031)) 
sum(table(fuzzy_duplicates) - 1) # 38 documents should be removed.
it3_dedup <- extract_unique_references(it3_dedup, fuzzy_duplicates) # Extract unique references. 
length(it3_dedup$title) # De-duplicated output is 19,510. 38 documents removed. 
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3/2. Merged")
write_refs(it3_dedup, format = "ris", tag_naming = "synthesisr", file = "it3_dedup_refman")
write_refs(it3_dedup, format = "bib", tag_naming = "synthesisr", file = "it3_dedup_refman")
# EndNote and Zotero indicate that 59 duplicates remain in the .ris file, which are removed. We furthermore 
# remove  retracted papers that are flagged in EndNote: "The heterogeneous effect of diversity: Ascriptive 
# identities, class and redistribution in developed democracies" (1), "Interprofessional learning in acute 
# care: Developing a theoretical framework" (1), "Bridging the Gap on Facebook: Assessing Intergroup Contact 
# and Its Effects for Intergroup Relations" (3), "The Evolution of Intergroup Bias: Perceptions and Attitudes 
# in Rhesus Macaques" (4), "When contact changes minds: An experiment on transmission of support for gay 
# equality" (2), "Visual Darkness Reduces Perceived Risk of Contagious-Disease Transmission From 
# Interpersonal Interaction (1), "Helping the ingroup versus harming the outgroup: Evidence from morality-based
# groups" (2). The result is exported from Zotero in the file "it3_dedup.ris" which is subsequently imported.  
it3_dedup <- read_bibliography("it3_dedup.ris")
length(it3_dedup$title) # 19,437 documents. 73 documents removed. 

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_group_contact$title, it3_dedup$title) # 6 of 6 gold standard 
# articles are retrieved.
## External precision check.
sum(round(as.numeric(check_recall(ex_group_contact$title, it3_dedup$title)[, 3]), 0)) # 66 of 98 external 
# articles are retrieved.

#### Check third iteration search against the naive, first, and second iteration searches. 

### Against the naive search.
## Overlap with naive search. Here I do not use the "check_recall" function because it takes a long time, 
## and it is not exact / conservative enough, for the purpose of merging the naive search documents that are 
## not in the second iteration search. I do this to try and maximize coverage. 
overlap_naive_it3 <- bind_rows(naive_dedup, it3_dedup) # Merge corpora. Should be 2,043 + 19,437 = 21,480. 
## Exact matching.
length(overlap_naive_it3$title) # Input is 21,480 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_naive_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_naive_it3$title, exact_duplicates) # Perform a manual 
# check.
length(exact_manual$title) # Sum of 3,904 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 1,952 documents should be removed.
it3_naive_dedup_out <- synthesisr::extract_unique_references(overlap_naive_it3, exact_duplicates) 
length(it3_naive_dedup_out$title) # Output after exact matching is 19,528. 1,952 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it3_naive_dedup_out$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it3_naive_dedup_out$title, fuzzy_duplicates)) # Perform a manual check.
# 12503 and 19501 are duplicates.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(12503, 19501)) 
sum(table(fuzzy_duplicates) - 1) # 22 documents should be removed.
it3_naive_dedup_out <- extract_unique_references(it3_naive_dedup_out, fuzzy_duplicates) # Extract unique 
# references.
length(it3_naive_dedup_out$title) # De-duplicated output is 19,506. 22 documents were removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3")
write_refs(it3_naive_dedup_out, format = "ris", tag_naming = "synthesisr", 
           file = "it3_naive_dedup_out_refman")
write_refs(it3_naive_dedup_out, format = "bib", tag_naming = "synthesisr", 
           file = "it3_naive_dedup_out_refman")
# EndNote and Zotero indicate that  duplicates remain in the .ris file, which are removed. The result is 
# exported from Zotero in the file "it3_dedup.ris" which is subsequently imported. 
it3_naive_dedup_out <- read_bibliography("it3_naive_dedup_out.ris")
length(it3_naive_dedup_out$title) # 17,667 documents. 8 documents removed. 

## Coverage gain relative to the naive search.  
length(naive_dedup$title) # 2,043 articles.
length(it3_dedup$title) # 19,437 articles.
(length(it3_dedup$title) - length(naive_dedup$title)) # 17,394 additional articles.
(length(it3_naive_dedup_out$title) - length(it3_dedup$title)) # 64 articles from  naive not in third 
# iteration. This amounts to 17,394 + 64  = 17,458 or 17,458 / 2,043 = 8.55 as a factor increase.
1 - (length(it3_naive_dedup_out$title) - length(it3_dedup$title)) / length(naive_dedup$title) # 96.87% of the 
# naive search was in the third iteration search. 

### Against the first iteration. Note that the naive search articles are also in the first iteration 
### output object. 
## Overlap with first iteration search. Here I do not use the "check_recall" function because it takes a 
## long time, and it is not exact / conservative enough, for the purpose of merging the first iteration 
## search documents that are not in the second iteration search. I do this to try and maximize coverage. 
length(it3_naive_dedup_out$title)
overlap_it1_it3 <- bind_rows(it1_dedup_out, it3_naive_dedup_out) # Merge corpora. Should be 6,410 + 19,501 = 
# 25,911.
## Exact matching.
length(overlap_it1_it3$title) # Input is 25,911 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_it1_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_it1_it3$title, exact_duplicates) # Perform a manual 
# check.
length(exact_manual$title) # Sum of 10,956 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 5,478 documents should be removed.
it3_it1_dedup_out <- synthesisr::extract_unique_references(overlap_it1_it3, exact_duplicates) 
length(it3_it1_dedup_out$title) # Output after exact matching is 20,433. 5,478 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it3_it1_dedup_out$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it3_it1_dedup_out$title, fuzzy_duplicates)) # Perform a manual check.
# 5071 and 14483 are not duplicate combinations, remaining ones are.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(5071, 14483)) 
sum(table(fuzzy_duplicates) - 1) # 62 document should be removed.
it3_it1_dedup_out <- extract_unique_references(it3_it1_dedup_out, fuzzy_duplicates) # Extract unique 
# references.
length(it3_it1_dedup_out$title) # De-duplicated output is 20,371. 62 documents were removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3")
write_refs(it3_it1_dedup_out, format = "ris", tag_naming = "synthesisr", file = "it3_it1_dedup_out_refman")
write_refs(it3_it1_dedup_out, format = "bib", tag_naming = "synthesisr", file = "it3_it1_dedup_out_refman")
# EndNote and Zotero indicate that 4 duplicates remain in the .ris file after the de-duplication procedure. 
# Two retracted documents are additionally identified: "Ethnic threat and social control: Examining public 
# support for judicial use of ethnicity in punishment" (2) and "Structural stigma and all-cause mortality in 
# sexual minority populations" (2). After manually removing the duplicate documents from the 
# "it3_it1_dedup_out.ris" in EndNote, I overwrite the original  "it3_it1_dedup_out.ris" file with it, which I 
# then import. 
it3_it1_dedup_out <- read_bibliography("it3_it1_dedup_out.ris")
length(it3_it1_dedup_out$title) # 20,363 documents. 8 documents removed.

## Coverage gain relative to the first iteration search.  
length(it1_dedup_out$title) # 6,410 articles.
length(it3_naive_dedup_out$title) # 19,501 articles.
(length(it3_naive_dedup_out$title) - length(it1_dedup_out$title)) # 13,091 additional articles.
(length(it3_it1_dedup_out$title) - length(it3_naive_dedup_out$title)) # 862 articles from first not in third 
# iteration. This amounts to 682 + 13,091 = 13,773 or (13,773 / 6,410) = 2.15 as a factor increase.
1 - (length(it3_it1_dedup_out$title) - length(it3_naive_dedup_out$title)) / length(it1_dedup_out$title) # 
# 86.55% of the first iteration search was in the third iteration search. 

### Against the second iteration. Note that the first iteration search articles are also in the second 
### iteration output object. 
## Overlap with second iteration search. Here I do not use the "check_recall" function because it takes a 
## long time, and it is not exact / conservative enough, for the purpose of merging the second iteration 
## search documents that are not in the third iteration search. I do this to try and maximize coverage.
overlap_it2_it3 <- bind_rows(it2_dedup_out, it3_it1_dedup_out) # Merge corpora. Should be 15,417 + 20,363 = 
# 35,780.
## Exact matching.
length(overlap_it2_it3$title) # Input is 35,780 documents.
exact_duplicates <- synthesisr::find_duplicates(
  overlap_it2_it3$title, # Raw import as input;
  method = "exact", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE) # remove punctuation from input   
exact_manual <- synthesisr::review_duplicates(overlap_it2_it3$title, exact_duplicates) # Perform a manual 
# check.
length(exact_manual$title) # Sum of 29,562 duplicate combinations identified.
sum(as.numeric(table(exact_duplicates) - 1)) # 14,781 documents should be removed.
it3_dedup_out <- synthesisr::extract_unique_references(overlap_it2_it3, exact_duplicates) 
length(it3_dedup_out$title) # Output after exact matching is 20,999. 14,781 documents removed. 
## Fuzzy matching five.
fuzzy_duplicates <- find_duplicates(
  it3_dedup_out$title, # Exact match documents as input;
  method = "string_osa", # method is optimal string alignment (Damerau-Levenshtein distance); 
  to_lower = TRUE, # convert input to lower case;
  rm_punctuation = TRUE, # remove punctuation from input;
  threshold = 5) # default cutoff. 
(fuzzy_manual <- review_duplicates(it3_dedup_out$title, fuzzy_duplicates)) # Perform a manual check.
length(fuzzy_manual$title) # 74 potential duplicate combinations. 5700 and 18823 are not duplicate 
# combinations.
fuzzy_duplicates <- synthesisr::override_duplicates(fuzzy_duplicates, c(5700, 18823)) 
sum(table(fuzzy_duplicates) - 1) # 35 documents should be removed.
it3_dedup_out <- extract_unique_references(it3_dedup_out, fuzzy_duplicates) # Extract unique references.
length(it3_dedup_out$title) # De-duplicated output is 20,964. 35 documents were removed.
## Save .ris file of the merged file. 
setwd("C:/Academia/PhD/Meta-analysis paper/Literature data/2. Group contact/5. Iteration 3")
write_refs(it3_dedup_out, format = "ris", tag_naming = "synthesisr", file = "it3_dedup_out_refman")
write_refs(it3_dedup_out, format = "bib", tag_naming = "synthesisr", file = "it3_dedup_out_refman")
# EndNote and Zotero indicate that 11 duplicates remain in the .ris file after the de-duplication procedure. 
# These documents were removed manually for a final sum total of 20,955 documents. After manually removing 
# the duplicate documents from the "it3_dedup_out.ris" in EndNote, I overwrite the original 
# "it3_dedup_out.ris" file with it, which I then import. 
it3_dedup_out <- read_bibliography("it3_dedup_out.ris")
length(it3_dedup_out$title) # 20,955 documents.

### Merged corpus precision diagnostic. For convenience I assume that similarity scores > 0.5 are a match, 
## and those =< 0.5 are not.
## Gold standard precision check.
check_recall(gs_group_contact$title, it3_dedup_out$title) # 6 of 6 gold standard articles are retrieved.
## External precision check.
check_recall(ex_group_contact$title, it3_dedup_out$title) # 69 of 98 or 70,41% of external articles are 
# retrieved. 

## Coverage gain relative to the second iteration search.  
length(it2_dedup_out$title) # 15,417 articles.
length(it3_dedup_out$title) # 20,955 articles.
(length(it3_dedup_out$title) - length(it2_dedup_out$title)) # 5,538 additional articles.
(length(it3_dedup_out$title) - length(it3_it1_dedup_out$title)) # 592 articles from second not in third 
# iteration. This amounts to 5,538 + 592 = 6,130 or (6,130 / 15,417) = 0.40 as a factor increase.
1 - (length(it3_dedup_out$title) - length(it3_it1_dedup_out$title)) / length(it2_dedup_out$title) # 96.16% of 
# the second iteration search was in the third iteration search. 

### Approximately 97% of the naive, 87% of the first, and 96% of the second iteration search is contained in 
### the third iteration search. The coverage increase between the second and third iteration is 40%. The 
### external precision has however barely increased between the second and third iterations. Based on this 
### observation and the adequate overlap values, I do not continue with a fourth iteration, even though the 
### coverage increase was relatively high, which I surmise to be mostly non-relevant additions.

###########################
##### ASReview export #####
###########################

### Export the final document set as input for screening in ASReview.
## Data frame of the title, abstract, author, and year of publication. 
asreview_group_contact <- as.data.frame(cbind(it3_dedup_out$title, it3_dedup_out$abstract, 
                                              it3_dedup_out$author, it3_dedup_out$year))
## Assign column names.
colnames(asreview_group_contact) <- c("Title", "Abstract", "Author", "Year")
## Final number of articles.
length(asreview_group_contact$Title) # 20,955 candidate articles for the group threat determinant. 
## Write result as a .csv file.
write.csv(asreview_group_contact, paste0("C:/Academia/PhD/Meta-analysis paper/Literature data/2. G", 
                                         "roup contact/6. ASReview/ASReview_input_group_contact.csv"))

## Note that the .csv file is cleaned one more time before entering it into ASReview. We furthermore do not
## address missing values.

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

