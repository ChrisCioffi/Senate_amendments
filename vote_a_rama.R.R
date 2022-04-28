library(tidyverse)

#this data is located here: https://www.ou.edu/carlalbertcenter/research/pipc-votes -- it was downloaded on April 21
#Thanks to Sarah Binder for tipping me off to it, It's a collection of ALL Senate votes back to 1969!
#before reading in, I highlighted all columns and then deselected the ones with data before deleting the whitespace, to make sure there were no extra columns when read into R

#tried to go through and skip the columns i don't need and force the other columns to the type I'd like them to be.
senate_votes <- read_csv("senate_votes_new.csv", col_types = list( cong = col_integer(), year = col_integer(), vote = col_integer() ,question = col_character(), url = col_character(), amendment = col_character(),bill_title = col_character(), rnotvoting =  col_number(), billnum1 = col_skip(), dnotvoting = col_number(), amendment2 = col_character(), amendment3 = col_character(), dpresent =col_skip(), rpresent = col_skip(), iyeas = col_skip(), inays = col_skip(), inotvoting = col_skip(), ipresent = col_skip(), vpvote = col_skip()))

#, , rnotvoting = col_character, billnum1 = col_character()

#21 through 29 (except for 24 — those are motions to table amendments) — you’re going to catch all the amendment votes (in the “vote” column)
#then collapse by year — aka — group and count

#Amendment votes by year - let's look at the total number of amendment votes by year which requires us to filter out the votes we want, group them by year, and then count the number of amendment votes.

#So using the codebook provided with the data (https://www.ou.edu/content/dam/carlalbertcenter/documents/pipc-senate-codebook.pdf) We're going to want to focus on a couple of rows. 1 the "vote row" that describes what kind of vote it is. And, within that, we're going to focus on codes 21 - Straight Amendments (includes en bloc & amendments in the nature of a substitute), 22 Amendments to Amendments, 23 Substitute (to an amendment), 25 Amendment to Amendment to Substitute, 26 Perfecting Amendment, 27 Amendment to Substitute, 28 Perfecting Amendment to Substitute, 29 Suspension of Rules to Amend Bill But NOT 24 Motion to Table Amendment


#first let's filter out just our amendment votes https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e
amendment_votes <- senate_votes %>% 
  filter(vote %in% c(21, 22, 23, 25, 26, 27, 28, 29)) 
#now let's group and count the number of amendment votes by congress
amendment_total<- amendment_votes %>%
  group_by(cong) %>%
  summarize (count_total_amend_votes = n())

#brought up by Ryan Kelly- let's also look at the total number of bills that got votes on final passage. And we're taking out motion to table amendment votes in this data, using the inverse of in.

#https://www.r-bloggers.com/2018/07/the-notin-operator/
`%notin%` <- Negate(`%in%`)

all_bill_votes <- senate_votes %>% 
  filter(vote %notin% c(24))
#now let's group and count the number of total votes
bill_passage <- all_bill_votes %>%
  group_by(cong) %>%
  summarize (count_votes_no_table = n())
#now I just want the group by congress
bill_passage_by_cong <- senate_votes %>%
  group_by(cong) %>%
  summarize (count_all_votes = n())

# let's also group and count the number of bills that got amendment votes, and group by congress
amendment_bill <- amendment_votes %>%
  group_by(bill1, cong) %>%
  summarize (count_bills_amend_votes = n())

#We should also go ahead and do a group/count of bills that got amendment votes in each congress
amendment_cong <- amendment_bill %>%
  group_by(cong) %>%
  summarize (count_amend_bills_by_congress = n())

#and what if we dropped drop any bills that got less than 5 votes 
multi_bill_vote <- amendment_bill %>%
  filter(count_bills_amend_votes > 5) %>%
  group_by(cong) %>%
  summarize(more_than_five_amend_votes = n())
  

#let's make them in to one tidy tibble using reduce https://datacornering.com/multiple-left-joins-in-r/
# more_than_five_amend_votes is a count of bills with more than 5 amendment votes
# count_amend_bills_by_congress a group/count of bills that got amendment votes in each congress
# count_total_amend_votes a group/count the number of amendment votes by congress
#count_all_bills is the total bills that got a vote on final passage.
joined <- list(multi_bill_vote, amendment_cong, amendment_total, bill_passage, bill_passage_by_cong) %>% 
  reduce(left_join, by = "cong")


#Write csvs for my findings:

write_csv(bill_vote, "number_bills_with_with_amend_votes_by_congress.csv")
write_csv(multi_bill_vote, "number_bills_over_5_amendment_votes_by_congress.csv")
write_csv(amendment_votes, "all_votes_on_amendments.csv")

write_csv(joined, "findings_on_amendment_votes.csv")



# These are exploratory graphics ------ I didn't delete them. ------


# so some percentages were above 1. Is this a mistake on my part, or something else?


# Basic scatter plot of total amendments by congress // Binder's was by year 
ggplot(amendment_total, aes(x=cong, y=count_total_amend_votes)) + 
  geom_point() +
  stat_smooth()


# Basic scatter plot looking at the number of bills that got more than 5 amendment votes
ggplot(multi_bill_vote, aes(x=cong, y=multi_count)) + 
  geom_point() +
  stat_smooth()

#plot, by year, the number of bills that got more than 5 votes
ggplot(data = multi_bill_vote, aes(x=cong, y=multi_count)) + 
  geom_bar(stat="identity") 

