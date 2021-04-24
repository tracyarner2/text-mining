Lab 3.1 - Tidy Text & Word Counts
========================================================
author: The LASER Team
date: April 24, 2021
autosize: false


Agenda
========================================================
- The Context
- The Terminology
- The Literature
- The Lab


THE CONTEXT
========================================================
type: section

## - test 



www.world_wide_web_of_words
========================================================
World Wide Web of Words  
new sources of data 
- news sites
- blogs
- **social media** 

--- 
image of growth in data



Promise and Pitfalls of Social Media Data
========================================================
### Strengths
- Abundant
- Absent of Researcher Influence
- Relationships 

---

### Weaknesses
- Non-representative
- Positivity Bias
- Private or Senstive
- Unstructured


Getting Data From the Web
========================================================
## APIs
- df  

--- 

## Screen Scraping


Preparing Text Data for Analysis
========================================================
- 


THE TERMINOLOGY
========================================================
type: section

## - APIs
## - Tidy Text
## - Tokenization
## - n-grams



APIs
========================================================
test


Tidy Text
========================================================
test

THE LITERATURE
========================================================
type: section

Advancing new methods for understanding public sentiment about educational reforms:   

The case of Twitter and the Next Generation Science Standards.

citation  
[link to article](go.ncsu.edu)

<!--
Today's learning labs are guided by my colleague Josh Rosenberg’s recent article, Advancing new methods for understanding public sentiment about educational reforms: The case of Twitter and the Next Generation Science Standards. We will focus on conducting a very simplistic “replication study” by comparing the sentiment of tweets about the Next Generation Science Standards (NGSS) and Common Core State Standards (CCSS) in order to better understand public reaction to these two curriculum reform efforts. I highly recommend you watch the quick 3-minute overview of this work at https://stanford.app.box.com/s/i5ixkj2b8dyy8q5j9o5ww4nafznb497x
-->


Questions Explored 
==================


Questions Explored 
========================================================
incremental: true 

1.  What is the public sentiment expressed toward the NGSS?
2.  How does sentiment for teachers differ from non-teachers?
3.  How do tweets posted to \#NGSSchat differ from those without the hashtag?
4.  How does participation in \#NGSSchat relate to the public sentiment individuals express?
5.  How does public sentiment vary over time?

<!--
While the Next Generation Science Standards (NGSS) are a long-standing and widespread standards-based educational reform effort, they have received less public attention, and no studies have explored the sentiment of the views of multiple stakeholders toward them. To establish how public sentiment about this reform might be similar to or different from past efforts, we applied a suite of data science techniques to posts about the standards on Twitter from 2010-2020 (N = 571,378) from 87,719 users. Applying data science techniques to identify teachers and to estimate tweet sentiment, we found that the public sentiment towards the NGSS is overwhelmingly positive---33 times more so than for the CCSS. Mixed effects models indicated that sentiment became more positive over time and that teachers, in particular, showed a more positive sentiment towards the NGSS. We discuss implications for educational reform efforts and the use of data science methods for understanding their implementation.
-->

Data Sources
========================================================




<!-- Similar to what we'll be learning in this walkthrough, Rosenberg et al. used publicly accessible data from Twitter collected using the Full-Archive Twitter API and the `rtweet` package in R. Specifically, the authors accessed tweets and user information from the hashtag-based \#NGSSchat online community, all tweets that included any of the following phrases, with "/" indicating an additional phrase featuring the respective plural form: "ngss", "next generation science standard/s", "next gen science standard/s". -->

Methods
========================================================




<!-- Unlike this walkthrough, however, the authors determined Tweet sentiment using the Java version of SentiStrength to assign tweets to two 5-point scales of sentiment, one for positivity and one for negativity, because SentiStrength is a validated measure for sentiment in short informal texts (Thelwall et al., 2011). In addition, we used this tool because Wang and Fikis (2019) used it to explore the sentiment of CCSS-related posts. We'll be using the AFINN sentiment lexicon which also assigns words in a tweet to two 5-point scales, in addition to explore some other sentiment lexicons.
-->

Findings
========================================================
incremental: true 

1. In contrast to CSSS, sentiment about the NGSS science education reform effort is overwhelmingly positive.
2. Teachers were more positive than non-teachers, and sentiment became substantially more positive over time.
3. Tweets including #NGSSchat that were posted outside of chats were slightly more negative than posts about the NGSS that did not include the #NGSSchat hashtag.
4. Individuals posted more tweets during \#NGSSchat chats and sentiment was more positive  
5. While the context of individual tweets has a small effect, the effect upon individuals of being involved in the \#NGSSchat was positive.


THE LEARNING LAB
========================================================
type: section
## - Twitter API
## - Text Preprocessing
## - 




