#Load Packages
library(janitor)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

#READ IN THE DATA 


#Create and Format the Relative Frequency Table
armyTable <- key_individualRanks %>%
  tabyl(Rank, Branch) %>% #creates a base frequency table with rank as rows and Branch as columns) 
  adorn_totals(where = c("row", "col") ) %>% #add marginal totals row and column
  adorn_percentages(denominator = "all") %>% #create relative frequencies by dividing by the total
  adorn_pct_formatting(digits = 2) %>% #Set relative frequencies to be reported to two decimals and as a percentage
  adorn_title( #Turn the label for the first column into a title for both the rows (Rank) and columns (Branch)
    placement = "combined", #Places the title as the name of the first column
    row_name = "Rank",
    col_name = "Branch")

#Create and Format an Absolute Frequencies Table
formatNs <- attr(armyTable, "core")  %>% #Recreates the base frequency table
  adorn_totals(where = c("row", "col")) %>% #adds marginal totals again
  mutate( #Adds commas where necessary in numbers greater than 999
    across(where(is.numeric), format, big.mark = ",")
  )

#Combine the absolute and relative frequencies
armyFreqTable <- armyTable %>% #Combines the relative and absolute frequency tables
  adorn_ns(position = "front", ns = formatNs) #position determines whether the counts are; in our case we want the counts in front followed by the percentages
  
#Polish the Table
prettyarmyFreqTab <- armyFreqTable %>%
  kable(
    caption = "Rank and Branch of Soldiers", #Add title of table
    booktabs = TRUE,
    align = c("l", rep("c", 4)) #Specify by column whether cells should be left/center/right aligned; This says left align for all 6 columns
  ) %>%
  kableExtra::kable_styling( 
    bootstrap_options = c("striped", "condensed"), #This argument can take multiple values to format the table. "striped" adds shading for every other row; "condensed" reduces the extra space in each cell. You can also add boundaries using this argument
    font_size = 16 #choosing font size for each entry
  )
print(prettyarmyFreqTab) #We have to use the print function instead of View() for any stylized tables or plots

