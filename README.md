# UK Further Education Funding Allocations 2019-2020
Brief analysis of FE funding allocations for 2019-2020

## Data sources

FE-Allocations-2019-2020.csv is a copy of the 'Allocations' tab extracted from the ODS file at https://www.gov.uk/government/publications/16-to-19-allocation-data-2019-to-2020-academic-year. See the notes section below for more information about the data in this file.

LA-Political-Control-2019.csv is scraped from the Wikipedia page at https://en.wikipedia.org/wiki/Political_make-up_of_local_councils_in_the_United_Kingdom (with minimal changes to align LA naming).

LA-Geography-2019.csv is taken from the data at https://geoportal.statistics.gov.uk/datasets/bbb0e58b0be64cc1a1460aa69e33678f_0?geometry=-8.629%2C50.184%2C3.637%2C51.399 (with minimal changes to align LA naming). Note that this file can not be trivially used with the other two because it provides data for Local Authority Districts, rather than for Local Authorities (and I don't have a mapping between the two).

## Analysis

See analyse.R for an R script to clean and analyse the data. This script reads the 'funding' and 'political control' files above into 2 data frames (df and pcdf) and uses them to show average funding per student by local authority political control:

| LA Control | Number of LAs | Average Funding per Student |
| --- |:---:|:---:|
| LAB | 961	| £5516 |
| NOC | 468	| £5391 |
| LD | 59	| £5363 |
| CON | 1330 | £5238 |

That's about a 5% difference in funding per student between the lowest-funded areas and the highest-funded areas (on average). Actually, if we look at average funding per student by region:
	
Region | Count | Average Funding Per Student
| --- |:---:|:---:|
GL | 501 | £5616 |
NE | 132 | £5564 |
NW | 302 | £5532 |
SW | 286 | £5428 |
SE | 460 | £5330 |
YH | 236 | £5302 |
WM | 324 | £5217 |
EM | 267 | £5173 |
EE | 310 | £5039 |

there is a regional differential of about 11% between the lowest-funded region and the highest-funded region (on average).

Looking at funding for free meals, which we might assume to be indicative of social deprevation, we see:

| LA Control | Average Free Meals Funding per Student |
| --- |:---:|
| LAB | £14 |
| NOC | £12 |
| CON | £8 |
| LD | £5 |

This would appear to indicate that social deprevation is significantly worse in Labour controlled areas than it is in Conservative or Liberal Democrat. No big surprises there I guess.

## Education & Skills Funding Agency notes

These notes are taken from the ODS file at https://www.gov.uk/government/publications/16-to-19-allocation-data-2019-to-2020-academic-year with some information deleted because it is not relevant to the data provided here.

<i>
  
### 16-19 Allocations for the 2019 to 2020 academic year

This publication includes allocation data for institutions funded by the Education and Skills Funding Agency (ESFA) for the 2019 to 2020 academic year.

You will find the following areas of provision and allocations included in this workbook:

- 16-19 mainstream funding in further education (FE)

- School sixth-forms including academies

We provide funding for education and training for students up to the age of 19, or up to the age of 25 for those young people who have been issued an education, health and care (EHC) plan by their local authority. Students aged 19 to 25 with an EHC plan are reported alongside other students funded through the 16 to 19 budget. 19+ continuing learners are also included, as are a small number of non-formula funded institutions.

Students aged 16 to 19 undertaking an apprenticeship and most students aged 19 and over (other than those stated above) are not included here. Details of those allocations are published separately on gov.uk.

The 16-19 funding formula was significantly changed in the 2013 to 2014 academic year and so the figures published here are not comparable with similar publications preceding that year.

New for this year is the inclusion of advanced maths premium and alternative completions funding. More details of this funding is given on gov.uk

The figures and institution details shown within this publication are as at 28 September 2019. Any changes after this date, including but not limited to schools becoming academies, contract variations, institution mergers, name/UPIN changes etc. will not be updated in this published version of the allocations.

The 2019 to 2020 academic year covers 1 September 2019 through 31 August 2020 for academies and 1 August 2019 through 31 July 2020 for all other institutions

### Allocations sheet worksheet

This sheet has one line for each institution to display the core elements of allocated funding. The sheet has been sorted in ascending alphabetical order by institution name.

Column 'F' shows the institution's UK Provider Reference Number (UKPRN) as registered with the UK Register of Learning Providers (UKRLP) and has been included so that additional institution details can be found more easily in online databases such as UKRLP and Get Information About School (GIAS). Column 'G' shows the institutions UPIN which is the Unique Provider Identification Number (UPIN) used within the ESFA to distinguish individual institutions.

Column 'H' shows the total number of students, this includes all programme funded and high needs students (the number of whom are  shown separately in column 'I'). Column ‘J’ shows the total programme funding allocated to the institution which is calculated using the published formula.  Column ‘K’ shows how much of the total programme funding (‘J’) relates to disadvantage funding. Column 'L' shows the number of students that do not comply with the condition of funding for maths and English. Column 'M' shows the adjustment made that is attributable to the condition of funding policy after mitigation. 

Column ‘N’ shows how much high needs funding has been allocated with £6,000 per high needs student being represented in column 'N' and the programme funding (having been processed through the formula) being represented within column 'J'. For special schools and special academies, there is no programme funding and the high needs funding is £10,000 per place. Column ‘O’ shows the total student financial support funding, including discretionary bursary funding, free meals and residential funding. Discretionary bursary funding is shown in column 'P’ and free meals allocations are shown in column 'Q'. Industry placements capacity and delivery funding is shown in column 'R'. Advanced maths premium funding is shown in column 'S'. Alternative completions funding is shown in column 'T'.

The overall total funding allocation, including residential schemes, but excluding DaDA and bursary funding for maintained special schools (see separate worksheets) is shown in Column 'U'. 

High needs funding for post 16 places in maintained special schools and maintained schools with sixth forms, is not included within this publication as it is paid to local authorities in the Dedicated Schools Grant.  
</i>
