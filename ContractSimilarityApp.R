library(nflreadr)
library(ggplot2)
library(ggimage)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(gt)
library(paletteer)
library(webshot)
library(ggthemes)
library(readr)
library(ggtext)
library(ggforce)
library(stats)
library(mclust)
library(gghighlight)
library(na.tools)
library(magick)
library(vip)
library(gtExtras)
library(nflfastR)
library(nflplotR)
library(shiny)
library(shinythemes)
library(stringi)
library(reactable)
library(reactablefmtr)
library(bslib)
library(shinyjs)
library(shinyWidgets)
library(rsconnect)
library(data.table)
library(shinycssloaders)
library(future.apply)

rosters_2024 <- load_rosters(2024) |> select(full_name, headshot_url) |> mutate(full_name = clean_player_names(full_name))

pff_top_150 <- read.csv("pffT150FAs.csv")

OL_Matched <- read.csv("OLMatched.csv") |> select(-X)
OL_ID <- read.csv("OLID.csv") |> select(-X) |> 
  left_join(pff_top_150 |> summarize(Player_Source = clean_player_names(g.card__content), rank = g.label), by = "Player_Source") |> 
  filter(!is.na(year)) |> 
  mutate(rank = case_when(
    Player_Source == "Joe Noteboom" ~ 160,
    Player_Source == "Trent Brown" ~ 153,
    Player_Source == "George Fant" ~ 170,
    Player_Source == "Aaron Banks" ~ 35,
    Player_Source == "Kelvin Beachum" ~ 180,
    Player_Source == "Ben Bredeson" ~ 190,
    Player_Source == "Greg Van Roten" ~ 200,
    Player_Source == "Robert Jones" ~ 210,
    Player_Source == "Bradley Bozeman" ~ 220,
    Player_Source == "Liam Eichenberg" ~ 230,
    Player_Source == "Dillon Radunz" ~ 240,
    Player_Source == "Jaylon Moore" ~ 250,
    Player_Source == "Cody Ford" ~ 260,
    Player_Source == "Daniel Brunskill" ~ 270,
    TRUE ~ rank
  )) |> 
  filter(!is.na(rank)) |> 
  arrange(rank)  |> 
  select(-rank) |> 
  mutate(contract_prediction = case_when(
    Player_Source == "Trey Smith" ~ "4 Years $86M ($22.5M), ~$60M Guaranteed",
    Player_Source == "Cam Robinson" ~ "3 Years $48M ($16M), ~$30M Guaranteed",
    Player_Source == "Drew Dalman" ~ "4 Years $52M ($13M), ~$30M Guaranteed",
    Player_Source == "Will Fries" ~ "4 Years $54M ($13.5M), ~$25M Guaranteed",
    Player_Source == "Teven Jenkins" ~ "4 Years $60M ($15M), ~$38M Guaranteed",
    Player_Source == "Aaron Banks" ~ "3 Years $36M ($12M), ~$28M Guaranteed",
    Player_Source == "Kevin Zeitler" ~ "1 Year $7.5M ($7.5M), ~$7.5M Guaranteed",
    Player_Source == "James Daniels" ~ "3 Years $39M ($13M), ~$20M Guaranteed",
    Player_Source == "Tyron Smith" ~ "1 Year $7M ($7M), ~$7M Guaranteed",
    Player_Source == "Mekhi Becton" ~ "4 Years $48M ($12M), ~$35M Guaranteed",
    Player_Source == "Dan Moore" ~ "3 Years $41M ($13.66M), ~$27M Guaranteed",
    Player_Source == "Morgan Moses" ~ "1 Years $8.5M ($8.5M), ~$7M Guaranteed",
  )) |> 
  mutate(prediction_explanation = case_when(
    Player_Source == "Morgan Moses" ~ "Moses is a bit on the older side, but he should be able to command right in the middle of the Reilly Reiff contracts 
    that take the top 2 similarity score spots. I'm not ruling out the possibility of a 2 year deal, but Moses is already 34.",
    
    Player_Source == "Dan Moore" ~ "The Chukwuma Okorafor (second matched) contract seems spot on for Moore, who played 
    a significant amount of snaps for the Steelers this year. Moore should get more in guarantees due to his steady play.",
    
    Player_Source == "Mekhi Becton" ~ "Becton's comparisons, really don't encapsulate his season last year. I think a better comparison
    is Cesar Ruiz's 2023 contract, which inflates to right around this Becton contract. Becton has only played a year at OG, but played well enough to garner 
    a 4 year deal while still 25.",
    
    Player_Source == "Tyron Smith" ~ "Smith signed a very much incentive-laden contract with the Jets last year. 
    There's a high probability he signs a similar-type contract this year. This prediction is just indicative of what his 'numbers' were last year.",
    
    
    Player_Source == "James Daniels" ~ "Daniels' predictions all seem really off from what he might actually earn. 
    They're either too high or too low, a product of his only 209 snaps played this year. His limited play time in what seemed 
    like what was shaping up to be a stellar season will most likely ding his APY and guaranteed money.",
    
    Player_Source == "Kevin Zeitler" ~ "Zeitler signed for $6M with the Lions last year. A $1.5M increase seems an apt movement with the salary cap increase. This slides in a little below Saffold's 2022 contract.",
    
    Player_Source == "Teven Jenkins" ~ "The Glasgow comparison seems apt for a Jenkins, who has been good ever since the former 2nd round pick made the switch to G.",
    
    Player_Source == "Trey Smith" ~ "Trey Smith becomes the highest paid guard in the league, 
    edging out Landon Dickerson by 500k APY and falling right around Wyatt Teller's 2021 contract
    inflated APY.",
    
    Player_Source == "Cam Robinson" ~ "Cam Robinson, at age 29, gets a 3 year deal that should potentially take him to his 
    final contract. After playing well post-Minnesota trade, Robinson locks in a multi-year deal that resembles his 
    third most similar contract. His guarantees, however, are more along the lines of a cap-adjusted 2022 Joe Noteboom.",
    
    Player_Source == "Drew Dalman" ~ "Dalman comes in right above the Lloyd Cushenberry III contract from last year.
    He gets a little bit less than the cap-adjusted numbers for Cushenberry, but locks in a lucrative 2nd contract until age 30.",
    
    Player_Source == "Will Fries" ~ "None of Fries' matches particularly stand out to me, most likely a product of him playing well and then
    getting injured early last season. He has a few established years of production, so I could see a Ben Powers/Damien Lewis type contract 
    without major cap adjustments due to injury concerns."
  )) |> 
  mutate(contract = case_when(
    Player_Source == "Ronnie Stanley" ~ "3 Years $60M, $44M Guaranteed",
  )) |> 
  mutate(contract_explanation = case_when(
    Player_Source == "Ronnie Stanley" ~ "Ronnie Stanley gets exactly the highest similarity
    score predicted contract in Taylor Decker's 2024 numbers. In this case, he trades more guaranteed 
    at signing for a lower APY % of cap."
  ))


DL_Matched <- read.csv("DLMatched.csv") |> select(-X)
EDGE_Matched <- DL_Matched |> filter(position == "ED") 
IDL_Matched <- DL_Matched |> filter(position == "DI")


DL_ID <- read.csv("DLID.csv") |> select(-X)
EDGE_ID <- DL_ID |> filter(position == "ED") |> 
  left_join(pff_top_150 |> summarize(Player_Source = clean_player_names(g.card__content), rank = g.label), by = "Player_Source") |> 
  filter(!is.na(year)) |> 
  mutate(rank = case_when(
    Player_Source == "Joey Bosa" ~ 55,
    Player_Source == "Harold Landry" ~ 30,
    Player_Source == "DreMont Jones" ~ 70,
    Player_Source == "Preston Smith" ~ 180,
    Player_Source == "DeMarcus Walker" ~ 190,
    Player_Source == "Lorenzo Carter" ~ 200,
    Player_Source == "Clelin Ferrell" ~ 210,
    Player_Source == "Payton Turner" ~ 220,
    Player_Source == "Joe Tryon-Shoyinka" ~ 230,
    Player_Source == "Joseph Ossai" ~ 240,
    Player_Source == "KLavon Chaisson" ~ 250,
    Player_Source == "Anthony Nelson" ~ 260,
    Player_Source == "Al-Quadin Muhammad" ~ 270,
    Player_Source == "Deatrich Wise" ~ 280,
    TRUE ~ rank
  )) |> 
  filter(!is.na(rank)) |> 
  arrange(rank)  |> 
  select(-rank)|> 
  mutate(contract_prediction = case_when(
    Player_Source == "Khalil Mack" ~ "2 Years $50M ($25M), ~$40M Guaranteed",
    Player_Source == "Josh Sweat" ~ "3 Years $54M ($18M), ~$36M Guaranteed",
    Player_Source == "Malcolm Koonce" ~ "2 Years $32M ($16M), ~$20M Guaranteed",
    Player_Source == "Dayo Odeyingbo" ~ "3 Years $36M ($12M), ~$21M Guaranteed",
    Player_Source == "DeMarcus Lawrence" ~ "1 Year $10M ($10M), ~$10M Guaranteed",
    Player_Source == "Joey Bosa" ~ "1 Year $9M ($9M), ~$9M Guaranteed",
    Player_Source == "Azeez Ojulari" ~ "2 Years $14M ($7M), ~$7.5M Guaranteed",
  )) |> 
  mutate(prediction_explanation = case_when(
    Player_Source == "Azeez Ojulari" ~ "Ojulari should get a contract around what Baron Browning got and that Smoot prediction. 
    Ojulari should command a little lower than that, however because of his injury history. He hasn't played a full season since 2021.",
    
    Player_Source == "Joey Bosa" ~ "Bosa is an intruiging player because he really only played about an average of 30% of snaps over the past 
    3 years. I think because of this, he doesn't command as much money as many would think. The contract will be heavily incentive-laden.",
    
    Player_Source == "DeMarcus Lawrence" ~ "Lawrence seems like one of those players coming off of an injury that signs later in the offseason. 
    This'll probably come at a much cheaper number, but not a cheap as the model predicts (injury did a number on this one).",
    
    Player_Source == "Dayo Odeyingbo" ~ "Odeyingbo could have a very similar contract to Dorance Armstrong from last offseason. 
    This would be the analogue of that. It is also similar to the Nassib contract listed.",
    
    Player_Source == "Malcolm Koonce" ~ "Koonce missed all of last season, so these stats are from the 2023-24 seaosn. 
    Him not being able to build off of a stellar 2023 season due to a season-ending knee injury could hurt his value in terms of APY, but 
    it also motivates a 26 year old Koonce to take a short deal. I see the Davenport contract as extremely similar due to the injury histories of both players.",
    
    Player_Source == "Josh Sweat" ~ "Sweat has been very productive these past few seasons and should get something slightly above the Bryce Huff deal from last offseason. 
    There's a chance he comes in lower due to age, but none of the suggested contract comparisons stood out to me.",
    
    Player_Source == "Khalil Mack" ~ "Mack is already 34, but has been extremely productive these past few years. 
    He should command a deal similar to the highest similarity score Vonn Miller deal from 2022.",
  )) |> 
  mutate(contract = case_when(
    Player_Source == "Harold Landry" ~ "3 Years 43.5M ($14.5M), $26M Guaranteed",
    Player_Source == "Baron Browning" ~ "2 Years 15M ($7.5M)",
  )) |> 
  mutate(contract_explanation = case_when(
    Player_Source == "Harold Landry" ~ "Landry gets a decent amount of money despite being recently cut by the Titans after not being able to find a trade partner. 
    He comes in a little above the Quinn deal over a few years, which is where he probably should've been closer to.",
    
    Player_Source == "Baron Browning" ~ "Browning gets a contract almost identical to his 3rd ranked similar player, Dawuane Smoot, adjusted for the cap.",
  ))
  
  
IDL_ID <- DL_ID |> filter(position == "DI") |> 
  left_join(pff_top_150 |> summarize(Player_Source = clean_player_names(g.card__content), rank = g.label), by = "Player_Source") |> 
  filter(!is.na(year)) |> 
  mutate(rank = case_when(
    Player_Source == "Javon Hargrave" ~ 30,
    Player_Source == "Jonathan Allen" ~ 20,
    Player_Source == "Sheldon Rankins" ~ 100,
    Player_Source == "Javon Kinlaw" ~ 180,
    Player_Source == "Poona Ford" ~ 50,
    Player_Source == "Roy Robertson-Harris" ~ 100,
    Player_Source == "Raekwon Davis" ~ 200,
    Player_Source == "Folorunso Fatukasi" ~ 130,
    Player_Source == "Morgan Fox" ~ 220,
    Player_Source == "Solomon Thomas" ~ 230,
    Player_Source == "Jerry Tillery" ~ 240,
    Player_Source == "Jonathan Bullard" ~ 250,
    Player_Source == "Daniel Ekuale" ~ 260,
    Player_Source == "LJ Collier" ~ 270,
    Player_Source == "Bobby Brown" ~ 280,
    TRUE ~ rank
  )) |> 
  filter(!is.na(rank)) |> 
  arrange(rank)  |> 
  select(-rank)|> 
  mutate(contract_prediction = case_when(
    Player_Source == "Milton Williams" ~ "4 Years $84M ($21M), ~$54M Guaranteed",
    Player_Source == "Jonathan Allen" ~ "3 Years $45M ($15M), ~$30M Guaranteed",
    Player_Source == "Javon Hargrave" ~ "1 Year $10M ($10M), ~$10M Guaranteed",
    Player_Source == "Levi Onwuzurike" ~ "3 Year $39M ($13M), ~$19M Guaranteed",
    Player_Source == "BJ Hill" ~ "2 Year $20M ($10M), ~$12.5M Guaranteed",
    Player_Source == "Calais Campbell" ~ "1 Year $7M ($7M), ~$7M Guaranteed",
    Player_Source == "Poona Ford" ~ "3 Years $33M ($11M), ~$20M Guaranteed",
    Player_Source == "Tershawn Wharton" ~ "2 Years $18M ($9M), ~$10M Guaranteed",
  )) |> 
  mutate(prediction_explanation = case_when(
    Player_Source == "Tershawn Wharton" ~ "Wharton was very servicible player for KC in all 5 of his years there and is only 26. Not entirely sure right not whether he'll command a 2 or 3 year deal. Right now, none of the comparisons stick out to me.",
    
    Player_Source == "Poona Ford" ~ "Ford, despite journeying throughout the NFL before settling in LA, had a great season for the Chargers in 2024. 
    He's already 29 and should get in a touch below Shelby Harris. The numbers for Larry Ogunjobi's contract in 2023 extrapolated to today's salary cap should be around the range for Ford.",
    
    Player_Source == "Calais Campbell" ~ "Campbell is already 38 and if he decides to not retire, he should garner a contract around the amount he's been getting (~$6M), but cap adjusted.",
    
    Player_Source == "BJ Hill" ~ "B.J. Hill should get around what bigger DTs have gotten throughout the past few years on 2 year deals (cap adjusted, of course).",
    
    Player_Source == "Levi Onwuzurike" ~ "Think B.J. Hill's contract in 2022 does a good job predicting what Onwuzurike 
    will command on the open market. Even Henry Anderson's contract does the same. For a player who finally completeled a full season with Detroit and played well doing so.",
    
    Player_Source == "Javon Hargrave" ~ "This is another example of an injury not doing a player justice. I really don't know how the league is going to view a formerly dominant Hargrave 
    who has slowed down and suffered an injury. Think he could get an incentive-laden contract.",
    
    Player_Source == "Jonathan Allen" ~ "Allen should probably get something similar to Arik Armstead, who was cut last year 
    by the 49ers in a similar fashion. Harold Landry already displayed that this type of contract was a possibility. None of the predictions do him justice because of the injury last year.",
    
    Player_Source == "Milton Williams" ~ "Building off of the Osa Odighizua contract, Williams should command around $21M a year,
    similar to the inflated Ed Oliver contract. Overall, the contract should be a smidgeon-step above Osa's.",
  )) |> 
  mutate(contract = case_when(
    Player_Source == "Jarran Reed" ~ "3 Years 22M ($7.33M), $10M Guaranteed",
    Player_Source == "DJ Jones" ~ "($13M per year)",
  )) |> 
  mutate(contract_explanation = case_when(
    Player_Source == "DJ Jones" ~ "Jones eclipsed every prediction that was assigned to him. He signs what could be the analogue to his 2022 three year $30M contract.",
    Player_Source == "Jarran Reed" ~ "With a max value of $25M, Reed gets a contract very similar to Gerald McCoy in 2022.",
  ))



LB_Matched <- read.csv("LBMatched.csv") |> select(-X)
LB_ID <- read.csv("LBID.csv") |> select(-X) |> 
  left_join(pff_top_150 |> summarize(Player_Source = clean_player_names(g.card__content), rank = g.label), by = "Player_Source") |> 
  filter(!is.na(year)) |> 
  mutate(rank = case_when(
    Player_Source == "Ernest Jones" ~ 70,
    Player_Source == "EJ Speed" ~ 100,
    Player_Source == "DeVondre Campbell" ~ 180,
    Player_Source == "Isaiah Simmons" ~ 200,
    Player_Source == "Akeem Davis-Gaither" ~ 220,
    Player_Source == "Christian Rozeboom" ~ 90,
    Player_Source == "Divine Deablo" ~ 270,
    Player_Source == "KJ Britt" ~ 280,
    TRUE ~ rank
  )) |> 
  filter(!is.na(rank)) |> 
  arrange(rank)  |> 
  select(-rank)|> 
  mutate(contract_prediction = case_when(
    Player_Source == "Dre Greenlaw" ~ "1 Years $12M ($12M), ~$8M Guaranteed",
    Player_Source == "Robert Spillane" ~ "3 Years $27M ($9M), ~$14M Guaranteed",
    Player_Source == "Christian Rozeboom" ~ "3 Years $24M ($8M), ~$11M Guaranteed",
    Player_Source == "Tyrel Dodson" ~ "3 Years $26M ($8.66M), ~$12.5M Guaranteed",
  )) |> 
  mutate(prediction_explanation = case_when(
    Player_Source == "Tyrel Dodson" ~ "Dodson is another player who should benefit from this market. The TJ Edwards and Blake Cashman contracts seem like good stepping points.",
    Player_Source == "Christian Rozeboom" ~ "Rozeboom was more than servicible in Ernest Jones' absence. He might not command exactly get what Al-Shaair did, but should have a market in a league where every team seemingly wants to lock up their LBs.",
    Player_Source == "Robert Spillane" ~ "Spillane is not exactly a PFF darling, which lowered the adjusted apy of his matched, but with the linebacker market currently, a player with the experience Spillane has should get more than these matches are predicting.",
    Player_Source == "Dre Greenlaw" ~ "The linebacker market has proven to be very lucrative and Greenlaw might want to take advantage of that, signing for cheaper than he's worth, but on an incentive-laden 1 year deal.",
    
    )) |> 
  mutate(contract = case_when(
    Player_Source == "Zack Baun" ~ "3 Years 51M ($17M), $34M Guaranteed",
    Player_Source == "Nick Bolton" ~ "3 Years 45M ($15M), $30M Guaranteed",
    Player_Source == "Bobby Wagner" ~ "1 Year 9.5M ($9.5M)",
    Player_Source == "Jamien Sherwood" ~ "3 Years 45M ($15M), $30M Guaranteed",
    Player_Source == "Lavonte David" ~ "1 Year 10M ($10M) $9M Guaranteed",
    Player_Source == "Ernest Jones" ~ "3 Years 33M ($10M) $15M Guaranteed",
  )) |> 
  mutate(contract_explanation = case_when(
    Player_Source == "Ernest Jones" ~ "Ernest Jones received a few million above the inflated APY projections of 3/5 matched contracts. Despite switching hands a couple of times this past season, Jones put together a good season and was able to take advantage of a great LB market.",
    Player_Source == "Lavonte David" ~ "David's contract, interestingly enough comped well to his contract last year, which didn't show up through the model. He got a decent bit more than even Sean Lee's inflated value over 1 year.",
    Player_Source == "Jamien Sherwood" ~ "Sherwood was one of the first surprises of this free agency season. It seems as though he got something in between the Okereke and Baker contracts. 
    Was definitely an interesting choice for the Jets to pay him that much money after only 1 year of substantial production (but not to the level of someone like Zack Baun).",
    Player_Source == "Bobby Wagner" ~ "Bobby Wagner signed a deal very similar to Lavonte David's contract from 2024. A one year deal made sense for the 34 year old, who was a great veteran presence for Washington last year.",
    Player_Source == "Zack Baun" ~ "Baun signed a deal that massively eclipsed every contract he matched with except for Lavonte David. I've compared him to Matt Milano's 2023 extension with an extra year added. 
    Both players benefitted greatly from breakouts in that case.",
    Player_Source == "Nick Bolton" ~ "Nick Bolton recieved right around that Joe Schobert contract APY with the amount of years we've recently seen in contracts like Jerome Baker and JOK.",
  ))

S_Matched <- read.csv("SMatched.csv") |> select(-X)
S_ID <- read.csv("SID.csv") |> select(-X) |> 
  left_join(pff_top_150 |> summarize(Player_Source = clean_player_names(g.card__content), rank = g.label), by = "Player_Source") |> 
  filter(!is.na(year)) |> 
  mutate(rank = case_when(
    Player_Source == "Rayshawn Jenkins" ~ 200,
    Player_Source == "Jordan Fuller" ~ 220,
    Player_Source == "Chuck Clark" ~ 230,
    Player_Source == "Jordan Poyer" ~ 190,
    Player_Source == "Justin Reid" ~ 20,
    Player_Source == "Will Harris" ~ 280,
    Player_Source == "Vonn Bell" ~ 270,
    Player_Source == "Jason Pinnock" ~ 190,
    TRUE ~ rank
  )) |> 
  filter(!is.na(rank)) |> 
  arrange(rank)  |> 
  select(-rank)|> 
  mutate(contract_prediction = case_when(
    Player_Source == "Jevon Holland" ~ "4 Years $80M ($20M), ~$52M Guaranteed",
    Player_Source == "Camryn Bynum" ~ "3 Years $38M ($12.66M), ~$26M Guaranteed",
    Player_Source == "Talanoa Hufanga" ~ "3 Years $38M ($12.66M), ~$22M Guaranteed",
    Player_Source == "Justin Reid" ~ "3 Years $46.5M ($15.5M), ~$25M Guaranteed",
    Player_Source == "Trevon Moehrig" ~ "4 Years $60M ($15M), ~$35M Guaranteed",
    Player_Source == "Justin Simmons" ~ "1 Year $8M ($8M), ~$8M Guaranteed",
    Player_Source == "Jeremy Chinn" ~ "3 Years $30M ($10M), ~ $16M Guaranteed",
    Player_Source == "Andre Cisco" ~ "3 Years $34M ($11.33M), ~$19M Guaranteed",
    Player_Source == "Julian Blackmon" ~ "3 Years $24M ($8M), ~ $13.5M Guaranteed",
    
  )) |> 
  mutate(prediction_explanation = case_when(
    Player_Source == "Julian Blackmon" ~ "Blackmon outplayed his 1 year $3.7M contract from last season and has done enough to earn a multi-year deal, playing over 1,000 snaps last season.",
    Player_Source == "Andre Cisco" ~ "Cisco is a multi-year started that has missed minimal time in his 3 years starting for Jacksonville. That alone should help him garner a contract that would put him right outside the top 10 of safeties in terms of APY.",
    Player_Source == "Jeremy Chinn" ~ "Chinn provided a great backend presence for the Commanders' deep playoff run. He comps well to Eric Reid's 2018 season and should get around the inflated value of that contract.",
    Player_Source == "Justin Simmons" ~ "Simmons didn't have his greatest season with Atlanta last year, but is still a good safety, even at 31. He will probably sign for a bit over the Rayshawn Jenkins predicted contract (for around his contract last year).",
    Player_Source == "Trevon Moehrig" ~ "For a 25 year old Moehrig, the Minkah contract seems a bit steep, but the 2024 Dugger contract seems like the perfect analogue for this one. Both are even great against the run and have minimal pass-game production.",
    Player_Source == "Justin Reid" ~ "Reid was a huge part of the Chiefs defense these past few years and is a more than servicible safety. The Byard and Joyner APYs when extrapolated 
    seem like a good indicators of what Reid will get in free agency. He also can serve as an emergency kicker!",
    Player_Source == "Camryn Bynum" ~ "Bynum's contract should come a touch above Tracy Walker's apy cap % from 2022. The Grant Delpit contract from 2023 
    seems like a high-end comp for Bynum and Julian Love from last year seems low-end.",
    Player_Source == "Jevon Holland" ~ "Holland will command a top of the market safety contract this offseason 
    The APY cap % of Collins' contract and the length of Jamal Adams' seem apt for Holland. Safety guarantees usually are around 
    that $50M when adjusted to this years' cap number.",
  ))

CB_Matched <- read.csv("CBMatched.csv") |> select(-X)
CB_ID <- read.csv("CBID.csv") |> select(-X) |> 
  group_by(Player_Source) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  left_join(pff_top_150 |> summarize(Player_Source = clean_player_names(g.card__content), rank = g.label), by = "Player_Source") |> 
  filter(!is.na(year)) |> 
  mutate(rank = case_when(
    Player_Source == "Kendall Fuller" ~ 70,
    Player_Source == "Shaquill Griffin" ~ 150,
    Player_Source == "TreDavious White" ~ 170,
    Player_Source == "Paulson Adebo" ~ 50,
    Player_Source == "Isaac Yiadom" ~ 130,
    Player_Source == "Eric Stokes" ~ 200,
    Player_Source == "Adoree Jackson" ~ 145,
    Player_Source == "Brandin Echols" ~ 210,
    TRUE ~ rank
  )) |> 
  filter(!is.na(rank)) |> 
  arrange(rank)  |> 
  select(-rank)|> 
  mutate(contract_prediction = case_when(
    Player_Source == "DJ Reed" ~ "3 Years $48M ($16M), ~ $33M Guaranteed",
    Player_Source == "Charvarius Ward" ~ "3 Years $45M ($15M), ~ $30M Guaranteed",
    Player_Source == "Rasul Douglas" ~ "2 Years $25M ($12.5M), ~ $16M Guaranteed",
    Player_Source == "Byron Murphy" ~ "3 Years $48M ($16M), ~ $34.5M Guaranteed", #### FIX 
    Player_Source == "Carlton Davis" ~ "3 Years $42M ($14M), ~ $27.5M Guaranteed",
    Player_Source == "Asante Samuel" ~ "2 Years $24M ($12M), ~ $35M Guaranteed",
    Player_Source == "Mike Hilton" ~ "2 Years $24M ($12M), ~ $35M Guaranteed",
    Player_Source == "Paulson Adebo" ~ "2 Years $22M ($11M), ~ $16M Guaranteed",
  )) |> 
  mutate(prediction_explanation = case_when(
    Player_Source == "Paulson Adebo" ~ "Coming off of an injury, the market might be careful with Adebo. The Byron Murphy prove-it-esque deal seems apt.",
    Player_Source == "Mike Hilton" ~ "Hilton, who is already 31, should get a shorter-term deal. The Chris Harris deal in terms of APY cap % provides a good analogue, as both are/were mainly slot CBs.",
    Player_Source == "Asante Samuel" ~ "Samuel is only 25 years old and players who get to free agency early seem to get longer and more lucrative contracts. The Kendall Fuller contract seems like a good comparison in this case.",
    Player_Source == "Carlton Davis" ~ "Carlton Davis, who was traded from Tampa to Detroit mid-season, suffered a season-ending injury in week 15. While it wasn't lower-body, Davis should command a contract similar to his last (though not adjusted for the cap due to his performances as of the past few years.",
    Player_Source == "Byron Murphy" ~ "Murphy closely compares to 3 players who will average around $15M a year. However, since Murphy is still 27, his youth and breakout year should stand out to teams in free agency.",
    Player_Source == "Rasul Douglas" ~ "Douglas played well for both Green Bay and Buffalo and should finally cash in from these past few years. He's already 29, so he most likely won't get the 3 years his peers will.",
    Player_Source == "Charvarius Ward" ~ "Ward has been a staple of the 49ers defense for a few years. He's already 28 so he should get around 3 years and fit right in past the current gap in active CB contracts. His injury does him no favors with his comparisons.",
    Player_Source == "DJ Reed" ~ "DJ Reed is a more than servicible CB who massively outplayed his contract for the Jets. 
    He's one of the best CBs in this FA group and should fill in a current gap between the upper echelon of CBs contracts and the rest. 
    He'll slot in a few million above Cam Sutton's extrapolated APY.",
  ))

TE_Matched <- read.csv("TEMatched.csv") |> select(-X)
TE_ID <- read.csv("TEID.csv") |> select(-X) |> 
  left_join(pff_top_150 |> summarize(Player_Source = clean_player_names(g.card__content), rank = g.label), by = "Player_Source") |> 
  filter(!is.na(year)) |> 
  mutate(rank = case_when(
    Player_Source == "Evan Engram" ~ 1,
    Player_Source == "Gerald Everett" ~ 150,
    Player_Source == "Mo Alie-Cox" ~ 170,
    Player_Source == "Durham Smythe" ~ 180,
    Player_Source == "Tommy Tremble" ~ 140,
    Player_Source == "John Bates" ~ 200,
    Player_Source == "Kylen Granson" ~ 210,
    TRUE ~ rank
  )) |> 
  filter(!is.na(rank)) |> 
  arrange(rank)  |> 
  select(-rank)|> 
  mutate(contract_prediction = case_when(
    Player_Source == "Evan Engram" ~ "2 Years $22M ($11M), ~ $11M Guaranteed",
    Player_Source == "Juwan Johnson" ~ "3 Years $27M ($9M), ~ $15.5M Guaranteed",
    Player_Source == "Tyler Conklin" ~ "3 Years $26M ($8.66M), ~ $13.5M Guaranteed",
    Player_Source == "Tommy Tremble" ~ "3 Years $23.5M ($7.5M), ~ $12M Guaranteed",
  )) |> 
  mutate(prediction_explanation = case_when(
    Player_Source == "Tommy Tremble" ~ "Tremble is mostly a run blocking TE, and Tremble, who also provides some ability in the passing game should set the market here-- a similar scenario to Will Dissly's 2022 extension in Seattle.",
    Player_Source == "Tyler Conklin" ~ "Conklin compares closely to his former teammate CJ Uzomah, as well as players like Hunter Henry. The TE market hasn't had the opportunity to adapt with the cap, so he should come in at a little below those players.",
    Player_Source == "Juwan Johnson" ~ "Johnson is a consistent player at the TE position, who should command a similar contract to Mike Gesicki, barely edging out his inflated APY from his last contract.",
    Player_Source == "Evan Engram" ~ "Evan Engram despite having been cut, he will still probably garner interest in FA. 
    He will probably get slightly above the inflated APY of the similar contracts listed and right above the APY of Noah Fant's 2024 contract. 
    He is already 30, but despite the longevity of the TE position, Engram might want to get back to the market quickly.",
  )) |> 
  mutate(contract = case_when(
    Player_Source == "Mike Gesicki" ~ "3 Years 25.5M ($8.5M), $6.5M Guaranteed",
    Player_Source == "Zach Ertz" ~ "1 Years $6.25M ($6.25M), ~ $5.59M Guaranteed",
    Player_Source == "Austin Hooper" ~ "1 Years $5M ($5M), ~ $4M Guaranteed",
  )) |> 
  mutate(contract_explanation = case_when(
    Player_Source == "Austin Hooper" ~ "Hooper, at 30 years old received a 1 year deal with New England, which was around value for him. I do think he could have gotten at least another year at the same APY.",
    Player_Source == "Zach Ertz" ~ "Ertz received a contract slightly below his production from last year (as evident by his contract comparisons), most likely due to his age,",
    Player_Source == "Mike Gesicki" ~ "Gesicki received a contract very similar to his 2nd ranked similarity player, Hayden Hurst. He lags far behind in guarantees, however.",
  ))

WR_Matched <- read.csv("WRMatched.csv") |> select(-X)
WR_ID <- read.csv("WRID.csv") |> select(-X) |> 
  left_join(pff_top_150 |> summarize(Player_Source = clean_player_names(g.card__content), rank = g.label), by = "Player_Source") |> 
  filter(!is.na(year)) |> 
  mutate(rank = case_when(
    Player_Source == "Davante Adams" ~ 5,
    Player_Source == "Tyler Lockett" ~ 60,
    Player_Source == "Robert Woods" ~ 170,
    Player_Source == "Josh Reynolds" ~ 180,
    Player_Source == "Demarcus Robinson" ~ 160,
    Player_Source == "KJ Osborn" ~ 145,
    Player_Source == "Mack Hollins" ~ 129,
    Player_Source == "Van Jefferson" ~ 200,
    Player_Source == "Olamide Zaccheaus" ~ 210,
    Player_Source == "David Moore" ~ 220,
    Player_Source == "Tim Patrick" ~ 50,
    Player_Source == "Joshua Palmer" ~ 50,
    TRUE ~ rank
  )) |> 
  filter(!is.na(rank)) |> 
  arrange(rank)  |> 
  select(-rank)|> 
  mutate(contract_prediction = case_when(
    Player_Source == "Tee Higgins" ~ "3 Years $96M ($32M), ~ $75M Guaranteed",
    Player_Source == "Chris Godwin" ~ "2 Years $46M ($23M), ~ $45M Guaranteed",
    Player_Source == "Amari Cooper" ~ "2 Years $30M ($15M), ~ $14M Guaranteed",
    Player_Source == "Stefon Diggs" ~ "1 Year $17M ($17M), ~ $17M Guaranteed",
    Player_Source == "DeAndre Hopkins" ~ "1 Year $10M ($10M), ~ $10M Guaranteed",
    Player_Source == "Darius Slayton" ~ "3 Years $39M ($13M), ~ $25M Guaranteed",
    Player_Source == "Keenan Allen" ~ "2 Years $26M ($13M), ~ $14M Guaranteed",
    Player_Source == "Tyler Lockett" ~ "2 Years $20M ($10M), ~ $12M Guaranteed",
    Player_Source == "Tim Patrick" ~ "3 Years $24M ($8M), ~ $15M Guaranteed",
    Player_Source == "Joshua Palmer" ~ "3 Years $25M ($8.33M), ~ $16M Guaranteed",
  )) |> 
  mutate(prediction_explanation = case_when(
    Player_Source == "Joshua Palmer" ~ "Palmer should get something right in the middle of his matches. He's still only 25 and has the potential to be a high-end WR3 for the team that signs him.",
    Player_Source == "Tim Patrick" ~ "Tim Patrick doesn't really compare will to his listed comparisons, but will probably command a little more than Kendrick Bourne from last year. He had limited production in a crowded Lions offense, but should be able to shine elsewhere, further removed from his injuries.",
    Player_Source == "Tyler Lockett" ~ "Lockett right now is very reminiscent of Robert Woods' contract with the Texans after being cut by the Titans. Both players weren't/aren't at their peak necesarrily. Teams will see Lockett as a good veteran WR3.",
    Player_Source == "Keenan Allen" ~ "Keenan Allen comps well to Emmanuel Sanders listed below, but the direction of the WR market should get him a few million more than Sanders' inflated apy number.",
    Player_Source == "Darius Slayton" ~ "I think the true numbers of the Shakir contract limit Slayton's ceiling a little bit, Slayton gets a similar deal to recent tier WR 3 contracts (John Brown encapsulates an early version of this).",
    Player_Source == "DeAndre Hopkins" ~ "Hopkins, at age 32 compares really well to the Emmanuel Sanders and AJ Green contracts listed.",
    Player_Source == "Stefon Diggs" ~ "Though not listed because of injuries, Diggs' situation comps well to Odell's 1 year $15M deal with the Ravens in 2023. The numbers inflate to around this predicted value.",
    Player_Source == "Amari Cooper" ~ "Amari projects very closely to DeAndre Hopkins' 2023 extension, which is listed 4th on his similarity list.",
    Player_Source == "Chris Godwin" ~ "Godwin is coming off of an injury, so his matched players are a little off of what his contract might look like. He is a potential WR1 and a team will get him at a discount because of the injury concern.",
    Player_Source == "Tee Higgins" ~ "Higgins seems as though he is in a stalemate with Cincinnati and with DK's recent extension, Higgins is in line for $30M+, whether it's with the Bengals or some other team.",

  )) |> 
  mutate(contract = case_when(
    Player_Source == "Davante Adams" ~ "2 Years $44M ($22M), ~ $26M Guaranteed",
    Player_Source == "Marquise Brown" ~ "1 Years $11M ($11M), ~ $6.35M Guaranteed",
  )) |> 
  mutate(contract_explanation = case_when(
    Player_Source == "Marquise Brown" ~ "Marquise Brown signed for basically the same contract as last year. No discount from the time he missed in KC, but he gets another opportunity to prove himself with Andy Reid and Mahomes at age 27.",
    Player_Source == "Davante Adams" ~ "Adams gets a very similar contract to Mike Evans' last year when extrapolated to this year. He just gets less guaranteed.",
  ))

RB_Matched <- read.csv("RBMatched.csv") |> select(-X)
RB_ID <- read.csv("RBID.csv") |> select(-X) |> 
  left_join(pff_top_150 |> summarize(Player_Source = clean_player_names(g.card__content), rank = g.label), by = "Player_Source") |> 
  filter(!is.na(year)) |> 
  mutate(rank = case_when(
    Player_Source == "Raheem Mostert" ~ 150,
    Player_Source == "Jamaal Williams" ~ 160,
    Player_Source == "Alexander Mattison" ~ 170,
    Player_Source == "Ameer Abdullah" ~ 180,
    Player_Source == "Ty Johnson" ~ 175,
    Player_Source == "Kenneth Gainwell" ~ 155,
    TRUE ~ rank
  )) |> 
  mutate(rank = case_when(
    Player_Source == "Davante Adams" ~ 5,
    Player_Source == "Tyler Lockett" ~ 60,
    Player_Source == "Robert Woods" ~ 170,
    Player_Source == "Josh Reynolds" ~ 180,
    Player_Source == "Demarcus Robinson" ~ 160,
    Player_Source == "KJ Osborn" ~ 145,
    Player_Source == "Mack Hollins" ~ 129,
    Player_Source == "Van Jefferson" ~ 200,
    Player_Source == "Olamide Zaccheaus" ~ 210,
    Player_Source == "David Moore" ~ 220,
    Player_Source == "Tim Patrick" ~ 50,
    Player_Source == "Joshua Palmer" ~ 50,
    TRUE ~ rank
  )) |> 
  filter(!is.na(rank)) |> 
  arrange(rank)  |> 
  select(-rank)|> 
  mutate(contract_prediction = case_when(
    Player_Source == "Najee Harris" ~ "3 Years $33M ($11M), ~$22M Guaranteed",
    Player_Source == "JK Dobbins" ~ "2 Years $14M ($7M), ~$9M Guaranteed",
  )) |> 
  mutate(prediction_explanation = case_when(
    Player_Source == "JK Dobbins" ~ "Dobbins should be able to take a generally healthy season where he ran once again for over 4 yards a carry (on almost 200 carries!) into a new contract that slots him right below Tony Pollard.",
    Player_Source == "Najee Harris" ~ "With running back contracts on the mend, Najee Harris, who has never had a season below 1,000 rushing yards will benefit greatly. I think he'll get an APY slightly below Kenyon Drake's 2020 inflated number. His contract would serve as a fill to the gap between Mixon and Jacobs.",
  )) |> 
  mutate(contract = case_when(
    Player_Source == "Aaron Jones" ~ "2 Years $20M ($10M), ~$13M Guaranteed",
  )) |> 
  mutate(contract_explanation = case_when(
    Player_Source == "Aaron Jones" ~ "Aaron Jones gets more than all of his comparisons besides himself. He also got nice guarantees considering his age and position.",
  ))

QB_Matched <- read.csv("QBMatched.csv") |> select(-X) |> 
  mutate(years = ifelse(Match2 == "Aaron Rodgers" & Match_year == 2022, 3, years))
QB_ID <- read.csv("QBID.csv") |> select(-X) |> 
  left_join(pff_top_150 |> summarize(Player_Source = clean_player_names(g.card__content), rank = g.label), by = "Player_Source") |> 
  filter(!is.na(year)) |> 
  mutate(rank = case_when(
    Player_Source == "Aaron Rodgers" ~ 10,
    Player_Source == "Russell Wilson" ~ 12,
    Player_Source == "Trey Lance" ~ 200,
    Player_Source == "Jacoby Brissett" ~ 100,
    Player_Source == "Marcus Mariota" ~ 110,
    Player_Source == "Drew Lock" ~ 120,
    Player_Source == "Joe Flacco" ~ 175,
    Player_Source == "Jimmy Garoppolo" ~ 180,
    Player_Source == "Mason Rudolph" ~ 190,
    Player_Source == "Carson Wentz" ~ 200,
    Player_Source == "Tyler Huntley" ~ 210,
    Player_Source == "Daniel Jones" ~ 115,
    TRUE ~ rank
  )) |> 
  filter(!is.na(rank)) |> 
  arrange(rank)  |> 
  select(-rank) |> 
  mutate(contract_prediction = case_when(
    Player_Source == "Sam Darnold" ~ "3 Years $135M ($45M), ~ $75M Guaranteed",
    Player_Source == "Aaron Rodgers" ~ "2 Years $55M ($27.5M), ~ $30M Guaranteed",
    Player_Source == "Russell Wilson" ~ "2 Years $60M ($30M), ~ $35M Guaranteed",
    Player_Source == "Justin Fields" ~ "1 Year $10M ($10M), ~ $10M Guaranteed",
  )) |> 
  mutate(prediction_explanation = case_when(
    Player_Source == "Justin Fields" ~ "None of the sitations match perfectly to Fields's, as he will probably sign a prove-it deal similar not to Darnold's 2023 contract, but to his 2024 one with the Vikings.",
    Player_Source == "Russell Wilson" ~ "Russell Wilson, at age 36 will probably want a prove-it-esque 2 year deal to potentially spin that into a top of the market final contract if he can return to form. I think right around Jimmy G's inflated value but over 2 years fits.",
    Player_Source == "Sam Darnold" ~ "Darnold, with a stellar season in 2024 will most likely use this leverage into a massive contract, one very similar to Daniel Jones' contract in 2023, but a little lower than the inflated APY for Jones. He will be somewhere in between the QBs the algorithm labeled him most similar to.",
    Player_Source == "Aaron Rodgers" ~ "Aaron Rodgers is coming off of a fairly good statistical season all things considered. He'll probably sign a 2 year contract that if chooses to retire, the team singing him does not suffer.",
  )) 

Everyone_ID <- bind_rows(QB_ID, RB_ID, WR_ID, TE_ID, OL_ID, 
                         IDL_ID, EDGE_ID, LB_ID, CB_ID, S_ID)

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(
    bootswatch = "cosmo", 
    base_font = "Source Sans Pro",
    auto = FALSE),
  
  tags$head(
    tags$meta(property = "og:title", content = "Contract Predictions 2025"),
    tags$meta(property = "og:url", content = "https://supercd7478.shinyapps.io/DraftPickScenarios/"),
    tags$head(
      tags$title("2025 Contract Prediction Analysis")
    ),
    tags$style(HTML("
        .custom-navbar {
          width: 100% !important;
          max-width: 100% !important;
          margin: 0;
          padding: 10px;
          background-color: #333;
          color: white;
          text-align: center;
        }
        .custom-navbar .navbar-nav {
          justify-content: center;
          width: 100%;
        }
      "))
  ),
  
  tags$style(HTML("
    .custom-select-label {
    margin-top: 20px;
    font-family: 'Source Sans Pro', sans-serif; 
    font-size: 18px; 
    font-weight: bold;
    color: #333; 
    }")),
  
  tags$style(HTML("
    .custom-select-label2 {
    margin-top: 10px;
    margin-top: 20px;
    font-family: 'Source Sans Pro', sans-serif; 
    font-size: 18px; 
    font-weight: bold;
    color: #333; 
    }")),
  
  fluidRow(
    column(12, 
           titlePanel(
             tags$div(
               style = "text-align: center;",
               tags$div(
                 "NFL Market Match",
                 style = "font-size: 36px; font-weight: bold; color: #004C54;"
               ),
               tags$div(
                 "A Comparison-Based Approach to Predicting 2025 Free Agency",
                 style = "font-size: 16px; font-style: italic; color: #7f8c8d; margin-top: 10px;"
               )
             )
           )
    )
  ),
  tags$div(class = "custom-navbar",
           navbarPage("By: Cooper Davis (@CDFBAnalysis) | Data via OTC, PFF, and nflverse",
                      id = "nav")
  ),
  
  tabsetPanel(
    tabPanel("Contract Predictions", # First tab
             fluidRow(
               column(2, align = "center",
                      tags$div(class = "custom-select-label", "Position:"),
                      selectInput("position", label = NULL, 
                                  choices = c("QB", "RB", "WR", "TE", "OL", "EDGE", "IDL", "LB", "CB", "S"), 
                                  selected = 'QB'),
                      uiOutput("Player_Source")
               ),
               column(2, align = "center",
                      tags$div(class = "custom-select-label", "Player:"),
                      selectInput("Player", label = NULL, choices = QB_ID$Player_Source, 
                                  selected = QB_ID$Player_Source[1])
               ),
               column(8, align = "center",
                      tableOutput('player_tbbl')
               )
             ),
             
             fluidRow(
               column(12, align = "center",
                      tableOutput('similarity'))
             ),
             
             fluidRow(
               column(6,
                      tags$div(
                        "Contract Prediction:",
                        style = "font-size: 32px; font-weight: bold; color: #004C54;"
                      )
               ),
               column(6,
                      tags$div(
                        "Actual Contract:",
                        style = "font-size: 32px; font-weight: bold; color: #004C54;"
                      )
               )
             ),
             
             fluidRow(
               column(6, align = "center",
                      tags$div(style = "font-size: 24px; font-weight: bold;", 
                               textOutput("Prediction"))
               ),
               column(6, align = "center",
                      tags$div(style = "font-size: 24px; font-weight: bold;", 
                               textOutput('Contract'))
               )
             ), 
             
             fluidRow(
               column(6, align = "center",
                      tags$div(class = "custom-select-label2", 
                               textOutput('PredictionExplanation'))
               ),
               column(6, align = "center",
                      tags$div(class = "custom-select-label", 
                               textOutput('ContractExplanation'))
               )
             )
    ), 
    
    tabPanel("About", 
             fluidRow(
               column(12, align = "center",
                      tags$div(
                        tags$h2("About"),
                        tags$p("NFL Market Match exists to help visualize potentially similar contracts to this year's free agency's top players. 
                               The ShinyApp uses a knn algorthm to measure similarities between players at the time of signing their contracts. 
                               It is heavily skewed towards the previous season, which in the case of injured players, results in poor predictions. 
                               I will try to remedy this for next year's model! I inserted my own opinions for the poorly matched top-of-market injured players."),
                        tags$p("I've included some predictions myself and analysis on contracts that already have been signed through 3/10/25. 
                               If you have any questions or concerns, feel free to reach out!"),
                        tags$p("I will try to update as contracts come through."),
                        tags$p("Developed by: Cooper Davis (@CDFBAnalysis)"),
                        tags$p("Data Sources: OverTheCap (OTC), Pro Football Focus (PFF), and nflverse.")
                      )
               )
             )
    ) 
    
  ) 
)

server <- function(input, output, session) {
  observe({
    # Update the "team" dropdown based on selected position
    player_choices <- switch(input$position,
                             "QB" = QB_ID$Player_Source,
                             "RB" = RB_ID$Player_Source,
                             "WR" = WR_ID$Player_Source,
                             "TE" = TE_ID$Player_Source,
                             "OL" = OL_ID$Player_Source,
                             "IDL" = IDL_ID$Player_Source,
                             "EDGE" = EDGE_ID$Player_Source,
                             "LB" = LB_ID$Player_Source,
                             "CB" = CB_ID$Player_Source,
                             "S" = S_ID$Player_Source)
    
    if (is.null(player_choices) || length(player_choices) == 0) {
      player_choices <- "No Players Available"
    }
    
    updateSelectInput(session, "Player", 
                      choices = player_choices, 
                      selected = player_choices[1])
  })
  
  # output$player_image <- renderUI({
  #    Player <- input$Player
  #      path_to <- rosters_2024$headshot_url[rosters_2024$full_name == Player]
  #      tags$img(src = path_to, 
  #               height = "270px",
  #               width = "360px")
  #  })
  
  output$Prediction <- renderText({
    Everyone_ID |> filter(Player_Source == input$Player) |> pull(contract_prediction)
  })
  
  output$PredictionExplanation <- renderText({
      Everyone_ID |> filter(Player_Source == input$Player) |> pull(prediction_explanation)
  })
  
  output$Contract <- renderText({
    Everyone_ID |> filter(Player_Source == input$Player) |> pull(contract)
  })
  
  output$ContractExplanation <- renderText({
    Everyone_ID |> filter(Player_Source == input$Player) |> pull(contract_explanation)
  })
  
  output$player_tbbl <- render_gt({
    req(input$position)
    req(input$Player)
    validate(need(input$Player %in% Everyone_ID$Player_Source, "Loading data..."))
    validate(need(nrow(data) > 0, "No data available."))
    if (input$position == "OL") {
      OL_ID |> filter(Player_Source == input$Player) |> 
        summarize(player = Player_Source, headshot_url, position, height, weight, age_at_signing, contract_number, snaps = snap_counts_offense, rolling_3_snaps, draft_overall,
                  grades_offense, grades_pass_block, grades_run_block, pbe) |> 
        gt() |> 
        gt_img_rows(headshot_url)  |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          snaps = "Snap Count",
          rolling_3_snaps = "Tot. 3Y Snaps",
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_offense = "PFF Grade",
          grades_pass_block = "PBKG",
          grades_run_block = "RBKG",
          pbe = "pbe"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, grades_offense))
        ) |>
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = everything())
        ) |> 
        tab_source_note(
          source_note = md("")
        ) 
    } else if (input$position == "IDL") {
      IDL_ID |> filter(Player_Source == input$Player) |> 
        summarize(player = Player_Source, headshot_url, position, height, weight, age_at_signing, contract_number, snaps, rolling_3_snaps, draft_overall,
                  grades_defense, PRWR,total_pressures, stops) |> 
        gt() |> 
        gt_img_rows(headshot_url)  |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          snaps = "Snap Count",
          rolling_3_snaps = "Tot. 3Y Snaps",
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_defense = "PFF Grade",
          PRWR = "PRWR",
          total_pressures = "Pressures",
          stops = "Stops"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, grades_defense))
        ) |>
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = everything())
        ) |> 
        tab_source_note(
          source_note = md("")
        ) 
    }  else if (input$position == "EDGE") {
      EDGE_ID |> filter(Player_Source == input$Player) |> 
        summarize(player = Player_Source, headshot_url, position, height, weight, age_at_signing, contract_number, snaps, rolling_3_snaps, draft_overall,
                  grades_defense, PRWR,total_pressures, stops) |> 
        gt() |> 
        gt_img_rows(headshot_url)  |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          snaps = "Snap Count",
          rolling_3_snaps = "Tot. 3Y Snaps",
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_defense = "PFF Grade",
          PRWR = "PRWR",
          total_pressures = "Pressures",
          stops = "Stops"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, grades_defense))
        ) |>
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = everything())
        ) |> 
        tab_source_note(
          source_note = md("")
        ) 
    } else if (input$position == "LB") {
      LB_ID |> filter(Player_Source == input$Player) |> 
        summarize(player = Player_Source, headshot_url, position, height, weight, age_at_signing, contract_number, snaps, rolling_3_snaps, draft_overall,
                  grades_coverage_defense, tackles, grades_tackle, missed_tackles, stops, pass_break_ups) |> 
        gt() |> 
        gt_img_rows(headshot_url)  |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          snaps = "Snap Count",
          rolling_3_snaps = "Tot. 3Y Snaps",
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_coverage_defense = "Cov. Grade",
          tackles = "Tackles",
          grades_tackle = "Tackle Grade",
          missed_tackles = "Miss. Tackles",
          stops = "Stops",
          pass_break_ups = "PBUs"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, draft_overall))
        ) |>
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = everything())
        ) |> 
        tab_source_note(
          source_note = md("")
        ) 
    } else if (input$position == "S") {
      S_ID |> filter(Player_Source == input$Player) |> 
        summarize(player = Player_Source, headshot_url, position, height, weight, age_at_signing, contract_number, snaps, rolling_3_snaps, draft_overall,
                  grades_run_defense,
                  grades_coverage_defense, tackles,
                  interceptions, pass_break_ups, missed_tackles) |> 
        gt() |> 
        gt_img_rows(headshot_url)  |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          snaps = "Snap Count",
          rolling_3_snaps = "Tot. 3Y Snaps",
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_coverage_defense = "Cov. Grade",
          tackles = "Tackles",
          grades_run_defense = "Run Grade",
          missed_tackles = "Miss. Tackles",
          interceptions = "Interceptions",
          pass_break_ups = "PBUs"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, draft_overall))
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = everything())
        ) |> 
        tab_source_note(
          source_note = md("")
        ) 
    } else if (input$position == "CB") {
      CB_ID |> filter(Player_Source == input$Player) |> 
        summarize(player = Player_Source, headshot_url, position, height, weight, age_at_signing, contract_number, snaps, rolling_3_snaps, draft_overall,
                  forced_incompletes, 
                  grades_coverage_defense, pass_break_ups,
                  interceptions, yards_per_coverage_snap) |> 
        gt() |> 
        gt_img_rows(headshot_url)  |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          contract_number = "Contr. #",
          snaps = "Snap Count",           
          rolling_3_snaps = "Tot. 3Y Snaps", 
          draft_overall = "Pick",
          grades_coverage_defense = "Cov. Grade",
          yards_per_coverage_snap = "YPS",
          forced_incompletes = "FINCs",
          interceptions = "Interceptions",
          pass_break_ups = "PBUs"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, draft_overall))
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = everything())
        ) |> 
        tab_source_note(
          source_note = md("")
        ) 
    } else if (input$position == "TE") {
      TE_ID |> filter(Player_Source == input$Player) |> 
        summarize(player = Player_Source, headshot_url, position, height, weight, age_at_signing, contract_number, snaps, rolling_3_snaps, draft_overall,
                  grades_offense, receptions, targets, yards, touchdowns, grades_pass_block, grades_run_block) |> 
        gt() |> 
        gt_img_rows(headshot_url)  |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          snaps = "Snap Count",
          rolling_3_snaps = "Tot. 3Y Snaps",
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_offense = "PFF Grade",
          receptions = "Rec",
          targets = "Tgt", 
          yards = "Yds", 
          touchdowns = "TDs", 
          grades_pass_block = "PBG", 
          grades_run_block = "RBG"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, grades_offense))
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = everything())
        ) |> 
        tab_source_note(
          source_note = md("")
        ) 
    } else if (input$position == "WR") {
      WR_ID |> filter(Player_Source == input$Player) |> 
        summarize(player = Player_Source, headshot_url, position, height, weight, age_at_signing, contract_number, snaps, rolling_3_snaps, draft_overall,
                  grades_offense, receptions, targets, yards, touchdowns, yprr, avg_depth_of_target, slot_rate) |> 
        gt() |> 
        gt_img_rows(headshot_url)  |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          snaps = "Snap Count",
          rolling_3_snaps = "Tot. 3Y Snaps",
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_offense = "PFF Grade",
          receptions = "Rec",
          targets = "Tgt", 
          yards = "Yds", 
          touchdowns = "TDs", 
          slot_rate = "SLT%", 
          yprr = "YPRR", 
          avg_depth_of_target = "ADOT"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, grades_offense))
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = everything())
        ) |> 
        tab_source_note(
          source_note = md("")
        ) 
    } else if (input$position == "RB") {
      RB_ID |> filter(Player_Source == input$Player) |> 
        summarize(player = Player_Source, headshot_url, position, height, weight, age_at_signing, contract_number, snaps, rolling_3_snaps, draft_overall,
                  grades_offense, attempts, rushing_yards, rec_yards, targets, touchdowns) |> 
        gt() |> 
        gt_img_rows(headshot_url)  |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          snaps = "Snap Count",
          rolling_3_snaps = "Tot. 3Y Snaps",
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_offense = "PFF Grade",
          grades_offense = "PFF Grade",
          targets = "Tgt", 
          rushing_yards = "Rush Yds", 
          touchdowns = "TDs", 
          attempts = "Att", 
          rec_yards = "Rec Yards"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, grades_offense))
        ) |>
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = everything())
        ) |> 
        tab_source_note(
          source_note = md("")
        ) 
    } else if (input$position == "QB") {
      QB_ID |> filter(Player_Source == input$Player) |> 
        summarize(player = Player_Source, headshot_url, position, height, weight, age_at_signing, contract_number, snaps, rolling_3_snaps, draft_overall,
                  grades_offense, attempts, passing_yards, touchdowns, turnover_worthy_plays, sacks, rushing_yards) |> 
        gt() |> 
        gt_img_rows(headshot_url)  |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          snaps = "Snap Count",
          rolling_3_snaps = "Tot. 3Y Snaps",
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_offense = "PFF Grade",
          passing_yards = "Passing Yards",
          rushing_yards = "Rush Yds", 
          turnover_worthy_plays = "TWP",
          touchdowns = "TDs", 
          attempts = "Pass Att", 
          sacks = "Sacks Taken"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = everything())
        ) |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray",  # Light color for subtle lines
            weight = px(0.5)      # Very thin lines
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, grades_offense))
        ) |> 
        tab_source_note(
          source_note = md("")
        ) 
    }
  })
  
  
  
  output$similarity <- render_gt({
    req(input$position)
    req(input$Player)
    if (input$position == "OL") {
      OL_Matched |> filter(Player_Source == input$Player) |> 
        arrange(-Score) |> 
        summarize(player = Match2, headshot_url, year = Match_year, position, Score, team_logo_espn, height, weight, age_at_signing,
                  contract_number, years, value, apy, apy_cap_pct, inflated_apy = round(inflated_apy, 2), guaranteed = round(guaranteed, 2), guaranteed_pct = round(guaranteed/value, 2), snaps = snap_counts, rolling_3_snaps, draft_overall,
                  grades_offense, grades_pass_block, grades_run_block, pbe) |> 
        gt() |> 
        gt_img_rows(team_logo_espn) |> 
        gt_img_rows(headshot_url) |>
        #tab_options(
        #  data_row.padding = px(1)
        #) |> 
        tab_header(
          title = "Top Contract Comparisons"
        ) |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          team_logo_espn = "",
          Score = "Similarity",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          value = "Total",
          apy = "apy",
          apy_cap_pct = "apy%",
          inflated_apy = "Infl apy",
          contract_number = "Contr. #",
          guaranteed = "Guaranteed",
          guaranteed_pct = "Guaranteed %",
          snaps = "Snap Count",           
          rolling_3_snaps = "Tot. 3Y Snaps", 
          draft_overall = "Pick",
          grades_offense = "PFF Grade",
          grades_pass_block = "PBKG",
          grades_run_block = "RBKG",
          pbe = "pbe"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, guaranteed_pct, grades_offense))
        ) |>
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            css("padding-top" = "10px", "padding-bottom" = "10px", "padding-left" = "15px", "padding-right" = "0px")
          ),
          locations = cells_body(columns = c(team_logo_espn))
        ) |> 
        #tab_style(
        #  style = list(
        #    css("padding-left" = "13px", "padding-right" = "13px")
        #  ),
        #  locations = cells_body(columns = c(pick_prob, sos, win_pct, avg_pos, first_pick_prob, top_five_prob, top_ten_prob))
        #) |> 
        tab_style(
          style = cell_fill(color = "#F9F9F9"),
          locations = cells_body(
            rows = seq(1, 5, 2)
          ) 
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = -c(years, value, apy, guaranteed))
        ) |> 
        fmt_percent(columns = c(apy_cap_pct, guaranteed_pct), decimals = 2) |> 
        tab_source_note(
          source_note = md("")
        ) |> 
        data_color(
          columns = c(Score),
          target_columns = c(Score),
          colors = scales::col_numeric(
            palette = viridis::viridis(100),
            domain = c(0, 100)
          )
        )|> 
        tab_footnote(
          footnote = 
            "By: Cooper Davis (CDFBAnalysis) | Data via PFF, OTC, nflverse")
    }
    else if (input$position == "IDL") {
      IDL_Matched |> filter(Player_Source == input$Player) |> 
        arrange(-Score) |> 
        summarize(player = Match2, headshot_url, year = Match_year, position, Score, team_logo_espn, height, weight, age_at_signing,
                  contract_number, years, value, apy, apy_cap_pct, inflated_apy = round(inflated_apy, 2), guaranteed = round(guaranteed, 2), guaranteed_pct = round(guaranteed/value, 2), snaps, rolling_3_snaps, draft_overall,
                  grades_defense, PRWR,total_pressures, stops) |> 
        gt() |> 
        gt_img_rows(team_logo_espn) |> 
        gt_img_rows(headshot_url) |>
        #tab_options(
        #  data_row.padding = px(1)
        #) |> 
        tab_header(
          title = "Top Contract Comparisons"
        ) |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          team_logo_espn = "",
          Score = "Similarity",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          value = "Total",
          apy = "apy",
          apy_cap_pct = "apy%",
          inflated_apy = "Infl apy",
          contract_number = "Contr. #",
          guaranteed = "Guaranteed",
          guaranteed_pct = "Guaranteed %",
          snaps = "Snap Count",           
          rolling_3_snaps = "Tot. 3Y Snaps", 
          draft_overall = "Pick",
          grades_defense = "PFF Grade",
          PRWR = "PRWR",
          total_pressures = "Pressures",
          stops = "Stops"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, guaranteed_pct, grades_defense))
        ) |>
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            css("padding-top" = "10px", "padding-bottom" = "10px", "padding-left" = "15px", "padding-right" = "0px")
          ),
          locations = cells_body(columns = c(team_logo_espn))
        ) |> 
        #tab_style(
        #  style = list(
        #    css("padding-left" = "13px", "padding-right" = "13px")
        #  ),
        #  locations = cells_body(columns = c(pick_prob, sos, win_pct, avg_pos, first_pick_prob, top_five_prob, top_ten_prob))
        #) |> 
        tab_style(
          style = cell_fill(color = "#F9F9F9"),
          locations = cells_body(
            rows = seq(1, 5, 2)
          ) 
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = -c(years, value, apy, guaranteed))
        ) |> 
        fmt_percent(columns = c(apy_cap_pct, guaranteed_pct), decimals = 2) |> 
        tab_source_note(
          source_note = md("")
        ) |> 
        data_color(
          columns = c(Score),
          target_columns = c(Score),
          colors = scales::col_numeric(
            palette = viridis::viridis(100),
            domain = c(0, 100)
          )
        )|> 
        tab_footnote(
          footnote = 
            "By: Cooper Davis (CDFBAnalysis) | Data via PFF, OTC, nflverse")
    } else if (input$position == "EDGE") {
      EDGE_Matched |> filter(Player_Source == input$Player) |> 
        arrange(-Score) |> 
        summarize(player = Match2, headshot_url, year = Match_year, position, Score, team_logo_espn, height, weight, age_at_signing,
                  contract_number, years, value, apy, apy_cap_pct, inflated_apy = round(inflated_apy, 2), guaranteed = round(guaranteed, 2), guaranteed_pct = round(guaranteed/value, 2), snaps, rolling_3_snaps, draft_overall,
                  grades_defense, PRWR,total_pressures, stops) |> 
        gt() |> 
        gt_img_rows(team_logo_espn) |> 
        gt_img_rows(headshot_url) |>
        #tab_options(
        #  data_row.padding = px(1)
        #) |> 
        tab_header(
          title = "Top Contract Comparisons"
        ) |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          team_logo_espn = "",
          Score = "Similarity",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          value = "Total",
          apy = "apy",
          apy_cap_pct = "apy%",
          inflated_apy = "Infl apy",
          contract_number = "Contr. #",
          guaranteed = "Guaranteed",
          guaranteed_pct = "Guaranteed %",
          snaps = "Snap Count",           
          rolling_3_snaps = "Tot. 3Y Snaps", 
          draft_overall = "Pick",
          grades_defense = "PFF Grade",
          PRWR = "PRWR",
          total_pressures = "Pressures",
          stops = "Stops"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, guaranteed_pct, grades_defense))
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            css("padding-top" = "10px", "padding-bottom" = "10px", "padding-left" = "15px", "padding-right" = "0px")
          ),
          locations = cells_body(columns = c(team_logo_espn))
        ) |> 
        #tab_style(
        #  style = list(
        #    css("padding-left" = "13px", "padding-right" = "13px")
        #  ),
        #  locations = cells_body(columns = c(pick_prob, sos, win_pct, avg_pos, first_pick_prob, top_five_prob, top_ten_prob))
        #) |> 
        tab_style(
          style = cell_fill(color = "#F9F9F9"),
          locations = cells_body(
            rows = seq(1, 5, 2)
          ) 
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = -c(years, value, apy, guaranteed))
        ) |> 
        fmt_percent(columns = c(apy_cap_pct, guaranteed_pct), decimals = 2) |> 
        tab_source_note(
          source_note = md("")
        ) |> 
        data_color(
          columns = c(Score),
          target_columns = c(Score),
          colors = scales::col_numeric(
            palette = viridis::viridis(100),
            domain = c(0, 100)
          )
        )|> 
        tab_footnote(
          footnote = 
            "By: Cooper Davis (CDFBAnalysis) | Data via PFF, OTC, nflverse")
    } else if (input$position == "LB") {
      LB_Matched |> filter(Player_Source == input$Player) |> 
        arrange(-Score) |> 
        summarize(player = Match2, headshot_url, year = Match_year, position, Score, team_logo_espn, height, weight, age_at_signing,
                  contract_number, years, value, apy, apy_cap_pct, inflated_apy = round(inflated_apy, 2), guaranteed = round(guaranteed, 2), guaranteed_pct = round(guaranteed/value, 2), snaps, rolling_3_snaps, draft_overall,
                  grades_coverage_defense, tackles, grades_tackle, missed_tackles, stops, pass_break_ups) |> 
        gt() |> 
        gt_img_rows(team_logo_espn) |> 
        gt_img_rows(headshot_url) |>
        tab_header(
          title = "Top Contract Comparisons"
        ) |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          team_logo_espn = "",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          value = "Total",
          apy = "apy",
          apy_cap_pct = "apy%",
          inflated_apy = "Infl apy",
          contract_number = "Contr. #",
          guaranteed = "Guaranteed",
          guaranteed_pct = "Guaranteed %",
          snaps = "Snap Count",           
          rolling_3_snaps = "Tot. 3Y Snaps", 
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_coverage_defense = "Cov. Grade",
          tackles = "Tackles",
          grades_tackle = "Tackle Grade",
          missed_tackles = "Miss. Tackles",
          stops = "Stops",
          pass_break_ups = "PBUs"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, guaranteed_pct, draft_overall))
        ) |>
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, guaranteed_pct, grades_coverage_defense))
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            css("padding-top" = "10px", "padding-bottom" = "10px", "padding-left" = "15px", "padding-right" = "0px")
          ),
          locations = cells_body(columns = c(team_logo_espn))
        ) |> 
        #tab_style(
        #  style = list(
        #    css("padding-left" = "13px", "padding-right" = "13px")
        #  ),
        #  locations = cells_body(columns = c(pick_prob, sos, win_pct, avg_pos, first_pick_prob, top_five_prob, top_ten_prob))
        #) |> 
        tab_style(
          style = cell_fill(color = "#F9F9F9"),
          locations = cells_body(
            rows = seq(1, 5, 2)
          ) 
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = -c(years, value, apy, guaranteed))
        ) |> 
        fmt_percent(columns = c(apy_cap_pct, guaranteed_pct), decimals = 2) |> 
        tab_source_note(
          source_note = md("")
        ) |> 
        data_color(
          columns = c(Score),
          target_columns = c(Score),
          colors = scales::col_numeric(
            palette = viridis::viridis(100),
            domain = c(0, 100)
          )
        )|> 
        tab_footnote(
          footnote = 
            "By: Cooper Davis (CDFBAnalysis) | Data via PFF, OTC, nflverse")
    } else if (input$position == "S") {
      S_Matched |> filter(Player_Source == input$Player) |> 
        arrange(-Score) |> 
        summarize(player = Match2, headshot_url, year = Match_year, position, Score, team_logo_espn, height, weight, age_at_signing,
                  contract_number, years, value, apy, apy_cap_pct, inflated_apy = round(inflated_apy, 2), guaranteed = round(guaranteed, 2), guaranteed_pct = round(guaranteed/value, 2), snaps, rolling_3_snaps, draft_overall,
                  grades_run_defense,
                  grades_coverage_defense, tackles,
                  interceptions, pass_break_ups, missed_tackles
        ) |> 
        gt() |> 
        gt_img_rows(team_logo_espn) |> 
        gt_img_rows(headshot_url) |>
        #tab_options(
        #  data_row.padding = px(1)
        #) |> 
        tab_header(
          title = "Top Contract Comparisons"
        ) |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          team_logo_espn = "",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          value = "Total",
          apy = "apy",
          apy_cap_pct = "apy%",
          inflated_apy = "Infl apy",
          contract_number = "Contr. #",
          guaranteed = "Guaranteed",
          guaranteed_pct = "Guaranteed %",
          snaps = "Snap Count",           
          rolling_3_snaps = "Tot. 3Y Snaps", 
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_coverage_defense = "Cov. Grade",
          tackles = "Tackles",
          grades_run_defense = "Run Grade",
          missed_tackles = "Miss. Tackles",
          interceptions = "Interceptions",
          pass_break_ups = "PBUs"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, guaranteed_pct, draft_overall))
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            css("padding-top" = "10px", "padding-bottom" = "10px", "padding-left" = "15px", "padding-right" = "0px")
          ),
          locations = cells_body(columns = c(team_logo_espn))
        ) |> 
        #tab_style(
        #  style = list(
        #    css("padding-left" = "13px", "padding-right" = "13px")
        #  ),
        #  locations = cells_body(columns = c(pick_prob, sos, win_pct, avg_pos, first_pick_prob, top_five_prob, top_ten_prob))
        #) |> 
        tab_style(
          style = cell_fill(color = "#F9F9F9"),
          locations = cells_body(
            rows = seq(1, 5, 2)
          ) 
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = -c(years, value, apy, guaranteed))
        ) |> 
        fmt_percent(columns = c(apy_cap_pct, guaranteed_pct), decimals = 2) |> 
        tab_source_note(
          source_note = md("")
        ) |> 
        data_color(
          columns = c(Score),
          target_columns = c(Score),
          colors = scales::col_numeric(
            palette = viridis::viridis(100),
            domain = c(0, 100)
          )
        )|> 
        tab_footnote(
          footnote = 
            "By: Cooper Davis (CDFBAnalysis) | Data via PFF, OTC, nflverse")
    } else if (input$position == "CB") {
      CB_Matched |> filter(Player_Source == input$Player) |> 
        arrange(-Score) |> 
        summarize(player = Match2, headshot_url, year = Match_year, position, Score, team_logo_espn, height, weight, age_at_signing,
                  contract_number, years, value, apy, apy_cap_pct, inflated_apy = round(inflated_apy, 2), guaranteed = round(guaranteed, 2), guaranteed_pct = round(guaranteed/value, 2), snaps, rolling_3_snaps, draft_overall,
                  forced_incompletes, 
                  grades_coverage_defense, pass_break_ups,
                  interceptions, yards_per_coverage_snap
        ) |> 
        gt() |> 
        gt_img_rows(team_logo_espn) |> 
        gt_img_rows(headshot_url) |>
        #tab_options(
        #  data_row.padding = px(1)
        #) |> 
        tab_header(
          title = "Top Contract Comparisons"
        ) |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          team_logo_espn = "",
          value = "Total",
          apy = "apy",
          apy_cap_pct = "apy%",
          inflated_apy = "Infl apy",
          contract_number = "Contr. #",
          guaranteed = "Guaranteed",
          guaranteed_pct = "Guaranteed %",
          snaps = "Snap Count",           
          rolling_3_snaps = "Tot. 3Y Snaps", 
          contract_number = "Contr. #",
          draft_overall = "Pick",
          grades_coverage_defense = "Cov. Grade",
          yards_per_coverage_snap = "YPS",
          forced_incompletes = "FINCs",
          interceptions = "Interceptions",
          pass_break_ups = "PBUs"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, guaranteed_pct, draft_overall))
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            css("padding-top" = "10px", "padding-bottom" = "10px", "padding-left" = "15px", "padding-right" = "0px")
          ),
          locations = cells_body(columns = c(team_logo_espn))
        ) |> 
        #tab_style(
        #  style = list(
        #    css("padding-left" = "13px", "padding-right" = "13px")
        #  ),
        #  locations = cells_body(columns = c(pick_prob, sos, win_pct, avg_pos, first_pick_prob, top_five_prob, top_ten_prob))
        #) |> 
        tab_style(
          style = cell_fill(color = "#F9F9F9"),
          locations = cells_body(
            rows = seq(1, 5, 2)
          ) 
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = -c(years, value, apy, guaranteed))
        ) |> 
        fmt_percent(columns = c(apy_cap_pct, guaranteed_pct), decimals = 2) |> 
        tab_source_note(
          source_note = md("")
        ) |> 
        data_color(
          columns = c(Score),
          target_columns = c(Score),
          colors = scales::col_numeric(
            palette = viridis::viridis(100),
            domain = c(0, 100)
          )
        )|> 
        tab_footnote(
          footnote = 
            "By: Cooper Davis (CDFBAnalysis) | Data via PFF, OTC, nflverse")
    }  else if (input$position == "TE") {
      TE_Matched |> filter(Player_Source == input$Player) |> 
        arrange(-Score) |> 
        summarize(player = Match2, headshot_url, year = Match_year, position, Score, team_logo_espn, height, weight, age_at_signing,
                  contract_number, years, value, apy, apy_cap_pct, inflated_apy = round(inflated_apy, 2), guaranteed = round(guaranteed, 2), guaranteed_pct = round(guaranteed/value, 2), snaps, rolling_3_snaps, draft_overall,
                  grades_offense, receptions, targets, yards, touchdowns, grades_pass_block, grades_run_block) |> 
        gt() |> 
        gt_img_rows(team_logo_espn) |> 
        gt_img_rows(headshot_url) |>
        #tab_options(
        #  data_row.padding = px(1)
        #) |> 
        tab_header(
          title = "Top Contract Comparisons"
        ) |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          team_logo_espn = "",
          Score = "Similarity",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          value = "Total",
          apy = "apy",
          apy_cap_pct = "apy%",
          inflated_apy = "Infl apy",
          contract_number = "Contr. #",
          guaranteed = "Guaranteed",
          guaranteed_pct = "Guaranteed %",
          snaps = "Snap Count",           
          rolling_3_snaps = "Tot. 3Y Snaps", 
          draft_overall = "Pick",
          grades_offense = "PFF Grade",
          receptions = "Rec",
          targets = "Tgt", 
          yards = "Yds", 
          touchdowns = "TDs", 
          grades_pass_block = "PBG", 
          grades_run_block = "RBG"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, guaranteed_pct, grades_offense))
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            css("padding-top" = "10px", "padding-bottom" = "10px", "padding-left" = "15px", "padding-right" = "0px")
          ),
          locations = cells_body(columns = c(team_logo_espn))
        ) |> 
        #tab_style(
        #  style = list(
        #    css("padding-left" = "13px", "padding-right" = "13px")
        #  ),
        #  locations = cells_body(columns = c(pick_prob, sos, win_pct, avg_pos, first_pick_prob, top_five_prob, top_ten_prob))
        #) |> 
        tab_style(
          style = cell_fill(color = "#F9F9F9"),
          locations = cells_body(
            rows = seq(1, 5, 2)
          ) 
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = -c(years, value, apy, guaranteed))
        ) |> 
        fmt_percent(columns = c(apy_cap_pct, guaranteed_pct), decimals = 2) |> 
        tab_source_note(
          source_note = md("")
        ) |> 
        data_color(
          columns = c(Score),
          target_columns = c(Score),
          colors = scales::col_numeric(
            palette = viridis::viridis(100),
            domain = c(0, 100)
          )
        )|> 
        tab_footnote(
          footnote = 
            "By: Cooper Davis (CDFBAnalysis) | Data via PFF, OTC, nflverse")
    }  else if (input$position == "WR") {
      WR_Matched |> filter(Player_Source == input$Player) |> 
        arrange(-Score) |> 
        summarize(player = Match2, headshot_url, year = Match_year, position, Score, team_logo_espn, height, weight, age_at_signing,
                  contract_number, years, value, apy, apy_cap_pct, inflated_apy = round(inflated_apy, 2), guaranteed = round(guaranteed, 2), guaranteed_pct = round(guaranteed/value, 2), snaps, rolling_3_snaps, draft_overall,
                  grades_offense, receptions, targets, yards, touchdowns, yprr, avg_depth_of_target, slot_rate) |> 
        gt() |> 
        gt_img_rows(team_logo_espn) |> 
        gt_img_rows(headshot_url) |>
        #tab_options(
        #  data_row.padding = px(1)
        #) |> 
        tab_header(
          title = "Top Contract Comparisons"
        ) |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          team_logo_espn = "",
          Score = "Similarity",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          value = "Total",
          apy = "apy",
          apy_cap_pct = "apy%",
          inflated_apy = "Infl apy",
          contract_number = "Contr. #",
          guaranteed = "Guaranteed",
          guaranteed_pct = "Guaranteed %",
          snaps = "Snap Count",           
          rolling_3_snaps = "Tot. 3Y Snaps", 
          draft_overall = "Pick",
          grades_offense = "PFF Grade",
          receptions = "Rec",
          targets = "Tgt", 
          yards = "Yds", 
          touchdowns = "TDs", 
          slot_rate = "SLT%", 
          yprr = "YPRR", 
          avg_depth_of_target = "ADOT"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, guaranteed_pct, grades_offense))
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            css("padding-top" = "10px", "padding-bottom" = "10px", "padding-left" = "15px", "padding-right" = "0px")
          ),
          locations = cells_body(columns = c(team_logo_espn))
        ) |> 
        #tab_style(
        #  style = list(
        #    css("padding-left" = "13px", "padding-right" = "13px")
        #  ),
        #  locations = cells_body(columns = c(pick_prob, sos, win_pct, avg_pos, first_pick_prob, top_five_prob, top_ten_prob))
        #) |> 
        tab_style(
          style = cell_fill(color = "#F9F9F9"),
          locations = cells_body(
            rows = seq(1, 5, 2)
          ) 
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = -c(years, value, apy, guaranteed))
        ) |> 
        fmt_percent(columns = c(apy_cap_pct, guaranteed_pct), decimals = 2) |> 
        tab_source_note(
          source_note = md("")
        ) |> 
        data_color(
          columns = c(Score),
          target_columns = c(Score),
          colors = scales::col_numeric(
            palette = viridis::viridis(100),
            domain = c(0, 100)
          )
        )|> 
        tab_footnote(
          footnote = 
            "By: Cooper Davis (CDFBAnalysis) | Data via PFF, OTC, nflverse")
    } else if (input$position == "RB") {
      RB_Matched |> filter(Player_Source == input$Player) |> 
        arrange(-Score) |> 
        summarize(player = Match2, headshot_url, year = Match_year, position, Score, team_logo_espn, height, weight, age_at_signing,
                  contract_number, years, value, apy, apy_cap_pct, inflated_apy = round(inflated_apy, 2), guaranteed = round(guaranteed, 2), guaranteed_pct = round(guaranteed/value, 2), snaps, rolling_3_snaps, draft_overall,
                  grades_offense, attempts, rushing_yards, rec_yards, targets, touchdowns,) |> 
        gt() |> 
        gt_img_rows(team_logo_espn) |> 
        gt_img_rows(headshot_url) |>
        #tab_options(
        #  data_row.padding = px(1)
        #) |> 
        tab_header(
          title = "Top Contract Comparisons"
        ) |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          team_logo_espn = "",
          Score = "Similarity",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          value = "Total",
          apy = "apy",
          apy_cap_pct = "apy%",
          inflated_apy = "Infl apy",
          contract_number = "Contr. #",
          guaranteed = "Guaranteed",
          guaranteed_pct = "Guaranteed %",
          snaps = "Snap Count",           
          rolling_3_snaps = "Tot. 3Y Snaps", 
          draft_overall = "Pick",
          grades_offense = "PFF Grade",
          targets = "Tgt", 
          rushing_yards = "Rush Yds", 
          touchdowns = "TDs", 
          attempts = "Att", 
          rec_yards = "Rec Yards", 
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, guaranteed_pct, grades_offense))
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            css("padding-top" = "10px", "padding-bottom" = "10px", "padding-left" = "15px", "padding-right" = "0px")
          ),
          locations = cells_body(columns = c(team_logo_espn))
        ) |> 
        #tab_style(
        #  style = list(
        #    css("padding-left" = "13px", "padding-right" = "13px")
        #  ),
        #  locations = cells_body(columns = c(pick_prob, sos, win_pct, avg_pos, first_pick_prob, top_five_prob, top_ten_prob))
        #) |> 
        tab_style(
          style = cell_fill(color = "#F9F9F9"),
          locations = cells_body(
            rows = seq(1, 5, 2)
          ) 
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = -c(years, value, apy, guaranteed))
        ) |> 
        fmt_percent(columns = c(apy_cap_pct, guaranteed_pct), decimals = 2) |> 
        tab_source_note(
          source_note = md("")
        ) |> 
        data_color(
          columns = c(Score),
          target_columns = c(Score),
          colors = scales::col_numeric(
            palette = viridis::viridis(100),
            domain = c(0, 100)
          )
        )|> 
        tab_footnote(
          footnote = 
            "By: Cooper Davis (CDFBAnalysis) | Data via PFF, OTC, nflverse")
    }  else if (input$position == "QB") {
      QB_Matched |> filter(Player_Source == input$Player) |> 
        arrange(-Score) |> 
        summarize(player = Match2, headshot_url, year = Match_year, position, Score, team_logo_espn, height, weight, age_at_signing,
                  contract_number, years, value, apy, apy_cap_pct, inflated_apy = round(inflated_apy, 2), guaranteed = round(guaranteed, 2), guaranteed_pct = round(guaranteed/value, 2), snaps, rolling_3_snaps, draft_overall,
                  grades_offense, attempts, passing_yards, touchdowns, turnover_worthy_plays, sacks, rushing_yards) |> 
        gt() |> 
        gt_img_rows(team_logo_espn) |> 
        gt_img_rows(headshot_url) |>
        #tab_options(
        #  data_row.padding = px(1)
        #) |> 
        tab_header(
          title = "Top Contract Comparisons"
        ) |> 
        cols_label(
          player = "Name",
          headshot_url = "",
          position = "Pos",
          team_logo_espn = "",
          Score = "Similarity",
          height = "Height",
          weight = "Weight",
          age_at_signing = "Age",
          value = "Total",
          apy = "apy",
          apy_cap_pct = "apy%",
          inflated_apy = "Infl apy",
          contract_number = "Contr. #",
          guaranteed = "Guaranteed",
          guaranteed_pct = "Guaranteed %",
          snaps = "Snap Count",           
          rolling_3_snaps = "Tot. 3Y Snaps", 
          draft_overall = "Pick",
          grades_offense = "PFF Grade",
          passing_yards = "Passing Yards",
          rushing_yards = "Rush Yds", 
          turnover_worthy_plays = "TWP",
          touchdowns = "TDs", 
          attempts = "Pass Att", 
          sacks = "Sacks Taken"
        ) |> 
        gt_theme_538() |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_borders(sides = "bottom", weight = px(2))
          ),
          locations = cells_column_labels(everything())
        ) |> 
        cols_align(
          align = "center",
          columns = everything()
        ) |> 
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            css("padding-top" = "10px", "padding-bottom" = "10px", "padding-left" = "15px", "padding-right" = "0px")
          ),
          locations = cells_body(columns = c(team_logo_espn))
        ) |> 
        tab_style(
          style = cell_borders(
            sides = "right",
            color = "lightgray", 
            weight = px(0.5)     
          ),
          locations = cells_body(columns = c(age_at_signing, rolling_3_snaps, guaranteed_pct, grades_offense))
        ) |> 
        #tab_style(
        #  style = list(
        #    css("padding-left" = "13px", "padding-right" = "13px")
        #  ),
        #  locations = cells_body(columns = c(pick_prob, sos, win_pct, avg_pos, first_pick_prob, top_five_prob, top_ten_prob))
        #) |> 
        tab_style(
          style = cell_fill(color = "#F9F9F9"),
          locations = cells_body(
            rows = seq(1, 5, 2)
          ) 
        ) |> 
        tab_style(
          style = list(cell_fill(color = "#013369"), cell_text(color = "white")),
          locations = list(cells_title(groups = "title"), cells_column_labels(everything()))
        ) |> 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(columns = -c(years, value, apy, guaranteed))
        ) |> 
        fmt_percent(columns = c(apy_cap_pct, guaranteed_pct), decimals = 2) |> 
        tab_source_note(
          source_note = md("")
        ) |> 
        data_color(
          columns = c(Score),
          target_columns = c(Score),
          colors = scales::col_numeric(
            palette = viridis::viridis(100),
            domain = c(0, 100)
          )
        )|> 
        tab_footnote(
          footnote = 
            "By: Cooper Davis (CDFBAnalysis) | Data via PFF, OTC, nflverse")
    }
    
  })
  
}

shinyApp(ui, server)

