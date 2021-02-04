# Census-Block-Group-Level-2016-Presidential-Election-Results
A dataset ('votes2016_byCbg.csv.gz') with estimates of Census Block Group level election results, and the underlying data and code that generated it. 

Derived from the [dataset](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NH5S2I) compiled by the Voting and Election Science Team at the University of Florida and Wichita State University. Vote counts are allocated to census block groups based on the share of the precincts' areas that overlap with the given CBG. A folder with all CBG shapefiles can be downloaded [here](https://www.dropbox.com/sh/7e21bjgtt08ajw6/AABgbldYCfNUkBUKZMPj7lCfa?dl=0). For a description of the matching algorithm, see Van Dijcke and Wright (2021). 

Please cite: 

Van Dijcke, David and Wright, Austin L., Profiling Insurrection: Characterizing Collective Action Using Mobile Device Data (January 31, 2021). Available at SSRN: https://ssrn.com/abstract=3776854

Voting and Election Science Team. 2018. “2016 Precinct-Level Election Results.”.
URL: https://doi.org/10.7910/DVN/NH5S2I


| variable name     | description                                                                 |
|-------------------|--------------------------------------------------------------------         |
| poi_cbg           | Census Block Group ID. Make sure to pad it with zeros on the left!          |
| trump_share       | Share of Trump votes in total CBG population                                |
| clinton_share     | Share of Clinton votes in total CBG population                              |
| trump_share_votes | Share of Trump votes in total votes cast                                    |
| clinton_share     | Share of Clinton votes in total votes cast                                  |
| g16prertru        | Estimated votes cast for Trump in CBG                                       |
| g16predcli        | Estimated votes cast for Clinton in CBG                                     |
| trump_neighbor    | Total votes for Trump in neighboring CBGs / total votes in neighboring CBGS |
| clinton_neighbor  | Same for Clinton votes                                                      |
| trump_neighbor_avg| Average trump vote share in neighboring CBGs (and same for Clinton          |
