http://scholarscompass.vcu.edu/hist_data/1/

Document Type
Dataset
Publication Date
2015
Description
7 data files in CSV and SQL format, presented in a zip file as the primary download and individually as supplemental content.
Abstract
Data to support the "Mapping the Second Ku Klux Klan, 1919-1940" project.
http://labs.library.vcu.edu/klan/
The article, "Publicity and Prejudice: The New York World’s Exposé of 1921 and the History of the Second Ku Klux Klan", provides additional context to the data:
http://scholarscompass.vcu.edu/hist_pubs/12/
Data
The zip download file contains 4 CSV (comma separated value) files and 3 SQL (structured query language) files that will help researchers recreate the project database that was compiled from the research notes of Dr. John Kneebone.
The CSV files can be opened using any spreadsheet software including Microsoft Excel. The SQL files can be opened using Excel or database software such as Microsoft Access.
For a quick analysis, the list of all Klaverns, minus the data sources, can be found in klaverns_all_sans_sources.csv. To recreate the full database structure, use the remaining CSV or SQL files.
The geospatial coordinates are approximate town/city centers as dictated by Google Maps and/or Wikipedia.
Useful SQL queries:
Query to return all klaverns by state:
SELECT * FROM `klavern` JOIN sources ON klavern.id = sources.klavern_id JOIN states ON states.id = klavern.state_id WHERE states.state = "";
Query to return all klaverns founded by a certain year:
SELECT * FROM `klavern` WHERE klavern.year =
Research compiled by Dr. John Kneebone. Database export and quality assurance by Shariq Torres.
Last data update: 12/1/2015
File Format
CSV, SQL
Rights
CC BY-NC-SA 3.0 https://creativecommons.org/licenses/by-nc-sa/3.0/
Is Part Of
VCU History Data
Date of Submission
11-2015

Date of Submission
November 2015
Abstract
In September 1921 the New York World published a series of articles exposing the Knights of the Ku Klux Klan, Inc., which had just started recruiting members across the country. Nearly two-dozen other newspapers carried the series, and the Klan became national news. Historians have argued that the World’s series had the ironic effect of publicizing the Klan to the nation, leading directly to the Klan’s millions of members a few years later. This article proposes that the World actually aroused opposition to the Klan, put forward arguments against the Klan that others would also use, and caused the Klan to shift its appeal away from vigilantism to politics. The World’s exposure of the Klan was more successful than the Klan’s later numbers seem to indicate.