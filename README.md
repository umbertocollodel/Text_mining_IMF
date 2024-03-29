# Text_mining_IMF


## Description

This repository makes available the code to produce all figures and tables in the paper: Betin, Collodel (2021). ["The Complex Crises Database: 70 Years of Macroeconomic Crises"](https://halshs.archives-ouvertes.fr/halshs-03268889/document).
Please cite us if you refer to our paper.

We scrape the IMF archives to obtain all country reports and program-related documents references. After downloading all documents and applying OCR to them to make them suitable for statistical analysis, we build a dictionary with 20 categories and a list of keywords referring to each one of them. We perform an iterative term-frequency for each category to construct a new database of crises discussion. Finally, we compare the resulting measures with established indicators to validate our approach and study how the correlation of these indicators evolves over time through network analysis. We find that crises have significantly complexified over time and that financial nodes became central in the network.

### Crises complexity from 1950-1970 to 2010-2019

<p float="left">
  <img src="https://user-images.githubusercontent.com/33840988/166907404-9dfc2fd4-60ee-481a-a4b7-9661b068fe83.png" width="300" />
  <img src="https://user-images.githubusercontent.com/33840988/166907413-318e5fb7-e1b3-41eb-a6fb-fc8d7c3b2fb7.png" width="450" /> 
</p>


## Authors

- Manuel Betin
- Umberto Collodel

## Language

R


## Organization

0. Main Source

1. Scraping IMF archives

2. Download documents and create OCR corpus

3. Perform validity checks on extraction

4. Analysis of resulting measures

5. Shiny app to display differences with benchmark (preliminary work)

6. Network analysis


The main sourcing file runs the entire project.
The main file of each section cleans the global environment, installs and loads the packages required and sources all the scripts in the section. The function file contains custom functions used in the sections. Individual files run the functions and export the output.


## License

The data and codes are under the MIT license. This means that you can use everything as you please for research or commercial purposes, as long as you refer back to us.

## Contributing

If you find irregularities or bugs, please open an issue here.
