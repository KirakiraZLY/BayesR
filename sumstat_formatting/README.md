# Summary Statistics Formatting

The GWAS summary statistics maintains a lot different formats, to include the different information included within the datasets from a wide range of sources as possible. However, for the use of PRS, it is somehow a headache and need to do some works to change the summary statistics into the format that the PRS tool needs.   

A standard PRS tool computes with the information of SNP id, effect and non-effect allele effect size, and sample size. For example, in BayesR, we require a summary statistics file contain columns: **Predictor, A1, A2, n, Z**    

We provide a formatting programme which was implemented in R. We hope you can enjoy using this tool. And if you have any questions or bug reports, please contact  us.   

## Requirement

To run this programme, we require you have **tidyverse** and **dplyr** installed in your R package.

You can install it via the following commands:   

<code>conda install -c r r-tidyverse</code>  
<code>conda install -c r r-dplyr</code>

## How to Run

<code>./ss_to_ldak_format.R --inputFile [Sumstat]  --outputFile [Output]  --bfile [Genotype] </code> 

Sumstat: Your summary statistics with full name and the path.   
Output: Output name with the directory, after running it will generate the file with name **[Output].ldak**   
Genotype: Your genotype file in plink format.   