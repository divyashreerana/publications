The available data and code has been used in the manuscript titled "Genetic Erosion at the Edge: Landscape fragmentation and connectivity in sloth bears of the Indian terai" submitted to bioRxiv in September 2025.

The description of attached files are as follows:

Codes:
Linux_codes - Bioinformatic codes to check and trim raw fastq files, mapping to reference genome, making fastq file of mapped reads for ipyrad pipeline, and lastly VCF filtering, 1.Identifying_population_structure - a. Indetifying genotypes and recaptures, b. Identifying optimal K, c. Plotting admixture for optimal K spatially, d. Plotting custom admixture, e. Making supervised DAPC, f. Estimating population genetic measures
2.Preparing_landscape_data - a. Raster data preparation at different spatial scales, b. Fixing rasters and writing in ascii, c. Checking correlation between rasters
3.Landscape_analyses - a. Preparing genetic and geographic data, b. Checking correlation between genetic and geographic distance, c. Single surface optimization, d. Multiple surface optimization, e. Preparing circuitscape input and plotting output
*ipyrad parameters are outlined in the supplementary material.

Data:
allsb_sunbear_mac3_964x66_60indmiss_40snpmiss.vcf - filtered vcf with unique individuals
agriculture_density_utm1km_fixed.tif, dist_settlement_utm1km_fixed.tif, distance_water_utm1km_fixed.tif, evi_utm1km_fixed.tif, road_density_utm1km_fixed.tif - uncorrelated landscape rasters used in landscape genetic analyses
