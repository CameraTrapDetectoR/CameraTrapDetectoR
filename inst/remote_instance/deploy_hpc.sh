#!/bin/bash

#SBATCH --job-name='deployCTD'
#SBATCH --partition='partition'
#SBATCH --mail-user=First.Last@myemail.com
#SBATCH --mail-type=BEGIN,END,FAIL

module purge

module load r/4.2.0

# navigate to dir where your R script is stored
cd /project/cameratrapdetector

# run R script with user arguments specified
Rscript deploy_model_hpc.R -d /90daydata/cameratrapdetector/minitrain -o /project/camertrapdetector/minitrain/ctd_deploy_out/ --sample50 TRUE