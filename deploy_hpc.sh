#!/bin/bash

#SBATCH --job-name='deployCTD'
#SBATCH --partition='debug'
#SBATCH --mail-user=Amira.Burns@usda.gov
#SBATCH --mail-type=BEGIN,END,FAIL

module purge

module load r/4.2.0

cd /project/cameratrapdetector

Rscript deploy_model_hpc.R -d /90daydata/cameratrapdetector/minitrain -o /project/camertrapdetector/minitrain/ctd_deploy_out/ --sample50 TRUE