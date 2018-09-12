#!/bin/bash

#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --job-name=mult-var-fit
#SBATCH --mem=3G
#SBATCH --partition=verysmallmem
#SBATCH --mail-user=therimalaya@gmail.com
#SBATCH --mail-type=END

module load gcc/6.2.0
module load R/3.5.0

Rscript scripts/02-simulation.r $SLURM_ARRAY_TASK_ID
