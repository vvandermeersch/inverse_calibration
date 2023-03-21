#!/bin/sh
#SBATCH --mail-type=ALL
#SBATCH --mail-user=victor.vandermeersch@cefe.cnrs.fr
#SBATCH --job-name=CMAES1
#SBATCH --output=/scratch/vvandermeersch/phenofit_calibration/logs/CMAES_log.txt
#SBATCH --cpus-per-task=30
#SBATCH --mem=80G
#SBATCH --time 1-12:00:00

. /local/env/envconda.sh

conda activate /home/genouest/mnhn_cesco/vvandermeersch/env/env_capsis_cmaes

Rscript calibration.R
