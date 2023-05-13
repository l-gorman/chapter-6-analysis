#!/bin/bash

#SBATCH --job-name=simulation
#SBATCH -o ./Report/output.simulation_only.%a.out
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --time=0-6:00:00
#SBATCH --mem=12G
#SBATCH --account=sscm012844
#SBATCH --array=1-3

cd "${SLURM_SUBMIT_DIR}"

echo "Running on host $(hostname) \n"
echo "Time is $(date) \n"
echo "Directory is $(pwd) \n"
echo "Slurm job ID is ${SLURM_JOBID} \n"
echo "This jobs runs on the following machines: \n"
echo "${SLURM_JOB_NODELIST}"

echo "Keep track of job by entering sacct -j ${SLURM_JOBID}  \n"
echo "Cancel your job by entering scancel ${SLURM_JOBID}  \n"
echo "More details on submitting jobs here https://www.acrc.bris.ac.uk/protected/hpc-docs/job_types/ \n"

module add languages/r/4.1.0

out_directory="/user/work/lg14410/chapter-6/outputs/"
iterations=4000
warmup=2000
cores=4

Rscript "./src/05-simulation/01-simulation.R" -i $iterations -w $warmup  -o $out_directory -c $cores -j ${SLURM_ARRAY_TASK_ID}

unset out_directory
unset iterations
unset warmup
unset cores
