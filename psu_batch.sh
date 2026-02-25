#!/bin/bash

#SBATCH --account=open
#SBATCH --partition=open

a=51
while [ $a -le 51 ]
do 

    sbatch << EOJ
#!/bin/bash

#SBATCH --account=open
#SBATCH --partition=open
#SBATCH --job-name=M${a}
#SBATCH -o $PWD/log/M${a}.txt
#SBATCH -e $PWD/log/M${a}.log
#SBATCH --mail-type=FAIL
#SBATCH -n 21
#SBATCH -N 1
#SBATCH -t 48:00:00

cd $PWD

module load gcc/13.2.0 openmpi/5.0.7 R/4.4.0

mpirun -n 21 Rscript code/main_slurm.R ${a}

EOJ

((a++))
done

