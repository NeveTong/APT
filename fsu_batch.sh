#!/bin/bash

a=41
while [ $a -le 42 ]
do 

    sbatch << EOJ
#!/bin/bash

#SBATCH --job-name=M${a}
#SBATCH -o $PWD/log/M${a}.txt
#SBATCH -e $PWD/log/M${a}.log
#SBATCH --mail-type=FAIL
#SBATCH -n 11
#SBATCH --mem=64G
#SBATCH -p genacc_q
#SBATCH -t 12:00:00

cd $PWD

module purge
module load gnu/13.2.0 openmpi/4.1.6 R/4.4.0

mpirun -n 11 Rscript code/main_slurm.R ${a}

EOJ

((a++))
done

