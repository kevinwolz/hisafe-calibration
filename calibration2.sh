#!/bin/sh

IDS = 1:1000

for(plot in c("A2", "A3", "A4")) {
	mv ./*plot*_simulation ./Capsis/data

	for (id in IDS) {

	cp ./tree_files/walnut-hyrbid_i.tree ./A2_simulation/treeSpecies/walnut-hyrbid.tree
	
	sbatch A2_simulation.sh
	WAIT UNTIL ALL SIMULATIONS ARE COMPLETE ## HOW TO DO THIS?
	
	mv ./A2_simulation/output-simulation ./A2_output/A2_output_i
	
	mv ./slurm-xxxxx.out ./A2_logs/A2_log_i.out ## HOW TO KNOW SLURM ID??	

	}
}


## FOLDER STRUCTURE
	A2_simulation 
		(hisafe folder strucutre)
		A2.sim
		A2.pld
		A2.wth
	A3_simulation 
		(hisafe folder strucutre)
		A3.sim
		A3.pld
		A3.wth
	A4_simulation 
		(hisafe folder strucutre)
		A4.sim
		A4.pld
		A4.wth

	tree_files
		walnut-hybrid_1.tree
		walnut-hybrid_2.tree
		walnut-hybrid_3.tree

	A2_output
		A2_output_1
		A2_output_2
		A2_output_3
	A3_output
		A3_output_1
		A3_output_2
		A3_output_3
	A4_output
		A4_output_1
		A4_output_2
		A4_output_3

	A2_logs
		A2_log_1.out
		A2_log_2.out
		A2_log_3.out
	A3_logs
		A3_log_1.out
		A3_log_2.out
		A3_log_3.out
	A4_logs
		A4_log_1.out
		A4_log_2.out
		A4_log_3.out