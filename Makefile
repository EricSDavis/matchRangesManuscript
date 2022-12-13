.PHONY: clean

objects :=\
	data/benchmarkResults.rds\
	tables/benchmarkResults.txt\
	figures/supplementaryFigure1.pdf\
	figures/figure1.pdf\
	data/lowResults.rds\
	data/mediumResults.rds\
	data/highResults.rds\
	data/benchmarkResults2.rds\
	figures/supplementaryFigure1_v2.pdf\
	tables/benchmarkResults2.txt\
	figures/supplementaryFigure1_v2.pdf\
	figures/supplementaryFigureX.png\
	figures/supplementaryFigureX.pdf

all: $(objects)

clean:
	rm -rf $(objects)

data/benchmarkResults.rds:\
	scripts/makeExampleData.R\
	scripts/runBenchmark.R
		mkdir -p data
		Rscript scripts/runBenchmark.R

tables/benchmarkResults.txt\
figures/supplementaryFigure1.pdf:\
	data/benchmarkResults.rds\
	scripts/supplementaryFigure1.R
		mkdir -p tables figures
		Rscript scripts/supplementaryFigure1.R

figures/figure1.pdf:\
	scripts/figure1.R
		mkdir -p figures
		Rscript scripts/figure1.R
		
data/lowResults.rds\
data/mediumResults.rds\
data/highResults.rds\
data/benchmarkResults2.rds:\
	scripts/makeExampleData.R\
	scripts/runBenchmark2.R
		mkdir -p data
		Rscript scripts/runBenchmark2.R

tables/benchmarkResults2.txt\
figures/supplementaryFigure1_v2.pdf:\
	data/benchmarkResults2.rds\
	scripts/supplementaryFigure1_v2.R
		mkdir -p tables figures
		Rscript scripts/supplementaryFigure1_v2.R
	
figures/supplementaryFigure1_v3.pdf:\
	data/benchmarkResults2.rds\
	scripts/supplementaryFigure1_v2.R
		mkdir -p figures
		Rscript scripts/supplementaryFigure1_v2.R

figures/supplementaryFigureX.png\
figures/supplementaryFigureX.pdf:\
	scripts/makeExampleData.R\
	scripts/compareBalance.R
		mkdir -p figures
		Rscript scripts/compareBalance.R