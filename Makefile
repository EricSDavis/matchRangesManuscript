.PHONY: clean

objects :=\
	data/benchmarkResults.rds\
	tables/benchmarkResults.txt\
	figures/supplementaryFigure1.pdf

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
		