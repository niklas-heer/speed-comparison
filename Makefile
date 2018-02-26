all: build up

build:
	docker build -t="niklas-heer/speed-comparison" .

up:
	docker run -v "$$PWD/results":/usr/src/app/results -it --rm niklas-heer/speed-comparison

plot:
	docker run -v "$$PWD":/usr/src/app -it --rm niklas-heer/speed-comparison /usr/bin/Rscript plot.r
