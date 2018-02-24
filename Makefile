all: build up

build:
	docker build -t="niklas-heer/speed-comparison" .

up:
	docker run -v "$$PWD/results":/usr/src/app/results -it --rm niklas-heer/speed-comparison
