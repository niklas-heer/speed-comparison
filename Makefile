all: build up

build:
	docker build -t="niklas-heer/speed-comparison" .

up:
	docker run -it --rm niklas-heer/speed-comparison
