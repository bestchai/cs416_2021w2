.PHONY: client tracing clean all

all: server coord client tracing

server:
	go build -o bin/server ./cmd/server

coord:
	go build -o bin/coord ./cmd/coord

client:
	go build -o bin/client ./cmd/client

tracing:
	go build -o bin/tracing ./cmd/tracing-server

clean:
	rm -f bin/*
