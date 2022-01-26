.PHONY: client
client:
	go build -o bin/client ./cmd/client

.PHONY: example
example:
	go build -o bin/example ./cmd/fcheck-example

.PHONY: tracing
tracing:
	go build -o bin/tracing ./cmd/tracing-server

.PHONY: all
all: client tracing example

.PHONY: clean
clean:
	rm -rf bin/*
