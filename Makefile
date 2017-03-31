.PHONY: build clean ghci watch
all: build

build: 
	stack build

clean:
	stack clean

# Watch for changes.
watch:
	stack build --file-watch --fast .

# Run ghci using stack.
ghci:
	stack ghci
