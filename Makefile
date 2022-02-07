IDRIS ?= idris2
SRC_DIR = src
TRGT = buffered-channels
IDR_FILES := $(SRC_DIR)/System/Concurrency/Queue.idr
IDR_FILES += $(SRC_DIR)/System/Concurrency/BufferedChannel.idr
IDR_FILES += $(SRC_DIR)/System/Concurrency/Pipe.idr
IPKG_FILE = $(TRGT).ipkg

all: $(TRGT)

build: $(TRGT)

$(TRGT): $(IDR_FILES)
	$(IDRIS) --build $(IPKG_FILE)

install: $(TRGT)
	$(IDRIS) --install $(IPKG_FILE)

.PHONY: all build clean

clean:
	$(RM) -r build
	$(RM) -r src/build

