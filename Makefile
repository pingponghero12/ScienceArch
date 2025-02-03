# vim: noet
#
SRC_DIR := app
BUILD_DIR := build

all: $(BUILD_DIR)
	cd $(BUILD_DIR) && ghc -o sa -threaded -hidir . -odir . ../$(SRC_DIR)/ConnectDB.hs ../$(SRC_DIR)/FunctionsDB.hs ../$(SRC_DIR)/Main.hs ../$(SRC_DIR)/RenderUI.hs

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)

run: all
	./$(BUILD_DIR)/sa

.PHONY: all clean run
