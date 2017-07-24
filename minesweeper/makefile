SRC_DIR = src
OBJ_DIR = obj
HI_DIR = hi

EXECUTABLE = minesweeper

DEPS = $(wildcard $(SRC_DIR)/*.hs)

all : minesweeper

minesweeper : $(DEPS)
	ghc $(SRC_DIR)/* -odir $(OBJ_DIR) -hidir $(HI_DIR) -o $(EXECUTABLE)

run : $(EXECUTABLE)
	./$(EXECUTABLE)

.PHONEY : clean

clean :
	rm -f $(OBJ_DIR)/*.o
	rm -d $(OBJ_DIR)
	rm -f $(HI_DIR)/*.hi
	rm -d $(HI_DIR)
	rm -f $(EXECUTABLE)
