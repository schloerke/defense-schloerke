
all: compile

pirate:
	Rscript code.R

compile:
	make dotbuild

dotbuild:
	dot "cogs.dot" -Tpng -o "imgs/cogs.png"

test:
	echo "test!"
