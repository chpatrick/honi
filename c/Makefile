.PHONY: runPrintFrameInfo
runPrintFrameInfo: printFrameInfo
	LD_LIBRARY_PATH=$(HOME)/openni/OpenNI2/Bin/x64-Release:$(HOME)/openni/OpenNI2/Bin/x64-Release/OpenNI2/Drivers ./printFrameInfo

printFrameInfo: printFrameInfo.c
	clang -I$(HOME)/openni/OpenNI2/Include -L$(HOME)/openni/OpenNI2/Bin/x64-Release -L$(HOME)/openni/OpenNI2/Bin/x64-Release/OpenNI2/Drivers -lOpenNI2 -lfreenect printFrameInfo.c -o printFrameInfo
