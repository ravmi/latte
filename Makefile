MKDIR_P = mkdir -p
BUILD_DIR = build
LATTE_LIBS = ${BUILD_DIR}/ASM.hs ${BUILD_DIR}/QuadData.hs ${BUILD_DIR}/ImdLatte.hs \
			 ${BUILD_DIR}/NextUses.hs ${BUILD_DIR}/GarbageCollector.hs \
			 ${BUILD_DIR}/CFG.hs ${BUILD_DIR}/LatteErrors.hs

.PHONY: directories clean all ${LATTE_LIBS}

all: directories latc_x86_64

directories: ${BUILD_DIR}

${BUILD_DIR}: 
	${MKDIR_P} ${BUILD_DIR}

latc_x86_64: lat_to_asm_bin programs
	cp src/latc_x86_64 latc_x86_64

lat_to_asm_bin: ${BUILD_DIR}/latte.hs ${BUILD_DIR}/LexLatte.hs ${BUILD_DIR}/ParLatte.hs ${LATTE_LIBS}
	cd ${BUILD_DIR}; ghc --make -ibuild latte.hs -o lat_to_asm_bin; mv lat_to_asm_bin ../

programs:
	${MKDIR_P} programs

${BUILD_DIR}/ASM.hs: src/ASM.hs
	cp src/ASM.hs ${BUILD_DIR}

${BUILD_DIR}/QuadData.hs: src/QuadData.hs
	cp src/QuadData.hs ${BUILD_DIR}

${BUILD_DIR}/ImdLatte.hs: src/ImdLatte.hs
	cp src/ImdLatte.hs ${BUILD_DIR}

${BUILD_DIR}/NextUses.hs: src/NextUses.hs
	cp src/NextUses.hs ${BUILD_DIR}

${BUILD_DIR}/GarbageCollector.hs: src/GarbageCollector.hs
	cp src/GarbageCollector.hs ${BUILD_DIR}

${BUILD_DIR}/CFG.hs: src/CFG.hs
	cp src/CFG.hs ${BUILD_DIR}

${BUILD_DIR}/LatteErrors.hs: src/LatteErrors.hs
	cp src/LatteErrors.hs ${BUILD_DIR}

${BUILD_DIR}/latte.hs: src/latte.hs
	cp src/latte.hs ${BUILD_DIR}

${BUILD_DIR}/LexLatte.hs: ${BUILD_DIR}/LexLatte.x
	cd ${BUILD_DIR}; alex -g LexLatte.x

${BUILD_DIR}/LexLatte.x: ${BUILD_DIR}/Latte.cf
	cd ${BUILD_DIR}; bnfc Latte.cf

${BUILD_DIR}/Latte.cf:
	cp src/Latte.cf ${BUILD_DIR}

${BUILD_DIR}/ParLatte.hs: ${BUILD_DIR}/ParLatte.y
	cd ${BUILD_DIR}; happy -gca ParLatte.y

clean:
	@rm -f lat_to_asm_bin latc_x86_64;
	@rm -rf build;
	@rm -rf programs;
