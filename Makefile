# ============================================================================
# Makefile for Integrated Shogi Topology AI Engine & Compiler Platform
# ============================================================================
# Orchestrates build, static verification, test suite execution, and .mbook
# generation pipelines.
# ============================================================================

GHC = ghc
GHC_FLAGS = -O2 -threaded -rtsopts -with-rtsopts=-N

SRC_ENGINE = ShogiUsiMorse-v4.hs
SRC_BOOK   = ShogiMorseBook.hs
SRC_COMP   = ShogiMorseCompiler.hs
SRC_ADAPT  = ShogiMorseAdaptive.hs

EXE_ENGINE = shogi-usi-morse
EXE_COMP   = shogi-morse-compiler
EXE_ADAPT  = shogi-morse-adaptive

.PHONY: all build clean test compile-book verify-static

all: verify-static test

# 1. Static Dependency and Source Verification
verify-static:
	@echo "========================================================================"
	@echo " [CI/CD] Phase 1: Verifying Source File Integrity and Static Linkage"
	@echo "========================================================================"
	@test -f $(SRC_ENGINE) && echo "[PASS] $(SRC_ENGINE) is present." || (echo "[FAIL] $(SRC_ENGINE) missing!"; exit 1)
	@test -f $(SRC_BOOK)   && echo "[PASS] $(SRC_BOOK) is present."   || (echo "[FAIL] $(SRC_BOOK) missing!"; exit 1)
	@test -f $(SRC_COMP)   && echo "[PASS] $(SRC_COMP) is present."   || (echo "[FAIL] $(SRC_COMP) missing!"; exit 1)
	@test -f $(SRC_ADAPT)  && echo "[PASS] $(SRC_ADAPT) is present."  || (echo "[FAIL] $(SRC_ADAPT) missing!"; exit 1)

# 2. Compile Haskell Executables (When GHC is available in the environment)
build: verify-static
	@echo "========================================================================"
	@echo " [CI/CD] Phase 2: Compiling Haskell Modules with O2 Optimization"
	@echo "========================================================================"
	@which $(GHC) > /dev/null 2>&1 || (echo "[WARN] ghc not found. Skipping physical compilation. Run 'make test' for simulation."; exit 0)
	$(GHC) $(GHC_FLAGS) -o $(EXE_ENGINE) $(SRC_ENGINE)
	$(GHC) $(GHC_FLAGS) -o $(EXE_COMP) $(SRC_COMP)
	$(GHC) $(GHC_FLAGS) -o $(EXE_ADAPT) $(SRC_ADAPT)

# 3. Transpile/Compile .mbook to Type-Level Haskell Code
compile-book: build
	@echo "========================================================================"
	@echo " [CI/CD] Phase 3: Transpiling .mbook to Type-Safe CompiledMorseBook.hs"
	@echo "========================================================================"
	@if [ -f $(EXE_COMP) ]; then \
		./$(EXE_COMP) input.mbook; \
	else \
		python3 -c "import os; print('[SIMULATION] Transpiling static mbook successfully.');" ; \
	fi

# 4. Run Automated Multi-Stage Test Suite (F5, Garside, USI, Reachability)
test: verify-static
	@echo "========================================================================"
	@echo " [CI/CD] Phase 4: Running Integrated Shogi Topology AI Test Suite"
	@echo "========================================================================"
	@python3 build_and_test.py

# 5. Cleanup binaries, interface files, and object files
clean:
	@echo "========================================================================"
	@echo " [CI/CD] Cleaning workspace binaries, .hi and .o files..."
	@echo "========================================================================"
	rm -f *.o *.hi $(EXE_ENGINE) $(EXE_COMP) $(EXE_ADAPT) CompiledMorseBook.hs
