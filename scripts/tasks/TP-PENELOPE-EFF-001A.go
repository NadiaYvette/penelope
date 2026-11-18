package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()

	if err := os.MkdirAll(filepath.Dir(dst), 0755); err != nil {
		return err
	}

	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()

	_, err = io.Copy(out, in)
	return err
}

func findSeed(name string) string {
	candidates := []string{
		filepath.Join("seeds", name),
		filepath.Join(os.Getenv("HOME"), "src/plans/seeds", name),
		filepath.Join("..", "seeds", name),
		filepath.Join("/home/nyc/src/plans/seeds", name),
	}
	for _, c := range candidates {
		if _, err := os.Stat(c); err == nil {
			return c
		}
	}
	return filepath.Join("seeds", name)
}

func main() {
	fmt.Println("=== [TP-PENELOPE-EFF-001A Task Runner (Go)] ===")

	// 1. Copy Interpreter.hs
	srcInterp := findSeed("TP-PENELOPE-EFF-001A-Interpreter.hs")
	dstInterp := "packages/penelope-effects/src/Penelope/Effects/Interpreter.hs"
	if err := copyFile(srcInterp, dstInterp); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcInterp, dstInterp, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstInterp)

	// 2. Update penelope-effects.cabal
	cabalPath := "packages/penelope-effects/penelope-effects.cabal"
	cabalBytes, err := os.ReadFile(cabalPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	cabalStr := string(cabalBytes)
	if !strings.Contains(cabalStr, "Penelope.Effects.Interpreter") {
		cabalStr = strings.Replace(cabalStr, "    Penelope.Effects.Concurrent\n", "    Penelope.Effects.Concurrent\n    Penelope.Effects.Interpreter\n", 1)
		if err := os.WriteFile(cabalPath, []byte(cabalStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", cabalPath, err)
			os.Exit(1)
		}
		fmt.Printf("  [+] Updated %s (exposed Penelope.Effects.Interpreter)\n", cabalPath)
	}

	// 3. Update packages/penelope-effects/src/Penelope/Effects.hs
	effectsHsPath := "packages/penelope-effects/src/Penelope/Effects.hs"
	effBytes, err := os.ReadFile(effectsHsPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", effectsHsPath, err)
		os.Exit(1)
	}
	effStr := string(effBytes)
	if !strings.Contains(effStr, "Penelope.Effects.Interpreter") {
		effStr = strings.Replace(effStr, "  , module Penelope.Effects.Concurrent\n", "  , module Penelope.Effects.Concurrent\n  , module Penelope.Effects.Interpreter\n", 1)
		effStr = strings.Replace(effStr, "import Penelope.Effects.Concurrent\n", "import Penelope.Effects.Concurrent\nimport Penelope.Effects.Interpreter\n", 1)
		if err := os.WriteFile(effectsHsPath, []byte(effStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", effectsHsPath, err)
			os.Exit(1)
		}
		fmt.Printf("  [+] Updated %s (re-exported Penelope.Effects.Interpreter)\n", effectsHsPath)
	}

	// 4. Update packages/penelope-effects/test/Main.hs with interpreter effects test group
	testMainPath := "packages/penelope-effects/test/Main.hs"
	testBytes, err := os.ReadFile(testMainPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", testMainPath, err)
		os.Exit(1)
	}
	testStr := string(testBytes)
	if !strings.Contains(testStr, "Penelope.Effects.Interpreter Repertoire") {
		testSnippet := `
  describe "Penelope.Effects.Interpreter Repertoire" $ do
    it "verifies StepBudget fuel tracking and exhaustion" $ do
      fuelRef <- newIORef (5 :: Int)
      let handler = runStepBudget fuelRef
          hMap = insertField (Field @"stepBudget") handler emptyRecord
      let prog :: Eff '[] '["stepBudget" ':= UnaryEffect StepBudget] (Bool, Int, Bool)
          prog = do
            ok1 <- consumeStep 3
            remF <- remainingBudget
            ok2 <- consumeStep 10
            pure (ok1, remF, ok2)
      res <- runEffM hMap prog
      res ` + "`shouldBe`" + ` (True, 2, False)

    it "verifies FreshSym non-colliding symbol generation" $ do
      symRef <- newIORef (1 :: Int)
      let handler = runFreshSym symRef
          hMap = insertField (Field @"freshSym") handler emptyRecord
      let prog :: Eff '[] '["freshSym" ':= UnaryEffect FreshSym] (T.Text, Int, T.Text)
          prog = do
            s1 <- freshSymbol "tmp"
            u <- freshUnique
            s2 <- freshSymbol "var"
            pure (s1, u, s2)
      res <- runEffM hMap prog
      res ` + "`shouldBe`" + ` ("tmp_1", 2, "var_3")
`
		// Also ensure import qualified Data.Text as T is present in test/Main.hs
		if !strings.Contains(testStr, "import qualified Data.Text as T") {
			testStr = strings.Replace(testStr, "import qualified Control.Concurrent as C\n", "import qualified Control.Concurrent as C\nimport qualified Data.Text as T\n", 1)
		}
		// Append test group before the final end of main
		lastIndex := strings.LastIndex(testStr, "logEntries `shouldContain` [\"worker2-rel\"]")
		if lastIndex != -1 {
			target := "logEntries `shouldContain` [\"worker2-rel\"]"
			testStr = strings.Replace(testStr, target, target+"\n"+testSnippet, 1)
		}
		if err := os.WriteFile(testMainPath, []byte(testStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", testMainPath, err)
			os.Exit(1)
		}
		fmt.Printf("  [+] Updated %s (added interpreter effects spec)\n", testMainPath)
	}

	fmt.Println("=== [TP-PENELOPE-EFF-001A Task Runner Finished] ===")
}
