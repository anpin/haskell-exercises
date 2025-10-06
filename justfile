default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ ARGS }}

# Run ghcid -- auto-recompile and run `main` function
run *ARGS:
    ghcid --command="cabal repl exe:exercises" -T ':main {{ ARGS }}' 2>&1 | cat

# Run ghcid -- auto-recompile and run `main` function
rung *ARGS:
    ghcid --command="cabal repl exe:game-of-life" -T ':main {{ ARGS }}' 2>&1 | cat


# Run tests
test:
    cabal test --test-show-details=direct

# Run tests in watch mode with ghcid
test-watch:
    ghcid --command="cabal repl exercises-test" --test=":main"
