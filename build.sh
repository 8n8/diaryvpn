set -e

elm make frontend/Main.elm --output=elm.js
go build
./backend
