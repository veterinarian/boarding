elm.js: Boarding.elm Main.elm elm-stuff
	elm-make Main.elm --output elm.js

elm-stuff: elm-package.json

all: elm.js


