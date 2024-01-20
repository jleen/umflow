flow.js : src/Main.elm
	elm make src/Main.elm --output flow.js

debug :
	elm make src/Main.elm --output flow.js --debug

serve : flow.js
	python -m http.server
