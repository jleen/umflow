flow.js : src/Main.elm
	elm make src/Main.elm --output flow.js

serve : flow.js
	python -m http.server
