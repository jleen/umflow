flow.js : src/Main.elm
	elm make src/Main.elm --output flow.js

serve : index.html
	python -m http.server
