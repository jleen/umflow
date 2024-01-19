index.html : src/Main.elm
	elm make src/Main.elm

serve : index.html
	python -m http.server
