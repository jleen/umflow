index.html :
	elm make src/Main.elm

serve : index.html
	python -m http.server
