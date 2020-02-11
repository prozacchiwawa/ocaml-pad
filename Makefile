all:
	mkdir -p stdlib
	cp ${BS_PLAYGROUND}/exports.js .
	cp ${BS_PLAYGROUND}/stdlib/* ./stdlib
	python ./create-require.py ./stdlib=./stdlib > ./src/libs.ml
	yarn build
	browserify -o bundle.js index.js
	python ./compose.py
