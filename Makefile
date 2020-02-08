all:
	python ./create-require.py ./stdlib=./node_modules/bs-platform/lib/js > ./src/libs.ml
	yarn build
	browserify -o bundle.js index.js
	python ./compose.py
