all:
	yarn build
	browserify -o bundle.js index.js
