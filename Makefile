.PHONY: build
build:
	./build.sh
.PHONY: sync
sync:
	make build
	git status
	git add .
	git commit -m 'sync'
	git pull --rebase
	git push
