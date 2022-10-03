
ELM_ENTRYPOINT_PATH=src/Drench.elm
JS_ARTEFACT_FILE_NAME=drench-artifact.js

build-debug:
	elm make $(ELM_ENTRYPOINT_PATH) --output=$(JS_ARTEFACT_FILE_NAME) --debug

build:
	elm make $(ELM_ENTRYPOINT_PATH) --output=$(JS_ARTEFACT_FILE_NAME)

build-watch: build
	elm-live $(ELM_ENTRYPOINT_PATH) --hot -- --output=$(JS_ARTEFACT_FILE_NAME)

review-watch:
	elm-review --fix-all --watch

test-watch:
	elm-test --watch
