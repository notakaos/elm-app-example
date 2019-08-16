# elm-app-example

## Development

### Requirements

* Docker
* Visual Studio Code + Remote Development Extension (Remote - Containers)

```sh
git clone https://github.com/notakaos/elm-app-example.git
cd elm-app-example
code .
# [Cmd + Shift + P] -> [Remote-Containers: Reopen Folder in Container]
```

```sh
# in container
elm-live src/01-button.elm --start-page=index.html -- --output=tmp/elm.js

#=> open http://localhost:8000
```