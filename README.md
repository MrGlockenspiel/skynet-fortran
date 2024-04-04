# skynet-fortran

AI will take over the world. <br><br> My first Fortran program, made to learn
the language. <br> A Discord bot using a markov chain trained on a channel as
messages are sent. <br>

## Configuration

Skynet-fortran is made with `fpm` <br> Ensure you have installed `fpm` and run
`fpm build` to build the project, and `fpm run` to run it. <br><br>

It expects a `config.json` file in the project root folder, containing these
contents:

```json
{
  "token": "xxxxxxxxxxxxxxxxxxxxxxxxx",
  "channels": [
    "xxxxxxxxxxxxxxxx",
    "xxxxxxxxxxxxxxxx"
  ]
}
```

## Commands

Command prefix is `SKYNET`, not configurable yet <br> Available commands:

- `HELPME`
  - Show available commands
- `RSTDAT`
  - Clear markov data
- `MARKOV`
  - Generate a string of garbage
