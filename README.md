# diaryvpn

**This is work in progress and not ready to use.**

DiaryVPN is a tiny web app for keeping a diary. It is intended to be run on the local network or over Tailscale.

# Run for development

The static files and database are kept in `$HOME/.diaryvpn`

## Frontend

The frontend is written in Elm. Run these commands to build it:

```
cd frontend
mkdir ~/.diaryvpn
elm make src/Main.elm --output=$HOME/.diaryvpn/index.html
```

## Backend

Run the following commands:

```
cd backend
stack build
stack exec diaryvpn-exe
```

This will listen on port 3000. Go to http://localhost:3000 to see the app.
