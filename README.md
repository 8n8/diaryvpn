# diaryvpn

**This is work in progress and not ready to use.**

DiaryVPN is a tiny web app for keeping a diary. It is intended to be run on the local network or over Tailscale.

# Run for development

The database file is kept at `$HOME/.diaryvpn`

## Backend

The backend is written in Haskell. You need to install the Haskell Tool Stack to build it.

Run the following commands:

```
cd backend
stack build
stack exec diaryvpn-exe
```

This will listen on port 3000. Go to http://localhost:3000 to see the app.
