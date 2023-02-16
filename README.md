# diaryvpn

DiaryVPN is a tiny web app for keeping a diary. It is intended to be run over Tailscale, see https://tailscale.com.

Run the server on a computer that is on most of the time, and then access the web app from all your other devices over your Tailscale network.

# Install and run

## Set up Tailscale

Tailscale is a mesh VPN for connecting your devices, using your Google account for authentication. All the data is end-to-end encrypted between your devices. The free tier is fine for running DiaryVPN. It's really easy to set up with no configuration.

Install Tailscale on all your devices using the instructions here: https://tailscale.com/kb/installation. I have tried it on Linux desktop (Ubuntu) and on Android. After installing, you shouldn't need to do any configuration, just start up Tailscale and log into your Google account.

## Set the path to the database file

First clone this repository: `git clone git@github.com:8n8/diaryvpn.git`.

Then open the file `diaryvpn/app/Main.hs` and set the path of the file to store the diary in by editing the variable `dbPath` at the top. For example, I store it in `/home/t/.diaryvpn`.

## Build and run DiaryVPN

DiaryVPN is written in Haskell. You first need to install the Haskell Tool Stack to build it. Follow these instructions: https://docs.haskellstack.org/en/stable/#how-to-install-stack.

Then run the following commands in the root of this repository:

```
stack build
stack exec diaryvpn-exe
```

Do this on your computer that is on for most of the time.

This will start a web server on port 3456. Go to http://localhost:3456 to see the app.

On your other devices, open Tailscale to get the IP address on the Tailscale network of the device running the server. Then open the web browser and type in the IP address followed by a colon and the port number 3456, for example 100.32.78.97:3456.

# Diary data format

The diary data is kept in an ordinary file, with a simple binary format. Each new diary entry is appended to this file. Each entry is:

- a 32-bit timestamp: this is a little-endian integer, containing the POSIX time in seconds
- a 32-bit little-endian integer, containing the number of bytes in the diary entry
- a UTF8-encoded Unicode string, containing the diary entry
