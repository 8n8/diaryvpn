import flask
import json

app = flask.Flask(__name__)

@app.route("/")
def root():
    with open("index.html", "r") as f:
        return f.read()

@app.route("/summary")
def summary():
    try:
        with open("summary.json", "r") as f:
            return json.load(f)
    except FileNotFoundError:
        return []

@app.route("/favicon.ico")
def favicon():
    with open("favicon.ico", "rb") as f:
        return f.read()

