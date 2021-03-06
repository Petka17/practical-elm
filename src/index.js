import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: localStorage.getItem("sessionId")
});

app.ports.saveSessionId.subscribe(sessionId => {
  if (sessionId === null) {
    localStorage.removeItem("sessionId");
  } else {
    localStorage.setItem("sessionId", sessionId);
  }
});
