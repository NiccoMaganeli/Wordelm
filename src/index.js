import { Elm } from "./Main.elm";

const now = new Date();
const year = now.getFullYear();
const month = now.getMonth();
const day = now.getDate();

const seed = +new Date(year, month, day);
const key = "wordelm";
const gameHistory = localStorage.getItem(key);

const app = Elm.Main.init({
  flags: { seed, gameHistory },
});

app.ports.saveGameHistory.subscribe((string) => {
  localStorage.setItem(key, string);
});

app.ports.copyToClipboard.subscribe((string) => {
  navigator.clipboard.writeText(string);
});
