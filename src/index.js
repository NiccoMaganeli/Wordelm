import { Elm } from "./Main.elm";

const now = new Date();
const year = now.getUTCFullYear();
const month = now.getUTCMonth();
const day = now.getUTCDate();

const seed = +new Date(year, month, day);

Elm.Main.init({
    flags: { seed },
});