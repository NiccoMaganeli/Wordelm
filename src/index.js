import { Elm } from "./Main.elm";
import { words } from "../constants/words";

const now = new Date();
const year = now.getUTCFullYear();
const month = now.getUTCMonth();
const day = now.getUTCDate();

const seed = +new Date(year, month, day);

Elm.Main.init({
    flags: { seed, words },
});