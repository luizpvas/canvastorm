import "../css/app.css";
import { Elm } from "../elm/Main";
import "./web-components/canvastorm-widget-textarea";

export class Canvastorm {
    constructor() {
        this.onKeydown = this.keydown.bind(this);
        document.addEventListener("keydown", this.onKeydown);
    }

    /**
     * Canvastorm registers some events on `document` to detect global shortcuts.
     * If you're using this in a SPA, make sure to call this method whenever the user
     * navigates away to another page.
     */
    shutdown() {
        document.removeEventListener("keydown", this.onKeydown);
    }

    /**
     * A key was pressed. We need feed this signal into the Elm app.
     *
     * @param {KeyboardEvent} ev
     */
    keydown(ev) {
        if (this.app) {
            if (this.shouldIgnoreKeypress()) return;

            let key = ev.key;

            if (ev.ctrlKey) key = "ctrl+" + key;

            this.app.ports.shortcutPressed.send(key);
        }
    }

    /**
     * The user just pressed a key. Should we send to the Elm app for a possible shortcut or not?
     *
     * @return {boolean}
     */
    shouldIgnoreKeypress() {
        if (!document.activeElement) return false;
        if (document.activeElement.tagName == "TEXTAREA") return true;
        if (document.activeElement.tagName == "INPUT") return true;

        return false;
    }

    /**
     * Renders canvas to the given element.
     *
     * @param {HTMLElement} element
     */
    render(element) {
        requestAnimationFrame(() => {
            let rect = element.getBoundingClientRect();

            this.app = Elm.Main.init({
                node: element,
                flags: {
                    latestId: new Date().getTime(),
                    maxHistorySize: 50,
                    embedLeft: rect.left,
                    embedTop: rect.top,
                    embedWidth: rect.width,
                    embedHeight: rect.height,
                },
            });
        });
    }
}

window.addEventListener("load", () => {
    new Canvastorm().render(document.querySelector("#app"));
});
