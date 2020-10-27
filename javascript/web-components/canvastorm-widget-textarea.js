const SCROLL_OFFSET = 5;

class CanvastormWidgetTextarea extends HTMLElement {
    connectedCallback() {
        this.textarea = document.createElement("textarea");
        this.textarea.className = this.className;
        this.textarea.style.whiteSpace = "pre";
        this.className = "";

        this.appendChild(this.textarea);

        if (this._cachedValue) {
            this.updateTextareaValue(this._cachedValue);
            this._cachedValue = undefined;
        }
    }

    focus() {
        this.textarea.focus();
    }

    set value(newValue) {
        if (this.textarea) {
            this.updateTextareaValue(newValue);
        } else {
            this._cachedValue = newValue;
        }
    }

    updateTextareaValue(newValue) {
        this.textarea.value = newValue;
        this.textarea.style.height = "5px";
        this.textarea.style.width = "5px";

        let height = this.textarea.scrollHeight + SCROLL_OFFSET;
        let width = this.textarea.scrollWidth + SCROLL_OFFSET;

        this.textarea.style.height = height + "px";
        this.textarea.style.width = width + "px";

        this.dispatchEvent(
            new CustomEvent("change-size", { detail: { width, height } })
        );
    }

    disconnectedCallback() {}
}

customElements.define("canvastorm-widget-textarea", CanvastormWidgetTextarea);
