/*---- Custom Element ----
 * Parent receives the focus when intersecting with a 2%*1% rectangle on the viewport
 * at 40% 49% 59% 49% from top, right, bottom, left respectively.
 * 
 * This custom element gives you a `focus` and a `blur` event.
 * 
 * Note: Some browsers do not yet implement the `preventScroll` option.
 *       See https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/focus
 */

customElements.define(
  "focus-when-in-center",
  class extends HTMLElement {

    constructor() {
      super();
    }
    connectedCallback() {
      console.log("connectedCallback", this);

      this.parentElement.tabIndex = "0";

      let options = {
        rootMargin: "40% 49% 59% 49%",
        threshold: 0.9
      }

      let intersected = (entries, observer) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting) {
            console.log("SELF OBSERVED ", entry.target, this.parentElement)
            this.parentElement?.focus({ preventScroll: true });
          }
        });
      };
      let observer = new IntersectionObserver(intersected, options);
      observer.observe(this.parentElement || this);
    }

    attributeChangedCallback(_name, _oldValue, _newValue) {
      console.log("attributeChangedCallback", _name, _oldValue, _newValue);
    }


    static get observedAttributes() {
      return ["increment"];
    }
  }
);
