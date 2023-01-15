/*---- Custom Element ----
 * Centers itself smoothly into the viewport
 */

customElements.define(
  "center-me",
  class extends HTMLElement {
    constructor() {
      super();
    }
    connectedCallback() {
      var closestAisle = this;
      console.log("connectedCallback", this);
      window.setTimeout(
        () =>
          requestAnimationFrame(() =>
            this.parentElement?.scrollIntoView({ behavior: "smooth", block: "center", inline: "center" })
          ),
        50
      );
    }


    static get observedAttributes() {
      return ["increment"];
    }
  }
);
