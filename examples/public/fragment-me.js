/*---- Custom Element ----
 * Sets the fragment of the Url to its parent's #id
 * when intersecting with a 2%*1% rectangle on the viewport
 * at 40% 49% 59% 49% from top, right, bottom, left respectively.
 * The browser will then try to scroll the target completely in view.
 * 
 * Note 1: The target is this element's parent (if it has an #id)
 * Note 2: Make scrolling smooth by setting css on the scroll-container:
 *    `scroll-behavior: smooth;`
 * Note 3: This element competes with any other form of setting the #hash.
 *     To alleviate, we listen for hash changes and disable the observer
 *     until the hash is centered.
 */

customElements.define(
  "fragment-me",
  class extends HTMLElement {
    observer;
    id;
    options;
    intersected;
    counterIntersected;
    setObserver;

    constructor() {
      super();
    }



    connectedCallback() {

      console.log("connectedCallback", this);


      this.options = {
        rootMargin: "40% 49% 59% 49%",
        threshold: 0.9
      }
      this.id = this.parentElement?.id
      this.intersected = (entries, observer) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting) {
            console.log("SELF OBSERVED ", entry.target)
            window.location.hash = this.id
          }
        });
      };

      this.counterIntersected = (entries, observer) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting) {
            console.log("COUNTER OBSERVED ", entry.target)
            console.log("set self-observer", this.id, "<-:", window.location.hash);
            this.observer?.disconnect();
            this.observer = new IntersectionObserver(this.intersected, this.options);
            this.observer.observe(this.parentElement || this);
          }
        });
      };
      this.setObserver = () => {
        if (window.location.hash != '#' + this.id) {
          console.log("set counter-observer", this.id, ":->", window.location.hash);
          this.observer?.disconnect();
          this.observer = new IntersectionObserver(this.counterIntersected, this.options);
          this.observer.observe(document.querySelector(window.location.hash));
        } else {
          console.log("set self-observer", this.id, "<-:", window.location.hash);
          this.observer?.disconnect();
          this.observer = new IntersectionObserver(this.intersected, this.options);
          this.observer.observe(this.parentElement || this);
        }
      }


      this.setObserver();

      window.addEventListener('hashchange', e => { // Disable until hash is scrolled to (see Note 3)
        console.log("HASH CHANGED ->")
        this.setObserver();
      })
    }

    attributeChangedCallback(_name, _oldValue, _newValue) {
      console.log("attributeChangedCallback", this);
      if (this.setObserver)
        this.setObserver();
    }


    static get observedAttributes() {
      return ["increment"];
    }
  }
);
