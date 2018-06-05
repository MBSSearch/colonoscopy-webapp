# Colonoscopy Item Finder - Single Page Application 

## Run

To leverage hot reloading while developing the actual business logic run `elm reactor`, then select `Colonoscopy.elm` from [localhost:8000](http://localhost:8000).

The method above doesn't perform the compilation of the `.js` used in the `index.html`. If you need to tweak the design or markup, then you'll have to edit the `.html` directly. This of course doesn't support hot reloading in the browser.

To see changes made to the `.elm` logic in the `index.html` page you'll have to rebuild the `.js`:

```
elm make Colonoscopy.elm --output=colonoscopy.js
```
