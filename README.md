# Elmsfeuer

[![JetBrains team project](http://jb.gg/badges/team.svg)](https://confluence.jetbrains.com/display/ALL/JetBrains+on+GitHub)

Blog post: https://blog.jetbrains.com/team/2018/12/11/coding-the-art-continued/

Hosted at: http://code2art.jetbrains.com.

With the _Tron UI_: http://code2art.jetbrains.com#tron.

Generating Art since 2017.

Feel free to touch and drag anything you want.

Under Creative Commons License: <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/80x15.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>.

# Development

Install Elm and Webpack:

`npm install -g elm@0.18 webpack`

Then install required packages:

`npm install` (it will automatically call `elm-install` as well)

Run with:

`npm start`

If you want to build / minify, use:

`npm run build:player`
`npm run build`

See `./package.json` and `Dockerfile` for
