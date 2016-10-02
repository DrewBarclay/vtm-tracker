# vtm-tracker

Simple web app written in Haskell to track blood/willpower/etc. for the VTM tabletop game.

To use:

```
cd <project dir>
stack build
stack exec vtmtracker
```

Warning: this is a toy app I did to get more familiar with CSS3 and some Haskell web technologies. App is not secure (there isn't even a login), nor will it handle multiple connections gracefully.
