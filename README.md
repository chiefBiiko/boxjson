# boxjson

[![Build Status](https://travis-ci.org/chiefBiiko/boxjson.svg?branch=master)](https://travis-ci.org/chiefBiiko/boxjson) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/chiefBiiko/boxjson?branch=master&svg=true)](https://ci.appveyor.com/project/chiefBiiko/boxjson)

Utility to transform JSON from **unboxed** to **boxed** state and vice versa.

## Approach

An *atom* is a string, number, boolean or null object that is not an item 
of an array.

A JSON string that *contains any atoms* is **unboxed**.

A JSON string that *does not contain any atoms* is **boxed**.

## Get it

``` r
devtools::install_github('chiefBiiko/boxjson')
```